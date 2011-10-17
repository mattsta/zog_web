-module(zog_route_server).

-include("zog_route.hrl").

-import(proplists, [get_value/2, get_value/3]).

%% gen_server callbacks
-export([init/1,
         handle_call/3, handle_cast/2,
         handle_info/2, terminate/2, code_change/3]).

%% api callbacks
-export([start_link/0]).
-export([route/1, routes/0]).
-export([strategy_for_hostpath/2]).
-export([update_route/3]).
-export([add_config_dir/1]).
-export([load_route_from_route_db_instead_of_file/1,
         add_route_db_entry/1,
         add_route_db_entry_immediate/1]).

-record(state, {route_table,
                strategy_table,
                route_db_table,
                file_domain_table}).

-define(RTABLE, zog_route_server_routes).          % RTABLE = Route Table
-define(STABLE, zog_route_server_strategies).      % STABLE = Strategy Table
-define(FTABLE, zog_route_server_filename_domain). % FTABLE = filename<->domain
-define(RDBTABLE, zog_route_server_db_routes).     % RDBTABLE = DB of configs

%%%--------------------------------------------------------------------
%%% api callbacks
%%%--------------------------------------------------------------------
start_link() ->
  gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

%%%--------------------------------------------------------------------
%%% gen_server callbacks
%%%--------------------------------------------------------------------
init([]) ->
  EtsArgs = [public, named_table,
             {keypos, 2},  % we're storing #zog_route{}s
             {read_concurrency, true}],
  EtsFirstKeyArgs = EtsArgs -- [{keypos, 2}],
  RouteTableId = ets:new(?RTABLE, EtsArgs),
  StrategyTableId = ets:new(?STABLE, EtsArgs),
  FileDomainTableId = ets:new(?FTABLE, [duplicate_bag | EtsFirstKeyArgs]),
  RouteDBTableId = ets:new(?RDBTABLE, EtsFirstKeyArgs),

  load_routes_by_env(),

  {ok, #state{route_table = RouteTableId,
              strategy_table = StrategyTableId,
              route_db_table = RouteDBTableId,
              file_domain_table = FileDomainTableId}}.

handle_call(_What, _From, State) ->
  {reply, not_implemented, State}.

handle_cast(_Msg, State) ->
  {noreply, State}.

handle_info(_Info, State) ->
  {noreply, State}.

terminate(_Reason, _State) ->
  ok.

code_change(_OldVsn, State, _Extra) ->
  {ok, State}.

%%%--------------------------------------------------------------------
%%% Loading Config
%%%--------------------------------------------------------------------
load_routes_by_env() ->
  case application:get_env(zog_web, route_dirs) of
    {ok, Dirs} -> load_routes_by_dir(Dirs);
             _ -> no_env
  end,
  case application:get_env(zog_web, app_route_dirs) of
    {ok, AppDirs} -> load_routes_by_dir(AppDirs);
                _ -> no_app_env
  end,
  case application:get_env(zog_web, load_external_routes_fun) of
    {ok, {M, F, A}} -> M:F(A);
                  _ -> no_load_fun_env
  end.


add_config_dir(Dir) when is_list(Dir) andalso is_integer(hd(Dir)) ->
  load_routes_by_dir([Dir]).

load_routes_by_dir(Dirs) when is_list(Dirs) andalso is_list(hd(Dirs)) ->
  FixedDirs = fix_dirs(Dirs),
  [liveconfig_sup:start_watcher(Dir, "*.conf", fun update_route/3, 5000) ||
    Dir <- FixedDirs].

% Paths are either absolute or relative to the project root
% (one level above the main project's ebin/).
fix_dirs([]) -> [];
fix_dirs([Path | T]) ->
  case filename:pathtype(Path) of
    absolute -> [Path | fix_dirs(T)];
    relative ->
      ZogWebEbinDir = filename:dirname(code:which(?MODULE)),
      % deps/zog_web/ebin/zog_route_server.beam
      % deps/zog_web/ebin/
      % deps/zog_web/ebin/../ -> deps/zog_web/
      % deps/zog_web/../      -> deps/
      % deps/../              -> Project Directory
      [filename:join([ZogWebEbinDir, "..", "..", "..", Path]) |
        fix_dirs(T)]
  end.

update_route(Changed, New, Deleted) ->
  [load_route_by_file(C) || C <- Changed],
  [load_route_by_file(N) || N <- New],
  [delete_route_by_file(D) || D <- Deleted].

load_route_by_file(Filename) ->
  case file:consult(Filename) of
    {ok, Terms} -> load_routes(Terms, Filename);
              _ -> error
  end.

delete_route_by_file(Filename) ->
  case ets:lookup(?FTABLE, Filename) of
    [{Filename, Hostname}] -> [R] = ets:lookup(?RTABLE, Hostname),
                              HandlerMod = R#zog_route.handler_module,
                              code:purge(HandlerMod),
                              ets:delete(?RTABLE, Hostname),
                              Pattern =
                              #zog_route_strategy{hostpath = {Hostname, '_'}},
                              ets:match_delete(?STABLE, Pattern);
                         _ -> none
  end.


%%%--------------------------------------------------------------------
%%% Applying Loaded Config
%%%--------------------------------------------------------------------
% keys of config proplist: domain, module, default_path_function,
% default_subdomain_function, extra_args, path_mods, path_strategies
% path_strategies: authenticate, [{not_authenticated_function, F}, {paths}]
%                 authorize: [{not_authorized_function, F}, {paths, [{P, Fun}]}]
load_routes([], _) -> done;
load_routes([Route|T], Filename) ->
  Domain = get_value(domain, Route),
  Module = get_value(module, Route),
  ExtraArgs = get_value(extra_args, Route, []),
  UnkPath = get_value(default_path_function, Route, default_page),
  UnkSubdomain = get_value(default_subdomain_function, Route,default_subdomain),
  PathMods = get_value(path_mods, Route, []),
  PathStrategies = get_value(path_strategies, Route, []),
  StringDomain = case Domain of
                   D when is_binary(D) -> binary_to_list(D);
                     D when is_list(D) -> D
                 end,
  TokenizedDomain = string:tokens(StringDomain, "."),
  NewRoute = #zog_route{hostname = TokenizedDomain,
                        handler_module = Module,
                        default_path_function = UnkPath,
                        default_subdomain_function = UnkSubdomain,
                        extra_args = ExtraArgs,
                        path_mods = PathMods,
                        path_strategies = PathStrategies},
  true = ets:insert(?FTABLE, {Filename, TokenizedDomain}),
  provision_strategies(NewRoute),
  true = ets:insert(?RTABLE, NewRoute),
  load_routes(T, Filename).

provision_strategies(#zog_route{hostname = Domain,
                                handler_module = HandlerModule,
                                path_strategies = Strategies}) ->
  case Strategies of
    none -> ok;
       _ -> Authens = proplists:get_all_values(authenticate, Strategies),
            provision_authenticate(Domain, HandlerModule, Authens),
            Authorzs = proplists:get_all_values(authorize, Strategies),
            provision_authorization(Domain, HandlerModule, Authorzs)
  end.

provision_authenticate(_, _, []) -> done;
provision_authenticate(Domain, HandlerMod, [Authen|T]) ->
  Failure = get_value(not_authenticated_function, Authen, noauth),
  Paths   = get_value(paths, Authen, []),
  RouteStrategies =
  [#zog_route_strategy{hostpath = {Domain, Path},
                       strategy = authenticate,
                       failure_function = Failure} || Path <- Paths],
  true = ets:insert(?STABLE, RouteStrategies),
  provision_authenticate(Domain, HandlerMod, T).

provision_authorization(_, _, []) -> done;
provision_authorization(Domain, HandlerMod, [Authorz|T]) ->
  Failure = get_value(not_authorized_function, Authorz, noauthz),
  PathAuthorizers = get_value(paths, Authorz, []),
  RouteStrategies =
  [#zog_route_strategy{hostpath = {Domain, Path},
                       strategy = authorize,
                       failure_function = Failure,
                       run_function = AuthorizeFunction} ||
    {Path, AuthorizeFunction} <- PathAuthorizers],
  true = ets:insert(?STABLE, RouteStrategies),
  provision_authorization(Domain, HandlerMod, T).

%%%--------------------------------------------------------------------
%%% Route Getting
%%%--------------------------------------------------------------------
route(Hostname) when is_list(Hostname) andalso is_list(hd(Hostname)) ->
  % See if the hostname we used exists first.  If so, use that route.
  case ets:lookup(?RTABLE, Hostname) of
    [R] -> R;
     [] -> route(tl(Hostname))
  end;
route(Hostname) when is_list(Hostname) andalso is_integer(hd(Hostname)) ->
  route(string:tokens(Hostname, "."));
route([]) -> noroute.

routes() ->
  ets:tab2list(?RTABLE).

% Use the Route Config DB to load a config instead of from a file
load_route_from_route_db_instead_of_file(Hostname) when
    is_list(Hostname) andalso is_list(hd(Hostname)) ->
  case ets:lookup(?RDBTABLE, Hostname) of
    [{Hostname, R}] -> case lists:keyfind(module, 1, R) of
                         {module, M} when is_atom(M) ->
                           load_routes([R], {from_db, Hostname});
                         Other -> throw({not_implemented, Other})
                       end,
                       route(Hostname);
                 [] -> load_route_from_route_db_instead_of_file(tl(Hostname))
  end;
load_route_from_route_db_instead_of_file([]) -> nodbroute.

add_route_db_entry(Config) ->
  Hostname =
  case element(2, lists:keyfind(domain, 1, Config)) of
    H when is_list(H) and is_list(hd(H)) -> H;  % domain is already tokenized
    H when is_binary(H) -> string:tokens(binary_to_list(H), ".");
    H when is_list(H) -> string:tokens(H, ".")
  end,
  ets:insert(?RDBTABLE, {Hostname, Config}),
  % NOTE!  This does not update the RTABLE.  Use immediate for RTABLE forcing.
  Hostname.

add_route_db_entry_immediate(Config) ->
  Hostname = add_route_db_entry(Config),
  load_route_from_route_db_instead_of_file(Hostname).

strategy_for_hostpath(Domain, Path) ->
  ets:lookup(?STABLE, {Domain, Path}).
