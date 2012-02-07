-module(zog_mochiweb).

-include("zog_route.hrl").

-import(proplists, [get_value/2, get_value/3]).
-import(lists, [keysort/2]).
-export([start/1, stop/0, loop/1]).

%%%----------------------------------------------------------------------
%%% starting/stopping
%%%----------------------------------------------------------------------
start(Options) ->
  mochiweb_http:start([{name, ?MODULE},
                       {backlog, 511},
%                       {acceptor_pool_size, 16},
%                       {nodelay, true},
                       {loop, fun loop/1} | Options]).

stop() ->
  mochiweb_http:stop(?MODULE).

%%%----------------------------------------------------------------------
%%% mochiweb event loop
%%%----------------------------------------------------------------------
loop(Req) ->
  process_flag(error_handler, zog_web_error_handler),
  Path = Req:get(path),
  TokenPath = string:tokens(Path, "/"),
  Host = Req:get_header_value("host"),
  try
    mux_domain(Req, Host, TokenPath)
  catch
    throw:{perm_redirect, Where} -> zog_page:perm_redirect(Req, Where);
    throw:{temp_redirect, Where} -> zog_page:temp_redirect(Req, Where);
    throw:{action, Action}       -> Action();
    Type:What ->
      Report = ["web request failed",
            {host, Host},
            {path, Path},
            {tokenized_path, TokenPath},
            {type, Type}, {what, What},
            {trace, erlang:get_stacktrace()}],
      error_logger:error_report(Report),
      Req:respond({500, [{"Content-Type", "text/plain"}],
             "gomez, your request failed. sorry.\n"})
  end.


mux_domain(Req, Hostname, Path) ->
  TokenizedHost = string:tokens(Hostname, "."),
  RunRoute = fun(Route) ->
               run_route(Req, TokenizedHost, Req:get(method), Path, Route)
             end,
  case zog_route_server:route(TokenizedHost) of
    #zog_route{} = R -> RunRoute(R);
             noroute -> throw(nosite)
   end.


%%%----------------------------------------------------------------------
%%% route running
%%%----------------------------------------------------------------------
run_route(Req, OriginalHostname, Method, Path,
    #zog_route{hostname = OriginalHostname,
               default_path_function = DefaultFun,
               extra_args = Extra,
               authentication_module = AuthenModule,
               authorization_module = AuthzModule,
               path_strategies = []} = Route) ->
  Cxn = zog_cxn:new(Req, Extra, AuthenModule, AuthzModule),
  HandlerMod = handler_mod(Path, Route),
  HandlerFun = handler_name(Path, DefaultFun),
  UsePath = use_path(Path, HandlerFun, DefaultFun),
  fun_or_exported(HandlerMod, HandlerFun, DefaultFun, Method, UsePath, Cxn);

run_route(Req, OriginalHostname, Method, Path,
    #zog_route{hostname = OriginalHostname,
               default_path_function = DefaultFun,
               authentication_module = AuthenModule,
               authorization_module = AuthzModule,
               extra_args = Extra} = Route) ->
  Cxn = zog_cxn:new(Req, Extra, AuthenModule, AuthzModule),
  HandlerMod = handler_mod(Path, Route),
  HandlerFun = check_strategy(Method, Path, Cxn, Extra, HandlerMod, Route),
  UsePath = use_path(Path, HandlerFun, DefaultFun),
  fun_or_exported(HandlerMod, HandlerFun, DefaultFun, Method, UsePath, Cxn);

% This only runs if the found hostname doesn't match the requested name
run_route(Req, _OriginalHostname, Method, Path,
    #zog_route{hostname = _OtherHostname,
               handler_module = HandlerMod,
               default_subdomain_function = DefaultSubdomainFun,
               authentication_module = AuthenModule,
               authorization_module = AuthzModule,
               extra_args = Extra}) ->
  Cxn = zog_cxn:new(Req, Extra, AuthenModule, AuthzModule),
  fun_or_exported(HandlerMod, DefaultSubdomainFun, Method, Path, Cxn).

-compile({inline, [{action_path, 1}]}).
action_path([]) -> [];
action_path([_Action|MorePath]) -> MorePath.

-compile({inline, [{use_path, 3}]}).
use_path(Path, HandlerFun, DefaultFun) ->
  case HandlerFun of
    DefaultFun -> Path;
             _ -> action_path(Path)
  end.

%%%----------------------------------------------------------------------
%%% strategy checking and running
%%%----------------------------------------------------------------------
% We sort the strategies so authEnticate runs before authOrize
check_strategy(Method, Path, Cxn, Extras, Mod,
    #zog_route{hostname = Domain,
               default_path_function = DefaultFun}) ->
  case zog_route_server:strategy_for_hostpath(Domain, Path) of
        [] -> handler_name(Path, DefaultFun);
    Strats -> SortedStrategies = keysort(#zog_route_strategy.strategy, Strats),
              case strategy_cycle(Mod, Extras,
                                  Method, Path, Cxn, SortedStrategies) of
                {fail, FailFun} -> FailFun;
                             ok -> handler_name(Path, DefaultFun)
              end
  end.

strategy_cycle(Mod, Extras, Method, Path, Cxn, Strategies) ->
  strategy_cycle(Mod, Extras, Method, Path, Cxn, Strategies, ok).

strategy_cycle(Mod, Extras, Method, Path, Cxn, [Strategy|T], ok) ->
  Status = run_strategy(Mod, Extras, Method, Path, Cxn, Strategy),
  strategy_cycle(Mod, Extras, Method, Path, Cxn, T, Status);
strategy_cycle(_, _, _, _, _, _, {fail, FailFun}) -> FailFun;
strategy_cycle(_, _, _, _, _, [], ok) -> ok.

% Authenticate
run_strategy(_, _, _, _, Cxn,
    #zog_route_strategy{strategy = authenticate,
                        failure_function = FailFun}) ->
  case Cxn:logged_in() of
     true -> ok;
    false -> {fail, FailFun}
  end;
% Authorize
run_strategy(Mod, Extras, Method, Path, Cxn,
    #zog_route_strategy{strategy = authorize,
                        run_function = RunFun,
                        failure_function = FailFun}) ->
  case Mod:RunFun(Method, Path, Cxn) of
    allow -> ok;
     deny -> {fail, FailFun};
                     % this probably isn't right anymore.  we need to populate
                     % the new Cxn with proper module params.  We can't use
                     % the existing Cxn as an exiting Req.
    {deny, Error} -> NewCxn = zog_cxn:new(Cxn, [{error, Error} | Extras]),
                     Failure =
                     case is_function(FailFun) of
                       false -> fun(_, _, _) ->
                                  Mod:FailFun(Method, Path, NewCxn)
                                end;
                        true -> fun(_, _, _) ->
                                  FailFun(Method, Path, NewCxn)
                                end
                     end,
                     {fail, Failure}
    end.

%%%----------------------------------------------------------------------
%%% helpers
%%%----------------------------------------------------------------------
-compile({inline, [{handler_name, 2}]}).
handler_name(Path, DefaultFun) ->
  try
    list_to_existing_atom(hd(Path))
  catch
    error:badarg -> DefaultFun
  end.

-compile({inline, [{handler_mod, 2}]}).
handler_mod(Path, #zog_route{handler_module = DefaultMod,
                             path_mods = PathMods}) ->
  try
    get_value(hd(Path), PathMods, DefaultMod)
  catch
    error:badarg -> DefaultMod
  end.

-compile({inline, [{fun_or_exported, 5}]}).
% Auto-duplicate HandlerFun here to fake a default.
fun_or_exported(HandlerMod, HandlerFun, Method, Path, Cxn) ->
  fun_or_exported(HandlerMod, HandlerFun, HandlerFun, Method, Path, Cxn).

% Our funs may be function references or just atom names. Figure it out here.
fun_or_exported(_, Fun, _, Method, Path, Cxn) when is_function(Fun) ->
  Fun(Method, Path, Cxn);
fun_or_exported(HandlerMod, HandlerFun, DefaultFun, Method, Path, Cxn) ->
  fun_or_exported(first, HandlerMod, HandlerFun, DefaultFun, Method, Path, Cxn).

fun_or_exported(first, HandlerMod, HandlerFun, DefaultFun, Method, Path, Cxn) ->
  case erlang:function_exported(HandlerMod, HandlerFun, 3) of
     true -> HandlerMod:HandlerFun(Method, Path, Cxn);
    false -> try  % funcion_exported doesn't load modules.  this will try:
               HandlerMod:module_info(imports) % probably empty so not wasteful
             catch
               _:_ -> ok  % we don't care if it failed, we run again only once
             end,
             fun_or_exported(again, HandlerMod, HandlerFun,
                                    DefaultFun, Method, Path, Cxn)
  end;
fun_or_exported(again, HandlerMod, HandlerFun, DefaultFun, Method, Path, Cxn) ->
  case erlang:function_exported(HandlerMod, HandlerFun, 3) of
     true -> HandlerMod:HandlerFun(Method, Path, Cxn);
    false -> case is_atom(DefaultFun) of
               true -> HandlerMod:DefaultFun(Method, Path, Cxn);
               false when is_function(DefaultFun) ->
                 DefaultFun(Method, Path, Cxn);
               false -> []
             end
  end.
