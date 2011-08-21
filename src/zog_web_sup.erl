-module(zog_web_sup).

-behaviour(supervisor).

%% External exports
-export([start_link/0]).

%% supervisor callbacks
-export([init/1]).

start_link() ->
  supervisor:start_link({local, ?MODULE}, ?MODULE, []).

init([]) ->
  {ok, WebIP}    = application:get_env(zog_web, web_ip),
  {ok, WebPort}  = application:get_env(zog_web, web_port),

  Web = web_specs(zog_mochiweb, WebIP, WebPort),
  LiveConfig = {liveconfig_sup, {liveconfig_sup, start_link, []},
                permanent, infinity, supervisor, [liveconfig_sup]},
  RouteServer = {zog_route_server, {zog_route_server, start_link, []},
                 permanent, 5000, worker, [zog_route_server]},

  Processes = [LiveConfig,
               RouteServer,
               Web],
  Strategy = {one_for_one, 10, 10},
  {ok,
   {Strategy, lists:flatten(Processes)}}.

web_specs(Mod, PreIP, Port) when is_integer(Port) ->
  {ok, IP} = inet:getaddr(PreIP, inet),
  WebConfig = [{ip, IP}, {port, Port}],
  {Mod,
   {Mod, start, [WebConfig]},
   permanent, 5000, worker, [Mod]}.
