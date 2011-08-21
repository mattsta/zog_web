-module(zog_web).

-export([start/0]).
-export([add_config_dir/1]).

%%%--------------------------------------------------------------------
%%% application starting
%%%--------------------------------------------------------------------
start() ->
  application:start(zog_web).

%%%--------------------------------------------------------------------
%%% config management
%%%--------------------------------------------------------------------
add_config_dir(Dir) ->
  zog_route_server:add_config_dir(Dir).
