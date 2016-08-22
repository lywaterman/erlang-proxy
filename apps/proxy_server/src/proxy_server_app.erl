-module(proxy_server_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-define(LUA_ROOT_MODULE, "lua/root.lua").
-define(LUA_LOGIC_MODULE, "lua/shadowsocks.lua").

%% ===================================================================
%% Application callbacks
%% ===================================================================


start(_StartType, _StartArgs) ->
    proxy_server_sup:start_link().

stop(_State) ->
    ok.
