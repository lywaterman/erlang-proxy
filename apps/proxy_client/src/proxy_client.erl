-module(proxy_client).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1, reload_lua/0]).

-export([start/0]).

-define(LUA_ROOT_MODULE, "lua/root.lua").
-define(LUA_LOGIC_MODULE, "lua/shadowsocks.lua").

%% ===================================================================
%% Application callbacks
%% ===================================================================

priv_dir() ->
	Ebin = filename:dirname(code:which(?MODULE)),
	filename:join(filename:dirname(Ebin), "priv").

create_luavm() ->
	PrivDir = priv_dir(),
	RootLuaModule = filename:join(PrivDir, ?LUA_ROOT_MODULE),
	LuaModule = filename:join(PrivDir, ?LUA_LOGIC_MODULE),
    setup_logicvm_by_name(luavm,  RootLuaModule, LuaModule).

reload_lua() ->
  reload_logicvm_by_name(luavm).

reload_logicvm_by_name(Name) ->
    PrivDir = priv_dir(),
    RootLuaModule = filename:join(PrivDir, ?LUA_ROOT_MODULE),
    ok = moon:load(Name, RootLuaModule),

    {ok, _} = moon:call(Name, "reload", []),

	LuaModule = filename:join(PrivDir, ?LUA_LOGIC_MODULE),
	ok = moon:load(Name, LuaModule).

setup_logicvm_by_name(Name, RootLuaModule, LuaModule) ->
    {ok, VMPid} = moon:start_vm(),
    register(Name, VMPid),

    ok = moon:load(VMPid, RootLuaModule),
    {ok, _} = moon:call(Name, "set_lua_path", [erlang:list_to_binary(filename:join(priv_dir(), "lua/?.lua"))]),

    moon:call(Name, "set_appenv", [application:get_all_env(), node()]),

    ok = moon:load(VMPid, LuaModule).


start(_StartType, _StartArgs) ->
    lager:set_loglevel(lager_console_backend, debug),
    create_luavm(),
    proxy_client_sup:start_link().

stop(_State) ->
    ok.

start() ->
    application:start(lager),
    application:start(moon),
    application:start(?MODULE).
