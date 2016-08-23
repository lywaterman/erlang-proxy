%%%-------------------------------------------------------------------
%%% @author Wang ShuYu <andelf@gmail.com>
%%% @copyright (C) 2013, Wang ShuYu
%%% @doc
%%%
%%% @end
%%% Created :  8 Apr 2013 by Wang ShuYu <andelf@gmail.com>
%%%-------------------------------------------------------------------
-module(proxy_client_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
         terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-record(state, {server_ip,
                server_port,
                server_sock,
                client_ip,
                client_port,
                client_sock,
                ss_addr = none
               }).

-define(SOCK_OPTIONS,
        [binary,
         {reuseaddr, true},
         {active, false},
         {nodelay, true}
        ]).

-include("proxy_defs.hrl").


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
% Leave name {local, name} so that the process
% remains unregistered. This enables us to start
% mutliple processes using the pr_sup:start_child() call
start_link(ClientSock) ->
    ConfFile = filename:join(code:priv_dir(proxy_client), "client.conf"),
    case file:consult(ConfFile) of
        {ok, Conf} ->
            gen_server:start_link(?MODULE, [{client_sock, ClientSock}|Conf], []);
        {error, _Reason} ->
            {error, conf_file_error}
    end.

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init(Conf) ->
    ServerIP = proplists:get_value(server_ip, Conf),
    ServerPort = proplists:get_value(server_port, Conf),
    ClientIP = proplists:get_value(listen_ip, Conf),
    ClientPort = proplists:get_value(listen_port, Conf),
    Client = proplists:get_value(client_sock, Conf),

    %%lager:debug("conf:~p",[Conf]),
    case gen_tcp:connect(getaddr_or_fail(ServerIP), ServerPort, ?SOCK_OPTIONS) of
        {ok, RemoteSocket} ->
            %%连接远端服务器成功
            {ok, Return} = moon:call(luavm, "worker_init", [pid_to_binary(self())]),
            lager:debug("return:~p, ~p", [self(), Return]),
            ok = inet:setopts(RemoteSocket, [{active, true}]),
            {ok, #state{server_ip=ServerIP,
                        server_port=ServerPort,
                        server_sock=RemoteSocket,
                        client_ip=ClientIP,
                        client_port=ClientPort,
                        client_sock=Client}, 0};
                 %%communicate(Client, RemoteSocket);
        {error, Error} ->
            lager:debug("Connect error, ~p. ~p:~p", [Error, ServerIP, ServerPort]),
            gen_tcp:close(Client),
            {stop, server_connect_fail}
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info(timeout, #state{server_sock=RemoteSocket, client_sock=Client, client_ip=LocalIP, client_port=LocalPort} = State) ->
%    try
        case find_target(Client) of
            {ok, Mod, {connect, Addr}} ->
                lager:debug("addr:~p", [Addr]),
                SSAddr = encode_addr(Addr),
                ok = inet:setopts(Client, [{active, true}]),
                IP = list_to_binary(tuple_to_list(getaddr_or_fail(LocalIP))),
                {ok, Data} = moon:call(luavm, send_data, [pid_to_binary(self()), SSAddr]),
                ok=gen_tcp:send(RemoteSocket, Data),
                ok = gen_tcp:send(Client, Mod:unparse_connection_response({granted, {ipv4, IP, LocalPort}})),
                {noreply, State#state{ss_addr=SSAddr}};
            {error, client_closed} ->
                {stop, normal, State};
            {error, Reason} ->
                ?LOG("client communication init error: ~p~n", [Reason]),
                {stop, Reason, State}
    %%     end
    %% catch
    %%     error:{badmatch,_} ->
    %%         {stop, normal, State};
    %%     _Error:_Reason ->
    %%         ?LOG("client recv error, ~p: ~p~n", [_Error, _Reason]),
    %%         {stop, normal, State}
    end;
handle_info({tcp, Client, Request}, #state{server_sock=RemoteSocket, client_sock=Client, ss_addr=SSAddr} = State) ->
    lager:debug("send:~p", [Request]),
    {ok, Data} = moon:call(luavm, send_data, [pid_to_binary(self()), Request]),
    case gen_tcp:send(RemoteSocket, Data) of
        ok ->
            {noreply, State};
        {error, _Error} ->
            {stop, _Error, State}
    end;
handle_info({tcp, RemoteSocket, Response}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    lager:debug("response:~p", [Response]),
    {ok, Data} = moon:call(luavm, recv_data, [pid_to_binary(self()), Response]),
    lager:debug("after decrypt:~p", [Data]),
    case gen_tcp:send(Client, Data) of
        ok ->
            %%{stop, 1, State};
            {noreply, State};
        {error, _Error} ->
            {stop, _Error, State}
    end;
handle_info({tcp_closed, ASocket}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case ASocket of
        Client ->
            lager:debug("client server tcp close"),
            {stop, normal, State};
        RemoteSocket ->
            lager:debug("remote server tcp close"),
            {stop, normal, State}
    end;
handle_info({tcp_error, ASocket, _Reason}, #state{server_sock=RemoteSocket, client_sock=Client} = State) ->
    case ASocket of
        Client ->
            ?LOG("~p client tcp error~n", [ASocket]),
            {stop, _Reason, State};
        RemoteSocket ->
            ?LOG("~p server tcp error~n", [ASocket]),
            {stop, _Reason, State}
    end;

handle_info(_Info, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason,  #state{server_sock=RemoteSocket, client_sock=Client}) ->
    {ok, _} = moon:call(luavm, "worker_terminate", [pid_to_binary(self())]),
    gen_tcp:close(RemoteSocket),
    gen_tcp:close(Client),
    ok;
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
getaddr_or_fail(IP) ->
    {ok, Addr} = inet:getaddr(IP, inet),
    Addr.


find_target(Client) ->
    %% 0x05:version
    case gen_tcp:recv(Client, 0) of
        {ok, <<Version:8, _/binary>> = Greeting} ->
            socks_proxy_handshake(Client, Version, Greeting);
        {error, closed} ->
            {error, client_closed};
        {error, Reason} ->
            {error, Reason}
    end.

socks_proxy_handshake(Client, Version, Greeting) ->
    case Version of
        %% SOCKS4
        16#04 ->
            case proxy_proto_socks4:parse_greeting_request(Greeting) of
                {connect, _UserId, Addr} ->
                    {ok, proxy_proto_socks4, {connect, Addr}};
                {error, Reason} ->
                    {error, Reason}
            end;
        16#05 ->
            {auth_methods, _} = proxy_proto_socks5:parse_greeting_request(Greeting),
            gen_tcp:send(Client, proxy_proto_socks5:unparse_greeting_response(no_auth)),
            {ok, ConnReq} = gen_tcp:recv(Client, 0),
            case proxy_proto_socks5:parse_connection_request(ConnReq) of
                {connect, Addr} ->
                    {ok, proxy_proto_socks5, {connect, Addr}};
                {error, Reason} ->
                    {error, Reason}
            end
    end.

pid_to_binary(Pid) ->
    list_to_binary(pid_to_list(Pid)).

encode_ss({Iv, Addr, Data}) ->
    <<Iv/binary, Addr/binary, Data/binary>>;
encode_ss({Addr, Data}) ->
    <<Addr/binary, Data/binary>>.

encode_addr({ipv4, Address, Port}) ->
    <<1:8, Address:32, Port:16>>;
encode_addr({ipv6, Address, Port}) ->
    <<4:8, Address:128, Port:16>>;
encode_addr({domain, DomainBin, Port}) ->
    <<3:8/little, (byte_size(DomainBin)):8, DomainBin/binary, Port:16>>;
encode_addr(_) ->
    error.
