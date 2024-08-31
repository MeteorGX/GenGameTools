%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 网络服务的应用
%%% @end
%%%-------------------------------------------------------------------
-module(net_app).
-behaviour(application).
-include("agent.hrl").


-export([start/2, stop/1]).

%% @doc 回调接口
-export([
  on_tcp_established/1,
  on_tcp_message/2,
  on_tcp_closed/1
]).

%% @doc TCP 监听配置
-define(TCP_OPTS, [
  binary,
  {packet, 0},
  {active, false},
  {reuseaddr, true},
  {nodelay, true},
  {delay_send, false},
  {backlog, 5120},
  {send_timeout, 12000},
  {keepalive, false},
  {exit_on_close, true}
]).

%% @doc 应用启动回调
start(_StartType, _StartArgs) ->
  Cores = sys_utils:cpu_cores(), % 监听池数量


  % TCP 监听服务
  {?ok, TcpPid} = tcp_listener:start_link(9377, [
    {?pool_size, Cores},
    {?socket_options, ?TCP_OPTS},
    {?on_established, ?MODULE, on_tcp_established},
    {?on_message, ?MODULE, on_tcp_message},
    {?on_closed, ?MODULE, on_tcp_closed}
  ]),
  io:format("TCP Listener By ~w~n", [TcpPid]),


  {?ok, proc_utils:proc_id()}.

%% @doc 应用退出回调
stop(_State) ->
  ?ok.

%% @doc 会话初始化
on_tcp_established(State = #agent{}) ->
  io:format("Established = ~w~n", [State]),
  State.

%% @doc 进程关闭
on_tcp_closed(State = #agent{}) ->
  io:format("Closed = ~w~n", [State]),
  ?skip.


%% @doc 消息回调
on_tcp_message(<<Length:?u32_t, Protocol:?u32_t, Bytes:Length/?bytes_t, Next/?bytes_t>>, State = #agent{
  session = #session{}
}) ->
  io:format("Session Request(L: ~w, Id: ~w) = ~w", [Length, Protocol, Bytes]),
  on_tcp_request(Protocol, Bytes, State#agent{
    bytes = Next % 移交下段二进制等待消息消化
  });
on_tcp_message(Bytes, State = #agent{socket = Socket}) ->
  case prim_inet:async_recv(Socket, 0, -1) of
    {?ok, Ref} -> {?noreply, State#agent{ref = Ref, bytes = Bytes}};
    Error -> {?stop, Error, State}
  end.


%% @doc 递归确认请求
on_tcp_request(ProtoId, Bytes, State = #agent{
  bytes = Next,
  session = #session{}
}) ->

  % todo: 后续消息处理
  NewSession = State#agent.session,

  % 递归调用
  on_tcp_message(Next, State#agent{session = NewSession}).

