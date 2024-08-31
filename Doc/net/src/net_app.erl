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
-export([
  on_established/1,
  on_message/2,
  on_closed/1
]).

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
  Cores = sys_utils:cpu_cores(),
  {?ok, _TcpPid} = tcp_listener:start_link(9377, [
    {?pool_size, Cores},
    {?socket_options, ?TCP_OPTS},
    {?on_established, ?MODULE, ?on_established},
    {?on_message, ?MODULE, ?on_message},
    {?on_closed, ?MODULE, ?on_closed}
  ]),
  {?ok, proc_utils:proc_id()}.

%% @doc 应用退出回调
stop(_State) ->
  ?ok.

%% @doc 会话初始化
on_established(State = #agent{}) ->
  io:format("Established = ~w~n", [State]),
  State.

%% @doc 进程关闭
on_closed(State = #agent{}) ->
  io:format("Closed = ~w~n", [State]),
  ?skip.


%% @doc 消息回调
on_message(<<Length:?u32_t, Protocol:?u32_t, Bytes:Length/?bytes_t, Next/?bytes_t>>, State = #agent{
  session = #session{}
}) ->
  io:format("Session Request(L: ~w, Id: ~w) = ~w", [Length, Protocol, Bytes]),

  on_request(Protocol, Bytes, State#agent{
    bytes = Next % 移交下段二进制等待递归清空
  });
on_message(Bytes, State = #agent{socket = Socket}) ->
  case prim_inet:async_recv(Socket, 0, -1) of
    {?ok, Ref} -> {?noreply, State#agent{ref = Ref, bytes = Bytes}};
    Error -> {?stop, Error, State}
  end.


%% @doc 递归确认请求
on_request(ProtoId, Bytes, State = #agent{
  bytes = Next,
  session = #session{}
}) ->

  % todo: 后续消息处理
  NewSession = State#agent.session,

  % 递归调用
  on_message(Next, State#agent{session = NewSession}).

