%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_client_svr).
-include("constant.hrl").
-behaviour(gen_server).

-export([start_link/1]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


-record(client_svr_state, {
  socket
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

start_link(Port) -> gen_server:start_link({?local, ?MODULE}, ?MODULE, [Port], []).

init([Port]) ->
  case gen_tcp:connect("127.0.0.1", Port, [
    binary,
    {packet, 0},
    {active, true},
    {nodelay, true},
    {delay_send, false}
  ]) of
    {?ok, Socket} -> % 连接成功
      {?ok, #client_svr_state{
        socket = Socket
      }};
    Reason -> Reason
  end.


handle_call(_Request, _From, State = #client_svr_state{}) ->
  {?reply, ?ok, State}.


terminate(_Reason, _State = #client_svr_state{}) ->
  ?ok.

code_change(_OldVsn, State = #client_svr_state{}, _Extra) ->
  {?ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% 模拟客户端接收 - Begin
%%%===================================================================

%% @doc 消息获取
handle_info({tcp, _Socket, Bytes}, State = #client_svr_state{}) ->
  io:format("Net Message ~w~n", [Bytes]),
  handle_message(Bytes, State);

%% @doc Socket断开
handle_info({tcp_closed, _Socket}, _ = #client_svr_state{}) ->
  io:format("Socket Closed ~n"),
  {?stop, "Scoket Closed"};

%% @doc 默认处理
handle_info(Info, State = #client_svr_state{}) ->
  io:format("Unknown Message ~w~n", [Info]),
  {?noreply, State}.

%% @doc 消息拦截
handle_message(<<Size:?u32_t, Proto:?u32_t, Message:Size/?bytes_t>>, State = #client_svr_state{}) ->
  io:format("Protocol = ~w, Size = ~w, Data = ~w~n", [Proto, Size, Message]),
  {?noreply, State};
handle_message(_Bytes, State = #client_svr_state{}) ->
  {?noreply, State}.

%%%===================================================================
%%% 模拟客户端接收 - End
%%%===================================================================


%%%===================================================================
%%% 模拟客户端推送 - Begin
%%%===================================================================


handle_cast({raw, Bytes}, State = #client_svr_state{socket = Socket}) ->
  sys_utils:send_bytes(Socket, Bytes),
  {?noreply, State};

%% @doc 默认回调
handle_cast(_Request, State = #client_svr_state{}) ->
  {?noreply, State}.


%%%===================================================================
%%% 模拟客户端推送 - End
%%%===================================================================



