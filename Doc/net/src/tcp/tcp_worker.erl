%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 客户端进程服务, 独立映射的会化进程
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_worker).
-behaviour(gen_server).
-include("agent.hrl").

-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).


%% @doc 客户端重新设置Socket配置
-define(SOCKET_OPTS, [
  active,
  nodelay,
  keepalive,
  delay_send,
  priority,
  tos
]).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%% @doc 启动服务, 注意这里采用 start_link(?MODULE,...) 才能被全局 fork处理
-spec start_link(Callbacks, Listener, Socket) -> gen_server:start_ret()
  when Listener :: port(),
  Socket :: port(),
  Callbacks :: list().
start_link(Callbacks, Listener, Socket) -> gen_server:start_link(?MODULE, [
  Callbacks, Listener, Socket
], []).


%% @doc 将监听Socket设置客户端Socket
set_socket_opts(Listener, Socket, Opts) when erlang:is_port(Listener) andalso erlang:is_port(Socket) ->
  case inet_db:lookup_socket(Listener) of
    {?ok, Mod} ->
      case inet_db:register_socket(Socket, Mod) of
        ?true -> case prim_inet:getopts(Listener, Opts) of
                   {?ok, TcpOpts} -> case prim_inet:setopts(Socket, TcpOpts) of
                                       ?ok -> {?ok, TcpOpts};
                                       Error -> gen_tcp:close(Socket), Error
                                     end;
                   Error -> gen_tcp:close(Socket), Error
                 end;
        ?false -> {?error, ?badarg}
      end;
    {?error, Reason} -> {?error, Reason}
  end;
set_socket_opts(_, _, _) ->
  {?error, ?badarg}.


%% @doc 异步回调响应数据
async_recv(Socket, Length) when erlang:is_port(Socket) ->
  case prim_inet:async_recv(Socket, Length, -1) of
    {?ok, Ref} -> {?ok, Ref};
    Error -> Error
  end;
async_recv(_Socket, _Length) ->
  {?error, ?unknown}.




%% @doc 初始化函数
init([Callbacks, Listener, Socket]) ->
  sys_utils:set_trap_exit(),

  % 检索内部回调
  OnEstablished = lists:keyfind(?on_established, 1, Callbacks),
  OnMessage = lists:keyfind(?on_message, 1, Callbacks),
  OnClosed = lists:keyfind(?on_closed, 1, Callbacks),


  % 移交给 Agent 监听处理
  OwnerPid = proc_utils:proc_id(),
  gen_tcp:controlling_process(Socket, OwnerPid),
  case set_socket_opts(Listener, Socket, ?SOCKET_OPTS) of
    {?ok, _} ->
      case async_recv(Socket, 0) of
        {?ok, Ref} ->
          Agent = #agent{

            %% 写入 Agent 所需信息
            listen = Listener,
            ref = Ref,
            bytes = <<>>,
            socket = Socket,

            %% 协议回调
            on_established = OnEstablished,
            on_message = OnMessage,
            on_closed = OnClosed,

            %% 写入初始化登录信息
            session = #session{
              proc_id = OwnerPid,
              socket = Socket,
              status = 0,
              create_at = datetime_utils:timestamp(),
              ip_address = sys_utils:ip_address(Socket)
            }
          },

          % 确认初始化回调
          case OnEstablished of
            {?on_established, M, F} ->
              case M:F(Agent) of
                {?ok, NewAgent} when erlang:is_record(NewAgent, agent) ->
                  {?ok, NewAgent};
                NewAgent when erlang:is_record(NewAgent, agent) ->
                  {?ok, NewAgent};
                _ -> {?stop, ?on_established}
              end;
            _ -> {?ok, Agent}
          end;
        Error -> {?stop, Error}
      end;
    {?error, Reason} ->
      {?stop, Reason}
  end.


handle_call(_Request, _From, State = #agent{}) ->
  {?reply, ?ok, State}.

handle_cast(_Request, State = #agent{}) ->
  {?noreply, State}.

code_change(_OldVsn, State = #agent{}, _Extra) ->
  {?ok, State}.


%% @doc 异常退出
terminate(_Reason, State = #agent{
  on_closed = OnClose,
  session = #session{
    create_at = CreateAt,
    online = Online
  }
}) ->

  %% 计算在线时间
  Now = datetime_utils:timestamp(),
  NewState = State#agent{
    session = State#agent.session#session{
      online = (Now - CreateAt) + Online
    }
  },

  %% 推送退出信号, 判断是否为 Socket, 如果是就退出
  case erlang:is_port(NewState#agent.socket) of
    ?true -> try gen_tcp:close(NewState#agent.socket) catch
               _:_ -> ?ignore
             end;
    _ -> ?ignore
  end,

  %% 回调退出方法
  case OnClose of
    {?on_closed, M, F} ->
      M:F(NewState),
      ?ignore;
    _ -> ?ignore
  end,

  % 休眠下等消息推送
  proc_utils:sleep(500),
  ?ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% 工具方法 - End
%%%===================================================================


%% @doc 异步消息请求
do_info({inet_async, _, Ref, {?ok, Message}}, State = #agent{
  on_message = OnMessage,
  bytes = Bytes
}) -> % 将传递过来的消息合并在目前消息解构尾部
  NewState = State#agent{
    ref = Ref
  },
  case OnMessage of
    {?on_message, M, F} -> % 转发回调
      M:F(<<Bytes/?bytes_t, Message/?bytes_t>>, NewState);
    _ -> % 其他响应跳过
      {?noreply, NewState}
  end;


%% @doc 错误码返回客户端
do_info({?exit, Id, Type, Code}, State = #agent{socket = Socket})
  when erlang:is_number(Code) andalso erlang:is_number(Type) ->
  Message = <<Type:?u32_t, Code:?u32_t>>,
  sys_utils:send_bytes(Socket, byte_utils:encode_bytes(Id, Message)),
  {?stop, ?normal, State};


%% @doc 异步关闭连接请求
do_info({inet_async, _Reason, _, {?error, ?closed}}, State = #agent{}) ->
  {?stop, ?normal, State};


%% @doc 异步连接超时请求
do_info({inet_async, _, _, {?error, etimedout}}, State = #agent{}) ->
  {?stop, ?normal, State};


%% @doc 默认异步处理
do_info({inet_async, _, _, _Reason}, State = #agent{}) ->
  {?stop, ?normal, State};


%% @doc 进程异常回调
do_info({'EXIT', _Pid, Reason}, State = #agent{}) ->
  {?stop, Reason, State};


%% @doc 被其他进程调用
do_info({?exec, Mod, Fun, Args}, State = #agent{}) ->
  case Mod:Fun(State, Args) of
    NewState when erlang:is_record(NewState, agent) -> {?noreply, NewState};
    _ -> {?noreply, State}
  end;


%% @doc 退出
do_info(?exit, State = #agent{}) ->
  {?stop, ?normal, State};

%% @doc 默认回调
do_info(_Info, State = #agent{}) ->
  {?noreply, State}.

%% @doc 默认回调
handle_info(Info, State) ->
  try do_info(Info, State) catch
    Error:Reason ->
      io:format("[HandleInfoError(Throw: ~w - ~w)] Info: ~w, State ~w", [Error, Reason, Info, State]),
      {?stop, Reason, State}
  end.


%%%===================================================================
%%% 消息请求回调 - End
%%%===================================================================