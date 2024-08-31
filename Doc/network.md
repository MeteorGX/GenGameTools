# 网络库

客户端的网络库架构接触不多, 后续等找到资料再单独说明, 所以这里主要讲解的 `Erlang` 服务端网络架构.

> 这里项目初始化构建是采用这个代码库内部的 `GenRebar3Project.py` 构建, 所有代码操作都是基于此该初始化项目工具.

对于网络应用来说常规分为 `tcp|udp|kcp|websocket|http` 等做数据传输

- `tcp`: 泛用性比较广的网络协议, 好处就是不需要关注消息时序(`udp数据无序化|数据粘包`)就能协议对应消息交换
- `udp`: 基本上为围绕低延迟高性能的数据交换, 实际上应用基本上在 `FPS|动作|格斗|Moba` 等场景
- `kcp`: 结合比 `tcp` 多耗费带宽为代价降低极高延迟的 `udp` 网络协议, 但需要客户端也要对应做好处理(实现客户端协议)
- `websocket`: 轻度休闲 `H5` 游戏比较多选择的协议, 好处就是基于 `http` 在 `Web` 应用极广, 缺点性能在所有协议当中最低
- `http`: `http` 大部分不参与游戏服务端消息交换, 只集中于游戏管理 `GM` 命令如 `充值推送|游戏关服|推送邮件|封禁账号`

> 这里比较多接触 `tcp` 以此为主要说明, `udp` 相关协议在日常项目当中应用比较少, 所以等后续比较深入应用再展开

自定义的代码最好采用 `高内聚低耦合` 方式, 外部只需要传递参数(`端口|TCP设置|消息回调函数`), 剩下交由内部处理即可.

构建网络应用入口, 这里以构建出来的项目为根目录:

```shell
# 构建名为 fight 的游戏项目, 附带有工具库
python Rebar3Builder/GenRebar3Project.py -p fight -lib 
cd fight # 进入项目以此为根目录
```

后续就是构建网络应用的的 `app`:

```shell
mkdir -p apps/net/src # 源代码功能放置于此
touch apps/net/src/net.app.src #  app 信息文件
touch apps/net/src/net_app.erl # 应用入口 main 文件
```

这里首先编写处理应用启动相关信息内容 `net.app.src`:

```plain
{application, net, % 声明应用为 net, 可以被 application:start(net) 启动
 [{description, "Game Network Framework"},
  {vsn, "0.1.0"}, % 应用版本
  {registered, []},
  {mod, {net_app, []}}, % 启动引用文件, 要求同目录有  net_app.erl 文件
  {applications, % 依赖其他应用
   [kernel,
    stdlib
   ]},
  {env,[]},
  {modules, []},
  {licenses, ["MIT"]},
  {links, []}
 ]}.
```

这里相当于应用启动的配置文件, 用于引导 `net_app.erl` 文件来启动, 这里先编写入口驱动文件:

```erlang
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 网络服务的应用
%%% @end
%%%-------------------------------------------------------------------
-module(net_app).
-behaviour(application).

-export([start/2, stop/1]).

%% @doc 应用启动回调
start(_StartType, _StartArgs) ->
  {ok, self()}.

%% @doc 应用退出回调
stop(_State) ->
  ok.
```

这里就是相对比较精简的网络应用脚本, 目前只有启动和退出的回调, 后续扩展 `Socket` 内部方法用于 `会话请求回调|退出回调`.

## 构建准备

这里创建独立 `tcp` 目录做服务器监听处理:

```shell
mkdir apps/net/src/tcp # 创建 TCP 服务目录
touch apps/net/src/tcp/tcp_listener.erl # 监听服务, 实际上就是 supervisor 启动 gen_tcp:listen
touch apps/net/src/tcp/tcp_acceptor.erl # 多个进程监听调配服务, 实际上就是 gen_server 启动 gen_tcp:accept
touch apps/net/src/tcp/tcp_executor.erl # accept 之后在服务端动态创建 agent, 实际上为 supervisor 的 simple_one_for_one 动态 fork 进程
touch apps/net/src/tcp/tcp_worker.erl # 基于 tcp_executor 的 gen_server, 会话是被动态创建并挂载, 客户端在服务器当中会话对象
```

这里面的流程关系是这样:

1. 服务端启动 `tcp_listener` 监听服务
2. `tcp_listener` 内部启动 `gen_tcp:listen` 做监听并同时跟随启动 `tcp_acceptor|tcp_executor` 池
3. 客户端发起 `Socket` 请求, 首次请求会被 `tcp_acceptor` 池捕获
4. `tcp_acceptor` 池中拦截到监听到会话请求, 调用 `start_child` 来动态启动 `tcp_executor` 的子进程
5. `tcp_executor` 被唤醒启动 `tcp_worker` 客户端进程, 该进程用于客户端 `Socket` 挂载在服务端的代理进程
6. 游戏内部相关的业务就是基于自己请求所动态构建进程( `agent` ), 这里还有场景 `socket` 跨进程的问题暂且留着后续说明

这里还有 `Agent` 和 `session` 关键点:

- `Agent`: 用户放置客户端 `Session|Ref|缓冲区|进程id|客户端Socket|监听Socket`
- `Session`: 具体进程的会话信息

对于客户端建立会话后动态建立 `tcp_worker` 和客户端映射对象, 这种就是 `Agent`:

```shell
mkdir -p apps/net/include # 头文件目录
touch apps/net/include/agent.hrl # 会话记录
```

`Agent` 结构体头文件的最终内容如下:

```erlang
-include("constant.hrl"). % 引入功能头


-ifndef(__NET_HEADERS__).
-define(__NET_HEADERS__, 1).

%% 配置相关
-define(pool_size, tcp_pool_size). % 连接池数量
-define(socket_options, tcp_options). % Socket配置

%% 回调相关事件
-define(on_established, on_established). % 连接回调
-define(on_message, on_message). % 消息回调
-define(on_closed, on_closed). % 关闭回调


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 玩家在服务器挂载实体 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(session, {

  %% 账号基础信息 ---------------------------------------
  sid = 0 :: non_neg_integer(), % 服务器ID
  uid = 0 :: non_neg_integer(), % 用户id
  id = 0 :: non_neg_integer(), % 角色id
  version = 0 :: non_neg_integer(), % 游戏客户端版本
  proc_id :: pid(), % 进程ID
  socket :: port(), % Socket对象

  %% 游戏基础信息 ---------------------------------------
  %% 下面的属性大部分在MMORPG游戏用的比较多, 需要跨场景进行处理的时候需要用到
  %% 举例上次处于某个主城|野外公共地图, 需要发送给场景说明用户上线并进行广播监听|发送
  scene_id = 0 :: non_neg_integer(), % 关卡场景id, 简单游戏业务不需要记录进程

  %% 登录基础信息 ---------------------------------------
  create_at = 0 :: non_neg_integer(), % 会话创建时间 | 登录时间
  ip_address = "Unknown" :: string(), % 会话创建IP | 登录IP
  online = 0 :: non_neg_integer(), % 在线时长, 不计算本次登录

  status = [] :: list() %% 状态记录器, 记录进程当中状态信息
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 玩家在服务器挂载实体 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器代理 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(agent, {
  listen :: port(), %% 监听 Listener Socket 句柄
  socket :: port(), %% 客户端 Socket 会话句柄
  ref :: number(), %% Socket 依赖,Ref
  session :: #session{}, %% 会话
  on_established :: ?false | {?on_established, module()|atom(), atom()}, %% 初始化回调
  on_message :: ?false | {?on_message, module()|atom(), atom()}, %% 消息调用回调
  on_closed :: ?false | {?on_closed, module()|atom(), atom()}, %% 进程退出回调
  bytes = <<>> :: bitstring() %% 客户端请求的二进制数据流
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器代理 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-endif.
```

> 注: 这里的 `include` 目录需要在 `rebar.config` 文件追加 `{i, "apps/net/include"}` 在 `erl_opts` 之中

## 服务编写

将功能抽象成库提供给外部调用的时候比较考验开发者功能抽象能力, 网络功能库里尽量屏蔽底层实现让外部调用只需要关注以下功能:

- 初始化函数, 在动态创建客户端进程时候初始化回调, 类似其他语言的 `on_init|on_established`
- 消息回调, 客户端消息传递进来时候回调 `Hook` 给调用者处理, 类似其他语言的 `on_update|on_message`
- 进程退出, 客户端关闭会话(Socket退出等) 处理异常|正常退出回调, 类似其他语言的 `on_exit|on_close`

这些功能基本上都是暴露给调用者来处理, 只需要暴露以上 `API` 的话调用者就不用管内部的实现.

> 注: 后续的网络库需要有一定 `Erlang` 的功底才能处理, 至少理解内部处理方法

首先就是 `tcp_listener.erl`, 作为基础的启动 `Supervisor` 负责接收外部全部参数:

```erlang
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% TCP监听服务, 启动 TCP 服务来接受服务器监听
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_listener).
-behaviour(supervisor).
-include("agent.hrl").

%% @doc 导出方法
-export([start_link/2, init/1]).

%% @doc 启动函数
-spec start_link(Port, Opts) -> supervisor:startlink_ret()
  when Port :: inet:port_number(),
  Opts :: list().
start_link(Port, Opts) ->
  supervisor:start_link({?local, ?MODULE}, ?MODULE, [Port, Opts]).


%% @doc 初始化函数
init([Port, Opts]) ->
  sys_utils:set_trap_exit(), % 传递进程退出回调
  {?pool_size, SocketPoolSize} = lists:keyfind(?pool_size, 1, Opts),
  {?socket_options, SocketOpts} = lists:keyfind(?socket_options, 1, Opts),
  case gen_tcp:listen(Port, SocketOpts) of % 启动 TCP 监听
    {?ok, Listener} ->
      % 启动动态分配管理器
      Executor = #{
        id => tcp_executor,
        start => {tcp_executor, ?start_link, [tcp_worker, Opts]},
        restart => ?permanent,
        shutdown => ?brutal_kill, % 外部管理器可以无条件终止
        type => ?supervisor,
        modules => [tcp_executor]
      },


      % 创建多个 Accept 监听调配
      CoresLists = lists:seq(1, SocketPoolSize),
      Acceptors = lists:map(fun(Element) ->
        Id = convert_utils:to_atom(io_lib:format("tcp_acceptor_~w", [Element])),
        #{
          id => Id,
          % 传递给 Acceptor 启动 参数信息
          start => {tcp_acceptor, ?start_link, [Id, Listener, tcp_executor]},
          restart => ?permanent,
          shutdown => ?brutal_kill,  % 外部管理器可以无条件终止
          type => ?worker,
          modules => [tcp_acceptor]
        } end, CoresLists),


      %% 启动管理器参数
      MaxRestarts = 100,
      MaxSecondsBetweenRestarts = 6,
      SupFlags = #{
        strategy => ?one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
      },
      {?ok, {SupFlags, [Executor | Acceptors]}};
    Error -> Error % 异常错误
  end.
```

这里就是简单启动服务, 用来驱动后续的 `tcp_acceptor` 和 `tcp_executor` 处理客户端的 `Socket` 链接.

现在构建 `tcp_acceptor.erl`, 用于监听端口处理会话:

```erlang
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Accept池, 接受客户端会话并且分配给指定子进程
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_acceptor).
-behaviour(gen_server).
-include("constant.hrl").


-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Accept 运转状态
-record(tcp_acceptor_state, {
  name :: atom(), % 进程名
  proc_id :: pid(), % 进程id
  listener :: port(), % 监听对象
  ref :: number(), % Socket Ref
  mod :: atom() | module(), % 会话 fork 子进程模块名
  closed = ?false :: boolean() % 预留字段, 用于停服将 accept 切换为关闭状态
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%% @doc 启动函数
start_link(Name, Listener, Mod) -> gen_server:start_link({?local, Name}, ?MODULE, [Name, Listener, Mod], []).

%% @doc 初始化函数
init([Name, Listener, Mod]) ->
  sys_utils:set_trap_exit(),
  State = #tcp_acceptor_state{
    name = Name,
    proc_id = proc_utils:proc_id(),
    listener = Listener,
    mod = Mod,
    closed = ?true
  },

  % 注意这里是多个进程竞争状态, 异步方式移交给其他进程处理
  case prim_inet:async_accept(Listener, -1) of
    {?ok, Ref} ->
      NewState = State#tcp_acceptor_state{
        ref = Ref,
        closed = ?false
      },
      {?ok, NewState};
    Error -> {?stop, Error}
  end.


%%%===================================================================
%%% 下面的回调基本上没什么用, 保持默认即可
%%%===================================================================

handle_call(_Request, _From, State = #tcp_acceptor_state{}) ->
  {?reply, ?ok, State}.

handle_cast(_Request, State = #tcp_acceptor_state{}) ->
  {?noreply, State}.


terminate(_Reason, _State = #tcp_acceptor_state{}) ->
  ?ok.

code_change(_OldVsn, State = #tcp_acceptor_state{}, _Extra) ->
  {?ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% 状态变动指令 - Begin
%%%===================================================================

%% 子进程唤醒
fork(Mod, Listener, Socket) ->
  case supervisor:start_child(Mod, [Listener, Socket]) of
    {?ok, ChildPid} ->
      proc_utils:proc_unlink(ChildPid),
      {?ok, ChildPid};
    {?error, {already_started, ChildPid}} ->
      proc_utils:proc_unlink(ChildPid),
      {?ok, ChildPid};
    {?error, Reason} ->
      {?error, Reason}
  end.

%% @doc 异步 accept
async_accept(Listener, State = #tcp_acceptor_state{}) ->
  case prim_inet:async_accept(Listener, -1) of
    {?ok, Ref} ->
      {?noreply, State#tcp_acceptor_state{ref = Ref}};
    Error -> {?stop, Error, State}
  end.

%% @doc 核心任务回调 - 异步返回
handle_info({inet_async, Listener, Ref, {?ok, Socket}}, State = #tcp_acceptor_state{
  listener = Listener, ref = Ref, closed = Closed, mod = Mod
}) ->
  % 确认是否关服, 如果关服移交到关闭服务
  case Closed of
    ?true -> % 关服直接拒绝连接
      gen_tcp:close(Socket),
      async_accept(Listener, State);
    ?false ->  % 移交动态创建 agent
      fork(Mod, Listener, Socket),
      async_accept(Listener, State)
  end;


%% @doc 用户主动退出 - 异步返回
handle_info({inet_async, Listener, Ref, {?error, closed}}, State = #tcp_acceptor_state{
  listener = Listener, ref = Ref
}) -> {?stop, ?normal, State};


%% @doc 未知中断 - 异步返回
handle_info({inet_async, Listener, Ref, {?error, _Error}}, State = #tcp_acceptor_state{
  listener = Listener, ref = Ref
}) -> {?stop, ?normal, State};


%% @doc 系统异常退出
handle_info({'EXIT', _Pid, _Reason}, State) ->
  {?noreply, State};


%% @doc 其他进程需要回调
handle_info({?exec, Mod, Fun, Args}, State) ->
  NewState = Mod:Fun(Args, State),
  {?noreply, NewState};

%% @doc 默认处理
handle_info(_Info, State = #tcp_acceptor_state{}) ->
  {?noreply, State}.

%%%===================================================================
%%% 状态变动指令 - End
%%%===================================================================
```

这里面当有 `Socket` 访问的时候都会被 `accept` 捕获从而触发后续的子进程动态构建.

之后就是构建动态进程模块, 用于映射客户端和本地进程操作, 首先是 `tcp_executor.erl`:

```erlang
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 动态构建客户端进程管理器
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_executor).
-behaviour(supervisor).
-include("constant.hrl").

-export([start_link/2, init/1]).

%% @doc 启动服务
-spec start_link(Mod, Opts) -> supervisor:startlink_ret()
  when Mod :: module()|atom(),
  Opts :: list().
start_link(Mod, Opts) ->
  supervisor:start_link({?local, ?MODULE}, ?MODULE, [Mod, Opts]).

%% @doc 初始化
init([Mod, Opts]) ->
  sys_utils:set_trap_exit(),
  Worker = #{
    id => Mod,
    start => {Mod, ?start_link, [Opts]},
    restart => ?transient,
    shutdown => 2000,
    type => ?worker,
    modules => [Mod]
  },

  % 启动标识, 客户端进程错误应该不需要太多重启, 直接 Socket 断线让客户端重连即可
  MaxRestart = 4, % 最多重启4次即可
  MaxSecondsBetweenRestarts = 1,
  SupFlags = #{
    strategy => ?simple_one_for_one, % 动态构建 worker 服务
    intensity => MaxRestart,
    period => MaxSecondsBetweenRestarts
  },
  {?ok, {SupFlags, [Worker]}}.
```

这里是作为动态进程管理的 `Supervisor`, 最后就是编写好服务 `tcp_worker.erl`:

```erlang
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
```

至此就完成网络库的封装, 之后就是怎么调用启动并处理消息的问题, 这里初始化编写服务 `net_app.erl`:

```erlang
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
```

这里启动监听端口 `9377` 的 `TCP` 服务, 通过 `Erlang` 的 `Shell` 执行命令 `application:start(net).` 启动服务.

这里可以先不处理消息来访问处理下确认联通会话, 利用 `netcat` 工具:

```shell
# 模拟连接, nc 或者 netcat 命令皆可
nc 127.0.0.1 9377

# 最后 Erlang 的 Shell 会输出以下内容:
# Established = {agent,#Port<0.6>,#Port<0.7>,0,{session,0,0,0,0,<0.326.0>,0,#Port<0.7>,0,1725123970,[49,50,55,46,48,46,48,46,49],0,[]},{on_established,net_app,on_established},{on_message,net_app,on_message},{on_closed,net_app,on_closed},<<>>}
# Closed      = {agent,#Port<0.6>,#Port<0.7>,0,{session,0,0,0,0,<0.326.0>,0,#Port<0.7>,0,1725123970,[49,50,55,46,48,46,48,46,49],2,[]},{on_established,net_app,on_established},{on_message,net_app,on_message},{on_closed,net_app,on_closed},<<>>}
```

这里会发现 `on_message` 有部分回调没被调用, 这时候需要编写客户端来进行测试处理, 后续另开章节编写客户端.

> 因为网络负担的功能比较多且 `Session|Agent` 变动可能过大，所以没办法简单封装成单一库.

具体项目样例单独提取样例: [网络库简单封装](net)











