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

## 构建服务

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

```erlang
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器代理 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(agent, {
  listen :: port(), %% 监听 Listener Socket 句柄
  socket :: port(), %% 客户端 Socket 会话句柄
  ref :: number(), %% Socket 依赖,Ref
  session :: #session{}, %% 会话结构对象, 后续有定义
  bytes = <<>> :: bitstring(), %% 客户端请求的二进制数据流
  mod :: module() | atom() | pid(), %% 回调过来的模块|进程
  func :: atom() %% 回调唤起函数
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器代理 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

内部记录了客户端对应信息, 除了 `session` 不清楚其他都基本知道起到什么作用.

> Agent 内部定型之后基本很少会变动内部数据( session 和 bytes 除外 )

之后就是会话 `session` 对象, 记录进程当中系统数据:

```erlang
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
  scene_id = 0 :: non_neg_integer(), % 关卡场景id, 简单游戏业务不需要记录进程
  socket :: port(), % Socket对象
  status = 0 :: non_neg_integer(), %% 会话状态, 0 代表未授权, 其他值扩展出其他状态

  %% 登录相关
  create_at = 0 :: non_neg_integer(), % 会话创建时间 | 登录时间
  ip_address = "Unknown" :: string(), % 会话创建IP | 登录IP
  online = 0 :: non_neg_integer(), % 在线时长, 不计算本次登录

  %% 上次心跳时间, 按照当前心跳时间比较上次心跳时间防止心跳过快, 过快代表异常直接中断会话
  % heartbeat_time = 0 :: non_neg_integer(),

  logs = [] :: list() % 最新10条请求协议, 用于记录调试
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 玩家在服务器挂载实体 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

> 内部可以记录其他跨进程信息, 一些不那么重要的系统数据可以缓存放置在 session 结构之中

暂时确定好之后就可以编写头信息用 `record` 机构记录:

```shell
mkdir -p apps/net/include # 头文件记录
touch apps/net/include/agent.hrl # 会话记录
```

结构体头文件的最终内容如下:

```erlang
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器代理 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(agent, {
  listen :: port(), %% 监听 Listener Socket 句柄
  socket :: port(), %% 客户端 Socket 会话句柄
  ref :: number(), %% Socket 依赖,Ref
  session :: #session{}, %% 会话
  bytes = <<>> :: bitstring(), %% 客户端请求的二进制数据流
  mod :: module() | atom() | pid(), %% 回调过来的模块|进程
  func :: atom() %% 回调唤起函数
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器代理 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


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
  scene_id = 0 :: non_neg_integer(), % 关卡场景id, 简单游戏业务不需要记录进程
  socket :: port(), % Socket对象
  status = 0 :: non_neg_integer(), %% 会话状态, 0 代表未授权, 其他值扩展出其他状态

  %% 登录相关
  create_at = 0 :: non_neg_integer(), % 会话创建时间 | 登录时间
  ip_address = "Unknown" :: string(), % 会话创建IP | 登录IP
  online = 0 :: non_neg_integer(), % 在线时长, 不计算本次登录

  %% 上次心跳时间, 按照当前心跳时间比较上次心跳时间防止心跳过快, 过快代表异常直接中断会话
  % heartbeat_time = 0 :: non_neg_integer(),

  logs = [] :: list() % 最新10条请求协议, 用于记录调试
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 玩家在服务器挂载实体 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
```

> 注: 如果是编写系统库尽可能对外的结构要将类型声明好, 避免传递一些奇奇怪怪的数据被内部调用出问题








