# ETS/Dict/State

这里对应以下 `Erlang` 概念:

- `ets`: `Erlang` 自带的内存数据库, 可全局进程共享数据
- `dict`: 进程字典数据, 只在当前进程做数据共享
- `state`: `gen_server` 作为状态机共享状态数据, 仅在服务迭代共享

这三种类型在 `Erlang` 当中各自职责不一样, 所以需要区分开来使用, 这里从最简单到最复杂说起.

## State

`Erlang` 的 `GenServer` 作为状态机服务, 内部具有最基础 `State` 状态定义:

```erlang
-behaviour(gen_server).

%% @doc 状态数据
-record(state, {}).

%% @doc 这里初始化状态
init(Args) ->
  {ok, #state{}}.
```

如果启动 `GenServer` 之后内部就是围绕 `State` 状态做递归修改, 最常见的就是网络请求 `State`:

```erlang
-record(agent, {
  listen :: port(), %% 监听句柄
  socket :: port(), %% 会话句柄
  ref :: number(), %% Ref
  session, %% 会话进程内容, 对应 session 的 record 对象
  bytes = <<>> :: bitstring(), %% 二进制数据流
  events = [] :: list() %% 进程事件
}).


-record(session, {

  %% 账号基础信息 ---------------------------------------
  sid = 0 :: non_neg_integer(), % 服务器ID
  uid = 0 :: non_neg_integer(), % 用户id
  id = 0 :: non_neg_integer(), % 角色id
  version = 0 :: non_neg_integer(), % 游戏客户端版本
  proc_id :: pid(), % 进程ID
  socket :: port(), % Socket对象
  permission :: boolean(), % 是否授权允许登录

  %% 时间信息     ---------------------------------------
  create_time = 0 :: non_neg_integer(), % 会话创建时间 | 登录时间
  create_ip = "Unknown" :: string(), % 会话创建IP | 登录IP
  online = 0 :: non_neg_integer(), % 在线时长, 不计算本次登录
  heartbeat_time = 0 :: non_neg_integer(), % 上次心跳时间, 按照当前心跳时间比较上次心跳时间防止心跳过快

  logs = [] :: list() % 最新10条请求协议
}).
```

网络内部记录好 `Listen(监听Socket句柄)` 和 `Socket(客户端连接Socket)`, 还有更加重要的 `Session`;
这里就是被 `GenServer` 不断迭代共享:

```erlang
%% @doc 常规回调, 诸如类似不断递归回调
handle_info(_Request, State = #agent{}) ->
  {noreply, State}.
```

> `GenServer` 被进程系统共享, 内部的 `record` 结构最好不要牵涉太多游戏逻辑.

常规来说 `State(Session)` 基础记录数据:

- `Listen`: 监听者 `Acceptor` 句柄
- `Socket`: 客户端监听 `Socket` 句柄, 用户进程推送客户端
- `Uid`: 第三方用户标识Id
- `Sid`: 玩家登录服务器Id
- `Id`: 玩家在该服务器玩家Id
- `Ver`: 玩家登录游戏版本
- `ProcId`: 客户端 `Fork` 进程Id, 用于全局通知
- `CreateTime`: 客户端访问时候时间戳, 单位:秒
- `CreateIp`: 客户端来源IP地址
- `Online`: 客户端在线时长, 用于客户端退出时候追加上次在线时间更新写入的总在线时长

这是游戏服务必须要记录的状态, 可以看出这些数据特性: `不可变性`

> 注意: 后续需要做 ETS 处理将会话 {Id,Socket,ProcId } 做全局记录, 就能构建全局会话表用于统计在线人数等功能, 后续会说明

可以看到这就是 `State` 的状态机所用的状态数据, 这里的数据是进程+服务独有.

## Dict

当启动 `Erlang` 进程的时候, 内部会初始化内存数据字典, 被称为 `process dictionary`.

进程字典的操作也十分简单:

```erlang
%% @doc 进程字典写入哈希
put_dict(Key, Value) ->
  erlang:put(Key, Value).

%% @doc 获取进程字段内容
get_dict(Key) ->
  erlang:get(Key).

%% @doc 获取当前进程所有 key
get_dicts() ->
  erlang:get_keys().
```

注意: `进程字典挂载数据是跟随进程, 也就是当进程被释放退出的时候, 数据也会被直接清空.`

进程字典是 `Erlang` 当中数据读写交换最快的方式, 也是玩家验证登录之后将玩家数据挂载内存当中处理方式.

具体进程字典使用最多方式之一:

1. 用户验证登录
2. 数据库取出 `record` 结构数据行
3. 将数据 `record` 实体写入在进程字典当中
4. 启动进程定时器定时将 `record` 落地到数据库
5. 玩家数据 `CURD` 操作都是对进程字段的 `record` 处理而不进行数据库操作
6. Socket进程中断的时候会将 `record` 最后数据落地到数据库

对于游戏来说, 大部分玩家是作为 `Agent` 挂载在服务器当中对着自己内存进程字典数据做 `自娱自乐` 处理, 这实际上就是游戏服务器的本质.

> 对于简单休闲游戏来说, 这些就足够处理完整的游戏相关基础逻辑( 前提不含全服排行|多场景变换|战斗服务这类需要跨进程模块 )

## ETS

`Erlang` 自带的基于内存 `KV-Table` 的数据中心, 用来支持大数据量存储和高效读写.

这里看起来和 `Dict` 差不多, 但是 `ETS` 具有跨进程特性让其允许在进程之中做数据交换.

这个特性在游戏当中应用十分广泛, 比如以下场景:

网络进程授权完成挂载后在 `ETS` 写入 `玩家ID` 和 `进程ID`, 其他进程可以利用 `ETS` 推送跨进程数据

> `跨进程` 交换数据就是 `ETS` 最主要功能, 用于全局做数据交换.

可以想想 `标识统计玩家在线` 功能怎么设计, 因为网络进程客户端都是 `fork` 出来的,
无论 `State` 和 `Dict` 都没办法满足全局记录在线标识.

而 `Dict/State` 和 `ETS` 看起来都能起到内存临时数据处理功能, 那需要怎么选择两者?

注意: 除非需要跨线程处理的情况, 否则优先选择速度效率最快 `Dict`.

因为对于 `ETS` 来说数据获取是重新拷贝新数据来操作, 如果数据使用频繁会疯狂从 `ETS` 拷贝内存数据;
而进程字典则是直接挂载当前进程的数据结构体, 获取对象是当前进程内存数据而不会出现数据拷贝问题.

并且字典数据会自动随着进程退出而销毁, 而 `ETS` 需要手动处理删除, 另外因为 `Dict` 是进程级别所以不具有锁结构速度也会相应比较快.

而 `State` 和另外两者功能完全不一致, 是作为用于保存系统进程数据的情况, 一般是不暴露给游戏业务处理, 总结起来:

- `State`: 保存系统进程信息, 保存 `Socket/外部Config/ProcId` 等系统相关数据
- `Dict`: 用于数据库内存实体映射, 用于读取操作的异步写入落地到数据库
- `ETS`: 跨进程交换数据, 用于全局数据处理标识, 全局数据库暴露给所有 `application|gen_server` 等






























