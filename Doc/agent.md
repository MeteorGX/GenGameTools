# 玩家模块

如果之前学习过 `skynet` 或者其他相关的游戏框架, 那么就能了解玩家其实大部分时间都是处于在自己进程|线程 `自娱自乐` 的状态.

这里的 `Erlang` 游戏框架其实也是基于此实现, 在用户进行 `Socket` 访问之后会自动 `fork` 创建进程:

```erlang
%% 详情文件: apps/net/src/tcp_acceptor.erl

%% @doc 核心任务回调 - 异步返回
handle_info({inet_async, Listener, Ref, {?ok, Socket}}, State = #tcp_acceptor_state{
  listener = Listener, ref = Ref, closed = Closed, mod = Mod
}) ->
  % 确认是否关服, 如果关服移交到关闭服务
  case Closed of
    ?true -> gen_tcp:close(Socket);
    % 移交动态创建 agent
    ?false -> fork(Mod, Listener, Socket)
  end,
  each(Listener, State);


%% 子进程唤醒
fork(Mod, Listener, Socket) ->
  case supervisor:start_child(Mod, [Listener, Socket]) of
    {?ok, ChildPid} ->
      proc_utils:proc_unlink(ChildPid),
      _ = lager:info("[created] fork ~w~n", [ChildPid]),
      {?ok, ChildPid};
    {?error, {already_started, ChildPid}} ->
      _ = lager:info("[already] fork ~w~n", [ChildPid]),
      proc_utils:proc_unlink(ChildPid),
      {?ok, ChildPid};
    {?error, Reason} ->
      {?error, Reason}
  end.
```

这里服务端创建的 `Erlang` 进程就是玩家的 `agent` 对象, 用于代理玩家所有游戏的操作.

> `Erlang` 进程概念不等于 `Linux` 进程概念, 两者不是一类东西注意别混淆, 后续进程都是代指 `Erlang` 进程概念.

可以看到这里创建 `agent` 其实也就是 `actor` 概念, 玩家大部分情况就是处于自己 `actor` 进程自己操作自己数据.

在没有 `多人对战|世界Boss|全局排行|场景切换` 等跨进程操作时候, 一般休闲小游戏单单靠自己进程 `自娱自乐` 就足够支撑内容了.
