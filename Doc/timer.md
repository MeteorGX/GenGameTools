# 定时器处理

游戏当中会涉及到很多需要定时任务模块, 包括但不限于:

- 服务端心跳返回
- 游戏活动定时检测
- 场景服务广播
- 内存数据定时落地
- 其他定时任务......

而 `Erlang` 当中定时器是有两种分类的, 在其他游戏服务端概念也有类似定时器概念:

- `全局定时器`: 全局被共享的定时器服务, 支持跨进程唤醒.
- `进程定时器`: 仅在进程当前定时器服务, 本地进程独享不会跨进程唤起.

全局定时器的创建调用:

```plain
%% timer 模块, 构建时候会自动生成 gen_server 管理
Timer = timer:apply_interval(
    erlang:round(timer:seconds(0.5)),
    唤起模块,
    唤起函数,
    [] %% 传递参数
),

%% 取消定时器
timer:cancel(Timer).
```

而进程定时器则直接发出直接挂起发出信号通知:

```plain
MillSec = 1000, % 定时单位毫秒
erlang:start_timer(
    MillSec，
    erlang:self(), %% 进程Pid或者原子量标识, 如果是进程Pid则进程退出时候会自动取消, 原子则不会
    "Message" %% 消息内容, 也就是进程消息推送的内容, 也就是 Pid | Message 的 Message 内容
)
%% erlang:send_after 和 erlang:start_timer 功能类似
%% 推荐采用 start_timer 处理, 因为其返回的消息内容为 {timeout,TimerRef,Msg}, 而 send_after 只会返回 {Msg}, 这样没办法单独取消事件


%% 附着在 GenServer 上就需要自己去设置 info 监听事件
handle_info({timeout,TimerRef,Message},State)->
    %% 唤起代码逻辑
    erlang:cancel_timer(TimerRef), %% 取消定时器
    {noreply,State}.
```

> 这两者都是单次定时器所以需要额外编写持续执行的定时器逻辑

这两者都是定时处理, 区别在于是否共享进程数据, 除了这些之外还有更加关键差异就是 `性能效率`.

## 全局定时器

`timer` 是系统挂载单个进程做定时器 `GenServer`, 问题也就是出在多任务调度单进程情况;
在网络进程当中每个客户端会话都会开辟出新的进程做单独进程处理, 如果采用 `timer` 当玩家量过万的时候单进程的定时器调度会明显感受到卡顿延迟.

> 比如客户端有个定时收益结算(类似在线玩家增长按时长自动发放游戏货币这种功能),需要定时检查同时客户端GUI展示游戏货币资源,
> 这种情况就是每个客户端进程都统一定时调度唯一定时器, 单进程效率被严重拖累导致出现消息延迟.

所以在动态构建进程这种没办法明确指定控制进程数量的地方使用 `timer` 做定时器调度要十分谨慎, 尽可能避免这种多进程任务推送单进程定时器情况出现,
像 `定时游戏活动计时推送` 这种明确清晰知道任务进程数量可控的情况就十分适合全局定时器处理.

这里提供个全局定时器服务样例, `schedule_monitor` 为定时任务管理器, `schedule_factory` 就是具体执行服务。

`schedule_monitor` 定时服务管理器代码如下:

```erlang
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% 全局定时管理器
%%% @end
%%%-------------------------------------------------------------------
-module(schedule_monitor).
-author("MeteorCat").
-include("constant.hrl").
-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

%% Executor Module
-define(WORKER_MOD, schedule_factory).

-export([
  seed/0,
  interval_half/0,
  timeout_add/10,
  timeout_del/1,
  timeout_lists/0,
  interval_add/5,
  interval_del/1,
  interval_lists/0,
  register/2,
  unregister/2,
  time_after/4,
  time_after_cancel/1
]).


%%%===================================================================
%%% API functions
%%%===================================================================

%% @doc Starts the supervisor
-spec(start_link() -> {?ok, Pid :: pid()} | ?ignore | {?error, Reason :: term()}).
start_link() -> supervisor:start_link({?local, ?MODULE}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%% @private
%% @doc Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
-spec(init(Args :: term()) ->
  {?ok, {SupFlags :: {RestartStrategy :: supervisor:strategy(),
    MaxR :: non_neg_integer(), MaxT :: non_neg_integer()},
    [ChildSpec :: supervisor:child_spec()]}}
  | ?ignore | {?error, Reason :: term()}).
init([]) ->
  MaxRestarts = 65536,
  MaxSecondsBetweenRestarts = 64,
  SupFlags = #{
    strategy => one_for_one,
    intensity => MaxRestarts,
    period => MaxSecondsBetweenRestarts
  },

  Worker = #{
    id => ?WORKER_MOD,
    start => {?WORKER_MOD, start_link, []},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [?WORKER_MOD]},

  {?ok, {SupFlags, [Worker]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% 对外暴露接口 - Begin
%%%===================================================================

%% 获取时间种子
seed() ->
  case erlang:whereis(?WORKER_MOD) of
    Pid when erlang:is_pid(Pid) -> ?WORKER_MOD:seed();
    _ ->
      {M, S, Ms} = os:timestamp(),
      {M, S, Ms div 1000}
  end.


%%% @doc 多少秒后执行(Module,Function,Args)
%%% 调用的 M|F 必须是原子量, 不能动态传入匿名函数
%%% @end
time_after(AfterTime, Module, Function, Args) ->
  TaskID = erlang:make_ref(),
  Limit = 1,  % 执行上限次数  1:执行一次就消失
  ?WORKER_MOD:interval_add(TaskID, AfterTime, Limit, Module, Function, Args),
  TaskID.

%% @doc 取消任务
time_after_cancel(TaskId) -> ?WORKER_MOD:interval_del(TaskId).


%% @doc 注册一个时间
register(Pid, Time) -> ?WORKER_MOD:register_cast(Pid, Time).


%% @doc 撤销注册一个时间
unregister(Pid, Time) -> ?WORKER_MOD:unregister_cast(Pid, Time).


%% @doc 列出当前任务
timeout_lists() -> ?WORKER_MOD:timeout_lists().


%% @doc 添加一个任务
timeout_add(TaskID, Module, Function, Args, Sec, Min, Hour, Day, Month, Week) ->
  Limit = 0, % 执行上限次数  0:没上限
  ?WORKER_MOD:timeout_add(TaskID, Module, Function, Args, Sec, Min, Hour, Day, Month, Week, Limit).


%% @doc 删除一个任务(参数要与添加任务时提供的参数相同)
timeout_del(TaskId) -> ?WORKER_MOD:timeout_del(TaskId).


%% @doc 列出当前任务
interval_lists() -> ?WORKER_MOD:interval_lists().


%% @doc 添加一个任务
interval_add(TaskID, Module, Function, Args, Interval) ->
  Limit = 0, % 执行上限次数  0:没上限
  ?WORKER_MOD:interval_add(TaskID, Module, Function, Args, Interval, Limit).

%% @doc 删除一个任务
interval_del(TaskId) -> ?WORKER_MOD:interval_del(TaskId).

%% @doc 定时回调(半秒), Erlang定时器准时回调触发
interval_half() -> ?WORKER_MOD:interval_half_cast().


%%%===================================================================
%%% 对外暴露接口 - End
%%%===================================================================
```

`schedule_factory` 定时执行服务如下:

```erlang
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc 定时器
%%% @end
%%%-------------------------------------------------------------------
-module(schedule_factory).
-include("constant.hrl").
-behaviour(gen_server).

-export([start_link/0]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).
-export([
  seed/0,
  register_cast/2,
  unregister_cast/2,
  timeout_lists/0,
  timeout_add/11,
  timeout_del/1,
  interval_lists/0,
  interval_del/1,
  interval_half_cast/0,
  interval_add/6
]).

% 时间戳定义
-type timestamp() :: non_neg_integer().


%% 延时任务
-record(task_timeout, {
  id,
  sec, min, hour, day, month, week,
  mod,
  func,
  args,
  limit,
  last_exec_time :: timestamp()
}).


%% 间隔任务
-record(task_interval, {
  id,
  interval,
  mod,
  func,
  args,
  limit,
  last_exec_time :: timestamp()
}).


%% @doc 定时状态
-record(schedule_state, {
  timer = ?null, % 定时器
  half = 0, % 半秒周期计数
  total = 0, % 开始定时器到现在秒数
  task_half = [] :: [#task_interval{}],
  task_interval = [] :: [#task_interval{}],
  task_timeout = [] :: [#task_timeout{}]
}).


%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

-spec start_link() -> gen_server:start_ret().
start_link() -> gen_server:start_link({?local, ?MODULE}, ?MODULE, [], []).

%% @doc 初始化定时器
init([]) ->
  sys_utils:set_trap_exit(),

  %% 获取目前启动的时间种子
  {M, S, Ms} = os:timestamp(),
  erlang:put(seed, {M, S, Ms div 1}),

  %% 设置定时器0.5s进行调用,
  %% 调用对象为: schedule_factory:interval_half([])
  %% 推荐采用全局 0.5s 矫正时间准确一些
  {?ok, Timer} = timer:apply_interval(
    erlang:round(timer:seconds(0.5)),
    schedule_monitor,
    interval_half,
    []
  ),
  State = #schedule_state{
    timer = Timer
  },
  {?ok, State}.




terminate(_Reason, _State = #schedule_state{timer = Timer}) ->
  timer:cancel(Timer),
  ?ok.

code_change(_OldVsn, State = #schedule_state{}, _Extra) ->
  {?ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%%===================================================================
%%% 工具函数 - Begin
%%%===================================================================


%% @doc 时间检查
check_time(Task, {_Y, M, D}, {H, I, S}, Week) ->
  CheckList = [{Task#task_timeout.sec, S}, {Task#task_timeout.min, I}, {Task#task_timeout.hour, H},
    {Task#task_timeout.day, D}, {Task#task_timeout.month, M}, {Task#task_timeout.week, Week}],
  check_time(CheckList).
check_time([]) -> ?true;
check_time([H | T]) ->
  case check_time2(H) of
    ?true -> check_time(T);
    ?false -> ?false
  end.
check_time2({[], _NowTime}) -> ?true;
check_time2({TaskTime, NowTime}) -> lists:member(NowTime, TaskTime).


%% @doc 追加任务
task_record({Id, {Mod, Func, Args}, 0.5, Limit}, State) ->
  task_record({Id, {Mod, Func, Args}, 0.5, Limit}, State);
task_record({Id, {Mod, Func, Args}, Interval, Limit}, State) ->
  % 间隔任务
  Task = #task_interval{
    id = Id,
    interval = Interval,
    mod = Mod, func = Func, args = Args,
    limit = Limit,
    last_exec_time = 0
  },
  % 追加到任务队列之中
  io:format("Add Interval Task = ~w~n", [Task]),
  TaskInterval = [Task | State#schedule_state.task_interval],
  State#schedule_state{task_interval = TaskInterval};
task_record({Id, {Mod, Func, Args}, Sec, Min, Hour, Day, Month, Week, Limit}, State) ->
  % 定时任务, 参照 crontab 格式
  Task = #task_timeout{
    id = Id,
    sec = Sec, min = Min, hour = Hour,
    day = Day, month = Month, week = Week,
    mod = Mod, func = Func, args = Args,
    limit = Limit,
    last_exec_time = 0
  },
  io:format("Add Timeout Task = ~w~n", [Task]),
  TaskTimeout = [Task | State#schedule_state.task_timeout],
  State#schedule_state{task_timeout = TaskTimeout};
task_record(_Data, State) ->
  State.


%%% @doc 最核心的定时器调回执行入口
%%% 扫描任务列表, 执行可执行的任务
%%% @end
task_run_half(State = #schedule_state{
  task_half = TaskHalf,
  half = Half,
  total = Total
}) ->

  % 遍历执行目前需要执行的间隔任务, 间隔任务比较特殊 0.5s 就要检查一次
  NewHalf = (Half + 1) rem 2,
  {NewTaskHalf, _Total} = lists:foldl(
    fun task_run_half/2,
    {[], Total},
    TaskHalf %% 确认任务列表
  ),

  % 因为定时任务比较简单, 只需要判断1s就启动
  case NewHalf of
    0 -> % 整合秒数正好到达1s, 可以取出任务执行
      task_run(State#schedule_state{task_half = NewTaskHalf, half = NewHalf});
    _ -> % 目前仅仅 0.5 没办法满足任务需求
      State#schedule_state{task_half = NewTaskHalf, half = NewHalf}
  end.


%% @doc 确认执行间隔任务
%% @return {[],0}
task_run_half(TaskHalf, {Interval, Total}) when erlang:is_record(TaskHalf, task_interval) ->
  task_run_interval(TaskHalf, Interval, Total);
task_run_half(_Data, Res) ->
  Res.


%% @doc 执行间隔任务
task_run_interval(TaskHalf = #task_interval{
  mod = Mod,
  func = Func,
  args = Args,
  limit = Limit
}, Interval, Total) when erlang:is_record(TaskHalf, task_interval) ->
  % 启动进程执行
  _Pid = erlang:spawn(
    Mod, Func, Args
  ),
  case Limit of
    0 ->
      NewTaskHalf = TaskHalf#task_interval{last_exec_time = Total},
      {[NewTaskHalf | Interval], Total};
    Value when 1 >= Value ->
      {Interval, Total};
    _ ->
      NewLimit = Limit - 1,
      NewTaskHalf = TaskHalf#task_interval{last_exec_time = Total, limit = NewLimit},
      {[NewTaskHalf | Interval], Total}
  end.
task_run_interval(TaskHalf = #task_interval{
  interval = Interval
}, {ExecInterval, Total}) when erlang:is_record(TaskHalf, task_interval) ->
  case Total rem Interval of
    0 -> task_run_interval(TaskHalf, ExecInterval, Total);
    _ -> {[TaskHalf | ExecInterval], Total}
  end;
task_run_interval(_Data, Res) -> Res.


%% @doc 执行定时任务
task_run_timeout(TaskHalf = #task_timeout{
  mod = Mod,
  func = Func,
  args = Args,
  limit = Limit
}, {Timeout, Total, Date, Time, Week}) when erlang:is_record(TaskHalf, task_timeout) ->
  %% 检查是否到达时间
  case check_time(TaskHalf, Date, Time, Week) of
    ?true ->
      % 启动进程执行
      _Pid = erlang:spawn(
        Mod, Func, Args
      ),
      case Limit of
        0 -> % 到达执行时间
          NewTaskHalf = TaskHalf#task_timeout{last_exec_time = Total},
          {[NewTaskHalf | Timeout], Total, Date, Time, Week};
        Value when 1 >= Value -> %% 时间还没到, 不需要执行
          {Timeout, Total, Date, Time, Week};
        _ ->
          NewLimit = Limit + 1,
          NewTaskHalf = TaskHalf#task_timeout{last_exec_time = Total, limit = NewLimit},
          {[NewTaskHalf | Timeout], Total, Date, Time, Week}
      end;
    _ -> % 没有任务
      {[TaskHalf | Timeout], Total, Date, Time, Week}
  end;
task_run_timeout(_Data, Rs) -> Rs.


%% @doc 执行定时任务
task_run(State = #schedule_state{
  total = Total,
  task_interval = TaskInterval,
  task_timeout = TaskTimeout
}) ->

  % 遍历执行目前需要执行的定时任务
  NewTotal = Total + 1,
  {NewTaskInterval, _} = lists:foldl(
    fun task_run_interval/2,
    {[], NewTotal},
    TaskInterval
  ),

  %% 获取本地时间, 确认定时任务
  {Date, Time} = datetime_utils:datetime(),
  Week = calendar:day_of_the_week(Date),
  {NewTaskTimeout, _, _, _, _} = lists:foldl(
    fun task_run_timeout/2,
    {[], NewTotal, Date, Time, Week},
    TaskTimeout
  ),


  %% 至此定时任务已经完成一轮, 等待下次执行
  State#schedule_state{
    total = NewTotal,
    task_interval = NewTaskInterval,
    task_timeout = NewTaskTimeout
  }.


%%%===================================================================
%%% 工具函数 - End
%%%===================================================================


%%%===================================================================
%%% info回调 - Begin
%%%===================================================================

%% @doc 定时器唤起回调
handle_info({?loop, Time}, State) ->
  case get({timer, Time}) of
    ?undefined -> ?ok;

    % 获取当前定时器任务
    PidList ->
      % 对所有绑定注册 PID 对象响应 loop
      % 发送 gen_server:handle_info(doloop,State) 事件
      case [begin Pid ! ?loop, Pid end || Pid <- PidList,
        begin case erlang:is_pid(Pid) of
                ?true -> erlang:is_process_alive(Pid);
                ?false ->
                  case erlang:whereis(Pid) of
                    ?undefined ->
                      ?false;
                    PID ->
                      erlang:is_process_alive(PID)
                  end
              end
        end] of
        [] -> erlang:erase({timer, Time}); % 没有定时器删除掉进程字典
        NewPidList -> % 有新的注册者直接等待下一次回调
          erlang:send_after(Time, erlang:self(), {?loop, Time}),
          erlang:put({timer, Time}, NewPidList)
      end
  end,
  {?noreply, State};


%% @doc 外部调用
handle_info({?exec, Mod, Fun, Args}, State) ->
  NewState = Mod:Fun(State, Args),
  {?noreply, NewState};

%% @doc 默认回调
handle_info(_Info, State = #schedule_state{}) ->
  {?noreply, State}.


%%%===================================================================
%%% info回调 - End
%%%===================================================================


%%%===================================================================
%%% call回调 - Begin
%%%===================================================================

%% @doc 获取时间种子
handle_call(rand_seed, _From, State) ->
  {M1, S1, Ms1} = erlang:get(seed),
  M2 = (M1 * 171) rem 30269,
  S2 = (S1 * 172) rem 30307,
  Ms2 = (Ms1 * 170) rem 30323,
  erlang:put(seed, {M2, S2, Ms2}),
  {?reply, {M1, S1, Ms1}, State};

%% @doc 获取定时任务列表
handle_call(timeout_lists, _From, State = #schedule_state{task_timeout = Reply}) ->
  {?reply, Reply, State};

%% @doc 获取间隔任务列表
handle_call(interval_lists, _From, State = #schedule_state{task_interval = Reply}) ->
  {?reply, Reply, State};

%% @doc 默认处理
handle_call(_Request, _From, State = #schedule_state{}) ->
  {?reply, ?ok, State}.


%%%===================================================================
%%% call回调 - End
%%%===================================================================


%%%===================================================================
%%% cast回调 - Begin
%%%===================================================================


%% @doc 注册任务
handle_cast({register, Pid, Time}, State) ->
  case erlang:get({timer, Time}) of
    ?undefined ->
      % 未定义的时候在指定时候推送 gen_server:do_info({?loop,Time},..) 唤醒
      erlang:put({timer, Time}, [Pid]),
      erlang:send_after(Time, proc_utils:proc_id(), {?loop, Time});
    L ->
      case lists:member(Pid, L) of
        ?true -> ?skip;
        ?false -> erlang:put({timer, Time}, [Pid | L])
      end
  end,
  {?noreply, State};

%% @doc 删除任务
handle_cast({unregister, Pid, Time}, State) ->
  case erlang:get({timer, Time}) of
    ?undefined ->
      ?skip;
    L ->
      case lists:member(Pid, L) of
        ?true -> erlang:put({timer, Time}, lists:delete(Pid, L));
        ?false -> ?skip
      end
  end,
  {?noreply, State};


%% @doc 新增定时任务
handle_cast({
  timeout_add,
  TaskId, Mod, Func, Args, Sec, Min, Hour, Day, Monty, Week, Limit
}, State = #schedule_state{}) ->
  NewState = task_record({
    TaskId, {Mod, Func, Args}, Sec, Min, Hour, Day, Monty, Week, Limit
  }, State),
  {?noreply, NewState};


%% @doc 删除定时任务
handle_cast({timeout_del, TaskId}, State = #schedule_state{task_timeout = TaskTimeout}) ->
  %% 遍历取出目前定时任务队列
  Task = lists:foldl(fun(Data, InnerTask) when erlang:is_record(Data, task_timeout) andalso Data#task_timeout.id /= TaskId ->
    [Data | InnerTask];
    (_Data, InnerTask) -> InnerTask end, [], TaskTimeout),
  {?noreply, State#schedule_state{task_timeout = Task}};


%% @doc 追加间隔任务
handle_cast({
  interval_add,
  TaskId, Mod, Func, Args, Interval, Limit
}, State = #schedule_state{}) ->
  NewState = task_record({
    TaskId, {Mod, Func, Args}, Interval, Limit
  }, State),
  {?noreply, NewState};


%% @doc 删除间隔任务
handle_cast({interval_del, TaskId}, State = #schedule_state{task_interval = TaskInterval}) ->
  %% 遍历取出目前间隔任务队列
  Task = lists:foldl(fun(Data, InnerTask) when erlang:is_record(Data, task_interval) andalso Data#task_interval.id /= TaskId ->
    [Data | InnerTask];
    (_Data, InnerTask) -> InnerTask end, [], TaskInterval),
  {?noreply, State#schedule_state{task_interval = Task}};


%% @doc 被定时器唤醒任务, 核心关键调用
handle_cast(?schedule_half, State) ->
  {?noreply, task_run_half(State)};


%% @doc 默认处理
handle_cast(_Request, State = #schedule_state{}) ->
  {?noreply, State}.

%%%===================================================================
%%% cast回调 - End
%%%===================================================================


%%%===================================================================
%%% 对外暴露函数 - Begin
%%%===================================================================

%% @doc 获取时间种子
seed() -> gen_server:call(?MODULE, ?rand_seed).

%% @doc 注册时间
register_cast(Pid, Time) -> gen_server:cast(?MODULE, {?register, Pid, Time}).

%% @doc 注销时间
unregister_cast(Pid, Time) -> gen_server:cast(?MODULE, {?unregister, Pid, Time}).

%% @doc 定时任务列表
timeout_lists() -> gen_server:call(?MODULE, ?timeout_lists, 1000).

%% @doc 添加定时任务
timeout_add(TaskId, Mod, Func, Args, Sec, Min, Hour, Day, Monty, Week, Limit) ->
  gen_server:cast(?MODULE, {
    ?timeout_add, TaskId, Mod, Func, Args, Sec, Min, Hour, Day, Monty, Week, Limit
  }).


%% @doc 移除延迟任务
timeout_del(TaskId) ->
  gen_server:cast(?MODULE, {?timeout_del, TaskId}).


%% @doc 间隔列表
interval_lists() -> gen_server:call(?MODULE, ?interval_lists, 1000).

%% @doc 添加间隔任务
interval_add(TaskId, Mod, Func, Args, Interval, Limit) ->
  gen_server:cast(?MODULE, {
    ?interval_add, TaskId, Mod, Func, Args, Interval, Limit
  }).


%% @doc 移除间隔任务
interval_del(TaskId) ->
  gen_server:cast(?MODULE, {?interval_del, TaskId}).


%% @doc 间隔回调,半秒定时
interval_half_cast() -> gen_server:cast(?MODULE, ?schedule_half).


%%%===================================================================
%%% 对外暴露函数 - End
%%%===================================================================
```

调用的方法如下, 实际上就是类似启动个 `GenServer`:

```shell
# %% 全局定时启动服务
schedule_monitor:start_link().

# %% 获取定时任务种子
schedule_monitor:seed().

# %% 构建单次定时任务, 参考 schedule_monitor 实现，返回 TimerRef 来句柄做取消定时器功能
# schedule_monitor:timeout_add(所需参数).

# %% 构建多次执行的任务, 参考 schedule_monitor 实现, 其他同上
# schedule_monitor:interval_add(所需参数).
```

全局定时器适合做固定任务的派发, 更加类似 `Linux` 当中 `crontab` 定时服务, 可以用来指定年月日时分秒启动的服务唤醒.

## 进程定时器

进程定时器是附加在本地进程上的定时器, 通知也是被唤醒到 `GenServer:handle_info` 之中,
因为内部定时进程都是本地进程唤醒, 所以性能相比全局多个进程共享单进程定时器有很明显提升, 特别在客户端会话量级到达一定程度的时候.








