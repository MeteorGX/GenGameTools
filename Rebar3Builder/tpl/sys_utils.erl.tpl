%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 系统工具
%%% 系统内部需要用到函数方法
%%% @end
%%%-------------------------------------------------------------------
-module(sys_utils).
-author("MeteorCat").
-include("constant.hrl").

%% API
-export([
  cpu_cores/0,
  cpu_core_id/0,
  smp_enable/0,
  os_type/0,
  set_trap_exit/0,
  set_trap_exit/1,
  uniform/0,
  uniform/1,
  rand/2,
  rand_list/1,
  rand_str/1,
  rand_str/2,
  area_id/2,
  guid/2,
  send_bytes/2,
  send_flush/2,
  ip_address/1
]).

%% @doc 获取主机目前CPU核心线程, 用于进程分配调度
-spec cpu_cores() -> pos_integer().
cpu_cores() ->
  % erlang:get|put 是 erlang 进程维护的内存表进程会把一起连带数据销毁
  % 所以可以在运行期间可以作为进程当中的临时变量做处理
  case erlang:get(schedulers) of
    ?undefined ->
      Cores = erlang:system_info(schedulers),
      erlang:put(schedulers, Cores),
      Cores;
    Cores -> Cores
  end.


%% @doc 获取 Erlang 目前运行在CPU哪个核心线程
-spec cpu_core_id() -> pos_integer().
cpu_core_id() ->
  case erlang:get(scheduler_id) of
    ?undefined ->
      CoreIdx = erlang:system_info(scheduler_id),
      erlang:put(scheduler_id, CoreIdx),
      CoreIdx;
    CoreIdx -> CoreIdx
  end.

%% @doc 确认SMP是否启动
-spec smp_enable() -> boolean().
smp_enable() ->
  case erlang:get(smp_support) of
    ?undefined ->
      EnableSmp = erlang:system_info(smp_support),
      erlang:put(smp_support, EnableSmp),
      EnableSmp;
    EnableSmp -> EnableSmp
  end.


%% @doc 获取系统类型
-spec os_type() -> term().
os_type() ->
  case erlang:get(os_type) of
    ?undefined ->
      OsType = erlang:system_info(os_type),
      erlang:put(os_type, OsType),
      OsType;
    OsType -> OsType
  end.

%% @doc 设置崩溃拦截
-spec set_trap_exit(?true | ?false) -> term().
set_trap_exit(Flag) -> erlang:process_flag(?trap_exit, Flag).
set_trap_exit() -> erlang:process_flag(?trap_exit, ?true).


%% @doc 替代系统核心随机种子
uniform(N) when erlang:is_integer(N), N >= 1 ->
  erlang:trunc(?MODULE:uniform() * N) + 1.
uniform() ->
  {L1, L2, L3} = case get(random_seed) of
                   ?undefined -> os:timestamp();
                   Tuple -> Tuple
                 end,
  R1 = (L1 * 171) rem 30269,
  R2 = (L2 * 172) rem 30307,
  R3 = (L3 * 170) rem 30323,
  erlang:put(random_seed, {R1, R2, R3}),
  R = L1 / 30269 + L2 / 30307 + L3 / 30323,
  R - erlang:trunc(R).


%% @doc 产生介于Min到Max之间的随机整数
rand(Same, Same) -> Same;
rand(N1, N2) ->
  {Min, Max} = if N1 >= N2 -> {N2, N1};
                 ?true -> {N1, N2}
               end,
  %% 如果没有种子，将从核心服务器中去获取一个种子，以保证不同进程都可取得不同的种子
  M = Min - 1,
  ?MODULE:uniform(Max - M) + M.


%% @doc 从列表随机抽取元素
rand_list([]) -> ?null;
rand_list(List) ->
  Len = length(List),
  Idx = ?MODULE:uniform(Len),
  lists:nth(Idx, List).

%% @doc 生成指定长度的随机字符串
rand_str(Len) ->
  rand_str(Len, []).
rand_str(0, Chars) -> Chars;
rand_str(Len, Chars) ->
  Value = 97 + ?MODULE:uniform(25),
  rand_str(Len - 1, [Value | Chars]).


%% @doc 合并渠道+区服的唯一区域Id, 方便后续合服
%% MajorId 渠道不超过255个, 也就是 0xff
%% MinorId 区服不超过65535, 也就是 0xffff
area_id(MajorId, MinorId) when
  erlang:is_number(MajorId) andalso erlang:is_number(MinorId)
    andalso MajorId =< 255 andalso MinorId =< 65535 ->
  ((MajorId band 255) bsl 16) bor (MinorId band 65535);
area_id(_, _) -> ?unsupported.


%% @doc 生成全局唯一64位id
%% MajorId 渠道ID, 占据 10 bit
%% MinorId 服务器ID, 占 12 bit
%% 时间戳混合占据 41 bit
%% ((millisecond / 1000 + second + 1000) << (12 + 10)) + (MinorId << 10) + MajorId
%% 转化得出: (MS div 1000 + S * 1000) bsl (12+10) + (3(服务器ID) bsl 100) + 1(渠道ID)
%% 公式计算: ((erlang:system_time(millisecond) div 1000 + erlang:system_time(seconds) * 1000) bsl (12+10)) + (10001 bsl 10) + 1.
%% 可以测试下高并发的时候生成唯一标识碰撞概率
guid(MajorId, MinorId) when
  erlang:is_number(MajorId) andalso erlang:is_number(MinorId)
    andalso MajorId =< 255 andalso MinorId =< 65535 ->
  MS = erlang:system_time(millisecond),
  S = erlang:system_time(seconds),
  ((MS div 1000 + S * 1000) bsl (12 + 10)) + (MinorId bsl 10) + MajorId.


%% @doc 推送信息, 优先采用的数推送方法
send_bytes(_Socket, <<>>) -> ?true;
send_bytes(Socket, Bytes) when erlang:is_port(Socket) ->
  case send_flush(Socket, Bytes) of
    ?ok -> ?true;
    {?error, _} -> ?false
  end;
send_bytes(Pid, Bytes) when erlang:is_pid(Pid) ->
  % 进程推送 {send_bytes,bytes} 信号
  proc_utils:proc_send(Pid, {send_bytes, Bytes});
send_bytes(_, _) -> ?false.


%% @doc 数据优化推送
-spec send_flush(port(), bitstring()) -> ?ok|{?error, Reason}
  when Reason :: ?closed | {?timeout, RestData} | inet:posix(),
  RestData :: binary() | erlang:iovec().
send_flush(Socket, Bytes) ->
  <<_Len:?u32_t, _Protocol:?u32_t, _Message/?bytes_t>> = Bytes,
  %% !!! 注意这里是有版本BUG
  %% otp-27 erts-15: 新版本少了2两个位
  %% Use erlang:port_command(Socket, <<0,0,0,1>>), but only receive <<0,1>>.
  %% otp-25 erts-13.2.2.8: 旧版本正常工作
  %% Use erlang:port_command(Socket, <<0,0,0,1>>), receive <<0,0,0,1>>.
  %% Issues: https://github.com/erlang/otp/issues/8635
  %try erlang:port_command(Socket, Bytes, [force, nosuspend]) of
  try gen_tcp:send(Socket, Bytes) of
    ?ok -> ?ok;
    {?error, Reason} -> {?error, Reason}
  catch Err:Reason ->
    io:format("Failed by SendFlush[~w]:~p~n", [Err, Reason]),
    {?error, einval}
  end.


%% @doc 获取Socket的IP地址
-spec ip_address(port()) -> list().
ip_address(Socket) ->
  case inet:peername(Socket) of
    {?ok, {IP, _}} ->
      IPS = [integer_to_list(X) || X <- erlang:tuple_to_list(IP)],
      if length(IPS) > 6 ->
        string:join(IPS, ":");
        ?true -> string:join(IPS, ".")
      end;
    _ -> "Unknown"
  end.