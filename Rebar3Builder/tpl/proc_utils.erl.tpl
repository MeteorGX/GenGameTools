%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 进程工具
%%% 功能包装, 有的IDE没办法识别 erlang 系列函数需要自己重新包装
%%% @end
%%%-------------------------------------------------------------------
-module(proc_utils).
-author("MeteorCat").
-include("constant.hrl").

%% API
-export([
  proc_id/0,
  proc_alive/1,
  proc_send/2,
  proc_find/1,
  proc_link/1,
  proc_unlink/1,
  proc_proxy/2,
  sleep/1,
  sleep/2
]).

%% @doc 获取当前 Erlang 进程ID
-spec proc_id() -> pid().
proc_id() -> erlang:self().


%% @doc 确认进程存活
-spec proc_alive(Pid) -> boolean() when Pid :: pid().
proc_alive(Pid) -> erlang:is_process_alive(Pid).


%% @doc 进程推送投递
proc_send(Pid, Msg) when erlang:is_pid(Pid) ->
  Pid ! Msg,
  ?true;
proc_send(RegName, Msg) when erlang:is_atom(RegName) ->
  case erlang:whereis(RegName) of
    Pid when erlang:is_pid(Pid) ->
      Pid ! Msg,
      ?true;
    ?undefined -> ?false
  end;
proc_send(?undefined, _Msg) -> ?false;
proc_send(?null, _Msg) -> ?false;
proc_send(_, _Msg) -> ?false.


%% @doc 查找进程
-spec proc_find(term()) -> ?undefined | pid().
proc_find(RegName) -> erlang:whereis(RegName).


%% @doc 进程退出, 发出退出信号
-spec proc_unlink(pid()) -> term().
proc_unlink(Pid) -> erlang:unlink(Pid).


%% @doc 标识进程连接
-spec proc_link(pid()) -> term().
proc_link(Pid) -> erlang:link(Pid).


%% @doc 进程代理执行, 会推送 handle_info({?exec,M,F,A}) 信号
proc_proxy(Pid, {Mod, Func, Args}) when erlang:is_pid(Pid) ->
  proc_send(Pid, {?exec, Mod, Func, Args});
proc_proxy(RegName, {Mod, Func, Args}) when erlang:is_atom(RegName) ->
  case erlang:whereis(RegName) of
    Pid when erlang:is_pid(Pid) ->
      proc_proxy(Pid, {Mod, Func, Args});
    ?undefined -> ?false
  end;
proc_proxy(_, _) ->
  ?false.


%% @doc 休眠等待
-spec sleep(MillSec) -> boolean() when MillSec :: integer().
sleep(MillSec) ->
  receive after MillSec -> ?true end.


%% @doc 休眠等待之后执行匿名函数
sleep(MillSec, F) when erlang:is_function(F, 0) ->
  receive after MillSec -> F() end;
sleep(MillSec, _F) ->
  sleep(MillSec).