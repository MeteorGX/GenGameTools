%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 日期时间工具
%%% @end
%%%-------------------------------------------------------------------
-module(datetime_utils).
-author("MeteorCat").
-include("constant.hrl").

%% API
-export([
  second/0,
  millisecond/0,
  microsecond/0,
  nanosecond/0,
  now/0,
  timestamp/0,
  timestamp_ms/0,
  date/0,
  time/0,
  week_number/0,
  week/0,
  datetime/0,
  date_ymd/0,
  date_yw/0
]).


%% @doc erlang:system_time(second) 别名
-spec second() -> non_neg_integer().
second() -> erlang:system_time(second).


%% @doc erlang:system_time(milli_seconds) 别名
-spec millisecond() -> non_neg_integer().
millisecond() -> erlang:system_time(millisecond).

%% @doc erlang:system_time(microsecond) 别名
-spec microsecond() -> non_neg_integer().
microsecond() -> erlang:system_time(microsecond).

%% @doc erlang:system_time(nanosecond) 别名
-spec nanosecond() -> non_neg_integer().
nanosecond() -> erlang:system_time(nanosecond).


%% @doc 获取时间信息, 老版本 erlang:now 新替代
-spec now() -> erlang:timestamp().
now() -> erlang:timestamp().


%%% @doc 获取时间戳, 秒级
%%% @end
-spec timestamp() -> non_neg_integer().
timestamp() -> second().

%%% @doc 获取时间戳, 毫秒级别
%%% @end
-spec timestamp_ms() -> non_neg_integer().
timestamp_ms() -> millisecond().

%% @doc 获取今年第几周
-spec week_number() -> non_neg_integer().
week_number() ->
  Datetime = ?MODULE:date(), %% 获取年月
  {_Y, WeekNum} = calendar:iso_week_number(Datetime),
  WeekNum.

%% @doc 获取这个星期周几: `1`: Monday, `2`: Tuesday
-spec week() -> non_neg_integer().
week() ->
  Datetime = ?MODULE:date(),
  calendar:day_of_the_week(Datetime).

%% @doc 获取当前年月日 {Y,M,D}
-spec date() -> {calendar:year1970(), calendar:month(), calendar:day()}.
date() ->
  {Date, _Time} = calendar:now_to_local_time(?MODULE:now()),
  Date.

%% @doc 获取当前时分秒 {H,m,s}
-spec time() -> {calendar:hour(), calendar:minute(), calendar:second()}.
time() ->
  {_Date, Time} = calendar:now_to_local_time(?MODULE:now()),
  Time.


%% @doc 获取当前年月日时分秒 {{Y,M,D},{H:I:S}}
-spec datetime() -> calendar:datetime1970().
datetime() -> calendar:now_to_local_time(?MODULE:now()).


%% @doc 获取年月日的 int 值
-spec date_ymd() -> non_neg_integer().
date_ymd() ->
  {Y, M, D} = ?MODULE:date(),
  Y * 10000 + M * 100 + D.


%% @doc 获取年月周的 int 值, 用于计算每周
-spec date_yw() -> non_neg_integer().
date_yw() ->
  Datetime = ?MODULE:date(), %% 获取年月
  {Y, WeekNum} = calendar:iso_week_number(Datetime),
  Y * 100 + WeekNum.