# !/usr/bin/python
# -*- coding: UTF-8 -*-
# ===================================================
# Build Rebar3 Project | 构建 Rebar3 项目工程
# ===================================================
import argparse
import os
import re
import shutil
import subprocess
import time
import urllib.request
from datetime import datetime

FILE_DIR = os.path.dirname(os.path.realpath(__file__))
ROOT_DIR = os.path.abspath(os.path.join(FILE_DIR, "../"))

REBAR_CDN = "https://s3.amazonaws.com/rebar3/rebar3"
REBAR_EXE = "rebar3"
REBAR_DOC = "https://rebar3.org/docs/getting-started"
ERL_SCRIPT = "escript.exe" if os.name == "nt" else "escript"
ERL_EXE = "werl.exe" if os.name == "nt" else "erl"
BUILD_YMD = datetime.now().strftime("%Y%m%d")


# rewrite rebar.config
def rewrite_rebar_config(project_dir, project_name):
    rebar_config = os.path.join(project_dir, "rebar.config")
    if not os.path.exists(rebar_config):
        raise Exception("Config Non Exists")

    # 改写模板
    template = r"""
{minimum_otp_vsn, "27.0"}. % OTP最低版本|OTP Version
{root_dir, "."}. % 根目录 | Root Path
{base_dir, "_build"}. % 编译目录 | Build Path
{deps_dir, "lib"}. % 第三方库目录 | Library Build Path

%% 启动配置
{erl_opts, [
    {platform_define, "__VERSION__", gen_rebar3_date}, % 项目启动时间 | Create Datetime 
    {i, "include"}, % 应用头文件 | Global Headers
    
    % 系统其他配置 | Erlang Options
    warn_keywords,
    debug_info
]}.

%% 发布编译配置
{relx, [{release, {__PROJECT__, "__VERSION__"},
    [
        sasl,
        crypto,
        utils,
        __PROJECT__
    ]},
    
    {mode, dev},
    
    %% automatically picked up if the files
    %% exist but can be set manually, which
    %% is required if the names aren't exactly
    %% sys.config and vm.args
    {sys_config, "./config/sys.config"},
    {vm_args, "./config/vm.args"},
    
    %% the .src form of the configuration files do
    %% not require setting RELX_REPLACE_OS_VARS
    %% {sys_config_src, "./config/sys.config.src"},
    %% {vm_args_src, "./config/vm.args.src"}
    
    {extended_start_script, true}
]}.

%% @doc 打包设置 | Package Settings
{profiles, [

    %% 测试包|Test Package
    %% 打包|Package: rebar3 as dev release
    %% 压缩包|Compress: rebar3 as dev tar
    {prod, [{relx, [
        {dev_mode, true},
        {include_erts, false},
        {include_src, true},
        {debug_info, true}
    ]}]},
    
    %% 正式包|Release
    {prod, [{relx, [
        {dev_mode, false},
        {include_erts, true},
        {include_src, false},
        {debug_info, strip}]}
    ]}
]}.

%% 命令行调用|Shell Run
%% 命令行调用 VM 需要配置环境变量: ERL_FLAGS = -args_file config/vm.args
{shell, [
  {config, "config/sys.config"},
  {apps, [
    sasl,
    crypto,
    utils,
    __PROJECT__ 
  ]}
]}.


%% CDN
{rebar_packages_cdn, "https://hexpm.upyun.com"}.
    """.replace("__VERSION__", BUILD_YMD).replace("__PROJECT__", project_name)
    with open(rebar_config, "w", encoding="utf-8") as f:
        f.write(template)


# rewrite erlang.config
def rewrite_app_config(project_dir, project_name):
    sys_config = os.path.join(project_dir, "config", "sys.config")
    vm_args = os.path.join(project_dir, "config", "vm.args")
    if not os.path.exists(sys_config) or not os.path.exists(vm_args):
        raise Exception("AppConfig Non Exists")

    # 写入 sys.config
    template = r"""
[
    {__PROJECT___app, []}
].
""".replace("__PROJECT__", project_name)
    with open(sys_config, "w", encoding="utf-8") as f:
        f.write(template)

    # 写入 vm.args
    template = r"""
-sname __PROJECT___1 # 启动分布式, 多服就按照 __PROJECT___1,__PROJECT___2,... 分服, node() 查看节点
-setcookie __PROJECT___cookie # 节点会话标识
-smp enable # 支持SMP调度

+K true # 启用异步池
+pc unicode # 启用多语言支持
+P 102400 # 最大进程数
+A 30 # 异步池数量
+e 102400 # ETS最大数量
    """.replace("__PROJECT__", project_name)
    with open(vm_args, "w", encoding="utf-8") as f:
        f.write(template)


# rewrite constant header
def rewrite_constant_header(project_dir, project_name):
    project_keyboard = str.upper(project_name)
    project_header_dir = os.path.join(project_dir, "include")
    project_header_file = os.path.join(project_header_dir, "constant.hrl")
    if not os.path.exists(project_header_dir):
        os.mkdir(project_header_dir)

    # 写入 constant.hrl
    template = (r"""
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc 
%%% 全局常量
%%% @end
%%%-------------------------------------------------------------------
-author("MeteorCat").

-ifndef(__CONSTANT_NAME__).
-define(__CONSTANT_NAME__, __CONSTANT_VER__).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  系统相关定义 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(supervisor, supervisor).
-define(worker, worker).
-define(start_link, start_link).
-define(one_for_one, one_for_one).
-define(one_for_all, one_for_all).
-define(rest_for_one, rest_for_one).
-define(simple_one_for_one, simple_one_for_one).
-define(permanent, permanent).
-define(transient, transient).
-define(temporary, temporary).
-define(brutal_kill, brutal_kill).
-define(infinity, infinity).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  系统相关定义 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  系统原子量定义 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(true, true).
-define(false, false).
-define(null, null).
-define(ok, ok).
-define(error, error).
-define(exit, exit).
-define(exec, exec).
-define(done, done).
-define(stop, stop).
-define(skip, skip).
-define(continue, continue).
-define(normal, normal).
-define(takeover, takeover).
-define(failover, failover).
-define(undefined, undefined).
-define(ignore, ignore).
-define(trap_exit, trap_exit).
-define(local, local).
-define(hibernate, hibernate).
-define(reply, reply).
-define(noreply, noreply).
-define(shutdown, shutdown).
-define(inet_reply, inet_reply).
-define(loop, loop).
-define(unknown, unknown).
-define(unsupported, unsupported).
-define(register, register).
-define(unregister, unregister).
-define(closed, closed).
-define(badarg, badarg).
-define(timeout, timeout).
-define(interval, interval).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  系统原子量定义 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  定义类型, 参考 Rust 的定义 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-define(nan, nan). %% 不是数值类型
-define(bool, boolean). %% 布尔值
-define(i8, i8). %% 8位带符号整型
-define(u8, u8). %% 8位无符号整型
-define(i16, i16). %% 16位带符号整型
-define(u16, u16). %% 16位无符号整型
-define(i32, i32). %% 32位带符号整型
-define(u32, u32). %% 32位无符号整型
-define(i64, i64). %% 64位带符号整型
-define(u64, u64). %% 64位无符号整型
-define(f64, f64). %% 双精度(64 位)浮点数
-define(str, str). %% 字符串

% 直接类型声明
-define(u8_t, 8 / big - integer - unsigned).
-define(u16_t, 16 / big - integer - unsigned).
-define(u32_t, 32 / big - integer - unsigned).
-define(u64_t, 64 / big - integer - unsigned).
-define(i8_t, 8 / big - integer - signed).
-define(i16_t, 16 / big - integer - signed).
-define(i32_t, 32 / big - integer - signed).
-define(i64_t, 64 / big - integer - signed).
-define(f32_t, 32 / big - float).
-define(f64_t, 64 / big - float).
-define(bytes_t, bytes).


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  定义类型 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%



%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  宏函数 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-define(IF(C, L, R), case (C) of ?true -> (L);?false -> (R) end).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%  宏函数 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


-endif.  
    """
                .replace("__CONSTANT_NAME__", "__CONSTANT_" + project_keyboard + "__")
                .replace("__CONSTANT_VER__", BUILD_YMD))
    with open(project_header_file, "w", encoding="utf-8") as f:
        f.write(template)


# rewrite convert utils
def rewrite_convert_erl(project_dir, _project_name):
    # 不存在目录直接创建
    project_apps_dir = os.path.join(project_dir, "apps")
    project_tools_dir = os.path.join(project_apps_dir, "utils", "src")
    if not os.path.isdir(project_tools_dir):
        os.makedirs(project_tools_dir)

    # convert_utils.erl
    template = r"""
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc 
%%% 转化方法
%%% @end
%%%-------------------------------------------------------------------
-module(convert_utils).
-author("MeteorCat").
-include("constant.hrl").


%% API
-export([
  list2atom/1,
  to_list/1,
  list2string/4,
  string2term/1,
  term2bitstring/1,
  bitstring2term/1,
  term2string/1,
  to_atom/1,
  to_binary/1,
  to_float/1,
  to_integer/1,
  to_tuple/1,
  term2binary/1,
  binary2term/1,
  binary2hex/1,
  list2hex/1
]).

%% @doc List转换Atom
-spec list2atom(list()) -> term().
list2atom(L) ->
  try erlang:list_to_existing_atom(L)
  catch _:_ -> erlang:list_to_atom(L) end.
  

%% @doc list 转换方法
to_list(V) when erlang:is_list(V) -> V;
to_list(V) when erlang:is_tuple(V) ->
  erlang:tuple_to_list(V);
to_list(V) when erlang:is_atom(V) ->
  erlang:atom_to_list(V);
to_list(V) when erlang:is_binary(V) ->
  erlang:binary_to_list(V);
to_list(V) when erlang:is_integer(V) ->
  erlang:integer_to_list(V);
to_list(V) when erlang:is_float(V) ->
  erlang:float_to_list(V);
to_list(_) -> [].


%% @doc 数组转字符串
%% H 附加在开头
%% M 夹在中间
%% T 附加在尾部
list2string([], _, _, _) -> [];
list2string([Head | Tail], H, M, T) ->
  list2string(Tail, H, M, T, Head ++ to_list(Head)).
list2string([], _, _, T, Str) -> Str ++ T;
list2string([Head | Tail], H, M, T, Str) ->
  list2string(Tail, H, M, T, Str ++ M ++ to_list(Head)).


%%% @doc term序列化, term转换为string格式
%%% [{a},1] => "[{a},1]"
term2string(Term) -> binary_to_list(term2bitstring(Term)).
string2term(Str) ->
  case erl_scan:string(Str ++ ".") of
    {?ok, Token, _} ->
      case erl_parse:parse_term(Token) of
        {?ok, Term} -> Term;
        _ -> ?null
      end;
    _ -> ?null
  end.
  
  
%%% @doc Term 序列化, term转换为bitstring格式
%%% [{a},1] => <<"[{a},1]">>
term2bitstring(Term) -> erlang:iolist_to_binary(io_lib:write(Term)).
bitstring2term(?undefined) -> ?undefined;
bitstring2term(BitStr) -> string2term(erlang:binary_to_list(BitStr)).

%%% @doc atom 转换方法
to_atom(V) when erlang:is_atom(V) -> V;
to_atom(V) when erlang:is_binary(V) ->
  list2atom(erlang:binary_to_list(V));
to_atom(V) when erlang:is_integer(V) ->
  list2atom(erlang:integer_to_list(V));
to_atom(V) when erlang:is_float(V) ->
  list2atom(erlang:float_to_list(V));
to_atom(V) when erlang:is_tuple(V) ->
  list2atom(erlang:tuple_to_list(V));
to_atom(V) when erlang:is_list(V) ->
  V2 = erlang:list_to_binary(V),
  V3 = erlang:binary_to_list(V2),
  list2atom(V3);
to_atom(_) -> list2atom("").


%%%  @doc binary 转换方法
to_binary(V) when erlang:is_binary(V) -> V;
to_binary(V) when erlang:is_atom(V) ->
  erlang:list_to_binary(erlang:atom_to_list(V));
to_binary(V) when erlang:is_list(V) ->
  erlang:list_to_binary(V);
to_binary(V) when erlang:is_integer(V) ->
  erlang:list_to_binary(integer_to_list(V));
to_binary(V) when erlang:is_float(V) ->
  erlang:list_to_binary(float_to_list(V));
to_binary(_) -> <<>>.


%%% float 转化方法
to_float(V) -> list_to_float(?MODULE:to_list(V)).


%%% integer 转化方法
to_integer(V) when erlang:is_integer(V) -> V;
to_integer(V) when erlang:is_binary(V) ->
  V2 = erlang:binary_to_list(V),
  erlang:list_to_integer(V2);
to_integer(V) when erlang:is_list(V) ->
  erlang:list_to_integer(V);
to_integer(_) -> 0.


%% @doc tuple 转化方法
to_tuple(T) when erlang:is_tuple(T) -> T;
to_tuple(T) when erlang:is_list(T) ->
  erlang:list_to_tuple(T);
to_tuple(T) -> {T}.


%% @doc 结构体序列化
-spec term2binary(term()) -> erlang:ext_binary().
term2binary(Term) -> erlang:term_to_binary(Term).

%% @doc 结构体反序列化
-spec binary2term(erlang:ext_binary()) -> term().
binary2term(Bytes) -> erlang:binary_to_term(Bytes).

%% 二进制转16进制字符
-spec binary2hex(bitstring()) -> list().
binary2hex(Bin) ->
  List = binary_to_list(Bin),
  lists:flatten(list2hex(List)).

%% 列表转16进制字符
-spec list2hex(list()) -> list().
list2hex(L) ->
  lists:map(fun(X) -> int2hex(X) end, L).

%% Char值转16进制字符
int2hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].
hex(N) when N < 10 ->
  $0 + N;
hex(N) when N >= 10, N < 16 ->
  $a + (N - 10).


    """
    with open(os.path.join(project_tools_dir, "convert_utils.erl"), "w", encoding="utf-8") as f:
        f.write(template)

    # 最终写入应用版本内容
    utils_template = r"""
{application, utils,
  [{description, "Global System Utils"},
    {vsn, "__VERSION__"},
    {registered, []},
    {applications,
      [
        kernel,
        stdlib,
        crypto
      ]},
    {env, []},
    {modules, []},
    {licenses, ["Apache-2.0"]},
    {links, []}
  ]}.
  """.replace("__VERSION__", BUILD_YMD)
    with open(os.path.join(project_tools_dir, "utils.app.src"), "w", encoding="utf-8") as f:
        f.write(utils_template)


# rewrite tool utils
def rewrite_tools_lib(project_dir, _project_name):
    # 不存在目录直接创建
    project_apps_dir = os.path.join(project_dir, "apps")
    project_tools_dir = os.path.join(project_apps_dir, "utils", "src")
    if not os.path.isdir(project_tools_dir):
        os.makedirs(project_tools_dir)

    # 写入基础库工具 - proc
    proc_template = r"""
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
    """
    with open(os.path.join(project_tools_dir, "proc_utils.erl"), "w", encoding="utf-8") as f:
        f.write(proc_template)

    # 写入基础库工具 - sys
    sys_template = r"""
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
  guid/2
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
    """
    with open(os.path.join(project_tools_dir, "sys_utils.erl"), "w", encoding="utf-8") as f:
        f.write(sys_template)

    # 写入基础库工具 - datetime
    datetime_template = r"""
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
    """
    with open(os.path.join(project_tools_dir, "datetime_utils.erl"), "w", encoding="utf-8") as f:
        f.write(datetime_template)

    # 写入基础库工具 - byte
    byte_template = r"""
%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc 
%%% 二进制处理库
%%% @end
%%%-------------------------------------------------------------------
-module(byte_utils).
-author("MeteorCat").
-include("constant.hrl").

%% API
-export([
  decode_value/2,
  decode_bytes/2,
  encode_bytes/2,
  encode_list/1,
  encode_unicode_string/1,
  encode_tuples/1
]).

%% @doc 格式消息解码映射
decode_value(<<Val:?i8_t, Rest/?bytes_t>>, ?i8) ->
  {Val, Rest};
decode_value(<<Val:?u8_t, Rest/?bytes_t>>, ?u8) ->
  {Val, Rest};
decode_value(<<Val:?i16_t, Rest/?bytes_t>>, ?i16) ->
  {Val, Rest};
decode_value(<<Val:?u16_t, Rest/?bytes_t>>, ?u16) ->
  {Val, Rest};
decode_value(<<Val:?i32_t, Rest/?bytes_t>>, ?i32) ->
  {Val, Rest};
decode_value(<<Val:?u32_t, Rest/?bytes_t>>, ?u32) ->
  {Val, Rest};
decode_value(<<Val:?i64_t, Rest/?bytes_t>>, ?i64) ->
  {Val, Rest};
decode_value(<<Val:?u64_t, Rest/?bytes_t>>, ?u64) ->
  {Val, Rest};
decode_value(<<Len:?u32_t, Val:Len/?bytes_t, Rest/?bytes_t>>, ?str) ->
  {Val, Rest};
decode_value(Bytes, ?f64) ->
  try <<Val:?f64_t, Next/?bytes_t>> = Bytes, {Val, Next}
  catch _:_ -> <<_:64, NewNext/?bytes_t>> = Bytes, {?nan, NewNext} end.
  

%%% @doc 遍历解构数据
%%% 解包示例: decode_bytes({?i32,?i32},传入二进制数据), 返回数据 {解包数据A,解包数据B,Next}
%%% @end
decode_bytes(Types, Bytes) ->
  Size = erlang:tuple_size(Types),
  Data = erlang:make_tuple(Size + 1, 0),
  decode_bytes(1, Size, Data, Types, Bytes).
decode_bytes(Idx, Size, Data, Types, Bytes) ->
  {Val, Next} = decode_value(Bytes, erlang:element(Idx, Types)),
  if Idx =:= Size ->
    NewData = erlang:setelement(Idx, Data, Val),
    erlang:setelement(Idx + 1, NewData, Next);
    ?true ->
      NewData = erlang:setelement(Idx, Data, Val),
      decode_bytes(Idx + 1, Size, NewData, Types, Next)
  end.
  
  
%% @doc 消息数据封包, [length:uint32_t] [id:uint32_t] [data:bytes]
-spec encode_bytes(non_neg_integer(), bitstring()) -> bitstring().
encode_bytes(Id, Data) ->
  Size = byte_size(Data),
  encode_bytes(Id, Data, Size).
-spec encode_bytes(non_neg_integer(), bitstring(), non_neg_integer()) -> bitstring().
encode_bytes(Id, Data, DataLen) ->
  Size = DataLen,
  <<Size:?u32_t, Id:?u32_t, Data/?bytes_t>>.


%% @doc 打包列表
-spec encode_list(bitstring() | list()) -> bitstring().
encode_list(L) when erlang:is_list(L) ->
  encode_list(convert_utils:to_binary(L));
encode_list(L) when erlang:is_binary(L) ->
  Size = byte_size(L),
  <<Size:?u32_t, L/?bytes_t>>.

%% @doc 打包 unicode 字符串
-spec encode_unicode_string(string()) -> bitstring().
encode_unicode_string(String) ->
  UnicodeString = unicode:characters_to_binary(String),
  encode_list(UnicodeString).


%% @doc 对 [{xxx,yyy,...},...] 对象进行数据编码
%% 留意递归程度不要太深, 这里仅仅作为一维展开
%% 常见于 [ {道具ID-1,道具数量-1},{道具ID-2,道具数量-2} ]
%% 确认是否存在: lists:keyfind(104,1,Awards).
%% 如果不存在添加: lists:keystore(104,1,Awards,{104,1}).
%% 存在则替换: lists:keyreplace(101,1,Awards,{101,100}).
%% 最后编码的二进制: <<列表长度,<<列表占位>>>>
-spec encode_tuples([tuple()]) -> bitstring().
encode_tuples(L) ->
  Size = length(L),
  Res = encode_tuples_1(L, <<>>),
  <<Size:?u32_t, Res/?bytes_t>>.
encode_tuples_1([], Res) -> Res;
encode_tuples_1([Head | Tail], Res) when erlang:is_tuple(Head) ->
  TSize = erlang:size(Head),
  TRes = encode_tuples_2(Head, TSize, 1, <<>>),
  encode_tuples_1(Tail, <<Res/?bytes_t, TRes/?bytes_t>>);
encode_tuples_1([_ | Tail], Res) -> %% 不是元组对象的匹配
  encode_tuples_1(Tail, Res).
encode_tuples_2(_, Size, Idx, Res) when Idx > Size -> Res;
encode_tuples_2(T, Size, Idx, Res) ->
  Element = erlang:element(Idx, T),
  %% 匹配类型
  case Element of
    Element when erlang:is_number(Element) ->
      encode_tuples_2(T, Size, Idx + 1, <<Res/?bytes_t, Element:?u32_t>>);
    Element when erlang:is_binary(Element) ->
      encode_tuples_2(T, Size, Idx + 1, <<Res/?bytes_t, Element/?bytes_t>>);
    _ ->
      encode_tuples_2(T, Size, Idx + 1, Res)
  end.
    """
    with open(os.path.join(project_tools_dir, "byte_utils.erl"), "w", encoding="utf-8") as f:
        f.write(byte_template)


# Main
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate Rebar3 Project")
    parser.add_argument("-p", "--project", help="Project name", required=True)
    parser.add_argument("-d", "--directory", help="Directory path", required=False, default=".")
    parser.add_argument("-b", "--backup", help="Backup Old Directory", action="store_true", required=False)
    parser.add_argument("-lib", "--lib", help="System Utils", action="store_true", required=False)
    args = parser.parse_args()

    # invalid project name?
    if re.fullmatch(r'[\\w]+', args.project) is not None:
        raise Exception("Invalid Project Name")

    # project exists ?
    filepath = os.path.join(args.directory, args.project)
    filepath = os.path.abspath(filepath)
    if os.path.exists(filepath):
        if args.backup:
            backup_timestamp = "_back_%d" % time.time()
            backup = filepath + backup_timestamp
            shutil.move(filepath, backup)
        else:
            raise Exception("Directory Exists")

    # rebar3 exists ?
    rebar_file = FILE_DIR + os.sep + REBAR_EXE
    if not os.path.exists(rebar_file):
        print("Download Rebar3 ......")
        urllib.request.urlretrieve(REBAR_CDN, rebar_file)

    # rebar3 downloaded
    if not os.path.exists(rebar_file):
        raise Exception("Rebar3 Download Failed")
    print("Generating Path:", filepath)
    print("Rebar3 Document:", REBAR_DOC)

    # build rebar3 project
    os.chdir(args.directory)
    result = subprocess.call([ERL_SCRIPT, rebar_file, "new", "umbrella", args.project])
    if result != 0 or not os.path.exists(filepath):
        raise Exception("Rebar3 Generated Failed")

    # move rebar3 to project
    shutil.copy2(rebar_file, str(filepath))

    # rewrite files
    rewrite_rebar_config(filepath, args.project)
    rewrite_app_config(filepath, args.project)
    rewrite_constant_header(filepath, args.project)
    rewrite_convert_erl(filepath, args.project)

    # tools
    if args.lib:
        rewrite_tools_lib(filepath, args.project)
