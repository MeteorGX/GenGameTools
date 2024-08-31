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