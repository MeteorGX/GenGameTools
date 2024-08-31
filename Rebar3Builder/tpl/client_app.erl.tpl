%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(client_app).
-include("constant.hrl").
-behaviour(application).

-export([start/2, stop/1]).
-export([
  login/4
]).


%% @doc 客户端初始化
%% 启动方式: application:start(client).
start(_StartType, _StartArgs) ->
  % 启动连接会话进程
  {?ok, _ClientPid} = tcp_client_sup:start_link(9377),
  {?ok, proc_utils:proc_id()}.


stop(_State) ->
  ?ok.


%% @doc 登录
%% 测试登录: client_app:login(1,20240811,"dev123","dev123").
%% 如果没有方法就响应: terminated with reason: undef, 有则成功
-spec login(non_neg_integer(), non_neg_integer(), string(), string()) -> ?ok.
login(Sid, Version, Username, Password) ->
  % 打包协议数据
  NewUsername = byte_utils:encode_list(Username),
  NewPassword = byte_utils:encode_list(Password),
  Message = byte_utils:encode_bytes(1001, <<
    Sid:?u32_t,
    Version:?u32_t,
    NewUsername/?bytes_t,
    NewPassword/?bytes_t
  >>),
  gen_server:cast(tcp_client_svr, {raw, Message}).
