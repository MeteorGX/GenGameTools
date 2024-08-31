%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 动态构建客户端进程管理器
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_executor).
-behaviour(supervisor).
-include("constant.hrl").

-export([start_link/2, init/1]).

%% @doc 启动服务
-spec start_link(Mod, Opts) -> supervisor:startlink_ret()
  when Mod :: module()|atom(),
  Opts :: list().
start_link(Mod, Opts) ->
  supervisor:start_link({?local, ?MODULE}, ?MODULE, [Mod, Opts]).

%% @doc 初始化
init([Mod, Opts]) ->
  sys_utils:set_trap_exit(),
  Worker = #{
    id => Mod,
    start => {Mod, ?start_link, [Opts]},
    restart => ?transient,
    shutdown => 2000,
    type => ?worker,
    modules => [Mod]
  },

  % 启动标识, 客户端进程错误应该不需要太多重启, 直接 Socket 断线让客户端重连即可
  MaxRestart = 4, % 最多重启4次即可
  MaxSecondsBetweenRestarts = 1,
  SupFlags = #{
    strategy => ?simple_one_for_one, % 动态构建 worker 服务
    intensity => MaxRestart,
    period => MaxSecondsBetweenRestarts
  },
  {?ok, {SupFlags, [Worker]}}.
