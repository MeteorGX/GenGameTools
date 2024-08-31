%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% TCP监听服务, 启动 TCP 服务来接受服务器监听
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_listener).
-behaviour(supervisor).
-include("agent.hrl").

%% @doc 导出方法
-export([start_link/2, init/1]).

%% @doc 启动函数
-spec start_link(Port, Opts) -> supervisor:startlink_ret()
  when Port :: inet:port_number(),
  Opts :: list().
start_link(Port, Opts) ->
  supervisor:start_link({?local, ?MODULE}, ?MODULE, [Port, Opts]).


%% @doc 初始化函数
init([Port, Opts]) ->
  sys_utils:set_trap_exit(), % 传递进程退出回调
  {?pool_size, SocketPoolSize} = lists:keyfind(?pool_size, 1, Opts),
  {?socket_options, SocketOpts} = lists:keyfind(?socket_options, 1, Opts),
  case gen_tcp:listen(Port, SocketOpts) of % 启动 TCP 监听
    {?ok, Listener} ->
      % 启动动态分配管理器
      Executor = #{
        id => tcp_executor,
        start => {tcp_executor, ?start_link, [tcp_worker, Opts]},
        restart => ?permanent,
        shutdown => ?brutal_kill, % 外部管理器可以无条件终止
        type => ?supervisor,
        modules => [tcp_executor]
      },


      % 创建多个 Accept 监听调配
      CoresLists = lists:seq(1, SocketPoolSize),
      Acceptors = lists:map(fun(Element) ->
        Id = convert_utils:to_atom(io_lib:format("tcp_acceptor_~w", [Element])),
        #{
          id => Id,
          % 传递给 Acceptor 启动 参数信息
          start => {tcp_acceptor, ?start_link, [Id, Listener, tcp_executor]},
          restart => ?permanent,
          shutdown => ?brutal_kill,  % 外部管理器可以无条件终止
          type => ?worker,
          modules => [tcp_acceptor]
        } end, CoresLists),


      %% 启动管理器参数
      MaxRestarts = 100,
      MaxSecondsBetweenRestarts = 6,
      SupFlags = #{
        strategy => ?one_for_one,
        intensity => MaxRestarts,
        period => MaxSecondsBetweenRestarts
      },
      {?ok, {SupFlags, [Executor | Acceptors]}};
    Error -> Error % 异常错误
  end.
