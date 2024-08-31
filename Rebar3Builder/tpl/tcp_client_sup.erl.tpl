%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_client_sup).
-include("constant.hrl").
-behaviour(supervisor).

-export([start_link/1, init/1]).

start_link(Port) ->
  supervisor:start_link({?local, ?MODULE}, ?MODULE, [Port]).

init([Port]) ->
  sys_utils:set_trap_exit(),
  Worker = #{
    id => tcp_client_svr,
    start => {tcp_client_svr, start_link, [Port]},
    restart => permanent,
    shutdown => 2000,
    type => worker,
    modules => [tcp_client_svr]},
  {?ok, {#{
    strategy => one_for_one,
    intensity => 5,
    period => 30},
    [Worker]
  }}.
