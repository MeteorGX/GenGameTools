%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, <COMPANY>
%%% @doc
%%% Accept池, 接受客户端会话并且分配给指定子进程
%%% @end
%%%-------------------------------------------------------------------
-module(tcp_acceptor).
-behaviour(gen_server).
-include("constant.hrl").


-export([start_link/3]).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2, code_change/3]).

%% @doc Accept 运转状态
-record(tcp_acceptor_state, {
  name :: atom(), % 进程名
  proc_id :: pid(), % 进程id
  listener :: port(), % 监听对象
  ref :: number(), % Socket Ref
  mod :: atom() | module(), % 会话 fork 子进程模块名
  closed = ?false :: boolean() % 预留字段, 用于停服将 accept 切换为关闭状态
}).

%%%===================================================================
%%% Spawning and gen_server implementation
%%%===================================================================

%% @doc 启动函数
start_link(Name, Listener, Mod) -> gen_server:start_link({?local, Name}, ?MODULE, [Name, Listener, Mod], []).

%% @doc 初始化函数
init([Name, Listener, Mod]) ->
  sys_utils:set_trap_exit(),
  State = #tcp_acceptor_state{
    name = Name,
    proc_id = proc_utils:proc_id(),
    listener = Listener,
    mod = Mod,
    closed = ?true
  },

  % 注意这里是多个进程竞争状态, 异步方式移交给其他进程处理
  case prim_inet:async_accept(Listener, -1) of
    {?ok, Ref} ->
      NewState = State#tcp_acceptor_state{
        ref = Ref,
        closed = ?false
      },
      {?ok, NewState};
    Error -> {?stop, Error}
  end.


%%%===================================================================
%%% 下面的回调基本上没什么用, 保持默认即可
%%%===================================================================

handle_call(_Request, _From, State = #tcp_acceptor_state{}) ->
  {?reply, ?ok, State}.

handle_cast(_Request, State = #tcp_acceptor_state{}) ->
  {?noreply, State}.


terminate(_Reason, _State = #tcp_acceptor_state{}) ->
  ?ok.

code_change(_OldVsn, State = #tcp_acceptor_state{}, _Extra) ->
  {?ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================


%%%===================================================================
%%% 状态变动指令 - Begin
%%%===================================================================

%% 子进程唤醒
fork(Mod, Listener, Socket) ->
  case supervisor:start_child(Mod, [Listener, Socket]) of
    {?ok, ChildPid} ->
      proc_utils:proc_unlink(ChildPid),
      {?ok, ChildPid};
    {?error, {already_started, ChildPid}} ->
      proc_utils:proc_unlink(ChildPid),
      {?ok, ChildPid};
    {?error, Reason} ->
      {?error, Reason}
  end.

%% @doc 异步 accept
async_accept(Listener, State = #tcp_acceptor_state{}) ->
  case prim_inet:async_accept(Listener, -1) of
    {?ok, Ref} ->
      {?noreply, State#tcp_acceptor_state{ref = Ref}};
    Error -> {?stop, Error, State}
  end.

%% @doc 核心任务回调 - 异步返回
handle_info({inet_async, Listener, Ref, {?ok, Socket}}, State = #tcp_acceptor_state{
  listener = Listener, ref = Ref, closed = Closed, mod = Mod
}) ->
  % 确认是否关服, 如果关服移交到关闭服务
  case Closed of
    ?true -> % 关服直接拒绝连接
      gen_tcp:close(Socket),
      async_accept(Listener, State);
    ?false ->  % 移交动态创建 agent
      fork(Mod, Listener, Socket),
      async_accept(Listener, State)
  end;


%% @doc 用户主动退出 - 异步返回
handle_info({inet_async, Listener, Ref, {?error, closed}}, State = #tcp_acceptor_state{
  listener = Listener, ref = Ref
}) -> {?stop, ?normal, State};


%% @doc 未知中断 - 异步返回
handle_info({inet_async, Listener, Ref, {?error, _Error}}, State = #tcp_acceptor_state{
  listener = Listener, ref = Ref
}) -> {?stop, ?normal, State};


%% @doc 系统异常退出
handle_info({'EXIT', _Pid, _Reason}, State) ->
  {?noreply, State};


%% @doc 其他进程需要回调
handle_info({?exec, Mod, Fun, Args}, State) ->
  NewState = Mod:Fun(Args, State),
  {?noreply, NewState};

%% @doc 默认处理
handle_info(_Info, State = #tcp_acceptor_state{}) ->
  {?noreply, State}.

%%%===================================================================
%%% 状态变动指令 - End
%%%===================================================================