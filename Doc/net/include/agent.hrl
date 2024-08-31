-include("constant.hrl"). % 引入功能头


-ifndef(__NET_HEADERS__).
-define(__NET_HEADERS__, 1).

%% 配置相关
-define(pool_size, tcp_pool_size). % 连接池数量
-define(socket_options, tcp_options). % Socket配置

%% 回调相关事件
-define(on_established, on_established). % 连接回调
-define(on_message, on_message). % 消息回调
-define(on_closed, on_closed). % 关闭回调


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 玩家在服务器挂载实体 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(session, {

  %% 账号基础信息 ---------------------------------------
  sid = 0 :: non_neg_integer(), % 服务器ID
  uid = 0 :: non_neg_integer(), % 用户id
  id = 0 :: non_neg_integer(), % 角色id
  version = 0 :: non_neg_integer(), % 游戏客户端版本
  proc_id :: pid(), % 进程ID
  socket :: port(), % Socket对象

  %% 游戏基础信息 ---------------------------------------
  %% 下面的属性大部分在MMORPG游戏用的比较多, 需要跨场景进行处理的时候需要用到
  %% 举例上次处于某个主城|野外公共地图, 需要发送给场景说明用户上线并进行广播监听|发送
  scene_id = 0 :: non_neg_integer(), % 关卡场景id, 简单游戏业务不需要记录进程

  %% 登录基础信息 ---------------------------------------
  create_at = 0 :: non_neg_integer(), % 会话创建时间 | 登录时间
  ip_address = "Unknown" :: string(), % 会话创建IP | 登录IP
  online = 0 :: non_neg_integer(), % 在线时长, 不计算本次登录

  status = [] :: list() %% 状态记录器, 记录进程当中状态信息
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 玩家在服务器挂载实体 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%


%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器代理 - Begin
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
-record(agent, {
  listen :: port(), %% 监听 Listener Socket 句柄
  socket :: port(), %% 客户端 Socket 会话句柄
  ref :: number(), %% Socket 依赖,Ref
  session :: #session{}, %% 会话
  on_established :: ?false | {?on_established, module()|atom(), atom()}, %% 初始化回调
  on_message :: ?false | {?on_message, module()|atom(), atom()}, %% 消息调用回调
  on_closed :: ?false | {?on_closed, module()|atom(), atom()}, %% 进程退出回调
  bytes = <<>> :: bitstring() %% 客户端请求的二进制数据流
}).
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 服务器代理 - End
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%

-endif.




