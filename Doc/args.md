# 外部参数

日常游戏服务器会用到大量外部配置, 如 `监听端口|服务器ID|数据库配置` 等等.

`Rebar3` 内部项目考虑到这些所有在 `rebar.config` 配置项当中有 `{sys_config, "./config/sys.config"}` 配置,
用于设置外部的所需配置传递给内部 `apps` 读取.

这里面 `./config/sys.config` 文件如下:

```plain
[
    {应用名, []}
].
```

很单纯的列表格式, 但是可以被 `application` 功能对应捕获:

- `application:get_all_env(xxx)`: 获取应用环境全部配置, `xxx` 就是 `{首位Key,[]}` 对应的 `首位Key` 标识
- `application:get_env(xxx, game_listen_port)`: 获取应用环境指定配置

这里编写个数据库配置 `database_app`, 专门暴露给 `database_app.erl` 使用 :

```palin
[
  %% 数据库
  {database_app, [

    %% 玩家库
    {mysql_user, [
      {pool, 2},
      {host, "127.0.0.1"},
      {port, 3306},
      {user, "fox"},
      {password, "fox123"},
      {database, "fox_user"},
      {encoding, utf8mb4}
    ]},

    %% 游戏库
    {mysql_game, [
      {pool, 2},
      {host, "127.0.0.1"},
      {port, 3306},
      {user, "fox"},
      {password, "fox123"},
      {database, "fox_game"},
      {encoding, utf8mb4}
    ]},

    %% 日志库
    {mysql_log, [
      {pool, 2},
      {host, "127.0.0.1"},
      {port, 3306},
      {user, "fox"},
      {password, "fox123"},
      {database, "fox_logger"},
      {encoding, utf8mb4}
    ]}
  ]}
].
```

然后内部 `app` 如果想使用直接调用获取即可, 这里可以通过 `Erlang` 的 `shell` 模式测试获取:

```plain
application:get_all_env(database_app). %% 这里的 database_app 就是定义的key
```

以上执行就能获取到内部定义的全部关键值, 用于提供给数据库内部遍历列表构建处理;
除了获取对应 `key` 全部数据之外还支持提取单独映射数据, 通过 `net_app` 的 `Key` 来做获取:

```plain
[
  %% 网络请求
  {net_app, [
    {delay_ms, 1000}, % 延迟启动时间, 网络应用要延迟等待其他服务启动
    {tcp_port, 9377}, % TCP监听端口

    %% TCP启动配置
    {tcp_opt, [
      binary,
      {packet, 0},
      {active, false},
      {reuseaddr, true},
      {nodelay, true},
      {delay_send, false},
      {backlog, 5120},
      {send_timeout, 12000},
      {keepalive, false},
      {exit_on_close, true}
    ]}
  ]}
].
```

可以直接单纯获取配置当中唯一项, 这里也可以通过 `shell` 模式测试:

```plain
application:get_env(net_app, tcp_port). %% 读取监听访问端口
```

上面会直接返回 `{ok,9377}`, 当指定 `app` 获取之后就可以针对这些配置做功能参数初始化.



