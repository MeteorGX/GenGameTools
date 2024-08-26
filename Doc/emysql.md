# emysql驱动

> 官方文档: [emysql](https://github.com/Eonblast/Emysql)

最开始在数据选型的时候在多个数据库驱动抉择:

- `erlang-mysql-driver`: 不带有连接池处理的原始Mysql驱动, 内部实现属于很早期功能且确实很严重
- `mysql-otp`: 也是需要手动 `start_link` 且没有连接池维护方法
- `emysql`: 支持以 `application` 启动且带连接池管理, 还支持数据实体映射到 `record`
- `erlang-odbc`: 官方唯一指名的数据库驱动支持, 但是需要外部第三方支持 ODBC 驱动然后连接

因为游戏服务记录表都是采用 `record` 结构做数据保存, 为了维护方便最后选择 `emysql` 做数据库驱动.

整个游戏框架都是基于 `rebar3` 构建的 `Apps(多App)` 项目, 所以直接挂载 `emysql` 的 `application` 即可.

但这里还是有坑的, 目前项目基于 `OTP 27 | Emulator 15` 所以会出现 `Erlang` 内部 `ABI` 变动导致错误:

```plain
# 主要是找不到以下类型的声明, 在新版本更新这些结构类型已经变动了
gb_tree()/queue()/dict().
```

官方目前提出的方案是 `Fork` 官方版本自己做修改, 而社区当中也提供追加构建方法:

```erlang
%% 检测外部 `rebar3` 是否含有宏定义
%% 如果有 namespaced_types 宏代表是新版本, 从而重写新版本结构类型
-ifdef(namespaced_types).
-type gb_tree() :: gb_trees:tree().
-type queue() :: queue:queue().
-type dict() :: dict:dict().
-else.
-type gb_tree() :: gb_tree().
-type queue() :: queue().
-type dict() :: dict().
-endif.
```

这样在 `rebar.config` 追加配置启动全局宏定义:

```plain
%% Erlang 启动配置
{erl_opts, [
    % emysql 新版本依赖确认宏定义
    {platform_define, "^([1-9][0-9][0-9].*?)|([2-9][0-9].*?)|(1[8-9])", namespaced_types},

    %% 其他略
]}.
```

当然除了 `Fork` 自己版本库维护也可以通过直接下载处理( `其实不推荐这样做, 但有时候需要相对简洁直接引入源代码` ),
直接下载源代码之后将 `emysql.xxx` 文件全部复制到 `apps/emysql/src` 即可.

追加所需的加密库头文件宏 `crypto_compat.hrl`:

```erlang
-define(HASH_SHA(Data), crypto:hash(sha, Data)).
-define(HASH_FINAL(Data), crypto:hash_final(Data)).
-define(HASH_UPDATE(Data, Salt), crypto:hash_update(Data, Salt)).
-define(HASH_INIT(), crypto:hash_init(sha)).
```

这样基本处理完成本地库 `application` 引入, 之后在 `rebar.config` 设置启动时候跟随启动即可:

```plain
%% 发布编译配置
{relx, [{release, {打包应用名, "2024.08.10"},
  [
    sasl,
    crypto,
    utils,
    emysql, %% 引入启动
    %% 其他略
  ]
}]}.

%% 命令行调用 VM 需要配置环境变量: ERL_FLAGS = -args_file config/vm.args
{shell, [
  {config, "config/sys.config"},
  {apps, [
    sasl,
    crypto,
    emysql, %% 引入启动
    %% 其他略
  ]}
]}.
```

后续直接在启动时候 shell 测试是否已经引入启动:

```shell
emysql:start().
# 提示报错标识 emysql 早已启动
# {error,{already_started,emysql}}
```

后续就是数据库初始化连接情况, 这里直接从官方文档查看即可;
但是如果是本地源代码引入, 这里推荐直接自己编写初始化器启动加载数据库配置:

```shell
# 在引入的源代码 app 追加初始化 GenServer 脚本, 在其中 init 函数当中加载配置 emysql:connect
vim apps/emysql/src/emysql_init.erl

# 最后在 emysql_sup 的 supervisor 之中追加启动 gen_server
vim apps/emysql/src/emysql_sup.erl
```

也可以单独维护 `application` 来做数据库初始化对象, 这里处理比较灵活一切都看自身需求处理.

这里目前采用自己 `fork` 出来的版本来支持直接 `Git` 拉取:

[EMySQL Support OTP27](https://github.com/MeteorGX/Emysql)












