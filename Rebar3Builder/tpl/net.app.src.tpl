{application, net, % 声明应用为 net, 可以被 application:start(net) 启动
  [{description, "Game Network Framework"},
    {vsn, "__VERSION__"}, % 应用版本
    {registered, []},
    {mod, {net_app, []}}, % 启动引用文件, 要求同目录有 net_app.erl 文件
    {applications, % 依赖其他应用
      [kernel,
        stdlib
      ]},
    {env, []},
    {modules, []},
    {licenses, ["MIT"]},
    {links, []}
  ]}.