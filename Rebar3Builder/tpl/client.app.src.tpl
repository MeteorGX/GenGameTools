{application, client, % 声明应用为 client, 可以被 application:start(client) 启动
  [{description, "Game Network Client"},
    {vsn, "__VERSION__"}, % 应用版本
    {registered, []},
    {mod, {client_app, []}}, % 启动引用文件, 要求同目录有 client_app.erl 文件
    {applications, % 依赖其他应用
      [kernel,
        stdlib
      ]},
    {env, []},
    {modules, []},
    {licenses, ["MIT"]},
    {links, []}
  ]}.