{minimum_otp_vsn, "27.0"}. % OTP最低版本|OTP Version
{root_dir, "."}. % 根目录 | Root Path
{base_dir, "_build"}. % 编译目录 | Build Path
{deps_dir, "lib"}. % 第三方库目录 | Library Build Path
{project_app_dirs, ["apps/*", "lib/*", "."]}. %  OTP applications

%% 启动配置
{erl_opts, [
    {platform_define, "__VERSION__", gen_rebar3_date}, % 项目启动时间 | Create Datetime
    {i, "include"}, % 应用头文件 | Global Headers

    %% __NET_HEADER__ %%, % 网络库 | Net Headers

    % 系统其他配置 | Erlang Options
    warn_keywords,
    debug_info
]}.

%% 发布编译配置
{relx, [{release, {__PROJECT__, "__VERSION__"},[
            sasl,
            crypto,
            utils,
            %% __NET_NAME__ %%,
            __PROJECT__
        ]},

        {mode, dev},

        %% automatically picked up if the files
        %% exist but can be set manually, which
        %% is required if the names arenot exactly
        %% sys.config and vm.args
        {sys_config, "./config/sys.config"},
        {vm_args, "./config/vm.args"},

        %% the .src form of the configuration files do
        %% not require setting RELX_REPLACE_OS_VARS
        %% {sys_config_src, "./config/sys.config.src"},
        %% {vm_args_src, "./config/vm.args.src"}

        {extended_start_script, true}
]}.

%% @doc 打包设置 | Package Settings
{profiles, [

    %% 测试包|Test Package
    %% 打包|Package: rebar3 as dev release
    %% 压缩包|Compress: rebar3 as dev tar
    {prod, [{relx, [
        {dev_mode, true},
        {include_erts, false},
        {include_src, true},
        {debug_info, true}
    ]}]},

    %% 正式包|Release
    {prod, [{relx, [
        {dev_mode, false},
        {include_erts, true},
        {include_src, false},
        {debug_info, strip}]}
    ]}
]}.

%% 命令行调用|Shell Run
%% 命令行调用 VM 需要配置环境变量: ERL_FLAGS = -args_file config/vm.args
{shell, [
  {config, "config/sys.config"},
  {apps, [
    sasl,
    crypto,
    utils,
    %% __NET_NAME__ %%,
    __PROJECT__
  ]}
]}.


%% CDN
{rebar_packages_cdn, "https://hexpm.upyun.com"}.