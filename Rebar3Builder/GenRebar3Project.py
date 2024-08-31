# !/usr/bin/python
# -*- coding: UTF-8 -*-
# ===================================================
# Build Rebar3 Project | 构建 Rebar3 项目工程
# ===================================================
import argparse
import os
import re
import shutil
import subprocess
import time
import urllib.request
from datetime import datetime

FILE_DIR = os.path.dirname(os.path.realpath(__file__))
ROOT_DIR = os.path.abspath(os.path.join(FILE_DIR, "../"))
TPL_DIR = os.path.join(FILE_DIR, "tpl")

REBAR_CDN = "https://s3.amazonaws.com/rebar3/rebar3"
REBAR_EXE = "rebar3"
REBAR_DOC = "https://rebar3.org/docs/getting-started"
ERL_SCRIPT = "escript.exe" if os.name == "nt" else "escript"
ERL_EXE = "werl.exe" if os.name == "nt" else "erl"
BUILD_YMD = datetime.now().strftime("%Y%m%d")

TPL_REBAR_CONF_FILE = os.path.join(TPL_DIR, "rebar.config.tpl")
TPL_SYS_CONF_FILE = os.path.join(TPL_DIR, "sys.config.tpl")
TPL_VM_ARGS_FILE = os.path.join(TPL_DIR, "vm.args.tpl")
TPL_CONSTANT_FILE = os.path.join(TPL_DIR, "constant.hrl.tpl")
TPL_CONVERT_UTILS = os.path.join(TPL_DIR, "convert_utils.erl.tpl")

TPL_UTILS_SRC = os.path.join(TPL_DIR, "utils.app.src.tpl")
TPL_PROC_UTILS = os.path.join(TPL_DIR, "proc_utils.erl.tpl")
TPL_SYS_UTILS = os.path.join(TPL_DIR, "sys_utils.erl.tpl")
TPL_DATETIME_UTILS = os.path.join(TPL_DIR, "datetime_utils.erl.tpl")
TPL_BYTE_UTILS = os.path.join(TPL_DIR, "byte_utils.erl.tpl")

TPL_NET_SRC = os.path.join(TPL_DIR, "net.app.src.tpl")
TPL_NET_HEADERS = os.path.join(TPL_DIR, "agent.hrl.tpl")
TPL_NET_TCP_LISTENER = os.path.join(TPL_DIR, "tcp_listener.erl.tpl")
TPL_NET_TCP_ACCEPTOR = os.path.join(TPL_DIR, "tcp_acceptor.erl.tpl")
TPL_NET_TCP_EXECUTOR = os.path.join(TPL_DIR, "tcp_executor.erl.tpl")
TPL_NET_TCP_WORKER = os.path.join(TPL_DIR, "tcp_worker.erl.tpl")
TPL_NET_APP_FILE = os.path.join(TPL_DIR, "net_app.erl.tpl")

TPL_CLIENT_SRC = os.path.join(TPL_DIR, "client.app.src.tpl")
TPL_CLIENT_APP_FILE = os.path.join(TPL_DIR, "client_app.erl.tpl")
TPL_CLIENT_TCP_SUP = os.path.join(TPL_DIR, "tcp_client_sup.erl.tpl")
TPL_CLIENT_TCP_SVR = os.path.join(TPL_DIR, "tcp_client_svr.erl.tpl")


# rewrite rebar.config
def rewrite_rebar_config(project_dir, project_name):
    rebar_config = os.path.join(project_dir, "rebar.config")
    if not os.path.exists(rebar_config):
        raise Exception("Config Non Exists")

    # 改写模板
    with open(TPL_REBAR_CONF_FILE, 'r', encoding="utf-8") as file:
        template = file.read()
        template = template.replace("__VERSION__", BUILD_YMD).replace("__PROJECT__", project_name)
        with open(rebar_config, "w", encoding="utf-8") as f:
            f.write(template.strip())


# rewrite erlang.config
def rewrite_app_config(project_dir, project_name):
    sys_config = os.path.join(project_dir, "config", "sys.config")
    vm_args = os.path.join(project_dir, "config", "vm.args")
    if not os.path.exists(sys_config) or not os.path.exists(vm_args):
        raise Exception("AppConfig Non Exists")

    # 改写模板,写入 sys.config
    with open(TPL_SYS_CONF_FILE, 'r', encoding="utf-8") as file:
        template = file.read()
        template = template.replace("__PROJECT__", project_name)
        with open(sys_config, "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 改写模板,写入 vm.args
    with open(TPL_VM_ARGS_FILE, 'r', encoding="utf-8") as file:
        template = file.read()
        template = template.replace("__PROJECT__", project_name)
        with open(vm_args, "w", encoding="utf-8") as f:
            f.write(template.strip())


# rewrite constant header
def rewrite_constant_header(project_dir, project_name):
    project_keyboard = str.upper(project_name)
    project_header_dir = os.path.join(project_dir, "include")
    project_header_file = os.path.join(project_header_dir, "constant.hrl")
    if not os.path.exists(project_header_dir):
        os.mkdir(project_header_dir)

    # 写入 constant.hrl
    with open(TPL_CONSTANT_FILE, 'r', encoding="utf-8") as file:
        template = file.read()
        template = template.replace("__CONSTANT_NAME__", "__CONSTANT_" + project_keyboard + "__")
        template = template.replace("__CONSTANT_VER__", BUILD_YMD)
        with open(project_header_file, "w", encoding="utf-8") as f:
            f.write(template.strip())


# rewrite convert utils
def rewrite_convert_erl(project_dir, _project_name):
    # 不存在目录直接创建
    project_apps_dir = os.path.join(project_dir, "apps")
    project_tools_dir = os.path.join(project_apps_dir, "utils", "src")
    if not os.path.isdir(project_tools_dir):
        os.makedirs(project_tools_dir)

    # convert_utils.erl
    with open(TPL_CONVERT_UTILS, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(project_tools_dir, "convert_utils.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 最终写入应用版本内容
    with open(TPL_UTILS_SRC, 'r', encoding="utf-8") as file:
        template = file.read()
        template = template.replace("__VERSION__", BUILD_YMD)
        with open(os.path.join(project_tools_dir, "utils.app.src"), "w", encoding="utf-8") as f:
            f.write(template.strip())


# rewrite tool utils
def rewrite_tools_lib(project_dir, _project_name):
    # 不存在目录直接创建
    project_apps_dir = os.path.join(project_dir, "apps")
    project_tools_dir = os.path.join(project_apps_dir, "utils", "src")
    if not os.path.isdir(project_tools_dir):
        os.makedirs(project_tools_dir)

    # 写入基础库工具 - proc
    with open(TPL_PROC_UTILS, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(project_tools_dir, "proc_utils.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入基础库工具 - sys
    with open(TPL_SYS_UTILS, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(project_tools_dir, "sys_utils.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入基础库工具 - datetime
    with open(TPL_DATETIME_UTILS, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(project_tools_dir, "datetime_utils.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入基础库工具 - byte
    with open(TPL_BYTE_UTILS, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(project_tools_dir, "byte_utils.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())


# rewrite network framework
def rewrite_networks(project_dir, project_name):
    rebar_config = os.path.join(project_dir, "rebar.config")
    project_keyboard = str.upper(project_name)
    project_apps_dir = os.path.join(project_dir, "apps")
    net_app_dir = os.path.join(project_apps_dir, "net")
    net_include_dir = os.path.join(net_app_dir, "include")
    net_src_dir = os.path.join(net_app_dir, "src")
    net_tcp_dir = os.path.join(net_src_dir, "tcp")
    os.makedirs(net_app_dir)
    os.makedirs(net_include_dir)
    os.makedirs(net_src_dir)
    os.makedirs(net_tcp_dir)

    # 写入网络库信息
    with open(TPL_NET_SRC, 'r', encoding="utf-8") as file:
        template = file.read()
        template = template.replace("__VERSION__", BUILD_YMD)
        with open(os.path.join(net_src_dir, "net.app.src"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入头信息
    with open(TPL_NET_HEADERS, 'r', encoding="utf-8") as file:
        template = file.read()
        template = template.replace("__NET_NAME__", "__NET_" + project_keyboard + "__")
        template = template.replace("__NET_VER__", BUILD_YMD)
        with open(os.path.join(net_include_dir, "agent.hrl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入 tcp_listener.erl
    with open(TPL_NET_TCP_LISTENER, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(net_tcp_dir, "tcp_listener.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入 tcp_acceptor.erl
    with open(TPL_NET_TCP_ACCEPTOR, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(net_tcp_dir, "tcp_acceptor.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入 tcp_executor.erl
    with open(TPL_NET_TCP_EXECUTOR, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(net_tcp_dir, "tcp_executor.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入 tcp_worker.erl
    with open(TPL_NET_TCP_WORKER, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(net_tcp_dir, "tcp_worker.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 写入 net_app.erl
    with open(TPL_NET_APP_FILE, 'r', encoding="utf-8") as file:
        template = file.read()
        with open(os.path.join(net_src_dir, "net_app.erl"), "w", encoding="utf-8") as f:
            f.write(template.strip())

    # 附加网络库信息
    with open(rebar_config, 'r', encoding="utf-8") as file:
        template = file.read()
        template = template.replace("%% __NET_NAME__ %%", "net")
        template = template.replace("%% __NET_HEADER__ %%", r'{i, "apps/net/include"}')
        with open(rebar_config, "w", encoding="utf-8") as f:
            f.write(template.strip())


# rewrite network client
def rewrite_client(project_dir, _project_name):
    project_apps_dir = os.path.join(project_dir, "apps")
    client_app_dir = os.path.join(project_apps_dir, "client")
    client_src_dir = os.path.join(client_app_dir, "src")
    if not os.path.exists(client_src_dir):
        os.makedirs(client_src_dir)

        # 写入版本信息
        with open(TPL_CLIENT_SRC, 'r', encoding="utf-8") as file:
            template = file.read()
            template = template.replace("__VERSION__", BUILD_YMD)
            with open(os.path.join(client_src_dir, "client.app.src"), "w", encoding="utf-8") as f:
                f.write(template.strip())

        # 写入 tcp_client_sup.erl
        with open(TPL_CLIENT_TCP_SUP, 'r', encoding="utf-8") as file:
            template = file.read()
            with open(os.path.join(client_src_dir, "tcp_client_sup.erl"), "w", encoding="utf-8") as f:
                f.write(template.strip())

        # 写入 tcp_client_svr.erl
        with open(TPL_CLIENT_TCP_SVR, 'r', encoding="utf-8") as file:
            template = file.read()
            with open(os.path.join(client_src_dir, "tcp_client_svr.erl"), "w", encoding="utf-8") as f:
                f.write(template.strip())

        # 写入 client_app.erl
        with open(TPL_CLIENT_APP_FILE, 'r', encoding="utf-8") as file:
            template = file.read()
            with open(os.path.join(client_src_dir, "client_app.erl"), "w", encoding="utf-8") as f:
                f.write(template.strip())


# Main
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate Rebar3 Project")
    parser.add_argument("-p", "--project", help="Project name", required=True)
    parser.add_argument("-d", "--directory", help="Directory path", required=False, default=".")
    parser.add_argument("-b", "--backup", help="Backup Old Directory", action="store_true", required=False)
    parser.add_argument("-lib", "--lib", help="System Utils", action="store_true", required=False)
    parser.add_argument("-n", "--net", help="Network Framework", action="store_true", required=False)
    args = parser.parse_args()

    # invalid project name?
    if re.fullmatch(r'[\\w]+', args.project) is not None:
        raise Exception("Invalid Project Name")

    # project exists ?
    filepath = os.path.join(args.directory, args.project)
    filepath = os.path.abspath(filepath)
    if os.path.exists(filepath):
        if args.backup:
            backup_timestamp = "_back_%d" % time.time()
            backup = filepath + backup_timestamp
            shutil.move(filepath, backup)
        else:
            raise Exception("Directory Exists")

    # rebar3 exists ?
    rebar_file = FILE_DIR + os.sep + REBAR_EXE
    if not os.path.exists(rebar_file):
        print("Download Rebar3 ......")
        urllib.request.urlretrieve(REBAR_CDN, rebar_file)

    # rebar3 downloaded
    if not os.path.exists(rebar_file):
        raise Exception("Rebar3 Download Failed")
    print("Generating Path:", filepath)
    print("Rebar3 Document:", REBAR_DOC)

    # build rebar3 project
    os.chdir(args.directory)
    result = subprocess.call([ERL_SCRIPT, rebar_file, "new", "umbrella", args.project])
    if result != 0 or not os.path.exists(filepath):
        raise Exception("Rebar3 Generated Failed")

    # move rebar3 to project
    shutil.copy2(rebar_file, str(filepath))

    # rewrite files
    rewrite_rebar_config(filepath, args.project)
    rewrite_app_config(filepath, args.project)
    rewrite_constant_header(filepath, args.project)
    rewrite_convert_erl(filepath, args.project)

    # tools
    if args.lib:
        rewrite_tools_lib(filepath, args.project)

    # network
    if args.net:
        rewrite_networks(filepath, args.project)
        rewrite_client(filepath, args.project)
