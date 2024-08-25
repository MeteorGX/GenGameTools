# !/usr/bin/python
# -*- coding: UTF-8 -*-
import argparse
import os

FILE_DIR = os.path.dirname(os.path.realpath(__file__))
ROOT_DIR = os.path.abspath(os.path.join(FILE_DIR, "../"))
PY_EXE = "python.exe" if os.name == "nt" else "python"

TPL_DIR = os.path.join(FILE_DIR, "tpl")
TPL_GDS_FILE = os.path.join(TPL_DIR, "protocols.gd.tpl")
TPL_GD_FILE = os.path.join(TPL_DIR, "protocol.gd.tpl")
GDS_FILE = "protocols.gd"

TPL_PID_KEYBOARD = "__PID__"
TPL_KEYBOARD = "___TPL_FILE___"
TPL_GDS_C2S_TEXT_KEYBOARD = "%% !__C2S_TEXT__! %%"
TPL_GDS_S2C_TEXT_KEYBOARD = "%% !__S2C_TEXT__! %%"
TPL_GDS_C2S_KEYBOARD = "%% !__C2S__! %%"
TPL_GDS_S2C_KEYBOARD = "%% !__S2C__! %%"

if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate Godot Protocol")
    parser.add_argument("-f", "--file", help="Protocol file", required=True)
    parser.add_argument("-o", "--output", help="Output directory", required=True)
    args = parser.parse_args()

    if not os.path.exists(args.file):
        raise Exception("Protocol File Non Exists")

    if not os.path.exists(args.output):
        os.makedirs(args.output)

    # 转化成 gd 格式头文件用于加载
    data = []
    c2s = []
    s2c = []
    with open(args.file, "r", encoding="utf-8") as file:
        for line in file:
            stripped_line = line.strip()
            # 排除空行和注释
            if len(stripped_line) <= 0 or stripped_line.startswith("//"):
                continue

            # 匹配必须要关键字 .s2c | .c2s
            if not (".s2c" in stripped_line) and not (".c2s" in stripped_line):
                continue

            # 处理成 erlang 文件
            split = stripped_line.split(".")
            if len(split) != 2:
                continue
            p_id = split[0]
            p_name = split[1]

            # 写入输出协议文件
            if ".s2c" in stripped_line:
                s2c.append("const %s = %s" % (p_name.upper(), p_id))

            if ".c2s" in stripped_line:
                data.append((p_id, p_name))
                c2s.append("const %s = %s" % (p_name.upper(), p_id))

    # 构建遍历
    proto_c2s_data = []
    for (proto_id, proto_name) in data:
        proto_file = os.path.abspath(os.path.join(args.output, proto_name + ".gd"))
        print("id:", proto_id, "|", "name:", proto_file)

        # 写入协议映射表
        proto_c2s_data.append("\t%s:\"%s\"" % (proto_id, proto_name))

        # 写入协议文件
        with open(TPL_GD_FILE, 'r', encoding="utf-8") as file:
            content = file.read()
            with open(proto_file, "w+", encoding="utf-8") as p:
                content = content.replace(TPL_PID_KEYBOARD, proto_id)
                p.write(content.replace(TPL_KEYBOARD, proto_name))

    if len(proto_c2s_data) > 0:
        with open(TPL_GDS_FILE, 'r', encoding="utf-8") as file:
            content = file.read()
            proto_file = os.path.abspath(os.path.join(args.output, GDS_FILE))
            with open(proto_file, 'w+', encoding="utf-8") as p:
                content = content.replace(TPL_KEYBOARD, GDS_FILE.replace(".gd", ""))
                content = content.replace(TPL_GDS_C2S_TEXT_KEYBOARD, ",\n".join(proto_c2s_data))
                content = content.replace(TPL_GDS_C2S_KEYBOARD, "\n".join(c2s))
                content = content.replace(TPL_GDS_S2C_KEYBOARD, "\n".join(s2c))
                p.write(content)
