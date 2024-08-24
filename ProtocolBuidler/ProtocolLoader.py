# !/usr/bin/python
# -*- coding: UTF-8 -*-
# ------------------------------------------------------------
# 构建协议文件
# ------------------------------------------------------------
import argparse
import os
import re

FILE_DIR = os.path.dirname(os.path.realpath(__file__))
ROOT_DIR = os.path.abspath(os.path.join(FILE_DIR, "../"))
DEF_KEYBOARD = ".txt"
OUTPUT_NAME = "protocol.txt"

op_cb_var = 0
op_sb_var = 30000
cmd2op = {}


# parse files
def parse(file):
    global op_cb_var, op_sb_var, cmd2op
    opc = op_cb_var
    ops = op_sb_var
    assert (opc < 30000)
    assert (ops < 60000)
    protocols = ["", "", "", "//------- " + file]
    with open(file, "r", encoding="utf-8") as f:
        for line in f.readlines():
            line = line.replace('\r', '')
            line = line.replace('\n', '')
            cmd = re.findall(r'\.([a-zA-Z])2([a-zA-Z])_(\w+)', line)
            if cmd is not None and len(cmd) > 0:
                (input_cmd, output_cmd, cmd_name) = cmd[0]
                cmd_string = "%s2%s_%s" % (input_cmd, output_cmd, cmd_name)

                # 客户端和服务端协议
                if input_cmd == 'c' or output_cmd == 'c':
                    opc = opc + 1
                    assert (opc < 30000)
                    cmd2op[cmd_string] = opc
                    protocols.append("%d.%s" % (opc, cmd_string))
                else:  # 其他特殊协议
                    ops = ops + 1
                    assert (ops < 60000)
                    cmd2op[cmd_string] = ops
                    protocols.append("%d.%s" % (ops, cmd_string))
            else:  # 匹配不到
                protocols.append(line)

    # 最后返回
    protocols.append('')
    return protocols


# load files
def load(path, file):
    global op_cb_var, op_sb_var
    op_cb_var = op_cb_var + 100
    op_sb_var = op_sb_var + 100
    filename = "%s%s%s" % (path, os.sep, file)
    protocols = parse(filename)
    return "\n".join(protocols)


# Main
if __name__ == "__main__":
    parser = argparse.ArgumentParser(description="Generate Protocol Files")
    parser.add_argument("-d", "--directory", help="Directory path", required=True)
    parser.add_argument("-o", "--output", help="Output directory", required=True)
    parser.add_argument("-k", "--keyboard", help="Keyboard file", required=False, default=DEF_KEYBOARD)
    parser.add_argument("-n", "--name", help="Output filename", required=False, default=OUTPUT_NAME)
    args = parser.parse_args()

    if not os.path.exists(args.directory):
        raise Exception("Directory Non Exists: " + os.path.abspath(args.directory))

    directory = os.path.abspath(args.directory)
    output = os.path.abspath(args.output)
    output_file = os.path.join(output, args.name)
    print("Search Directory:", directory)
    print("Files Keyboard:", args.keyboard)
    print("Output Name:", output_file)
    proto_files = [f for f in os.listdir(directory) if f.endswith(args.keyboard)]
    if len(proto_files) == 0:
        print("Empty Protocol Files")
        exit(0)

    # sorted
    proto_files = sorted(proto_files)
    with open(output_file, "w+", encoding="utf-8") as f:
        f.write("//此文件自动生成,请勿手动修改!!!!!\n\n"),
        for proto_file in proto_files:
            print("Write Protocol:", proto_file)
            context = load(directory, proto_file)
            f.write(context)
