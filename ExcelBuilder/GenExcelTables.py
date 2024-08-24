#! python
# -*- coding: UTF-8 -*-
import argparse
import time
from writer import *
from writer_erlang_erl import *
from writer_erlang_hrl import *
from writer_elixir import *
from writer_lua import *
from writer_xml import *
from writer_gdscript import *
from writer_json_object import *
from writer_json_array import *
from writer_python import *
from optparse import OptionParser

from Decoder import ExcelDoc


class Reader:
    # @input_path:excel文件所在目录
    # @srv_path  :server输出目录
    # @clt_path  :客户端输出目录
    # @timeout   :只处理文档最后更改时间在N秒内的文档
    # @suffix    :excel文件后缀
    def __init__(self, dirname, srv, clt, timeout, suffix, srv_writer, clt_writer):
        self.input_path = dirname
        self.srv_path = srv
        self.clt_path = clt
        self.timeout = timeout
        self.suffix = suffix

        self.srv_writer = None
        self.clt_writer = None

        if not (None is srv_writer):
            self.srv_writer = eval(srv_writer.capitalize() + "Writer")
        if not (None is clt_writer):
            self.clt_writer = eval(clt_writer.capitalize() + "Writer")
        print("=".ljust(36, "="), "开始导表|ExportTable", "=".ljust(36, "="))

    def can_read(self, file, abspath):
        if not os.path.isfile(abspath):
            return False
        # ~开头的excel文件是临时文件，linux下wps临时文件以.~开头
        if file.startswith("~") or file.startswith(".") or file.startswith("$"):
            return False
        if "" != self.suffix and not file.endswith(self.suffix):
            return False

        if self.timeout > 0:
            now = time.time()
            mtime = os.path.getmtime(abspath)

            if now - mtime > self.timeout:
                return False

        return True

    def read(self):
        if self.timeout > 0:
            print("read %s files from %s modified within %d seconds" % (self.suffix, self.input_path, self.timeout))
        else:
            print("read %s files from %s" % (self.suffix, self.input_path))

        if not (None is self.srv_path) and not os.path.exists(self.srv_path):
            os.makedirs(self.srv_path)
        if not (None is self.clt_path) and not os.path.exists(self.clt_path):
            os.makedirs(self.clt_path)

        now = time.time()
        file_list = os.listdir(self.input_path)
        for file in file_list:
            abspath = os.path.join(self.input_path, file)
            if self.can_read(file, abspath):
                self.read_one(file, abspath)
        print("done,%d seconds elapsed" % (time.time() - now))

    def read_one(self, file, abspath):
        doc = ExcelDoc(file, abspath)
        doc.decode(
            self.srv_path,
            self.clt_path,
            self.srv_writer,
            self.clt_writer)


if __name__ == '__main__':
    parser = argparse.ArgumentParser(description="Export Excel Tables")
    parser.add_argument("-i", "--input", help="read all files from this path", required=True)
    parser.add_argument("-s", "--srv", help="write all server file to this path", required=False, default="")
    parser.add_argument("-c", "--clt", help="write all client file to this path", required=False, default="")
    parser.add_argument("-t", "--timeout", help="only convert files modified within seconds", required=False, type=int,
                        default=-1)
    parser.add_argument("-f", "--suffix", help="what type of file will be read.empty mean all files", required=False,
                        default=".xlsx")
    parser.add_argument("-w", "--swriter",
                        help="which server writer you wish to use:lua|xml|json|gdscript|elixir|erlanghrl|erlangerl",
                        required=False, default=None)
    parser.add_argument("-l", "--cwriter",
                        help="which client writer you wish to use:lua|xml|json|gdscript|elixir|erlanghrl|erlangerl",
                        required=False, default=None)
    args = parser.parse_args()
    input_path = os.path.abspath(args.input)
    if not os.path.exists(input_path):
        raise Exception("Directory Non Exists: " + args.input)

    if len(args.srv.strip()) <= 0 and len(args.clt.strip()) <= 0:
        raise Exception("Arguments are required: -s/--srv|-c/--clt")

    srv_path = args.srv if args.srv.endswith("/") else args.srv + "/"
    clt_path = args.clt if args.clt.endswith("/") else args.clt + "/"
    reader = Reader(
        input_path,
        srv_path,
        clt_path,
        args.timeout,
        args.suffix,
        args.swriter,
        args.cwriter
    )
    reader.read()
