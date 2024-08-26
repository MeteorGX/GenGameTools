# 游戏工具集

汇集日常 `游戏服务端|客户端|策划` 常用的工具集, 采用 `Python3` 做处理, 相关其他文档内容:

> [具体游戏项目规划](Doc/README.md)

## Erlang服务端

初始化 `Erlang` 游戏项目项目, 自带了 `Rebar3` 做包管理:

```shell
# 构建出名为 game 游戏服务端, -lib 附带有自定义的工具
python Rebar3Builder/GenRebar3Project.py -p game -lib
```

## 网络协议

这里导出网络数据传输模板文件, 采用二进制数据做数据封包处理, 这里执行模板内容定义:

```plain
// 参考 Proto/defines/user.txt
.c2s_login_req
	//请求登录, 客户端连接成功后发的第一条协议
	{
		uint:sid 		//服号
		uint:ver 		//版本号
		string:username //账号
		string:password // 密码
	}
```

执行协议转化工具:

```shell
# -d 为读取的协议模板, -o 为导出协议模板文件目录, 默认导出协议文件名为 protocol.txt
python ProtocolBuidler\GenProtocolLoader.py -d Proto/defines -o Proto/output
```

最后导出的模板文件, 也就是 `Proto/output/protocol.txt` 文件:

```plain
//登录相关, 这里首位 201 就是提交协议Id, 客户端按照协议封包方式提交
201.c2s_login_req
	//请求登录, 客户端连接成功后发的第一条协议
	{
		uint:sid 		//服号
		uint:ver 		//版本号
		string:username //账号
		string:password // 密码
	}
```

> 客户端需要以此模板文件自己做命令行工具解构成自己源代码文件

网络数据结构, 采用大端网络序列做数据提交:

```plain
[ uint32(数据长度) ][ uint32(协议id) ][ bytes(数据二进制) ]
```

二进制封包数据后续再扩展怎么做数据封包和解包, 这里暂时仅做示范, 这里也提供个 `Erlang` 服务端和 `Godot` 客户端编码解码工具:

```shell
# -f 就是 GenProtocolLoader.py 解析的协议文件, -o 就是 Erlang 协议输出目录
python ProtocolBuidler\GenProtocolErlang.py -f Proto/output/protocol.txt -o Proto/output/erlang
# 最后会在 Proto/output/erlang 目录输出对应源文件和头文件

# Godot 功能也是一致
python ProtocolBuidler\GenProtocolGodot.py -f Proto/output/protocol.txt -o Proto/output/gd
```

## 策划打表

日常数据需要策划进行 `excel` 配表设置, 需要先安装 `Python3` 依赖:

```shell
pip install openpyxl
pip install slpp
```

之后就是打表为服务端和客户端文件, 这里提供随机昵称码表工具调用( `cfg_nickname` ):

```shell
# 导出 erlang 服务端所需文件
python ExcelBuilder/GenExcelTables.py --input Excel/xls --srv Excel/output/server  --timeout -1 --suffix .xlsx --swriter erlangerl
python ExcelBuilder/GenExcelTables.py --input Excel/xls --srv Excel/output/server --timeout -1 --suffix .xlsx --swriter erlanghrl

# 导出 lua 客户端所需文件
python ExcelBuilder/GenExcelTables.py --input Excel/xls --clt Excel/output/client --timeout -1 --suffix .xlsx --cwriter lua

# 导出 Godot 客户端所需文件
python ExcelBuilder/GenExcelTables.py --input Excel/xls --clt Excel/output/client --timeout -1 --suffix .xlsx --cwriter gdscript
```

这里就会导出文件到 ` Excel/output/server` 和 `Excel/output/client` 目录.
