# 游戏工具集

汇集日常 `游戏服务端|客户端|策划` 常用的工具集, 采用 `Python3` 做处理.

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

二进制封包数据后续再扩展怎么做数据封包和解包, 这里暂时仅做示范, 这里也提供个 `Erlang` 服务端解构工具:

```shell
# -f 就是 GenProtocolLoader.py 解析的协议文件, -o 就是 Erlang 协议输出目录
python ProtocolBuidler\GenProtocolErlang.py -f Proto/output/protocol.txt -o Proto/output/erlang

# 最后会在 Proto/output/erlang 目录输出对应源文件和头文件
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
```

这里就会导出文件到 ` Excel/output/server` 和 `Excel/output/client` 目录.

## 网络封包

游戏项目也是直接采用二进制编码处理就行, `Erlang` 处理二进制数据封包 :

```plain
%% 数值二进制打包: <<0,0,3,233>>, 默认采用网络序列编码
NumberBitString = <<1001:32 / big - integer - unsigned>>.

%% 复杂的类型如 List 和 Tuple 则需要首位将长度编码进去后续填充内容
L = [1, 3, 4, 6].
LSize = length(L).
ListBitString = <<LSize :32 / big - integer - unsigned, ...>>. % 这里 ... 是编码将每个 int 迭代写入, 因为篇幅有限所以没处理

%% 字符串就比较特殊, 因为字符串还有中文等多国字符集, 所以需要官方的 Unicode 转化库转化成 binary 处理. 
ChineseBitString = <<"中文"/utf8>>. %% 中文需要声明好编码
ChinaeseBitStringLen = byte_size(ChineseBitString).
Utf8BitString = <<ChinaeseBitStringLen:32 / big - integer - unsigned, ChineseBitString/bytes>>.
```

对于网络二进制编码来说只有两种类型:

- `简单类型`, 定长的类型, 数值位字节长度已经规定好, 直接知道数据位是什么即可
- `复合类型`, 列表|元祖这种未知长度的, 会不断叠加延长的数据不确定最终数据长度

这里客户端只需要定义请求响应模板, 这里已经集成好 `协议对接构建器`:

```plain
//登录相关

.c2s_login_req
	//请求登录模板, 字段按照以下内容顺序处理
	{
		uint:sid 		//游戏服号, 简单类型 - uint32_t
		uint:ver 		//版本号, 简单类型 - uint32_t
		string:username //账号, 复合类型 - <<StrLen:uint32_t,Bytes/bytes>>
		string:password // 密码, 复合类型 - <<StrLen:uint32_t,Bytes/bytes>>
	}
```

传输的时候数据会自动做对齐, 则直接首位 `uint32` 做长度, 次字段 `uint32` 做协议id:

```plain
[Body Length (uint32)] [Protobuf Id (uint32)] [Body (bytes)]

# 假设这里以发送 10001 的 uint32 类型值给服务端为例
# 同时假定目前推送协议 id = 201
Body = <<10001:32 / big - integer - unsigned>>. %% 注意这里采用 uint32
%% 这里 Body 内容为 <<0,0,39,17>> 二进制, 总计 4字节

%% 字节长度获取
BodyLen = byte_size(Body). %% 这里直接得出字节长度

%% 最后就是拼凑出完整的消息包
ProtoId = 201.
Message = <<BodyLen:32 / big - integer - unsigned,ProtoId:32 / big - integer - unsigned,Body/bytes>>.
%% 最终消息包结构: <<0,0,0,4,0,0,0,201,0,0,39,17>>

% 这里细分消息结构:
%% <<(0,0,0,4)   (0,0,0,201)   (0,0,39,17)>>
%%   [bodyLen]    [protoId]    [  body  ]  
```

这里就是消息包的整体构成, 相对来说比较简单且高效, 同时还兼容 `Erlang` 自身的热更新处理.




