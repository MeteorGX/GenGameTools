# 协议封装

客户端和服务端数据封包交换目前常见的有 `GoogleProtobuf(以下简称Protobuf)` 和 `直接二进制编码`,
其实对于 `Erlang` 来说更加推荐直接二进制封包传输而非 `Protobuf` 处理.

`Protobuf` 的引入带来很大不确定性, 且生成那些序列化处理文件的代码堆叠量都不知道怎么处理,
全局搜索业务函数常常跳到 `protoc` 生成序列文件, 还有其他无法进行热更新加载新结构问题.

> 另外还有 `Protobuf-V2` 和 `Protobuf-V3` 版本差异, 两者的 ABI 可能差异更大

所以这里的游戏项目也是直接采用二进制编码处理就行, `Erlang` 处理二进制也是十分方便 :

```erlang
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

.s2c_login_res
	//登录响应模板, 账号不存在会默认帮助其注册
	{
		uint:state		//登录状态: 0-正常 | 其他错误码
		uint:ctime		//创建时间 
		uint:uid		//账号id
		uint:player_id	//角色id: 等于0代表需要创建角色跳创角页面
	}
```

这里的协议编写工具执行下就可以导出协议模板文件:

```shell
## 执行以下命令会生成模板 resources/proto/protocol.txt
python resources/proto/protocol_loader.py
```

协议模板如下, 客户端和服务端可以参照这个模板自己编写正则生成自己游戏代码格式文件:

```plain
301.c2s_login_req
	//请求登录模板, 字段按照以下内容顺序处理
	{
		uint:sid 		//服号
		uint:ver 		//版本号
		string:username //账号
		string:password // 密码
	}

302.s2c_login_res
	//登录响应模板, 账号不存在会默认帮助其注册
	{
		uint:state		//登录状态: 0-正常 | 其他错误码
		uint:ctime		//创建时间
		uint:uid		//账号id
		uint:player_id	//角色id: 等于0代表需要创建角色跳创角页面
	}
```

- `c2s_xxx_req` 代表客户端请求服务器消息
- `s2c_yyy_res` 代表服务端响应给客户端消息

> 这里输出的协议文件其实更加类似于文档功能, 用于给服务端和客户端做消息架构同步

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
%% <<(0,0,0,4,)  (0,0,0,201,)  (0,0,39,17)>>
%%   [bodyLen]    [protoId]    [  body  ]  
```

这里就是消息包的整体构成, 相对来说比较简单且高效, 同时还兼容 `Erlang` 自身的热更新处理.

