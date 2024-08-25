class_name ___TPL_FILE___
extends Object
###-------------------------------------------------------------------
### 该文件由程序生成, 按照协议来处理改动
###-------------------------------------------------------------------

const PID:int = __PID__ # 协议ID


### 消息打包二进制
static func encode(__PARAM__)->PackedByteArray:
    var msg:StreamPeerBuffer = StreamPeerBuffer.new()
    # todo: 消息编码处理
    return packet(msg.data_array)


### 协议请求编码数据
static func packet(msg:PackedByteArray)->PackedByteArray:
    var data:StreamPeerBuffer = StreamPeerBuffer.new()
    data.put_u32(msg.size())
    data.put_u32(PID)
    data.put_data(msg)
    return data.data_array