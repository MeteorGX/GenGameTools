# 分区分服

传统游戏选服架构会在玩家授权登录之后返回服务器列表提供给客户端选择,
亦或是不同应用商店渠道服( `快手|抖音|哔哩哔哩` 特供服务器 )等数据不互通服务器.

虽然现代游戏架构采用无选服的分布逻辑( `玩家|战斗|活动等模块` 分布不同服务器处理, 不再单服处理全部业务模糊服务器概念 ),
但是现实当中有的运营方可能自己提供私有服务器部署要求数据落地到运营方, 然后可能出于某些原因最后停止维护需要将玩家合并到官服之中.

在现实当中游戏服务端环境远比理想当中复杂, 所以分区分服也算是宁愿冗余也要建议保存数据的方式.

玩家的 `ID标识` 必须要是关联 `分区|服务器ID` 的唯一ID, 所以基于此在设计数据库表的时候 `玩家ID永远不能采用自增`.

> 另外还有说法就是如果 用户ID|玩家ID 采用自增会比较容易按照 int 自增推敲具体用户游戏量

这里我处理好了采用字节偏移处理换算出玩家在 `分区|服务器上的唯一ID`:

```erlang
%% 工具文件: apps/utils/src/sys_utils.erl

%% @doc 生成全局唯一64位id
%% MajorId 渠道ID, 占据 10 bit
%% MinorId 服务器ID, 占 12 bit
%% 时间戳混合占据 41 bit
%% ((millisecond / 1000 + second + 1000) << (12 + 10)) + (MinorId << 10) + MajorId
%% 转化得出: (MS div 1000 + S * 1000) bsl (12+10) + (3(服务器ID) bsl 100) + 1(渠道ID)
%% 公式计算: ((erlang:system_time(millisecond) div 1000 + erlang:system_time(seconds) * 1000) bsl (12+10)) + (10001 bsl 10) + 1.
%% 可以测试下高并发的时候生成唯一标识碰撞概率
guid(MajorId, MinorId) when
  erlang:is_number(MajorId) andalso erlang:is_number(MinorId)
    andalso MajorId =< 255 andalso MinorId =< 65535 ->
  MS = erlang:system_time(millisecond),
  S = erlang:system_time(seconds),
  ((MS div 1000 + S * 1000) bsl (12 + 10)) + (MinorId bsl 10) + MajorId.
```

这里的生成 RoleId 涵盖:

- 渠道ID, 占 uint8 长度, 1~255 取值足够大量第三方渠道平台使用
- 服务器ID, 占 uint32 长度, 1~65535 取值应该足够开这些服务器
- 最后获取秒和毫秒处理偏移得出值

对应其他语言当中的偏移计算方式:

```cpp
// C版本, 其他语言参考, 但是注意得出来的值必须是支持 uint64 类型语言
#include <stdio.h>
#include <stdint.h>

// 函数入口
int main() {
    uint8_t MajorId = 3; // 渠道ID
    uint32_t ServerId = 10; // 服务器ID
    uint64_t Ms = 1723520278289; // 毫秒时间戳
    uint64_t S = 1723520538; // 秒级别时间戳 
    // 12+10是固定的, 采用 Redis 的计算id方法
    uint64_t RoleId = (( (uint64_t)(Ms / 1000) + S * 1000) << (12 + 10)) + (ServerId << 10) + MajorId;
    printf("RoleId = %lu",RoleId); // 7236198054611658755, 最后计算结果比较确认没问题
    return 0;
}
```

> 必须时刻记得偏移得出的值是 uint64 的, 数据库做记录必须要用 bigint 类型, 着重注意类似 Java 这种没有 unsigned 的值溢出问题

后续如果渠道服关闭合并到官方服的时候直接将数据追加进新表而不会出现 ID 冲突的情况.

