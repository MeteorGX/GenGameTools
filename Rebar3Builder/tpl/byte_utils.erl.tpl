%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 二进制处理库
%%% @end
%%%-------------------------------------------------------------------
-module(byte_utils).
-author("MeteorCat").
-include("constant.hrl").

%% API
-export([
  decode_value/2,
  decode_bytes/2,
  encode_bytes/2,
  encode_list/1,
  encode_unicode_string/1,
  encode_tuples/1
]).

%% @doc 格式消息解码映射
decode_value(<<Val:?i8_t, Rest/?bytes_t>>, ?i8) ->
  {Val, Rest};
decode_value(<<Val:?u8_t, Rest/?bytes_t>>, ?u8) ->
  {Val, Rest};
decode_value(<<Val:?i16_t, Rest/?bytes_t>>, ?i16) ->
  {Val, Rest};
decode_value(<<Val:?u16_t, Rest/?bytes_t>>, ?u16) ->
  {Val, Rest};
decode_value(<<Val:?i32_t, Rest/?bytes_t>>, ?i32) ->
  {Val, Rest};
decode_value(<<Val:?u32_t, Rest/?bytes_t>>, ?u32) ->
  {Val, Rest};
decode_value(<<Val:?i64_t, Rest/?bytes_t>>, ?i64) ->
  {Val, Rest};
decode_value(<<Val:?u64_t, Rest/?bytes_t>>, ?u64) ->
  {Val, Rest};
decode_value(<<Len:?u32_t, Val:Len/?bytes_t, Rest/?bytes_t>>, ?str) ->
  {Val, Rest};
decode_value(Bytes, ?f64) ->
  try <<Val:?f64_t, Next/?bytes_t>> = Bytes, {Val, Next}
  catch _:_ -> <<_:64, NewNext/?bytes_t>> = Bytes, {?nan, NewNext} end.


%%% @doc 遍历解构数据
%%% 解包示例: decode_bytes({?i32,?i32},传入二进制数据), 返回数据 {解包数据A,解包数据B,Next}
%%% @end
decode_bytes(Types, Bytes) ->
  Size = erlang:tuple_size(Types),
  Data = erlang:make_tuple(Size + 1, 0),
  decode_bytes(1, Size, Data, Types, Bytes).
decode_bytes(Idx, Size, Data, Types, Bytes) ->
  {Val, Next} = decode_value(Bytes, erlang:element(Idx, Types)),
  if Idx =:= Size ->
    NewData = erlang:setelement(Idx, Data, Val),
    erlang:setelement(Idx + 1, NewData, Next);
    ?true ->
      NewData = erlang:setelement(Idx, Data, Val),
      decode_bytes(Idx + 1, Size, NewData, Types, Next)
  end.


%% @doc 消息数据封包, [length:uint32_t] [id:uint32_t] [data:bytes]
-spec encode_bytes(non_neg_integer(), bitstring()) -> bitstring().
encode_bytes(Id, Data) ->
  Size = byte_size(Data),
  encode_bytes(Id, Data, Size).
-spec encode_bytes(non_neg_integer(), bitstring(), non_neg_integer()) -> bitstring().
encode_bytes(Id, Data, DataLen) ->
  Size = DataLen,
  <<Size:?u32_t, Id:?u32_t, Data/?bytes_t>>.


%% @doc 打包列表
-spec encode_list(bitstring() | list()) -> bitstring().
encode_list(L) when erlang:is_list(L) ->
  encode_list(convert_utils:to_binary(L));
encode_list(L) when erlang:is_binary(L) ->
  Size = byte_size(L),
  <<Size:?u32_t, L/?bytes_t>>.

%% @doc 打包 unicode 字符串
-spec encode_unicode_string(string()) -> bitstring().
encode_unicode_string(String) ->
  UnicodeString = unicode:characters_to_binary(String),
  encode_list(UnicodeString).


%% @doc 对 [{xxx,yyy,...},...] 对象进行数据编码
%% 留意递归程度不要太深, 这里仅仅作为一维展开
%% 常见于 [ {道具ID-1,道具数量-1},{道具ID-2,道具数量-2} ]
%% 确认是否存在: lists:keyfind(104,1,Awards).
%% 如果不存在添加: lists:keystore(104,1,Awards,{104,1}).
%% 存在则替换: lists:keyreplace(101,1,Awards,{101,100}).
%% 最后编码的二进制: <<列表长度,<<列表占位>>>>
-spec encode_tuples([tuple()]) -> bitstring().
encode_tuples(L) ->
  Size = length(L),
  Res = encode_tuples_1(L, <<>>),
  <<Size:?u32_t, Res/?bytes_t>>.
encode_tuples_1([], Res) -> Res;
encode_tuples_1([Head | Tail], Res) when erlang:is_tuple(Head) ->
  TSize = erlang:size(Head),
  TRes = encode_tuples_2(Head, TSize, 1, <<>>),
  encode_tuples_1(Tail, <<Res/?bytes_t, TRes/?bytes_t>>);
encode_tuples_1([_ | Tail], Res) -> %% 不是元组对象的匹配
  encode_tuples_1(Tail, Res).
encode_tuples_2(_, Size, Idx, Res) when Idx > Size -> Res;
encode_tuples_2(T, Size, Idx, Res) ->
  Element = erlang:element(Idx, T),
  %% 匹配类型
  case Element of
    Element when erlang:is_number(Element) ->
      encode_tuples_2(T, Size, Idx + 1, <<Res/?bytes_t, Element:?u32_t>>);
    Element when erlang:is_binary(Element) ->
      encode_tuples_2(T, Size, Idx + 1, <<Res/?bytes_t, Element/?bytes_t>>);
    _ ->
      encode_tuples_2(T, Size, Idx + 1, Res)
  end.