%%%-------------------------------------------------------------------
%%% @author MeteorCat
%%% @copyright (C) 2024, MeteorCat
%%% @doc
%%% 转化方法
%%% @end
%%%-------------------------------------------------------------------
-module(convert_utils).
-author("MeteorCat").
-include("constant.hrl").


%% API
-export([
  list2atom/1,
  to_list/1,
  list2string/4,
  string2term/1,
  term2bitstring/1,
  bitstring2term/1,
  term2string/1,
  to_atom/1,
  to_binary/1,
  to_float/1,
  to_integer/1,
  to_tuple/1,
  term2binary/1,
  binary2term/1,
  binary2hex/1,
  list2hex/1
]).

%% @doc List转换Atom
-spec list2atom(list()) -> term().
list2atom(L) ->
  try erlang:list_to_existing_atom(L)
  catch _:_ -> erlang:list_to_atom(L) end.


%% @doc list 转换方法
to_list(V) when erlang:is_list(V) -> V;
to_list(V) when erlang:is_tuple(V) ->
  erlang:tuple_to_list(V);
to_list(V) when erlang:is_atom(V) ->
  erlang:atom_to_list(V);
to_list(V) when erlang:is_binary(V) ->
  erlang:binary_to_list(V);
to_list(V) when erlang:is_integer(V) ->
  erlang:integer_to_list(V);
to_list(V) when erlang:is_float(V) ->
  erlang:float_to_list(V);
to_list(_) -> [].


%% @doc 数组转字符串
%% H 附加在开头
%% M 夹在中间
%% T 附加在尾部
list2string([], _, _, _) -> [];
list2string([Head | Tail], H, M, T) ->
  list2string(Tail, H, M, T, Head ++ to_list(Head)).
list2string([], _, _, T, Str) -> Str ++ T;
list2string([Head | Tail], H, M, T, Str) ->
  list2string(Tail, H, M, T, Str ++ M ++ to_list(Head)).


%%% @doc term序列化, term转换为string格式
%%% [{a},1] => "[{a},1]"
term2string(Term) -> binary_to_list(term2bitstring(Term)).
string2term(Str) ->
  case erl_scan:string(Str ++ ".") of
    {?ok, Token, _} ->
      case erl_parse:parse_term(Token) of
        {?ok, Term} -> Term;
        _ -> ?null
      end;
    _ -> ?null
  end.


%%% @doc Term 序列化, term转换为bitstring格式
%%% [{a},1] => <<"[{a},1]">>
term2bitstring(Term) -> erlang:iolist_to_binary(io_lib:write(Term)).
bitstring2term(?undefined) -> ?undefined;
bitstring2term(BitStr) -> string2term(erlang:binary_to_list(BitStr)).

%%% @doc atom 转换方法
to_atom(V) when erlang:is_atom(V) -> V;
to_atom(V) when erlang:is_binary(V) ->
  list2atom(erlang:binary_to_list(V));
to_atom(V) when erlang:is_integer(V) ->
  list2atom(erlang:integer_to_list(V));
to_atom(V) when erlang:is_float(V) ->
  list2atom(erlang:float_to_list(V));
to_atom(V) when erlang:is_tuple(V) ->
  list2atom(erlang:tuple_to_list(V));
to_atom(V) when erlang:is_list(V) ->
  V2 = erlang:list_to_binary(V),
  V3 = erlang:binary_to_list(V2),
  list2atom(V3);
to_atom(_) -> list2atom("").


%%%  @doc binary 转换方法
to_binary(V) when erlang:is_binary(V) -> V;
to_binary(V) when erlang:is_atom(V) ->
  erlang:list_to_binary(erlang:atom_to_list(V));
to_binary(V) when erlang:is_list(V) ->
  erlang:list_to_binary(V);
to_binary(V) when erlang:is_integer(V) ->
  erlang:list_to_binary(integer_to_list(V));
to_binary(V) when erlang:is_float(V) ->
  erlang:list_to_binary(float_to_list(V));
to_binary(_) -> <<>>.


%%% float 转化方法
to_float(V) -> list_to_float(?MODULE:to_list(V)).


%%% integer 转化方法
to_integer(V) when erlang:is_integer(V) -> V;
to_integer(V) when erlang:is_binary(V) ->
  V2 = erlang:binary_to_list(V),
  erlang:list_to_integer(V2);
to_integer(V) when erlang:is_list(V) ->
  erlang:list_to_integer(V);
to_integer(_) -> 0.


%% @doc tuple 转化方法
to_tuple(T) when erlang:is_tuple(T) -> T;
to_tuple(T) when erlang:is_list(T) ->
  erlang:list_to_tuple(T);
to_tuple(T) -> {T}.


%% @doc 结构体序列化
-spec term2binary(term()) -> erlang:ext_binary().
term2binary(Term) -> erlang:term_to_binary(Term).

%% @doc 结构体反序列化
-spec binary2term(erlang:ext_binary()) -> term().
binary2term(Bytes) -> erlang:binary_to_term(Bytes).

%% 二进制转16进制字符
-spec binary2hex(bitstring()) -> list().
binary2hex(Bin) ->
  List = binary_to_list(Bin),
  lists:flatten(list2hex(List)).

%% 列表转16进制字符
-spec list2hex(list()) -> list().
list2hex(L) ->
  lists:map(fun(X) -> int2hex(X) end, L).

%% Char值转16进制字符
int2hex(N) when N < 256 ->
  [hex(N div 16), hex(N rem 16)].
hex(N) when N < 10 ->
  $0 + N;
hex(N) when N >= 10, N < 16 ->
  $a + (N - 10).
