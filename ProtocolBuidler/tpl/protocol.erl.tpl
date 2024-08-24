%%%-------------------------------------------------------------------
%%% 该文件由程序生成, 按照协议来处理改动
%%%-------------------------------------------------------------------
-module(___TPL_FILE___).
-author("MeteorCat").
-include("constant.hrl").
-include("protocols.hrl").


%% API
-export([
  request/3
]).


%% @doc 请求入库
request(Id, Session, Bytes) ->
  io:format("[Proto(~w)] Request ~w~n", [Id, Bytes]),
  {?ok, Session}.