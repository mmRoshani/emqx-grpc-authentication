%%%-------------------------------------------------------------------
%%% @author mmroshani
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 28. Aug 2023 12:31 AM
%%%-------------------------------------------------------------------
-module(grpc_checker).
-author("mmroshani").

%% API
-export([is_grpc_call_successful/1, get_grpc_status/1, is_zero/1]).


is_grpc_call_successful(Result) ->
  case Result of
    {ok, Props, Headers} ->
      is_zero(grpc_response_code(Headers));
    _ ->
      false
  end.

grpc_response_code(Headers) ->
  case get_grpc_status(Headers)  of
    undefined -> -1;
    Code -> Code
  end.

get_grpc_status([{<<"grpc-status">>, Value} | _]) -> Value;
get_grpc_status([_ | Rest]) -> get_grpc_status(Rest);
get_grpc_status([]) -> undefined.

is_zero(<<"0">>) ->
  true;
is_zero(_) ->
  false.