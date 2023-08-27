%%%-------------------------------------------------------------------
%%% @author mmroshani
%%% @copyright (C) 2023, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 27. Aug 2023 11:31 PM
%%%-------------------------------------------------------------------
-module(emqx_grpc_authentication_svr).
-author("mmroshani").


-behavior(emqxgrpcauthentication_emqx_grpc_authentication_svr_bhvr).

-compile(export_all).
-compile(nowarn_export_all).

-define(LOG(Fmt, Args), io:format(standard_error, "[Svr] " ++ Fmt, Args)).

%%--------------------------------------------------------------------
%% Callbacks

get_feature(Request, _Md) ->
  ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Request]),
  {ok, #{}, _Md}.

list_features(Stream, _Md) ->
  {eos, [Request], NStream} = grpc_stream:recv(Stream),
  ?LOG("~p: ~0p~n", [?FUNCTION_NAME, Request]),

  grpc_stream:reply(Stream, [#{name => "City1", location => #{latitude => 1, longitude => 1}}]),
  grpc_stream:reply(Stream, [#{name => "City2", location => #{latitude => 2, longitude => 2}}]),
  grpc_stream:reply(Stream, [#{name => "City3", location => #{latitude => 3, longitude => 3}}]),
  {ok, NStream}.
