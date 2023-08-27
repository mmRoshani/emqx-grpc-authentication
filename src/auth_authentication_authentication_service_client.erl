%%%-------------------------------------------------------------------
%% @doc Client module for grpc service auth_authentication.AuthenticationService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(auth_authentication_authentication_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpc/include/grpc.hrl").

-define(SERVICE, 'auth_authentication.AuthenticationService').
-define(PROTO_MODULE, 'grpc_auth_authentication_pb').
-define(MARSHAL(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Path, Req, Resp, MessageType),
        #{path => Path,
          service =>?SERVICE,
          message_type => MessageType,
          marshal => ?MARSHAL(Req),
          unmarshal => ?UNMARSHAL(Resp)}).

-spec decrypt(grpc_auth_authentication_pb:access_token())
    -> {ok, grpc_auth_authentication_pb:auth_token(), grpc:metadata()}
     | {error, term()}.
decrypt(Req) ->
    decrypt(Req, #{}, #{}).

-spec decrypt(grpc_auth_authentication_pb:access_token(), grpc:options())
    -> {ok, grpc_auth_authentication_pb:auth_token(), grpc:metadata()}
     | {error, term()}.
decrypt(Req, Options) ->
    decrypt(Req, #{}, Options).

-spec decrypt(grpc_auth_authentication_pb:access_token(), grpc:metadata(), grpc_client:options())
    -> {ok, grpc_auth_authentication_pb:auth_token(), grpc:metadata()}
     | {error, term()}.
decrypt(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/auth_authentication.AuthenticationService/Decrypt">>,
                           access_token, auth_token, <<"auth_authentication.AccessToken">>),
                      Req, Metadata, Options).

