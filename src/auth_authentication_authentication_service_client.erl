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

-spec token(grpc_auth_authentication_pb:auth_request())
    -> {ok, grpc_auth_authentication_pb:auth_response(), grpc:metadata()}
     | {error, term()}.
token(Req) ->
    token(Req, #{}, #{}).

-spec token(grpc_auth_authentication_pb:auth_request(), grpc:options())
    -> {ok, grpc_auth_authentication_pb:auth_response(), grpc:metadata()}
     | {error, term()}.
token(Req, Options) ->
    token(Req, #{}, Options).

-spec token(grpc_auth_authentication_pb:auth_request(), grpc:metadata(), grpc_client:options())
    -> {ok, grpc_auth_authentication_pb:auth_response(), grpc:metadata()}
     | {error, term()}.
token(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/auth_authentication.AuthenticationService/Token">>,
                           auth_request, auth_response, <<"auth_authentication.AuthRequest">>),
                      Req, Metadata, Options).

-spec logout(grpc_auth_authentication_pb:access_token())
    -> {ok, grpc_auth_authentication_pb:void(), grpc:metadata()}
     | {error, term()}.
logout(Req) ->
    logout(Req, #{}, #{}).

-spec logout(grpc_auth_authentication_pb:access_token(), grpc:options())
    -> {ok, grpc_auth_authentication_pb:void(), grpc:metadata()}
     | {error, term()}.
logout(Req, Options) ->
    logout(Req, #{}, Options).

-spec logout(grpc_auth_authentication_pb:access_token(), grpc:metadata(), grpc_client:options())
    -> {ok, grpc_auth_authentication_pb:void(), grpc:metadata()}
     | {error, term()}.
logout(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/auth_authentication.AuthenticationService/Logout">>,
                           access_token, void, <<"auth_authentication.AccessToken">>),
                      Req, Metadata, Options).

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

-spec mqtt(grpc_auth_authentication_pb:access_token())
    -> {ok, grpc_auth_authentication_pb:mqtt_auth_result(), grpc:metadata()}
     | {error, term()}.
mqtt(Req) ->
    mqtt(Req, #{}, #{}).

-spec mqtt(grpc_auth_authentication_pb:access_token(), grpc:options())
    -> {ok, grpc_auth_authentication_pb:mqtt_auth_result(), grpc:metadata()}
     | {error, term()}.
mqtt(Req, Options) ->
    mqtt(Req, #{}, Options).

-spec mqtt(grpc_auth_authentication_pb:access_token(), grpc:metadata(), grpc_client:options())
    -> {ok, grpc_auth_authentication_pb:mqtt_auth_result(), grpc:metadata()}
     | {error, term()}.
mqtt(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/auth_authentication.AuthenticationService/Mqtt">>,
                           access_token, mqtt_auth_result, <<"auth_authentication.AccessToken">>),
                      Req, Metadata, Options).

-spec password_otp(grpc_auth_authentication_pb:access_token())
    -> {ok, grpc_auth_authentication_pb:code(), grpc:metadata()}
     | {error, term()}.
password_otp(Req) ->
    password_otp(Req, #{}, #{}).

-spec password_otp(grpc_auth_authentication_pb:access_token(), grpc:options())
    -> {ok, grpc_auth_authentication_pb:code(), grpc:metadata()}
     | {error, term()}.
password_otp(Req, Options) ->
    password_otp(Req, #{}, Options).

-spec password_otp(grpc_auth_authentication_pb:access_token(), grpc:metadata(), grpc_client:options())
    -> {ok, grpc_auth_authentication_pb:code(), grpc:metadata()}
     | {error, term()}.
password_otp(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/auth_authentication.AuthenticationService/PasswordOtp">>,
                           access_token, code, <<"auth_authentication.AccessToken">>),
                      Req, Metadata, Options).

-spec password_change(grpc_auth_authentication_pb:new_password_request())
    -> {ok, grpc_auth_authentication_pb:void(), grpc:metadata()}
     | {error, term()}.
password_change(Req) ->
    password_change(Req, #{}, #{}).

-spec password_change(grpc_auth_authentication_pb:new_password_request(), grpc:options())
    -> {ok, grpc_auth_authentication_pb:void(), grpc:metadata()}
     | {error, term()}.
password_change(Req, Options) ->
    password_change(Req, #{}, Options).

-spec password_change(grpc_auth_authentication_pb:new_password_request(), grpc:metadata(), grpc_client:options())
    -> {ok, grpc_auth_authentication_pb:void(), grpc:metadata()}
     | {error, term()}.
password_change(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/auth_authentication.AuthenticationService/PasswordChange">>,
                           new_password_request, void, <<"auth_authentication.NewPasswordRequest">>),
                      Req, Metadata, Options).

