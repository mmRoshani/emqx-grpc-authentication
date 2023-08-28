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

-spec mqtt_authenticate_cache_valid_topics(grpc_auth_authentication_pb:mqtt_authentication())
    -> {ok, grpc_auth_authentication_pb:auth_token(), grpc:metadata()}
     | {error, term()}.
mqtt_authenticate_cache_valid_topics(Req) ->
    mqtt_authenticate_cache_valid_topics(Req, #{}, #{}).

-spec mqtt_authenticate_cache_valid_topics(grpc_auth_authentication_pb:mqtt_authentication(), grpc:options())
    -> {ok, grpc_auth_authentication_pb:auth_token(), grpc:metadata()}
     | {error, term()}.
mqtt_authenticate_cache_valid_topics(Req, Options) ->
    mqtt_authenticate_cache_valid_topics(Req, #{}, Options).

-spec mqtt_authenticate_cache_valid_topics(grpc_auth_authentication_pb:mqtt_authentication(), grpc:metadata(), grpc_client:options())
    -> {ok, grpc_auth_authentication_pb:auth_token(), grpc:metadata()}
     | {error, term()}.
mqtt_authenticate_cache_valid_topics(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/auth_authentication.AuthenticationService/MqttAuthenticateCacheValidTopics">>,
                           mqtt_authentication, auth_token, <<"auth_authentication.MqttAuthentication">>),
                      Req, Metadata, Options).

