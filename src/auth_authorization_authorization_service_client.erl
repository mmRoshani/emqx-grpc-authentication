%%%-------------------------------------------------------------------
%% @doc Client module for grpc service auth_authorization.AuthorizationService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(auth_authorization_authorization_service_client).

-compile(export_all).
-compile(nowarn_export_all).

-include_lib("grpc/include/grpc.hrl").

-define(SERVICE, 'auth_authorization.AuthorizationService').
-define(PROTO_MODULE, 'grpc_auth_authorization_pb').
-define(MARSHAL(T), fun(I) -> ?PROTO_MODULE:encode_msg(I, T) end).
-define(UNMARSHAL(T), fun(I) -> ?PROTO_MODULE:decode_msg(I, T) end).
-define(DEF(Path, Req, Resp, MessageType),
        #{path => Path,
          service =>?SERVICE,
          message_type => MessageType,
          marshal => ?MARSHAL(Req),
          unmarshal => ?UNMARSHAL(Resp)}).

-spec mqtt_topic_authorization(grpc_auth_authorization_pb:mqtt_authorization())
    -> {ok, grpc_auth_authorization_pb:topic_is_available(), grpc:metadata()}
     | {error, term()}.
mqtt_topic_authorization(Req) ->
    mqtt_topic_authorization(Req, #{}, #{}).

-spec mqtt_topic_authorization(grpc_auth_authorization_pb:mqtt_authorization(), grpc:options())
    -> {ok, grpc_auth_authorization_pb:topic_is_available(), grpc:metadata()}
     | {error, term()}.
mqtt_topic_authorization(Req, Options) ->
    mqtt_topic_authorization(Req, #{}, Options).

-spec mqtt_topic_authorization(grpc_auth_authorization_pb:mqtt_authorization(), grpc:metadata(), grpc_client:options())
    -> {ok, grpc_auth_authorization_pb:topic_is_available(), grpc:metadata()}
     | {error, term()}.
mqtt_topic_authorization(Req, Metadata, Options) ->
    grpc_client:unary(?DEF(<<"/auth_authorization.AuthorizationService/MqttTopicAuthorization">>,
                           mqtt_authorization, topic_is_available, <<"auth_authorization.MqttAuthorization">>),
                      Req, Metadata, Options).

