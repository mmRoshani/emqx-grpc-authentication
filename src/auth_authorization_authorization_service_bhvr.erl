%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service auth_authorization.AuthorizationService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(auth_authorization_authorization_service_bhvr).

-callback mqtt_topic_authorization(grpc_auth_authorization_pb:mqtt_authorization(), grpc:metadata())
    -> {ok, grpc_auth_authorization_pb:topic_is_available(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

