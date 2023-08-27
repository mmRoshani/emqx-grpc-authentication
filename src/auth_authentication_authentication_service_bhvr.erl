%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service auth_authentication.AuthenticationService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(auth_authentication_authentication_service_bhvr).

-callback token(grpc_auth_authentication_pb:auth_request(), grpc:metadata())
    -> {ok, grpc_auth_authentication_pb:auth_response(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback logout(grpc_auth_authentication_pb:access_token(), grpc:metadata())
    -> {ok, grpc_auth_authentication_pb:void(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback decrypt(grpc_auth_authentication_pb:access_token(), grpc:metadata())
    -> {ok, grpc_auth_authentication_pb:auth_token(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback mqtt(grpc_auth_authentication_pb:access_token(), grpc:metadata())
    -> {ok, grpc_auth_authentication_pb:mqtt_auth_result(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback password_otp(grpc_auth_authentication_pb:access_token(), grpc:metadata())
    -> {ok, grpc_auth_authentication_pb:code(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

-callback password_change(grpc_auth_authentication_pb:new_password_request(), grpc:metadata())
    -> {ok, grpc_auth_authentication_pb:void(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

