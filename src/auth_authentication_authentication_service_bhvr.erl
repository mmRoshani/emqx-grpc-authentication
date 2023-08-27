%%%-------------------------------------------------------------------
%% @doc Behaviour to implement for grpc service auth_authentication.AuthenticationService.
%% @end
%%%-------------------------------------------------------------------

%% this module was generated and should not be modified manually

-module(auth_authentication_authentication_service_bhvr).

-callback decrypt(grpc_auth_authentication_pb:access_token(), grpc:metadata())
    -> {ok, grpc_auth_authentication_pb:auth_token(), grpc:metadata()}
     | {error, grpc_stream:error_response()}.

