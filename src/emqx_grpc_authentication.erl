-module(emqx_grpc_authentication).


-behaviour(supervisor).
-behaviour(application).

-include_lib("emqx/include/emqx.hrl").
-include_lib("emqx/include/emqx_hooks.hrl").
-include_lib("emqx/include/logger.hrl").

-export([ load/1
        , unload/0
        ]).

%% Client Lifecycle Hooks
-export([ on_client_connect/3
        , on_client_connack/4
        , on_client_connected/3
        , on_client_disconnected/4
        , on_client_authenticate/3
        , on_client_authorize/5
        , on_client_subscribe/4
        , on_client_unsubscribe/4
        ]).

%% Session Lifecycle Hooks
-export([ on_session_created/3
        , on_session_subscribed/4
        , on_session_unsubscribed/4
        , on_session_resumed/3
        , on_session_discarded/3
        , on_session_takenover/3
        , on_session_terminated/4
        ]).

%% Message Pubsub Hooks
-export([ on_message_publish/2
        , on_message_delivered/3
        , on_message_acked/3
        , on_message_dropped/4
        ]).

%% -----
%% gRPC
%% -----
-export([init/1]).
-export([start_services/0, start_client_channel/0,
  stop_services/0, stop_client_channel/0]).

%% -------
%% defines
%% -------

-define(SERVER_NAME, authentication).
-define(AUTHENTICATION_CHANN_NAME,  channel1).
-define(AUTHORIZATION_CHANN_NAME,  channel2).

%% ----------------
%% Custom functions
%% ----------------
start_services() ->
  Services = #{protos => [grpc_auth_authentication_pb],
    services => #{'auth_authentication.AuthenticationService' => emqx_grpc_authentication_svr}
  },
  Options = [],
  {ok, _} = grpc:start_server(?SERVER_NAME, 5005, Services, Options),
  io:format("Start service ~s on 5005 successfully!~n", [?SERVER_NAME]).

start_client_channel() ->
  ClientOps = #{},
  AuthenticationSvrAddr = "http://192.168.111.242:5076",
  AuthorizationSvrAddr = "http://192.168.111.242:5087",
  {ok, _} = grpc_client_sup:create_channel_pool(
    ?AUTHENTICATION_CHANN_NAME,
    AuthenticationSvrAddr,
    ClientOps
  ),
  io:format("Start client channel ~s for ~s successfully! :)~n~n"
  "Call the 'auth_authentication_authentication_service_client' module exported functions "
  "to use it. e.g:~n",
    [?AUTHENTICATION_CHANN_NAME, AuthenticationSvrAddr]),
  {ok, _} = grpc_client_sup:create_channel_pool(
  ?AUTHORIZATION_CHANN_NAME,
  AuthorizationSvrAddr,
  ClientOps
  ),
  io:format("Start client channel ~s for ~s successfully! :)~n~n"
  "Call the 'auth_authorization_authorization_service_client' module exported functions "
  "to use it. e.g:~n",
    [?AUTHORIZATION_CHANN_NAME, AuthorizationSvrAddr]).

stop_services() ->
  grpc:stop_server(?SERVER_NAME).

stop_client_channel() ->
  io:format("client Channel close! :("),
  grpc_client_sup:stop_channel_pool(?AUTHENTICATION_CHANN_NAME),
  grpc_client_sup:stop_channel_pool(?AUTHORIZATION_CHANN_NAME).

%%--------------------------------------------------------------------
%% callbacks for supervisor

init([]) ->
  SupFlags = #{strategy => one_for_all,
    intensity => 0,
    period => 1},
  ChildSpecs = [],
  {ok, {SupFlags, ChildSpecs}}.


%% ---------------------
%% plugins functionality
%% ---------------------

%% Called when the plugin application start
load(Env) ->
%%    hook('client.connect',      {?MODULE, on_client_connect, [Env]}),
%%    hook('client.connack',      {?MODULE, on_client_connack, [Env]}),
%%    hook('client.connected',    {?MODULE, on_client_connected, [Env]}),
%%    hook('client.disconnected', {?MODULE, on_client_disconnected, [Env]}),
    stop_client_channel(), %% Close be fore starting new one since the old one might not closed due to some problems
    start_client_channel(),
    hook('client.authenticate', {?MODULE, on_client_authenticate, [Env]}),
    hook('client.authorize',    {?MODULE, on_client_authorize, [Env]}).
%%    hook('client.subscribe',    {?MODULE, on_client_subscribe, [Env]}),
%%    hook('client.unsubscribe',  {?MODULE, on_client_unsubscribe, [Env]}),
%%    hook('session.created',     {?MODULE, on_session_created, [Env]}),
%%    hook('session.subscribed',  {?MODULE, on_session_subscribed, [Env]}),
%%    hook('session.unsubscribed',{?MODULE, on_session_unsubscribed, [Env]}),
%%    hook('session.resumed',     {?MODULE, on_session_resumed, [Env]}),
%%    hook('session.discarded',   {?MODULE, on_session_discarded, [Env]}),
%%    hook('session.takenover',   {?MODULE, on_session_takenover, [Env]}),
%%    hook('session.terminated',  {?MODULE, on_session_terminated, [Env]}),
%%    hook('message.publish',     {?MODULE, on_message_publish, [Env]}),
%%    hook('message.delivered',   {?MODULE, on_message_delivered, [Env]}),
%%    hook('message.acked',       {?MODULE, on_message_acked, [Env]}),
%%    hook('message.dropped',     {?MODULE, on_message_dropped, [Env]}).

%%--------------------------------------------------------------------
%% Client Lifecycle Hooks
%%--------------------------------------------------------------------

on_client_connect(ConnInfo, Props, _Env) ->
    %% this is to demo the usage of EMQX's structured-logging macro
    %% * Recommended to always have a `msg` field,
    %% * Use underscore instead of space to help log indexers,
    %% * Try to use static fields
    ?SLOG(debug, #{msg => "demo_log_msg_on_client_connect",
                   conninfo => ConnInfo,
                   props => Props}),
    %% If you want to refuse this connection, you should return with:
    %% {stop, {error, ReasonCode}}
    %% the ReasonCode can be found in the emqx_reason_codes.erl
    {ok, Props}.

on_client_connack(ConnInfo = #{clientid := ClientId}, Rc, Props, _Env) ->
    io:format("Client(~s) connack, ConnInfo: ~p, Rc: ~p, Props: ~p~n",
              [ClientId, ConnInfo, Rc, Props]),
    {ok, Props}.

on_client_connected(ClientInfo = #{clientid := ClientId}, ConnInfo, _Env) ->
    io:format("Client(~s) connected, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ClientInfo, ConnInfo]).

on_client_disconnected(ClientInfo = #{clientid := ClientId}, ReasonCode, ConnInfo, _Env) ->
  io:format("Client(~s) disconnected due to ~p, ClientInfo:~n~p~n, ConnInfo:~n~p~n",
              [ClientId, ReasonCode, ClientInfo, ConnInfo]).

%% Username is the token JWT token that user has been already fetched from third party server
on_client_authenticate(ClientInfo = #{clientid := ClientId, username := Username, password := Password}, Result, Env) ->
  %% Fetch third party server access token
  Response = auth_authentication_authentication_service_client:mqtt_authenticate_cache_valid_topics(#{
    access_token => Password,
    active_session => ClientId,
    mqtt_username => Username},
    #{channel => ?AUTHENTICATION_CHANN_NAME
    }),
  Success = grpc_checker:is_grpc_call_successful(Response),
  io:format("gRPC call successful: ~p~n", [Success]),

  %% Continue if RPC call is successfully
  case Success of
    true ->
      {ok, Result}; %% Check the suitable `Result` value
    false ->
      io:format("Error: ~p~n", [Response]),
      {stop, {error, banned}}
  end.

on_client_authorize(ClientInfo = #{clientid := ClientId, username := Username}, PubSub, Topic, Result, Env) ->
  io:format("Client(~s) with username(~s) try to authorize on topic(~s)",
    [ClientId,Username,  Topic]),

  %% Fetch third party server access token
  Response = auth_authorization_authorization_service_client:mqtt_topic_authorization(#{
    topic => Topic,
    active_session => ClientId,
    mqtt_username => Username},
    #{channel => ?AUTHORIZATION_CHANN_NAME
    }),
  Success = grpc_checker:is_grpc_call_successful(Response),
  io:format("gRPC call successful: ~p~n", [Success]),


  %% Continue if RPC call is successfully
  case Success of
    true ->
      {ok, #{ available := Value }, _} = Response,
      is_one(Value);
    false ->
      io:format("Error: ~p~n", [Response]),
      {stop, #{result => deny, from => default}}
  end.

on_client_subscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    io:format("Client(~s) will subscribe: ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

on_client_unsubscribe(#{clientid := ClientId}, _Properties, TopicFilters, _Env) ->
    io:format("Client(~s) will unsubscribe ~p~n", [ClientId, TopicFilters]),
    {ok, TopicFilters}.

%%--------------------------------------------------------------------
%% Session Lifecycle Hooks
%%--------------------------------------------------------------------

on_session_created(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) created, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_subscribed(#{clientid := ClientId}, Topic, SubOpts, _Env) ->
    io:format("Session(~s) subscribed ~s with subopts: ~p~n", [ClientId, Topic, SubOpts]).

on_session_unsubscribed(#{clientid := ClientId}, Topic, Opts, _Env) ->
    io:format("Session(~s) unsubscribed ~s with opts: ~p~n", [ClientId, Topic, Opts]).

on_session_resumed(#{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) resumed, Session Info:~n~p~n", [ClientId, SessInfo]).

on_session_discarded(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is discarded. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_takenover(_ClientInfo = #{clientid := ClientId}, SessInfo, _Env) ->
    io:format("Session(~s) is takenover. Session Info: ~p~n", [ClientId, SessInfo]).

on_session_terminated(_ClientInfo = #{clientid := ClientId}, Reason, SessInfo, _Env) ->
    io:format("Session(~s) is terminated due to ~p~nSession Info: ~p~n",
              [ClientId, Reason, SessInfo]).

%%--------------------------------------------------------------------
%% Message PubSub Hooks
%%--------------------------------------------------------------------

%% Transform message and return
on_message_publish(Message = #message{topic = <<"$SYS/", _/binary>>}, _Env) ->
    {ok, Message};

on_message_publish(Message, _Env) ->
    io:format("Publish ~p~n", [emqx_message:to_map(Message)]),
    {ok, Message}.

on_message_dropped(#message{topic = <<"$SYS/", _/binary>>}, _By, _Reason, _Env) ->
    ok;
on_message_dropped(Message, _By = #{node := Node}, Reason, _Env) ->
    io:format("Message dropped by node ~p due to ~p:~n~p~n",
              [Node, Reason, emqx_message:to_map(Message)]).

on_message_delivered(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message delivered to client(~s):~n~p~n",
              [ClientId, emqx_message:to_map(Message)]),
    {ok, Message}.

on_message_acked(_ClientInfo = #{clientid := ClientId}, Message, _Env) ->
    io:format("Message acked by client(~s):~n~p~n",
              [ClientId, emqx_message:to_map(Message)]).

%% Called when the plugin application stop
unload() ->
  stop_client_channel(),
%%    unhook('client.connect',      {?MODULE, on_client_connect}),
%%    unhook('client.connack',      {?MODULE, on_client_connack}),
%%    unhook('client.connected',    {?MODULE, on_client_connected}),
%%    unhook('client.disconnected', {?MODULE, on_client_disconnected}),
    unhook('client.authenticate', {?MODULE, on_client_authenticate}),
    unhook('client.authorize',    {?MODULE, on_client_authorize}),
%%    unhook('client.subscribe',    {?MODULE, on_client_subscribe}),
%%    unhook('client.unsubscribe',  {?MODULE, on_client_unsubscribe}),
%%    unhook('session.created',     {?MODULE, on_session_created}),
%%    unhook('session.subscribed',  {?MODULE, on_session_subscribed}),
%%    unhook('session.unsubscribed',{?MODULE, on_session_unsubscribed}),
%%    unhook('session.resumed',     {?MODULE, on_session_resumed}),
%%    unhook('session.discarded',   {?MODULE, on_session_discarded}),
%%    unhook('session.takenover',   {?MODULE, on_session_takenover}),
%%    unhook('session.terminated',  {?MODULE, on_session_terminated}),
%%    unhook('message.publish',     {?MODULE, on_message_publish}),
%%    unhook('message.delivered',   {?MODULE, on_message_delivered}),
%%    unhook('message.acked',       {?MODULE, on_message_acked}),
    unhook('message.dropped',     {?MODULE, on_message_dropped}).

hook(HookPoint, MFA) ->
    %% use highest hook priority so this module's callbacks
    %% are evaluated before the default hooks in EMQX
    emqx_hooks:add(HookPoint, MFA, _Property = ?HP_HIGHEST).

unhook(HookPoint, MFA) ->
    emqx_hooks:del(HookPoint, MFA).


is_one(<<"1">>) ->
  {ok, #{result => allow, from => default}};
is_one(_) ->
  {stop, #{result => deny, from => default}}.