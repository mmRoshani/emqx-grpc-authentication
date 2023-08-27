-module(emqx_grpc_authentication_app).

-behaviour(application).

-emqx_plugin(?MODULE).

-export([ start/2
        , stop/1
        ]).

start(_StartType, _StartArgs) ->
    {ok, Sup} = emqx_grpc_authentication_sup:start_link(),
  emqx_grpc_authentication:load(application:get_all_env()),

    emqx_ctl:register_command(emqx_grpc_authentication, {emqx_grpc_authentication_cli, cmd}),
    {ok, Sup}.

stop(_State) ->
    emqx_ctl:unregister_command(emqx_grpc_authentication),
    emqx_grpc_authentication:unload().
