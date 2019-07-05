-module(vernemq_demo_plugin).

-behaviour(auth_on_register_hook).
-behaviour(auth_on_subscribe_hook).
-behaviour(auth_on_publish_hook).

-export([auth_on_register/5,
         auth_on_publish/6,
         auth_on_subscribe/3]).

%% This file demonstrates the hooks you typically want to use
%% if your plugin deals with Authentication or Authorization.
%%
%% All it does is:
%%  - authenticate every user and write the log
%%  - authorize every PUBLISH and SUBSCRIBE and write it to the log
%%
%% You don't need to implement all of these hooks, just the one
%% needed for your use case.
%%
%% IMPORTANT:
%%  these hook functions run in the session context
%%
auth_on_register({_IpAddr, _Port} = Peer, {_MountPoint, _ClientId} = SubscriberId, UserName, Password, CleanSession) ->
    error_logger:info_msg("auth_on_register: ~p ~p ~p ~p ~p", [Peer, SubscriberId, UserName, Password, CleanSession]),

    Client = aws_client:make_client(<<>>, <<>>, <<"ap-southeast-2">>),

    Login = aws_cognito_idp:initiate_auth(Client, #{<<"AuthFlow">> => <<"USER_PASSWORD_AUTH">>,
                                                    <<"ClientId">> => <<"5ltsi83d284v80775qvv46l370">>,
                                                    <<"AuthParameters">> => #{<<"USERNAME">> => UserName,
                                                                              <<"PASSWORD">> => Password}}, []),
    case Login of
        {ok, _, _} -> ok;
        {error, _, _} -> error;
        {error, _} -> error
    end.

    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> CONNECT is authenticated
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {ok, [{ModifierKey, NewVal}...]} -> CONNECT is authenticated, but we might want to set some options used throughout the client session:
    %%      - {mountpoint, NewMountPoint::string}
    %%      - {clean_session, NewCleanSession::boolean}
    %% 4. return {error, invalid_credentials} -> CONNACK_CREDENTIALS is sent
    %% 5. return {error, whatever} -> CONNACK_AUTH is sent

    %% we return 'ok'

auth_on_publish(UserName, {_MountPoint, _ClientId} = SubscriberId, QoS, Topic, Payload, IsRetain) ->
    error_logger:info_msg("auth_on_publish: ~p ~p ~p ~p ~p ~p", [UserName, SubscriberId, QoS, Topic, Payload, IsRetain]),
    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> PUBLISH is authorized
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {ok, NewPayload::binary} -> PUBLISH is authorized, but we changed the payload
    %% 4. return {ok, [{ModifierKey, NewVal}...]} -> PUBLISH is authorized, but we might have changed different Publish Options:
    %%     - {topic, NewTopic::string}
    %%     - {payload, NewPayload::binary}
    %%     - {qos, NewQoS::0..2}
    %%     - {retain, NewRetainFlag::boolean}
    %% 5. return {error, whatever} -> auth chain is stopped, and message is silently dropped (unless it is a Last Will message)
    %%
    %% we return 'ok'
    ok.

auth_on_subscribe(UserName, ClientId, [{_Topic, _QoS}|_] = Topics) ->
    error_logger:info_msg("auth_on_subscribe: ~p ~p ~p", [UserName, ClientId, Topics]),
    %% do whatever you like with the params, all that matters
    %% is the return value of this function
    %%
    %% 1. return 'ok' -> SUBSCRIBE is authorized
    %% 2. return 'next' -> leave it to other plugins to decide
    %% 3. return {error, whatever} -> auth chain is stopped, and no SUBACK is sent

    %% we return 'ok'
    ok.
