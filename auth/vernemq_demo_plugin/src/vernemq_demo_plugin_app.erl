-module(vernemq_demo_plugin_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_StartType, _StartArgs) ->
    vernemq_demo_plugin_sup:start_link().

stop(_State) ->
    ok.
