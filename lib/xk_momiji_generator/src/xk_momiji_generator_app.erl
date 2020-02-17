-module(xk_momiji_generator_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%% ===================================================================
%% Application callbacks
%% ===================================================================

start(_Type, Args) ->
	xk_momiji_generator_support_supervisor:start_link(Args).

stop(_State) ->
	ok.
