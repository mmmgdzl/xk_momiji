-module(xk_momiji_generator).
-export([start/0]).

start() ->
	ApplicationList = [crypto, asn1, public_key, ssl, emysql, xk_momiji_generator],
	F = fun(ApplicationName) ->
			io:format("Starting application : ~p~n", [ApplicationName]),
			ok = application:start(ApplicationName),
			io:format("Application started : ~p~n", [ApplicationName])
		end,
	lists:foreach(F, ApplicationList).