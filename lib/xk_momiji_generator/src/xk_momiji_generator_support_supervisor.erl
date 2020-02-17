%%%-------------------------------------------------------------------
%%% @author MMMGDZL
%%% @copyright (C) 2019, XF
%%% @doc
%%%
%%% @end
%%% Created : 30. 12月 2019 14:36
%%%-------------------------------------------------------------------
-module(xk_momiji_generator_support_supervisor).
-behaviour(supervisor).
%% API
-export([start_link/1]).
%% 监控器回调函数
-export([init/1]).
-define(SERVER, ?MODULE).
%%%===================================================================
%%% API函数
%%%===================================================================
start_link(Args) ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, Args).

%%%===================================================================
%%% 监控器回调函数
%%%===================================================================
init(Args) ->
    %启动子监听服务
    RestartStrategy = one_for_one,
    MaxRestarts = 1000,
    MaxSecondsBetweenRestarts = 3600,
    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},
    Restart = temporary,
    Shutdown = 2000,
    ChildList = [
        {xk_momiji_generator_maker, {xk_momiji_generator_maker, start_link, Args}, Restart, Shutdown, worker, [xk_momiji_generator_maker]}
    ],
    {ok, {SupFlags, ChildList}}.
%%%===================================================================
%%% 内部函数
%%%===================================================================