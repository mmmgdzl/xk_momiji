%%%-------------------------------------------------------------------
%%% @author mmmgdzl
%%% @copyright (C) 2020, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 15. 二月 2020 0:59
%%%-------------------------------------------------------------------
-module(xk_momiji_generator_maker).
-author("mmmgdzl").

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1,
    handle_call/3,
    handle_cast/2,
    handle_info/2,
    terminate/2,
    code_change/3]).

-include("emysql.hrl").

-define(SERVER, ?MODULE).
-define(MYSQL_POOL_ID, xk_momiji_pool).
-define(MYSQL_POOL_SIZE, 10).

-record(state, {
    database,
    table_matcher
}).

-record(table_column, {column_name, data_type, column_comment}).
-define(TABLE_COLUMNS_FIELDS, [column_name, data_type, column_comment]).

%%%===================================================================
%%% API
%%%===================================================================
start_link(Args) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Args], []).

init([Args]) ->
    io:format("~p~n", [Args]),
    %% 创建数据库连接池
    emysql:add_pool(?MYSQL_POOL_ID, ?MYSQL_POOL_SIZE,
        maps:get(username, Args, "root"), maps:get(password, Args, "root"), maps:get(host, Args, "127.0.0.1"),
        maps:get(port, Args, 3306), maps:get(database, Args), maps:get(code, Args, utf8)),
    erlang:send_after(100, self(), {start}),
    {ok, #state{
        database = maps:get(database, Args),
        table_matcher = maps:get(table_matcher, Args)
    }}.

handle_call(_Request, _From, State) ->
    {reply, ok, State}.

handle_cast(_Request, State) ->
    {noreply, State}.

handle_info({start}, State) ->
    c:pwd(),
    case State#state.table_matcher of
        [] -> error_logger:error_msg("至少要有一条表匹配表达式! 请在修改后重新运行.~n");
        _ ->
            %% 获取所有匹配的表名称
            TableNameResult = emysql:execute(?MYSQL_POOL_ID, <<"SELECT table_name FROM INFORMATION_SCHEMA.TABLES WHERE table_schema='", (list_to_binary(State#state.database))/binary, "' AND (", (create_like(State#state.table_matcher))/binary, ")">>),
            TableNames = lists:map(fun([TableName]) -> TableName end, TableNameResult#result_packet.rows),
            io:format("匹配到表格:~p~n", [TableNames]),
            %% 对于每张表获取字段名称
            TableInfos = lists:map(
                fun(TableName) ->
                    TableColumnResult = emysql:execute(?MYSQL_POOL_ID, <<"SELECT column_name,column_key,data_type,column_comment FROM information_schema.columns WHERE table_schema='", (list_to_binary(State#state.database))/binary, "' AND table_name = '", TableName/binary, "'">>),
                    TableColumns = emysql:as_record(TableColumnResult, table_column, ?TABLE_COLUMNS_FIELDS),
                    lists:foreach(fun(TableColumn) ->
                            io:format("表格~p字段~p~n", [TableName, TableColumn#table_column.column_name])
                        end, TableColumns),
                    PrimaryKeyName = lists:foldl(
                        fun(Row, Acc) ->
                            case lists:nth(2, Row) of
                                <<"PRI">> ->
                                    lists:nth(1, Row);
                                _ ->
                                    Acc
                            end
                        end, undefined, TableColumnResult#result_packet.rows
                    ),
                    {TableName, TableColumns, PrimaryKeyName}
                end, TableNames),
            create_record(TableInfos),
            create_data(TableInfos),
            io:format("导出完成, 请关闭窗口~n")
    end,
    {noreply, State};
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
create_like(TableNames) ->
    create_like(<<"">>, TableNames).
create_like(Binary, [TableName]) ->
    <<Binary/binary, " table_name LIKE '", (list_to_binary(TableName))/binary, "'">>;
create_like(Binary, [TableName | TableNames]) ->
    create_like(<<Binary/binary, " table_name LIKE '", (list_to_binary(TableName))/binary, "' OR ">>, TableNames).

%% 输出xk_momiji_record.hrl
create_record(TableInfos) ->
    io:format("开始导出xk_momiji_record.hrl~n"),
    file:make_dir("export"),
    {ok, S} = file:open("export/xk_momiji_record.hrl", [write]),
    %% 编写文件头
    file:write(S, <<"-author(\"mmmgdzl\").\n\n"/utf8>>),
    file:write(S, <<"%% 表格信息
-record(table_info, {
    name, %% 表格名称
    primary_key_name, %% 主键名称
    fields %% 表格字段列表
}).

%% 排序标准
-record(order_by_info, {
    order %% 排序标准 Binary
}).

%% 分页
-record(page_info, {
    page, %% 页数, Integer
    limit %% 页大小 Integer
}).\n\n"/utf8>>),
    %% 编写表格信息
    lists:foreach(
        fun({TableName, TableColumns, _PrimaryKeyName}) ->
            TableNameStr = binary_to_list(TableName),
            file:write(S, "-define(TABLE_" ++ (string:to_upper(TableNameStr)) ++ ", " ++ TableNameStr ++ ").\n"),
            file:write(S, "-record(" ++ TableNameStr ++ ",{" ++ lists:join(", ", lists:map(fun(TableColumn) -> TableColumn#table_column.column_name end, TableColumns)) ++ "}).\n\n")
        end, TableInfos),
    file:close(S),
    io:format("导出xk_momiji_record.hrl完成~n").

%% 输出xk_momiji_data.erl
create_data(TableInfos) ->
    io:format("开始导出xk_momiji_data.erl~n"),
    {ok, S} = file:open("export/xk_momiji_data.erl", [write]),
    %% 编写文件头
    file:write(S, <<"-module(xk_momiji_data).
-author(\"mmmgdzl\").

-include(\"xk_momiji_record.hrl\").

%% API
-export([
    get_info/1,
    get_all_table/0
]).\n\n"/utf8>>),
    %% 编写每个表格信息
    Size = erlang:length(TableInfos),
    {Result, _} = lists:foldl(
        fun({TableName, TableColumns, PrimaryKeyName}, {Acc, Count}) ->
            Acc1 = Acc ++ "get_info(" ++ binary_to_list(TableName) ++ ") ->\n"
                ++ "    #table_info{\n"
                ++ "        name = <<\"" ++ binary_to_list(TableName) ++ "\">>,\n"
                ++ "        primary_key_name = " ++ binary_to_list(PrimaryKeyName) ++ ",\n"
                ++ "        fields = [" ++ lists:join(", ", lists:map(fun(TableColumn) -> TableColumn#table_column.column_name end, TableColumns)) ++ "]\n",
            Acc2 = if
                       Count == Size -> Acc1 ++ "    }.\n";
                       true -> Acc1 ++ "    };\n"
                   end,
            {Acc2, Count + 1}
        end, {"", 1}, TableInfos),
    file:write(S, Result),
    %% 编写获取全部表格
    file:write(S, <<"get_all_table() ->\n">>),
    file:write(S, "    [" ++ lists:join(", ", lists:map(fun({TableName, _TableColumns, _PrimaryKeyName}) -> TableName end, TableInfos)) ++ "]."),
    file:close(S),
    io:format("导出xk_momiji_data.erl完成~n").

