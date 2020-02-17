%%%-------------------------------------------------------------------
%%% @author mmmgdzl
%%% @copyright (C) 2020, <COMPANY>
%%% @doc for Yuzuki Momiji(Supipala)
%%%
%%% @end
%%% Created : 10. 二月 2020 15:48
%%%-------------------------------------------------------------------
-module(xk_momiji).
-author("mmmgdzl").

%% API
-export([init/1]).
-export([
    insert/1,

    delete_by_primary_key/2,
    delete_by_example/2,

    select_by_primary_key/2,
    select_by_example/2,
    select_by_example/3,
    select_by_example/4,

    count_by_example/2,

    execute_select_sql/3,

    update_by_primary_key/2,
    update_by_primary_key_selective/2,
    update_by_example/2,
    update_by_example_selective/2
]).

-include("xk_momiji_record.hrl").
-include("emysql.hrl").

-record(where_maker, {
    sql, %% 拼接中的sql语句
    need_and, %% 当前语句是否需要添加AND前缀
    need_or %% 当前语句是否需要添加OR前缀
}).

-define(MYSQL_POOL_ID, xk_momiji_pool).
% 数据库连接池大小
-define(MYSQL_POOL_SIZE, 10).
% 数据库IP地址
-define(MYSQL_ADDRESS, "127.0.0.1").
% 数据库端口
-define(MYSQL_PORT, 3306).
% 数据库账号
-define(MYSQL_USERNAME, "root").
% 数据库密码
-define(MYSQL_PASSWORD, "9879871221..s.k").
% 数据库名
-define(MYSQL_DATABASE, "music-nation").

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 初始化
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
init(InitModels) ->
    %% 创建数据库连接池
    emysql:add_pool(?MYSQL_POOL_ID, ?MYSQL_POOL_SIZE,
        ?MYSQL_USERNAME, ?MYSQL_PASSWORD, ?MYSQL_ADDRESS, ?MYSQL_PORT,
        ?MYSQL_DATABASE, utf8),
    %% 对于每一张表创建INSERT语句
    CreateInsertF = fun(TableName) ->
        PrepareName = create_insert_prepare_name(TableName),
        PrepareSize= erlang:length(xk_momiji_util:get_fields(TableName)),
        PrepareSql = <<"INSERT INTO ", (atom_to_binary(TableName, utf8))/binary, " VALUES(", (create_prepare_sql(PrepareSize))/binary, ")">>,
        emysql:prepare(PrepareName, PrepareSql)
    end,
    lists:foreach(CreateInsertF, xk_momiji_data:get_all_table()),
    %% 执行各个类的初始化
    lists:foreach(
        fun(Model) ->
            Model:init()
        end, InitModels).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 接口
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 插入数据
insert(Record) ->
    RecordName = erlang:element(1, Record),
    InsertPrepareName = create_insert_prepare_name(RecordName),
    Result = execute_prepare(InsertPrepareName, convert_record_to_insert_fields(Record)),
    {Result#ok_packet.insert_id, Result#ok_packet.affected_rows}.

%% 通过id来删除
delete_by_primary_key(RecordName, PrimaryKey) ->
    PrimaryKeyName = (xk_momiji_data:get_info(RecordName))#table_info.primary_key_name,
    Result = execute(create_delete(RecordName, [#{
        PrimaryKeyName => PrimaryKey
    }])),
    Result#ok_packet.affected_rows.

%% 通过查询条件来删除
delete_by_example(RecordName, ExampleList) ->
    Result = execute(create_delete(RecordName, ExampleList)),
    Result#ok_packet.affected_rows.

%% 通过id查询
select_by_primary_key(RecordName, PrimaryKey) ->
    PrimaryKeyName = (xk_momiji_data:get_info(RecordName))#table_info.primary_key_name,
    Result = execute(create_select(RecordName, [#{
        PrimaryKeyName => PrimaryKey
    }])),
    xk_momiji_util:as_record(Result, RecordName, xk_momiji_util:get_fields(RecordName)).

%% 通过查询条件查询
%% KEY = VALUE -- =
%% KEY = {not_equal, VALUE} -- <>
%% KEY = {gt, VALUE} -- >
%% KEY = {ge, VALUE} -- >=
%% KEY = {lt, VALUE} -- <
%% KEY = {le, VALUE} -- <=
%% KEY = {in, VALUE} -- IN
%% KEY = {not_in, VALUE} -- NOT IN
%% KEY = {like, VALUE} -- LIKE
%% KEY = {not_like, VALUE} -- NOT LIKE
%% KEY = {is_null} -- IS NULL
%% KEY = {not_null} -- NOT NULL
select_by_example(RecordName, ExampleList) ->
    select_by_example(RecordName, ExampleList, undefined, undefined).
select_by_example(RecordName, ExampleList, #page_info{} = PageInfo) ->
    select_by_example(RecordName, ExampleList, undefined, PageInfo);
select_by_example(RecordName, ExampleList, #order_by_info{} = OrderByInfo) ->
    select_by_example(RecordName, ExampleList, OrderByInfo, undefined).
select_by_example(RecordName, ExampleList, OrderByInfo, PageInfo) ->
    SelectSql = create_select(RecordName, ExampleList),
    OrderBySql = create_order_by(SelectSql, OrderByInfo),
    PageSql = create_page(OrderBySql, PageInfo),
    Result = execute(PageSql),
    xk_momiji_util:as_record(Result, RecordName, xk_momiji_util:get_fields(RecordName)).

%% 通过查询条件来统计
count_by_example(RecordName, ExampleList) ->
    SelectSql = create_count(RecordName, ExampleList),
    Result = execute(SelectSql),
    [[Count]] = Result#result_packet.rows,
    Count.

%% 执行自定义sql
execute_select_sql(RecordName, Sql, ResultColumns) ->
    Result = execute(Sql),
    xk_momiji_util:as_record(Result, RecordName, ResultColumns).

%% 通过id来修改
update_by_primary_key(UpdateRecord, PrimaryKey) ->
    RecordName = erlang:element(1, UpdateRecord),
    PrimaryKeyName = (xk_momiji_data:get_info(RecordName))#table_info.primary_key_name,
    Result = execute(create_update(UpdateRecord, [#{
        PrimaryKeyName => PrimaryKey
    }], false)),
    Result#ok_packet.affected_rows.
update_by_primary_key_selective(UpdateRecord, PrimaryKey) ->
    RecordName = erlang:element(1, UpdateRecord),
    PrimaryKeyName = (xk_momiji_data:get_info(RecordName))#table_info.primary_key_name,
    Result = execute(create_update(UpdateRecord, [#{
        PrimaryKeyName => PrimaryKey
    }], true)),
    Result#ok_packet.affected_rows.

%% 通过查询条件来修改
update_by_example(UpdateRecord, ExampleList) ->
    Result = execute(create_update(UpdateRecord, ExampleList, false)),
    Result#ok_packet.affected_rows.
update_by_example_selective(UpdateRecord, ExampleList) ->
    Result = execute(create_update(UpdateRecord, ExampleList, true)),
    Result#ok_packet.affected_rows.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 内部函数
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 语句拼接
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 构造选择语句
create_select(RecordName, [H | _] = ExampleList) when is_atom(RecordName) andalso is_list(ExampleList) andalso not is_map(H)  ->
    Example1 = lists:map(fun xk_momiji_util:to_map/1, ExampleList),
    create_select(RecordName, Example1);
create_select(RecordName, ExampleList) when is_atom(RecordName) andalso is_list(ExampleList) ->
    TableInfo = xk_momiji_data:get_info(RecordName),
    <<"SELECT * FROM ", (TableInfo#table_info.name)/binary, " ", (create_where(ExampleList))/binary>>.

%% 构造统计选择语句
create_count(RecordName, [H | _] = ExampleList) when is_atom(RecordName) andalso is_list(ExampleList) andalso not is_map(H) ->
    Example1 = lists:map(fun xk_momiji_util:to_map/1, ExampleList),
    create_count(RecordName, Example1);
create_count(RecordName, ExampleList) when is_atom(RecordName) andalso is_list(ExampleList) ->
    TableInfo = xk_momiji_data:get_info(RecordName),
    <<"SELECT COUNT(*) FROM ", (TableInfo#table_info.name)/binary, " ", (create_where(ExampleList))/binary>>.

%% 构造更新语句
create_update(UpdateRecord, [H | _] = ExampleList, IsSelective) when is_list(ExampleList) andalso not is_map(H) ->
    Example1 = lists:map(fun xk_momiji_util:to_map/1, ExampleList),
    create_update(UpdateRecord, Example1, IsSelective);
create_update(UpdateRecord, ExampleList, IsSelective) when  is_list(ExampleList) ->
    [RecordName | RecordFields] = tuple_to_list(UpdateRecord),
    TableInfo = xk_momiji_data:get_info(RecordName),
    <<"UPDATE ", (TableInfo#table_info.name)/binary, " SET ", (create_update_set(TableInfo#table_info.fields, RecordFields, IsSelective))/binary,
        " ", (create_where(ExampleList))/binary>>.

%% 构造删除语句
create_delete(RecordName, [H | _] = ExampleList) when is_atom(RecordName) andalso is_list(ExampleList) andalso not is_map(H) ->
    Example1 = lists:map(fun xk_momiji_util:to_map/1, ExampleList),
    create_delete(RecordName, Example1);
create_delete(RecordName, ExampleList) when is_atom(RecordName) andalso is_list(ExampleList) ->
    TableInfo = xk_momiji_data:get_info(RecordName),
    <<"DELETE FROM ", (TableInfo#table_info.name)/binary, " ", (create_where(ExampleList))/binary>>.

%% 构造WHERE语句
create_where([]) ->
    <<"">>;
create_where([H | _] = ExampleList) when is_list(ExampleList) andalso is_map(H) ->
    %% 构造查询条件的方法
    FieldF = fun(Key, Value, Acc) ->
        case Value of
            undefined -> Acc;
            _ ->
                case Acc#where_maker.need_and of
                    true ->
                        Acc#where_maker{
                            sql = <<(Acc#where_maker.sql)/binary, " AND ", (convert_field(Key, Value))/binary>>
                        };
                    false ->
                        Acc#where_maker{
                            sql = <<(convert_field(Key, Value))/binary>>,
                            need_and = true
                        }
                end
        end
    end,
    %% 构造查询条件组的方法
    GroupF = fun(Example, Acc) ->
        %% 先转换查询条件
        WhereMakerTemp = maps:fold(FieldF, #where_maker{sql = <<"">>, need_and = false}, Example),
        %% 如果为有效查询条件
        case WhereMakerTemp#where_maker.need_and of
            true ->
                case Acc#where_maker.need_or of
                    true ->
                        Acc#where_maker{
                            sql = <<(Acc#where_maker.sql)/binary, " OR (", (WhereMakerTemp#where_maker.sql)/binary, ") ">>
                        };
                    false ->
                        Acc#where_maker{
                            sql = <<" WHERE (", (WhereMakerTemp#where_maker.sql)/binary, ") ">>,
                            need_or = true
                        }
                end;
            false ->
                Acc
        end
    end,
    FinalWhereMaker = lists:foldl(GroupF, #where_maker{sql = <<"">>, need_or = false}, ExampleList),
    FinalWhereMaker#where_maker.sql.

%% 拼接update的set
create_update_set(ColumnNames, FieldLs, IsSelective) when is_list(FieldLs) ->
    S = lists:zip(ColumnNames, FieldLs),
    do_create_update_set(<<"">>, S, IsSelective).
do_create_update_set(Binary, [], _IsSelective) ->
    %%删除最开始的逗号
    <<",", UpdateSet/binary>> = Binary,
    UpdateSet;
do_create_update_set(Binary, [{ColumnName, Field} | FieldLs], IsSelective) ->
    do_create_update_set(<<Binary/binary, (create_new_set(ColumnName, Field, IsSelective))/binary>>, FieldLs, IsSelective).

create_new_set(Key, Value, IsSelective) ->
    case Value of
        undefined ->
            case IsSelective of
                true ->
                    <<"">>;
                false ->
                    <<", ", (column_safe(Key))/binary, " = NULL">>
            end;
        _ ->
            <<", ", (column_safe(Key))/binary, " = ", (value_safe(Value))/binary>>
    end.

%% 拼接In语句
create_in(List) when is_list(List) ->
    create_in(<<"">>, List).
create_in(Binary, [H]) ->
    <<Binary/binary, (value_safe(H))/binary>>;
create_in(Binary, [H | List]) ->
    create_in(<<Binary/binary, (value_safe(H))/binary,  " ,">>, List).

%% 拼接Order By语句
create_order_by(Sql, OrderByInfo) ->
    case OrderByInfo of
        undefined -> Sql;
        #order_by_info{order = Order} ->
            <<Sql/binary, " ORDER BY ", Order/binary>>
    end.

create_page(Sql, PageInfo) ->
    case PageInfo of
        undefined -> Sql;
        #page_info{page = Page, limit = Limit} ->
            RealPage = (Page - 1) * Limit,
            <<Sql/binary, " LIMIT ", (integer_to_binary(RealPage))/binary, ",", (integer_to_binary(Limit))/binary>>
    end.

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 数据转换
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 将Example转换为binary键值对
convert_field(Key, {in, Field}) when is_atom(Key) andalso is_list(Field) andalso erlang:length(Field) > 0 ->
    <<" ", (column_safe(Key))/binary, " IN (", (create_in(Field))/binary, ") ">>;
convert_field(Key, {not_in, Field}) when is_atom(Key) andalso is_list(Field) andalso erlang:length(Field) > 0 ->
    <<" ", (column_safe(Key))/binary, " NOT IN (", (create_in(Field))/binary, ") ">>;
convert_field(Key, {like, Field}) when is_atom(Key) andalso is_list(Field) ->
    <<" ", (column_safe(Key))/binary, " LIKE ", (value_safe(Field))/binary>>;
convert_field(Key, {not_like, Field}) when is_atom(Key) andalso is_list(Field) ->
    <<" ", (column_safe(Key))/binary, " NOT LIKE ", (value_safe(Field))/binary>>;
convert_field(Key, {is_null}) when is_atom(Key) ->
    <<" ", (column_safe(Key))/binary, " IS NULL ">>;
convert_field(Key, {not_null}) when is_atom(Key) ->
    <<" ", (column_safe(Key))/binary, " IS NOT NULL ">>;
convert_field(Key, {gt, Field}) when is_atom(Key) ->
    <<" ", (column_safe(Key))/binary, " > ", (value_safe(Field))/binary>>;
convert_field(Key, {ge, Field}) when is_atom(Key) ->
    <<" ", (column_safe(Key))/binary, " >= ", (value_safe(Field))/binary>>;
convert_field(Key, {lt, Field}) when is_atom(Key) ->
    <<" ", (column_safe(Key))/binary, " < ", (value_safe(Field))/binary>>;
convert_field(Key, {le, Field}) when is_atom(Key) ->
    <<" ", (column_safe(Key))/binary, " <= ", (value_safe(Field))/binary>>;
convert_field(Key, {not_equal, Field}) when is_atom(Key) ->
    <<" ", (column_safe(Key))/binary, " <> ", (value_safe(Field))/binary>>;
convert_field(Key, Field) when is_atom(Key) ->
    <<" ", (column_safe(Key))/binary, " = ", (value_safe(Field))/binary>>.

%% 创建Prepare语句的问号序列
create_prepare_sql(Num) ->
    create_prepare_sql(<<"">>, Num).
create_prepare_sql(Sql, 1) ->
    <<Sql/binary, "?">>;
create_prepare_sql(Sql, Num) when Num > 0 ->
    create_prepare_sql(<<Sql/binary, "?,">>, Num-1).

%% 将Record转换为插入语句参数
convert_record_to_insert_fields(Record) ->
    [_RecordName | FieldLs] = tuple_to_list(Record),
    ConvertF = fun(Field) ->
        case Field of
            undefined -> null;
            _ -> Field
        end
    end,
    lists:map(ConvertF, FieldLs).

%% 参数安全化
value_safe(Value) ->
    <<"'", (xk_momiji_util:to_binary(Value))/binary, "'">>.

%% 字段名安全化
column_safe(ColumnName) ->
    <<"`", (xk_momiji_util:to_binary(ColumnName))/binary, "`">>.
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 其他
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 创建INSERT的Prepare语句名
create_insert_prepare_name(RecordName) when is_atom(RecordName) ->
    list_to_atom("insert_" ++ (atom_to_list(RecordName))).

%% 执行数据库语句
execute(Sql) ->
    Result = emysql:execute(?MYSQL_POOL_ID, Sql),
    case Result of
        #error_packet{msg = Msg} ->
            error("Sql error : ~p by Sql[~p]~n", [Msg, Sql]);
        _ ->
            Result
    end.
execute_prepare(PrepareName, FieldLs) ->
    Result = emysql:execute(?MYSQL_POOL_ID, PrepareName, FieldLs),
    case Result of
        #error_packet{msg = Msg} ->
            error("Sql error : ~p by PrepareName[~p], FieldLs[~p]~n", [Msg, PrepareName, FieldLs]);
        _ ->
            Result
    end.