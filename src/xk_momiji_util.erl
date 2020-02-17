%%%-------------------------------------------------------------------
%%% @author mmmgdzl
%%% @copyright (C) 2020, <COMPANY>
%%% @doc for Yuzuki Momiji(Supipala)
%%%
%%% @end
%%% Created : 10. 二月 2020 15:48
%%%-------------------------------------------------------------------
-module(xk_momiji_util).
-author("mmmgdzl").

-include("xk_momiji_record.hrl").
-include("emysql.hrl").

%% API
-export([
    to_binary/1,
    as_record/3,
    as_record/4,
    get_fields/1,
    to_map/1,

    current_time_str/0,
    timestamp_to_time_str/1,
    datetime_to_timestamp/1

]).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 类型相关
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 将属性转换为二进制
to_binary(Field) when is_binary(Field) ->
    Field;
to_binary(Field) when is_integer(Field) ->
    integer_to_binary(Field);
to_binary(Field) when is_atom(Field) ->
    atom_to_binary(Field, utf8);
to_binary(Field) when is_list(Field) ->
    %% 将所有 ' 替换为 \'
    F = fun(Char, Acc) ->
        case Char of
            $'  -> "'\\" ++ Acc;
            _ -> [Char | Acc]
        end
    end,
    list_to_binary(lists:reverse(lists:foldl(F, [], Field))).

%% 将查询结果集封装为记录
as_record(Result = #result_packet{}, RecordName, Fields) when is_atom(RecordName), is_list(Fields) ->
    as_record(Result, RecordName, Fields, fun(A) -> A end).
as_record(Result = #result_packet{}, RecordName, Fields, Fun) when is_atom(RecordName), is_list(Fields), is_function(Fun) ->
    Columns = Result#result_packet.field_list,
    RecordFields = get_fields(RecordName),

    S = lists:seq(1, length(Columns)),
    P = lists:zip3([binary_to_atom(C1#field.name, utf8) || C1 <- Columns], [C1#field.type || C1 <- Columns], S),
    F = fun(FieldName) ->
            ConvertF = case proplists:lookup(FieldName, P) of
                none ->
                    fun(_) -> undefined end;
                {FieldName, Type, Pos} ->
                    fun(Row) ->
                        case lists:nth(Pos, Row) of
                            undefined -> undefined;
                            Value ->
                                convert_database_data(Type, Value)
                        end
                    end
            end,
            {lists:member(FieldName, RecordFields), FieldName,  ConvertF}
        end,
    Fs = [ F(FieldName) || FieldName <- Fields ],
    F1 = fun(Row) ->
        {RecordData, RecordExt} = lists:foldl(
            fun({IsNotExt, FieldName, Fx}, {DataAcc, ExtAcc}) ->
                case IsNotExt of
                    true ->
                        {[Fx(Row) | DataAcc], ExtAcc};
                    false ->
                        {DataAcc, maps:put(FieldName, Fx(Row), ExtAcc)}
                end
            end, {[], #{}}, Fs
        ),
        Fun(list_to_tuple([RecordName | lists:reverse(RecordData)] ++ [RecordExt]))
    end,
    [ F1(Row) || Row <- Result#result_packet.rows ].

%% 获取表格参数
get_fields(RecordName) ->
    (xk_momiji_data:get_info(RecordName))#table_info.fields.

%% 将记录转换为map
to_map(Obj) ->
    Keys = get_fields(erlang:element(1, Obj)),
    {Result, _} = lists:foldl(
        fun(Key, {Map, Count}) ->
            {maps:put(Key, erlang:element(Count, Obj), Map), Count + 1}
        end, {maps:new(), 2}, Keys),
    Result.

%% 将数据库对象转换为对应的对象
%% Type 整型-3
%%      时间-12
%%      字符串-253
convert_database_data(3, Value) ->
    Value;
convert_database_data(12, Value) ->
    {datetime, RealTime} = Value,
    datetime_to_timestamp(RealTime) * 1000;
convert_database_data(253, Value) ->
    binary_to_list(Value).

%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%%% 时间相关
%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%%
%% 返回yyyy-MM-dd HH:mm:ss字符串
current_time_str() ->
    timestamp_to_time_str(erlang:timestamp()).
timestamp_to_time_str(TimeStamp) ->
    {{Year, Month, Day}, {Hour, Minute, Second}} = calendar:now_to_local_time(TimeStamp),
    integer_to_list(Year) ++ "-" ++ integer_to_list(Month) ++ "-" ++ integer_to_list(Day)
        ++ " " ++ integer_to_list(Hour) ++ ":" ++ integer_to_list(Minute) ++ ":" ++ integer_to_list(Second).

% 时间转时间戳，格式：{{2013,11,13}, {18,0,0}}
datetime_to_timestamp(DateTime) ->
    calendar:datetime_to_gregorian_seconds(DateTime) -
        calendar:datetime_to_gregorian_seconds({{1970,1,1}, {8,0,0}}).