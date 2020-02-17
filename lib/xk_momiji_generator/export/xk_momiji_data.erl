-module(xk_momiji_data).
-author("mmmgdzl").

-include("xk_momiji_record.hrl").

%% API
-export([
    get_info/1,
    get_all_table/0
]).

get_info(im_chatlog) ->
    #table_info{
        name = <<"im_chatlog">>,
        primary_key_name = id,
        fields = [id, from_id, to_id, group_id, content, send_time, type, status]
    };
get_info(im_friend) ->
    #table_info{
        name = <<"im_friend">>,
        primary_key_name = id,
        fields = [id, friend_group_id, user_id, nickName]
    };
get_info(im_friend_group) ->
    #table_info{
        name = <<"im_friend_group">>,
        primary_key_name = id,
        fields = [id, user_id, name]
    };
get_info(im_group) ->
    #table_info{
        name = <<"im_group">>,
        primary_key_name = id,
        fields = [id, groupname, avatar, notice, create_user_id, approval]
    };
get_info(im_group_user) ->
    #table_info{
        name = <<"im_group_user">>,
        primary_key_name = id,
        fields = [id, group_id, user_id, nick_name]
    };
get_info(im_msgbox) ->
    #table_info{
        name = <<"im_msgbox">>,
        primary_key_name = id,
        fields = [id, user_id, from_id, extend, type, content, remark, href, read, time]
    };
get_info(t_user) ->
    #table_info{
        name = <<"t_user">>,
        primary_key_name = id,
        fields = [id, account, username, phone, email, password, state, avatar_url, register_time, active_time, gender, signature]
    }.
get_all_table() ->
    [im_chatlog, im_friend, im_friend_group, im_group, im_group_user, im_msgbox, t_user].