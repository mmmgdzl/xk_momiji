-author("mmmgdzl").

%% 表格信息
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
}).

-define(TABLE_IM_CHATLOG, im_chatlog).
-record(im_chatlog,{id, from_id, to_id, group_id, content, send_time, type, status}).

-define(TABLE_IM_FRIEND, im_friend).
-record(im_friend,{id, friend_group_id, user_id, nickName}).

-define(TABLE_IM_FRIEND_GROUP, im_friend_group).
-record(im_friend_group,{id, user_id, name}).

-define(TABLE_IM_GROUP, im_group).
-record(im_group,{id, groupname, avatar, notice, create_user_id, approval}).

-define(TABLE_IM_GROUP_USER, im_group_user).
-record(im_group_user,{id, group_id, user_id, nick_name}).

-define(TABLE_IM_MSGBOX, im_msgbox).
-record(im_msgbox,{id, user_id, from_id, extend, type, content, remark, href, read, time}).

-define(TABLE_T_USER, t_user).
-record(t_user,{id, account, username, phone, email, password, state, avatar_url, register_time, active_time, gender, signature}).

