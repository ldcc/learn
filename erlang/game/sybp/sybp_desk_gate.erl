%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : desk_gate.erl
%%% Created : 2008.06.20
%%% Description: 
%%% -------------------------------------------------------------------
-module(sybp_desk_gate).

%% -behaviour(gamecore_gate).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.desk_scene.hrl"). 
-include("mod.game_zpsybp.hrl").

%%
%% Exported Functions
%%
-export([way/3]).

% 桌子准备好了(发桌上的玩家/观众) [3508]
way(?R_DESK_LOAD_OK, #gate{scene_pid=DeskPid,uid=Uid,mpid=Mpid}=Gate, Bin) ->
	{_DeskId, IsReconn} = msg:decode({?int32u, ?bool}, Bin),
	msg:send_pid(DeskPid, desk_zpsybp_api, load_cb, {Uid, Mpid, IsReconn}),
	{?ok, Gate};


% 游戏匹配参数 [4003]
way(?R_DESK_ZPSYBP_MATCH_ARGS, Gate, {GameId, RoomId, IsMate, Info, _Bin}) ->
	Args = ?null,
	desk_srv:mate(GameId, RoomId, {Gate#gate.uid, Args, Info, IsMate}),
    {?ok,Gate};


% 触发的事件 [4104]
way(?R_DESK_ZPSYBP_EVENT, #gate{scene_pid=DeskPid,uid=Uid}=Gate, <<Event:8, _Count:8, Bin/binary>>) ->
	Deal = [#bpc{kind=Kind,state=Event,point=Point} || <<Kind:8, Point:8>> <= Bin],
	msg:send_pid(DeskPid, desk_zpsybp_api, handle_event_cb, {Uid, Event, Deal}),
    {?ok,Gate};


% 玩家改变牌位置 [4107]
way(?R_DESK_ZPSYBP_POSITION_ALTER, #gate{scene_pid=DeskPid,uid=Uid}=Gate, <<_Count:8, Bin/binary>>) ->
	{Vectors, Cards} = lists:unzip([{[X, Y], #bpc{kind=K,state=S,point=P}} || <<X:8, Y:8, K:8, S:8, P:8>> <= Bin]),
	msg:send_pid(DeskPid, desk_zpsybp_api, player_alter_card_cb, {Uid, Vectors, Cards}),
    {?ok,Gate};


%% 错误匹配 ------------
way(MsgId,Gate,Bin)-> % 错误匹配
	?MSG_ERROR("~n************~w ERROR************~nMsgID:~p,DataList:~p,Bin:~p",[?MODULE,MsgId,Gate,Bin]),
	{?ok, Gate}.
