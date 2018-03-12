%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : desk_gate.erl
%%% Created : 2008.06.20
%%% Description: 
%%% -------------------------------------------------------------------
-module(sj_desk_gate).

%% -behaviour(gamecore_gate).
%% --------------------------------------------------------------------
%% Include files
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.game_sj.hrl").
-include("mod.desk_scene.hrl"). 

%%
%% Exported Functions
%%
-export([way/3]).

% 桌子准备好了(发桌上的玩家/观众) [3508]
way(?R_DESK_LOAD_OK, #gate{scene_pid=DeskPid,uid=Uid,mpid=Mpid}=Gate, Bin) ->
	{_DeskId, IsReconn} = msg:decode({?int32u, ?bool}, Bin),
	msg:send_pid(DeskPid, desk_sj_api, load_cb, {Uid, Mpid, IsReconn}),
	{?ok, Gate};

% 游戏匹配参数 [4003]
way(?R_DESK_SJ_MATCH_ARGS, #gate{uid=Uid}=Gate, {GameId, RoomId, IsMate, Info, _Bin}) ->
	desk_srv:mate(GameId, RoomId, {Uid, ?null, Info, IsMate}),
    {?ok,Gate};

% 触发事件 [4110]
way(?R_DESK_SJ_CARDS_EVENT, #gate{scene_pid=DeskPid,uid=Uid,mpid=Mpid}=Gate, <<Behavior:8, _Count:8, Bin/binary>>) -> 
	Cards = [#sjc{suit=Suit,point=Point} || <<Suit:8, Point:8>> <= Bin],
	msg:send_pid(DeskPid, desk_sj_api, eventing_cb, {Behavior, Uid, Mpid, Cards}),
    {?ok, Gate};

%% 错误匹配 ------------
way(MsgId,Gate,Bin)-> % 错误匹配
	?MSG_ERROR("~n************~w ERROR************~nMsgID:~p,DataList:~p,Bin:~p",[?MODULE,MsgId,Gate,Bin]),
	{?ok, Gate}.
