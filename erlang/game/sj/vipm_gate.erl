%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : vipm_sj_gate.erl
%%% Created : 2008.06.20
%%% Description: 
%%% -------------------------------------------------------------------
-module(vipm_gate).

-behaviour(gamecore_gate).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.desk_scene.hrl").  
-include("mod.game_sj.hrl").
%% --------------------------------------------------------------------
%% 以下系统默认导出(勿删)
-export([way/3]).
%% 扩展导出
%% -export([]).

%% way(MsgId,Gate,Bin)->
%%	{?ok,Gate}
%% --------------------------------------------------------------------
%% 开房参数 [3010]
way(?R_VIPM_SJ_CREATE, #gate{uid=MasterUid}=Gate, {CardNum,MasterJoin, Bin}) ->
    {IsLevelup, Level, PlayMode, Data} = msg:decode({?bool, ?int8u, ?int8u, ?int8u}, Bin),
	GameId = ?CONST_GAME_ID_SJ,
	{BoutTotal, Multiple} = ?IF(PlayMode == ?bout, {Data, ?zero}, {-?one, Data}),
	vipm_api:vipm_create(GameId, MasterUid, MasterJoin, BoutTotal, CardNum, {PlayMode, Level, IsLevelup, Multiple}),
	{?ok,Gate};


% 微信分享请求 [3450]
way(?R_VIPM_SJ_WECHAT_SHARE, #gate{io=#io{game_id=GameId},mpid=Mpid}=Gate, Bin) ->
    {VipmNum} = msg:decode({?int32u},Bin),
	case vipm_api:vipm_pid_gid(VipmNum) of
		{DeskPid, GameId} ->			
			msg:send_pid(DeskPid, desk_sj_api, wechat_share_cb, Mpid);
		_Error -> 
			system_api:send_error(Mpid, ?E_PRIVATE_ROOM_ID_NOT_FOUND)
	end,
    {?ok,Gate};


%% 错误匹配(勿删)
way(MsgId, Gate, Bin)->
	?MSG_GATEWAY(MsgId, Gate, Bin),
	{?ok, Gate}.

