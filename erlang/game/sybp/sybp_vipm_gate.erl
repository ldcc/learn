%%% -------------------------------------------------------------------
%%% Author  : 一平|yiping@erldoc.com
%%% File    : vipm_zpsybp_gate.erl
%%% Created : 2008.06.20
%%% Description: 
%%% -------------------------------------------------------------------
-module(sybp_vipm_gate).

-behaviour(gamecore_gate).
%% --------------------------------------------------------------------
%% Include files 
%% --------------------------------------------------------------------
-include("comm.hrl").
-include("mod.gate.hrl").
-include("mod.desk_scene.hrl").  
-include("mod.game_zpsybp.hrl").
%% --------------------------------------------------------------------
%% 以下系统默认导出(勿删)
-export([way/3]).
%% 扩展导出
%% -export([]).

%% way(MsgId,Gate,Bin)->
%%	{?ok,Gate}
%% --------------------------------------------------------------------
%% 开房参数 [3010]
way(?R_VIPM_ZPSYBP_CREATE, #gate{uid=MasterUid}=Gate, {CardNum,MasterJoin, Bin}) ->
    {CardNum, SatisKeepBanker, HuRule, PlayerLimit} = msg:decode({?int8u, ?bool, ?int8u, ?int8u},Bin),
	GameId = ?CONST_GAME_ID_ZPSYBP,
	[Type, Data] = data_room_game:get_game_num(GameId, CardNum),
	BoutTotal = ?IF(Type == ?one, Data, ?zero),
	vipm_api:vipm_create(GameId, MasterUid,MasterJoin, BoutTotal, CardNum, {SatisKeepBanker, HuRule, PlayerLimit}),
	{?ok,Gate};


% 微信分享请求 [3450]
way(?R_VIPM_ZPSYBP_WECHAT_SHARE_REQ, #gate{io=#io{game_id=GameId},mpid=Mpid}=Gate, Bin) ->
    {VipmNum} = msg:decode({?int32u},Bin),
	case vipm_api:vipm_pid_gid(VipmNum) of
		{DeskPid, GameId} ->			
			msg:send_pid(DeskPid, desk_zpsybp_api, wechat_share_cb, Mpid);
		_Error -> 
			system_api:send_error(Mpid, ?E_PRIVATE_ROOM_ID_NOT_FOUND)
	end,
    {?ok,Gate};


%% 错误匹配(勿删)
way(MsgId, Gate, Bin)->
	?MSG_GATEWAY(MsgId, Gate, Bin),
	{?ok, Gate}.

