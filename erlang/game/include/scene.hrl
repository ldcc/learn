%% --------------------------------------------------------------------
%% 自定义宏
-define(seat_up,	0). 									% 起身
-define(seat_out,	1).										% 退出
-define(seat_alt,	2).										% 换位


%% --------------------------------------------------------------------
%% 对战日志(游戏内日志)
-record(u_vs,			{
						 list					= [],	% 战绩列表 [#r_logs...]
						 into					= []		% 进入过的房间列表	[{MasterUid,RoomNum,CreateTime}...]
						}).

%% --------------------------------------------------------------------
%% 匹配队列
-record(mate,			{
						 game_id				= 0,
						 room_id				= 0,
						 queue_id				= 0,
						 uid					= 0,
						 is_ready				= 0,		% 是否准备
						 time					= 0,
						 args					= null		% 座位参数
						}).

%% 机器人数据
-record(robot,			{
						 exit_time				= 0,		% 结束一局后离开房间的时间	
						 discard_time			= 0,		% 出牌时间(每次重新随机一个值) 
						 event_time				= 0			% 响应碰杠事件时间(每次重新随机一个值) 
						}).

%% 桌子信息
-record(desk, 			{
						 desk_id				= 0,		% 桌子ID(key)
						 desk_num				= 0,		% 桌子人数
						 vipm_key				= 0,	    % 房间号 key
						 master_uid             = 0,        % 房主uid
						 consume_card           = 0,        % 消耗房卡数
						 
						 room_id				= 0,		% 房间ID
						 room_type				= 0,		% 房间类型(1:经典;2:竞赛,3好友 4:)
						 game_id				= 0,     	% 玩法游戏ID
						 time_create			= 0,	    % 创建时间
						 time_end				= 0,		% 结束时间
						 time_start				= 0,		% 游戏开始的时间 (时间戳)
						 time_last				= 0,		% 上次操作时间 (时间戳)
						 
						 event_queue			= [],		% 待处理事件队列(前端发过来的请求消息,不能马上处理的存在这里) [#we...]
						 event_wait				= [],		% 待处理事件列表 [#de...]
						 event_ing				= null,	% 正在处理的事件  #de
						 event_finish			= [],		% 完成事件列表   [#de...] 先来的在后面
						 
						 curr_seat_idx			= 0,		% 当前操作玩家椅子id
						 curr_uid				= 0,		% 当前操作玩家uid
						 
						 round_idx				= 0,		% 轮数 
						 bout_idx				= 0,		% 当前局数
						 bout_total             = 0,        % 总局数 
						 banker_idx				= 0, 		% 庄家椅子编号
						 
						 card_heap				= [],		% 牌堆
						 lead_heap				= [],		% 已出的牌
						 
						 seat_list				= [],		% 在房间的玩家列表 [#seat{}...]
						 u_vs_logs				= [],		% 战绩 [] 先来的在前面 (未使用)
						 vote_list				= [],    	% 投票列表[#vote_info..] 
						 op_logs_list			= [],		% 操作日志列表 [#op_logs{}...], 
						 state					= 0,		    	% 游戏阶段
						 state_time				= 0,		    	% 游戏阶段持续时间
						 state_hold				= 0,		        % 离线时，保持进程存活计时
						 state_tag				= null,			% 定时器类型
						 curr_state_tag			= null,			% 当前定时器类型
						 delay					= [],				% 其他定时执行任务[{tag, second},...]
						 
						 has_player_in			= false,	% 是否有玩家加入，从上次重置以来
						 is_start_game			= false,			% 是否已经开始过一局游戏
						 is_cancle_dismiss		= false,			% 是否取消自动解散房间(true:取消)
						 
						 api_desk				= EMPTY_MOD,    % desk具体游戏扩展
						 api_vipm				= EMPTY_MOD,    % vipm具体游戏扩展
						 api_vipg				= EMPTY_MOD,    % vipg具体游戏扩展
						 
						 ext_d					= EMPTY_RECORD, % 具体游戏桌子扩展(金币)
						 ext_dm					= EMPTY_RECORD  % 具体游戏桌子扩展(好友房)
						}).

%% 桌子事件-单项 desk_event
-record(de,				{
						 event_list				= [],				% 事件牌列表 [{EventType,CardList}...] 
						 event_finish			= EMPTY_RECORD,	% 列表里面的一个完成的事件牌 {EventType,CardList} | 升级中的最大事件牌
						 mpid					= EMPTY_PID,		% 玩家进程Mpid
						 seat_idx				= 0,		    	% 触发事件的椅子编号 | 升级中的第一个出牌玩家uid	
						 seat_idx_target		= 0,		    	% 牌所属的椅子编号
						 time_touch				= 0,		   	    % 事件触发时间
						 time_finish			= 0					% 事件完成时间
						}).	

%% 待处理事件-单项 wait_event
-record(we,				{
						 uid				= 0,
						 event_type			= 0,
						 suits				= 0,
						 point				= 0,
						 lose_id_list		= []
						}).

%% 操作日志-单项/战绩-单局 desk_logs
-record(dl,				{
						 origin			        = {0,0},	% 来源 {Uid,seatId}
						 gone			        = {0,0},	% 去向 {Uid,seatId}
						 card 			        = {0,0,0},	% {Suits,Status,Point} 
						 type			        = 0,		% 操作类型 OP_TYPE_*
						 uid			        = 0,		% 玩家UID
						 time			        = 0,		% 操作时间
						 list					= []		% [{seatId,#scoreboard}...]
						}).

%% 在椅子上的玩家
-record(seat,			{ 
						 seat_id				= 0,		   % 椅子编号(旁观者的seat_id为0)  椅子编号(1,2,3,4,5)
						 seat_id_view			= 0,		   % 玩家视角
						 
						 uid					= 0,		   		   % 玩家UID
						 mpid					= EMPTY_PID,       % 玩家进程Mpid
						 socket					= EMPTY_RECORD,    % 玩家socket
						 
						 uname					= <<>>,	    % 姓名
						 skin_id				= 0,         % 皮肤ID
						 icon_id				= 0,		    % 头像
						 icon_url				= <<>>,	    % 头像URL 
						 vip_lv					= 0, 	    % vip等级
						 sex					= 0,			% 性别
						 lv						= 0,			% 等级
						 charm					= 0,			% 魅力
						 gold					= 0,		    % 金币数
						 wx_info				= 0,         % 存为元组{wx_icon_url,wx_name,wx_sex},0代表无使用微信信息
						 
						 ip						= <<>>,      % Ip地址
						 place					= 0,		% 位置信息 tuple {Longitude,Latitude,Site} {经度,纬度,地点}
						 
						 is_ok					= false,	% 准备状态
						 is_quit				= false,	% 退出/离线，不再观战
						 is_game				= false,	% 是否已玩过一局游戏
						 is_ready				= false,   % ?true:已准备,?false:未准备
						 is_play				= false,	% 是否已出牌
						 is_win					= false,	% 胜利
						 is_sleep				= false,	% 是否托管
						 is_banker				= false,	% 
						 is_auto				= false,	% 
						 is_robot				= false,	% 机器人

						 ext_s					= EMPTY_RECORD,	 % 私人房间扩展字段 #xxx_s  在 mod.game_xxxx.hrl 里定义
						 ext_sm					= EMPTY_RECORD 	 % 私人房间扩展字段 #xxx_sm 在 mod.game_xxxx.hrl 里定义
						}).

%% 群友房间椅子数据扩充  seat_offline
-record(so,		        {
						 uid					= 0,	   	% 玩家UID
						 uname					= <<>>,	    % 姓名
						 icon_url				= <<>>,		% 头像url
						 chip					= 0	        % 玩家积分(输赢)
						}).


%% 好友房 管理 vipm_ctl
-record(vipm,			{
						 vipm_key				= 0,			% key
						 time_create			= 0,			% 创建时间
						 desk_id				= 0,        	% 房室id
						 desk_pid				= EMPTY_PID,   % 房室id
						 group_id				= 0,        	% 0:好友房 1:群友房
						 game_id				= 0,        	% 游戏Id
						 
						 bout_num				= 0,			% 剩余游戏局数
						 bout_total				= 0,        	% 总局数
						 state					= 0,			% 游戏阶段  0:未开局，1:已开局，2:已解散
						 man_num				= 0,			% 房间人数
						 master_uid				= 0,			% 房主UID
						 so_list				= [],       	% [#so{}]
						 args					= EMPTY_RECORD % 开房参数
						}).

%% 群友房间信息(ets)
-record(vipg,			{
						 num_key    		    = {0,0},	% 房间号和创建时间(key) {RoomNum,CreateTime} 
						 num_man			    = 0,		% 房间人数
						 num_game			    = 0,		% 剩余游戏局数
						 master_uid			    = 0			% 房主UID
						}).

%% -------------------------------------------------------------------


%% 游戏级别场合
-record(d_scene_level,	{  
						 game_id				= 0 ,     %% 游戏ID
						 room_id				= 0 ,     %% 房间ID
						 name 				    = <<>>  , %% 名称	
						 antes 				    = 0 , 	  %% 底分	
						 time  				    = 0 , 	  %% 匹配时间
						 low   				    = 0 ,     %% 提拉下限	
						 top   				    = 0 ,     %% 提拉上限 
						 score_low 			    = 0 ,     %% 积分下限
						 ticket    			    = 0 ,     %% 门票
						 type      			    = 0 ,     %% 类型（1=经典，2=活动比赛，3=积分模式）
						 player_limit			= 0 ,	  %% 桌子玩家上限
						 recommend_low   	    = 0 ,     %% 推荐提拉数下限（比赛场不推荐）
						 recommend_top   	    = 0 ,     %% 推荐提拉数上限（比赛场不推荐）
						 recommend_times 	    = 0 ,     %% 每天推荐次数
						 recommend_condition	= 0 ,     %% 推荐条件 
						 ai						= 0 ,     %% 机器人ai难度（1=普通，2=困难）
						 gold					= [] ,    %% 机器人提拉范围 Range = [LowGold,TopGold]
						 icon					= [] ,    %% 机器人头像随机列表 [IconId...]
						 vip					= [] ,    %% 机器人VIP等级范围 Range = [LowVipLv,TopVipLv]
						 bag					= [] ,    %% 机器人背包物品列表[{GoodsId,Range}...]
						 vs_count				= [] ,    %% 机器人对战局数范围 Range = [LowNum,TopNum]
						 vs_win_rate			= [] ,    %% 机器人胜率万分比范围 Range = [LowRate,TopRate]
						 exit_time				= [] ,    %% 机器人离开时间 [LowTime,TopTime]
						 discard_time			= [] ,    %% 机器人出牌随机时间 [{Time,Weight}...]
						 event_time				= [] ,    %% 机器人响应事件随机时间 [{Time,Weight}...]
						 number_times			= 0 
						}). 


%% 投票信息
-record(vote_info,		{
						 vote_type         		= 0, 		%% 投票类型
						 v_list       		    = [], 	    %% [{uid,type}..],type(1:同意,2:拒绝,3:待选择)
						 creator_uid       		= 0,		%% 发起者uid	
						 creator_time    		= [],  		%% 发起投票的时间
						 end_time       	    = 1  		%% 超时结束时间
						}).


%% 计分信息-单人
-record(scoreboard,		{
						 seat_id                = 0,        % 座位id
						 %%uid 					= 0,		% 玩家UID
						 uname					= <<>>,		% 角色名
						 score					= 0			%  得分
						}).

%% 战绩-单局
-record(one_vs,			{
						 game_id                = 0,        % 游戏id
						 desk_id                = 0,        % 桌子自增id
						 vipm_key				= 0,		% 房间号
						 start_time             = 0,        % 本局开始时间
						 end_time				= 0,		% 本局结束时间
						 room_type				= 0,		% 房间类型
						 finish_score			= 0,		% 最终得分
						 list					= []		% [#scoreboard...]
						}).

%% 对战日志(游戏内日志)
-record(u_vs_logs,		{
						 into_list				= []		% 进入过的房间列表	[DeskId...]
						}).


%% 操作日志
-record(op_logs,		{
						 op_type				= 0,		% 操作类型-通用常量 CONST_VIPM_TYPE
						 uid					= 0,		% 玩家UID
						 time					= 0,		% 操作时间
						 ext					= []		% 扩展数据
						}).
