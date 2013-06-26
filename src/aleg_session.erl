%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@simplecs.net>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Luis F Urrea <lfurrea@simplecs.net>
%%%-------------------------------------------------------------------
-module(aleg_session).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-include("callflow.hrl").

-type startlink_err() :: {'already_started', pid()} |'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

-spec start_link(string(), pid()) -> startlink_ret().

start_link(UUID, ControllerPid) ->
    gen_server:start_link(?MODULE, [UUID, ControllerPid], []).

%%--------------------------------------------------------------------
%% @doc
%% Hangup this channel and destroy session?
%% @end
%%--------------------------------------------------------------------


%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------

-spec init(list()) -> {ok, #cf_call{}}.

init([UUID, ControllerPid]) ->
    State = #cf_call{aleg_uuid = UUID, controller_pid = ControllerPid},
    {ok, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

-spec handle_call(term(), {pid(), _}, #cf_call{}) -> {reply, not_implemented, #cf_call{}}. 

handle_call(_Request, _From, State) ->
    Reply = not_implemented,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

-spec handle_cast(term(), #cf_call{}) -> {noreply, #cf_call{}}.

handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------

-spec handle_info({Msg, {event, list()}}, #cf_call{}) -> {noreply, #cf_call{}} when
      Msg :: call | call_event;
		 (call_hangup, #cf_call{}) -> {stop, normal, #cf_call{}}.

handle_info({call, {event, [UUID | Rest]}}, #cf_call{} = Call) ->
    error_logger:info_msg("Got initial call event ~p",[UUID]),
    DestNumber = proplists:get_value("Channel-Destination-Number", Rest),
    NewState = Call#cf_call{destination_number= DestNumber},
    {noreply, NewState};

handle_info({call_event, {event, [UUID | Rest]}}, #cf_call{destination_number = DestNumber, aleg_uuid = UUID} = Call) ->
    Event = proplists:get_value("Event-Name", Rest),
    ChannelState = proplists:get_value("Channel-State", Rest),
    case Event of
	"CHANNEL_PARK" ->
	    error_logger:info_msg("A-leg is PARKED: ~p, now we will execute our call flow to number ~p",[UUID, DestNumber]),
	    Flow = [cf_device, cf_voicemail],
	    supervisor:start_child(cf_exe_sup, [Call, Flow]),
	    {noreply, Call};
	"CHANNEL_BRIDGE" ->
	    error_logger:info_msg("We bridged to a B-leg ~p",[UUID]),
	    {noreply, Call};
	"CHANNEL_STATE" ->
	    error_logger:info_msg("Aleg call state ~p",[ChannelState]),
	    {noreply, Call};
	_ ->
	    error_logger:info_msg("Got fucked ~p",[Event]),
	    {noreply, Call}
    end;
handle_info(call_hangup, #cf_call{aleg_uuid = UUID} = Call)->
    error_logger:info_msg("Got call_hangup event for: ~p, will proceed to deallocate A-leg", [UUID]),
    simpleswitch_core:deallocate_me(UUID, self()),
    {noreply, Call};
handle_info(_Info, State) ->
    error_logger:info_msg("Got ~p",[_Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------

-spec terminate(term(), #cf_call{}) -> normal.

terminate(_Reason, _State) ->
    normal.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------

-spec code_change(term(), #cf_call{}, term()) -> {ok, #cf_call{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
