%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created : 30 Oct 2012 by Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%%-------------------------------------------------------------------
-module(fs_outbound_extn_controller).

-behaviour(gen_server).

%% API
-export([start_link/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {
	 cnode :: atom(),
	 number :: string(),
	 exten :: string(),
	 uuid :: string(),
	 controller_pid :: pid()}).

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
start_link(UUID, ControllerPid) ->
    gen_server:start_link(?MODULE, [UUID, ControllerPid], []).

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
init([UUID, ControllerPid]) ->
    State = #state{cnode = freeswitch@mindcoder, uuid = UUID, controller_pid = ControllerPid},
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

handle_call(_Request, _From, State) ->
    error_logger:info_msg("Got ~p",[_Request]),
    Reply = ok,
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
handle_cast(_Msg, State) ->
    error_logger:info_msg("Got ~p",[_Msg]),
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
handle_info({call, {event, [UUID | Rest]}}, #state{cnode = Node} = State) ->
    error_logger:info_msg("Got initial call event ~p",[UUID]),
    Number = proplists:get_value("Channel-Destination-Number", Rest),
    NewState = State#state{number= Number},
    {noreply, NewState};

handle_info({call_event, {event, [UUID | Rest]}}, #state{cnode = Node, number = Number, uuid = UUID} = State) ->
    Event = proplists:get_value("Event-Name", Rest),
    case Event of 
	"CHANNEL_PARK" ->
	    error_logger:info_msg("A-leg is PARKED: ~p, now we will try the our call flow to number ~p",[UUID, Number]),
	    freeswitch:sendmsg(Node, UUID,
	    		       [{"call-command", "execute"},
	    		       {"execute-app-name", "bridge"},
	    		       {"execute-app-arg", "user/" ++ Number}]),
	    {noreply, State};
	"CHANNEL_BRIDGE" ->
	    error_logger:info_msg("We bridged to a B-leg ~p",[UUID]),
	    {noreply, State};
	_ ->
	    error_logger:info_msg("Got fucked ~p",[Event]),
	    {noreply, State}
    end;
handle_info(call_hangup, #state{uuid = UUID, controller_pid = ControllerPid} = State)->
    error_logger:info_msg("Got call_hangup event for: ~p, will proceed to deallocate A-leg", [UUID]),
    ControllerPid ! {deallocate_me, UUID, self()},
    {stop, normal, State};
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
terminate(_Reason, _State) ->
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
