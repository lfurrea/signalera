%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@simplecs.net>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc Call request server to ring an endpoint as part of a callflow,
%%%      it attempts to bridge to and endpoint or returns continue if
%%%      it fails, returns stop when sucessful
%%% @end
%%% Created : 16 Nov 2012 by Luis F Urrea <lfurrea@simplecs.net>
%%%-------------------------------------------------------------------
-module(cf_device).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks

-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	terminate/2, code_change/3]).

-define(SERVER, ?MODULE).

-include("callflow.hrl").

-type startlink_err() :: {'already_started', pid()} |'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | 'ignore' | {'error', startlink_err()}.

%%% ISSUES

%%--------------------------------------------------------------------
%% Originator Cancel
%% When the Aleg cancels the call before we have bridged we need to
%% cancel the Bleg
%%--------------------------------------------------------------------


%%% API

%%--------------------------------------------------------------------
%% @doc Starts the server. Wrapper around start_link/4 to start
%%      a gen_server container process
%% @spec start_link(Call::record()) -> {ok, Pid}
%% where
%%  Pid = pid()
%% @end
%%--------------------------------------------------------------------

-spec start_link(#cf_call{}) -> startlink_ret().

start_link(Call) ->
    gen_server:start_link(?MODULE, Call, []).


%%--------------------------------------------------------------------
%% @doc Stops the server. Asynchronously sends atom stop to the server
%%      In theory we should stop the server when we receive a
%%      call_hangup event, or if we failed to bridge to an endpoint. This 
%%      server was triggered by the call flow
%%      executioner, the executioner is stopped after a successful bridge
%%      but this server remains until the bleg is hung up.
%% @spec
%% @end
%%--------------------------------------------------------------------

%% -spec stop() -> no_return().

%% stop() ->
%%     gen_server:cast(?SERVER, stop).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Entry point for this module, attempts to call an endpoint as defined
%% in the Call payload. Returns continue if fails to connect or stop
%% when successfull, gen_server doesnt start until init returns so
%% it should quickly and explictly return when things go wrong
%% @spec
%% @end
%%--------------------------------------------------------------------

-spec init(#cf_call{}) -> {ok, #cf_call{}, non_neg_integer()} | {stop, term()} | ignore.

init(#cf_call{cnode=FSnode, cf_pid=CFPid}=Call) ->
    case freeswitch:api(FSnode, create_uuid) of
	{ok, UUID} ->
	    NewCall = Call#cf_call{bleg_uuid=UUID},
	    {ok, NewCall, 0};
	{error, Reason} ->
	    CFPid ! {continue},
	    {stop, Reason};
	timeout ->
	    CFPid ! {continue},
	    {stop, timeout}
    end.






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

-spec handle_info(timeout | term(), #cf_call{}) -> {noreply, #cf_call{}} | {stop, term(), #cf_call{}}.

handle_info(timeout, #cf_call{cf_pid=CFPid} = Call) ->
    case build_dial_string(Call) of
	{ok, DialString} ->
	    bridge_to_endpoint(Call, DialString, CFPid);
	{error, no_dial_string} ->
	    error_logger:info_msg("no endpoint to bridge to, ~p"),
	    CFPid ! {continue},
	    {stop, normal, Call}
    end;
handle_info({call_event, {event, [UUID | Rest]}}, Call) ->
    Event = proplists:get_value("Event-Name", Rest),
    case Event of 
	"CHANNEL_PARK" ->
	    error_logger:info_msg("B-leg is PARKED: ~p, now we will bridge our two legs",[UUID]),
	    {noreply, Call};
	"CHANNEL_BRIDGE" ->
	    error_logger:info_msg("We bridged to a B-leg ~p",[UUID]),
	    {noreply, Call};
	_ ->
	    error_logger:info_msg("Got fucked ~p",[Event]),
	    {noreply, Call}
    end;
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

-spec terminate(term(), #cf_call{}) -> ok.

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

-spec code_change(term(), #cf_call{}, term()) -> {ok, #cf_call{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc Builds proper dialstring to ring an endpoint
%% 
%% @end
%%--------------------------------------------------------------------

-spec build_dial_string(#cf_call{}) -> {ok, string()} | {error, atom()}.

build_dial_string(#cf_call{destination_number=DestNumber, bleg_uuid=BlegId}) ->
    case DestNumber =/= 'undefined' of
	true ->
	    {ok, "{origination_uuid=" ++ BlegId ++ "}user/" ++ DestNumber};
	false ->
	    {error, no_dial_string}
    end.
%
%--------------------------------------------------------------------
%% @private
%% @doc Make a backgrounded API call to FreeSWITCH to originate &park a 
%% channel to the proper built endpoint. The asynchronous reply is
%% sent to calling process (We are not the calling process since this is
%% encapsulated by freeswitch:bgapi) after it is received, so for us
%% this is a blocking call with a timeout. This function
%% returns the result of the initial bgapi call or `timeout' if FreeSWITCH fails
%% to respond.
%% 
%% @end
%%--------------------------------------------------------------------

-spec bridge_to_endpoint(#cf_call{}, string(), pid()) -> {noreply, #cf_call{}} | {stop, term(), #cf_call{}}.

bridge_to_endpoint(#cf_call{cnode=FSnode, bleg_uuid=BlegId}=Call, DialString, Parent) ->
    case freeswitch:bgapi(FSnode, originate, DialString ++ " &park()") of
	{ok, _BgJobId} ->
	    %% Here we get the handle to our call, the fact that we don't get the
	    %% handle doesn't mean that the call did not go through. The use of bgapi
	    %% in a non blocking call is not relevant here, we stay is the case above
	    %% until we receive, so whats important here is that we dont miss the handle
	    %% otherwise we will have a phantom call
	    case freeswitch:handlecall(FSnode,BlegId) of
		ok ->
		    error_logger:info_msg("Starting channel for ~p our handle is ~p", [BlegId, self()]), 
		    {noreply, Call};
		{error, Reason} ->
		    %%TODO: we should hangup the call if we miss the handle
		    error_logger:info_msg("Crap this is a real error, because ~p", [Reason]),
		    {stop,normal, Call};
		timeout  ->
		    error_logger:info_msg("timeout: Never got a handle response from FS"),
		    {stop,timeout, Call}
	    end;
	{error, Failure} ->
	    error_logger:info_msg("Bleg originate timedout to get a JobId"),
	    Parent ! {continue},
	    {stop, Failure, Call};
	timeout ->
	    error_logger:info_msg("Bleg originate timedout to get a JobId"),
	    Parent ! {continue},
	    {stop, timeout, Call}
    end.
    
