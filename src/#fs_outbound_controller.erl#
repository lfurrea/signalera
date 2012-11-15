%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2012 by Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%%-------------------------------------------------------------------
-module(fs_outbound_controller).

-behaviour(gen_server).

%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {dict}).

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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).

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
init([]) ->
    {ok, #state{dict = dict:new()}}.

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
handle_call({reply, Ref, ExtnPid}, _From, State) ->
    Reply = {Ref, ExtnPid},
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
handle_info({get_pid, UUID, Ref, From}, State) ->
    {ExtnPid, NewState} = find_or_create_extn(UUID, State),
    From ! {Ref, ExtnPid},
    {noreply, NewState};

handle_info({deallocate_me, UUID, Pid}, #state{dict = Uuid2Pid} = State) ->
    NewDict = dict:erase(UUID, Uuid2Pid),
    supervisor:terminate_child(fs_outbound_extn_sup, Pid),
    error_logger:info_msg("Successfully deallocated ~p", [UUID]),
    {noreply, State#state{dict=NewDict}}.

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

find_or_create_extn(UUID, #state{dict = Uuid2Pid} = State) ->
    case dict:find(UUID, Uuid2Pid) of

	{ok, Pid} ->
	    {Pid, State};
	_ ->
	    {ok, AlegPid} = supervisor:start_child(fs_outbound_extn_sup,[UUID, self()]),
	    {AlegPid, State#state{
			dict = dict:store(UUID, AlegPid, Uuid2Pid)
		       }}
    end.
