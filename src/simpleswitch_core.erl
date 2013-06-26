%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@.simplecs.net>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2012 by Luis F Urrea <lfurrea@.simplecs.net>
%%%-------------------------------------------------------------------
-module(simpleswitch_core).

-behaviour(gen_server).

%% API
-export([start_link/0, deallocate_me/2]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 

-record(state, {dict}).

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

-spec start_link() -> startlink_ret().

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

-spec init([]) -> {ok, #state{}}.

init([]) ->
    {ok, #state{dict = dict:new()}}.

%%--------------------------------------------------------------------
%% @public
%% @doc
%% A session object is an abstraction  that  provides an API to be able
%% to interact with the channel. This is called when there is nothing 
%% else to do with this session and the process needs to be shutdown.
%% @end
%%--------------------------------------------------------------------

-spec deallocate_me(string(), pid()) -> ok.

deallocate_me(UUID, Pid) ->
    gen_server:cast(?SERVER, {deallocate_me, UUID, Pid}).


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

-spec handle_call(term(), {pid(), _}, #state{}) -> {reply, not_implemented, #state{}}.

handle_call(_Request, _From, State) ->
    {reply, not_implemented, State}.

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

-spec handle_cast({deallocate_me, string(), pid()}, #state{}) -> {noreply, #state{}}.

handle_cast({deallocate_me, UUID, _Pid}, #state{dict = Uuid2Pid} = State) ->
    NewDict = dict:erase(UUID, Uuid2Pid),
%%    supervisor:terminate_child(aleg_session_sup, Pid),
    error_logger:info_msg("Successfully deallocated ~p", [UUID]),
    {noreply, State#state{dict=NewDict}}.


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

-spec handle_info({get_pid, string(), reference(), pid()}, #state{}) -> {noreply, #state{}}.

handle_info({get_pid, UUID, Ref, From}, State) ->
    {SessionPid, NewState} = find_or_create_session(UUID, State),
    From ! {Ref, SessionPid},
    error_logger:info_msg("Sending events to ~p", [SessionPid]),
    {noreply, NewState}.

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

-spec terminate(term(), #state{}) -> ok.

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

-spec code_change(term(), #state{}, term()) -> {ok, #state{}}.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Called in order to get a handle on an existing session object by 
%% uuid, or when a new one is required.
%% TODO: Return value when there is an issue starting a session ob.
%% @end
%%--------------------------------------------------------------------


-spec find_or_create_session(string(), #state{}) -> {pid(), #state{}}.

find_or_create_session(UUID, #state{dict = Uuid2Pid} = State) ->
    case dict:find(UUID, Uuid2Pid) of
	{ok, Pid} ->
	    {Pid, State};
	_ ->
	    {ok, AlegPid} = supervisor:start_child(aleg_session_sup,[UUID, self()]),
	    {AlegPid, State#state{
			dict = dict:store(UUID, AlegPid, Uuid2Pid)
		       }}
    end.
