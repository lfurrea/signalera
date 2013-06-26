%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@simplecs.net>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2012 by Luis F Urrea <lfurrea@simplecs.net>
%%%-------------------------------------------------------------------
-module(simpleswitch_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().
-type startlink_ret() :: {'ok', pid()} | ignore | {'error', startlink_err()}.
-type sup_child_spec() :: supervisor:child_spec().
-type sup_child_specs() :: [sup_child_spec()] | [].
-type sup_start_flags() :: {supervisor:strategy(), non_neg_integer(), non_neg_integer()}.
-type sup_init_ret() :: ignore | {'ok', {sup_start_flags(), sup_child_specs()}}.

-spec init(Args) -> sup_init_ret() when
      Args :: [].

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------

-spec start_link() -> startlink_ret().

start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).

%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------



init([]) ->
    FsWorker = {simpleswitch_core, {simpleswitch_core, start_link, []},
	       permanent, 2000, worker, [simpleswitch_core]},
    AlegSup = {aleg_session_sup, {aleg_session_sup, start_link, []}, 
	      permanent, 2000, supervisor, [aleg_session_sup]},
    CFexeSup = {cf_sup, {cf_exe_sup, start_link, []}, 
	      permanent, 2000, supervisor, [cf_exe_sup]},
    Children = [FsWorker, AlegSup, CFexeSup],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
