%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created : 15 Nov 2012 by Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%%-------------------------------------------------------------------
-module(cf_exe_sup).

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

-spec init(Args) -> sup_init_ret() when
      Args :: [].

init([]) ->
    RestartStrategy = simple_one_for_one,
    MaxRestarts = 0,
    MaxSecondsBetweenRestarts = 1,

    SupFlags = {RestartStrategy, MaxRestarts, MaxSecondsBetweenRestarts},

    {ok, {SupFlags, [
		     {cf_exe, {cf_exe, start_link, []}, temporary, 2000, worker, [cf_exe]}
		     ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
