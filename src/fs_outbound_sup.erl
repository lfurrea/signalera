%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2012 by Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%%-------------------------------------------------------------------
-module(fs_outbound_sup).

-behaviour(supervisor).

%% API
-export([start_link/0]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

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
    FsWorker = {outbound_controller, {fs_outbound_controller, start_link, []},
	       permanent, 2000, worker, [fs_outbound_controller]},
    ExtnSup = {outbound_extn_sup, {fs_outbound_extn_sup, start_link, []}, 
	      permanent, 2000, supervisor, [fs_outbound_extn_sup]},
    Children = [FsWorker, ExtnSup],
    RestartStrategy = {one_for_one, 10, 10},
    {ok, {RestartStrategy, Children}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
