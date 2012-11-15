%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created :  6 Nov 2012 by Luis F Urrea <lfurrea@mindcoder.simplecs.sa>
%%%-------------------------------------------------------------------
-module(fs_outbound_extn_sup).

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
    {ok, {{simple_one_for_one, 0, 1},
          [
           {outbound_extn_controller, {fs_outbound_extn_controller, start_link, []},
            temporary, 2000, worker, [fs_outbound_extn_controller]}
          ]}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
