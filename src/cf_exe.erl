%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@simplecs.net>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%% Inspired on whistle platform callflows not a bit by bit copy
%%% Executes callflow modules sequentially, moves to subsequent flows
%%% depending on the outcome or previous 
%%%
%%% @end
%%% Created : 15 Nov 2012 by Luis F Urrea <lfurrea@simplecs.net>
%%%-------------------------------------------------------------------
-module(cf_exe).

-include("callflow.hrl").

%% API
-export([start_link/2]).
-export([init/3]).

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Sets up callflow executioner for this call, and starts
%% execution of the first Flow
%% @end
%%--------------------------------------------------------------------

-spec start_link/2 :: (Call, Flow) -> tuple(ok, pid()) when
      Call :: #cf_call{},
      Flow :: list().

start_link(Call, Flow) ->
    proc_lib:start_link(?MODULE, init, [self(), Call, Flow]).


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes callflow execution process
%% @end
%%--------------------------------------------------------------------

-spec init/3 :: (ParentPid, Call, Flow) -> no_return() when
      ParentPid :: pid(),
      Call :: #cf_call{},
      Flow :: list().

init(ParentPid, #cf_call{}=Call, Flow) ->
    process_flag(trap_exit, true),
    NewCall = Call#cf_call{cf_pid=self()},
    proc_lib:init_ack(ParentPid, {ok, self()}),
    _ = next(NewCall, Flow, active).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Executes the top Flow from the stack on the given call request
%% then waits for the modules reply, unexpected death, or timeout.
%% @end
%%--------------------------------------------------------------------

-spec next/3 :: (Call, [] | Flow, CallStatus) -> no_return() when
      Call :: #cf_call{},
      Flow :: list(),
      CallStatus :: undefined | atom().

next(#cf_call{}, [], _CallStatus) -> self() ! {stop};
next(#cf_call{last_action=_LastAction}=Call, [H|_T] = Flow, CallStatus) ->
    CF_Module = H,
    {_Result, Pid} =
	try
	    apply(CF_Module, start_link, [Call])
	catch
	    _:_ ->
		self() ! {continue},
		{error, undefined}
	end,
    wait(Call, Flow, Pid, CallStatus).

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Waits for the module handling the current call request to reply
%% or timeout and advance the call flow accordingly
%% @spec
%% @end
%%--------------------------------------------------------------------

-spec wait/4 :: (Call, Flow, Pid, CallStatus) -> no_return() when
      Call :: #cf_call{},
      Flow :: list(),
      Pid :: undefined | pid(),
      CallStatus :: undefined | atom().

wait(#cf_call{}=Call, Flow = [_H|T], Pid, CallStatus) ->
    receive
	{continue} ->
	    next(Call, T, CallStatus);
	{stop} ->
	    cf_call_command:hangup(Call);
	_Msg ->
	    wait(Call, Flow, Pid, CallStatus)
	after
	    %% TODO: I am missing an explicit hangup on this timeout
	    120000 ->
		case CallStatus of
		    undefined ->
			cf_call_command:hangup(Call);
		    _ ->
			wait(Call, Flow, Pid, undefined)
		end
	end.
