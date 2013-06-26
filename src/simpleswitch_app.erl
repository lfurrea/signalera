%%%-------------------------------------------------------------------
%%% @author Luis F Urrea <lfurrea@simplecs.net>
%%% @copyright (C) 2012, Luis F Urrea
%%% @doc
%%%
%%% @end
%%% Created : 26 Nov 2012 by Luis F Urrea <lfurrea@simplecs.net>
%%%-------------------------------------------------------------------

-module(simpleswitch_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-type startlink_err() :: {'already_started', pid()} | 'shutdown' | term().

%% ===================================================================
%% Application callbacks
%% ===================================================================


%%--------------------------------------------------------------------
%% @public
%% @doc
%% Called when the OTP system wants to start the application,
%% it must perform the actual startup and return the process id of the
%% root supervisor.
%% We can do any other startup tasks here as well, such as reading
%% a configuration file, initialize ETS tables a.s.o.
%% @end
%%--------------------------------------------------------------------

-spec start/2 :: (StartType, StartArgs) -> tuple(ok, pid()) | tuple(error, startlink_err()) when
      StartType :: term(),
      StartArgs :: term().

start(_StartType, _StartArgs) ->
    simpleswitch_sup:start_link().

%%--------------------------------------------------------------------
%% @public
%% @doc
%% Called when the OTO system wants to shutdown the application,
%% nothing special to do so we ignore the param and return ok.
%% @end
%%--------------------------------------------------------------------

-spec stop/1 :: (State) -> ok when
      State :: term().

stop(_State) ->
    ok.
