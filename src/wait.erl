-module(wait).
-compile(export_all).
-on_load(init/0).

-define(ASYNC_NIF_CALL(Fun, Args),
        begin
            Ref = erlang:make_ref(),
            case erlang:apply(Fun, [Ref|Args]) of
                {ok, Metric} ->
                    io:format("queue depth: ~p~n", [Metric]),
                    erlang:bump_reductions(Metric * 100), %% TODO: 100 is a *guess*
                    receive
                        {Ref, Reply} ->
                            Reply
                    end;
                Other -> Other
            end
        end).

%% These are called via erlang:apply/3 within ASYNC_NIF_CALL()s
-compile([{nowarn_unused_function,
           [
            {sleep_nif, 2}
           ]}]).


%%  N = number of spawned procs
%%  W = microseconds spent waiting in NIF
%%  R = number of repeat calls to NIF
%% BW = iterations of pure Erlang busy wait
%%
%% Example that piled up single scheduler for me:
%% wait:run(100,300000, 5, 10000).
%% wait:run(50,300000, 10, 5000000).
%% wait:run(50,300000, 5, 50000000).
%% wait:run(50,300000, 5, 100000000).
%% wait:run(50,300000, 5, 200000000).
run(N,W,R,BW) ->
    Repeat = lists:seq(1,R),
    spawn_n(N, fun() ->
                       erlang:display(erlang:statistics(run_queues)),
                       [sleep(W) || _ <- Repeat],
                       erlang:display(erlang:statistics(run_queues)),
                       busywait(BW),
                       erlang:display(process_info(self(), [reductions])),
                       erlang:display(erlang:statistics(run_queues))
               end).

spawn_n(0, _) ->
    ok;
spawn_n(N, F) ->
    spawn(F),
    spawn_n(N-1, F).

busywait(0) ->
    ok;
busywait(N) ->
    %ASYNC_NIF_CALL(fun busywait_nif/2, [N]).
    busywait(N-1).

busywait_nif(_Ref, _Count) ->
    not_loaded.

sleep(Microseconds) ->
    case ?ASYNC_NIF_CALL(fun sleep_nif/2, [Microseconds]) of
        {eror, shutdown}=Error ->
            Error;
        {error, _Reason}=Error ->
            Error;
        {ok, Slept} ->
            io:format("slept ~p~n", [Slept]),
            ok
    end.

sleep_nif(_Ref, _Microseconds) ->
    not_loaded.

init() ->
    case code:which(?MODULE) of
        Filename when is_list(Filename) ->
            NIF = filename:join([filename:dirname(Filename),"../priv", "wait"]),
            erlang:load_nif(NIF, 0)
    end.
