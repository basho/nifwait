-module(wait).
-compile(export_all).
-on_load(init/0).

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
    busywait(N-1).

busywait_nif(_Count, _Pid) ->
    not_loaded.

sleep(Microseconds) ->
    Timeout = 2 * Microseconds,
    {ok, Metric} = sleep_nif(Microseconds, self()),
    erlang:yield(),
    io:format("queue depth: ~p~n", [Metric]),
    erlang:bump_reductions(Metric),
    receive
        {eror, shutdown}=Error ->
            Error;
        {error, _Reason}=Error ->
            Error;
        {ok, Slept} ->
            io:format("slept ~p~n", [Slept]),
            ok
    after
        Timeout ->
            throw({error, timeout, erlang:make_ref()})
    end.

sleep_nif(_Microseconds, _Pid) ->
    not_loaded.

init() ->
    case code:which(?MODULE) of
        Filename when is_list(Filename) ->
            NIF = filename:join([filename:dirname(Filename),"../priv", "wait"]),
            erlang:load_nif(NIF, 0)
    end.
