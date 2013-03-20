-module(wait).
-compile(export_all).
-on_load(init/0).

%%  N = number of spawned procs
%%  W = size of binary (in bytes) to MD5
%%  R = number of repeat calls MD5
%% BW = iterations of pure Erlang busy wait
%%
run([NS, WS, RS, BWS]) ->
    run(list_to_integer(NS),
        list_to_integer(WS),
        list_to_integer(RS),
        list_to_integer(BWS)).

run(N,W,R,BW) ->
    Repeat = lists:seq(1,R),
    Bin = random_binary(W, <<>>),
    spawn_n(N, fun() ->
                       report(),
                       lists:foreach(fun(_) ->
                                             report(),
                                             M1 = crypto:md5_init(),
                                             M2 = crypto:md5_update(M1, Bin),
                                             crypto:md5_final(M2),
                                             busywait(BW)
                                     end, Repeat),
                       report(),
                       report()
               end).

report() ->
    {reductions, Reds} = process_info(self(), reductions),
    RQ = erlang:statistics(run_queues),
    erlang:display({RQ, Reds}).
   
spawn_n(0, _) ->
    ok;
spawn_n(N, F) ->
    spawn(F),
    spawn_n(N-1, F).

busywait(0) ->
    ok;
busywait(N) ->
    busywait(N-1).

busywait_nif(_) ->
    not_loaded.

sleep_nif(_) ->
    not_loaded.

init() ->
    case code:which(?MODULE) of
        Filename when is_list(Filename) ->
            NIF = filename:join([filename:dirname(Filename),"../priv", "wait"]),
            erlang:load_nif(NIF, 0)
    end.

random_binary(0, Bin) ->
    Bin;
random_binary(N, Bin) ->
    X = random:uniform(255),
    random_binary(N-1, <<Bin/binary, X:8/integer>>).
