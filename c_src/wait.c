#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "erl_nif.h"
#include "async_nif.h"


static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_OK;

ASYNC_NIF_DECL(busywait_nif,
  {
    /* Store your arguments during the work block here,
       this becomes a struct {} which you later use in the
       work block: `args->___` */
    unsigned int count;
  },
  {
    /* Check pre-conditions and gather values from arguments for
       use later.  If you need a arg, enif_make_copy() here
       and then in the post block be sure to call enif_free().
       Returns: {ok, Metric} | {error, Reason}
       `Metric` is just a non-descript way to say queue-depth
       which could be used to adjust the reductions and create
       back pressure in the VM and help the scheduler do the
       right thing. */
    if(!enif_get_uint(env, argv[0], &args->count))
      return ATOM_ERROR;
  },
  {
    /* Perform work in this block, sends async reply to calling process.

       An call into a NIF function that doesn't block the scheduler
       always looks like:
 
       do_something_async_nif(_Arg1, _Arg2) ->
         nif_not_loaded.

       do_something(AnArg, AnotherArg) ->
         Result =
          case do_something_async_nif(Arg1, Arg2) of
            {ok, Metric} ->
              erlang:bump_reductions(Metric * Magic),
              receive
                {eror, shutdown}=Error ->
                    %% Work unit was not executed, requeue it.
                    Error;
                {error, _Reason}=Error ->
                    %% Work unit returned an error.
                    Error;
                {ok, Result} ->
                    Result
            after
                Timeout ->
                    throw({error, timeout, erlang:make_ref()})
            end
          end,
          ...
        end.
    */
    while(args->count > 0) { args->count--; }
    ERL_NIF_TERM msg = enif_make_tuple2(env, ATOM_OK, enif_make_int(env, args->count));
    /* or enif_make_resource(); enif_release_resource(); */
    ASYNC_NIF_REPLY(msg);
  },
  {
    /* Release resources allocted to hold arguments in this
       block.
       Normally this should be a few enif_free(args->___) calls.

       This block will be called:
       1) when the work is finished
       2) during shutdown when the work queue is destroyed
       3) if something goes wrong setting up the work block
    */
    /* NOTHING ALLOCATED/RETAINED so nothing to do here. */
  });

ASYNC_NIF_DECL(sleep_nif,
  { unsigned int count; }, 
  {
    if(!enif_get_uint(env, argv[0], &(args->count)))
      return ATOM_ERROR;
  },
  {
    usleep(args->count);
    ERL_NIF_TERM msg = enif_make_tuple2(env, ATOM_OK, enif_make_int(env, args->count));
    ASYNC_NIF_REPLY(msg);
  },
  {});

static ErlNifFunc nif_funcs[] =
{
  {"busywait_nif", 1, busywait_nif},
  {"sleep_nif", 1, sleep_nif}
};

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_OK = enif_make_atom(env, "ok");
  ASYNC_NIF_LOAD();
  return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
  ASYNC_NIF_UNLOAD()
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  ASYNC_NIF_UPGRADE()
  return 0;
}

ERL_NIF_INIT(wait, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
