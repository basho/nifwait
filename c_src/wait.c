#include <unistd.h>
#include <stdio.h>
#include <string.h>
#include <errno.h>

#include "erl_nif.h"
#include "async_nif.h"


static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_OK;


ASYNC_NIF_DECL(busywait_nif, { unsigned int count; }, {})
{
  if(!enif_get_uint(env, argv[0], &args->count))
    return ATOM_ERROR;

  ASYNC_NIF_RETURN(busywait_nif, ATOM_OK, {
      for(; args->count > 0; --(args->count)) {  }
      ERL_NIF_TERM msg = enif_make_tuple2(env, ATOM_OK, enif_make_int(env, args->count));
      ASYNC_NIF_REPLY(msg);
    });
}

ASYNC_NIF_DECL(sleep_nif, { unsigned int count; }, {})
{
  if(!enif_get_uint(env, argv[0], &(args->count)))
    return ATOM_ERROR;

  ASYNC_NIF_RETURN(sleep_nif, ATOM_OK, {
      usleep(args->count);
      ERL_NIF_TERM msg = enif_make_tuple2(env, ATOM_OK, enif_make_int(env, args->count));
      ASYNC_NIF_REPLY(msg);
    });
}

static ErlNifFunc nif_funcs[] =
{
  {"busywait_nif", 2, busywait_nif},
  {"sleep_nif", 2, sleep_nif}
};

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_OK = enif_make_atom(env, "ok");
  ASYNC_NIF_INIT();
  return 0;
}

static void on_unload(ErlNifEnv* env, void* priv_data)
{
  ASYNC_NIF_SHUTDOWN()
}

static int on_upgrade(ErlNifEnv* env, void** priv_data, void** old_priv_data, ERL_NIF_TERM load_info)
{
  ASYNC_NIF_SHUTDOWN()
  return 0;
}

ERL_NIF_INIT(wait, nif_funcs, &on_load, NULL, &on_upgrade, &on_unload)
