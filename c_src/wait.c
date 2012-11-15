#include <unistd.h>
#include "erl_nif.h"

static ERL_NIF_TERM ATOM_ERROR;
static ERL_NIF_TERM ATOM_OK;

static ERL_NIF_TERM busywait_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int count;

  if(!enif_get_uint(env, argv[0], &count))
    return ATOM_ERROR;

  for(; count > 0; --count) {
  }
  return ATOM_OK;
}

static ERL_NIF_TERM sleep_nif(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[])
{
  unsigned int count;

  if(!enif_get_uint(env, argv[0], &count))
    return ATOM_ERROR;

  usleep(count);
  return ATOM_OK;
}

static ErlNifFunc nif_funcs[] =
{
  {"busywait_nif", 1, busywait_nif},
  {"sleep_nif", 1, sleep_nif}
};

static int on_load(ErlNifEnv* env, void** priv_data, ERL_NIF_TERM load_info)
{
  ATOM_ERROR = enif_make_atom(env, "error");
  ATOM_OK = enif_make_atom(env, "ok");
  return 0;
}

ERL_NIF_INIT(wait, nif_funcs, &on_load, NULL, NULL, NULL)
