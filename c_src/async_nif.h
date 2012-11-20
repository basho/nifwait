#ifndef __ASYNC_NIF_H__
#define __ASYNC_NIF_H__

/* Redefine this in your NIF implementation before including this file to
   change the thread pool size. */
#ifndef ANIF_MAX_WORKERS
#define ANIF_MAX_WORKERS 64
#endif

#ifndef __offsetof
#define __offsetof(st, m) \
     ((size_t) ( (char *)&((st *)0)->m - (char *)0 ))
#endif
#include "queue.h"

struct anif_req_entry {
  ErlNifPid pid;
  void *args;
  unsigned int assigned_to_worker;
  void (*fn_work)(ErlNifEnv*, ErlNifPid*, void *);
  void (*fn_post)(void *);
  STAILQ_ENTRY(anif_req_entry) entries;
};
STAILQ_HEAD(reqs, anif_req_entry) anif_reqs = STAILQ_HEAD_INITIALIZER(anif_reqs);

struct anif_worker_entry {
  ErlNifTid tid;
  ErlNifEnv *env;
  unsigned int worker_num;
  LIST_ENTRY(anif_worker_entry) entries;
};
LIST_HEAD(idle_workers, anif_worker_entry) anif_idle_workers = LIST_HEAD_INITIALIZER(anif_worker);

static volatile unsigned int anif_req_count = 0;
static volatile unsigned int anif_shutdown = 0;
static ErlNifMutex *anif_req_mutex = NULL;
static ErlNifMutex *anif_worker_mutex = NULL;
static ErlNifCond *anif_cnd = NULL;
static struct anif_worker_entry anif_worker_entries[ANIF_MAX_WORKERS];

#define ET2_2A(A, B) enif_make_tuple2(env, enif_make_atom(env, A), enif_make_atom(env, B))
#define ET2_1A1I(A, B) enif_make_tuple2(env, enif_make_atom(env, A), enif_make_int(env, B))

#define ASYNC_NIF_DECL(name, frame, pre_block, work_block, post_block)  \
  struct name ## _args frame;                                           \
  static void fn_work ## name (ErlNifEnv *env, ErlNifPid *pid, struct name ## _args *args) \
       work_block;                                                      \
  static void fn_post ## name (struct name ## _args *args) {            \
    do post_block while(0);                                             \
    enif_free(args);                                                    \
  }                                                                     \
  static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) { \
    struct name ## _args on_stack_args;                                 \
    struct name ## _args *args = &on_stack_args;                        \
    struct name ## _args *copy_of_args;                                 \
    struct anif_req_entry *r;                                           \
    ErlNifPid pid;                                                      \
    if (!enif_self(env, &pid))                                          \
      return ET2_2A("error", "pid");                                    \
    if (anif_shutdown) {                                                \
      enif_send(NULL, &pid, env,                                        \
                enif_make_tuple2(env, enif_make_atom(env, "error"),     \
                                 enif_make_atom(env, "shutdown")));     \
      return ET2_2A("error", "shutdown");                               \
    }                                                                   \
    do pre_block while(0);                                              \
    r = enif_alloc(sizeof(struct anif_req_entry));                      \
    if (!r) {                                                           \
      fn_post ## name (args);                                                    \
      return ET2_2A("error", "enomem");                                 \
    }                                                                   \
    copy_of_args = enif_alloc(sizeof(struct name ## _args));            \
    if (!copy_of_args) {                                                \
      fn_post ## name (args);                                                    \
      enif_free(r);                                                     \
      return ET2_2A("error", "enomem");                                 \
    }                                                                   \
    memcpy(copy_of_args, args, sizeof(struct name ## _args));           \
    memcpy(&(r->pid), &pid, sizeof(ErlNifPid));                         \
    r->args = (void *)copy_of_args;                                     \
    r->fn_work = (void (*)(ErlNifEnv *, ErlNifPid*, void *))fn_work ## name ; \
    r->fn_post = (void (*)(void *))fn_post ## name;                     \
    anif_enqueue_req(r);                                                \
    return ET2_1A1I("ok", anif_req_count);                              \
  }

#define ASYNC_NIF_LOAD() if (anif_init() != 0) return -1;
#define ASYNC_NIF_UNLOAD() anif_unload();
#define ASYNC_NIF_UPGRADE() anif_unload();

#define ASYNC_NIF_REPLY(msg) enif_send(NULL, pid, env, msg)

static void anif_enqueue_req(struct anif_req_entry *r)
{
  /* Add the request to the work queue. */
  enif_mutex_lock(anif_req_mutex);
  STAILQ_INSERT_TAIL(&anif_reqs, r, entries);
  anif_req_count++;
  enif_mutex_unlock(anif_req_mutex);
  enif_cond_broadcast(anif_cnd);
}

static void *anif_worker_fn(void *arg)
{
  struct anif_worker_entry *worker = (struct anif_worker_entry *)arg;
  struct anif_req_entry *req = NULL;

  /*
   * Workers are active while there is work on the queue to do and
   * only in the idle list when they are waiting on new work.
   */
  while(!anif_shutdown) {
    /* Examine the request queue, are there things to be done? */
    enif_mutex_lock(anif_req_mutex); check_again_for_work:
    if (!anif_shutdown && (req = STAILQ_FIRST(&anif_reqs)) == NULL) {
      /* Queue is empty, join the list of idle workers and wait for work */
      enif_mutex_lock(anif_worker_mutex);
      LIST_INSERT_HEAD(&anif_idle_workers, worker, entries);
      enif_mutex_unlock(anif_worker_mutex);
      enif_cond_wait(anif_cnd, anif_req_mutex);
      if (anif_shutdown) {
        enif_mutex_unlock(anif_req_mutex);
        break; /* Exit the do/while loop. */
      }
      goto check_again_for_work;
    } else {
      /* `req` is our work request and we hold the lock. */
      enif_cond_broadcast(anif_cnd);

      /* Clear the worker's environment as it's invalid after each use. */
      enif_clear_env(worker->env);

      /* Take the request off the queue. */
      STAILQ_REMOVE(&anif_reqs, req, anif_req_entry, entries); anif_req_count--;

      /* Now we need to remove this thread from the list of idle threads. */
      enif_mutex_lock(anif_worker_mutex);
      LIST_REMOVE(worker, entries);

      /* Release the locks in reverse order that we acquired them,
         so as not to self-deadlock. */
      enif_mutex_unlock(anif_worker_mutex);
      enif_mutex_unlock(anif_req_mutex);

      /* Finally, let's do the work! :) */
      req->assigned_to_worker = worker->worker_num;
      req->fn_work(worker->env, &(req->pid), req->args);
      req->fn_post(req->args);
      enif_free(req);
    }
  }
  enif_thread_exit(0);
  return 0;
}

static void anif_unload(void)
{
  /* Don't shutdown more than once at a time. */
  if (anif_shutdown) /* TODO */
    return;

  /* Signal the worker threads, stop what you're doing and exit. */
  anif_shutdown = 1;
  enif_cond_broadcast(anif_cnd);

  /* Join for the now exiting worker threads. */
  for (unsigned int i = 0; i < ANIF_MAX_WORKERS; ++i) {
    void *exit_value = 0; /* Ignore this. */
    enif_thread_join(anif_worker_entries[i].tid, &exit_value);
  }

  /* Worker threads are stopped, now toss anything left in the queue. */
  enif_mutex_lock(anif_req_mutex);
  struct anif_req_entry *e = NULL;
  STAILQ_FOREACH(e, &anif_reqs, entries) {
    ErlNifEnv *env = anif_worker_entries[e->assigned_to_worker].env;
    STAILQ_REMOVE(&anif_reqs, STAILQ_LAST(&anif_reqs, anif_req_entry, entries),
                  anif_req_entry, entries);
    enif_send(NULL, &(e->pid), env,
              enif_make_tuple2(env, enif_make_atom(env, "error"),
                               enif_make_atom(env, "shutdown")));
    e->fn_post(e->args);
    enif_free(e);
    anif_req_count--;
  }
  enif_mutex_unlock(anif_req_mutex);

  /* Clean up resources owned by the now exited worker threads. */
  for (unsigned int i = 0; i < ANIF_MAX_WORKERS; ++i) {
    enif_free_env(anif_worker_entries[i].env);
  }
  enif_cond_destroy(anif_cnd);
  /* Not strictly necessary. */
  memset(anif_worker_entries, sizeof(struct anif_worker_entry) * ANIF_MAX_WORKERS, 0);

  enif_mutex_destroy(anif_req_mutex); anif_req_mutex = NULL;
  enif_mutex_destroy(anif_worker_mutex); anif_worker_mutex = NULL;
}

static int anif_init(void)
{
  /* TODO: do we need this?
     Don't init more than once.
  if (anif_req_mutex)
    return -1;

  if (anif_shutdown == 1)
    return -1;
  */

  anif_req_mutex = enif_mutex_create("anif_req stailq");
  anif_worker_mutex = enif_mutex_create("anif_worker list");
  anif_cnd = enif_cond_create("anif_worker");

  /* Setup the requests management. */
  anif_req_count = 0;

  /* Setup the thread pool management. */
  enif_mutex_lock(anif_worker_mutex);
  for (unsigned int i = 0; i < ANIF_MAX_WORKERS; i++) {
    anif_worker_entries[i].worker_num = i;
    anif_worker_entries[i].env = enif_alloc_env();
    enif_thread_create("anif_worker", &anif_worker_entries[i].tid,
                       &anif_worker_fn, (void*)&anif_worker_entries[i], NULL);
  }
  enif_mutex_unlock(anif_worker_mutex);
  return 0;
}

#endif // __ASYNC_NIF_H__
