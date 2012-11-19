#ifndef __ARQ_H__
#define __ARQ_H__

/* BEGIN: include "queue.h" */

#define __offsetof(st, m) \
     ((size_t) ( (char *)&((st *)0)->m - (char *)0 ))
/*
 * Singly-linked Tail queue declarations.
 */
#define STAILQ_HEAD(name, type)                                         \
struct name {                                                           \
        struct type *stqh_first;/* first element */                     \
        struct type **stqh_last;/* addr of last next element */         \
}

#define STAILQ_HEAD_INITIALIZER(head)                                   \
        { NULL, &(head).stqh_first }

#define STAILQ_ENTRY(type)                                              \
struct {                                                                \
        struct type *stqe_next; /* next element */                      \
}

/*
 * Singly-linked Tail queue functions.
 */
#define STAILQ_CONCAT(head1, head2) do {                                \
        if (!STAILQ_EMPTY((head2))) {                                   \
                *(head1)->stqh_last = (head2)->stqh_first;              \
                (head1)->stqh_last = (head2)->stqh_last;                \
                STAILQ_INIT((head2));                                   \
        }                                                               \
} while (0)

#define STAILQ_EMPTY(head)      ((head)->stqh_first == NULL)

#define STAILQ_FIRST(head)      ((head)->stqh_first)

#define STAILQ_FOREACH(var, head, field)                                \
        for ((var) = STAILQ_FIRST((head));                              \
           (var);                                                       \
           (var) = STAILQ_NEXT((var), field))

#define STAILQ_INIT(head) do {                                          \
        STAILQ_FIRST((head)) = NULL;                                    \
        (head)->stqh_last = &STAILQ_FIRST((head));                      \
} while (0)

#define STAILQ_INSERT_AFTER(head, tqelm, elm, field) do {               \
        if ((STAILQ_NEXT((elm), field) = STAILQ_NEXT((tqelm), field)) == NULL)\
                (head)->stqh_last = &STAILQ_NEXT((elm), field);         \
        STAILQ_NEXT((tqelm), field) = (elm);                            \
} while (0)

#define STAILQ_INSERT_HEAD(head, elm, field) do {                       \
        if ((STAILQ_NEXT((elm), field) = STAILQ_FIRST((head))) == NULL) \
                (head)->stqh_last = &STAILQ_NEXT((elm), field);         \
        STAILQ_FIRST((head)) = (elm);                                   \
} while (0)

#define STAILQ_INSERT_TAIL(head, elm, field) do {                       \
        STAILQ_NEXT((elm), field) = NULL;                               \
        *(head)->stqh_last = (elm);                                     \
        (head)->stqh_last = &STAILQ_NEXT((elm), field);                 \
} while (0)

#define STAILQ_LAST(head, type, field)                                  \
        (STAILQ_EMPTY((head)) ?                                         \
                NULL :                                                  \
                ((struct type *)                                        \
                ((char *)((head)->stqh_last) - __offsetof(struct type, field))))

#define STAILQ_NEXT(elm, field) ((elm)->field.stqe_next)

#define STAILQ_REMOVE(head, elm, type, field) do {                      \
        if (STAILQ_FIRST((head)) == (elm)) {                            \
                STAILQ_REMOVE_HEAD((head), field);                      \
        }                                                               \
        else {                                                          \
                struct type *curelm = STAILQ_FIRST((head));             \
                while (STAILQ_NEXT(curelm, field) != (elm))             \
                        curelm = STAILQ_NEXT(curelm, field);            \
                if ((STAILQ_NEXT(curelm, field) =                       \
                     STAILQ_NEXT(STAILQ_NEXT(curelm, field), field)) == NULL)\
                        (head)->stqh_last = &STAILQ_NEXT((curelm), field);\
        }                                                               \
} while (0)

#define STAILQ_REMOVE_HEAD(head, field) do {                            \
        if ((STAILQ_FIRST((head)) =                                     \
             STAILQ_NEXT(STAILQ_FIRST((head)), field)) == NULL)         \
                (head)->stqh_last = &STAILQ_FIRST((head));              \
} while (0)

#define STAILQ_REMOVE_HEAD_UNTIL(head, elm, field) do {                 \
        if ((STAILQ_FIRST((head)) = STAILQ_NEXT((elm), field)) == NULL) \
                (head)->stqh_last = &STAILQ_FIRST((head));              \
} while (0)
/* END: include "queue.h" */


STAILQ_HEAD(arq, arq_entry) arq_head;
struct arq_entry {
  ErlNifEnv* env;
  ErlNifPid pid;
  int argc;
  void *args;
  void (*fn)(ErlNifEnv*, ErlNifPid*, int, void *);
  STAILQ_ENTRY(arq_entry) entries;
};
ErlNifEnv *arq_nif_env;
ErlNifMutex *arq_mutex;
ErlNifCond *arq_cnd;
ErlNifTid arq_tid;

#define ASYNC_NIF_DECL(name, block)                                     \
  struct name ## _args block;                                           \
  static void q_ ## name(ErlNifEnv*, ErlNifPid*, int, struct name ## _args *); \
  static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) { \
    struct name ## _args *args = malloc(sizeof(struct name ## _args));  \
    if (!args) return enif_make_tuple2(env,                             \
                 enif_make_atom(env, "error"),                          \
                 enif_make_atom(env, "enomem"));                        \
    do

#define ASYNC_NIF_RETURN(name, result, block)                           \
  } while(0);                                                           \
  struct arq_entry *r;                                                  \
  r = malloc(sizeof(struct arq_entry));                                 \
  r->env = env;                                                         \
  if(!enif_get_local_pid(env, argv[argc - 1], &(r->pid))) {             \
    free(r);                                                            \
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "pid")); \
  }                                                                     \
  r->args = (void *)args;                                               \
  r->fn = (void (*)(ErlNifEnv *, ErlNifPid*, int, void *))q_ ## name;   \
  enif_mutex_lock(arq_mutex);                                           \
  STAILQ_INSERT_HEAD(&arq_head, r, entries);                            \
  enif_mutex_unlock(arq_mutex);                                         \
  enif_cond_signal(arq_cnd);                                            \
  return result;                                                        \
  }                                                                     \
  static void q_ ## name(ErlNifEnv *env, ErlNifPid *pid, int argc, struct name ## _args *args) { \
    do block while(0);

#define ASYNC_NIF_INIT() if (!arq_init()) return -1;
#define ASYNC_NIF_SHUTDOWN() arq_shutdown();

#define ASYNC_NIF_REPLY(msg) enif_send(NULL, pid, env, msg); free(args);

static int arq_alive = 0;
static void *arq_worker_fn(void *args)
{
  while(arq_alive) {
    enif_mutex_lock(arq_mutex);
    new_work:
    if (!STAILQ_EMPTY(&arq_head)) {
      struct arq_entry *e = STAILQ_LAST(&arq_head, arq_entry, entries);
      STAILQ_REMOVE(&arq_head, STAILQ_LAST(&arq_head, arq_entry, entries), arq_entry, entries);
      enif_mutex_unlock(arq_mutex);
      e->fn(e->env, &(e->pid), e->argc, e->args);
      free(e);
    } else {
      enif_cond_wait(arq_cnd, arq_mutex);
      if (arq_alive) goto new_work;
    }
  }
  return 0;
}

static int arq_init(void)
{
  arq_nif_env = enif_alloc_env();
  arq_mutex = enif_mutex_create("wait_arq__mutex");
  arq_cnd = enif_cond_create("arq work enqueued");
  enif_mutex_lock(arq_mutex);
  STAILQ_INIT(&arq_head);
  arq_alive = 1;
  enif_mutex_unlock(arq_mutex);
  if (!enif_thread_create("arq_worker", &arq_tid, &arq_worker_fn, NULL, NULL))
    return errno;
  return 0;
}

static void arq_shutdown(void)
{
  void *exit_value = 0;

  /* stop, join the worker thread */
  arq_alive = 0;
  enif_cond_signal(arq_cnd);                                            \
  enif_thread_join(arq_tid, &exit_value);

  /* deallocate anything left in the queue */
  enif_mutex_lock(arq_mutex);
  while (!STAILQ_EMPTY(&arq_head)) {
    struct arq_entry *e = STAILQ_LAST(&arq_head, arq_entry, entries);
    STAILQ_REMOVE(&arq_head, STAILQ_LAST(&arq_head, arq_entry, entries), arq_entry, entries);
    free(e);
  }
  enif_mutex_unlock(arq_mutex);
  enif_thread_join(arq_tid, &exit_value);
}

#endif
