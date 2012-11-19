#ifndef __ASYNC_NIF_H__
#define __ASYNC_NIF_H__

/* BEGIN: include "queue.h" BSD 3-Clause License */

#ifndef __offsetof
#define __offsetof(st, m) \
     ((size_t) ( (char *)&((st *)0)->m - (char *)0 ))
#endif

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

struct arq_entry {
  ErlNifEnv* env;
  ErlNifPid pid;
  int argc;
  void *args;
  void (*fn)(ErlNifEnv*, ErlNifPid*, int, void *);
  void (*release)(ErlNifEnv*, void *);
  STAILQ_ENTRY(arq_entry) entries;
};
STAILQ_HEAD(arq, arq_entry) arq_head = STAILQ_HEAD_INITIALIZER(arq_head);
static volatile unsigned int arq_depth;
static ErlNifEnv *arq_nif_env;
static ErlNifMutex *arq_mutex;
static ErlNifCond *arq_cnd;
static ErlNifTid arq_tid;

#define ASYNC_NIF_DECL(name, block, release)                            \
  struct name ## _args block;                                           \
  static void r_ ## name(ErlNifEnv *env, struct name ## _args *args) release \
  static void q_ ## name(ErlNifEnv*, ErlNifPid*, int, struct name ## _args *); \
  static ERL_NIF_TERM name(ErlNifEnv* env, int argc, const ERL_NIF_TERM argv[]) { \
  struct name ## _args *args = enif_alloc(sizeof(struct name ## _args)); \
    if (!args) return enif_make_tuple2(env,                             \
                 enif_make_atom(env, "error"),                          \
                 enif_make_atom(env, "enomem"));                        \
    do

#define ASYNC_NIF_RETURN(name, result, block)                           \
  } while(0);                                                           \
  struct arq_entry *r;                                                  \
  r = enif_alloc(sizeof(struct arq_entry));                             \
  r->env = env;                                                         \
  if(!enif_get_local_pid(env, argv[argc - 1], &(r->pid))) {             \
    r_ ## name(env, args);                                              \
    enif_free(r);                                                       \
    return enif_make_tuple2(env, enif_make_atom(env, "error"), enif_make_atom(env, "pid")); \
  }                                                                     \
  r->args = (void *)args;                                               \
  r->fn = (void (*)(ErlNifEnv *, ErlNifPid*, int, void *))q_ ## name;   \
  r->release = (void (*)(ErlNifEnv *, void *))r_ ## name;               \
  enif_mutex_lock(arq_mutex);                                           \
  STAILQ_INSERT_TAIL(&arq_head, r, entries);                            \
  arq_depth++;                                                          \
  enif_cond_signal(arq_cnd);                                            \
  enif_mutex_unlock(arq_mutex);                                         \
  return enif_make_tuple2(env, result, enif_make_int(env, arq_depth));  \
  }                                                                     \
  static void q_ ## name(ErlNifEnv *env, ErlNifPid *pid, int argc, struct name ## _args *args) { \
    do block while(0);

#define ASYNC_NIF_INIT() if (!arq_init()) return -1;
#define ASYNC_NIF_SHUTDOWN() arq_shutdown();

#define ASYNC_NIF_REPLY(msg) enif_send(NULL, pid, env, msg); enif_free(args);

static int arq_alive = 0;
static void *arq_worker_fn(void *args)
{
  struct arq_entry *e;
  do {
    enif_mutex_lock(arq_mutex); work:
    e = NULL;
    if (arq_alive && ((e = STAILQ_FIRST(&arq_head)) != NULL)) {
      STAILQ_REMOVE_HEAD(&arq_head, entries);
      arq_depth--;
      enif_mutex_unlock(arq_mutex);
      e->fn(e->env, &(e->pid), e->argc, e->args);
      e->release(e->env, e->args);
      enif_free(e);
    } else {
      enif_cond_wait(arq_cnd, arq_mutex);
      goto work;
    }
  } while(1);
  return 0;
}

static int arq_init()
{
  arq_nif_env = enif_alloc_env();
  arq_mutex = enif_mutex_create("wait_arq_mutex");
  arq_cnd = enif_cond_create("arq work enqueued");
  enif_mutex_lock(arq_mutex);
  STAILQ_INIT(&arq_head);
  arq_depth = 0;
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
  struct arq_entry *e = NULL;
  STAILQ_FOREACH(e, &arq_head, entries) {
    STAILQ_REMOVE(&arq_head, STAILQ_LAST(&arq_head, arq_entry, entries), arq_entry, entries);
    e->release(e->env, e->args);
    arq_depth--;
    enif_free(e);
  }
  enif_mutex_unlock(arq_mutex);
  enif_thread_join(arq_tid, &exit_value);
}

#endif // __ASYNC_NIF_H__
