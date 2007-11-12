#include <string.h>
#include <stdlib.h>
#include <stdio.h>

#include <assert.h>

#include <sys/time.h>

typedef struct Cons {
  struct Cons *next;
  struct Cons *car;
  struct Cons *cdr;
} *CONS;

#define TAGBITS(x)	(((int) (x)) & 3)
#define DETAG(x)	(((int) (x)) >> 2)
#define ENTAG(x,t)	((CONS) (((x) << 2) | ((t) & 3)))

#define CONS_TAG	0
#define INT_TAG		1

#define CONSP(x)	(TAGBITS(x) == CONS_TAG)
#define INTP(x)		(TAGBITS(x) == INT_TAG)

#define ALLOC_TABLE_SIZE 1000003

static CONS alloc_table[ALLOC_TABLE_SIZE];

static struct Cons *heap;
static CONS heap_next;
static int heap_size;

typedef struct Root {
  CONS *root_pointer;
  struct Root *next;
} *ROOT;

static ROOT roots = NULL;

static void add_root(CONS *pointer) {
  ROOT r = malloc(sizeof(struct Root));
  r->root_pointer = pointer;
  r->next = roots;
  roots = r;
}

#define MARKIFY(x)	(ENTAG(DETAG(x), INT_TAG))
#define DEMARKIFY(x)	(ENTAG(DETAG(x), CONS_TAG))
#define SET_MARK(c)	((c)->next = MARKIFY((c)->next))
#define CLEAR_MARK(c)	((c)->next = DEMARKIFY((c)->next))
#define IS_MARKED(c)	(INTP((c)->next))

static void sweep_alloc_table(void) {
  int i;
  for (i = 0; i < ALLOC_TABLE_SIZE; i++) {
    CONS bucket = alloc_table[i];
    CONS prev = NULL;
    while (bucket != NULL) {
      if (IS_MARKED(bucket)) {
	if (prev == NULL) {
	  alloc_table[i] = bucket;
	} else {
	  prev->next = MARKIFY(bucket);
	}
	prev = bucket;
      }
      bucket = DEMARKIFY(bucket->next);
    }
    if (prev == NULL) {
      alloc_table[i] = NULL;
    }
  }
}

static void sweep_heap(void) {
  int i;
  heap_next = NULL;
  for (i = 0; i < heap_size; i++) {
    CONS c = &heap[i];
    if (IS_MARKED(c)) {
      CLEAR_MARK(c);
    } else {
      c->next = heap_next;
      heap_next = c;
    }
  }
}

static void mark(CONS c) {
 tail_loop:
  if (CONSP(c) && !IS_MARKED(c)) {
    SET_MARK(c);
    mark(c->car);
    c = c->cdr;
    goto tail_loop;
  }
}

static void init_gc(void) {
  int i;

  for (i = 0; i < ALLOC_TABLE_SIZE; i++) {
    alloc_table[i] = NULL;
  }

  heap_size = 2000000;
  heap = calloc(heap_size, sizeof(struct Cons));
  sweep_heap();
}

static void gc(void) {
  ROOT r;

  for (r = roots; r != NULL; r = r->next) {
    mark(*(r->root_pointer));
  }

  sweep_alloc_table();
  sweep_heap();
}

static CONS gc_alloc_pair(void) {
  gc();
  if (heap_next == NULL) {
    fprintf(stderr, "out of memory\n");
    exit(3);
  }
  {
    CONS result = heap_next;
    heap_next = heap_next->next;
    return result;
  }
}

static inline CONS alloc_pair(void) {
  if (heap_next != NULL) {
    CONS result = heap_next;
    heap_next = heap_next->next;
    return result;
  } else {
    return gc_alloc_pair();
  }
}

static CONS shared_cons(CONS car, CONS cdr) {
  int hash = (DETAG(car) + DETAG(cdr)) % ALLOC_TABLE_SIZE;
  CONS probe = alloc_table[hash];
  while (probe != NULL) {
    if (probe->car == car && probe->cdr == cdr) {
      return probe;
    }
    probe = probe->next;
  }

  {
    CONS result = alloc_pair();
    result->next = alloc_table[hash];
    alloc_table[hash] = result;
    result->car = car;
    result->cdr = cdr;
    return result;
  }
}

static CONS unshared_cons(CONS car, CONS cdr) {
  CONS result = alloc_pair();
  result->next = NULL;
  result->car = car;
  result->cdr = cdr;
  return result;
}

#define NUM_ITERATIONS	10000000

int main(int argc, char *argv[]) {
  char *mode;
  struct timeval start;
  struct timeval stop;

  if (argc < 2) {
    fprintf(stderr,
	    "Usage: hashcons <mode>\n"
	    "  where <mode> in unshared, shared, uniform\n");
    exit(1);
  }

  init_gc();

  mode = argv[1];
  if (!strcmp(mode, "unshared")) {
    int i;
    CONS chain = NULL;
    gettimeofday(&start, NULL);
    for (i = 0; i < NUM_ITERATIONS; i++) {
      chain = unshared_cons(ENTAG(i, INT_TAG), NULL);
    }
    gettimeofday(&stop, NULL);
  } else if (!strcmp(mode, "shared")) {
    int i;
    CONS chain = NULL;
    gettimeofday(&start, NULL);
    for (i = 0; i < NUM_ITERATIONS; i++) {
      chain = shared_cons(ENTAG(i, INT_TAG), NULL);
    }
    gettimeofday(&stop, NULL);
  } else if (!strcmp(mode, "uniform")) {
    int i;
    CONS chain = NULL;
    gettimeofday(&start, NULL);
    for (i = 0; i < NUM_ITERATIONS; i++) {
      chain = shared_cons(ENTAG(12345, INT_TAG), NULL);
    }
    gettimeofday(&stop, NULL);
  }

  double delta = (stop.tv_sec - start.tv_sec) + (stop.tv_usec - start.tv_usec) / 1000000.0;
  printf("Time delta:  %6g seconds\n", delta);
  printf("Iterations:  %d\n", NUM_ITERATIONS);
  printf("Rate:        %6g Hz\n", NUM_ITERATIONS / delta);
  printf("Period:      %6g microseconds\n", (delta * 1000000.0) / NUM_ITERATIONS);
}
