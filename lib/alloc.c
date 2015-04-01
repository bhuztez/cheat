#include <stdarg.h>
#include <stddef.h>
#include <stdint.h>
#include <stdio.h>
#include <stdlib.h>
#include <string.h>

#include <assert.h>
#include <sys/mman.h>

typedef uintptr_t T;

#define ROUND_UP(a,b)  (((T)(a)+(T)(b)-1) & (~((T)(b)-1)))
#define ROUND_DOWN(a,b) ((T)(a) & (~((T)(b)-1)))

struct block_header {
  unsigned int prev_size : 24;
  unsigned int size      : 24;
  unsigned int free      : 1;
  unsigned int marked    : 1;
  unsigned int tag       : 14;
} __attribute__((packed));

struct free_block {
  struct block_header header;
  struct free_block *prev_block;
  struct free_block *next_block;
};

#define PAGE_SIZE 4096
#define MMAP_BASE 0x10000000

void alloc_page(void *addr, size_t length) {
  assert(MAP_FAILED !=
         mmap(addr,
              length,
              PROT_READ|PROT_WRITE,
              MAP_ANONYMOUS|MAP_PRIVATE|MAP_FIXED,
              -1, 0));
}

/* fli number of words
 *   2 16 +0 +2 +4 +6 +8 +10 +12 +14
 *   1  8 +0 +1 +2 +3 +4  +5  +6  +7
 *   0  0 +0 +1 +2 +3 +4  +5  +6  +7
 */

struct memory_pool {
  unsigned int pages     : 32;
  unsigned int fl_bitmap : 32;
  unsigned char sl_bitmap[24];
  struct free_block *free_block_list[24][8];
  struct block_header *last_block;
  size_t free_size;
  size_t free_blocks;
} __attribute__((packed));


struct memory_pool default_pool = {0};

/* flsl1(0) -> 0
 * flsl1(1) -> 1
 * flsl1(2) -> 2
 * flsl1(3) -> 1
 * flsl1(4) -> 3
 */
int flsl1(unsigned long x) {
  if (x) {
    return 8 * sizeof(unsigned long) - __builtin_clzl(x);
  } else {
    return 0;
  }
}

static void mapping_insert(size_t nwords, int *fl, int *sl) {
  int fli = flsl1(nwords >> 3);
  int sli = nwords;
  if (fli) {
    sli >>= fli - 1;
  }

  *fl = fli;
  *sl = sli & 0x07;
}

static size_t mapping_roundup(size_t nwords) {
  int fli = flsl1(nwords >> 3);
  if (!fli)
    return nwords;
  return ROUND_UP(nwords, 1 << (fli-1));
}

static void mapping_search(size_t nwords, int *fl, int *sl) {
  nwords = mapping_roundup(nwords);
  int fli = flsl1(nwords >> 3);
  int sli = nwords;
  if (fli) {
    sli >>= fli - 1;
  }

  *fl = fli;
  *sl = sli & 0x07;
}

struct free_block *find_suitable_block(size_t nwords) {
  int fli, sli;
  mapping_search(nwords, &fli, &sli);

  if (default_pool.fl_bitmap & (1 << fli)) {
    int slb = flsl1(default_pool.sl_bitmap[fli] >> sli);
    if (slb) {
      return default_pool.free_block_list[fli][sli+slb-1];
    }
  }

  int flb = flsl1(default_pool.fl_bitmap >> (fli + 1));
  if (!flb)
    return NULL;

  fli += flb;
  int slb = flsl1(default_pool.sl_bitmap[fli]);
  return default_pool.free_block_list[fli][slb-1];
}

void insert_block(struct free_block *block) {
  block->header.free = 1;
  block->prev_block = NULL;
  default_pool.free_size += block->header.size;
  default_pool.free_blocks += 1;

  int fli, sli;
  mapping_insert(block->header.size, &fli, &sli);
  block->next_block = default_pool.free_block_list[fli][sli];

  if (block->next_block) {
    block->next_block->prev_block = block;
  }
  default_pool.free_block_list[fli][sli] = block;
  default_pool.sl_bitmap[fli] |= 1 << sli;
  default_pool.fl_bitmap |= 1 << fli;
}

void remove_block(struct free_block *block) {
  block->header.free = 0;
  default_pool.free_size -= block->header.size;
  default_pool.free_blocks -= 1;

  if (block->next_block) {
    block->next_block->prev_block = block->prev_block;
  }

  if (block->prev_block) {
    block->prev_block->next_block = block->next_block;
  } else {
    int fli, sli;
    mapping_insert(block->header.size, &fli, &sli);
    default_pool.free_block_list[fli][sli] = block->next_block;

    if (!block->next_block) {
      default_pool.sl_bitmap[fli] &= ~(1 << sli);
      if (!default_pool.sl_bitmap[fli])
        default_pool.fl_bitmap &= ~(1 << fli);
    }
  }
}

struct free_block *split_block(struct free_block *block, size_t nwords) {
  size_t orig_size = block->header.size;
  block->header.size = nwords;
  struct free_block *remain_block = (struct free_block *)((T *)(&(block->header)+1)+nwords);
  remain_block->header.prev_size = nwords;
  remain_block->header.size = orig_size - nwords - sizeof(struct block_header)/sizeof(T);

  if (&(block->header) == default_pool.last_block)
    default_pool.last_block = &(remain_block->header);

  return remain_block;
}

struct block_header *get_blocks_begin() {
  return (struct block_header *)MMAP_BASE;
}

struct block_header *get_blocks_end() {
  uintptr_t header = MMAP_BASE + PAGE_SIZE * default_pool.pages;
  return (struct block_header *)header;
}

struct block_header *find_prev_block(struct block_header *header) {
  return (struct block_header *)((T *)header - header->prev_size) - 1;

}

struct block_header *find_next_block(struct block_header *header) {
  return (struct block_header *)((T *)(header+1) + header->size);
}

struct block_header *merge_left_block(struct block_header *block) {
  struct block_header *left_block = find_prev_block(block);
  if ((left_block < get_blocks_begin()) || (!(left_block->free)))
    return block;

  remove_block((struct free_block *)left_block);
  left_block->size += block->size + sizeof(struct block_header)/sizeof(T);

  if (block == default_pool.last_block)
    default_pool.last_block = left_block;

  return left_block;
}

struct block_header *merge_right_block(struct block_header *block) {
  struct block_header *right_block = find_next_block(block);
  if ((right_block >= get_blocks_end()) || (!(right_block->free)))
    return block;

  remove_block((struct free_block *)right_block);
  block->size += right_block->size + sizeof(struct block_header)/sizeof(T);

  if (right_block == default_pool.last_block)
    default_pool.last_block = block;

  return block;
}

struct block_header *merge_block(struct block_header *block) {
  block = merge_left_block(block);
  return merge_right_block(block);
}


void gc_mark();

void term_set_mark(void *ptr) {
  ((struct block_header *)ptr - 1)->marked = 1;
}

int term_get_mark(void *ptr) {
  return ((struct block_header *)ptr - 1)->marked;
}

void gc_sweep() {
  struct block_header * header = get_blocks_begin();
  for(; header < get_blocks_end(); header = find_next_block(header)) {
    if (header->free)
      continue;

    if (header->marked) {
      header->marked = 0;
    } else {
      header = merge_block(header);
      insert_block((struct free_block *)header);
    }
  }
}

void *term_alloc(size_t size) {
  assert(size > 0);
  size_t nwords = ROUND_UP(size, sizeof(T))/sizeof(T);
  struct free_block *free_block = find_suitable_block(nwords);

  if (!free_block) {
    gc_mark();
    gc_sweep();
    free_block = find_suitable_block(nwords);
  }

  if (!free_block) {
    uintptr_t alloc_base = MMAP_BASE+PAGE_SIZE*default_pool.pages;
    size_t length;

    if (default_pool.last_block && (default_pool.last_block->free)) {
      length = ROUND_UP(sizeof(T)*(mapping_roundup(nwords) - default_pool.last_block->size), PAGE_SIZE);
    } else {
      length = ROUND_UP(sizeof(struct block_header) + sizeof(T)*mapping_roundup(nwords), PAGE_SIZE);
    }

    default_pool.pages += length/PAGE_SIZE;
    alloc_page((void *)alloc_base, length);
    struct block_header *header = (struct block_header *)alloc_base;

    if (default_pool.last_block)
      header->prev_size = default_pool.last_block->size;

    default_pool.last_block = header;
    header->size = (length - sizeof(struct block_header))/sizeof(T);
    free_block = (struct free_block *)merge_left_block(header);
  } else {
    remove_block(free_block);
  }

  assert(free_block != NULL);

  if (sizeof(T) * (free_block->header.size - nwords) > sizeof(struct free_block)) {
    struct free_block *remain_block = split_block(free_block, nwords);
    insert_block(remain_block);
  }

  free_block->header.marked = 0;
  free_block->header.tag = 0;
  return (void *)(&(free_block->header)+1);
}

int term_get_tag(void *ptr) {
  return ((struct block_header *)ptr - 1)->tag;
}

void term_set_tag(void *ptr, unsigned int tag) {
  ((struct block_header *)ptr - 1)->tag = tag;
}
