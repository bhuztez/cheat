T std_cons(T x, T y) {
  struct list *list = (struct list *)term_alloc(sizeof(struct list));
  term_set_tag(list, TAG_LIST);
  list->hd = x;
  list->tl = y;
  return (T)list;
}
