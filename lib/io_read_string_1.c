T io_read_string_1(T n) {
  if (TAG(n) != TAG_INT)
    BADMATCH;
  struct subbin *sb = binary_alloc(VALUE(n)+1);
  int r = scanf("%s", sb->bin->data);
  if (r == EOF) BADMATCH;
  T length = strlen(sb->bin->data);
  sb->bin->n = length;
  sb->length = length;
  return (T)sb;
}
