T io_print_string_1(T s) {
  if (!IS_POINTER(s))
    BADMATCH;
  if (PTAG(s) != TAG_SUBBIN)
    BADMATCH;
  struct subbin *sb = PSUBBIN(s);
  printf("%.*s\n", (int)(sb->length), ((char *)(sb->bin->data)+(sb->offset)));
  return I(0);
}
