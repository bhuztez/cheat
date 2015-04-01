T io_print_1(T i){
  if (TAG(i) != TAG_INT)
    BADMATCH;
  
  printf("%lu\n", VALUE(i));
  return I(0);
}
