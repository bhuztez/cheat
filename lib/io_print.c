T io_print(T i){
  if (TAG(i) != TAG_INT)
    BADMATCH;
  
  printf("%lu\n", VALUE(i));
  return I(0);
}
