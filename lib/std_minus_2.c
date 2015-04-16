T std_minus_2(T x,T y){
  if ((TAG(x) != TAG_INT) || (TAG(y) != TAG_INT))
    BADMATCH;
  return I(VALUE(x)-VALUE(y));
}
