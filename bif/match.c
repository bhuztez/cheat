T match(T x,T y){
if(x==y)RT True;
if(TAG(x)||TAG(y))RT False;
if(MT(x)!=MT(y))RT False;
switch(MT(x)){
case TL:
if(match(PL(x)->hd,PL(y)->hd)==False)RT False;
RT match(PL(x)->tl,PL(y)->tl);
case TT:
if(PT(x)->n!=PT(y)->n)RT False;
T i;
for(i=0;i<(PT(x)->n);i++)
  if(match(PT(x)->e[i],PT(y)->e[i])==False)
    RT False;
RT True;
default: RT False;}
}
