#include <string.h>
#include <stdio.h>

 VarDec(N,true,NONE,
 8)
 TypeDec[  (intArray,
   ArrayTy(int))]
 VarDec(row,true,NONE,
  ArrayExp(intArray,
 N,
 N))
 VarDec(col,true,NONE,
  ArrayExp(intArray,
 N,
 N))
 VarDec(diag1,true,NONE,
  ArrayExp(intArray,
 N+ N- 1,
 N+ N- 1))
 VarDec(diag2,true,NONE,
  ArrayExp(intArray,
 N+ N- 1,
 N+ N- 1))
 void printboard(){
   int main(){    ForExp(
i,true,
 0,
 N- 1,
     int main(){      ForExp(
j,true,
 0,
 N- 1,
       print(        IfExp(
            SubscriptVar(
col,
 i)= j,
" O",
" ."));)
      print("
");})
    print("
");}}

 void try(   int c){
   int main(){    IfExp(
 c= N,
     printboard();,
     ForExp(
r,true,
 0,
 N- 1,
      IfExp(
       IfExp(
        IfExp(
            SubscriptVar(
row,
 r)= 0,
            SubscriptVar(
diag1,
 r+ c)= 0,
 0),
           SubscriptVar(
diag2,
 r+ 7- c)= 0,
 0),
       int main(){        AssignExp(
         SubscriptVar(
row,
 r),
 1)
        AssignExp(
         SubscriptVar(
diag1,
 r+ c),
 1)
        AssignExp(
         SubscriptVar(
diag2,
 r+ 7- c),
 1)
        AssignExp(
         SubscriptVar(
col,
 c),
 r)
        try( c+ 1);
        AssignExp(
         SubscriptVar(
row,
 r),
 0)
        AssignExp(
         SubscriptVar(
diag1,
 r+ c),
 0)
        AssignExp(
         SubscriptVar(
diag2,
 r+ 7- c),
 0)})))}}

 int main(){  try( 0);}
