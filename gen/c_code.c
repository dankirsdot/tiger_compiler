#include <stdio.h>
int main()
{
NANI N =  8;
 TypeDec[  (intArray,
   ArrayTy(int))]
NANI row =   ArrayExp(intArray[
 N] = 
 N;;
NANI col =   ArrayExp(intArray[
 N] = 
 N;;
NANI diag1 =   ArrayExp(intArray[
 N + N - 1] = 
 N + N - 1;;
NANI diag2 =   ArrayExp(intArray[
 N + N - 1] = 
 N + N - 1;;
  void printboard(){
      for( int i =  0; i <=  N - 1; i++){
          for( int j =  0; j <=  N - 1; j++){
             print(if( col[ i] = j){
" O"
        }else{
" ."}
);}
      
      print("
");
}
    
    print("
");
  }

  void try(int c){
  if( c = N){
     printboard();
    }else{
     for( int r =  0; r <=  N - 1; r++){
     if( row[ r] = 0 || diag1[ r + c] = 0 || diag2[ r + 7 - c] = 0){
        row[ r] =  1;
        diag1[ r + c] =  1;
        diag2[ r + 7 - c] =  1;
        col[ c] =  r;
        try( c + 1);
        row[ r] =  0;
        diag1[ r + c] =  0;
        diag2[ r + 7 - c] =  0;

      }
      }
     }

  }

  try( 0);
  return 0;
}