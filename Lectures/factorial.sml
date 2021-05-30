print("/----------------\n\n");
fun factorial n =
    if n <= 1 then
      1
    else
      factorial (n-1) * n;

fun aux n =
  if n > 16 then
    ()
  else (
    print (Int.toString n ^ "! = " ^ Int.toString (factorial n) ^ "\n");
    aux (n + 1)
  );
aux 0;
print("----------------/\n");
OS.Process.exit(OS.Process.success);
