val infile = "./input.txt" ;

fun readlist (infile : string) = let 

  val ins = TextIO.openIn infile 

  fun loop ins = 

   case TextIO.inputLine ins of 

      SOME line => line :: loop ins 

    | NONE      => [] 

in 

  loop ins before TextIO.closeIn ins 

end ;

val pureGraph =  readlist(infile);


/*for (0 downto 20)
    (fn i => print (Int.toString pureGraph[i]))
end;*/
