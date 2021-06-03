val arg = CommandLine.arguments();
val st = List.nth(arg, 0);
CM.make("sources.cm");
		(* print("\n\n\n"^st^"\n\n\n"); *)
val AST = Parse.parse(st);
(* val sout = TextIO.stdOut; *)
val os = TextIO.openOut "c_code.c";
Gen.print(os, AST);
val _ = OS.Process.exit(OS.Process.success)
(* fun printToOutStream outstream str = let val os = outstream
                                     in
                                       TextIO.output(os,str);
                                       TextIO.closeOut os
                                     end;
val os = TextIO.openOut "c_code.c";
printToOutStream os AST; *)