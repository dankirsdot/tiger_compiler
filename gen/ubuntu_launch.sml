val arg = CommandLine.arguments();
val st = List.nth(arg, 0);
CM.make("sources.cm");
val AST = Parse.parse(st);
val sout = TextIO.stdOut;
val os = TextIO.openOut "c_code.c";
(* PrintAbsyn.print(sout, AST); *)
Gen.print(os, AST);
val _ = OS.Process.exit(OS.Process.success)