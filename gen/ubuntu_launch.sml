val arg = CommandLine.arguments();
val st = List.nth(arg, 0);
CM.make("sources.cm");
val AST = Parse.parse(st);
val os = TextIO.openOut "c_code.c";
val sout = TextIO.stdOut;
PrintAbsyn.print(sout, AST);
Gen.print(os, AST);
val _ = OS.Process.exit(OS.Process.success)