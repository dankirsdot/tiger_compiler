  
(* Pretty-prints an abstract syntax tree. *)
fun is_letter(ch) = 
	(let
		val ch_num = ord(ch);
	in
		if ((ch_num > 64 andalso ch_num < 91) orelse (ch_num > 96 andalso ch_num < 123)) then
			true
		else 
			false
	end)

fun is_digit(ch) = 
	(let
		val ch_num = ord(ch);
	in
		if (ch_num > 47 andalso ch_num < 58) then
			true
		else 
			false
	end)


structure Gen : 
     sig val print : TextIO.outstream * Absyn.exp -> unit 
         val opname : Absyn.oper -> string end =
struct

  structure A = Absyn

  fun opname A.PlusOp = "+"
    | opname A.MinusOp = "-"
    | opname A.TimesOp = "*"
    | opname A.DivideOp = "/"
    | opname A.EqOp = "="
    | opname A.NeqOp = "<>"
    | opname A.LtOp = "<"
    | opname A.LeOp = "<="
    | opname A.GtOp = ">"
    | opname A.GeOp = ">="
  fun print (outstream, e0) =
      let 
          fun say s = TextIO.output(outstream,s)
          fun sayln s = 
              (
                  say s; 
                  say "\n"
              )

          fun get_type smth = 
              (
                  if is_digit(String.sub(smth,0)) then 
                    "INTEGER" 
                  else 
                    "IDONTKNOW"
              )
          fun indent 0 = ()
            | indent i = 
              (
                  say " "; 
                  indent(i-1)
              )

          fun dobodylist d f [a] = 
              (
                  say ""; 
                  f(a,d+1)
              )
            | dobodylist d f (a::r) = 
              (
                  say ""; 
                  f(a,d+1);
                  say "\n";
                  dobodylist d f r
              )
            | dobodylist d f nil = ()

          fun doparamlist d f [a] = 
              (
                  say ""; 
                  f(a,d+1)
              )
            | doparamlist d f (a::r) = 
              (
                  say ""; 
                  f(a,d+1); 
                  say ","; 
                  doparamlist d f r
              )
            | doparamlist d f nil = ()


          fun var(A.SimpleVar(s,p),d) = 
              (
                  say(Symbol.name s)
              )
            | var(A.FieldVar(v,s,p),d) = 
              (
                  indent d; 
                  sayln "FieldVar(";
                  var(v,d+1); 
                  sayln ",";
                  indent(d+1); 
                  say(Symbol.name s); 
                  say ")"
              )
            | var(A.SubscriptVar(v,e,p),d) = 
              (
                  indent d; 
                  (* sayln "SubscriptVar("; *)
                  var(v,d+1); 
                  say "[";
                  exp(e,d+1);
                  say "]"
              )            

          and exp(A.VarExp v, d) = 
              (
                  indent 1; 
                  var(v,d+1)
              )
            | exp(A.NilExp, d) = 
              (
                  indent 1; 
                  say "NULL"
              )
            | exp(A.IntExp i, d) = 
              (
                  indent 1; 
                  say(Int.toString i)
              )

            | exp(A.StringExp(s,p),d) = 
              (
                  say "\"";
                  (* correct syntax is needed.
                  list is empty for a string constant. *)
                  say s; 
                  say "\""
              )

            | exp(A.CallExp{func,args,pos},d) =
              (
                  indent d; 
                  say(Symbol.name func);
                  say "(";
                  doparamlist d exp args; 
                  say ");"
              )

            | exp(A.OpExp{left,oper,right,pos},d) =
              ( 
                  exp(left,d+1); 
                  say(opname oper); 
                  exp(right,d+1)
              )

            | exp(A.RecordExp{fields,typ,pos},d) =
              let 
                  fun f((name,e,pos),d) = 
                      (
                          indent d; 
                          say "("; 
                          say(Symbol.name name);
                          sayln ","; exp(e,d+1);
                          say ")"
                      )
              in  
                  indent d; 
                  say "RecordExp("; 
                  say(Symbol.name typ); 
                  sayln ",["; 
                  doparamlist d f fields; 
                  say "])" 
              end
            | exp(A.SeqExp l, d) = 
              (
                  dobodylist d exp (map #1 l);
                  sayln ""
              )
            | exp(A.AssignExp{var=v,exp=e,pos},d) = 
              (
                  indent d; 
                  (* sayln "AssignExp(";  *)
                  var(v,d+1); 
                  say " = ";
                  exp(e,d+1);
                  say ";"
              )

            | exp(A.IfExp{test,then',else',pos},d) =
              (
                  say "if("; 
                  (* here correct syntax needed. 
                  test is got as then branch.
                  also first then expression is lost*)
                  exp(test,d+1); 
                  sayln "){";
                  exp(then',d+1); 
                  sayln "";
                  indent d;
                  say "}";
                  case else' of NONE => ()
                  | SOME e => 
                    (
                        sayln "else{"; 
                        exp(e,d+1); 
                        sayln "}"
                    )
              )

            | exp(A.WhileExp{test,body,pos},d) =
              (
                  indent d; 
                  say "while("; 
                  exp(test,d+1); 
                  sayln "){";
                  exp(body,d+1); 
                  say "}"
              )
            | exp(A.ForExp{var=v,escape=b,lo,hi,body,pos},d) =
              (
                  indent d; 
                  say "for( int ";
                  say(Symbol.name v); 
                  say " = ";
                  exp(lo,d+1); 
                  say "; "; 
                  say(Symbol.name v); 
                  say " <= "; 
                  exp(hi,d+1); 
                  say "; ";
                  say(Symbol.name v); 
                  sayln "++){";
                  indent d; 
                  exp(body,d+1);
                  say "}"
              )
            | exp(A.BreakExp p, d) = 
              (
                  say "break;"
              )
            | exp(A.LetExp{decs,body,pos},d) =
              (
                  dobodylist d dec decs; 
                  sayln ""; 
                  exp(body, d+1)
              )
            | exp(A.ArrayExp{typ,size,init,pos},d) =
              (
                  say "ArrayExp("; 
                  say(Symbol.name typ); 
                  sayln "[";
                  exp(size,d+1); 
                  sayln "] = "; 
                  (* correct syntax needed.
                  size and assign value is
                  the same lexem *)
                  exp(init,d+1); 
                  say ";"
              )

          and dec(A.FunctionDec l, d) = 
              let 
                  fun field({name,escape,typ,pos},d) = 
                      (
                          if ((Symbol.name typ) = "string") then 
                              (say "char*")
                          else 
                              (say(Symbol.name typ));
                          say " ";
                          say(Symbol.name name) 
                      )
                  fun f({name,params,result,body,pos},d) =
                      ( 
                          indent d; 
                          case result of NONE => 
                              (
                                  say "void"; 
                                  say " "
                              )
                          | SOME(s,_) => 
                              ( 
                                  say(Symbol.name s)
                              );
                          say (Symbol.name name); say "(";
                          doparamlist d field params; sayln "){";
                          indent d; 
                          exp(body,d+1);
                          indent d; 
                          sayln "}"
                      )
              in 
                  doparamlist d f l
              end

            | dec(A.VarDec{name,escape,typ,init,pos},d) =
              (
                  case typ of NONE => ( say "NANI") 
                  | SOME(s,p)=> 
                    (
                        say(Symbol.name s)
                    ); 

                  say " ";
                  say(Symbol.name name); 
                  say " = ";
                  exp(init,d+1); 
                  say ";"
                )
            | dec(A.TypeDec l, d) = 
                let 
                    fun tdec({name,ty=t,pos},d) = 
                        (
                            indent d; 
                            say"("; 
                            say(Symbol.name name); 
                            sayln ",";
                            ty(t,d+1); 
                            say ")"
                        )
                in  
                    indent d; 
                    say "TypeDec["; 
                    doparamlist d tdec l; 
                    say "]"
                end
        
          and ty(A.NameTy(s,p), d) = 
              (
                  indent d; 
                  say "NameTy("; 
                  say(Symbol.name s);
                  say ")"
              )
            | ty(A.RecordTy l, d) =  
              let 
                  fun f({name,escape,typ,pos},d) =
                      (
                          indent d; 
                          say "("; 
                          say (Symbol.name name);
                          say ","; 
                          say (Bool.toString (!escape)); 
                          say ",";
                          say (Symbol.name typ); 
                          say ")"
                      )
              in  
                  indent d; 
                  say "RecordTy[";
                  dobodylist d f l; 
                  say "]"
              end
            | ty(A.ArrayTy(s,p),d) = 
              (
                  indent d; 
                  say "ArrayTy("; 
                  say(Symbol.name s);
                  say ")"
              )
      in  
          sayln "#include <stdio.h>\nint main()\n{";
          exp(e0,0); 
          say "  return 0;\n}"; 
          TextIO.flushOut outstream
      end
end
