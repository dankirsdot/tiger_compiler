fun delete (item, list) = 
	List.filter(fn x => x <> item) list

fun is_space ch = 
	if ord(ch)=ord(#" ") then
		true
	else 
		false

fun show list : string list = 
	if (List.length(list)) = 0 then 
		List.take([""], 0)
	else
		(let 
		val str = List.nth(list, 0);
		val chrs = explode(str);
		val pure_chrs = delete( #"\n", (delete(#"\t", chrs)));
		in
		print(str^"\n");
		
		print(String.implode(pure_chrs)^"\n");

		show(List.rev(List.take((List.rev(list)), (List.length(list) - 1))))
		end)
(*
	Returns a list of strings from one single string

fun str_lexem_split str = 

fun lexem_split list start =
	if List.length(list) = 0 then
		[]
	else
		str_lexem_split(list[start]) @ lexem_split(List.take(list, 0))
*)


val infile = "./input.tig" ;

fun readlist (infile : string) = let 

  val ins = TextIO.openIn infile 

  fun loop ins = 

   case TextIO.inputLine ins of 

      SOME line => line :: loop ins 

    | NONE      => [] 

in 

  loop ins before TextIO.closeIn ins 

end ;

(*
	Read list of strings from file
*)

val pureGraph =  readlist(infile);
(* pop first element of the string list *)
(* val k = List.rev(List.take((List.rev(pureGraph)), (List.length(pureGraph) - 1)));
print( (List.nth(k, 0) ) ^ "\n" ); *)
show pureGraph;
(* val purepure = delete_nltab(pureGraph); *)
(*take first string element as a string
val k = List.nth(List.take(pureGraph, 1), 0);
print(List.nth(pureGraph, 0));
val o = List.drop(pureGraph, 2);
*)



(*
	splitting on " "
*)
(*
for (0 to p)
    (fn i => 
    	let
		val str = List.nth(pureGraph, i)
		val ln = size(str) - 1
    		val tmp = [] : char list;
    	in 
    		print("\n"^"Index:"^(Int.toString i)^"\n"^str^"\n"^"Length: "^(Int.toString ln));
    		for (0 to ln-1)
    			(fn j => 
    				let
    					val c = String.sub(str, j)
					val m = is_space(c)
					val tmp = c :: tmp
				in
					print((String.str c)^"\n"^(String.implode(tmp))^"\n")
				end)
	end);
*)