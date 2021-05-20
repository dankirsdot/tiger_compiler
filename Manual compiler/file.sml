fun delete (item, list) = 
	List.filter(fn x => x <> item) list

fun is_space ch = 
	if ord(ch)=ord(#" ") then
		true
	else 
		false
(*
	Returns 0 if item is found 
	or end of the list is reached
*)
fun get_item_index (mylist, item) = 
	if ((0 = List.length(mylist)) orelse ((List.nth(mylist, 0)) = item)) then
		(let
			val l = List.length(mylist);
		in
		print("Length: " ^ (Int.toString l));
		0
		end)
	else
	(let
		val returned = get_item_index(List.rev(List.take((List.rev(mylist)), (List.length(mylist) - 1))), item)
	in
		print("\nindex String: "^String.implode(mylist));
		print("\nReturned value: "^(Int.toString returned));
		1 + returned
	end)
				

(*
	Takes a list of chars
	Recursively surfes it until the first " "
	Returns a char list without the first string
*)
(* fun remove_first_string chrs : char list =
	if ((is_space (List.nth(chrs, 0)))) then 
		chrs
	else
		(let
			val item = List.nth(chrs, 0)
		in
			(* recursion on the rest of the list *)
			remove_first_string(List.rev(List.take((List.rev(chrs)), (List.length(chrs) - 1))))
		end) *)

(*
	Takes a list of chars
	Recursively surfes it until the first " "
	Returns a string
*)
fun get_strings strings : string list =
	(let
		val string_length = size(strings);
		val string_as_char_array = explode(strings);
		val space_index = get_item_index(string_as_char_array, #" ");
	in
		print("\n" ^ (Int.toString space_index) ^ "\n");
		print("\n" ^ (Int.toString string_length) ^ "\n");
		if (space_index <= string_length) then
			(let
				val subst = substring(strings, 0, space_index);
			in
				if (space_index = string_length) then
					(let
					in
						print("\nString: " ^ strings ^ "\n");
						print("\nSubstring: " ^ subst ^ "\n");
						if (subst = " " orelse subst = "") then 
							List.take([""], 0)
						else
							[subst]
					end)
				else
					(let
						val index_after_space = space_index+1;
						val rest_subst = substring(strings, index_after_space, string_length - index_after_space);
					in
						print("\n" ^ (Int.toString index_after_space) ^ "\n");
						print("\nRest substring: " ^ rest_subst ^ "\n");
						print("\nString: " ^ strings ^ "\n");
						print("\nSubstring: " ^ subst ^ "\n");
						if (subst = " " orelse subst = "") then 
							get_strings(rest_subst)
						else
							subst :: get_strings(rest_subst)
					end)
				
			end)
		else
			List.take([""], 0)
	end)

fun show list : string list = 
	if (List.length(list)) = 0 then 
		List.take([""], 0)
	else
		(let 
		val str = List.nth(list, 0);
		val chrs = explode(str);
		val pure_chrs = delete( #"\n", (delete(#"\t", chrs)));
		val str = String.implode(pure_chrs);

		(* val new_str = get_first_string(pure_chrs); *)

		in
		(* print(str^"\n"); *)
		(* print(new_str^"\n"); *)
		(* chrs_list_lexem_split(pure_chrs);		 *)

			(* recursion on the rest of the list *)
		get_strings(str) @ show(List.rev(List.take((List.rev(list)), (List.length(list) - 1))))
		end)

(* (*
	Returns a list of strings from one single string
*)
fun chrs_list_lexem_split chrs : string list = 
	if (is_space List.nth(chrs, 0)) then
		List.take([""], 0)
	else
		(let	
		val chrs_item_as_str =  String.str(List.nth(chrs, 0));

		in
		print(chrs_item_as_str);
		chrs_list_lexem_split(List.rev(List.take((List.rev(list)), (List.length(list) - 1))))
		end) *)



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




(* show pureGraph; *)

val str = List.nth(pureGraph, 0);
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