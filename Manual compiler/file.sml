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

fun concat_string_list(strings : string list):string = 
	if (List.length(strings) = 0) then
		""
	else
		(let
			val st = List.nth(strings, 0);
		in
			st^concat_string_list(List.rev(List.take((List.rev(strings)), (List.length(strings) - 1))))
		end)

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
	Reads from file and returns as a string list
 *)
fun readlist (infile : string) = 
	let 
		val ins = TextIO.openIn infile 
		fun loop ins = 
			case TextIO.inputLine ins of 
				SOME line => line :: loop ins
				| NONE => []
	in
		loop ins before TextIO.closeIn ins
	end ;

type lexem_entry = {lexem:string, lexem_line:int, lexem_pos:int};


fun print_lexems (lexems) = 
	if(List.length(lexems) = 0) then
		0 (* unnesessary return value to stop the recursion*)
	else
		(let
			val entry:lexem_entry = List.nth(lexems, 0);
			val entry_string = "\""^(#lexem entry)^"\" : " ^ (Int.toString (#lexem_line entry)) ^ " _ " ^ (Int.toString (#lexem_pos entry));
		in
			print(entry_string^"\n");
			print_lexems(List.drop(lexems, 1))
		end)
(* 
fun print_ascii (num:int)=
	if (num >= 255) then
		0
	else
		(let 
			val ch = chr num;
		in
			print("\n"^(Int.toString num)^" "^String.str(ch)^"\n");
			print_ascii(num + 1)
		end) *)
(* 
	Assembles if needed and returns a single operator as a string
 *)
fun get_operator chs : string = 
	(let
		val ch = List.nth(chs, 0);
	in
		case ch 
		of #":" => if (List.length(chs) > 1) then
					if (List.nth(chs, 1) = #"=") then
						":="
					else
						":"
				else
					":"
		| #"<" => if (List.length(chs) > 1) then
					if (List.nth(chs, 1) = #"=") then
						"<="
					else if (List.nth(chs, 1) = #">") then
						"<>"
					else 
						"<"
				else
					"<"
		| #">" => if (List.length(chs) > 1) then
					if (List.nth(chs, 1) = #"=") then
						">="
					else
						">"
				else
					">"
		| _ => String.str(ch)
	end)

(* 
	Assembles and returns a string literal as a string
	or all the rest char array as a single string
	if close quote is missed
 *)
fun get_string_literal(chs) = 
(* if end of the string without closed quote \" *)
	if (List.length(chs) = 0) then 
		""
	else if (List.nth(chs, 0) = #"\"") then 
		"\""
	else
		String.str(List.nth(chs, 0)) ^ get_string_literal(List.drop(chs, 1))

(* 
	Assembles and returns a comment as a string
	or all the rest char array as a single string
	if comment is not closed
 *)
fun get_comment(chs)=
(* if end of the string without closed comment "*/" *)
	if (List.length(chs) = 0) then
		""
	else if (List.length(chs) > 1) then
		if (List.nth(chs, 0) = #"*") andalso (List.nth(chs, 1) = #"/") then 
			"*/"
		else 
			String.str(List.nth(chs, 0)) ^ get_comment(List.drop(chs, 1))
	else
		String.str(List.nth(chs, 0))

(* 
	Assembles and returns consequtive letters, digits and _ as a string
 *)
fun get_id_or_keyword(chs) = 
	if (List.length(chs) = 0) then
		""
	else 
		(let
			val ch = List.nth(chs, 0);
		in
			if (is_letter ch) orelse (is_digit ch) orelse (ch = #"_") then
				String.str(ch) ^ get_id_or_keyword(List.drop(chs, 1))
			else 
				""
		end)
(* 
	Returns the first lexem as a string
 *)
fun get_lexem(chs : char list) = 
	if (List.length(chs) = 0) then (* if no more charatcers in array*)
		""
	else
		(let
			val ch = List.nth(chs, 0);
		in
			(* print("\n"^String.str(ch)^"\n"); *)
			if (ch = #" ") then
				" " (* upper level processing to ignore spaces and tabs*)
			else if (ch = #"\t") then
				"\t"
			else if (is_letter ch) then 
				String.str(ch) ^ get_id_or_keyword(List.drop(chs, 1))
			else if (ch = #"\"") then
				"\"" ^ get_string_literal(List.drop(chs, 1))
			else if (ch = #"/") then
				if (List.length(chs) > 1) then
					if (List.nth(chs, 1) = #"*") then
						"/*" ^ get_comment(List.drop(chs, 2))
					else
						"/"
				else
					"/"
			else (*  any other symbols on lexical analysis are got as operators*)
				get_operator(chs)

		end)

(* 
	Returns a list of lexems as records

	inp : list of characters 
	line : vertical lexet coordinate
	pos : horizontal lexem coordinate
*)
fun get_lexems(inp, line, pos):lexem_entry list = 
	if (List.length(inp) = 0) then (* if no more charatcers in array*)
		(let
			val lexem_str = get_lexem(inp);
		in
			if (lexem_str = "") then (* prevent of creating of a empty-string lexem*)
				[]
			else 
				(let
					val entry:lexem_entry = {lexem = lexem_str, lexem_line = line, lexem_pos = pos};
				in
					(* print("\nString: " ^ (String.implode(inp))^"\n");
					print("\nLexem string: " ^ lexem_str^"\n");
					print_lexems([entry]); *)
					[entry]
				end)
		end)
	else
		(let
			val ch = List.nth(inp, 0);
			val lexem_str = get_lexem(inp);
			val new_inp = List.drop(inp, size(lexem_str));
		in
			(* print((Int.toString (ptr+1))^" / "^(Int.toString (List.length(inp)))^"\n");
			print("Current: "^String.str(ch)^"\nNext: "^(String.str(next_ch)^"\n\n")); *)
			(* print("\nString: " ^ (String.implode(inp))^"\n");
			print("\nString: " ^ (String.implode(new_inp))^"\n");
			print("\nLexem string: " ^ lexem_str^"\n"); *)
			if (ch = #"\n") then
				[] @ get_lexems(new_inp, line+1, 0)
			else if (lexem_str = " " orelse lexem_str = "\t") then (* ignore spaces and tabs *)
				[] @ get_lexems(new_inp, line, pos+size(lexem_str))
			else
				(let
					val entry:lexem_entry list = get_lexems(new_inp, line, pos+size(lexem_str));
				in
					(* print_lexems(entry); *)
					{lexem = lexem_str, lexem_line = line, lexem_pos = pos}::entry
				end)
			end)

val infile = "./input.tig" ;
(*
	Read list of strings from file
*)
val pureGraph =  readlist(infile);
val f = concat_string_list(pureGraph);
val chars = explode(f);
val a = get_lexems(chars, 1, 1);