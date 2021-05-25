fun delete (item, list) = 
	List.filter(fn x => x <> item) list

fun is_space ch = 
	if (ch = #" " orelse ch = #"\t") then
		true
	else 
		false

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

fun concat_string_list(strings):string = 
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
	Splits an input string
	by " " into a list of strings
*)
(* fun get_strings strings : string list =
	(let
		val string_length = size(strings);
		val string_as_char_array = explode(strings);
		val space_index = get_item_index(string_as_char_array, #" ");
	in
		if (space_index <= string_length) then
			(let
				val subst = substring(strings, 0, space_index);
			in
				if (space_index = string_length) then
					(let
					in
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
						if (subst = " " orelse subst = "") then 
							get_strings(rest_subst)
						else
							subst :: get_strings(rest_subst)
					end)
				
			end)
		else
			List.take([""], 0)
	end) *)
(* 
	Surves through a list of strings
	with a function given *)
(* fun show list wat : string list = 
	if (List.length(list)) = 0 then 
		List.take([""], 0)
	else
		(let 
		val str = List.nth(list, 0);
		val chrs = explode(str);
		val pure_chrs = delete( #"\n", (delete(#"\t", chrs)));
		val str = String.implode(pure_chrs);
		in
		(* recursion on the rest of the list *)
		(wat(str)) @ show(List.rev(List.take((List.rev(list)), (List.length(list) - 1))))
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
val str = List.nth(pureGraph, 0);
val f = concat_string_list(pureGraph);
val chars = explode(f);

type lexem_entry = {lexem:string, lexem_line:int, lexem_pos:int};

fun print_lexems (lexems) = 
	if(List.length(lexems) = 0) then
		0
	else
		(let
			val entry:lexem_entry = List.nth(lexems, 0);
			val entry_string = "\""^(#lexem entry)^"\" : " ^ (Int.toString (#lexem_line entry)) ^ " _ " ^ (Int.toString (#lexem_pos entry));
		in
			print(entry_string^"\n");
			print_lexems(List.drop(lexems, 1))
		end)
(* fun get_string_literal_close_quote_index(chs, ptr):int =
	if(List.length(chs) = ptr) then
		(* end rached without closed quote *)
		~1
	else if (List.nth(chs, ptr) = #"\"") then
		ptr
	else
		get_string_literal_close_quote_index(chs, ptr+1) *)
(* val l = get_lexems(chars, 1, 1, 0); *)
(* fun entry_str (entry:lexem_entry):string = 
	"\""^(String.str(#lexem entry))^"\" : " ^ (Int.toString (#lexem_line entry)) ^ " _ " ^ (Int.toString (#lexem_pos entry)) *)

(* val k:lexem_entry = List.nth(l, 0); *)
(* val k1 = "\""^(String.str(#lexem k))^"\" : " ^ (Int.toString (#lexem_line k)) ^ " _ " ^ (Int.toString (#lexem_pos k)); *)
(* print_lexems(l, 0); *)
(* fun test hh : string= 
	  case hh of 
	  	1 => "1"
		| 2 => "2"
		| 3 => "3"
		| 4 => "4"
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

fun get_string_literal(chs) = 
(* if end of the string without closed quote \" *)
	if (List.length(chs) = 0) then 
		""
	else if (List.nth(chs, 0) = #"\"") then 
		"\""
	else
		String.str(List.nth(chs, 0)) ^ get_string_literal(List.drop(chs, 1))

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

val teststring = "/*bcdefg hiklm*/nop\trst";
val testchs = explode(teststring);
(* val operator = get_operator(testchs); *)
fun get_lexem(chs) = 
	if (List.length(chs) = 0) then 
		""
	else
		(let
			val ch = List.nth(chs, 0);
		in
			(* print("\n"^String.str(ch)^"\n"); *)
			if (ch = #" ") then
				" "
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
			else
				get_operator(chs)

		end)

(* inp list of characters *)
fun get_lexems(inp, line, pos):lexem_entry list = 
	if (List.length(inp) = 0) then
		(let
			val lexem_str = get_lexem(inp);
		in
			if (lexem_str = "") then
				[]
			else 
				(let
					val entry:lexem_entry = {lexem = lexem_str, lexem_line = line, lexem_pos = pos};
				in
					print("\nString: " ^ (String.implode(inp))^"\n");
					print("\nLexem string: " ^ lexem_str^"\n");
					print_lexems([entry]);
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
			print("\nString: " ^ (String.implode(inp))^"\n");
			print("\nString: " ^ (String.implode(new_inp))^"\n");
			print("\nLexem string: " ^ lexem_str^"\n");
			if (ch = #"\n") then
				(let
					val entry:lexem_entry list = get_lexems(new_inp, line+1, 0);
				in
					print_lexems(entry);
					{lexem = lexem_str, lexem_line = line, lexem_pos = pos}::entry
				end)
			else if (lexem_str = " " orelse lexem_str = "\t") then
				[] @ get_lexems(new_inp, line, pos+size(lexem_str))
			else
				(let
					val entry:lexem_entry list = get_lexems(new_inp, line, pos+size(lexem_str));
				in
					print_lexems(entry);
					{lexem = lexem_str, lexem_line = line, lexem_pos = pos}::entry
				end)
			end)

val a = get_lexems(testchs, 1, 1);
(* val bb = List.drop(pureGraph, 1); *)