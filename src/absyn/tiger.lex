type svalue = Tokens.svalue
type pos = int
type ('a, 'b) token = ('a, 'b) Tokens.token
type lexresult = (svalue, pos) token

val lineNum = ErrorMsg.lineNum
val linePos = ErrorMsg.linePos
fun err(p1,p2) = ErrorMsg.error p1

val comment_pos = ref 0
val nesting_level = ref 0
fun inc_nesting() = (nesting_level := !nesting_level + 1)
fun dec_nesting() = (nesting_level := !nesting_level - 1)

val str = ref ""
val str_pos = ref 0
val str_closed = ref true
val control_ord = ref 0
val control_str = ref ""
val ignored_pos = ref 0
val ignored_closed = ref true

fun eof() = let
                val pos = hd(!linePos)
            in
                if !nesting_level > 0
                    then (ErrorMsg.error(!comment_pos) ("comment was not closed"); Tokens.EOF(pos,pos))
                else if !ignored_closed = false
                    then (ErrorMsg.error(!ignored_pos) ("ignored sequence was not closed"); Tokens.EOF(pos,pos))
                else if !str_closed = false
                    then (ErrorMsg.error(!str_pos) ("string was not closed"); Tokens.EOF(pos,pos))
                else
                    (Tokens.EOF(pos,pos))
            end


%%
%header (functor TigerLexFun(structure Tokens: Tiger_TOKENS));
%s COMMENT STRING CONTROL IGNORED;

escape_sequence = \\(n|t|[0-9]{3}|\"|\\);
ignored_sequence = [\ \t\f];
id = [a-zA-Z][a-zA-Z0-9_]*;
digit = [0-9];
whitespace = [\ \t];


%%
<INITIAL, COMMENT, IGNORED> \n	=> (lineNum := !lineNum + 1; linePos := yypos :: !linePos; continue());

<INITIAL> "type"	=> (Tokens.TYPE(yypos, yypos + size(yytext)));
<INITIAL> "var"	=> (Tokens.VAR(yypos, yypos + size(yytext)));
<INITIAL> "function"	=> (Tokens.FUNCTION(yypos, yypos + size(yytext)));
<INITIAL> "break"	=> (Tokens.BREAK(yypos, yypos + size(yytext)));
<INITIAL> "of"	=> (Tokens.OF(yypos, yypos + size(yytext)));
<INITIAL> "end"	=> (Tokens.END(yypos, yypos + size(yytext)));
<INITIAL> "in"	=> (Tokens.IN(yypos, yypos + size(yytext)));
<INITIAL> "nil"	=> (Tokens.NIL(yypos, yypos + size(yytext)));
<INITIAL> "let"	=> (Tokens.LET(yypos, yypos + size(yytext)));
<INITIAL> "do"	=> (Tokens.DO(yypos, yypos + size(yytext)));
<INITIAL> "to"	=> (Tokens.TO(yypos, yypos + size(yytext)));
<INITIAL> "for"	=> (Tokens.FOR(yypos, yypos + size(yytext)));
<INITIAL> "while"	=> (Tokens.WHILE(yypos, yypos + size(yytext)));
<INITIAL> "else"	=> (Tokens.ELSE(yypos, yypos + size(yytext)));
<INITIAL> "then"	=> (Tokens.THEN(yypos, yypos + size(yytext)));
<INITIAL> "if"	=> (Tokens.IF(yypos, yypos + size(yytext)));
<INITIAL> "array"	=> (Tokens.ARRAY(yypos, yypos + size(yytext)));

<INITIAL> ":="	=> (Tokens.ASSIGN(yypos, yypos + size(yytext)));
<INITIAL> "|"	=> (Tokens.OR(yypos, yypos + size(yytext)));
<INITIAL> "&"	=> (Tokens.AND(yypos, yypos + size(yytext)));
<INITIAL> ">="	=> (Tokens.GE(yypos, yypos + size(yytext)));
<INITIAL> ">"	=> (Tokens.GT(yypos, yypos + size(yytext)));
<INITIAL> "<="	=> (Tokens.LE(yypos, yypos + size(yytext)));
<INITIAL> "<"	=> (Tokens.LT(yypos, yypos + size(yytext)));
<INITIAL> "<>"	=> (Tokens.NEQ(yypos, yypos + size(yytext)));
<INITIAL> "="	=> (Tokens.EQ(yypos, yypos + size(yytext)));

<INITIAL> "/"	=> (Tokens.DIVIDE(yypos, yypos + size(yytext)));
<INITIAL> "*"	=> (Tokens.TIMES(yypos, yypos + size(yytext)));
<INITIAL> "-"	=> (Tokens.MINUS(yypos, yypos + size(yytext)));
<INITIAL> "+"	=> (Tokens.PLUS(yypos, yypos + size(yytext)));

<INITIAL> "."	=> (Tokens.DOT(yypos, yypos + size(yytext)));
<INITIAL> "}"	=> (Tokens.RBRACE(yypos, yypos + size(yytext)));
<INITIAL> "{"	=> (Tokens.LBRACE(yypos, yypos + size(yytext)));
<INITIAL> "]"	=> (Tokens.RBRACK(yypos, yypos + size(yytext)));
<INITIAL> "["	=> (Tokens.LBRACK(yypos, yypos + size(yytext)));
<INITIAL> ")"	=> (Tokens.RPAREN(yypos, yypos + size(yytext)));
<INITIAL> "("	=> (Tokens.LPAREN(yypos, yypos + size(yytext)));
<INITIAL> ";"	=> (Tokens.SEMICOLON(yypos, yypos + size(yytext)));
<INITIAL> ":"	=> (Tokens.COLON(yypos, yypos + size(yytext)));
<INITIAL> ","	=> (Tokens.COMMA(yypos, yypos + size(yytext)));

<INITIAL> \"    => (YYBEGIN STRING; str := ""; str_pos := yypos; str_closed := false; continue());
<STRING> \" => (YYBEGIN INITIAL; str_closed := true; Tokens.STRING(!str, !str_pos, yypos + size(yytext)));
<STRING> {escape_sequence}*    => (str := !str ^ Option.valOf(String.fromString(yytext)); continue());
<STRING> "\\^"    => (YYBEGIN CONTROL; continue());
<CONTROL> . => (YYBEGIN STRING; control_ord := ord(String.sub(yytext, 0));
                if (!control_ord >= 64 andalso !control_ord <= 95)
                then
                    control_str := String.str(chr(!control_ord - 64))
                else
                    ErrorMsg.error yypos ("illegal control character: " ^ yytext);
                str := !str ^ !control_str; continue());
<STRING> \\ => (YYBEGIN IGNORED; ignored_pos := yypos; ignored_closed := false; continue());
<IGNORED> {ignored_sequence}*   => (continue());
<IGNORED> \\    => (YYBEGIN STRING; ignored_closed := true; continue());
<IGNORED> . => (ErrorMsg.error yypos ("illegal formatting character in ignored sequence: " ^ yytext); continue());
<STRING> \n	=> (lineNum := !lineNum + 1; linePos := yypos :: !linePos;
                ErrorMsg.error yypos ("illegal new line break in string literal"); continue());
<STRING> .  => (str := !str ^ yytext; continue());

<INITIAL> {digit}+	=> (Tokens.INT(Option.valOf(Int.fromString(yytext)), yypos, yypos + size(yytext)));

<INITIAL> {id}   => (Tokens.ID(yytext, yypos, yypos + size(yytext)));

<INITIAL> "/*"	=> (YYBEGIN COMMENT; comment_pos := yypos; inc_nesting(); continue());
<COMMENT> "/*"	=> (inc_nesting(); continue());
<COMMENT> "*/"	=> (dec_nesting(); if !nesting_level = 0 then YYBEGIN INITIAL else (); continue());
<COMMENT> .  => (continue());

<INITIAL> {whitespace}+    => (continue());

<INITIAL> .       => (ErrorMsg.error yypos ("illegal character " ^ yytext); continue());
