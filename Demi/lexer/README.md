# Lexer with MLex
_tokens.sig_ - Signature of the Tokens structure,used once for Mlex 

_tokens.sml_ - The Tokens structure, containing the token type and constructors that your lexer should use to build instances of the token type. 

_errormsg.sml_ - The ErrorMsg structure, useful for producing error messages with file names and line numbers.

_driver.sml_ - Run lexer on an input file.

_tiger.lex_ - File for Mlex due to which a ready-made lexer is generated

_sources.cm_ - A "makefile" for the ML Compilation Manager - 


use in cmd:

>SML -> CM.make("sourses".cm); 

After that you will get tiger.lex.sml - our lexer

Ask Parse structure for check lexer's work:

> Parse.parse "test.tig";

 
