(*<--------------------------
'<' for at least some text recognition in gedit

Function definition

*)
print("\n------Function definition:\n");

fun sum(x:int, y:int) = x + y;

fun quads(x:int, y:int):int*int = (x*x, y*y);

(*--------------------------

Currying

*)
print("\n------Currying:\n");

fun sum(x:int)(y:int) = x + y;


(*--------------------------

Pattern matching

*)
print("\n------Pattern matching:\n");

fun choice 1 = "One"
|   choice 2 = "Two"
|   choice 3 = "Three"
|   choice _= "Many";
choice 10;


(*--------------------------

Recursive functions

*)
print("\n------Recursive functions:\n");

fun fact 0= 1
|   fact n= n*fact(n-1);
fact 5;

fun fact n = if n = 0 then 1 else n*fact(n-1);
fact 5;

fun fib 1 = 0
|   fib 2 = 1
|   fib n = fib(n-1) + fib(n-2);
fib 5;


(*--------------------------

Unnamed functions

*)
print("\n------Unnamed functions:\n");

val r = (fn x =>(*<*) x+1)(0);

val quad = fn x =>(*<*) x*x;
val r = quad 4;


(*--------------------------

Functions as parameters

*)
print("\n------Functions as parameters:\n");

fun cube x = x*x*x;
fun apply f x = f x;
apply cube 5;
fun apple f x = f(x);
apple (fn x =>(*<*) x - 1.7) 4.0;


(*--------------------------

Polumorphic functions

*)
print("\n------Polymorphic functions:\n");

fun i x = x;
i 5;
i "people";


(*--------------------------

Function as a function result

*)
print("\n------Function as a function result:\n");

fun quad x = x*x;
fun choice c f g = if c then f else g;
val temp = choice true quad cube;
temp 2;


(*--------------------------

Variable scope

*)
print("\n------Variable scope:\n");

val c = 444;
val result =
	let
		val a = 3 + 4
		val b = 7 * a - 9
	in
		(a*b)-c
	end;

local
	val x = 5.0
	val y = 10.2 - x
in
	val z = x*y
end;


(*--------------------------

Several variables set

*)
print("\n------Several variables set:\n");

val (a, b) = quads(3, 4);


(*--------------------------

Tuple

*)
print("\n------Tuple:\n");

val t = ("string", true, 7.77);
val t2 = #1 t;


(*--------------------------

Record

*)
print("\n------Record:\n");

val man = { Name = "Smith", Age = 23 };
val age_of_man= #Age man;


(*--------------------------

List

*)
print("\n------List:\n");

val prim = [1, 3, 7, 11, 13];

fun length [] = 0
|   length (h::t) = 1 + length t;
fun append ([],l) = [l]
|   append (h::t, l) = h :: append(t, l)

val g = append(prim, 6);
val g = length prim;

val b = (1,2) :: [(3,4)];
val b = [1,2]@[3,4];

val h::t = [1, 2, 3];

(*--------------------------

Function composition

*)
print("\n------Function composition:\n");

fun drop_neg x = if x < 0.0 then 0.0 else x;
val nonneg_sin = drop_neg o Math.sin;
drop_neg(Math.sin(1.0));
nonneg_sin(1.0);


(*--------------------------

Operator functions

*)
print("\n------Operator functions:\n");

infix concat;
infix +;
fun (x:string) concat (y:string) = x ^ y;
fun (x:string) + (y:string) = x ^ y;
"One"^"Two";
"One" concat "Two";
"One" + "Two";
(op+)("One","Two");
