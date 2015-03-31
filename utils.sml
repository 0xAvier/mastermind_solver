(* Define a bool value for the variable *)
(* It's either a bool or undefined *)
datatype boolValue  = DEFINED of bool | UNDEFINED

(* Translate a boolValue to a string*)
fun BoolValue2String(b : boolValue) : string = 
    case b 
     of DEFINED(b) => Bool.toString(b)
      | UNDEFINED => "Undefined"
;

fun BoolValue2Bool(b : boolValue) : bool =
    case b of 
	DEFINED(false) => false
      | DEFINED(true) => true
;

(* Return the absolute value of an integer*)
fun abs(r: int) : int =
  if r < 0 then ~r else r
;

(* Display a list of int *)
fun displayList(l : int list) =
    case l
     of nil => print("")
      | h::t => (print(Int.toString(h) ^ ",") ; displayList(t))
;



fun displayL2(l2 : int list list) =
    if length(l2) = 0 then
	print("")
    else
	(
	 print("[");
	 displayList(hd(l2));
	 print("]");
	 displayL2(tl(l2))
	)
;

fun displayL3(l3 : int list list list) =
    if length(l3) = 0 then
	print("")
    else
	(
	 print("[");
	 displayL2(hd(l3));
	 print("]\n");
	 displayL3(tl(l3))
	)
;

(* Allow the programm to exit for any reason *)
fun exit() =
    OS.Process.exit(OS.Process.success)
;

(* standard filter with lambda function  *)
fun filter (f, []) = []
  | filter (f, x::xs) = if f x
			then x::(filter (f , xs))
			else filter (f , xs)
;


(* Returns n! *)
fun fact(n)  : int =
    if n=0 then 1 else n * fact(n - 1)
;

(* Returns the binomial coefficient  *)
fun binom(n : int, k : int) =
    fact(n) div (fact(k) * fact(n - k))
;

(* The int list is the list of the current set being negated *)
(* N-Uplet stands for !lineSize-Uplet *)
fun generateAllNUplet(start : int, size : int, max : int) =
    (* only one 0-Uplet exist : the empty list *)
    if size <= 0 then
	[[]]
    else
	(* let val nbUplet : int = binom(max + 1 - start, size) *)
	let val nbUplet : int = max - size + 1 - start + 1
	in
	    (
	     List.concat (List.tabulate(nbUplet, fn x =>
						    map (fn l : int list => (start+x)::l)
							(generateAllNUplet(start + 1 + x, size - 1, max))
				       )
			 )
	    )
	end
;

(* Entry  [[[listA0],[listA1]],[[listB0],[listB1]], ...] *)
(* Output [[[x::listA0],[listA1]],[[x::listB0],[listB1]], ...] *)
(* Pos correspond to the list to which you want to be concatane (0 or 1) *)
(* The example is for pos = 0 *)
fun concatToInsideList(l3 : int list list list, x : int, pos : int) : int list list list =
    if pos = 0 then
	map (fn l2 => [x::List.nth(l2, pos), List.nth(l2, pos + 1)]) l3
    else
	map (fn l2 => [List.nth(l2, pos-1), x::List.nth(l2, pos)]) l3
;

val a : IntInf.int = Time.toSeconds(Time.now());
val ab : int = IntInf.toInt(a mod 10000000);
val b : IntInf.int = Time.toSeconds(Time.now());
val bb : int = IntInf.toInt(b mod 10000000);
val alea = Random.rand(ab, bb);
(* Time.toSeconds()); *)


