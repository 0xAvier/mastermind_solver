(* Define a Variable type *)
(* The int is the number of the variable *)
(* The boolValue correspond to an assignation of the variable *)
type Variable = int * boolValue
(* Used to count the number of variable in the program*)
val varNumber = ref(0)

(* Return a new Variable *)
fun newVariable() : Variable =
    (
     varNumber := !varNumber + 1;
     (!varNumber, UNDEFINED)
    )
;


(* Returns true iff v = DEFINED(true) *)
fun isTrue(b : boolValue) : bool =
    case b of
	DEFINED(true) => true
      | DEFINED(false) => false
      | UNDEFINED => false
;

(* Return true iff a Variable is positive *)
fun signOfVariable(n : int) : boolValue =
    DEFINED(n > 0)
;

(* Return the number of a Variable *)
fun number(v : Variable) : int =
    #1(v)
;

(* Returns true iff the Variable is defined *)
fun isAffected(v : Variable) : bool =
    case v of
	(_, UNDEFINED) => false
      | (_, DEFINED(_)) => true
;

(* Affect a Variable if it's not already affected *)
fun affect(v : Variable, b : boolValue) : Variable =
    case #2(v) of
	UNDEFINED => (#1(v), b)
      | _ => v
;

(* Affect the first occurence of a Variable in a list of Variable *)
fun affectVariableInVList(vList : Variable list, v : Variable , b : boolValue) : Variable list =
    case vList 
     of (h::t) => if (h = v) then
		      affect(v, b)::t
		  else
		      h::affectVariableInVList(t, v, b)
      | nil => nil
;

(**** Display function ****)

(* Display a Variable *)
fun displayVariable(v : Variable) : unit =
    print("Variable number " ^ Int.toString(#1(v)) ^ " = " ^  BoolValue2String(#2(v)) ^ "\n")
;

(* Display a Variable list *)
fun displayVariableList(v : Variable list) : unit =
    case v of 
	nil => print("")
      | h::t => (displayVariable(h); displayVariableList(t))
;
