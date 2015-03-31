(* A clause is a disjunction of logical variable *)
(* Here we define a Clause by a list of int *)
(* Each int is either a number of Variable or it's opposite, *)
(* wether the Variable is positive or negative in the clause. *)
type Clause = int list

(* Returns a new Clause from a list of Variable and int *)
(* Each int in the list indicates if the corresponding Variable *)
(* is negative or not. *)
(* length(vList) = length(s) *)
fun newClause(vList : Variable list, signList : int list) : Clause =
    case (vList, signList) of
	(h::t, hs::ts) => let val n = if hs = ~1 then 0 - #1(h) else #1(h)
			  in
			      n::newClause(t, ts)
			  end
      | (nil, nil) => nil
      | (nil, _) => (print("CLAUSE ERROR: The number of sign exceeds the number of variable.\n"); exit())
      | (_, nil) => (print("CLAUSE ERROR: The number of variable exceeds the number of sign.\n"); exit())
;

fun newClauseFromInt(vList : int list, signList : int list) : Clause =
    case (vList, signList) of
	(h::t, hs::ts) => let val n = if hs = ~1 then ~h else h
			  in
			      n::newClauseFromInt(t, ts)
			  end
      | (nil, nil) => nil
      | (nil, _) => (print("CLAUSE ERROR: The number of sign exceeds the number of variable.\n"); exit())
      | (_, nil) => (print("CLAUSE ERROR: The number of variable exceeds the number of sign.\n"); exit())
;


(* Adds a Variable a Clause, and negates it if positive is set to false *)
fun addVariableToClause(c : Clause, v : Variable, positive : bool) : Clause =
    let val n : int = (if positive then number(v) else 0-number(v))
    in
	n::c
    end
;

(* Return true if the Variable is present in the Clause *)
fun VariableIsInClause(c : Clause, n : int) : bool =
    case c
     of (h::t) => if ((h = n) orelse (h = 0-n)) then
		      true
		  else
		      VariableIsInClause(t, n)
      | (nil) => false
;

(* Returns true iff the variable appears with the sign b in the clause *)
fun signOfVariableInClauseEquals(c : Clause, n : int, b : boolValue) : bool =
    case c
     of (h::t) => if h = n andalso b = DEFINED(true) then
		      true
		  else 
		      if h = ~n andalso b = DEFINED(false) then
			  true
		      else
			  signOfVariableInClauseEquals((t), n, b)
      |  (nil) => false
;

(* Returns true iff the variable appears with the same sign in the clause *)
fun signedVariableIsInClause(c : Clause, n : int) : bool =
    signOfVariableInClauseEquals(c, n, signOfVariable(n))
;

(* Affect a Variable in a Clause *)
(* b correspond to the value affected *)
fun affectVariableInClause(c : Clause, n : int, b : boolValue) : Clause =
    (
     if (VariableIsInClause(c, n)) then
	 if signOfVariableInClauseEquals(c, n, b) then
	   nil
	 else 
	   case c
	    of (h::t) => if (h = n orelse h = 0-n) then
			     0::t
			 else
			     h::affectVariableInClause((t), n, b)
	     | (nil) => c
     else
	 c
    )
;

(* Affect a Variable in a Clause list *)
fun affectVariableInClauseList(cList : Clause list, n : int, b : boolValue) : Clause list =
    case cList
     of h::t => affectVariableInClause(h, n, b)::affectVariableInClauseList(t, n, b)
      | nil => nil
;

(* Remove every empty Clause in the Clause list *)
fun removeEmptyClause(cList : Clause list) : Clause list =
    case cList of
	(nil)::t => removeEmptyClause(t)
      | h::t => h::removeEmptyClause(t)
      | nil => nil
;

(**** Display function ****)

(* Display a Clause *)
fun displayClause(c : Clause) : unit =
    case c
     of  (nil) => print("")
       | (h::t) => (print(Int.toString(h) ^ " ") ; displayClause(t))
;

(* Display a Clause list *)
fun displayClauseList(c : Clause list)  : unit =
    case c
     of nil => print("")
      | h::t => (print("Clause : ") ; displayClause(h) ; print("\n") ; displayClauseList(t))
;


fun cnf2dnf(c2 : Clause) : Clause list =
    map (fn v => [v]) c2
;

fun removeMultiOccurences(c  : Clause, s : int) : Clause =
    if s >= length(c) then
	c
    else
	let val newC = (List.filter (fn x => x <> hd(c)) c) @ [hd(c)]
	in
	    removeMultiOccurences(newC, s + 1)
	end
;

fun isTautology(c : Clause, s : int) : bool =
    if s >= length(c) then
	false
    else
	let val onlyNegative = (List.filter (fn x => x = ~(hd(c))) c) 
	    val res = length(onlyNegative) <> 0
	in
	    res orelse isTautology(tl(c), s + 1)
	end
;
    

fun simplifyClause(c : Clause) : Clause =
    let val c = removeMultiOccurences(c, 0)
    	val isTauto = isTautology(c, 0)
    in
    	if isTauto then
    	    nil
    	else
    	    c
    end
;

fun isIncludeIn(c1 : Clause, c2 : Clause) : bool =
    if length(c1) <= length(c2) then
	foldl (fn (v,b) => b andalso (length(List.filter (fn x => x = v) c2) <> 0)) (true) c1
    else
	false
;

fun filterIncludedClause(cl : Clause list, s : int, stop : int) : Clause list =
    let val stop = if s = 0 then length(cl) else stop
    in
	if s > length(cl)  then
	    cl
	else
	    let val hC : Clause = hd(cl)
		val tC : Clause list = tl(cl)
		val res : Clause list = (List.filter (fn c => not(isIncludeIn(hC, c))) tC)
	    in
		filterIncludedClause(res @ [hC], s + 1, stop)
	    end
    end
;

fun mergeCNFWithDNF(cnf : Clause, dnf : Clause list) : Clause list =
    let 
	val dnf = (map (fn x => removeMultiOccurences(x, 0)) dnf)
	(* Here "isTautology" return the opposite -> true if the dnf is always false *)
	val dnf = (List.filter (fn x => not(isTautology(x, 0))) dnf)
	(* val t = (print("Before, length = " ^ Int.toString(length(dnf))) ; print(".\n\n\n")) *)
	val dnf = filterIncludedClause(dnf, 0, 0)
	(* val t = (print("After, length = " ^ Int.toString(length(dnf))) ; print(".\n\n\n")) *)
	(* val t = (displayL2(dnf); print("\n\n\n")) *)
	val res = List.concat(map (fn vc => map (fn vd => vc::vd) dnf) cnf)
	val cnf = (map (fn x => removeMultiOccurences(x, 0)) res)
	val cnf = (List.filter (fn x => not (isTautology(x, 0))) cnf)
    in
	(
	 cnf
	)
    end
;

fun cnfList2dnfList(dl : Clause list) : Clause list =
    if length(dl) >= 2 then
	foldl (mergeCNFWithDNF) (cnf2dnf(hd(dl))) (tl(dl))
    else
	cnf2dnf(hd(dl))
;
