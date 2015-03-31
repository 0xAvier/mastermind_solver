(* Defines a list of clause and a corresponding variable list *)
type Set = Clause list * Variable list

(* Returns a new Set *)
fun newSet() : Set =
    (nil, nil)
;

(* Add a Variable to the Set *)
fun addVariableToSet(s : Set, v : Variable) : Set =
    (#1(s), #2(s)@[v])
;

(* Add a Clause to the Set *)
fun addClauseToSet(s : Set, c : Clause) : Set =
    let val newClauseList : Clause list = #1(s)@[simplifyClause(c)]
	(* val newClauseList : Clause list  = filterIncludedClause(newClauseList, 0, 0) *)
	val variable = #2(s)
    in
	(* (#1(s)@[c], #2(s)) *)
	(newClauseList, variable)
    end
;

(* Add a list of Clause to the Set *)
fun addListOfClauseToSet(s : Set, cList : Clause list) : Set =
    if length(cList) = 0 then
	s
    else
	let val newSet = addClauseToSet(s, hd(cList))
	in
	    addListOfClauseToSet(newSet, tl(cList))
	end
;


(* Affect a Variable in a Set *)
fun affectVar(s : Set, v : Variable, b : boolValue) : Set =
    (affectVariableInClauseList(#1(s), number(v), b), affectVariableInVList(#2(s), v, b))
;

(* remove every empty Clause in the Set *)
fun removeEmptyClauseFromSet(s : Set) : Set =
    (removeEmptyClause(#1(s)), #2(s))
;

(* Makes the affectation of every Variable in the list *)
(* The list contains only unitary variable *)
fun affectUnitaryVariableList(s : Set, c : Clause list) : Set =
    case c of
	h::t =>
	let val varNumber = List.nth(filter(fn x => x <>0, h), 0)
	    val varAbs = abs(varNumber)
	in
	    affectUnitaryVariableList(affectVar(s, List.nth(#2(s), varAbs-1), signOfVariable(varNumber)), t)
	end
      | nil => s
;

(* Affect every unitary variable in a Set *)
fun affectUnitaryVariable(s : Set) : Set =
    let val listUnitaryClause = filter((fn x => (length(filter(fn y => y <> 0, x)) = 1)),#1(s))
	val newS = affectUnitaryVariableList(s, listUnitaryClause)
    in
	(
	 (* displayClauseList(listUnitaryClause); *)
	 newS
	)
    end
;

(* Returns true iff all the Variable are defined *)
fun allVariableAffected(s : Set) : bool =
    case s of
	(_, h::t) => isAffected(h) andalso allVariableAffected((nil, t))
      |	(_, nil) => true
;

(* Returns a Variable that is not affected *)
fun getVarToAffect(v : Variable list) : Variable =
    case v of
	h::t => if isAffected(h) then
		    getVarToAffect(t)
		else
		    h
      | nil => (~10000000, UNDEFINED)
;


(* Returns true iff the list contains some 0 *)
fun listContains0(l : int list) =
    length(filter(fn x => x = 0, l)) <> 0
;

(* Returns true iff the length does not change when you keep only the 0's *)
fun clauseLengthStableTo0Keeping(l : int list) =
    length(l) = length(filter(fn x => x = 0, l))
;

(* Returns true iff the list contains only 0 *)
fun listContainsOnly0(l : int list) =
    listContains0(l) andalso clauseLengthStableTo0Keeping(l)
;

(* Returns true if a Variable only contains the "0"th Variable *)
fun falseClauseExist(s : Set) : bool =
    length(filter(fn l => listContainsOnly0(l), #1(s))) <> 0
;

(* Returns a reference to the most recently added Variable of the set  *)
fun lastVariableRef(s : Set) : Variable ref =
    let val lastVar = List.nth(#2(s), length(#2(s)) - 1)
    in
	ref(lastVar)
    end
;

(**** Display functions ****)

(* Displays a Set *)
fun displaySet(s : Set) =
    (displayVariableList(#2(s)) ;
     displayClauseList(#1(s)))
;

