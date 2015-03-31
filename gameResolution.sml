(* Color number * Is Present * Is at this position on the line *)
(* Hence the list must be !size long *)
type ColorVariable = int * Variable ref * Variable ref list
(* Defines a dictionnary of the colors variables variables *)
val colorVariableDictionnary : ColorVariable list ref = ref(nil)

(* Add the position variables for a color *)
(* step = position - 1 *)
fun addPositionVariables(refSet : Set ref, step : int) =
    if step = !lineSize then
	nil
    else
	let val s = addVariableToSet(!refSet, newVariable())
	    val posVar : Variable ref = lastVariableRef(s)
	in
	    (
	     refSet := s;
	     posVar::addPositionVariables(refSet, step + 1)
	    )
	end
;

(* Creates a new color variable  *)
fun newColorVariable(refSet : Set ref, number : int) : ColorVariable =
    (* Add a new variable to the set *)
    (* It's the presence variable *)
    let val newRef : Set ref = ref(addVariableToSet(!refSet, newVariable()));
	(* Collect its reference *)
    	val presenceVar : Variable ref = lastVariableRef(!newRef)
	(* Add some position variable *)
    	val colorPosVar : Variable ref list = addPositionVariables(newRef, 0)
    in
    	(
	 (* Keep the ref on the current Set *)
    	 refSet := !newRef;
	 (* Returns the new Color Variable *)
    	 (number, presenceVar, colorPosVar)
    	)
    end
;

(* Creates all the color variables *)
(* step begins at 0 *)
fun addColorVariable(s : Set, step : int) =
    if step = !numberOfColor then
	s
    else
	(* Create a new Set ref to keep the current Set  *)
	let val refSet : Set ref = ref(s)
	    val var : ColorVariable = newColorVariable(refSet, step + 1)
	in
	    (
	     (* Add a ColorVariable to the dictionnary *)
	     colorVariableDictionnary := var::(!colorVariableDictionnary);
	     (* colorVariableDictionnary := (!colorVariableDictionnary) @ [var]; *)
	     (* Recursion *)
	     addColorVariable(!refSet , step + 1)
	    )
	end
;

(* Returns the Colors variable with the corresponding number *)
fun getColorVariable(colorNumber : int) : ColorVariable =
    let in
	hd(filter(fn x => #1(x) = colorNumber, !colorVariableDictionnary))
    end
    handle Empty => (print("The color n°" ^ Int.toString(colorNumber) ^ " has not been found.\n"); raise Empty)
;    

(* Returns the Variable with indicates whether or not the corresponding Color is in the Line *)
fun getPresenceVariable(colorNumber : int) : Variable =
    let val colorVar : ColorVariable = getColorVariable(colorNumber)
    in
	!(#2(colorVar))
    end
    handle Empty => (print("The color n°" ^ Int.toString(colorNumber) ^ " has not been found.\n"); raise Empty)
;

fun getPositionVariable(colorNumber : int, posNumber : int) : Variable =
    let val colorVar : ColorVariable = getColorVariable(colorNumber)
	val res = !(List.nth(#3(colorVar), posNumber))
    in
	 res
    end
;

(* Returns the list of the presence variables *)
(* step = number of color - 1 *)
fun listOfPresenceVariable(step : int) : Variable list =
    (* map(getPresenceVariable, (List.tabulate(!numberOfColor, fn x => x + 1))) *)
    map getPresenceVariable (List.tabulate(!numberOfColor, fn x => x + 1))
;

(**** Game to Problem ****)

(* Add all the n-uplet as a negative clause *)
fun addExcludingPeerClauses(s : Set, nUplet : int list list) : Set =
    if length(nUplet) = 0 then
	s
    else
	let val s : Set = addClauseToSet(s, newClause((map (fn x => getPresenceVariable(x)) (hd(nUplet))), List.tabulate(length(hd(nUplet)), fn x => ~1)))
	in
	    addExcludingPeerClauses(s, tl(nUplet))
	end
;

(* On every group of !lineSize + 1 variables, at leat one is not in the solution *)
fun excludingPeers(s : Set) = 
    let val l : int list list = generateAllNUplet(1, !lineSize + 1, !numberOfColor)
    in
	addExcludingPeerClauses(s, l)
    end
;

(* Add all the n-uplet as a negative clause *)
fun addIncludingPeerClauses(s : Set, nUplet : int list list) : Set =
    if length(nUplet) = 0 then
	s
    else
	let val s : Set = addClauseToSet(s, newClause((map (fn x => getPresenceVariable(x)) (hd(nUplet))), List.tabulate(length(hd(nUplet)), fn x => 1)))
	in
	    addIncludingPeerClauses(s, tl(nUplet))
	end
;

(* On every group of !numberOfColor - !lineSize + 1 variables, at leat one is in the solution *)
fun includingPeers(s : Set) = 
    let val l : int list list = generateAllNUplet(1, !numberOfColor - !lineSize + 1, !numberOfColor)
    in
	addIncludingPeerClauses(s, l)
    end
;


(* Compute the fact that a color cannot be presents twice on a line *)
fun onlyOnceInColor(s : Set, colorNumber : int, a : int, b : int) =
    if a + 1 >= !lineSize then
	(* You don't want to exclude the last from anything uppon it *)
	(* There's nohting! Nohting! *)
	s
    else
	let 
	    val c : Clause =  newClause([getPositionVariable(colorNumber, a), getPositionVariable(colorNumber, b)], [ ~1, ~1])
	    val s : Set = addClauseToSet(s, c)
	in
	    if b + 1 >= !lineSize then
		onlyOnceInColor(s, colorNumber, a + 1, a + 2)
	    else
		onlyOnceInColor(s, colorNumber, a, b + 1)
	end
;

fun onlyOnce(s : Set, colorNumber : int) : Set =
    if colorNumber > !numberOfColor then
	s
    else
	onlyOnce(onlyOnceInColor(s, colorNumber, 0, 1), colorNumber + 1)
;

fun addNotPresentOrPos(s : Set, cVar : ColorVariable, pos : int) : Set =
    if pos = !lineSize then
	s
    else
	let val c : Clause = newClause([getPresenceVariable(#1(cVar)), getPositionVariable(#1(cVar), pos)], [1, ~1])
	    val newS : Set = addClauseToSet(s, c)
	in
	    addNotPresentOrPos(newS, cVar, pos + 1)
	end
;

(* Compute the constraint present(a) <=> p1(a) or p2(a) or ... or pn(a) *)
fun presenceImplication(s : Set, number : int) : Set =
    if number > !numberOfColor then
	s	
    else
	(* define list *)
	let val posList : Variable list = map (fn x => (getPositionVariable(number, x))) (List.tabulate(!lineSize, fn x => x))
	    val varList : Variable list = getPresenceVariable(number)::posList
	    val c1 : Clause = newClause(varList, ~1::(List.tabulate(!lineSize, fn x => 1)))
	    val s : Set = addClauseToSet(s, c1)
	    val s : Set = addNotPresentOrPos(s, getColorVariable(number), 0)
	in
	    presenceImplication(s, number + 1)
	end
;

(* Add all the n-uplet as a negative clause *)
fun atMostOnePerPositionClauses(s : Set, nUplet : int list list, pos : int) : Set =
    if length(nUplet) = 0 then
	s
    else
	let val varList : Variable list = map (fn x : int => (getPositionVariable(x, pos))) (hd(nUplet))
	    val signList : int list = List.tabulate(length(hd(nUplet)), fn x => ~1)
	    val s : Set = addClauseToSet(s, newClause(varList, signList))
	in
	    atMostOnePerPositionClauses(s, tl(nUplet), pos)
	end
;

(* On every group of 2 variable of the same pos, one is not in the solution *)
fun atMostOnePerPosition(s : Set, pos : int) : Set =
    if pos >= !lineSize then
	s
    else
	let val l : int list list = generateAllNUplet(1, 2, !numberOfColor)
	in
	    atMostOnePerPosition(atMostOnePerPositionClauses(s, l, pos), pos +1)
	end
	
;

(* For each pos, one color must be present*)
fun atLeastOnePerPosition(s : Set, pos : int) : Set =
    if pos >= !lineSize then
	s
    else
	let val c : Clause = newClause(List.tabulate(!numberOfColor, (fn x => getPositionVariable(x + 1, pos))),
				       List.tabulate(!numberOfColor, (fn x => 1)))
	in
	    atLeastOnePerPosition(addClauseToSet(s, c), pos + 1)
	end
;



(**** Display function ****)

fun displayPresence(step : int) =
    if step = length(!colorVariableDictionnary) then
	print("")
    else
	(
	 print(Int.toString(step + 1) ^ " : " ^ Int.toString(#1(getPresenceVariable(step + 1))) ^ " = " ^ 
	       BoolValue2String(#2(getPresenceVariable(step + 1))) ^ ".\n");
	 print("Position variables are the following:\n");
	 displayVariableList(List.tabulate(!lineSize, fn x => getPositionVariable(step + 1, x)));
	 displayPresence(step + 1)
	)
;

(* Computes the basic set for the 1 hole problem *)
fun computeClauses() =
    let	val s : Set = newSet()
	val s : Set = addColorVariable(s, 0)

	(* Totally useless *)
	(* val presence : Clause = newClause(listOfPresenceVariable(0), List.tabulate(!numberOfColor, fn x => 1)) *)
	(* val s : Set = addClauseToSet(s, presence) *)

	(* On every group of !lineSize + 1 variables, at leat one is not in the solution *)
	(* For each u = (!lineSize+1)-Uplet, add the clause not(conjonction(u)) *)
	(* val t = print("Before excluding peers.\n") *)
	(* val s : Set = excludingPeers(s) *)

	(* On every group of !numberOfColor - !lineSize + 1 variables, at leat one is in the solution *)
	(* For each u = (!numberOfColor - !lineSize+1)-Uplet, add the clause (conjonction(u)) *)
	(* val t = print("Before including peers.\n") *)
	(* val s : Set = includingPeers(s) *)

	(* Computes the meaning of the presence variable *)
	(* For each color, presence(color <=> (pos1(a) or pos2(a) or ... or posn(a)) *)
	(* val t = print("Before presence implications.\n") *)
	val s : Set = presenceImplication(s, 1)

	(* Computes the fact that a color can only appears once in the solution *)
	(* For each color, not(pos1(a) and pos2(a)) and not(pos1(a) and pos3(a))...and not(posn-1(a) and posn(a))*)
	(* val t = print("Before only once.\n") *)
	val s : Set = onlyOnce(s, 1)

	(* Computes the fact that you have to place no more than one color per position *)
	(* For each position, for each couple (a,b) , not(pos(a) and pos(b)) *)
	(* val t = print("Before at most one.\n") *)
	val s : Set = atMostOnePerPosition(s, 0)

	(* Computes the fact that you have to place a color to a position *)
	(* For each position, pos(a) or pos(b) or ... or pos(lastColor) *)
	(* val t = print("Before at least one.\n") *)
	val s : Set = atLeastOnePerPosition(s, 0)
    in
	s
    end
;

fun computeClauses1Hole3Colors() =
    (
     lineSize := 1;
     numberOfColor := 3;
     let
	 val s : Set = newSet()
	 val s : Set = addColorVariable(s, 0)
	 val C1 : Variable = getPresenceVariable(0)
	 val C2 : Variable = getPresenceVariable(1)
	 val C3 : Variable = getPresenceVariable(2)
	 val s : Set = addClauseToSet(s, newClause([C1, C2, C3], [1, 1, 1]))
	 val s : Set = addClauseToSet(s, newClause([C1, C2], [~1, ~1]))
	 val s : Set = addClauseToSet(s, newClause([C1, C3], [~1, ~1]))
	 val s : Set = addClauseToSet(s, newClause([C2, C3], [~1, ~1]))
	 val tmp = displaySet(s)
     in
	 s
     end
    )
;


(* Choose your probelm type *)
fun computeBasicSet() =
    computeClauses();
;

(* Build a set  *)
val logicalSet : Set ref = ref(computeBasicSet());

fun naivCellResolution(s : Set, wp : int, bd : int, others : int, coupleColorPosition : int list list) : Set =
    let val signList : int list = List.tabulate(!lineSize, fn x => ~1)
	val varList : Variable list = (map (fn x : int list => getPositionVariable(hd(x), hd(tl(x)))) coupleColorPosition)
    in
	addClauseToSet(s, newClause(varList, signList))
    end
;

(* The result is under the following form: *)
(* [[[color list],[sign list]] list]*)
fun deductFromOthers(others : int, coupleColorPosition : int list list ) : int list list list =
    (* Quel est le réel arrêt ? *)
    if length(coupleColorPosition) = 0 then
	(
	 [[[], []]]
	)
    else
	let val firstColorVar : int = List.nth(hd(coupleColorPosition), 0)
	    val var : int = #1(getPresenceVariable(firstColorVar))
	    val recSolutionNotPresent = deductFromOthers(others -1, tl(coupleColorPosition))
	    val recSolutionPresent = deductFromOthers(others, tl(coupleColorPosition))
	    (* Place the variable in its position *)
	    val NotPresent = concatToInsideList(recSolutionNotPresent, var, 0)
	    (* Place the list in its position *)
	    val NotPresent = concatToInsideList(NotPresent, ~1, 1)
	    (* Place the variable in its position *)
	    val Present = concatToInsideList(recSolutionPresent, var, 0)
	    (* Place the list in its position *)
	    val Present = concatToInsideList(Present, 1, 1)
	in
	     if others = 0 then
		  Present
	     else 
		 if others = length(coupleColorPosition) then
		      NotPresent
		 else
		      List.concat([NotPresent, Present])
	end
;


(* The result is under the following form: *)
(* [[[color list],[sign list]] list]*)
fun deductFromWellPlaced(wp : int, coupleColorPosition : int list list ) : int list list list =
    (* Quel est le réel arrêt ? *)
    if length(coupleColorPosition) = 0 then
	(
	 [[[], []]]
	)
    else
	let val firstColorVar : int = List.nth(hd(coupleColorPosition), 0)
	    val pos : int = List.nth(hd(coupleColorPosition), 1)
	    val var : int = #1(getPositionVariable(firstColorVar, pos))
	    val recSolutionPresent = deductFromWellPlaced(wp-1, tl(coupleColorPosition))
	    val recSolutionNotPresent = deductFromWellPlaced(wp, tl(coupleColorPosition))
	    (* Place the variable in its position *)
	    val NotPresent = concatToInsideList(recSolutionNotPresent, var, 0)
	    (* Place the list in its position *)
	    val NotPresent = concatToInsideList(NotPresent, ~1, 1)
	    (* Place the variable in its position *)
	    val Present = concatToInsideList(recSolutionPresent, var, 0)
	    (* Place the list in its position *)
	    val Present = concatToInsideList(Present, 1, 1)
	in
	     if wp = 0 then
		  NotPresent
	     else 
		 if wp = length(coupleColorPosition) then
		      Present
		 else
		      List.concat([NotPresent, Present])
	end
;

fun getClauseListFromOthers(s : Set, others : int, coupleColorPosition : int list list) : Set =
    let val refS : Set ref = ref(newSet()) 
	val tmp = app (fn c => refS := addClauseToSet(!refS, newClauseFromInt(List.nth(c, 0), List.nth(c, 1))))
	     (deductFromOthers(others, coupleColorPosition));
	(* val t = print("Before transformation.\n") *)
	val cl : Clause list = cnfList2dnfList(#1(!refS))
    in
	(
	 addListOfClauseToSet(s, cl)
	 (* (#1(s)@cl, #2(s)) *)
	)
    end	 
;

fun getClauseListFromWellPlaced(s : Set, wp : int, coupleColorPosition : int list list) : Set =
    let val refS : Set ref = ref(newSet()) 
	val tmp = app (fn c => refS := addClauseToSet(!refS, newClauseFromInt(List.nth(c, 0), List.nth(c, 1))))
	     (deductFromWellPlaced(wp, coupleColorPosition));
	(* val t = print("Before transformation.\n") *)
	val cl : Clause list = cnfList2dnfList(#1(!refS))
    in
	(
	 addListOfClauseToSet(s, cl)
	 (* (#1(s)@cl, #2(s)) *)
	)
    end	 
;


(* app (fn c => refS := addClauseToSet(!refS, newClauseFromInt(List.nth(c, 0), List.nth(c, 1)))) *)
(*     (deductFromWellPlaced(wp, bd, others, coupleColorPosition)); *)

fun listeningResolution(s : Set, wp : int, bd : int, others : int, coupleColorPosition : int list list) =
    let 
	(* val t = print("Before from others.\n") *)
	val s : Set = getClauseListFromOthers(s, others, coupleColorPosition)	
	(* val t = print("After from others, before well placed.\n") *)
	val s : Set = getClauseListFromWellPlaced(s, wp, coupleColorPosition)
	(* val t = print("After well placed.\n") *)
    in 
	s
    end
;

(* Compute the Clause for the last Line *)
fun computeLineClauses(g : Grid, s : Set) =
    if length(#1(g)) = 0 then
	s
    else
	let val lastLine : Line = List.nth(#1(g), length(#1(g))-1)
	    val lastAttempt = #1(lastLine)
	    val result = #2(lastLine)
    	    val wp = #3(result)
    	    val bd = #2(result)
    	    val others = #1(result)
	    val coupleColorPosition : int list list = List.tabulate(length(lastAttempt), 
								    (fn x => [List.nth(lastAttempt, x), x]))
	    val s : Set = listeningResolution(s, wp, bd, others, coupleColorPosition)
	    val s : Set = naivCellResolution(s, wp, bd, others, coupleColorPosition)
	in
	    s
	end
;

(**** Problem to Game ****)


fun isTruePosVariable(color : ColorVariable, pos : int) : bool =
    let val colorNumber : int = #1(color)
	val posVariable : Variable = getPositionVariable(colorNumber, pos)
	val b : boolValue = #2(posVariable)
    in
	isTrue(b)
    end
;

fun getVariableAtPos(dic : ColorVariable list, pos : int) : int =
    let val listOfTrue : ColorVariable list = (List.filter (fn c => (isTruePosVariable(c, pos))) dic)
	val varList : Variable list  = (map (fn x => !(#2(x))) listOfTrue)
    	val varList : Variable list  = (map (fn x => !(#2(x))) listOfTrue)
    in
    	#1(List.nth((listOfTrue, 0)))
    end
;

fun getPresenceColor(dic : ColorVariable list) =
    List.tabulate(!lineSize, fn pos : int => getVariableAtPos(dic, pos))
;

(* Assign the variable into the color dictionnary *)
fun assignVariable(vList : Variable list, color : int, pos : int) =
    case vList of
	h::t =>
	if pos = 0 then
	    (
	     #2(List.nth(!colorVariableDictionnary, color - 1)) := h;
	      assignVariable(t, color, 1)
	    )
	else 
	    if pos = !lineSize + 1 then
		assignVariable(h::t, color + 1, 0)
	     else
		 (
		  List.nth(#3(List.nth(!colorVariableDictionnary, color - 1)), pos-1) := h;
		  assignVariable(t, color, pos + 1)
		 )
      | nil => print("")
;


(* Game to Problem to Game *)
fun chooseLine(g : Grid) : Color list =
    (
     (* print("Before compute line.\n"); *)
     logicalSet := computeLineClauses(g, !logicalSet);
     logicalSet := (filterIncludedClause(#1(!logicalSet), 0, 0), #2(!logicalSet));
     print("\n\n****** START DP ******\n\n");
     (* displaySet(!logicalSet); *)
     let val r = dp(!logicalSet, 0)
     val tmp = 	 print("\n\n******* END DP with complexity = " ^ Int.toString(!complexity) ^ " *******\n\n");
     in
	 if not(#2(r)) then
	     (* if false then *)
	     (
	      print("Error unsolvable system.\n");
	      exit()
	     )
	 else
	     (
	      let val s : Set = (#1(!logicalSet), #1(r))
	      in
		  assignVariable(#1(r), 1, 0);
		  (* displayVariableList(#1(r)); *)
		  getPresenceColor(!colorVariableDictionnary)
	      end
	     )
     end
    )
;
