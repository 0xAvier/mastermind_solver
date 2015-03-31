type Couple = Variable list * bool;

val complexity = ref(0)
(* DP function *)
fun dp(s : Set, d : int) =
    (
     if d = 0 then
	 complexity := 1
     else
	 complexity := !complexity + 1
   ;
   (* print("Deepth : " ^ Int.toString(d) ^ "\n"); *)
   (* displaySet(s); *)
   let val s = removeEmptyClauseFromSet(s)
       val s = affectUnitaryVariable(s)
       val s = removeEmptyClauseFromSet(s)
   in
       case s of
	   (nil, vList) => (vList, true)
	 | (cList, vList) =>
	   (
	    if allVariableAffected(s) orelse falseClauseExist(s) then
		(
		 (vList, false)
		)
	    else
		let val var = getVarToAffect(vList)
		    val st = affectVar(s, var, DEFINED(true))
		    val sf = affectVar(s, var, DEFINED(false))
		    val rt = dp(st, d + 1)
		in
		    if #2(rt) then
			rt
		    else
			dp(sf, d + 1)
		end
	   )
   end
    )
;
