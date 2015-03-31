use "utils.sml";
use "variable.sml";
use "clause.sml";
use "set.sml";
use "dp.sml";
  
(* val a = tbis([[0]]); *)
(* val a2 = tbis([[0, 1]]); *)
(* val a3 = tbis([[0, 1], [0, 1], [1, 1]]); *)
(* val a4 = tbis([[0, 1], [0, 0], [1, 1]]); *)
(* val a5 = tbis([[], [0, ~25]]) *)


val v1 : Variable = newVariable();
val v2 : Variable = newVariable();
val v3 : Variable = newVariable();
(* val v4 : Variable = newVariable(); *)
(* val v5 : Variable = newVariable(); *)
(* val v6 : Variable = newVariable(); *)

val c1 = newClause([v1, v2, v3], [~1, ~1, ~1]);
val c2 = newClause([v2], [~1]);
(* (\* val c1 = (nil); *\) *)
(* (\* val c1 = addVariableToClause(c1, v1, true); *\) *)
(* (\* val c1 = addVariableToClause(c1, v2, false); *\) *)

(* val c2 = newClause([v1, v3], [~1, 1]); *)
(* (\* val c2 = (nil); *\) *)
(* (\* val c2 = addVariableToClause(c2, v1, false); *\) *)
(* (\* val c2 = addVariableToClause(c2, v3, true); *\) *)

(* val c3 = newClause([v1, v2], [~1, 1]); *)
(* (\* val c3 = (nil); *\) *)
(* (\* val c3 = addVariableToClause(c3, v1, false); *\) *)
(* (\* val c3 = addVariableToClause(c3, v2, true); *\) *)

(* val c4 = newClause([v1, v3], [~1, ~1]); *)
(* (\* val c4 = (nil); *\) *)
(* (\* val c4 = addVariableToClause(c4, v1, false); *\) *)
(* (\* val c4 = addVariableToClause(c4, v3, false); *\) *)

(* val c5 = newClause([v2, v3], [1, 1]); *)
(* (\* val c5 = (nil); *\) *)
(* (\* val c5 = addVariableToClause(c5, v2, true); *\) *)
(* (\* val c5 = addVariableToClause(c5, v3, true); *\) *)

(* val c6 = newClause([v4], [1]); *)
(* (\* val c6 = (nil); *\) *)
(* (\* val c6 = addVariableToClause(c6, v4, true); *\) *)

(* val c7 = newClause([v5], [1]); *)
(* (\* val c7 = (nil); *\) *)
(* (\* val c7 = addVariableToClause(c7, v5, true); *\) *)

(* val c8 = newClause([v6], [1]); *)
(* (\* val c8 = (nil); *\) *)
(* (\* val c8 = addVariableToClause(c8, v6, true); *\) *)

val s1 = (nil, nil);
val s1 = addVariableToSet(s1, v1);
val s1 = addVariableToSet(s1, v2);
val s1 = addVariableToSet(s1, v3);
(* val s1 = addVariableToSet(s1, v4); *)
(* val s1 = addVariableToSet(s1, v5); *)
(* val s1 = addVariableToSet(s1, v6); *)

val s1 = addClausetoSet(s1, c1);
val s1 = addClausetoSet(s1, c2);
(* val s1 = addClausetoSet(s1, c3); *)
(* val s1 = addClausetoSet(s1, c4); *)
(* val s1 = addClausetoSet(s1, c5); *)
(* val s1 = addClausetoSet(s1, c6); *)
(* val s1 = addClausetoSet(s1, c7); *)
(* val s1 = addClausetoSet(s1, c8); *)

val t = displaySet(s1);
val r = dp(s1, 0);
val tmp = print("Complexity of " ^ Int.toString(!complexity) ^"\n");
if #2(r) then
    (
     displayVariableList(#1(r));
     print("Won\n")
    )
else
    (
     displayVariableList(#1(r));
     print("Lost\n")
    )
;
