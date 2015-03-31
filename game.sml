(* Defines the representation of the colors *)
type Color = int
(* Representation of a result *)
(* The first int represents the number of colors that are in the the line, that are not in the solution *)
(* The second, the number of colors that are in the line but wrongly placed *)
(* The third is the numuber of colors that are well-placed in the line *)
type Result = int * int * int
(* Represents a line. The Color list is the attempt from the player, and the result is in the second part *)
type Line = Color list * Result
(* The grid is only a list of Line and the second part represents the solution *)
type Grid = Line list * Color list

(* !lineSize is the number of color that you can put in a line *)
val lineSize = ref(8)
(* !numberOfColor is the total number of different color *)
val numberOfColor = ref(12)

fun randomColor() =
    Random.randInt(alea) mod !numberOfColor + 1
;

fun genCorrectLine(l : int list) =
    if length(l) = !lineSize then
	l
    else
	let val c = randomColor()
	in
	    if length(List.filter (fn x => c = x) l) = 0 then
		genCorrectLine(c::l)
	    else
		genCorrectLine(l)
	end
;

(* Generate a solution for the game *)
fun genSol() : int list =
    genCorrectLine(nil)
;

(* Generate a new, empty Line *)
fun genEmptyLine() : Line =
    (nil, (0, 0, 0))
;

(* Generate a new Grid *)
fun genGrid() : Grid =
    (nil, genSol())
;

(* Generate a random attempt. Useful for the first try *)
fun randomAttempt() : Color list =
    List.tabulate(!lineSize, fn x => x)
;

(* Returns the number of well-placed color *)
fun computeWellPlaced(l : Color list, s : Color list) : int =
    case (l, s) of
	(hl::tl, hs::ts) => if hl = hs then
				1 + computeWellPlaced(tl, ts)
			    else
				computeWellPlaced(tl, ts)
      | (nil, nil) => 0
      | (nil, _) => (print("\n\n Error \n\n") ; 0)
      | (_, nil) => (print("\n\n Error \n\n") ; 0)
;

(* Return the number of Color that are in the solution *)
fun computePresent(l : Color list, s : Color list) : int =
    case l of 
	(h::t) => length(filter(fn x => h = x, s)) + computePresent(t, s)
      | nil => 0
	       

(* Returns the number of badly-placed color *)
fun computeBadPlaced(l : Color list, s : Color list) : int =
    computePresent(l, s) - computeWellPlaced(l, s)
;

(* Compute result on the last line *)
fun computeResult(l : Color list, s : Color list) =
    let val wp = computeWellPlaced(l, s)
	val bp = computeBadPlaced(l, s)
	val others = !lineSize - wp - bp
    in
	(others, bp, wp)
    end
;

(* Add an add by giving an attempt *)
fun addLine(g : Grid, l : Color list) : Grid =
    (#1(g)@[(l, computeResult(l, #2(g)))], #2(g))
;

(* Returns true when the player has won, false otherwise *)
fun won(g : Grid) : bool =
    if length(#1(g)) > 0 then
	let val lastResult = #2(List.nth(#1(g), length(#1(g)) - 1))
	in
	    #3(lastResult) = !lineSize
	end
    else
	false
      ;

(**** Display functions ****)

fun color2string(c : Color) : string =
    "C" ^ Int.toString(c)
;

fun displayColors(cl : Color list) =
    case cl of
	h::t => (print(color2string(h) ^ " "); displayColors(t))
      | nil => print("")
;

fun displayResult(r : Result) =
    print(" : " ^ Int.toString(#1(r)) ^ " - " ^ Int.toString(#2(r)) ^ " - " ^ Int.toString(#3(r)))
;

fun displayLine(l : Line) =
    (displayColors(#1(l)); displayResult(#2(l)))
;

fun displayLines(llist : Line list) =
    case llist of
	h::t => (displayLine(h); print("\n"); displayLines(t))
      | nil => print("")
;

fun displayGrid(g : Grid) =
    (
     print("\nGrid \n");
     displayLines(#1(g));
     print("*** solution ***\n");
     displayColors(#2(g));
     print("\n")
    )
;
