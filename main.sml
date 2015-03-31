use "utils.sml";
use "variable.sml";
use "clause.sml";
use "set.sml";
use "dp.sml";

use "game.sml";
use "gameResolution.sml";

val nbTry : int ref = ref(0)	
fun play(g) =
    (
     (* print("play.\n"); *)
     displayGrid(g);
     if won(g) then
	 (
	  (* displayGrid(g); *)
	  print("\nWon!\n");
	  print("Last complexity = " ^ Int.toString(!complexity) ^ ".\n");
	  print("Number of try = " ^ Int.toString(!nbTry) ^ ".\n");
	  exit()
	 )
     else
	 let val attempt : Color list = chooseLine(g)
	 in
	     (
	      nbTry := !nbTry + 1;
	      play(addLine(g, attempt))
	     )
	 end
    )
;

val g : Grid = genGrid();
play(g)



