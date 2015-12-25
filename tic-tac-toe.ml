#directory "+extlib";;
#load "extLib.cma";;

open ExtLib;;

type cell = X | O | Empty;;
type player = Me | Opponent;;
type game_end = Lose | Draw | Win;;
type board = cell list;;

let make_move b plr i =
  List.take i b @ [plr] @ List.drop (i + 1) b;;

let get_boards b plr =
  let get_empty_cells b =
    let rec empty_cells b i =
      match b with
      | [] -> []
      | h :: t -> if h = Empty then i :: empty_cells t (i + 1)
                  else empty_cells t (i + 1) in
    empty_cells b 0 in
  List.map (make_move b plr) @@ get_empty_cells b;;

let get_symbol plr =
  match plr with
  | Me       -> X
  | Opponent -> O;;

let get_opponent plr =
  match plr with
  | Me       -> Opponent
  | Opponent -> Me;;

let final_rows [a1; a2; a3;
                b1; b2; b3;
                c1; c2; c3] =
  [[a1; a2; a3];
   [b1; b2; b3];
   [c1; c2; c3];

   [a1; b1; c1];
   [a2; b2; c2];
   [a3; b3; c3];

   [a1; b2; c3];
   [c1; b2; a3]];;

let is_win b plr =
  let plr_sym = get_symbol plr in
  let win_row = [plr_sym; plr_sym; plr_sym] in
  List.mem win_row @@ final_rows b;;

let is_lose b plr =
  let opp_sym = get_symbol @@ get_opponent plr in
  let lose_row = [opp_sym; opp_sym; opp_sym] in
  List.mem lose_row @@ final_rows b;;

let is_draw b plr =
  let plr_sym = get_symbol plr in
  let opp_sym = get_symbol @@ get_opponent plr in
  List.for_all (fun r -> (List.mem plr_sym r) && (List.mem opp_sym r))
    @@ final_rows b;;

let minimum l = List.hd @@ List.sort l;;
let maximum l = List.hd @@ List.rev @@ List.sort l;;

let rec get_score plr_to_move b =
  let plr = Me in
  if is_win b plr then Win
  else if is_lose b plr then Lose
  else if is_draw b plr then Draw
  else
    let bs = get_boards b @@ get_symbol plr_to_move in
    let scores = List.map (get_score @@ get_opponent plr_to_move) bs in
    if plr_to_move = plr then maximum scores
    else minimum scores;;

let string_of_cell c =
  match c with
  | X     -> "X"
  | O     -> "O"
  | Empty -> " ";;

let print_board [a1; a2; a3;
                 b1; b2; b3;
                 c1; c2; c3] =
  Printf.printf "+---+---+---+\n";
  Printf.printf "| %s | %s | %s |\n" (string_of_cell a1) (string_of_cell a2) (string_of_cell a3);
  Printf.printf "+---+---+---+\n";
  Printf.printf "| %s | %s | %s |\n" (string_of_cell b1) (string_of_cell b2) (string_of_cell b3);
  Printf.printf "+---+---+---+\n";
  Printf.printf "| %s | %s | %s |\n" (string_of_cell c1) (string_of_cell c2) (string_of_cell c3);
  Printf.printf "+---+---+---+\n";;

let best_move b plr =
  let scores = List.map (fun b -> (get_score (get_opponent plr) b, b))
                    (get_boards b @@ get_symbol plr) in
  let m = fst @@ if plr = Me then maximum scores else minimum scores in
  List.assoc m scores;;

let rec play b plr =
  print_board b;
  if not (List.mem Empty b) then ()
  else
    let nb = best_move b plr in
    play nb @@ get_opponent plr;;

play [Empty; Empty; Empty;
      Empty; Empty; Empty;
      Empty; Empty; Empty] Me;

