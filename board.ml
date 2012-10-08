(* file: board.ml *)

(* A square can contain a white piece, black piece, or neither. *)
type side_t = White | Black | Neither

(* A board is two lists of pieces, the first black and the second white. *)
type board_t = (int list * int list)

(* Get the pieces on the board by which color side we want. *)
let pieces (board : board_t) (side : side_t) : int list =
  if side = Black then fst board else snd board

(* Get the color of a piece *)
let piece_color (board : board_t) (index : int) : side_t =
  if List.exists (fun x -> x = index) (pieces board Black) then Black else
  if List.exists (fun x -> x = index) (pieces board White) then White else
  Neither

(* Remove a piece from the board. *)
let remove_piece (board : board_t) (index : int) : board_t =
  (List.filter (fun x -> x != index) (fst board),
   List.filter (fun x -> x != index) (snd board))

(* Add a piece to the board. *)
let add_piece (board : board_t) (index : int) (side : side_t) : board_t =
  let new_side = (List.sort compare (index :: (pieces board side))) in
  if (side = Black) then
    (new_side, snd board)
  else
    (fst board, new_side)

(* Board constants. *)
let numRows : int = 8
let numColumns : int = 8
let boardSize : int = numRows * numColumns

(* Board index to (x,y) coordinate. *)
let index_to_xy (index : int) : (int * int) = (index mod numColumns, index / numColumns)

(* Initialize the pieces on the board. *)
let rec init_piece_list
    (current : int) (n : int) (inc : int) : int list =
  if (n = 0) then []
  else
    let (x, y) = index_to_xy current in
    if (((current + (y mod 2)) mod 2) != 0) then
      current :: (init_piece_list (current + inc) (n-1) inc)
    else
      init_piece_list (current + inc) n inc

let init_pieces () : board_t =
  let piecesPerSide = ((numRows / 2) - 1) * (numColumns / 2) in
  (init_piece_list 0 piecesPerSide 1,
   List.rev (init_piece_list (numRows * numColumns - 1) piecesPerSide (-1)))

(* The other side. *)
let swap_side (side : side_t) : side_t =
  if (side = Black) then White else Black

(* Flip the index if playing down the board. *)
let flip_index (index : int) : int =
  let (x, y) = index_to_xy index in
  let flipped_y = numRows - y - 1 in
  (flipped_y * numColumns + x)

(* And don't flip it otherwise. *)
let identity (x : 'a) : 'a = x

(* Flip function selector. *)
let flipfn (side : side_t) : int -> int =
  if (side = Black) then identity else flip_index

(* Single possible move from a square either left or right, or none. *)
let possible_moves (left : bool) (index : int) : int list =
  let (x, y) = index_to_xy index in
  if (y != numRows - 1) then
    if (left && x > 0) then
      [(y+1) * numColumns + x - 1]
    else if (not left && x < numColumns - 1) then
      [(y+1) * numColumns + x + 1]
    else []
  else []

(* Single possible jump from a square either left or right, or none. *)
let possible_jumps (left : bool) (index : int) : int list =
  let captures = possible_moves left index in
  if (captures == []) then []
  else possible_moves left (List.hd captures)

(* Compute possible moves from each square. *)
let init_moves_aux
    (table : ('a, 'b list) Hashtbl.t)
    (fn : bool -> int -> 'b list)
    : ('a, 'b list) Hashtbl.t =
  for src = 0 to (boardSize - 1) do
    let existing_moves =
      try
        Hashtbl.find table src
      with Not_found -> [] in
    let dest = List.append (fn true src) (fn false src) in
    Hashtbl.replace table src (List.append dest existing_moves)
  done;
  table

(* Possible moves from each square. *)
let moves : (int, int list) Hashtbl.t =
  init_moves_aux
    (init_moves_aux (Hashtbl.create boardSize) possible_moves)
    possible_jumps

(* Is a move a jump? *)
let is_jump (src : int) (dest : int) : bool = abs(src - dest) > numColumns + 1

(* Is a jump valid? The pieces at src and capture points must be opposite colors,
   and the destination must be empty. The capture point is halfway between src and dest. *)
let is_valid_jump (board : board_t) (src : int) (dest : int) : bool =
  let dest_empty = ((piece_color board dest) = Neither) in
  let src_side = piece_color board src in
  let capture_side = piece_color board ((src + dest) / 2) in
  dest_empty && (capture_side != Neither) && (src_side != capture_side)

(* Is a move valid? Either it's a valid jump, or the destination must be empty. *)
let is_valid_move (board : board_t) (src : int) (dest : int) : bool =
  if is_jump src dest
  then is_valid_jump board src dest
  else piece_color board dest = Neither

(* Get the possible valid destination squares for a move or jump. *)
let find_destinations_for_move
    (flip : int -> int)
    (move_table : (int, int list) Hashtbl.t)
    (valid_fn : int -> int -> bool)
    (index : int) : int list =
  let flipped_moves = List.map flip (Hashtbl.find move_table (flip index)) in
  List.filter (valid_fn index) flipped_moves

(* Find valid moves for a piece. *)
let find_moves
    (board : board_t)
    (src : int) : int list =
  let side = piece_color board src in
  find_destinations_for_move (flipfn side) moves (is_valid_move board) src

(* Find valid jumps for a piece. *)
let find_jumps
    (board : board_t)
    (src : int) : int list =
  List.filter (is_jump src) (find_moves board src)

(* Does a piece have a jump available? *)
let has_jump (board : board_t) (src : int) : bool = find_jumps board src != []

(* Does a piece have a move available? *)
let has_move (board : board_t) (src : int) : bool = find_moves board src != []

(* Find all valid selectable squares (ie. sources of allowed moves for a side).
   Jumps only if any jumps exist. *)
let find_selectable_squares (board : board_t) (side : side_t) : int list =
  let ps = pieces board side in
  let js = List.filter (has_jump board) ps in
  if (js != []) then js
  else List.filter (has_move board) ps

(* Find valid destination squares for a piece.
   (Jumps if they exist, otherwise regular moves.) *)
let find_destinations
    (board : board_t)
    (src : int) : int list =
  let m = find_moves board src in
  let possible_jumps = List.filter (is_jump src) m in
  if (possible_jumps != []) then possible_jumps
  else m

(* Move a piece from src to dest. Capture if necessary.
   Return a new board and a list of squares to be redrawn. *)
let move_piece
    (board : board_t)
    (src : int) (dest : int) : (board_t * int list) =
  let src_side = piece_color board src in
  let b1 = remove_piece board src in
  let b2 = add_piece b1 dest src_side in
  if (is_jump src dest) then (
    let capture = (src + dest) / 2 in
    (remove_piece b2 capture, [src; capture; dest]))
  else
    (b2, [src; dest])

