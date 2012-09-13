(* file: checkers.ml *)
(* compile with: $ ocamlc -I +lablgtk2 lablgtk.cma gtkInit.cmo checkers.ml -o checkers *)

(* 8x8 board *)
let numRows : int = 8;;
let numColumns : int = 8;;
let boardSize : int = numRows * numColumns;;

(* Board colors. *)
let darkSquareColor : GDraw.color = (`NAME "dim gray");;
let lightSquareColor : GDraw.color = (`NAME "light gray");;
let selectableColor : GDraw.color = (`NAME "yellow");;
let selectedColor : GDraw.color = (`NAME "red");;
let destinationColor : GDraw.color = (`NAME "green");;

(* Draw a piece at 80% of the square width. *)
let pieceWidth : float = 0.8;;

(* A square can contain a white piece, black piece, or neither,
   and has a click function. *)
type squareContent = White | Black | Neither;;
type square = { content : squareContent;
                click_fn : int -> unit};;

(* The other side. *)
let swap_side (side : squareContent) : squareContent =
  if (side == Black) then White else Black

(* Null click function: for squares that aren't doing anything. *)
let null_click _ = ()

(* The board. *)
let board : square array = Array.make boardSize
    {content = Neither; click_fn = null_click};;

(* Board index to (x,y) coordinate. *)
let index_to_xy (index : int) : (int * int) = (index mod numColumns, index / numColumns)

(* Flip the index if playing down the board. *)
let flip_index (index : int) : int =
  let (x, y) = index_to_xy index in
  let flipped_y = numRows - y - 1 in
  (flipped_y * numColumns + x)

(* And don't flip it otherwise. *)
let identity (x : 'a) : 'a = x

(* Initialize the pieces on the board. *)
let init_pieces () : unit =
  let rowsPerSide = (numRows / 2) - 1 in
  for index = 0 to (boardSize - 1) do
    let (x, y) = index_to_xy index in
    if ((index + (y mod 2)) mod 2 != 0) then
      let setfn = Array.set board index in
      let s = {content = Neither; click_fn = null_click} in
      if y < rowsPerSide then
        setfn {s with content = White}
      else if y >= numRows - rowsPerSide then
        setfn {s with content = Black}
      else
        setfn s
  done

(* Possible moves. *)
let moves : (int, int list) Hashtbl.t = Hashtbl.create boardSize;;
let jumps : (int, int list) Hashtbl.t = Hashtbl.create boardSize;;

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
let init_moves_aux (table : ('a, 'b list) Hashtbl.t) (fn : bool -> int -> 'b list) : unit =
  for src = 0 to (boardSize - 1) do
    let dest = List.append (fn true src) (fn false src) in
    Hashtbl.replace table src dest
  done

(* Compute possible moves from each square. *)
let init_moves : unit = init_moves_aux moves possible_moves
let init_jumps : unit = init_moves_aux jumps possible_jumps

(* Is a move valid? The destination must be empty. *)
let is_valid_move (_ : int) (dest : int) : bool =
  let destContent = (Array.get board dest).content in
  destContent = Neither

(* Is a jump valid? The pieces at src and capture points must be opposite colors,
   and the destination must be empty. The capture point is halfway between src and dest. *)
let is_valid_jump (src : int) (dest : int) : bool =
  let srcContent = (Array.get board src).content in
  let captureContent = (Array.get board ((src + dest) / 2)).content in
  let destContent = (Array.get board dest).content in
  destContent = Neither &&
  ((srcContent = White && captureContent = Black) ||
  (srcContent = Black && captureContent = White))

(* Is a move a jump? *)
let is_jump (src : int) (dest : int) : bool = abs(src - dest) > numColumns + 1

(* Produce a list from i..j *)
let rec range (i : int) (j : int) : int list =
  if i >= j then [] else i :: (range (i+1) j)

(* Collect the valid moves for a given side. *)
let collect_moves
    (side : squareContent)
    (table : (int, int list) Hashtbl.t)
    (valid_fn : int -> int -> bool)
    (l : (int * int) list)
    (index, square) : (int * int) list =
  if (square.content != side) then l else
  let flipfn = if (side = White) then identity else flip_index in
  let flipped_moves = List.map flipfn (Hashtbl.find table (flipfn index)) in
  let possible_moves = (List.filter (valid_fn index) flipped_moves) in
  let m = List.map (fun x -> (index, x)) possible_moves in
  List.append l m

(* Find all valid moves for a side. Jumps only if any jumps exist. *)
let find_valid_moves (side : squareContent) : (int * int) list =
  (* First, zip the array with indices into a list. *)
  let squares = List.combine (range 0 boardSize) (Array.to_list board) in
  (* Now find the squares that have valid jumps for this side, if any. *)
  let jumpSquares = List.fold_left (collect_moves side jumps is_valid_jump) [] squares in
  if (jumpSquares != []) then jumpSquares
  else List.fold_left (collect_moves side moves is_valid_move) [] squares

(* Find valid jumps for a piece. *)
let find_valid_jumps (src : int) (flipfn : int -> int) : int list =
  let possible_jumps = Hashtbl.find jumps (flipfn src) in
  let flipped_jumps = List.map flipfn possible_jumps in
  List.filter (is_valid_jump src) flipped_jumps

(* Board dimensions : x origin, yorigin, square width, square height. *)
let board_dimensions (backing : GDraw.pixmap ref) : (int * int * int * int) =
  let (w, h) = !backing#size in
  let (xorg, yorg) = ((w mod numColumns) / 2, (h mod numRows) / 2) in
  let (squareWidth, squareHeight) = (w / numColumns, h / numRows) in
  (xorg, yorg, squareWidth, squareHeight)

(* Convert window (x,y) to board square (row,col). *)
let window_coord_to_board_coord
    (backing : GDraw.pixmap ref) (x : int) (y : int) : (int * int) =
  let (xorg, yorg, squareWidth, squareHeight) = board_dimensions backing in
  let (boardx, boardy) = ((x-xorg) / squareWidth, (y-yorg) / squareHeight) in
  ((max 0 (min (numColumns-1) boardx)), (max 0 (min (numRows-1) boardy)))

(* (x,y,w,h) of the window coordinates for a square *)
let window_coords
    (backing : GDraw.pixmap ref) (index : int) : (int * int * int * int) =
  let (xorg, yorg, squareWidth, squareHeight) = board_dimensions backing in
  let (boardx, boardy) = index_to_xy index in
  let (x, y) = (xorg + squareWidth * boardx, yorg + squareHeight * boardy) in
  (x, y, squareWidth, squareHeight)

(* Draw a highlight rectangle. *)
let draw_highlight (backing : GDraw.pixmap ref)
    (x : int) (y : int) (w : int) (h : int)
    (c : GDraw.color) : unit =
  !backing#set_line_attributes ~width:4 ();
  !backing#set_foreground c;
  !backing#rectangle ~x:(x+2) ~y:(y+2)
    ~width:(w-4) ~height:(h-4) ~filled:false ()

(* Highlight a square by index. *)
let highlight_square (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (c : GDraw.color) (index : int) =
  let (x, y, w, h) = window_coords backing index in
  draw_highlight backing x y w h c;
  let update_rect = Gdk.Rectangle.create x y w h in
  area#misc#draw (Some update_rect)

(* Draw a square and its piece, if any. *)
let draw_square (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (index : int) : unit =
  let (x, y, squareWidth, squareHeight) = window_coords backing index in
  let (boardx, boardy) = index_to_xy index in

  (* white/black squares *)
  if ((index + (boardy mod 2)) mod 2 != 0) then
    !backing#set_foreground darkSquareColor
  else
    !backing#set_foreground lightSquareColor;
  !backing#rectangle ~x ~y ~width:squareWidth ~height:squareHeight ~filled:true ();

  (* piece on the square *)
  if ((Array.get board index).content != Neither) then (
    let offset = int_of_float (float_of_int squareWidth *. (1.0 -. pieceWidth) /. 2.0) in
    if ((Array.get board index).content = White) then
      !backing#set_foreground `WHITE
    else if ((Array.get board index).content = Black) then
      !backing#set_foreground `BLACK;
    !backing#arc ~x:(x + offset) ~y:(y + offset)
      ~width:(int_of_float (float_of_int squareWidth *. pieceWidth))
      ~height:(int_of_float (float_of_int squareHeight *. pieceWidth))
      ~filled:true (););

  (* And update the drawing area *)
  let update_rect = Gdk.Rectangle.create x y squareWidth squareHeight in
  area#misc#draw (Some update_rect)

(* Draw the board *)
let draw_board (area : GMisc.drawing_area) (backing : GDraw.pixmap ref) : unit =
  for i = 0 to (boardSize - 1) do
    draw_square area backing i
  done;;

(* Move a piece from src to destination. Capture if necessary.
   Return a list of squares to be redrawn. *)
let move_piece (src : int) (dest : int) : int list =
  Array.set board dest (Array.get board src);
  Array.set board src {(Array.get board src) with content=Neither};
  if (is_jump src dest) then (
    let capture = (src + dest) / 2 in
    Array.set board capture {(Array.get board capture) with content=Neither};
    [src; capture; dest])
  else
    [src; dest]

(* Mark a square as a destination. *)
let rec mark_destination (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (m : (int * int) list)
    (select : bool) (src : int) (dest : int) : unit =
  let fn = if select then (move_click area backing m src) else null_click in
  let s = (Array.get board dest) in
  Array.set board dest {s with click_fn = fn};
  highlight_square area backing
    (if select then destinationColor else darkSquareColor) dest

(* Set a square's click function. *)
and set_click_fn (fn : int -> unit) (index : int) : unit =
  let s = Array.get board index in
  Array.set board index {s with click_fn = fn}

(* Move click function: for squares that are move destinations. *)
and move_click
    (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (valid_moves : (int * int) list)
    (src : int) (dest : int) : unit =
  let indices = fst (List.split valid_moves) in
  (* Unselect the squares (one of them is the last one we clicked) *)
  ignore (List.map (select_square area backing valid_moves false) indices);
  ignore (List.map (set_click_fn null_click) indices);

  (* Move to the destination and draw *)
  let redraws = move_piece src dest in
  ignore (List.map (draw_square area backing) redraws);

  (* If this was a jump, can the same piece jump again?
     If so, select it. Otherwise, set up the next side's moves. *)
  let side = (Array.get board dest).content in
  let flipfn = if (side = White) then identity else flip_index in
  let valid_jumps = if (is_jump src dest) then
    List.map (fun x -> (dest, x)) (find_valid_jumps dest flipfn) else [] in
  if (valid_jumps != []) then
    select_square area backing valid_jumps true dest
  else
    setup_moves area backing (swap_side side)

(* Mark a square as selected, and highlight the possible moves from it. *)
and select_square (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (m : (int * int) list)
    (select : bool) (index : int) : unit =
  (* Mark the square *)
  highlight_square area backing
    (if select then selectedColor else darkSquareColor) index;

  (* We will flip the board if necessary. *)
  let s = Array.get board index in
  let flipfn = if (s.content = White) then identity else flip_index in
  (* Mark the destinations. First, check any jumps, then moves. *)
  let possible_jumps = Hashtbl.find jumps (flipfn index) in
  let flipped_jumps = List.map flipfn possible_jumps in
  let valid_jumps = List.filter (is_valid_jump index) flipped_jumps in
  ignore (if (valid_jumps = []) then
    let possible_moves = Hashtbl.find moves (flipfn index) in
    let flipped_moves = List.map flipfn possible_moves in
    let valid_moves = List.filter (is_valid_move index) flipped_moves in
    List.map (mark_destination area backing m select index) valid_moves
  else
    List.map (mark_destination area backing m select index) valid_jumps)

(* Select click function: for squares that are selectable. *)
and select_click
    (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (valid_moves : (int * int) list) (index : int) : unit =
  let indices = fst (List.split valid_moves) in
  (* Unselect the squares (one of them is the last one we clicked) *)
  ignore (List.map (select_square area backing valid_moves false) indices);
  (* Re-highlight the squares (they are still selectable) *)
  ignore (List.map (highlight_square area backing selectableColor) indices);
  (* Select the clicked one *)
  select_square area backing valid_moves true index

(* Find valid moves, highlight the selectable squares, set up the click functions. *)
and setup_moves
    (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (side : squareContent) : unit =
  let valid_moves = find_valid_moves side in
  let indices = fst (List.split valid_moves) in
  ignore (List.map (highlight_square area backing selectableColor) indices);
  ignore (List.map (set_click_fn (select_click area backing valid_moves)) indices)

(* Reset the game state. *)
let new_game (area : GMisc.drawing_area) (backing : GDraw.pixmap ref) () : unit =
  init_pieces ();
  draw_board area backing;
  setup_moves area backing Black

(* On button click. *)
let button_pressed
    (area:GMisc.drawing_area) (backing:GDraw.pixmap ref)
    (ev : GdkEvent.Button.t) : bool =
  if GdkEvent.Button.button ev = 1 then (
    let x = int_of_float (GdkEvent.Button.x ev) in
    let y = int_of_float (GdkEvent.Button.y ev) in
    let (u, v) = window_coord_to_board_coord backing x y in
    let index = (v * numColumns + u) in
    let s = Array.get board index in
    s.click_fn index);
  false

(* Redraw the screen from the backing pixmap. *)
let expose
    (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (ev : GdkEvent.Expose.t) : bool =
  let expose_area = GdkEvent.Expose.area ev in
  let (x, y) = (Gdk.Rectangle.x expose_area, Gdk.Rectangle.y expose_area) in
  let (w, h) = (Gdk.Rectangle.width expose_area, Gdk.Rectangle.height expose_area) in
  let drawing =
    area#misc#realize ();
    new GDraw.drawable (area#misc#window)
  in
  drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width:w ~height:h !backing#pixmap;
  false

(* Create a new backing pixmap of the appropriate size *)
let configure
    (window : GWindow.window)
    (area : GMisc.drawing_area) (backing : GDraw.pixmap ref)
    (ev : GdkEvent.Configure.t) : bool =
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in
  let pixmap = GDraw.pixmap ~width ~height ~window () in
  pixmap#set_foreground `WHITE;
  pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  backing := pixmap;
  draw_board area backing;
  true

(* Finally, the main function. *)
let main () : unit =
  (* Create a new window; set title and border width *)
  let window = GWindow.window
      ~title:"Checkers"
      ~border_width:20 () in

  (* Set a handler for destroy event that immediately exits GTK. *)
  ignore (window#connect#destroy ~callback:GMain.Main.quit);

  (* Board is square: set aspect ratio hint *)
  window#set_geometry_hints ~aspect:(1.0, 1.0) window#coerce;

  let vbox = GPack.vbox ~packing:window#add () in

  (* Menu bar *)
  let menubar = GMenu.menu_bar ~packing:vbox#pack () in
  let factory = new GMenu.factory menubar in
  let accel_group = factory#accel_group in
  let file_menu = factory#add_submenu "File" in

  (* Create the drawing area and its backing pixmap. *)
  let squareSize = 60 in
  let drawWidth = (squareSize * numColumns) in
  let drawHeight = (squareSize * numRows) in
  let da = GMisc.drawing_area
      ~width:drawWidth
      ~height:drawHeight
      ~packing:vbox#add () in
  let backing = ref (GDraw.pixmap ~width:drawWidth ~height:drawHeight ()) in

  (* Connect signals. *)
  ignore (da#event#connect#button_press ~callback:(button_pressed da backing));
  ignore (da#event#connect#expose ~callback:(expose da backing));
  ignore (da#event#connect#configure ~callback:(configure window da backing));
  da#event#add [`BUTTON_PRESS;
                `EXPOSURE];

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_item "New Game" ~key:GdkKeysyms._N ~callback: (new_game da backing));
  ignore (factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback: GMain.Main.quit);

  (* Start the game, show the window, go. *)
  window#add_accel_group accel_group;
  window#show ();
  new_game da backing ();
  GMain.Main.main ()

let _ = Printexc.print main ()
