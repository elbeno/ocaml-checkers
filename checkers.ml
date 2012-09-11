(* file: checkers.ml *)
(* compile with: $ ocamlc -I +lablgtk2 lablgtk.cma gtkInit.cmo checkers.ml -o checkers *)

(* 8x8 board *)
let numRows : int = 8;;
let numColumns : int = 8;;
let boardSize : int = numRows * numColumns;;

(* Board colors. *)
let darkSquareColor : GDraw.color = (`NAME "dim gray");;
let lightSquareColor : GDraw.color = (`NAME "light gray");;
let highlightColor : GDraw.color = (`NAME "red");;
let destinationColor : GDraw.color = (`NAME "green");;

(* Draw a piece at 80% of the square width. *)
let pieceWidth : float = 0.8;;

(* A square can contain a white piece, black piece, or neither,
   can be selected, or can be the destination of a possible move. *)
type squareContent = White | Black | Neither;;
type square = { content : squareContent;
                selected : bool;
                possible_move : bool};;

(* The board. *)
let board:square array = Array.make boardSize
    {content = Neither; selected = false; possible_move = false};;
let selectedSquare : int ref = ref (-1);;

(* Flip the index if playing down the board. *)
let flip_index (index : int) : int =
  let boardx = index mod numColumns in
  let boardy = index / numRows in
  let y = numRows - boardy - 1 in
  (y * numColumns + boardx)

(* And don't flip it otherwise. *)
let identity (x : 'a) : 'a = x

(* Board index to (x,y) coordinate. *)
let index_to_xy (index : int) : (int * int) = (index mod numColumns, index / numColumns)

(* Initialize the pieces on the board. *)
let init_pieces () : unit =
  selectedSquare := -1;
  for index = 0 to (boardSize - 1) do
    let (x, y) = index_to_xy index in
    if ((index + (y mod 2)) mod 2 != 0) then
      let setfn = Array.set board index in
      let s = {content = Neither; selected = false; possible_move = false} in
      if y < 3 then
        setfn {s with content = White}
      else if y >= numRows - 3 then
        setfn {s with content = Black}
      else
        setfn s
  done

(* Possible moves. *)
let moves : (int, int list) Hashtbl.t = Hashtbl.create boardSize;;
let jumps : (int, (int * int) list) Hashtbl.t = Hashtbl.create boardSize;;

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
let possible_jumps (left : bool) (index : int) : (int * int) list =
  let captures = possible_moves left index in
  if (captures == []) then []
  else
    let capture = List.hd captures in
    let destinations = possible_moves left capture in
    if (destinations == []) then [] else [(capture, List.hd destinations)]

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
let is_valid_move (dest : int) : bool =
  let destContent = (Array.get board dest).content in
  destContent = Neither

(* Is a jump valid? The pieces at src and capture points must be opposite colors,
   and the destination must be empty. *)
let is_valid_jump (src : int) (dest : int * int) : bool =
  let srcContent = (Array.get board src).content in
  let captureContent = (Array.get board (fst dest)).content in
  let destContent = (Array.get board (snd dest)).content in
  destContent = Neither &&
  ((srcContent = White && captureContent = Black) ||
  (srcContent = Black && captureContent = White))

(* Convert window (x,y) to board square (row,col). *)
let window_coord_to_board_coord backing (x : int) (y : int) : (int * int) =
  let (w, h) = !backing#size in
  let (xorg, yorg) = ((w mod numColumns) / 2, (h mod numRows) / 2) in
  let (squareWidth, squareHeight) = (w / numColumns, h / numRows) in
  let (boardx, boardy) = ((x-xorg) / squareWidth, (y-yorg) / squareHeight) in
  ((max 0 (min (numColumns-1) boardx)), (max 0 (min (numRows-1) boardy)))

(* Redraw the screen from the backing pixmap. *)
let expose (drawing_area:GMisc.drawing_area) (backing:GDraw.pixmap ref) ev : bool =
  let area = GdkEvent.Expose.area ev in
  let (x, y) = (Gdk.Rectangle.x area, Gdk.Rectangle.y area) in
  let (w, h) = (Gdk.Rectangle.width area, Gdk.Rectangle.height area) in
  let drawing =
    drawing_area#misc#realize ();
    new GDraw.drawable (drawing_area#misc#window)
  in
  drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width:w ~height:h !backing#pixmap;
  false

(* Draw a square and its piece, if any. *)
let draw_square (area:GMisc.drawing_area) (backing:GDraw.pixmap ref) (index:int) : unit =
  let (w, h) = !backing#size in
  let (xorg, yorg) = ((w mod numColumns) / 2, (h mod numRows) / 2) in
  let (squareWidth, squareHeight) = (w / numColumns, h / numRows) in
  let (boardx, boardy) = index_to_xy index in
  let (x, y) = (xorg + squareWidth * boardx, yorg + squareHeight * boardy) in

  (* white/black squares *)
  if (((boardy * numColumns + boardx) + (boardy mod 2)) mod 2 != 0) then
    !backing#set_foreground darkSquareColor
  else
    !backing#set_foreground lightSquareColor;
  !backing#rectangle ~x ~y ~width:squareWidth ~height:squareHeight ~filled:true ();

  (* selected? *)
  !backing#set_line_attributes ~width:4 ();
  if ((Array.get board index).selected) then (
    !backing#set_foreground highlightColor;
    !backing#rectangle ~x:(x+2) ~y:(y+2)
      ~width:(squareWidth - 4) ~height:(squareHeight - 4) ~filled:false (););

  (* possible move? *)
  !backing#set_line_attributes ~width:4 ();
  if ((Array.get board index).possible_move) then (
    !backing#set_foreground destinationColor;
    !backing#rectangle ~x:(x+2) ~y:(y+2)
      ~width:(squareWidth - 4) ~height:(squareHeight - 4) ~filled:false (););

  (* piece on the square *)
  if ((Array.get board index).content != Neither) then (
    let offset = int_of_float (float_of_int squareWidth *. (1.0 -. pieceWidth)) in
    let (xorg, yorg) = ((w mod numColumns + offset) / 2, (h mod numRows + offset) / 2) in
    let (x, y) = (xorg + squareWidth * boardx, yorg + squareHeight * boardy) in
    if ((Array.get board index).content = White) then
      !backing#set_foreground `WHITE
    else if ((Array.get board index).content = Black) then
      !backing#set_foreground `BLACK;
    !backing#arc ~x ~y
      ~width:(int_of_float (float_of_int squareWidth *. pieceWidth))
      ~height:(int_of_float (float_of_int squareHeight *. pieceWidth))
      ~filled:true (););

  (* And update the drawing area *)
  let update_rect = Gdk.Rectangle.create x y squareWidth squareHeight in
  area#misc#draw (Some update_rect)

(* Draw the board *)
let draw_board (area:GMisc.drawing_area) (backing:GDraw.pixmap ref) : unit =
  for i = 0 to (numRows * numColumns - 1) do
    draw_square area backing i
  done;;

(* Create a new backing pixmap of the appropriate size *)
let configure window area backing ev =
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in
  let pixmap = GDraw.pixmap ~width ~height ~window () in
  pixmap#set_foreground `WHITE;
  pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  backing := pixmap;
  draw_board area backing;
  true

(* Mark a square as a destination. *)
let mark_destination area backing (select:bool) (index:int) : unit =
  let s = (Array.get board index) in
  Array.set board index {s with possible_move = select};
  draw_square area backing index

(* Mark a square as selected, and highlight the possible moves from it. *)
let select_square area backing (select:bool) (index:int) : unit =
  (* Mark the square *)
  let s = {(Array.get board index) with selected = select} in
  Array.set board index s;
  draw_square area backing index;

  (* We will flip the board if necessary. *)
  let flipfn = if (s.content = White) then identity else flip_index in

  (* Mark the destinations. First, check any jumps, then moves. *)
  let possible_jumps = Hashtbl.find jumps (flipfn index) in
  let flipped_jumps = List.map (fun (c,d) -> (flipfn c, flipfn d)) possible_jumps in
  let valid_jumps = List.filter (is_valid_jump index) flipped_jumps in
  if (valid_jumps = []) then
    let possible_moves = Hashtbl.find moves (flipfn index) in
    let flipped_moves = List.map flipfn possible_moves in
    let valid_moves = List.filter is_valid_move flipped_moves in
    List.map (mark_destination area backing select) valid_moves
  else
    List.map (mark_destination area backing select) (snd (List.split valid_jumps));
  ()

(* Move a piece from src to destination. *)
let move_piece (srcindex : int) (destindex : int) : unit =
  Array.set board destindex (Array.get board srcindex);
  Array.set board srcindex {(Array.get board srcindex) with content=Neither}

(* On button click. *)
let button_pressed (area:GMisc.drawing_area) (backing:GDraw.pixmap ref) ev =
  if GdkEvent.Button.button ev = 1 then (
    let x = int_of_float (GdkEvent.Button.x ev) in
    let y = int_of_float (GdkEvent.Button.y ev) in
    let (u, v) = window_coord_to_board_coord backing x y in

    let index = (v * numColumns + u) in
    let s = Array.get board index in
    if (s.possible_move) then (
      select_square area backing false !selectedSquare;
      move_piece !selectedSquare index;
      draw_square area backing !selectedSquare;
      selectedSquare := -1;
      draw_square area backing index)
    else
      if (s.content != Neither) then (
        if (!selectedSquare != -1) then
          select_square area backing false !selectedSquare;
        selectedSquare := index;
        select_square area backing true !selectedSquare)
   );
  false

let new_game (area:GMisc.drawing_area) (backing:GDraw.pixmap ref) () : unit =
  init_pieces ();
  draw_board area backing

let main () =
  (* Create a new window; set title and border width *)
  let window = GWindow.window
      ~title:"Checkers"
      ~border_width:20 () in

  (* Set a handler for destroy event that immediately exits GTK. *)
  window#connect#destroy ~callback:GMain.Main.quit;

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
  da#event#connect#button_press ~callback:(button_pressed da backing);
  da#event#connect#expose ~callback:(expose da backing);
  da#event#connect#configure ~callback:(configure window da backing);
  da#event#add [`BUTTON_PRESS;
                `EXPOSURE];

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  factory#add_item "New Game" ~key:GdkKeysyms._N ~callback: (new_game da backing);
  factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback: GMain.Main.quit;

  (* Start the game, show the window, go. *)
  new_game da backing ();
  window#add_accel_group accel_group;
  window#show ();
  GMain.Main.main ()

let _ = Printexc.print main ()
