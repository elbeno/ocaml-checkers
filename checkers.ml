(* file: checkers.ml *)

open Board

(* Board colors. *)
let darkSquareColor : GDraw.color = (`NAME "dim gray");;
let lightSquareColor : GDraw.color = (`NAME "light gray");;
let selectableColor : GDraw.color = (`NAME "yellow");;
let selectedColor : GDraw.color = (`NAME "red");;
let destinationColor : GDraw.color = (`NAME "green");;

(* Draw a piece at 80% of the square width. *)
let pieceWidth : float = 0.8;;

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

(* Draw a square, and its piece, if any. *)
let draw_square
    (area : GMisc.drawing_area)
    (backing : GDraw.pixmap ref)
    (board : board_t ref)
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
  let side = piece_color !board index in
  if (side != Neither) then (
    let offset = int_of_float (float_of_int squareWidth *. (1.0 -. pieceWidth) /. 2.0) in
    if (side = White) then
      !backing#set_foreground `WHITE
    else
      !backing#set_foreground `BLACK;
    !backing#arc ~x:(x + offset) ~y:(y + offset)
      ~width:(int_of_float (float_of_int squareWidth *. pieceWidth))
      ~height:(int_of_float (float_of_int squareHeight *. pieceWidth))
      ~filled:true (););

  (* And update the drawing area *)
  let update_rect = Gdk.Rectangle.create x y squareWidth squareHeight in
  area#misc#draw (Some update_rect)

(* Draw the board *)
let draw_board
    (area : GMisc.drawing_area)
    (backing : GDraw.pixmap ref)
    (board : board_t ref) : unit =
  for i = 0 to (boardSize - 1) do
    draw_square area backing board i
  done;;

(* Hash table of square index to click function. *)
let click_fns : (int, int -> unit) Hashtbl.t = Hashtbl.create boardSize;;

let set_click_fn (fn : int -> unit) (index : int) : unit =
  Hashtbl.replace click_fns index fn

let clear_click_fn (index : int) : unit =
  Hashtbl.remove click_fns index

(* Setup moves given a source and dests. *)
let rec setup_moves
    (area : GMisc.drawing_area)
    (backing : GDraw.pixmap ref)
    (board : board_t ref)
    (highlightedSquares : int list)
    (src : int)
    (dests : int list) : unit =
  (* highlight clicked square and dests *)
  highlight_square area backing selectedColor src;
  ignore (List.map (highlight_square area backing destinationColor) dests);
  (* set click functions for dests *)
  ignore (List.map (set_click_fn (move_click area backing board highlightedSquares src))
            dests)

(* Click on a destination square. *)
and move_click
    (area : GMisc.drawing_area)
    (backing : GDraw.pixmap ref)
    (board : board_t ref)
    (highlightedSquares : int list)
    (src : int)
    (dest : int) : unit =
  (* unhighlight highlighted squares *)
  ignore (List.map (highlight_square area backing darkSquareColor) highlightedSquares);
  (* move piece from src to dest *)
  let (newBoard, changedSquares) = move_piece !board src dest in
  board := newBoard;
  ignore (List.map (draw_square area backing board) changedSquares);
  (* clear click functions *)
  ignore (List.map clear_click_fn (List.append highlightedSquares changedSquares));
  (* if multi-jump, do that, else find new selectable squares for the other side *)
  let multijumps = if (is_jump src dest) then (find_jumps !board dest) else [] in
  if (multijumps != []) then
    let newHighlightedSquares = dest :: multijumps in
    setup_moves area backing board newHighlightedSquares dest multijumps
  else
    highlight_selectable_squares area backing board (swap_side (piece_color !board dest))

(* Click on a selected square. *)
and select_click
    (area : GMisc.drawing_area)
    (backing : GDraw.pixmap ref)
    (board : board_t ref)
    (highlightedSquares : int list)
    (index : int) : unit =
  (* unhighlight highlighted squares *)
  ignore (List.map (highlight_square area backing darkSquareColor) highlightedSquares);
  (* find destinations and selectable squares *)
  let dests = find_destinations !board index in
  let side = piece_color !board index in
  let selectable_squares = find_selectable_squares !board side in
  (* highlight selectable squares and reset their click functions *)
  ignore (List.map (highlight_square area backing selectableColor) selectable_squares);
  let newHighlightedSquares = List.append selectable_squares dests in
  ignore (List.map (set_click_fn (select_click area backing board newHighlightedSquares))
            selectable_squares);
  setup_moves area backing board newHighlightedSquares index dests

(* Highlight the selectable squares. *)
and highlight_selectable_squares
    (area : GMisc.drawing_area)
    (backing : GDraw.pixmap ref)
    (board : board_t ref)
    (side : side_t) : unit =
  (* highlight them *)
  let selectable_squares = find_selectable_squares !board side in
  ignore (List.map (highlight_square area backing selectableColor) selectable_squares);
  (* set the click functions *)
  ignore (List.map (set_click_fn (select_click area backing board selectable_squares))
            selectable_squares)

(* On button click. *)
let button_pressed
    (area:GMisc.drawing_area) (backing:GDraw.pixmap ref)
    (ev : GdkEvent.Button.t) : bool =
  if GdkEvent.Button.button ev = 1 then (
    let x = int_of_float (GdkEvent.Button.x ev) in
    let y = int_of_float (GdkEvent.Button.y ev) in
    let (u, v) = window_coord_to_board_coord backing x y in
    let index = (v * numColumns + u) in
    let fn =
      try
        Hashtbl.find click_fns index
      with Not_found -> (fun _ -> ()) in
    fn index);
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
    (area : GMisc.drawing_area) (backing : GDraw.pixmap ref) (board : board_t ref)
    (ev : GdkEvent.Configure.t) : bool =
  let width = GdkEvent.Configure.width ev in
  let height = GdkEvent.Configure.height ev in
  let pixmap = GDraw.pixmap ~width ~height ~window () in
  pixmap#set_foreground `WHITE;
  pixmap#rectangle ~x:0 ~y:0 ~width ~height ~filled:true ();
  backing := pixmap;
  draw_board area backing board;
  true

(* Reset the game state. *)
let new_game
    (area : GMisc.drawing_area)
    (backing : GDraw.pixmap ref)
    (board : board_t ref)
    () : unit =
  board := init_pieces ();
  draw_board area backing board;
  Hashtbl.clear click_fns;
  highlight_selectable_squares area backing board White

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

  let board = ref (init_pieces ()) in

  (* Connect signals. *)
  ignore (da#event#connect#button_press ~callback:(button_pressed da backing));
  ignore (da#event#connect#expose ~callback:(expose da backing));
  ignore (da#event#connect#configure ~callback:(configure window da backing board));
  da#event#add [`BUTTON_PRESS;
                `EXPOSURE];

  (* File menu *)
  let factory = new GMenu.factory file_menu ~accel_group in
  ignore (factory#add_item "New Game" ~key:GdkKeysyms._N ~callback: (new_game da backing board));
  ignore (factory#add_item "Quit" ~key:GdkKeysyms._Q ~callback: GMain.Main.quit);

  (* Start the game, show the window, go. *)
  window#add_accel_group accel_group;
  window#show ();
  new_game da backing board ();
  GMain.Main.main ()

let () = main ()
