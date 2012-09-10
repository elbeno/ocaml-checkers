(* file: checkers.ml *)
(* compile with: $ ocamlc -I +lablgtk2 lablgtk.cma gtkInit.cmo checkers.ml -o checkers *)

(* 8x8 board *)
let numRows = 8
let numColumns = 8

(* Draw a piece at 80% of the square *)
let pieceWidth = 0.8

(* A square can contain a white piece, black piece, or neither,
   and can be selected. *)
type squareContent = White | Black | Neither;;
type square = { content : squareContent;
                selected : bool;
                possible_move : bool;
                possible_jump : bool};;

(* The board. *)
let board = Array.make (numRows * numColumns)
    {content = Neither; selected = false;
     possible_move = false; possible_jump = false}
let selectedSquare = ref (-1)

(* Flip the index if playing down the board. *)
let flip_index index =
  let boardx = index mod numColumns in
  let boardy = index / numRows in
  let y = numRows - boardy - 1 in
  (y * numColumns + boardx)

let identity x = x

(* Initialize the pieces on the board *)
let init_pieces =
  selectedSquare := -1;
  for i = 0 to numRows - 1 do
    for j = 0 to numColumns - 1 do
      let index = (i * numColumns + j) in
      if ((index + (i mod 2)) mod 2 != 0) then
        let setfn = Array.set board index in
        if i < 3 then
          setfn {content = White; selected = false;
                 possible_move = false; possible_jump = false}
        else if i >= numRows - 3 then
          setfn {content = Black; selected = false;
                 possible_move = false; possible_jump = false}
        else
          setfn {content = Neither; selected = false;
                 possible_move = false; possible_jump = false}
    done
  done

(* Possible moves. *)
let moves:(int, int list) Hashtbl.t = Hashtbl.create ((numRows * numColumns) / 2)
let jumps:(int, (int * int) list) Hashtbl.t = Hashtbl.create ((numRows * numColumns) / 2)

let possible_moves left index =
  let x = index mod numColumns in
  let y = index / numColumns in
  if (y = numRows - 1) then
    []
  else if (left) then
    if (x > 0) then
      [(y+1) * numColumns + x - 1]
    else
      []
  else
    if (x < numColumns - 1) then
      [(y+1) * numColumns + x + 1]
    else
      []

let init_moves =
  for i = 0 to numRows - 1 do
    for j = 0 to numColumns - 1 do
      let src = (i * numColumns + j) in
      let dest = List.append (possible_moves true src) (possible_moves false src) in
      Hashtbl.replace moves src dest
    done
  done

let init_jumps =
  for i = 0 to numRows - 1 do
    for j = 0 to numColumns - 1 do
      let src = (i * numColumns + j) in
      let leftcaptures = possible_moves true src in
      let destleft = List.map (possible_moves true) leftcaptures in
      let leftjumps = List.map (fun (x,y) -> (x, List.hd y))
          (List.filter (fun (x,y) -> y != []) (List.combine leftcaptures destleft)) in
      let rightcaptures = possible_moves false src in
      let destright = List.map (possible_moves false) rightcaptures in
      let rightjumps = List.map (fun (x,y) -> (x, List.hd y))
          (List.filter (fun (x,y) -> y != []) (List.combine rightcaptures destright)) in
      let dest = List.append leftjumps rightjumps in
      Hashtbl.replace jumps src dest
    done
  done

(* Convert window (x,y) to board square (row,col). *)
let window_coord_to_board_coord backing x y =
  let (w, h) = !backing#size in
  let xorg = (w mod numColumns) / 2 in
  let yorg = (h mod numRows) / 2 in
  let rowSize = h / numRows in
  let colSize = w / numColumns in
  let boardx = (x-xorg) / colSize in
  let boardy = (y-yorg) / rowSize in
  ((max 0 (min (numColumns-1) boardx)),
   (max 0 (min (numRows-1) boardy)))

(* Redraw the screen from the backing pixmap *)
let expose (drawing_area:GMisc.drawing_area) (backing:GDraw.pixmap ref) ev =
  let area = GdkEvent.Expose.area ev in
  let x = Gdk.Rectangle.x area in
  let y = Gdk.Rectangle.y area in
  let width = Gdk.Rectangle.width area in
  let height = Gdk.Rectangle.width area in
  let drawing =
    drawing_area#misc#realize ();
    new GDraw.drawable (drawing_area#misc#window)
  in
  drawing#put_pixmap ~x ~y ~xsrc:x ~ysrc:y ~width ~height !backing#pixmap;
  false

(* Draw a square and its piece, if any. *)
let draw_square (area:GMisc.drawing_area) (backing:GDraw.pixmap ref) (index:int) =
  let (w, h) = !backing#size in
  let xorg = (w mod numColumns) / 2 in
  let yorg = (h mod numRows) / 2 in
  let squareWidth = w / numRows in
  let squareHeight = h / numColumns in
  let boardx = index mod numColumns in
  let boardy = index / numRows in
  let x = xorg + squareWidth * boardx in
  let y = yorg + squareHeight * boardy in

  (* white/black squares *)
  if (((boardy * numColumns + boardx) + (boardy mod 2)) mod 2 != 0) then
    !backing#set_foreground (`NAME "dim gray")
  else
    !backing#set_foreground (`NAME "light gray");
  !backing#rectangle ~x ~y ~width:squareWidth ~height:squareHeight ~filled:true ();

  (* selected? *)
  !backing#set_line_attributes ~width:4 ();
  if ((Array.get board index).selected) then (
    !backing#set_foreground (`NAME "red");
    !backing#rectangle ~x:(x+2) ~y:(y+2)
      ~width:(squareWidth - 4) ~height:(squareHeight - 4) ~filled:false (););

  (* possible move? *)
  !backing#set_line_attributes ~width:4 ();
  if ((Array.get board index).possible_move) then (
    !backing#set_foreground (`NAME "green");
    !backing#rectangle ~x:(x+2) ~y:(y+2)
      ~width:(squareWidth - 4) ~height:(squareHeight - 4) ~filled:false (););

  (* piece on the square *)
  if ((Array.get board index).content != Neither) then (
    let offset = int_of_float (float_of_int squareWidth *. (1.0 -. pieceWidth)) in
    let xorg = (w mod numColumns + offset) / 2 in
    let yorg = (h mod numRows + offset) / 2 in
    let x = xorg + squareWidth * boardx in
    let y = yorg + squareHeight * boardy in
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
let draw_board (area:GMisc.drawing_area) (backing:GDraw.pixmap ref) =
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

let select_possible_move area backing (index:int) (select:bool) =
  let s = (Array.get board index) in
  Array.set board index {s with possible_move = (select && (s.content == Neither))};
  draw_square area backing index

let rec select_possible_moves area backing (possible_moves:int list) (select:bool) =
  match possible_moves with
    head :: tail -> (
      select_possible_move area backing head select;
      select_possible_moves area backing tail select)
  | [] -> ()

let select_square area backing (index:int) (select:bool) =
  let s = {(Array.get board index) with selected=select} in
  Array.set board index s;
  draw_square area backing index;
  let flipfn = if (s.content = White) then identity else flip_index in
  let possible_moves = Hashtbl.find moves (flipfn index) in
  select_possible_moves area backing (List.map flipfn possible_moves) select
  (*let possible_jumps = Hashtbl.find jumps (flipfn index) in
  select_possible_jumps area backing (List.map flipfn possible_jumps) select*)

let move_piece srcindex destindex =
  Array.set board destindex (Array.get board srcindex);
  Array.set board srcindex {(Array.get board srcindex) with content=Neither}

let button_pressed area backing ev =
  if GdkEvent.Button.button ev = 1 then (
    let x = int_of_float (GdkEvent.Button.x ev) in
    let y = int_of_float (GdkEvent.Button.y ev) in
    let (u, v) = window_coord_to_board_coord backing x y in

    let index = (v * numColumns + u) in
    let s = Array.get board index in
    if (s.possible_move) then (
      select_square area backing !selectedSquare false;
      move_piece !selectedSquare index;
      draw_square area backing !selectedSquare;
      selectedSquare := -1;
      draw_square area backing index)
    else
      if (s.content != Neither) then (
        if (!selectedSquare != -1) then
          select_square area backing !selectedSquare false;
        selectedSquare := index;
        select_square area backing !selectedSquare true)
   );
  false

let new_game area backing _ =
  init_pieces;
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

  init_moves;
  init_jumps;

  window#add_accel_group accel_group;
  window#show ();
  new_game;

  GMain.Main.main ()

let _ = Printexc.print main ()
