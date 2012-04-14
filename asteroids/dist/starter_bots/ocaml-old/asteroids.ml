(* ocaml Asteroids starter package. Code has been borrowed from the ocaml
 * Ants starter package, which in turn borrwed from the PlanetWars package.
 *)

let out_chan = stderr (* open_out "mybot_err.log" *);;

let get_time () = Unix.gettimeofday ();;

let debug s = 
   output_string out_chan s; 
   flush out_chan
;;

type game_setup =
 {
   mutable turn : int;
   mutable width : int;
   mutable height : int;
   mutable player_id : int;
   mutable player_seed : int;
   mutable turntime : int;
   mutable loadtime : int;
   mutable turn_steps : int;
   mutable m_thrust : float;
   mutable m_turn : float;
   mutable ship_radius : int;
 }
;;

type body =
 {
   id : int;
   x : float;
   y : float;
   heading : float;
   speed : float;
 }
;;

type ship =
 {
   owner : int;
   s_body : body;
   x_speed : float;
   y_speed : float;
 }
;;

type game_state =
 {
   setup : game_setup;
   go_time : float;
   mutable asteroids : body list;
   mutable bullets : body list;
   mutable ships : ship list;
 }
;;

type order = (int * float * float * int);;

(* Begin input processing stuff *)

let uncomment s =
  try String.sub s 0 (String.index s '#')
  with Not_found -> s
;;

let sscanf_cps fmt cont_ok cont_fail s =
  try Scanf.sscanf s fmt cont_ok
  with _ -> cont_fail s
;;

let clear_gstate gstate =
   if gstate.setup.turn < 1 then () else
     (
      gstate.asteroids <- [];
      gstate.bullets <- [];
      gstate.ships <- [];
     )
;;

let new_body t1 t2 t3 t4 t5 =
 {
   id = t1;
   x = t2;
   y = t3;
   heading = t4;
   speed = t5
 }
;;

let add_asteroid gstate t1 t2 t3 t4 t5 =
   let a = new_body t1 t2 t3 t4 t5 in
      gstate.asteroids <- a :: gstate.asteroids
;;

let add_bullet gstate t1 t2 t3 t4 t5 =
   let b = new_body t1 t2 t3 t4 t5 in
      gstate.bullets <- b :: gstate.bullets
;;

let add_ship gstate t1 t2 t3 t4 t5 t6 t7 =
   let b = new_body t1 t2 t3 t4 0.0 in
   let s = {owner = t7; s_body = b; x_speed = t5; y_speed = t6} in
      gstate.ships <- s :: gstate.ships
;;

let five_term gstate key t1 t2 t3 t4 t5 =
   match key with
    | "a" -> add_asteroid gstate t1 t2 t3 t4 t5
    | "b" -> add_bullet gstate t1 t2 t3 t4 t5
    | _ -> ()
;;

let seven_term gstate key t1 t2 t3 t4 t5 t6 t7 =
   match key with
    | "s" -> add_ship gstate t1 t2 t3 t4 t5 t6 t7
    | _ -> ()
;;

let two_term gstate key value =
   match key with
    | "turn" -> gstate.setup.turn <- value
    | "width" -> gstate.setup.width <- value
    | "height" -> gstate.setup.height <- value
    | "player_id" -> gstate.setup.player_id <- value
    | "player_seed" -> gstate.setup.player_seed <- value
    | "loadtime" -> gstate.setup.loadtime <- value
    | "turntime" -> gstate.setup.turntime <- value
    | "turn_steps" -> gstate.setup.turn_steps <- value
    | "ship_radius" -> gstate.setup.ship_radius <- value
    | _ -> ()
;;

let two_term_float gstate key value =
   match key with
    | "m_thrust" -> gstate.setup.m_thrust <- value
    | "m_turn" -> gstate.setup.m_turn <- value
    | _ -> ()
;;

let add_line gstate line =
   sscanf_cps "%s %d %f %f %f %f %f %d" (seven_term gstate)
     (
      sscanf_cps "%s %d %f %f %f %f" (five_term gstate)
        (
         sscanf_cps "%s %d" (two_term gstate) 
           (
            sscanf_cps "%s %f" (two_term_float gstate) (fun _ -> ())
           )
        )
     )
     (uncomment line)

let update gstate lines =
   let cgstate =
      if gstate.setup.turn = 0 then gstate
      else (clear_gstate gstate; gstate)
   in
      List.iter (add_line cgstate) lines 
;;

let read_lines () =
  let rec read_loop acc =
    let line = read_line () in
    if String.length line >= 2 && String.sub line 0 2 = "go" 
    || String.length line >= 3 && String.sub line 0 3 = "end"
    || String.length line >= 5 && String.sub line 0 5 = "ready" then
     (
      List.rev acc
     )
    else
      read_loop (line :: acc)
  in
  try Some (read_loop []) with End_of_file -> None
;;

let read gstate =
  let ll = read_lines () in
  let go_time = get_time () in
  match ll with
  | Some lines -> Some {(update gstate lines; gstate) with go_time = go_time}
  | None -> None
;;

(* End input section *)

(* Begin output section *)

let issue_order (target, f1, f2, i) =
   Printf.printf "o %d %f %f %d\n" target f1 f2 i
;;

(* Print go, newline, and flush buffer *)
let finish_turn () = Printf.printf "go\n%!";;

(* End output section *)

class swrap state =
 object (self)
   val mutable state = state
   method get_state = state
   method set_state v = state <- v
   method issue_order o = issue_order o
   method get_ships = state.ships
   method my_id = state.setup.player_id
   method finish_turn () = finish_turn ()
   method turn = state.setup.turn
 end
;;

let loop engine =
  let proto_setup =
     {
      turn = -1;
      width = -1;
      height = -1;
      player_id = -1;
      player_seed = -1;
      turntime = -1;
      loadtime = -1;
      turn_steps = -1;
      m_thrust = -1.;
      m_turn = -1.;
      ship_radius = 5;
     }
  in
  let proto_gstate =
     {
      setup = proto_setup;
      go_time = -1.0;
      asteroids = [];
      bullets = [];
      ships = []
     }
  in
  let wrap = new swrap proto_gstate in
  let rec take_turn i gstate =
    match read gstate with
    | Some state ->
        begin try
         (
          wrap#set_state state;
          engine wrap;
          flush stdout;
         )
        with exc ->
         (
          debug (Printf.sprintf
             "Exception in turn %d :\n" i);
          debug (Printexc.to_string exc);
          raise exc
         )
        end;
        take_turn (i + 1) wrap#get_state
    | None ->
        ()
  in
     take_turn 0 proto_gstate
;;

