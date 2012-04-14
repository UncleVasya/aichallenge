(* asteroids.ml *)
module Log = struct
  let write channel level msg =
		Printf.fprintf channel "[%-5s] %s\n%!" level msg
  
  let debug msg = write stderr "DEBUG" msg
  and warn msg = write stderr "WARN" msg
  and info msg = write stderr "INFO" msg
  and error msg = write stderr "ERROR" msg
end

let get_time = Unix.gettimeofday

type game_setup = {
  turns : int;
  width : int;
  height : int;
  player_id : int;
  player_seed : int;
  turntime : int;
  loadtime : int;
  turn_steps : int;
  m_thrust : float;
  m_turn : float;
  ship_radius : int;
  speed_cap : float;
}

type asteroid = {
  a_size : int;
  a_x : float;
  a_y : float;
  a_heading : float;
  a_speed : float;
}

type bullet = {
  b_id : int;
  b_x : float;
  b_y : float;
  b_heading : float;
  b_speed : float;
}

type ship = {
  s_id : int;
  s_x : float;
  s_y : float;
  s_heading : float;
  s_x_speed : float;
  s_y_speed : float;
  s_owner : int;
}

type game_state = {
  turn : int;
  setup : game_setup;
  go_time : float;
  asteroids : asteroid list;
  bullets : bullet list;
  ships : ship list;
}

let add_asteroid state size x y heading speed =
  Log.debug (Printf.sprintf "add asteroid: %d %f %f %f %f" size x y heading speed);  
  let a = { a_size = size; a_x = x; a_y = y; a_heading = heading; a_speed = speed } in
  { state with asteroids = a :: state.asteroids }

let add_bullet state id x y heading speed =
  Log.debug (Printf.sprintf "add bullet: %d %f %f %f %f" id x y heading speed);
  let b = { b_id = id; b_x = x; b_y = y; b_heading = heading; b_speed = speed } in
  { state with bullets = b :: state.bullets }

let add_ship state id x y heading x_speed y_speed owner =
  Log.debug (Printf.sprintf "add ship: %d %f %f %f %f %f %d"
        id x y heading x_speed y_speed owner);
  let s = {
    s_id = id; s_x = x; s_y = y; s_heading = heading;
    s_x_speed = x_speed; s_y_speed = y_speed;
    s_owner = owner }
  in
  { state with ships = s :: state.ships }

module Input = struct
  exception End_of_block of string
  exception Unknown_command of string
  
  let split sep s =
    let list = ref [] in
    let last = ref ((String.length s) - 1) in
    begin try
      while true do
        let index = String.rindex_from s !last sep in
        let token = String.sub s (index +1) (!last - index) in
        list := token :: !list;
        last := index - 1
      done
    with Not_found ->
        let first = String.sub s 0 (!last +1) in
        list := first :: !list
    end;
    !list
  
  let int_of_int64_string s =
    try Int64.to_int (Int64.of_string s)
    with e -> 
       Log.warn "int_of_int64_string failed";
       raise e

  let int_of_int32_string s =
    try Int32.to_int (Int32.of_string s)
    with e -> 
      Log.warn "int_of_int32_string failed";
      raise e
  
  let int_of_player_seed_string s =
     try int_of_int64_string s
     with _ ->
       begin try int_of_int32_string s
       with e ->
          Log.warn "attempting int_of_string";
          int_of_string s
       end

  let read_setup channel =
    let rec loop setup =
      let line = input_line channel in
      let tokens = split ' ' line in
      match tokens with
      | [""] -> loop setup
      (* end of block *)
      | "ready"::[] -> setup
      (* game setup *)
      | "turn"::"0"::[] -> loop setup
      | "turns":: value::[] -> loop { setup with turns = int_of_string value } 
      | "width":: value::[] -> loop { setup with width = int_of_string value } 
      | "height":: value::[] -> loop { setup with height = int_of_string value } 
      | "player_id":: value::[] -> loop { setup with player_id = int_of_string value } 
(*      | "player_seed":: value::[] -> loop { setup with player_seed = int_of_int32_string value } *)
      | "player_seed":: value::[] -> loop { setup with player_seed = int_of_player_seed_string value }
      | "loadtime":: value::[] -> loop { setup with loadtime = int_of_string value } 
      | "turntime":: value::[] -> loop { setup with turntime = int_of_string value }       
      | "turn_steps":: value::[] -> loop { setup with turn_steps = int_of_string value } 
      | "ship_radius":: value::[] -> loop { setup with ship_radius = int_of_string value }       
      | "m_thrust":: value::[] -> loop { setup with m_thrust = float_of_string value } 
      | "m_turn":: value::[] -> loop { setup with m_turn = float_of_string value } 
      | "speed_cap":: value::[] -> loop { setup with speed_cap = float_of_string value } 
      (* parse errors *)
      | s :: _ when s.[0] = '#' -> Log.debug s; loop setup
      | _ -> raise (Unknown_command line)
    in
    let setup = { turns = -1; width = -1; height = -1; player_id = -1;
      player_seed = -1; turntime = -1; loadtime = -1; 
      turn_steps = -1; m_thrust = 0.; m_turn = 0.; ship_radius = -1; speed_cap = 0.}
    in
    loop setup
  
  let read_state channel setup =
    let rec loop state =
      let line = input_line channel in
      let tokens = split ' ' line in
      match tokens with
      | [""] -> loop state
      (* end of block *)
      | "go"::[] -> { state with go_time = get_time() }
      | "end"::[] -> { state with go_time = get_time() }
      (* game state *)
      | "turn":: value::[] -> loop { state with turn = int_of_string value }
      | "a":: a:: b:: c:: d:: e::[] ->
        let size = int_of_string a
        and x = float_of_string b
        and y = float_of_string c
        and heading = float_of_string d
        and speed = float_of_string e in
        loop (add_asteroid state size x y heading speed)
      | "b":: a:: b:: c:: d:: e::[] ->
        let id = int_of_string a
        and x = float_of_string b
        and y = float_of_string c
        and heading = float_of_string d
        and speed = float_of_string e in
        loop (add_bullet state id x y heading speed)
      | "s":: a:: b:: c:: d:: e:: f:: g::[] ->
        let id = int_of_string a
        and x = float_of_string b
        and y = float_of_string c
        and heading = float_of_string d
        and x_speed = float_of_string e 
        and y_speed = float_of_string f
        and owner = int_of_string g in
        loop (add_ship state id x y heading x_speed y_speed owner)
      (* parse errors *)
      | s :: _ when s.[0] = '#' -> Log.debug s; loop state
      | _ -> raise (Unknown_command line)
    in
    let state = { setup = setup; turn = 0; go_time = 0.;
      asteroids = []; bullets = []; ships = [] }
    in
    loop state
end

module Orders = struct
  open Printf
  
  let order ship thrust turn fire =
    let fire_int = if fire then 1 else 0 in 
    Log.debug (sprintf "issue order: o %d %f %f %d" ship.s_id thrust turn fire_int);
    printf "o %d %f %f %d\n" ship.s_id thrust turn fire_int
  
  let finish_turn () = printf "go\n%!"
end

let loop engine =
  let init setup =
    let state = { setup = setup; turn = 0; go_time = get_time();
      asteroids = []; bullets = []; ships = [] }  in
    engine state
  in  
  let rec play setup =
    let state = Input.read_state stdin setup in
    Log.info (Printf.sprintf "turn %d" state.turn);
    begin try
      engine state
    with exc ->
        Log.error (Printf.sprintf "Exception in turn %d : %s\n" state.turn (Printexc.to_string exc));
    end;
    Orders.finish_turn ();
    Log.info (Printf.sprintf "end of turn %d, used %f seconds" state.turn (get_time() -. state.go_time));
    play state.setup
  in
  let setup = Input.read_setup stdin in
  init setup;
  play setup

