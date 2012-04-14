(* MyBot.ml *)
open Asteroids

let mybot_engine state =   
  if state.turn > 0 then begin
      let mine, _ = List.partition (fun ship -> 
            ship.s_owner = state.setup.player_id) state.ships 
      in
      List.iter (fun ship -> Orders.order ship 0.05 (-0.15) true) mine;
    end;
    Orders.finish_turn ()

let _ =
  Asteroids.loop mybot_engine

