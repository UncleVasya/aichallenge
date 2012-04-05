let mybot_engine state =
   if state#turn = 0 then state#finish_turn ()
   else
    (
      let mine, _ = List.partition (fun ship -> 
            ship.Asteroids.owner = state#my_id) state#get_ships 
      in
      List.iter (fun ship ->
            Asteroids.issue_order (ship.Asteroids.s_body.Asteroids.id, 1.0, 0.0, 1);
      ) mine;
      state#finish_turn ()
    )
;;

Asteroids.loop mybot_engine;;

