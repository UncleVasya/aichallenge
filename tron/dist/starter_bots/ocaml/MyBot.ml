let mybot_engine state =
   if state#turn = 0 then state#finish_turn ()
   else
    (
      Asteroids.issue_order (0.05, -0.15, 1);
      state#finish_turn ()
    )
;;

Asteroids.loop mybot_engine;;

