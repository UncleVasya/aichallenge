let mybot_engine state =
   if state#turn = 0 then state#finish_turn ()
   else
    (
      Wargame.issue_order_deploy 1 0;
      state#finish_turn ()
    )
;;

Wargame.loop mybot_engine;;

