
let threat state territory =
   List.fold_left (fun acc t ->
      if t.Wargame.owner = state#my_id then acc
      else if t.Wargame.owner = state#neutral_id then acc + 1
      else acc + 2
   ) 0 territory.Wargame.neighbors
;;

let most_threatened state =
   let _, result = List.fold_left
      (fun (prev_score, prev_t) t ->
         let score = threat state t in
         match prev_t with
          | None -> (score, Some t)
          | Some prev_t ->
                  if score > prev_score then (score, Some t)
                  else (prev_score, Some prev_t)
      )
      (0, None) state#territories 
   in
      result
;;

let greatest_threat state territory =
   let _, result = List.fold_left
      (fun (prev_score, prev_t) t ->
         let score = threat state t in
         match prev_t with
          | None -> (score, Some t)
          | Some prev_t ->
                  if score > prev_score then (score, Some t)
                  else (prev_score, Some prev_t)
      )
      (0, None) territory.Wargame.neighbors
   in
      result
;;

(*
let greatest_threat territory =
   let _, result = List.fold_left
      (fun (prev_score, prev_t) t ->
         let score = threat t in
            if score > prev_score then (score, t)
            else (prev_score, prev_t)
      )
      (0, 0) territory.Wargame.neighbors
   in
      result
;;
*)

let mybot_engine state =
   if state#turn = 0 then state#finish_turn ()
   else
    (
      Wargame.debug "begin turn\n";
      let focus = most_threatened state in
      Wargame.debug "found focus\n";
      begin match focus with | None -> () | Some focus ->
         let myself = state#myself in
         Wargame.debug "found myself\n";
         let to_place = myself.Wargame.armies_to_place in
         Wargame.issue_order_deploy to_place focus;
         let target = greatest_threat state focus in
            begin match target with | None -> () | Some target ->
               Wargame.issue_order_move to_place focus target
            end;
      end;
      state#finish_turn ()
    )
;;

Wargame.loop mybot_engine;;

