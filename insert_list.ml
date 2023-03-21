#use "list_tools.ml";;

let insert_list list rank sublist =
  if rank < 1 then invalid_arg "insert_list : invalid rank" else
    let rec ins list rank = match (list,rank) with
      ([],e) when e > 1-> failwith "insert_list : list too short"
    | (_,1) -> append sublist list
    | (e::l,_) -> e::(ins l (rank-1))
    | _ -> failwith "insert_list : list too short"
  in ins list rank;;

let insert_list_multiple ml sl list_rank =
  let rec add_all x list = match list with
      [] -> []
    | e::l -> e+x::add_all x l
  in let taille_sl = length sl in
  let rec multiple ml list_rank = match list_rank with
      [] -> ml
    | e::l -> multiple(insert_list ml e sl) (add_all taille_sl l)
  in multiple ml list_rank;;