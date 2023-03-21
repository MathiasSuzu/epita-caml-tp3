#use "list_tools.ml";;
let rec rigit x list = match list with (* etourne la position de la première occurence de x dans list *)
    [] -> failwith("out of base")
  | e::_ when e = x -> 0
  | _::l -> 1 + rigit x l ;;

let recompose rep base =
  let p = length(base) in
  let rec puissance x n = 
    if n = 0 then 1 else 
      x* puissance x (n-1)
    in let rec recomp rep = match rep with
      [] -> 0
    | e::l -> (rigit e base)*puissance p (length l) + recomp l
  in recomp rep ;;

let decompose x base =
 if x < 0 then invalid_arg("negative number") else 
  let tbase = length(base) in
   let rec decomp x l = match x with
      0 -> l
    | _ -> decomp (x/tbase) (nth (x mod tbase) base::l)
 in decomp x [];;