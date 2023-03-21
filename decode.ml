#use "list_tools.ml";;

let rec is_prefix plist list = match list with 
    [] -> false
  | e::l -> if prefix e plist then true 
              else is_prefix plist l ;;

let rec decodable codes = match codes with
    [] | _::[] -> true 
  | e::l -> if is_prefix e l then false
              else decodable l;;