
module M = Map.Make(String)
module S = Set.Make(String)

let option_map f = function None -> None | Some x -> Some (f x)
let option_iter f = function None -> () | Some x -> f x

let map_fold_left f acc l =
  let acc, rev =
    List.fold_left
      (fun (acc, rev) e -> let acc, e = f acc e in acc, e :: rev)
      (acc, []) l
  in
  acc, List.rev rev

open Format

let rec print_list sep print fmt = function
  | [] -> ()
  | [x] -> print fmt x
  | x :: r -> print fmt x; sep fmt (); print_list sep print fmt r

let space fmt () = fprintf fmt "@ "
let nothing fmt () = ()
let comma fmt () = fprintf fmt ", "
let newline fmt () = fprintf fmt "@\n"

