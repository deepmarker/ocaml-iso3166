include Iso3166_base

let info t = infos.(Obj.magic (t : t))

let of_f f x =
  let rec loop i =
    if String.equal (f infos.(i)) x then (Obj.magic i : t) else loop (succ i)
  in
  try Some (loop 0) with
  | Invalid_argument _ -> None
;;

let of_f_exn f x =
  let rec loop i =
    if String.equal (f infos.(i)) x then (Obj.magic i : t) else loop (succ i)
  in
  try loop 0 with
  | _ -> failwith x
;;

let to_f f x = f (info x)
let of_name = of_f (fun x -> x.name)
let of_name_exn = of_f_exn (fun x -> x.name)
let of_alpha3 = of_f (fun x -> x.alpha3)
let of_alpha3_exn = of_f_exn (fun x -> x.alpha3)
let to_alpha3 = to_f (fun x -> x.alpha3)
let to_name = to_f (fun x -> x.name)
