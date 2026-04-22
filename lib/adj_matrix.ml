open Maps

type t = bool Int_map.t Int_map.t

let empty = Int_map.empty

let add_vertex g u =
  if Int_map.mem u g then g
  else Int_map.add u Int_map.empty g

let remove_vertex g u =
  Int_map.remove u g
  |> Int_map.map (Int_map.remove u)

let add_edge g u v =
  let set g a b =
    let row = Option.value (Int_map.find_opt a g) ~default:Int_map.empty in
    Int_map.add a (Int_map.add b true row) g
  in
  set (set g u v) v u

let remove_edge g u v =
  let unset g a b =
    match Int_map.find_opt a g with
    | None -> g
    | Some row -> Int_map.add a (Int_map.remove b row) g
  in
  unset (unset g u v) v u

let has_edge g u v =
  match Int_map.find_opt u g with
  | None -> false
  | Some row -> Option.value (Int_map.find_opt v row) ~default:false