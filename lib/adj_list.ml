open Maps

type t = Int_set.t Int_map.t

let empty = Int_map.empty

let add_vertex g u = 
  if Int_map.mem u g then g
  else Int_map.add u Int_set.empty g

let remove_vertex g u =
  Int_map.remove u g
  |> Int_map.map (Int_set.remove u)

let add_edge g u v =
  let add g a b =
    let neighbors = Option.value (Int_map.find_opt a g) ~default:Int_set.empty in
    Int_map.add a (Int_set.add b neighbors) g
  in
  add (add g u v) v u

let remove_edge g u v =
  let remove g a b =
    match Int_map.find_opt a g with
    | None -> g
    | Some s -> Int_map.add a (Int_set.remove b s) g
  in
  remove (remove g u v) v u

let neighbors g u =
  Option.value (Int_map.find_opt u g) ~default:Int_set.empty