module Make (Ord : Map.OrderedType) = struct
  module M = Map.Make (Ord)

  (** Vertex type fixed by the functor parameter. *)
  type v = Ord.t

  (** Persistent adjacency map: v -> neighbors *)
  type t = v list M.t

  let empty = M.empty

  let is_empty = M.is_empty

  let mem_vertex v g =
    M.mem v g

  let add_vertex v g =
    if M.mem v g then g else M.add v [] g

  let add_edge src dst g =
    let ns =
      match M.find_opt src g with
      | Some xs -> xs
      | None -> []
    in
    M.add src (dst :: ns) g

  let neighbors g v =
    match M.find_opt v g with
    | Some xs -> xs
    | None -> []

  let remove_vertex v g =
    M.remove v g

  let remove_edge src dst g =
    match M.find_opt src g with
    | None -> g
    | Some xs ->
      let rec remove_one acc = function
        | [] -> List.rev acc
        | x :: tl when x = dst -> List.rev_append acc tl
        | x :: tl -> remove_one (x :: acc) tl
      in
      let xs' = remove_one [] xs in
      if xs' == xs then g else M.add src xs' g

  let vertices g =
    M.fold (fun v _ acc -> v :: acc) g [] |> List.rev

  let fold_vertices f g acc =
    M.fold (fun v _ acc -> f v acc) g acc

  let fold_edges f g acc =
    M.fold
      (fun src ns acc ->
        List.fold_left (fun acc dst -> f src dst acc) acc ns)
      g acc

  let map_neighbors f g =
    M.mapi (fun v ns -> f v ns) g

  let of_edges es =
    List.fold_left (fun g (s, d) -> add_edge s d g) empty es

  let to_edges g =
    fold_edges (fun s d acc -> (s, d) :: acc) g [] |> List.rev
end