let bfs (adj : int list array) (start : int) : int list =
  let v = Array.length adj in
  let visited = Array.make v false in

let rec process_queue queue visited_nodes =
  match queue with
  | [] -> List.rev visited_nodes
  | current :: remaining ->
    let neighbours = adj.(current) in
    let next_queue, _ =
      List.fold_left
        (fun (q, seen) neighbour ->
          if not seen.(neighbour) then begin
            seen.(neighbour) <- true;
            (q @ [neighbour], seen)
          end else
            (q, seen))
        (remaining, visited)
        neighbours
    in
    process_queue next_queue (current :: visited_nodes)
  in

  visited.(start) <- true;
  process_queue [start] []

let add_edge (adj : int list array) u v =
  adj.(u) <- v :: adj.(u);
  adj.(v) <- u :: adj.(v)