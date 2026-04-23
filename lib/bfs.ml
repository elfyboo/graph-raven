open Adj_list
open Maps

let bfs (adj : Adj_list.t) (start : int) : int list =
  let visited = Hashtbl.create 16 in
  let rec process_queue queue visited_nodes =
    match queue with
    | [] -> List.rev visited_nodes
    | current :: remaining ->
      let neighbours = Adj_list.neighbors adj current |> Int_set.elements in
      let next_queue, _ =
        List.fold_left
          (fun (q, seen) neighbour ->
            if not (Hashtbl.mem seen neighbour) then begin
              Hashtbl.add seen neighbour true;
              (q @ [neighbour], seen)
            end else
              (q, seen))
          (remaining, visited)
          neighbours
      in
      process_queue next_queue (current :: visited_nodes)
  in
  Hashtbl.add visited start true;
  process_queue [start] []