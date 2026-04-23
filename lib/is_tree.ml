open Adj_list
open Adj_matrix
open Maps

let is_list_tree (g : Adj_list.t) : bool =
  let vertex_count = Int_map.cardinal g in
  let edge_count =
    Int_map.fold (fun _ neighbors acc ->
      acc + Int_set.cardinal neighbors) g 0
  in
  edge_count / 2 = vertex_count - 1

let is_matrix_tree (g : Adj_matrix.t) : bool =
  let vertex_count = Array.length g in
  let edge_count =
    Array.fold_left (fun acc row ->
      acc + Array.fold_left (fun a b ->
        if b then a + 1 else a) 0 row
    ) 0 g
  in
  edge_count / 2 = vertex_count - 1