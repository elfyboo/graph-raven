open Bfs
open Adj_list
open Maps

let test_0 () =
  let adj = empty
    |> fun g -> add_edge_to_list g 1 2
    |> fun g -> add_edge_to_list g 1 0
    |> fun g -> add_edge_to_list g 2 0
    |> fun g -> add_edge_to_list g 2 3
    |> fun g -> add_edge_to_list g 2 4
    |> fun g -> add_edge_to_list g 4 8
  in
  let res = bfs adj 0 in
  List.iter (fun x -> print_int x; print_char ' ') res;
  print_newline ()

let output_dot (g : t) (oc : out_channel) =
  Printf.fprintf oc "graph G {\n";
  Int_map.iter (fun u neighbors ->
    Int_set.iter (fun v ->
      if v > u then
        Printf.fprintf oc "  %d -- %d;\n" u v
    ) neighbors
  ) g;
  Printf.fprintf oc "}\n"

let test_1 () =
  let g = empty
    |> fun g -> add_edge_to_list g 1 2
    |> fun g -> add_edge_to_list g 1 0
    |> fun g -> add_edge_to_list g 2 0
    |> fun g -> add_edge_to_list g 2 3
    |> fun g -> add_edge_to_list g 2 4
    |> fun g -> add_edge_to_list g 4 8
  in
  let oc = open_out "graph.dot" in
  output_dot g oc;
  close_out oc;
  ignore (Sys.command "dot -Tpng graph.dot -o graph.png")