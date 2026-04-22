open Bfs

let () =
  let v = 5 in
  let adj = Array.make v [] in
  add_edge adj 1 2;
  add_edge adj 1 0;
  add_edge adj 2 0;
  add_edge adj 2 3;
  add_edge adj 2 4;
  let res = bfs adj 0 in
  List.iter (fun x -> print_int x; print_char ' ') res;
  print_newline ()