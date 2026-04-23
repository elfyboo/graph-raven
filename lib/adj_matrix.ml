type t = bool array array

let create v =
  Array.make_matrix v v false

let add_edge g u v =
  g.(u).(v) <- true;
  g.(v).(u) <- true

let remove_edge g u v =
  g.(u).(v) <- false;
  g.(v).(u) <- false

let has_edge g u v =
  g.(u).(v)