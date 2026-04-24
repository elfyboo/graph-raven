module One_hot : sig
  type t
  val dim : t -> int
  val index : t -> int
  val create : dim:int -> index:int -> t  (* raises on invalid *)
  val to_array : t -> int array           (* materialize 0/1 vector *)
end = struct
  type t = { dim : int; index : int }

  let dim t = t.dim
  let index t = t.index

  let create ~dim ~index =
    if dim <= 0 then invalid_arg "dim must be > 0";
    if index < 0 || index >= dim then invalid_arg "index out of bounds";
    { dim; index }

  let to_array t =
    let a = Array.make t.dim 0 in
    a.(t.index) <- 1;
    a
end