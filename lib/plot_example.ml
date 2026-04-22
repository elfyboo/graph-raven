open Hugin
open Nx

let run () =
  let x = linspace float32 0. (2. *. Float.pi) 100 in
  let y = Nx.sin x in

  let fig = figure ~width:800 ~height:600 () in
  let ax = subplot fig in
  let _ =
    ax
    |> Plotting.plot ~x ~y ~color:Artist.Color.blue ~label:"sin(x)"
    |> Axes.set_xlabel "x"
    |> Axes.set_ylabel "y"
    |> Axes.set_title "Sine Wave"
  in
  show fig