let parse _ = [[]]

let calc_1 (parsed : int list list) =
    let sort coll = List.sort compare coll in
    let sorted = List.map sort parsed in
        List.fold_left2
          (fun acc a b -> acc + Int.abs (a - b))
          0 (List.nth sorted 0) (List.nth sorted 1)

let part1 input_text = input_text |> parse |> calc_1 |> string_of_int
