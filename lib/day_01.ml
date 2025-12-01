let parse input = input |> String.split_on_char '\n'

let max = 100

let inc_if_zero x n = if x = 0 then n + 1 else n

let solve (xs : string list) =
    "Length of list: " ^ (string_of_int (List.length xs)) |> print_endline;
    xs
    |> List.map String.trim
    |> List.filter (fun x -> x <> "")
    |> List.map (fun x ->
        let sign = if String.contains x 'R' then 1 else -1 in
        match String.length x with
        | 0 -> 0
        | 1 -> 1
        | n ->
            let num = int_of_string (String.sub x 1 (n - 1)) in
            sign * num
    )
    |> List.fold_left (fun (acc : int * int) x ->
        let (p, c) = acc in
        let n = c + x in

        let wrapped =
            let r = (n mod 100) in
            if r < 0 then r + 100 else r
        in

        if wrapped = 0 then (p + 1, wrapped)
                 else (p, wrapped)
    ) (0, 50)

let part1 input_text = input_text
    |> parse
    |> solve
    |> (fun x -> let (p, _) = x in p)
    |> string_of_int
