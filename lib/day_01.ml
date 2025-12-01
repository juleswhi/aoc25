let parse input = input |> String.split_on_char '\n'

let pretty_print n t ws =
    let dir = if n < 0 then 'L' else 'R' in
    "The dial is rotated " ^ (String.make 1 dir) ^ (string_of_int (abs n)) ^
    " to a point at " ^ (string_of_int t) ^ "; during this operation, it points at 0 " ^ (string_of_int ws) ^ " times"
    |> print_endline

let solve_one (xs : string list) =
    xs
    |> List.filter (fun x -> x <> "")
    |> List.map (fun x ->
        let sign = if String.contains x 'R' then 1 else -1 in
        let n = int_of_string (String.sub x 1 ((String.length x) - 1)) in
        sign * n
    )
    |> List.fold_left (fun (acc : int * int) x ->
        let (ws, c) = acc in
        let n = c + x in

        let w =
            let r = (n mod 100) in
            if r < 0 then r + 100 else r
        in

        if w = 0 then (ws + 1, w)
                 else (ws, w)

    ) (0, 50)

let solve_two (xs : string list) =
    let moves =
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
        ) in

    let rec process_moves moves pos total_zeros =
        match moves with
        | [] -> total_zeros
        | move :: rest ->
            let step = if move > 0 then 1 else -1 in
            let rec simulate m_remaining current_pos zeros_so_far =
                if m_remaining = 0 then (current_pos, zeros_so_far)
                else
                    let next_pos = current_pos + step in
                    let next_zeros = if next_pos mod 100 = 0 then zeros_so_far + 1 else zeros_so_far in
                    simulate (m_remaining - 1) next_pos next_zeros
            in
            let (new_pos, new_zeros) = simulate (abs move) pos total_zeros in
            process_moves rest new_pos new_zeros
    in

    process_moves moves 50 0


let part1 input_text = input_text
    |> parse
    |> solve_one
    |> fst
    |> string_of_int

let part2 input_text = input_text
    |> parse
    |> solve_two
    |> string_of_int
