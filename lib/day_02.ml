let parse input = input |> String.split_on_char '\n'

let rec map_windows f xs =
    match xs with
    | x :: y :: xs -> (f x y) :: map_windows f (y :: xs)
    | _ -> []

let rec any f xs =
    match xs with
    | [] -> false
    | x :: xs -> if f x then true else any f xs

let create_range lo hi =
    let dif = (hi + 1) - lo in
    List.init dif (fun x -> x + lo)

let split_into_parts n s =
    let len = String.length s in
    let base = len / n in
    let extra = len mod n in
    let part_size i = if i < extra then base + 1 else base in
    let rec aux i pos acc =
        if i = n then List.rev acc
        else
            let size = part_size i in
            let chunk = String.sub s pos size in
            aux ( i + 1 ) (pos + size) (chunk :: acc)
    in
    aux 0 0 []

let is_repeating_two s =
    match String.length s with
    | len when len mod 2 = 0 ->
        String.sub s (len / 2) (len / 2) = String.sub s 0 (len / 2)
    | _ -> false

let is_repeating s =
  let len = String.length s in
  let range = create_range 2 len in
  range
  |> List.map (fun x ->
        s
        |> split_into_parts x
        |> map_windows (=)
        |> List.for_all (fun x -> x))
  |> any (fun x -> x)

let solve_one (xs : string list) =
    xs
    |> List.hd
    |> String.split_on_char ','
    |> List.map (fun x -> String.split_on_char '-' x)
    |> List.map (fun x -> int_of_string (List.hd x), (int_of_string (List.hd (List.tl x))))
    |> List.map (fun (hd, tl) -> create_range hd tl)
    |> List.map (fun r -> r |> List.filter (fun x -> x |> string_of_int |> is_repeating_two))
    |> List.map (fun r -> r |> List.fold_left (+) 0)
    |> List.fold_left (+) 0

let solve_two (xs : string list) =
    xs
    |> List.hd
    |> String.split_on_char ','
    |> List.map (fun x -> String.split_on_char '-' x)
    |> List.map (fun x -> int_of_string (List.hd x), (int_of_string (List.hd (List.tl x))))
    |> List.map (fun (hd, tl) -> create_range hd tl)
    |> List.map (fun r -> r |> List.filter (fun x -> x |> string_of_int |> is_repeating))
    |> List.map (fun r -> r |> List.fold_left (+) 0)
    |> List.fold_left (+) 0

let part1 inp = inp
    |> parse
    |> solve_one
    |> string_of_int

let part2 inp = inp
    |> parse
    |> solve_two
    |> string_of_int
