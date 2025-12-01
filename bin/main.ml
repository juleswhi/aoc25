let print_answer label answer = String.cat label answer |> print_endline

let () =
    Aoc.Util.read_file "input/day_01.txt" |> Aoc.Day_01.part1 |> print_answer
        "Day 1 Part 1: ";
    Aoc.Util.read_file "input/day_01.txt" |> Aoc.Day_01.part2 |> print_answer
        "Day 1 Part 2: ";
