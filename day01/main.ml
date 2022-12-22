let read_whole_file (filename: string): string =
  let ch = open_in filename in
  let s = really_input_string ch (in_channel_length ch) in
  close_in ch;
  s

let required_fuel (mass: int): int =
  mass / 3 - 2

let rec required_fuel_rec (mass: int): int =
  let fuel = required_fuel mass in
  if fuel <= 0
    then 0
    else fuel + required_fuel_rec fuel

let solve_part1 (masses: int list): int =
  masses
  |> List.map required_fuel
  |> List.fold_left (+) 0

let solve_part2 (masses: int list): int =
  masses
  |> List.map required_fuel_rec
  |> List.fold_left (+) 0

let solve_file (file_path: string) =
  let input = file_path
    |> read_whole_file
    |> String.split_on_char '\n'
    |> List.filter (fun s -> String.length s != 0)
    |> List.map int_of_string
  in
  input |> solve_part1 |> Printf.printf "Part 1: %d\n";
  input |> solve_part2 |> Printf.printf "Part 2: %d\n"

let () =
  Sys.argv
  |> Array.to_list
  |> List.tl
  |> List.iter solve_file
