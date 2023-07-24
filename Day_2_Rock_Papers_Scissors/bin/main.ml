open Base
open Stdio

(* Part one *)

type play = 
    | Rock 
    | Paper 
    | Scissors ;;

let play_of_char ch =
    match ch with
    | 'A' -> Rock
    | 'B' -> Paper
    | 'C' -> Scissors
    | 'X' -> Rock
    | 'Y' -> Paper
    | 'Z' -> Scissors
    | _ -> raise (Invalid_argument "Cannot transform that char to a play")

let score_of_play playValue =
    match playValue with
    | Rock -> 1
    | Paper -> 2
    | Scissors -> 3

let score_of_play_outcome playPlayer playEnemy =
    match playPlayer, playEnemy with
    |Rock, Rock -> 3
    |Rock, Paper -> 0
    |Rock, Scissors -> 6
    |Paper, Rock -> 6
    |Paper, Paper -> 3
    |Paper, Scissors -> 0
    |Scissors, Rock -> 0
    |Scissors, Paper -> 6
    |Scissors, Scissors -> 3

let playMatch playPlayer playEnemy =
    (score_of_play playPlayer) + (score_of_play_outcome playPlayer playEnemy)

let slice_string_by_whitespace input_string =
  String.split_on_chars input_string ~on:[' ']

let rec computePlayFromChannelOne' channel points =
    match In_channel.input_line channel with
    | None -> points
    | Some value -> 
        let charList = slice_string_by_whitespace value in
        match charList with
        | [] -> points
        | h0 :: h1 :: [] -> 
            let playEnemy = play_of_char (String.get h0 0) in
            let playPlayer = play_of_char (String.get h1 0) in
            computePlayFromChannelOne' channel (points + playMatch playPlayer playEnemy)
        | _ -> raise (Invalid_argument "The play does not have the two chars")

let computePlayFromChannelOne channel =
    computePlayFromChannelOne' channel 0

(* Part two *)

type playOutcome =
    | Lose 
    | Draw
    | Win ;;

let playOutcome_of_char ch =
    match ch with
    | 'X' -> Lose
    | 'Y' -> Draw
    | 'Z' -> Win
    | _ -> raise (Invalid_argument "Cannot transform that char to a playOutpume")

let computePlay playEnemy playOutcome =
    match playEnemy, playOutcome with
    | Rock, Lose -> Scissors
    | Rock, Draw -> Rock
    | Rock, Win -> Paper
    | Paper, Lose -> Rock
    | Paper, Draw -> Paper
    | Paper, Win -> Scissors
    | Scissors, Lose -> Paper
    | Scissors, Draw -> Scissors
    | Scissors, Win -> Rock

let rec computePlayFromChannelTwo channel points =
    match In_channel.input_line channel with
    | None -> points
    | Some value -> 
        let charList = slice_string_by_whitespace value in
        match charList with
        | [] -> points
        | h0 :: h1 :: [] -> 
            let playEnemy = play_of_char (String.get h0 0) in
            let playOutcome = playOutcome_of_char (String.get h1 0) in
            let playPlayer = computePlay playEnemy playOutcome in
            computePlayFromChannelTwo channel (points + playMatch playPlayer playEnemy)
        | _ -> raise (Invalid_argument "The play does not have the two chars")

let computePlayFromChannelTwo channel =
    computePlayFromChannelTwo channel 0

(* main *)

let () =
    let points = computePlayFromChannelOne (In_channel.create "plays.txt") in
    printf "Part one score: %s\n" (Int.to_string points) ;
    let points2 = computePlayFromChannelTwo (In_channel.create "plays.txt") in
    printf "Part two score: %s\n" (Int.to_string points2)


