open Base
open Stdio
open Str

(* part one *)

type area = {lower: int; upper: int}

let delimiters = Str.regexp {|[, -]+|}

let list_to_areas list = 
    match list with
    | l0 :: h0 :: t -> begin
        let area0 = {lower= Int.of_string l0; upper = Int.of_string h0} in
        match t with
        | l1 :: h1 :: [] -> 
            let area1 = {lower= Int.of_string l1; upper = Int.of_string h1} in
            area0, area1
        | _ -> assert false
    end
    | _ -> assert false

let is_contained' area contained_area =
    if area.lower <= contained_area.lower && area.upper >= contained_area.upper then
        true
    else
        false

let is_contained area0 area1 =
    (is_contained' area0 area1) || (is_contained' area1 area0)

let rec compute_test_areas' channel test_fun points =
    match In_channel.input_line channel with
    | None -> points
    | Some value -> 
        let list_str = Str.split delimiters value in
        let area0, area1 = list_to_areas list_str in
        if test_fun area0 area1 then
            compute_test_areas' channel test_fun (points + 1)
        else 
            compute_test_areas' channel test_fun points 
        
let compute_contained_areas channel =
    compute_test_areas' channel is_contained 0

(* part two *)

let test_overlap' area0 area1 =
    if area0.lower >= area1.lower && area0.lower <= area1.upper then
        true
    else if area0.upper >= area1.lower && area0.upper <= area1.upper then
        true
    else
        false

let test_overlap area0 area1 =
    (test_overlap' area0 area1) || (test_overlap' area1 area0)

let compute_overlap_areas channel =
    compute_test_areas' channel test_overlap 0

(* main *)

let () =
    let points = compute_contained_areas (In_channel.create "nums") in
    printf "Part one score: %s\n" (Int.to_string points) ;
    let points2 = compute_overlap_areas (In_channel.create "nums") in
    printf "Part one score: %s\n" (Int.to_string points2) 
