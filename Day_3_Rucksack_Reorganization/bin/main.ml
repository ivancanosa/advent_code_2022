open Base
open Stdio

(* Part one *)

let rec head' list value acc =
    if value <= 0 then acc 
    else match list with
        | [] -> acc
        | h :: t -> head' t (value-1) ( h :: acc )

let head list value =
    head' list value [] 

let tail list value =
    let reverse = List.rev list in 
    head' reverse value [] 

let partitionListHalf list =
    let size = List.length list in
    head list (size/2), tail list (size/2)
    
let list_of_string s = List.init (String.length s) ~f:(String.get s)

let point_of_char ch =
    let value = Char.to_int ch in
    if value < 97 then
        value - 96 + 58
    else
        value - 96

let rec computePoints' list points =
    match list with
    | [] -> points
    | h :: t -> computePoints' t (points + point_of_char h)

let computePoints list =
    computePoints' list 0

let rec computePrioritiesOne' channel points =
    match In_channel.input_line channel with
    | None -> points
    | Some value -> 
            let charList = list_of_string value in
            let sack0, sack1 = partitionListHalf charList in
            let set0 = Set.of_list (module Char) sack0 in
            let set1 = Set.of_list (module Char) sack1 in
            let intersec = Set.inter set0 set1 in
            let intersecList = Set.to_list intersec in
            computePrioritiesOne' channel (points + computePoints intersecList)

let computePrioritiesOne channel =
    computePrioritiesOne' channel 0

(* Part two *)

let computePriorityFromGroup (listGroup: string list) =
    let to_set pos =
        let str value = Option.value value ~default:"" in
        Set.of_list (module Char) (list_of_string (str (List.nth listGroup pos))) 
    in
    let s0 = to_set 0 in
    let s1 = to_set 1 in
    let s2 = to_set 2 in
    let intersec0 = Set.inter s0 s1 in
    let intersec1 = Set.inter intersec0 s2 in
    let lst = Set.to_list intersec1 in
    match lst with
    | [] -> raise (Invalid_argument "No common items in group")
    | (h :: _) -> computePoints [h]

let rec computePrioritiesTwo' channel group points =
    match In_channel.input_line channel with
    | None -> points
    | Some value -> 
        match group with
        | [] -> computePrioritiesTwo' channel [value] points
        | _ :: [] -> computePrioritiesTwo' channel (value :: group) points
        | _ :: _ :: [] -> 
            let groupComplete = value :: group in
            let p = computePriorityFromGroup groupComplete in
            computePrioritiesTwo' channel [] (points + p)
        | _ -> raise (Invalid_argument "Group too big")

let computePrioritiesTwo channel =
    computePrioritiesTwo' channel [] 0

(* main *)

let () =
    let points0 = computePrioritiesOne (In_channel.create "sacks.txt") in
    printf "Part one result: %s\n" (Int.to_string points0);
    let points1 = computePrioritiesTwo (In_channel.create "sacks.txt") in
    printf "Part two result: %s\n" (Int.to_string points1)
