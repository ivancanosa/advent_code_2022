open Base
open Stdio

let rec listHead' list value acc =
    if value <= 0 then acc 
    else match list with
        | [] -> acc
        | h :: t -> listHead' t (value-1) ( h :: acc )

let listHead list value =
    let h = listHead' list value [] in
    List.rev h

let computeTop3 accList  =
    let comp a b = b - a in
    let sortedList = List.sort accList ~compare:comp in
    listHead sortedList 3

let rec sum_lines' topList accFood channel = 
    match In_channel.input_line In_channel.stdin with
    | None -> topList
    | Some "" -> 
            let sum = List.fold_left accFood ~init:0 ~f:(fun x y -> x + y) in
            sum_lines' (computeTop3 (sum :: topList)) [] channel
    | Some value -> 
            sum_lines' topList ((Int.of_string value) :: accFood) channel

let sum_lines channel =
    sum_lines' [] [] channel

let rec print_list list =
    match list with
    | [] -> ()
    | h :: t -> 
            printf "%s " (Int.to_string h); 
            print_list t

let rec listSum list acc =
    match list with
    | [] -> acc
    | h :: t -> listSum t (h + acc)
    
let () =
    let top3List = sum_lines In_channel.stdin in
    let max = listSum top3List 0 in
    print_list top3List ;
    printf "\nMax accumulated from top3: %s\n" (Int.to_string max)


