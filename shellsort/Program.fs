module shellmod



let rec chunk inc data = 
    match data with
    | [] -> []
    | data when List.length(data) >= inc -> List.truncate inc data:: (chunk inc (List.skip inc data))
    | data -> [data]



let firsts (lsts: int list list, n:int) : int list =
    List.map (fun (l: int list) -> (if l.Length > n then List.nth l n else System.Int32.MaxValue)) lsts 

let rec fsort (lst: int list) =
    match lst with
    |x when x.Length > 0 ->
                let m = List.min x
                m::fsort(List.filter (fun x-> x > m) lst)
    |x -> x
     
let rec f3 (lsts: int list list, n): int list =
    match n with 
    | n when n = (List.nth lsts 0).Length -> []
    | n -> fsort  (firsts( lsts, n))  @ f3(lsts, n + 1)

let gaps = [5;3;2;1]

let rec f4 lst gaps = 
    match gaps with 
    |h::t -> f4 (f3 ((chunk h lst), 0)) t
    |[] -> List.filter (fun x -> x < System.Int32.MaxValue) lst

let l5 = [62;83;18;53;07;17;95;86;47;69;25;28;99;100;101]
printfn "input is  = %A" l5

let outlst: int list = f4 l5 gaps
printfn "output is = %A" outlst


