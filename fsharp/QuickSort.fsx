let rec quickSort (arr: int []) =
    if Array.length arr < 2 then
        arr
    else
        let pivot = arr.[0]
        let arrWithoutPivot = arr |> Array.skip 1

        let smaller =
            quickSort arrWithoutPivot
            |> Array.filter (fun n -> n <= pivot)

        let bigger =
            quickSort arrWithoutPivot
            |> Array.filter (fun n -> n > pivot)

        Array.append (Array.append smaller [| pivot |]) bigger

let numbers = [| 1; 5; 2; 9; 55; 6; 15 |]
let sorted = quickSort numbers
Array.iter (fun n -> printfn "%i" n) sorted
