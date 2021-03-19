open System

let rec binarySearch (arr: int [], number: int, startIndex, endIndex) =
    let averageIndex = (startIndex + endIndex) / 2
    let middleElement = arr.[averageIndex]

    if startIndex <= endIndex then
        match middleElement with
        | middleElement when middleElement = number -> Some averageIndex
        | middleElement when middleElement > number -> binarySearch (arr, number, startIndex, averageIndex - 1)
        | middleElement when middleElement < number -> binarySearch (arr, number, averageIndex + 1, endIndex)
        | _ -> None
    else
        None

[<EntryPoint>]
let main argv =
    let numbers = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |]

    let binarySearch (arr: int [], number) =
        binarySearch (arr, number, 0, arr.Length - 1)

    let input = int (Console.ReadLine())
    let index = binarySearch (numbers, input)

    if index.IsSome then 
        let value = index |> Option.get
        printfn "%d\n" value
    else
        printf $"None\n"
    0
