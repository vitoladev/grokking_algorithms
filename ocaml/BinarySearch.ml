open Printf

let rec binary_search (arr: int array) (number: int) start_index end_index =
  let average_index = (start_index + end_index) / 2 in
  let middle_element = arr.(average_index) in

  if start_index <= end_index then
    match middle_element with
    | middle_element when middle_element = number -> Some average_index
    | middle_element when middle_element > number -> 
      binary_search arr number start_index (average_index - 1)
    | middle_element when middle_element < number -> 
      binary_search arr number (average_index + 1) end_index
    | _ -> None
  else
    None

let () =
  let numbers = [| 1; 2; 3; 4; 5; 6; 7; 8; 9; 10 |] in
  let input = read_int() in

  let binary_search (arr: int array) (number: int) = 
    binary_search arr number 0 (Array.length arr - 1) in

  let index = binary_search (numbers) input in

  match index with 
  | Some index -> printf "%i\n" index
  | None -> print_endline "None"
