let unite_lists xs ys = xs @ List.filter (fun x -> not (List.mem x xs)) ys
let difference_lists xs ys = List.filter (fun x -> not (List.mem x ys)) xs
