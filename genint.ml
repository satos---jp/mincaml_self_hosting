

let genint = let c = ref 0 in (fun () -> c := (!c)+1; !c)




