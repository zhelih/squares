open Printf

module Set = Set.Make(struct type t = int * int let compare = compare end)

let make_set_f f bound alpha =
  let res = ref Set.empty in
  for i = 1 to (bound-1) do (* can enhance speed here *)
    for j = i to bound do
      if f i j = alpha then
        res := Set.add (i,j) !res
    done
  done;
  !res

let make_set_a alpha = make_set_f (+) (alpha-1) alpha
let make_set_b alpha = make_set_f (fun a b -> a*a + b*b) (alpha-1) alpha

let print_set name s =
  printf "set %s: " name;
  Set.iter (fun (i,j) -> printf "(%d,%d - %d) " i j (i+j)) s;
  print_endline ""

let printfn fmt = ksprintf print_endline fmt

let sq a b = a*a + b*b

let do_task i0 j0 =
  let a0 = make_set_a (i0+j0) in
  let b0 = make_set_b (sq i0 j0) in

  printfn "do_task for %d %d" i0 j0;
  print_set "a0" a0;
  print_set "b0" b0;

  let module H = Hashtbl in

  let h_a = H.create 1 in (* dynamic progamming starts here *)
  let h_b = H.create 1 in

  H.add h_a (i0+j0) a0;
  H.add h_b (sq i0 j0) b0;

  let make_subset f h a_i =
    Set.filter (fun (i,j) ->
      let s = try H.find h (f i j) with _ -> (let s = make_set_b (f i j) in H.add h (f i j) s; s) in
      Set.mem (i,j) s
    ) a_i
  in

  let make_subset_a = make_subset sq h_b in
  let make_subset_b = make_subset (+) h_a in

  let a1 = make_subset_a b0 in print_set "a1" a1;
  let b1 = make_subset_b a1 in print_set "b1" b1;
  let a2 = make_subset_a b1 in print_set "a2" a2;

  ()

let () =
  do_task 8 9
