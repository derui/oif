open Oif_lib.Std
module C = Oif_lib.Candidate
module V = Oif_lib.Vector

let tests =
  let candidate_t = Alcotest.testable C.pp C.equal in
  let candidate ~id ~text = C.make ~id ~text:"text" in
  [
    ( "make empty array",
      `Quick,
      fun () ->
        let array = V.empty () in
        Alcotest.(check int) "length" 0 @@ V.length array;
        Alcotest.(check @@ array candidate_t) "text" [||] @@ V.to_array array );
    ( "should be able to push candidate into array",
      `Quick,
      fun () ->
        let candidate = candidate ~id:1 ~text:"text" in
        let array = V.empty () in
        V.push ~value:candidate array;
        Alcotest.(check int) "length" 1 @@ V.length array;
        Alcotest.(check @@ array candidate_t) "text" [| candidate |] @@ V.to_array array );
    ( "should be able to convert sequence to array",
      `Quick,
      fun () ->
        let candidate = candidate ~id:1 ~text:"text" in
        let array = V.empty () in
        Seq.range ~stop:`exclusive 0 10 |> Seq.iter (fun _ -> V.push ~value:candidate array);
        Alcotest.(check int) "length" 10 @@ V.length array;
        Alcotest.(check @@ array candidate_t) "text" (Array.make 10 candidate) (V.to_seq array |> Array.of_seq) );
    ( "should be able to map candidate to candidate",
      `Quick,
      fun () ->
        let array = V.empty () in
        Seq.range ~stop:`exclusive 0 100
        |> Seq.iter (fun id ->
               let candidate = candidate ~id ~text:"text" in
               V.push ~value:candidate array);
        let array = V.map ~f:(fun c -> candidate ~id:(C.id c + 1) ~text:(C.text c)) array in
        let actual = V.empty () in
        Seq.range ~stop:`exclusive 0 100
        |> Seq.iter (fun id ->
               let candidate = candidate ~id:(id + 1) ~text:"text" in
               V.push ~value:candidate actual);
        Alcotest.(check int) "length" 100 @@ V.length array;
        Alcotest.(check @@ array candidate_t) "text" (V.to_seq actual |> Array.of_seq) (V.to_seq array |> Array.of_seq)
    );
    ( "should be able to map candidate to candidate",
      `Quick,
      fun () ->
        let array = V.empty () in
        Seq.range ~stop:`exclusive 0 100
        |> Seq.iter (fun id ->
               let candidate = candidate ~id:(succ id) ~text:"text" in
               V.push ~value:candidate array);
        let actual = ref 0 in
        V.iter ~f:(fun v -> actual := !actual + C.id v) array;
        Alcotest.(check int) "iterator" 5050 !actual );
    ( "should be able to make candidates with specified size",
      `Quick,
      fun () ->
        let array = V.make 2 1 in
        let actual = V.to_array array in
        Alcotest.(check @@ array int) "makes" [| 1; 1 |] actual );
    ( "should be able to append data into array that is made by make function",
      `Quick,
      fun () ->
        let array = V.make 2 1 in
        V.push ~value:2 array;
        let actual = V.to_array array in
        Alcotest.(check @@ array int) "makes" [| 1; 1; 2 |] actual );
    ( "should be able to set value to specified index",
      `Quick,
      fun () ->
        let array = V.make 2 1 in
        V.unsafe_set array 0 10;
        let actual = V.to_array array in
        Alcotest.(check @@ array int) "makes" [| 10; 1 |] actual );
    ( "should be able to iterate with index",
      `Quick,
      fun () ->
        let ret = ref [] in
        let _ = candidate ~id:1 ~text:"text" in
        let array = V.empty () in
        Seq.range ~stop:`exclusive 0 5 |> Seq.iter (fun v -> V.push ~value:(succ v) array);
        V.iteri ~f:(fun i v -> ret := (i, v) :: !ret) array;
        Alcotest.(check int) "length" 5 @@ List.length !ret;
        Alcotest.(check @@ list @@ pair int int) "text" [ (4, 5); (3, 4); (2, 3); (1, 2); (0, 1) ] !ret );
  ]
