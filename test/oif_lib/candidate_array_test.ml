open Oif_lib.Std
module L = Oif_lib.Line
module C = Oif_lib.Candidate
module A = Oif_lib.Candidate_array

let tests =
  let candidate_t = Alcotest.testable C.pp C.equal in
  let candidate ~id ~text =
    let line = L.make ~id ~text:"text" in
    C.make ~matched:[ (1, 3) ] line
  in
  [
    ( "make empty array",
      `Quick,
      fun () ->
        let array = A.empty () in
        Alcotest.(check int) "length" 0 @@ A.length array;
        Alcotest.(check @@ array candidate_t) "text" [||] @@ A.to_array array );
    ( "should be able to push candidate into array",
      `Quick,
      fun () ->
        let candidate = candidate ~id:1 ~text:"text" in
        let array = A.empty () in
        A.push ~value:candidate array;
        Alcotest.(check int) "length" 1 @@ A.length array;
        Alcotest.(check @@ array candidate_t) "text" [| candidate |] @@ A.to_array array );
    ( "should be able to convert sequence to array",
      `Quick,
      fun () ->
        let candidate = candidate ~id:1 ~text:"text" in
        let array = A.empty () in
        Seq.range ~stop:`exclusive 0 10 |> Seq.iter (fun _ -> A.push ~value:candidate array);
        Alcotest.(check int) "length" 10 @@ A.length array;
        Alcotest.(check @@ array candidate_t) "text" (Array.make 10 candidate) (A.to_seq array |> Array.of_seq) );
    ( "should be able to map candidate to candidate",
      `Quick,
      fun () ->
        let array = A.empty () in
        Seq.range ~stop:`exclusive 0 100
        |> Seq.iter (fun id ->
               let candidate = candidate ~id ~text:"text" in
               A.push ~value:candidate array);
        let array = A.map ~f:(fun c -> candidate ~id:(C.id c + 1) ~text:(C.text c)) array in
        let actual = A.empty () in
        Seq.range ~stop:`exclusive 0 100
        |> Seq.iter (fun id ->
               let candidate = candidate ~id:(id + 1) ~text:"text" in
               A.push ~value:candidate actual);
        Alcotest.(check int) "length" 100 @@ A.length array;
        Alcotest.(check @@ array candidate_t) "text" (A.to_seq actual |> Array.of_seq) (A.to_seq array |> Array.of_seq)
    );
    ( "should be able to map candidate to candidate",
      `Quick,
      fun () ->
        let array = A.empty () in
        Seq.range ~stop:`exclusive 0 100
        |> Seq.iter (fun id ->
               let candidate = candidate ~id:(succ id) ~text:"text" in
               A.push ~value:candidate array);
        let actual = ref 0 in
        A.iter ~f:(fun v -> actual := !actual + C.id v) array;
        Alcotest.(check int) "iterator" 5050 !actual );
  ]
