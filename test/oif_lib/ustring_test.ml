open Oif_lib
module U = Ustring

let tests =
  [
    ( "do not found when part is empty",
      `Quick,
      fun () ->
        let part = "" and src = "abc" in
        let actual = U.index ~src ~part in
        Alcotest.(check int) "not found" (-1) actual );
    ( "do not found when part is larger than src",
      `Quick,
      fun () ->
        let part = "abcd" and src = "abc" in
        let actual = U.index ~src ~part in
        Alcotest.(check int) "not found" (-1) actual );
    ( "do not found when part is not part of src",
      `Quick,
      fun () ->
        let part = "ef" and src = "abc" in
        let actual = U.index ~src ~part in
        Alcotest.(check int) "not found" (-1) actual );
    ( "get first index if src contains part",
      `Quick,
      fun () ->
        let part = "b" and src = "abc" in
        let actual = U.index ~src ~part in
        Alcotest.(check int) "not found" 1 actual );
    ( "do not first index if partial string in part is found",
      `Quick,
      fun () ->
        let part = "bd" and src = "abc" in
        let actual = U.index ~src ~part in
        Alcotest.(check int) "not found" (-1) actual );
    ( "get first index for Unicode",
      `Quick,
      fun () ->
        let data = [ ("あ", "おいあえう", 2); ("いあ", "おいあえう", 1); ("おいあえう", "おいあえう", 0) ] in
        List.iter
          (fun (part, src, expected) ->
            let actual = U.index ~src ~part in
            Alcotest.(check int) (Printf.sprintf "part is %s" part) expected actual)
          data );
    ( "get length of unicode",
      `Quick,
      fun () ->
        let data = [ ("", 0); ("abc", 3); ("あいうえお", 5); ("mix漢字3日本語", 9) ] in
        List.iter
          (fun (str, expected) ->
            let actual = U.length str in
            Alcotest.(check int) (Printf.sprintf "data is %s" str) expected actual)
          data );
  ]
