module LR = Oif.Line_separated_reader

let tests =
  let test_with_file strs f =
    let%lwt fname, chan = Lwt_io.open_temp_file () in
    Lwt.finalize
      (fun () ->
        Lwt_list.iter_s (Lwt_io.write_line chan) strs;%lwt
        let%lwt fd = Lwt_unix.(openfile fname [ O_RDONLY ] 0666) in
        f fd)
      (fun () -> Lwt_io.close chan)
  in
  [
    Alcotest_lwt.test_case "should be able to read all lines from file" `Quick (fun _ () ->
        let reader = LR.make () in
        let ret = ref [] in
        let rec taker () =
          let%lwt str = LR.take_candidate reader in
          ret := str :: !ret;
          taker ()
        in
        let%lwt _ =
          test_with_file [ "abc"; "def" ] (fun fd ->
              let fd = Lwt_unix.unix_file_descr fd in
              let reader', closer = LR.read_candidates_async ~fd reader in

              Lwt.async taker;
              Lwt.finalize (fun () -> reader') (fun () -> closer () |> Lwt.return))
        in

        Alcotest.(check @@ list string) "empty" [ "def"; "abc" ] !ret |> Lwt.return);
  ]
