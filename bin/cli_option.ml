type t = {
  prompt : string option;
  migemo_dict_directory : string option;
  query : string option;
  record_event_path : string option;
  replay_event_path : string option;
  height : int option;
}

let empty =
  {
    prompt = None;
    migemo_dict_directory = None;
    query = None;
    record_event_path = None;
    replay_event_path = None;
    height = None;
  }

let construct_option f query migemo_dict_directory prompt record_event_path replay_event_path height =
  f { query; migemo_dict_directory; prompt; record_event_path; replay_event_path; height } |> ignore

let default_env vname = "OIF_DEFAULT_" ^ String.uppercase_ascii vname

let parse_options f =
  let open Cmdliner in
  let env = Arg.env_var ~doc:"Initial query" @@ default_env "query" in
  let query = Arg.(value & opt (some string) None & info [ "q"; "query" ] ~env ~doc:"Initial query") in
  let env = Arg.env_var ~doc:"Directory of migemo dictionary" @@ default_env "migemo_dict_directory" in
  let migemo_dict_directory =
    Arg.(value & opt (some dir) None & info [ "migemo_dict_directory" ] ~env ~doc:"Directory of migemo directory")
  in
  let env = Arg.env_var ~doc:"Prompt of input (Default is 'QUERY> ')" @@ default_env "prompt" in
  let prompt =
    Arg.(value & opt (some string) None & info [ "prompt" ] ~env ~doc:"Prompt of input (Default is 'QUERY> ')")
  in
  let record_event_path =
    Arg.(value & opt (some string) None & info [ "record_event_path" ] ~env ~doc:"Path to record event of oif")
  in
  let replay_event_path =
    Arg.(value & opt (some string) None & info [ "replay_event_path" ] ~env ~doc:"Path to replay recorded event of oif")
  in
  let height_conv =
    ( (fun v ->
        let v = int_of_string_opt v in
        match v with
        | Some v -> if v < 1 then Error (`Msg "must be positive") else Ok v
        | None   -> Error (`Msg "not integer")),
      fun fmt v -> Format.fprintf fmt "%d" v )
  in
  let height =
    Arg.(value & opt (some @@ conv height_conv) None & info [ "height" ] ~env ~doc:"request height (Percent)")
  in

  let term =
    Term.(
      const construct_option $ const f $ query $ migemo_dict_directory $ prompt $ record_event_path $ replay_event_path
      $ height)
  in
  let info' = Term.info ~doc:"finder of OCaml" ~exits:Term.default_exits "oif" in

  Term.(eval (term, info'))

let parse f = parse_options f |> Cmdliner.Term.exit ~term_err:2
