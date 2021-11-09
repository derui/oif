type t = {
  prompt : string option;
  migemo_dict_directory : string option;
  query : string option;
  record_event_path : string option;
  replay_event_path : string option;
  print_nul : bool;
}

let empty =
  {
    prompt = None;
    migemo_dict_directory = None;
    query = None;
    record_event_path = None;
    replay_event_path = None;
    print_nul = false;
  }

let construct_option f query migemo_dict_directory prompt record_event_path replay_event_path print_nul =
  f { query; migemo_dict_directory; prompt; record_event_path; replay_event_path; print_nul } |> ignore

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
  let env = Arg.env_var ~doc:"Path of file to record event" @@ default_env "record_event_path" in
  let record_event_path =
    Arg.(value & opt (some string) None & info [ "record_event_path" ] ~env ~doc:"Path to record event of oif")
  in
  let env = Arg.env_var ~doc:"Path of file to replay event" @@ default_env "replay_event_path" in
  let replay_event_path =
    Arg.(value & opt (some string) None & info [ "replay_event_path" ] ~env ~doc:"Path to replay recorded event of oif")
  in
  let print_nul =
    Arg.(
      value & flag
      & info [ "print0" ]
          ~doc:"Print output delimited by ASCII NUL character. (this option only affect multi select option enabled)")
  in
  let term =
    Term.(
      const construct_option $ const f $ query $ migemo_dict_directory $ prompt $ record_event_path $ replay_event_path
      $ print_nul)
  in
  let info' = Term.info ~doc:"finder of OCaml" ~exits:Term.default_exits "oif" in

  Term.(eval (term, info'))

let parse f = parse_options f |> Cmdliner.Term.exit ~term_err:2
