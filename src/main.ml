open CamomileLibrary
open Core.Std
       
class read_line ~term ~history ~exit_code = object(self)
  inherit LTerm_read_line.read_line ()
  inherit [Zed_utf8.t] LTerm_read_line.term term

  method show_box = false

  method private exec actions =
    let open Lwt in
    let rec exec' = function
      | [] -> Printf.printf "eval\n"; return self#eval
      | action :: rest -> begin match action with
                                | LTerm_read_line.Accept -> return "acceptance!"
                                | _ -> exec' rest
                          end
    in
    exec' actions

  initializer
  let signal = LTerm_text.of_string "QUERY> " |> React.S.const in
      self#set_prompt signal;
      Zed_macro.add self#macro LTerm_read_line.Accept
end

let () =
  let open Lwt in
  let window = LTerm.create Lwt_unix.stdin Lwt_io.stdin Lwt_unix.stdout Lwt_io.stdout in
  begin
    let monad =
      window >>= fun window ->
      let term = new read_line window [] 0 in
      let size = LTerm.size window in
      let mat = LTerm_draw.make_matrix size in
      let ctx = LTerm_draw.context mat size in
      List.fold [] ~init:0 ~f:(fun current line ->
                               LTerm_draw.draw_string ctx current 0 line;
                               succ current
                              ) |> ignore;
      LTerm_read_line.bind [{LTerm_key.control = true;meta = false;shift = false;
                             code = LTerm_key.Char (UChar.of_char 'm')
                            }]
                           [LTerm_read_line.Accept];
      LTerm.goto window {LTerm_geom.row = 0;col = 0} >>= fun _ ->
      term#run >>= fun s -> Printf.printf "%s" s; return ()
    in 
    Lwt_main.run monad
  end
