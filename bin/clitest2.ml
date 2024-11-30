open Yojson.Safe.Util

let _show_keys json =
    Yojson.Safe.Util.keys json |> List.sort compare |> String.concat "," |> Printf.printf "%s\n"

let _show_type json =
  let type_ = member "type" json |> to_string in
  let keys_ = keys json in
    Printf.printf "%s -- %s\n" type_ (String.concat ", " keys_)


let _handle_file (filename:string) : unit =
  let channel = Stdlib.open_in filename in
  let json = channel |> Yojson.Safe.from_channel in
(*     Yojson.Safe.Util.keys json |> List.sort compare |> String.concat "," |> Printf.printf "%s\n" *)
  try
    let journal = member "journal" json in
      to_list journal |> List.iter _show_type
    ; Stdlib.close_in channel
  with _ ->
    Stdlib.close_in_noerr channel


(* toplevel keys:
   - title
   - story
   - journal
   - metrics
   - assets

type action =
  ; fork_page  : page option [@key "forkPage"] [@option]
  ; date       : int
  }

   Journal item types:
   # type = add
    after
    attribution
    date
    (error)
    fork
    id
    item
    site
    certificate

   # type = create
    date
    (error)
    fork
    id
    item
    site
    source

   # type = edit
    date
    (error)
    fork
    id
    item
    site

   # type = fork
    date
    (error)
    site

   # type = move

    date
    (error)
    fork
    id
    order

   # type = remove
    date
    (error)
    id
    removedTo

*)

type page =
  { title : string
  ; _story : Yojson.Safe.t
  ; journal : Yojson.Safe.t
  }

let create_action (story : Yojson.Safe.t list) (item : Yojson.Safe.t) : Yojson.Safe.t list =
  item :: story

let add_action (story : Yojson.Safe.t list) (item : Yojson.Safe.t) : Yojson.Safe.t list =
  item :: story

let get_item_id (json:Yojson.Safe.t) : Yojson.Safe.t option =
  json |> path [ "item"; "id" ]


let edit_action (story : Yojson.Safe.t list) (new_item : Yojson.Safe.t) : Yojson.Safe.t list =
  let new_item_id = member "id" new_item |> to_string in
    story
    |> List.map ( function story_item ->
        Printf.printf "%s\n" (Yojson.Safe.pretty_to_string story_item);
        match get_item_id story_item with
        | None -> story_item
        | Some story_id ->
            begin
              if (story_id |> to_string) = new_item_id
              then new_item
              else story_item
            end
    )



let do_action (acc : Yojson.Safe.t list) (item : Yojson.Safe.t) : Yojson.Safe.t list =
  let action = member "type" item |> to_string in
    match action with
    | "create" -> create_action acc item
    | "add"    -> add_action acc item
    | "edit"   -> edit_action acc item
    | "fork"   -> acc
    | "move"   -> acc
    | "remove" -> acc
    |  _ as w -> failwith (Printf.sprintf "Unknown action %s\n" w)

let story_from_journal (journal : Yojson.Safe.t) : Yojson.Safe.t =
  let entries = journal |> to_list in
  let story = `List (List.fold_left do_action [] entries |> List.rev ) in
(*     Printf.printf "%s\n" (Yojson.Safe.pretty_to_string story); *)
    story

let page_of_yojson (json : Yojson.Safe.t) : page =
  { title    = member "title" json |> to_string
  ; _story   = member "story" json
  ; journal  = member "journal" json
  }

let handle_file (filename : string) =
  let channel = Stdlib.open_in filename in
    try
      let json = Yojson.Safe.from_channel channel in
        let page = page_of_yojson json in
        let _story = story_from_journal page.journal in
          Printf.printf "title = '%s'\n" page.title
        ; Stdlib.close_in channel
    with e ->
      Stdlib.close_in_noerr channel
      ; Printf.eprintf "Exception in '%s'\n%s\n" filename (Printexc.to_string e)


let () =
  let usage_msg = "./clitest2 [-verbose] <file1> [<file2>] ... -o <output>" in
  let verbose = ref false in
  let input_files = ref [] in
  let output_file = ref "" in

  let anon_fun filename =
    input_files := filename::!input_files
  in

  let speclist =
    [("-verbose", Arg.Set verbose, "Output debug information");
     ("-o", Arg.Set_string output_file, "Set output file name")]
  in

    Arg.parse speclist anon_fun usage_msg
    ; !input_files |> List.iter handle_file
