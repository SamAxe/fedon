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
  { _title : string
  ; _story : Yojson.Safe.t
  ; journal : Yojson.Safe.t
  }

type render_page =
  { title : string
  ; story : Yojson.Safe.t list
  }

let empty_render_page =
  { title = ""
  ; story = []
  }


let get_item (json:Yojson.Safe.t) : Yojson.Safe.t option =
  json |> path [ "item" ]

let get_id (json:Yojson.Safe.t) : Yojson.Safe.t option =
  json |> path [ "id" ]



let get_item_id (json:Yojson.Safe.t) : Yojson.Safe.t option =
  json |> path [ "item"; "id" ]


(* TODO These actions really are creating pages, not stories...so when it's working, come back and revisit
   with page structure and basically replace all the story with page.story
*)

let create_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
  match get_item action_item with
  | None -> page
  | Some item ->
      begin
        let title' =
          match path [ "title" ] item with
          | None -> "no title"
          | Some item -> item |> to_string
        in
          match path ["story"] item with
          | None -> { title = title'; story = item :: page.story }
          | Some story' -> { title = title'; story = [ story' ] }
      end


let add_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
  (* TODO insert in the correct place *)
  match get_item action_item with
  | None -> page
  | Some item -> { page with story = item :: page.story }

let edit_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
  match get_item action_item with
  | None -> page
  | Some subitem ->
      begin
        let subitem_id = get_id subitem in
        let story' =
          page.story
          |> List.map ( function story_item ->
              let story_item_id = get_id story_item in
                if story_item_id = subitem_id
                then subitem
                else story_item
              )
        in
        { page with story = story' }
      end


(* TODO Revisit when this is a page structure *)
let fork_action (page : render_page) (_action_item : Yojson.Safe.t) : render_page =
  page
(*
  match get_item action_item with
  | None -> []
  | Some item ->
      begin
        let _new_title = path [ "title" ] in
          match path ["story"] item with
          | None -> []
          | Some story' -> story' |> to_list
      end

*)
let remove_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
  match get_item_id action_item with
  | None -> page   (* no item.id in action item, so nothing to remove *)
  | Some action_item_id ->
      begin
        let story' =
          page.story
          |> List.filter ( function story_item ->
              match get_id story_item with
              | None -> false
              | Some story_id ->
                  begin
                    if story_id = action_item_id
                    then false
                    else true
                  end
          )
        in
        { page with story = story'}
      end


let move_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =

  let move_action_helper (story:Yojson.Safe.t list) (acc:Yojson.Safe.t list) (order_id:Yojson.Safe.t) : Yojson.Safe.t list =
    match List.find_opt ( function item -> get_id item = get_id order_id ) story with
    | None   -> acc
    | Some i -> i :: acc
  in

  match path [ "item"; "order" ] action_item with
  | None -> page (* no order field, so skip it *)
  | Some order ->
      begin
        let story' =
          order |> to_list
          |> List.fold_left (move_action_helper page.story) []
        in
        { page with story = story' }
      end

let do_action (page : render_page) (item : Yojson.Safe.t) : render_page =
  let action = member "type" item |> to_string in
    match action with
    | "create" -> create_action page item
    | "add"    -> add_action page item
    | "edit"   -> edit_action page item
    | "fork"   -> fork_action page item
    | "move"   -> move_action page item
    | "remove" -> remove_action page item
    |  _ as w -> failwith (Printf.sprintf "Unknown action %s\n" w)

let page_from_journal (journal : Yojson.Safe.t) : render_page =
  let entries =
    journal
    |> to_list
    |> List.sort ( fun item1 item2 ->
        let date1 = path [ "date" ] item1 in
        let date2 = path [ "date" ] item2 in
          compare date1 date2
    )
  in
  let render_page = List.fold_left do_action empty_render_page entries in
    Printf.printf "%s\n%s\n" (render_page.title) (Yojson.Safe.pretty_to_string (`List render_page.story) );
    render_page

let page_of_yojson (json : Yojson.Safe.t) : page =
  { _title    = member "title" json |> to_string
  ; _story   = member "story" json
  ; journal  = member "journal" json
  }

let handle_file (filename : string) =
  let channel = Stdlib.open_in filename in
    try
      let json = Yojson.Safe.from_channel channel in
        let page = page_of_yojson json in
        let _story = page_from_journal page.journal in
          Stdlib.close_in channel
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
