

(*
   Start with implementing existing FedWiki server actions for updating
   json pages.



   - request an action
   - read the file into page components, title, story, and journal
   - Recreate the story from the journal and
   - apply the action to the story
   - add the action to the journal
   - write the file back out


   Note: there is likely some philosophy about what the story part
   represents in the persistence file.  e.g. it might be a cached version
   of the rendered journal.  Additionally, there is evidence it's a
   cached version of other rendering done by plugins, e.g. graphviz.
   A v2 version should address caching, with posibly a different mechanism.
*)

open Yojson.Safe.Util

type page =
  { title   : string
  ; story   : Yojson.Safe.t
  ; journal : Yojson.Safe.t
  }


let page_of_yojson (json : Yojson.Safe.t) : page =
  { title    = member "title" json |> to_string
  ; story    = member "story" json
  ; journal  = member "journal" json
  }

type render_page =
  { title : string
  ; story : Yojson.Safe.t list
  ; journal : Yojson.Safe.t list
  }

let empty_render_page =
  { title = ""
  ; story = []
  ; journal = []
  }

let get_item (json:Yojson.Safe.t) : Yojson.Safe.t option =
  json |> path [ "item" ]

let create_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
(*   Printf.printf "Create action\n"; *)
  match get_item action_item with
  | None -> page
  | Some item ->
      begin
        let title' =
          match path [ "title" ] item with
          | None -> "missing title"
          | Some item -> item |> to_string
        in
          match path ["story"] item with
          | None        -> { title = title'; story = item :: page.story ; journal = action_item :: page.journal }
          | Some story' ->
              begin
                match story' with
                | `List list -> { title = title'; story = list; journal = action_item :: page.journal }
                | `String _  -> { title = title'; story = []; journal = action_item :: page.journal }
                | _ -> failwith "Unhandled story construct in create item"
              end
      end

(* action_item_id is just a cached value from action_item, for convienence

  Options for if the after id is not found:
    - append to end (try this first)
    - discard it
 *)

let get_after_id (json:Yojson.Safe.t) : Yojson.Safe.t option =
  json |> path [ "after" ]

let get_id (json:Yojson.Safe.t) : Yojson.Safe.t option =
  json |> path [ "id" ]

let is_after (action_item_after_id_string : string) (story_item : Yojson.Safe.t ) : bool =
(*
  Printf.printf "\n --- is after --- %s\n" action_item_after_id_string;
  Printf.printf " story %s\n\n" (Yojson.Safe.pretty_to_string story_item);
*)
  match get_id story_item with
  | None -> false
  | Some story_item_id ->
      begin
        let story_item_id_string = to_string story_item_id in
(*           Printf.printf "compare '%s' with '%s'\n" action_item_after_id_string story_item_id_string; *)
          action_item_after_id_string = story_item_id_string
      end

(* is the current story item the action_item_after_id value?  If it is, then insert the action item after the current story item and construct the list *)
let rec add_action_helper (action_item_after_id : string) ( action_item : Yojson.Safe.t ) ( rprolog : Yojson.Safe.t list ) ( prolog : Yojson.Safe.t list ) : Yojson.Safe.t list =
  match prolog with
  | [] -> begin (* Printf.printf "D "; *) (action_item :: rprolog) |> List.rev end
  | hd::tl ->
      begin
        if is_after action_item_after_id hd
        then begin (* Printf.printf "C "; *) ( (action_item :: hd :: rprolog) |> List.rev) @ tl end
        else add_action_helper action_item_after_id action_item (hd::rprolog) tl
      end

let add_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
(*
  Printf.printf "Add Action\n%s\n" (Yojson.Safe.pretty_to_string action_item);
  Printf.printf "Before\n%s\n" (Yojson.Safe.pretty_to_string (`List page.story));
*)
  (* TODO insert in the correct place *)
  match get_item action_item with
  | None -> begin Printf.printf "Skipping add due to missing item in action"; page end
  | Some action_item_item ->
      begin
        match get_after_id action_item with
        | None ->
            begin
              let page' = { page with story = [ action_item_item ] @ page.story; journal = action_item :: page.journal } in
(*                 Printf.printf "After1\n%s\n\n\n" (Yojson.Safe.pretty_to_string (`List page'.story)); *)
                page'
            end

        | Some action_item_after_id ->
        let action_item_after_id' = action_item_after_id |> to_string in
          let story' = add_action_helper action_item_after_id' action_item_item [] page.story in
(*           Printf.printf "After2\n%s\n\n\n" (Yojson.Safe.pretty_to_string (`List story')); *)
          { page with story = story'; journal = action_item :: page.journal }
      end

let edit_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
(*   Printf.printf "Edit action\n"; *)
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
        { page with story = story'; journal = action_item :: page.journal }
      end


(* TODO Revisit when this is a page structure *)
let fork_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
(*   Printf.printf "Fork action\n"; *)
  { page with journal = action_item :: page.journal }
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
(*
  Printf.printf "Remove Action\n%s\n" (Yojson.Safe.pretty_to_string action_item);
  Printf.printf "Before\n%s\n" (Yojson.Safe.pretty_to_string (`List page.story));
*)
  match get_id action_item with
  | None -> page   (* no item.id in action item, so nothing to remove *)
  | Some action_item_id ->
      begin
        let story' =
          page.story
          |> List.filter ( function story_item ->
              let story_item_id = get_id story_item in
              match story_item_id with
              | None -> false
              | Some story_id ->
                  begin
                    if story_id = action_item_id
                    then false
                    else true
                  end
          )
        in
        let page' = { page with story = story'; journal = action_item :: page.journal } in
(*           Printf.printf "After\n%s\n" (Yojson.Safe.pretty_to_string (`List page'.story)); *)
          page'

      end


let move_action (page : render_page) (action_item : Yojson.Safe.t) : render_page =
(*
  Printf.printf "Move Action\n%s\n" (Yojson.Safe.pretty_to_string action_item);
  Printf.printf "Before\n%s\n" (Yojson.Safe.pretty_to_string (`List page.story));
*)

  let move_action_helper (story:Yojson.Safe.t list) (acc:Yojson.Safe.t list) (order_id:Yojson.Safe.t) : Yojson.Safe.t list =
(*     Printf.printf "Move story \n%s\n" (Yojson.Safe.pretty_to_string (`List story) ); *)
(*
    Printf.printf "order_id \n%s\n" (Yojson.Safe.pretty_to_string order_id );
    Printf.printf "acc \n%s\n" (Yojson.Safe.pretty_to_string (`List acc) );
*)
    match List.find_opt ( function story_item -> get_id story_item = Some order_id) story with
    | None   -> acc
    | Some i -> i :: acc
  in

  match path [ "order" ] action_item with
  | None -> page (* no order field, so skip it *)
  | Some order ->
      begin
        let story' =
          order |> to_list
          |> List.fold_left (move_action_helper page.story) []
          |> List.rev
        in
        let page' = { page with story = story'; journal = action_item :: page.journal } in
(*           Printf.printf "After\n%s\n" (Yojson.Safe.pretty_to_string (`List page'.story)); *)
        page'
      end


let do_action (page : render_page) (item : Yojson.Safe.t) : render_page =

  match member "type" item with
  | `Null -> create_action page item  (* should test for a title element *)
  | `String action ->
      begin
        match action with
        | "create" -> create_action page item
        | "add"    -> add_action page item
        | "edit"   -> edit_action page item
        | "fork"   -> fork_action page item
        | "move"   -> move_action page item
        | "remove" -> remove_action page item
        |  _ as w -> failwith (Printf.sprintf "Unknown action %s\n" w)
      end
  | _ -> failwith "type is some other json struct than missing or string"


let render_page_from_journal (journal : Yojson.Safe.t) : render_page =
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
    render_page

let apply_action_to_render_page (render_page:render_page) (action:Yojson.Safe.t) : render_page =
  do_action render_page action

let yojson_of_render_page (render_page:render_page) : Yojson.Safe.t =
  `Assoc
  [ ("title",   `String render_page.title)
  ; ("story",   `List render_page.story)
  ; ("journal", `List (render_page.journal |> List.rev) )
  ]

let write_results (filename:string) (render_page:render_page) : unit =
(*   let render_page' = { render_page with journal = page.journal} in *)
(*
  let render_page' = render_page in
  let fn_orig = "temp/" ^ filename ^ ".orig.json" in
  let fh_orig = Stdlib.open_out fn_orig in
    Printf.fprintf fh_orig "%s" (yojson_of_page page |> Yojson.Safe.pretty_to_string)
    ; Stdlib.close_out fh_orig
*)
    let fn_new = "./server/pages/" ^ filename ^ ".v1.json" in
      let fh_new = Stdlib.open_out fn_new in
        Printf.fprintf fh_new "%s" (yojson_of_render_page render_page |> Yojson.Safe.pretty_to_string)
        ; Stdlib.close_out fh_new
(*         ; cmds := Printf.sprintf "difft --check-only --skip-unchanged %s %s\n if [[ !$? ]]\nthen\n v -d %s %s\nfi\n" fn_orig fn_new fn_orig fn_new :: !cmds *)


(* Checks for a .v1.json file, and then a .json file and returns None if neither exist

  filename should be just the page name

 *)

let generate_v1_filename (filename:string) : string =
  let base = Filename.basename filename |> Filename.remove_extension in
  let v1_filename = "./server/pages/" ^ base ^ ".v1.json" in
  v1_filename

let select_page_file (filename:string) : string option =
  let orig_filename =  "./server/pages/" ^ filename in
  let v1_filename = generate_v1_filename filename in
  Dream.log "Checking '%s'" v1_filename;
  if Sys.file_exists v1_filename
  then Some v1_filename
  else
    begin
      if Sys.file_exists orig_filename
      then Some orig_filename
      else None
    end




let check_for_create_file_hack (filename : string) (action : Yojson.Safe.t) : string option =

  match member "type" action with
  | `String value ->
      begin
        if value = "create"
        then
          begin
            let v1_fn = generate_v1_filename filename in

            let fh = Stdlib.open_out v1_fn in
              Printf.fprintf fh "%s" (yojson_of_render_page empty_render_page |> Yojson.Safe.pretty_to_string)
              ; Stdlib.close_out fh
              ; Some v1_fn
          end
        else None
      end
  | _ -> None

let handle_file (filename : string) (action : Yojson.Safe.t) : unit =
  let filename' =
    match check_for_create_file_hack filename action  with
    | Some fn -> fn
    | None    -> generate_v1_filename filename

  in
  let channel = Stdlib.open_in filename' in
    try
      let json = Yojson.Safe.from_channel channel in
        let page = page_of_yojson json in

        let render_page = render_page_from_journal page.journal in
          let render_page' = apply_action_to_render_page render_page action in
          Stdlib.close_in channel
          ; let fname = Filename.basename filename in
            write_results fname render_page'
    with e ->
      Stdlib.close_in_noerr channel
      ; Printf.eprintf "Exception in '%s'\n%s\n" filename (Printexc.to_string e)


