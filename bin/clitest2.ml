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

type item_id = string [@@deriving show]

type fw_story_item = Yojson.Safe.t
type story_item =
  { id      : item_id   (* id and type are lifted from fw_item for convienence, but should be read only and not changed *)
  ; type_   : string
  ; fw_item : fw_story_item
  }

let id_from_fw_item (json_item : Yojson.Safe.t ) =
(*   Printf.printf "keys: %s\n in: %s\n" (Yojson.Safe.Util.keys json_item |> String.concat ", ") (Yojson.Safe.pretty_to_string json_item); *)
  let item = json_item |> member "id" in
  match item with
  | `Null -> None
  | _     -> Some (item |> to_string)

let type_from_fw_item (json_item : Yojson.Safe.t ) =
  let item = json_item |> member "type" in
  match item with
  | `Null -> None
  | _     -> Some (item |> to_string)

let withDefault ( default : string ) ( opt : string option ) : string =
  match opt with
  | None -> default
  | Some x -> x


let generateItemId () : string =
  let x1 = Random.full_int Int.max_int in
    Printf.sprintf "%016x" x1


let fw_story_item_to_story_item (fw_story_item:fw_story_item) : story_item =
  { id      = id_from_fw_item fw_story_item |> withDefault (generateItemId ())
  ; type_   = type_from_fw_item fw_story_item |> withDefault "no type found"
  ; fw_item = fw_story_item
  }

let _story_item_to_fw_story_item (story_item:story_item) : fw_story_item =
  story_item.fw_item

(* Journals are made of story_items because we want to be able to use
   Ocaml's standard library to manipulate journal and story.  Atleast
   for now.  A fw_story_item might make sense after I understand the
   problem a bit better, but for now, conversions and wrapping...
*)
type journal_add =
  { after_id     : item_id
  ; item         : story_item

(*     (error) *)
(*
  ; fork
  ; id           : item_id
  ; site         : string
  ; attribution         : string
  ; certificate         : string
*)
  }

type journal_create =
  { item        : story_item
(*     (error) *)

(*
  ; fork
  ; id
  ; site
  ; source
*)
  }

type journal_edit =
  { item         : story_item
(*     (error) *)
(*
  ; fork
  ; site
*)
  }

type journal_fork =
  { item         : story_item
(*     (error) *)
(*   ; site *)
  }

type journal_move =
  { item      : story_item
  ; after_id  : item_id
(*     (error) *)
(*   ; fork *)
(*   ; id *)
  }

type journal_remove =
  { item      : story_item
(*     (error) *)
(*   ; removedTo *)
  }

type journal_action =
  | Add    of journal_add    [@name "add"]
  | Create of journal_create [@name "create"]
  | Edit   of journal_edit   [@name "edit"]
  | Fork   of journal_fork   [@name "fork"]
  | Move   of journal_move   [@name "move"]
  | Remove of journal_remove [@name "remove"]

type story_transaction =
  { date   : int
  ; action : journal_action
  }

type journal = story_transaction list


let pp_story_item (fmt:Format.formatter) (story_item:story_item) : unit =
  let str = Printf.sprintf "{id=%s; type=%s; item=%s}" (show_item_id story_item.id) story_item.type_ (Yojson.Safe.to_string story_item.fw_item) in
    Format.pp_print_string fmt str

let show_story_item (story_item:story_item) : string =
  Printf.sprintf "{id=%s; %s}" (show_item_id story_item.id) (Yojson.Safe.to_string story_item.fw_item)

type story = story_item list [@@deriving show]
let empty_story = []

(*
type page =
  { _title : string
  ; _story : story
  }
*)

(* let _empty_page = { _title = ""; _story = [] } *)

let _add_id_to_item (add_item_id : item_id) (add_item : Yojson.Safe.t) : Yojson.Safe.t =

  let item_list =
    add_item
    |> Yojson.Safe.Util.to_assoc
    |> List.filter ( function (k,_v) -> k <> add_item_id )
  in
    `Assoc ( ("id", `String add_item_id) :: item_list )

let rec apply_add_transaction_to_story_helper (add_item : story_item) (after_id : item_id) (rprolog:story) (prolog:story) : story =
  if after_id = String.empty
  then add_item :: prolog
  else
    begin
      match prolog with
      | []     -> add_item :: rprolog |> List.rev
      | [hd]   -> add_item :: hd :: rprolog |> List.rev
      | hd::tl ->
          begin
            if hd.id = after_id
            then
              begin
                List.rev rprolog @ ( hd :: add_item :: tl)
              end
            else apply_add_transaction_to_story_helper add_item after_id (hd :: rprolog) tl
          end
    end

(* if after_id is not found, then appends at end of story *)
let apply_add_transaction_to_story (journal_add:journal_add) (story:story) : story =
  let story' =
    story
    |> apply_add_transaction_to_story_helper journal_add.item journal_add.after_id []
  in
(*     Printf.printf "story       = %s\n\n" (story' |> List.map ( function si -> si.id) |> String.concat ", "); *)
    story'

(* What does it mean to create a story that isn't empty?
*)

let apply_create_transaction_to_story (journal_create:journal_create) (story:story) : story =
  Printf.printf "Create story\n\n";
  let helper prolog epilog : story =
    match prolog with
    | []     -> journal_create.item :: epilog
    | _  -> failwith "Creating a story that isn't empty"
  in
    helper story []

let apply_edit_transaction_to_story (journal_edit:journal_edit) (story:story) : story =

  let rec helper prolog epilog : story =
    match prolog with
    | [] -> failwith (Printf.sprintf "Edit transaction didn't find valid id '%s'" (show_story_item journal_edit.item) )
    | hd::tl ->
        begin
          if hd.id = journal_edit.item.id
          then (epilog |> List.rev) @ ( journal_edit.item :: tl )
          else helper tl (hd :: epilog)
        end
  in
    helper story []

(* Fork replaces the story with the forked story *)
let apply_fork_transaction_to_story (journal_fork:journal_fork) (_story:story) : story =
  [ journal_fork.item ]

let apply_remove_transaction_to_story_helper (remove_item_id:item_id) (story:story) : story =
  story
    |> List.filter ( function story_item -> story_item.id <>  remove_item_id )

let apply_remove_transaction_to_story (journal_remove:journal_remove) (story:story) : story =
  apply_remove_transaction_to_story_helper journal_remove.item.id story

(* Move is the same as remove + add, so we'll reuse those helper functions *)
let apply_move_transaction_to_story (journal_move:journal_move) (story:story) : story =
  let story_item = story |> List.find ( function story_item -> story_item.id = journal_move.item.id ) in
  story
    |> apply_remove_transaction_to_story_helper journal_move.item.id
    |> apply_add_transaction_to_story_helper story_item journal_move.after_id []

let apply_transaction_to_story (story:story) (story_transaction:story_transaction) : story =
  try
    match story_transaction.action with
    | Add    ja -> apply_add_transaction_to_story    ja story
    | Create jc -> apply_create_transaction_to_story jc story
    | Edit   je -> apply_edit_transaction_to_story   je story
    | Fork   jf -> apply_fork_transaction_to_story   jf story
    | Move   jm -> apply_move_transaction_to_story   jm story
    | Remove jr -> apply_remove_transaction_to_story jr story
  with _ ->
    Printf.printf "transaction raised exception, not applying\n";
    story


let story_from_journal (journal : journal ) : story =
  let compare_story_transaction_dates (j1:story_transaction) (j2:story_transaction) : int = compare j1.date j2.date in
  let transactions = journal |> List.sort compare_story_transaction_dates in
  let story = transactions |> List.fold_left apply_transaction_to_story empty_story in
    story

let _j1 =
  [ { date   = 124
    ; action = Create { item = Yojson.Safe.from_string "{}" |> fw_story_item_to_story_item }
    }
  ; { date   = 124
    ; action = Add { after_id = "123"; item = Yojson.Safe.from_string "{}" |> fw_story_item_to_story_item }
    }
  ; { date   = 124
    ; action = Edit { item = Yojson.Safe.from_string "{}" |> fw_story_item_to_story_item }
    }
  ; { date   = 124
    ; action = Fork { item = Yojson.Safe.from_string "{}" |> fw_story_item_to_story_item }
    }
  ; { date   = 124
    ; action = Move { item = Yojson.Safe.from_string "{}" |> fw_story_item_to_story_item; after_id = "246" }
    }
  ; { date   = 124
    ; action = Remove { item = Yojson.Safe.from_string "{}" |> fw_story_item_to_story_item }
    }
   ]

let () =
  Random.self_init ();

  let journal =
    [ { date   = 1
      ; action = Create {                item = Yojson.Safe.from_string {|{ "id":"124","type": "create_page", "title": "A new page"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 2
      ; action = Add { after_id = "124"; item = Yojson.Safe.from_string {|{ "id":"128", "type": "paragraph", "text":"The third middle paragraph"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 3
      ; action = Add { after_id = "124"; item = Yojson.Safe.from_string {|{ "id":"127", "type": "paragraph", "text":"The second middle paragraph"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 4
      ; action = Add { after_id = "124"; item = Yojson.Safe.from_string {|{ "id":"126", "type": "paragraph", "text":"The middle paragraph"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 5
      ; action = Add { after_id = "124"; item = Yojson.Safe.from_string {|{ "id":"125", "type": "paragraph", "text":"Welcome to my new page"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 10
      ; action = Remove { item = Yojson.Safe.from_string {|{ "id":"127", "type": "paragraph", "text":"The second middle paragraph"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 11
      ; action = Remove { item = Yojson.Safe.from_string {|{ "id":"128", "type": "paragraph", "text":"The third middle paragraph"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 12
      ; action = Add { after_id = "126"; item = Yojson.Safe.from_string {|{ "id":"128", "type": "paragraph", "text":"The third middle paragraph"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 13
      ; action = Add { after_id = "126"; item = Yojson.Safe.from_string {|{ "id":"127", "type": "paragraph", "text":"The second middle paragraph"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 15
      ; action = Move { after_id = "124"; item = Yojson.Safe.from_string {|{ "id":"125", "type": "paragraph", "text":"Welcome to my new page"}|} |> fw_story_item_to_story_item }
      }
    ; { date   = 16
      ; action = Edit { item = Yojson.Safe.from_string {|{ "id":"x125", "type": "paragraph", "text":"Welcome to my other new page"}|} |> fw_story_item_to_story_item }
      }
     ]
  in
  let story = story_from_journal journal in
    Printf.printf "%s\n" (show_story story)

(*

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
*)
