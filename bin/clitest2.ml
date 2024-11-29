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

type journal_add =
  { after_id     : item_id
  ; id           : item_id
  ; item         : Yojson.Safe.t
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
  { item        : Yojson.Safe.t
(*     (error) *)

(*
  ; fork
  ; id
  ; site
  ; source
*)
  }

type journal_edit =
  { item         : Yojson.Safe.t
  ; id          : item_id
(*     (error) *)
(*
  ; fork
  ; site
*)
  }

type journal_fork =
  { item         : Yojson.Safe.t
(*     (error) *)
(*   ; site *)
  }

type journal_move =
  { from_id      : item_id
  ; after_id     : item_id
(*     (error) *)
(*   ; fork *)
(*   ; id *)
  }

type journal_remove =
  { id           : item_id
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

type story_item =
  { id   : item_id
  ; item : Yojson.Safe.t
  }

let pp_story_item (fmt:Format.formatter) (story_item:story_item) : unit =
  let str = Printf.sprintf "{id=%s; item=%s}" (show_item_id story_item.id) (Yojson.Safe.to_string story_item.item) in
    Format.pp_print_string fmt str

(*
let show_story_item (story_item:story_item) : string =
  Printf.sprintf "{id=%s; %s}" (show_item_id story_item.id) (Yojson.Safe.to_string story_item.item)
*)

type story = story_item list [@@deriving show]
let empty_story = []

type page =
  { _title : string
  ; _story : story
  }

let _empty_page = { _title = ""; _story = [] }

let add_id_to_item (add_item_id : item_id) (add_item : Yojson.Safe.t) : Yojson.Safe.t =

  let item_list =
    add_item
    |> Yojson.Safe.Util.to_assoc
    |> List.filter ( function (k,_v) -> k <> add_item_id )
  in
    `Assoc ( ("id", `String add_item_id) :: item_list )


let rec apply_add_transaction_to_story_helper (add_item_id : item_id) (add_item : Yojson.Safe.t) (after_id : item_id) epilog prolog : story =
  match prolog with
  | []     -> { id = add_item_id; item = add_item} :: epilog
  | [hd]   -> { id = add_item_id; item = add_item} :: hd :: epilog
  | hd::tl ->
      begin
        if hd.id = after_id
        then { id = add_item_id; item = add_item} :: hd :: tl @ epilog
        else apply_add_transaction_to_story_helper add_item_id add_item after_id (hd :: epilog) tl
      end

(* if after_id is not found, then appends at end of story *)
let apply_add_transaction_to_story (journal_add:journal_add) (story:story) : story =

  let item' = add_id_to_item journal_add.id journal_add.item in
    story
    |> apply_add_transaction_to_story_helper journal_add.id item' journal_add.after_id []

(* What does it mean to create a story that isn't empty?
*)

let itemId () : string =
  let x1 = Random.full_int Int.max_int in
(*
  let x2 = Random.full_int Int.max_int in
    Printf.sprintf "%08x%08x" x1 x2
*)
    Printf.sprintf "%08x" x1

let apply_create_transaction_to_story (journal_create:journal_create) (story:story) : story =

  let helper prolog epilog : story =
    match prolog with
    | []     -> { id = itemId (); item = journal_create.item} :: epilog
    | _  -> failwith "Creating a story that isn't empty"
  in
    helper story []

let apply_edit_transaction_to_story (journal_edit:journal_edit) (story:story) : story =

  let rec helper prolog epilog : story =
    match prolog with
    | [] -> failwith (Printf.sprintf "Edit transaction didn't find valid id '%s'" journal_edit.id)
    | hd::tl ->
        begin
          if hd.id = journal_edit.id
          then (epilog |> List.rev) @ ( { id = ""; item = journal_edit.item } :: tl )
          else helper tl (hd :: epilog)
        end
  in
    helper story []

(* Fork replaces the story with the forked story *)
let apply_fork_transaction_to_story (journal_fork:journal_fork) (_story:story) : story =
  let item_id = member "id" journal_fork.item |> to_string in
  [ { id   = item_id ; item = journal_fork.item } ]


let apply_remove_transaction_to_story_helper (remove_item_id:item_id) (story:story) : story =
  story
    |> List.filter ( function story_item -> story_item.id <>  remove_item_id )

let apply_remove_transaction_to_story (journal_remove:journal_remove) (story:story) : story =
  apply_remove_transaction_to_story_helper journal_remove.id story

(* Move is the same as remove + add, so we'll reuse those helper functions *)
let apply_move_transaction_to_story (journal_move:journal_move) (story:story) : story =
  let story_item = story |> List.find ( function story_item -> story_item.id = journal_move.from_id ) in
  story
    |> apply_remove_transaction_to_story_helper journal_move.from_id
    |> apply_add_transaction_to_story_helper story_item.id story_item.item journal_move.after_id []



let apply_transaction_to_story (story:story) (story_transaction:story_transaction) : story =
  match story_transaction.action with
  | Add    ja -> apply_add_transaction_to_story    ja story
  | Create jc -> apply_create_transaction_to_story jc story
  | Edit   je -> apply_edit_transaction_to_story   je story
  | Fork   jf -> apply_fork_transaction_to_story   jf story
  | Move   jm -> apply_move_transaction_to_story   jm story
  | Remove jr -> apply_remove_transaction_to_story jr story


let story_from_journal (journal : journal ) : story =
  let compare_story_transactions (j1:story_transaction) (j2:story_transaction) : int = compare j1.date j2.date in
  let transactions = journal |> List.sort compare_story_transactions in
  let story = transactions |> List.fold_left apply_transaction_to_story empty_story in
    story |> List.rev

let _j1 =
  [ { date   = 124
    ; action = Create { item = Yojson.Safe.from_string "{}" }
    }
  ; { date   = 124
    ; action = Add { id = "124"; after_id = "123"; item = Yojson.Safe.from_string "{}" }
    }
  ; { date   = 124
    ; action = Edit { id = "123"; item = Yojson.Safe.from_string "{}" }
    }
  ; { date   = 124
    ; action = Fork { item = Yojson.Safe.from_string "{}" }
    }
  ; { date   = 124
    ; action = Move { from_id = "123"; after_id = "246"; }
    }
  ; { date   = 124
    ; action = Remove { id = "123" }
    }
   ]

let () =

  let journal =
    [ { date   = 124
      ; action = Create { item = Yojson.Safe.from_string {|{ "title": "A new page"}|} }
      }
    ; { date   = 125
      ; action = Add { id = "125"; after_id = "124"; item = Yojson.Safe.from_string {|{ "type": "paragraph", "text":"Welcome to my new page"}|} }
      }
    ; { date   = 126
      ; action = Add { id = "126"; after_id = "125"; item = Yojson.Safe.from_string {|{ "type": "paragraph", "text":"The middle paragraph"}|} }
      }

    ; { date   = 127
      ; action = Add { id = "127"; after_id = "126"; item = Yojson.Safe.from_string {|{ "type": "paragraph", "text":"The second middle paragraph"}|} }
      }
    ; { date   = 128
      ; action = Add { id = "128"; after_id = "127"; item = Yojson.Safe.from_string {|{ "type": "paragraph", "text":"The third middle paragraph"}|} }
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
