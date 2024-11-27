open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type punt =
  { name : string option [@option]
  ; type_ : string option [@option] [@key "type"]
  ; size : int option [@option]
  ; lastModified : int option [@option]
  ; userAgent : string option [@option]
  }
  [@@deriving yojson , show]

type location =
  { latitude : string
  ; longitude : string
  }
  [@@deriving yojson , show]

type item =
  { type_      : string option [@option] [@key "type"]
  ; id         : string option [@option]
  ; text       : string option [@option]
  ; title      : string option [@option]
  ; prompt     : string option [@option]
  ; story      : item list option [@option]
  ; alias      : string option [@option]
  ; choices    : string option [@option]   (* Only in flagmatic? *)
  ; columns    : string list option [@option]
  ; community  : string list option [@option] (* urls, looks like an early roster maybe? *)
(*   ; data       : json blob  *)
  ; dot        : string option [@option]  (* Graphviz related *)
  ; svg        : string option [@option]  (* Graphviz related *)
(*   ; frozen     : point list option [@option] (* map plugin *) *)
  ; size        : string option [@option]  (* Image plugin *)
  ; width       : int option [@option]  (* Image plugin *)
  ; height      : int option [@option]  (* Image plugin *)
  ; url         : string option [@option]  (* Image plugin *)
  ; location    : location option [@option]  (* Image plugin *)
  ; caption      : string option [@option]   (* image plugin *)
  ; source       : string option [@option]  (* image plugin *)
(*   ; source       : string option [@option]  (* Used sparingly in non image cases*) *)

  ; key         : string option [@option]  (* Fivestar plugin *)
(*   ; outline    : outline option [@option]  (* outline plugin, it's a mess, skip it.  *) *)
(*   ; pages    : pages option [@option]  (* importer plugin, it's a mess, skip it.  *) *)
  ; punt     : punt option [@option] (* factory plugin, looks messy *)
  ; site         : string option [@option]  (* fork and reference plugin *)
  ; situated     : string list option [@option]  (* frame plugin *)
  ; slug         : string option [@option]  (* reference plugin *)
  ; stars       : string option [@option]  (* fivestar plugin *)
(*   ; survey       : string option [@option]  (* frame plugin, it's messy *) *)
  ; tile       : string option [@option]  (* map plugin *)
  ; wiki       : string option [@option]  (* paragraph plugin, looks like it could be dropped *)
  ; words       : string option [@option]  (* looks like there was a metrics plugin, looks like it could be dropped *)
  ; zoom       : int option [@option]  (* looks like there was a metrics plugin, looks like it could be dropped *)

(*
  wiki words zoom
*)
  }
  [@@deriving yojson, show]

type journal_error =
  { type_      : string [@key "type"]
  ; msg        : string
  ; response   : string option [@option]
  }
  [@@deriving yojson, show]

type removed_to =
  { page       : string
  }
  [@@deriving yojson, show]

type journal_attribution =
  { page       : string
  ; site       : string option [@option]
  }
  [@@deriving yojson, show]


type journal_item =
  { type_      : string option [@option] [@key "type"]
  ; id         : string option [@option]
  ; item       : item option [@option]
  ; removed_to : removed_to option [@option] [@key "removedTo"] (* part of Remove action *)
  ; order      : string list option [@option]  (* Part of Move action *)
  ; date       : int option [@option]
  ; after      : string option [@option]
  ; error      : journal_error option [@option]

  ; alias      : string option [@option]
  ; caption    : string option [@option]
  ; choices    : string option [@option]   (* Only in flagmatic? *)
  ; columns    : string list option [@option]
  ; community  : string list option [@option] (* urls, looks like an early roster maybe? *)
(*   ; data       : json blob  *)
  ; fork       : string option [@option]
  ; dot        : string option [@option]
  ; svg        : string option [@option]
(*   ; frozen     : point list option [@option] (* map plugin *) *)
  ; size        : string option [@option]  (* Image plugin *)
  ; width       : string option [@option]  (* Image plugin *)
  ; height      : string option [@option]  (* Image plugin *)
  ; source      : string option [@option]  (* image plugin *)
  ; url         : string option [@option]  (* Image plugin *)
(*   ; location    : location option [@option]  (* Image plugin *) *)
  ; key         : string option [@option]  (* Fivestar plugin *)
(*   ; outline    : outline option [@option]  (* outline plugin, it's a mess, skip it.  *) *)
  ; prompt      : string option [@option]
(*   ; punt     : string option [@option] (* factory plugin, looks messy *) *)
  ; site         : string option [@option]  (* fork action and reference plugin *)
  ; situated     : string list option [@option]  (* frame plugin *)
  ; slug         : string option [@option]  (* reference plugin *)
  ; stars       : string option [@option]  (* fivestar plugin *)
(*   ; survey       : string option [@option]  (* frame plugin, it's messy *) *)
  ; tile       : string option [@option]  (* map plugin *)
  ; words       : string option [@option]  (* looks like there was a metrics plugin, looks like it could be dropped *)
  ; attribution : journal_attribution option [@option] (* looks like dragDrop is only way for this tag to appear, should be used more *)
(*   ; certificate : certificate option [@option] (* always with text from mkplugin.sh, probably can be dropped *) *)
(*   ; error : error option [@option] (* Most often associated with a move event, but not sure this should be in the journal. *) *)
  }
  [@@deriving yojson, show]

type fork_page =
  { title      : string
  ; story      : item list option [@option]
  ; journal    : journal_item list option [@option]
  }
  [@@deriving yojson, show]


type action =
  { type_      : string [@key "type"]
  ; id         : string option [@option]
  ; item       : item option [@option]
  ; after      : string option [@option]
  ; order      : string list option [@option]
  ; fork_page  : fork_page option [@key "forkPage"] [@option]
  ; date       : int
  }
  [@@deriving yojson, show]


