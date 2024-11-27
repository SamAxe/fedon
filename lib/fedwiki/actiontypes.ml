open Ppx_yojson_conv_lib.Yojson_conv.Primitives

open Types

type action_edit =
  { date  : int
  ; type_ : string [@key "type"]

  ; id    : string
  ; item  : item
  }
  [@@deriving yojson, show]

type action_add =
  { date  : int
  ; type_ : string [@key "type"]

  ; id    : string
  ; item  : item
  ; after : string
  }
  [@@deriving yojson, show]

type action_remove =
  { date  : int
  ; type_ : string [@key "type"]

  ; id    : string
  }
  [@@deriving yojson, show]

type action_move =
  { date  : int
  ; type_ : string [@key "type"]

  ; id    : string
  ; order : string list
  }
  [@@deriving yojson, show]

type action_create_page =
  { date  : int
  ; type_ : string [@key "type"]

  ; item  : item
  }
  [@@deriving yojson, show]

type action_fork_page =
  { date  : int
  ; type_ : string [@key "type"]

  ; fork_page  : page [@key "forkPage"]
  }
  [@@deriving yojson, show]


