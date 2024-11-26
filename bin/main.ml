open Ppx_yojson_conv_lib.Yojson_conv.Primitives

type wiki_server_info =
  { title         : string
  ; pages         : string list ref
  ; authenticated : bool ref
  ; owned         : bool
  ; isOwner       : bool
  ; ownedBy       : string
  ; seedNeighbors : string
  ; user          : string
  }

let wiki_server_info =
  { title         = "Fedon"
  ; pages         = ref [ "welcome-visitors" ]
  ; authenticated = ref false
  ; owned         = true
  ; isOwner       = true
  ; ownedBy       = "two bits"
  ; seedNeighbors = ""
  ; user          = "two bits"
  }


let static_page  _req =
  let open Dream_html in
  let open HTML in
    html [class_ "no-js"]
      [ head []
          [ title [] "%s" wiki_server_info.title
          ; meta [content "text/html"; charset "UTF-8"; http_equiv `content_type]
          ; meta [content "width=device-width, height=device-height, initial-scale=1.0, user-scalable=no"; name "viewport"]
          ; link [id "favicon"; href "/favicon.png"; rel "icon"; type_ "image/png"]

          ; link [href "/style/style.css"; rel "stylesheet"; type_ "text/css"; media "screen"]
          ; link [href "/theme/style.css"; rel "stylesheet"; type_ "text/css"; media "screen"]
          ; link [href "/style/print.css"; rel "stylesheet"; type_ "text/css"; media "print"]
          ; link [href "/js/jquery-ui/1.13.2/jquery-ui.min.css"; rel "stylesheet"; type_ "text/css"]

          ; script [src "/js/jquery-3.6.3.min.js"; type_ "text/javascript"] ""
          ; script [src "/js/jquery-migrate-3.4.0.min.js"; type_ "text/javascript"] ""
          ; script [src "/js/jquery-ui/1.13.2/jquery-ui.min.js"; type_ "text/javascript"] ""
          ; script [src "/js/jquery.ui.touch-punch.min.js"; type_ "text/javascript"] ""
          ; script [src "/js/underscore-min.js"; type_ "text/javascript"] ""

          ; script [src "/client.js"; type_ "text/javascript"] ""
        ]
      ; body []
        [ section [ class_ "main"]
          (
            List.map ( fun page ->
              div [class_ "page"; id "%s" page; tabindex (-1); attr "{{{origin}}}"; attr "{{{generated}}}" ]
              [ div [ class_ "paper"]
                [ txt "{{{story}}}"
                ]
              ]
            ) !(wiki_server_info.pages)
          )
        ; footer []
          [ div [ id "site-owner"; class_ "footer-item"]
            [ (* txt "{{#owned}}" *)
              txt "Site Owned by: "
            ; span [id "site-owner"; style_ "text-transform:capitalize;"] [ txt "%s" wiki_server_info.ownedBy ]
(*             ; txt "{{/owned}}" *)
            ]

            ; div [id "security"; class_ "footer-item"] []

            ; span [class_ "searchbox"; class_ "footer-item"]
              [ input [ class_ "search"; name "search"; type_ "text"; placeholder "Search"]
              ; span [class_ "pages"] []
              ]

            ; span [class_ "neighborhood"] []
          ]
        ; script []
        {|
          var isAuthenticated = ("%b" === "true");
          var isClaimed = ("%b" === "true");
          var isOwner = ("%b" === "true");
          var ownerName = "%s";
          var seedNeighbors = "%s";
          var user = "%s"
          wiki.security(user);
         |}
          !(wiki_server_info.authenticated)
          wiki_server_info.owned
          wiki_server_info.isOwner
          wiki_server_info.ownedBy
          wiki_server_info.seedNeighbors
          wiki_server_info.user
        ]
      ]

let dialog_static_page  _req =
  let open Dream_html in
  let open HTML in
    html [class_ "no-js"]
      [ head []
        [ title [] ""
        ; link [id "favicon"; href "/favicon.png";rel "icon";type_"image/png"]
        ; link [href "/dialog/style.css";rel "stylesheet"]
        ; script []
          {|
            window.addEventListener('message', event => {
              const data = event.data
              console.log('data', data)
              document.title = data.title
              document.querySelector('.page').innerHTML = data.body
            })
          |}
        ]
      ; body []
        [ div [class_"page"] []
        ]
    ]



let handler req = Dream_html.respond (static_page req)
let dialog_handler req = Dream_html.respond (dialog_static_page req)

let dynamic_view_url req =
  let target = Dream.target req in
  let fields = String.split_on_char '/' (String.trim target) |> List.filter ( function f -> (f = "view" || (String.trim f) = "") |> not ) in
    wiki_server_info.pages := fields
    ; Dream.log "Requesting %d pages in: '%s'" (List.length fields) (String.concat "|" fields)
    ; handler req

let logout_handler req =
  wiki_server_info.authenticated := false
  ; Dream_html.respond (static_page req)

let reclaim_handler request =
  let%lwt body = Dream.body request in
  Dream.log "Comparing '%s' with '1234'" body;
    if body = "1234"
    then begin wiki_server_info.authenticated := true ; Dream_html.respond (static_page request) end
    else begin Dream_html.respond ~status:`Unauthorized (static_page request) end

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
  ; width       : string option [@option]  (* Image plugin *)
  ; height      : string option [@option]  (* Image plugin *)
  ; url         : string option [@option]  (* Image plugin *)
(*   ; location    : location option [@option]  (* Image plugin *) *)
  ; caption      : string option [@option]   (* image plugin *)
  ; source       : string option [@option]  (* image plugin *)
(*   ; source       : string option [@option]  (* Used sparingly in non image cases*) *)

  ; key         : string option [@option]  (* Fivestar plugin *)
(*   ; outline    : outline option [@option]  (* outline plugin, it's a mess, skip it.  *) *)
(*   ; pages    : pages option [@option]  (* importer plugin, it's a mess, skip it.  *) *)
(*   ; punt     : string option [@option] (* factory plugin, looks messy *) *)
  ; site         : string option [@option]  (* fork and reference plugin *)
  ; situated     : string list option [@option]  (* frame plugin *)
  ; slug         : string option [@option]  (* reference plugin *)
  ; stars       : string option [@option]  (* fivestar plugin *)
(*   ; survey       : string option [@option]  (* frame plugin, it's messy *) *)
  ; tile       : string option [@option]  (* map plugin *)
  ; wiki       : string option [@option]  (* paragraph plugin, looks like it could be dropped *)
  ; words       : string option [@option]  (* looks like there was a metrics plugin, looks like it could be dropped *)

(*
  wiki words zoom
*)
  }
  [@@deriving yojson, show]

type journal_error =
  { type_      : string [@key "type"]
  ; msg        : string
  ; response   : string
  }
  [@@deriving yojson, show]

type removed_to =
  { page       : string
  }
  [@@deriving yojson, show]

type journal_item =
  { type_      : string option [@option] [@key "type"]
  ; id         : string option [@option]
  ; item       : item option [@option]
  ; removed_to : removed_to option [@option] [@key "removedTo"] (* part of Remove action *)
  ; order      : string list option [@option]  (* Part of Move action *)
  ; date       : int
  ; after      : string option [@option]
  ; error      : journal_error option [@option]

  ; alias      : string option [@option]
  ; caption    : string option [@option]
  ; choices    : string option [@option]   (* Only in flagmatic? *)
  ; columns    : string list option [@option]
  ; community  : string list option [@option] (* urls, looks like an early roster maybe? *)
(*   ; data       : json blob  *)
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
(*   ; attribution : attribution option [@option] (* looks like dragDrop is only way for this tag to appear, should be used more *) *)
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

(*
  add
  edit
  remove
  create
  move
  fork
*)

let page_action_handler request =
  let page = Dream.param request "page" in
  let%lwt body = Dream.body request in
    if String.starts_with ~prefix:"action=" body
    then
      begin
        Dream.log "Got an action for %s" page
        ; let _decoded_body = (Dream.from_percent_encoded body) in
(*           Dream.log "  Body is '%s'" (String.sub decoded_body 0 (min (String.length decoded_body) 720)) *)
          Dream.log "  Body is '%s'" (String.sub body 0 (min (String.length body) 720))
        ; let action =
          try
            String.sub body 7 (String.length body - 7) |> Dream.from_percent_encoded
            |> Yojson.Safe.from_string
            |> action_of_yojson
          with
          | Yojson.Json_error msg ->
              failwith (Printf.sprintf "Terminating input '%s'" msg)
        in
          let action_string = show_action action in
          Dream.log "  Action is '%s'" (String.sub action_string 0 (min (String.length action_string) 720))
          ; Dream.html "ok"
      end
    else
      begin
        Dream.log "Got an unknown action for %s" page
        ; Dream.log "  Body is '%s'" body
        ; Dream.html "ok"
      end

let () =
  Dream.run
(*     ~tls:true *)
    ~port:8081
    ~error_handler:Dream.debug_error_handler
    @@ Dream.origin_referrer_check
    @@ Dream.logger
    @@ Dream.memory_sessions
    @@ Dream.router
    [ Dream.get  "/" handler
    ; Dream.get  "/dialog/style.css" (Dream.from_filesystem "./server/dialog" "dialog.css")   (* This is for debugging only *)
    ; Dream.get  "/dialog/" dialog_handler
    ; Dream.get  "/logout" logout_handler
    ; Dream.get  "/favicon.png" (Dream.from_filesystem "./server/" "favicon.png")
    ; Dream.get  "/client.js" (Dream.from_filesystem "./server/" "client.js")
    ; Dream.get  "/client.js.map" (Dream.from_filesystem "./server/" "client.js.map")   (* This is for debugging only *)
    ; Dream.get  "/security/**" (Dream.static "./server/security")
    ; Dream.get  "/system/**" (Dream.static "./server/system")
    ; Dream.get  "/images/**" (Dream.static "./server/images")
    ; Dream.get  "/plugins/**" (Dream.static "./server/plugins")
    ; Dream.get  "/js/**" (Dream.static "./server/js")
    ; Dream.get  "/style/**" (Dream.static "./server/style")
    ; Dream.get  "/theme/**" (Dream.static "./server/theme")
    ; Dream.get  "/view/**" dynamic_view_url
    ; Dream.post "/auth/reclaim/" reclaim_handler
    ; Dream.put  "/page/:page/action" page_action_handler

    ; Dream.get  "/**" (Dream.static "./server/pages")
    ; Dream.get  "/fail"
        (fun _ ->
          raise (Failure "The Web app failed!"))
    ; Dream.get "/bad"
        (fun _ ->
          Dream.empty `Bad_Request)
    ]


(*
  * The /dialog/ seems like it's very hacky and not sure it needs to hit the server for a popup window.
  * The JSON action structure is very dynamic and may have better consistency if there were individual structures for each command.
  * Mixing the serving of client files with the json pages/slugs is a bit confusing.  Routing
  * The static page is doing more client initialization than it seems like it should.
*)
