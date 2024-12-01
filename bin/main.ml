
open Fedwiki.Types
open Fedwiki.Actiontypes

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


(*
  add (paragraph)
  edit (paragraph)
  remove (paragraph)
  create (page)
  move (paragraph)
  fork (page)
*)

(* Journal

  date blob_type blob_action blob_payload


  1. play the journal (apply blob_action to blob_payload) e.g. the fold function
  2. render the blob
  (3.) cache the render



  document : item list
  item     : { type, id, blob }


  add    new item      to document
  edit   existing item in document
  remove existing item from document
  move   existing item within document


  *aside* Document could be an html document, or maybe a markdown document, or maybe a Paniolo document
  and then it's a bit about how the parts compose to make the whole, for example, all the items could
  expose their markdown and then the final document could be rendered in to html.  FedWiki has chosen to
  render each item independently and then assemble the renders into the whole.  Composition is related
  to "monads" and company.

*)

let parse_action_edit ( text : string ) : action_edit =
  Dream.log "Edit: Attempting to decode '%s'" text;
  text
  |> Yojson.Safe.from_string
  |> action_edit_of_yojson


let parse_action_add ( text : string ) : action_add =
  Dream.log "Add: Attempting to decode '%s'" text;
  text
  |> Yojson.Safe.from_string
  |> action_add_of_yojson

let parse_action_remove ( text : string ) : action_remove =
  Dream.log "Remove: Attempting to decode '%s'" text;
  text
  |> Yojson.Safe.from_string
  |> action_remove_of_yojson

let parse_action_create_page ( text : string ) : action_create_page =
  Dream.log "Create: Attempting to decode '%s'" text;
  text
  |> Yojson.Safe.from_string
  |> action_create_page_of_yojson

let parse_action_move ( text : string ) : action_move =
  Dream.log "Move: Attempting to decode '%s'" text;
  text
  |> Yojson.Safe.from_string
  |> action_move_of_yojson

let parse_action_fork ( text : string ) : action_fork_page =
  Dream.log "Fork: Attempting to decode '%s'" text;
  text
  |> Yojson.Safe.from_string
  |> action_fork_page_of_yojson


let page_action_handler request =
  let page = Dream.param request "page" in
  let%lwt body = Dream.body request in
    if String.starts_with ~prefix:"action=" body
    then
      begin
        Dream.log "Got an action for %s" page
        ; let decoded_body = body |> Str.global_replace (Str.regexp "+") " " |> (Dream.from_percent_encoded ) in
          Dream.log "  Body is '%s'" (String.sub decoded_body 0 (min (String.length decoded_body) 720))
(*           Dream.log "  Body is '%s'" (String.sub body 0 (min (String.length body) 720)) *)
        ; let decoded_body' = String.sub decoded_body 7 (String.length decoded_body - 7) in
          let action =
          try
            decoded_body'
            |> Yojson.Safe.from_string
            |> action_of_yojson
          with
          | Yojson.Json_error msg ->
              failwith (Printf.sprintf "Terminating input '%s'" msg)
          in
          let action_body = decoded_body' |> Yojson.Safe.from_string in

          begin
            match action.type_ with
            | "edit"   -> begin decoded_body' |> parse_action_edit |> ignore; Fedwiki.V1_actions.handle_file page action_body end
            | "add"    -> begin decoded_body' |> parse_action_add |> yojson_of_action_add |> Yojson.Safe.pretty_to_string |> Dream.log "%s" end
            | "remove" -> begin decoded_body' |> parse_action_remove |> ignore end
            | "create" -> begin decoded_body' |> parse_action_create_page |> ignore end
            | "move"   -> begin decoded_body' |> parse_action_move |> ignore end
            | "fork"   -> begin decoded_body' |> parse_action_fork |> ignore end
            | _      -> Dream.log "Unknown action '%s'" action.type_
          end
(*
          let action_string = show_action action in
          Dream.log "  Action is '%s'" (String.sub action_string 0 (min (String.length action_string) 720))
*)
          ; Dream.html "ok"
      end
    else
      begin
        Dream.log "Got an unknown action for %s" page
        ; Dream.log "  Body is '%s'" body
        ; Dream.html "ok"
      end

let select_page_file (filename:string) : string =
  let base = Filename.basename filename |> Filename.chop_extension in
  let v1_filename = "./server/pages/" ^ base ^ ".v1.json" in
  Dream.log "Checking '%s'" v1_filename;
  if Sys.file_exists v1_filename
  then v1_filename
  else "./server/pages/" ^ filename

let page_handler request =
  let json_page = Dream.param request "json_page" in
    Dream.log "Request for file '%s'" json_page;
  let filename = select_page_file json_page in
  let channel = Stdlib.open_in filename in
  let json = Yojson.Safe.from_channel channel in

  json
  |> Yojson.Safe.to_string
  |> Dream.json


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

(*     ; Dream.get  "/**" (Dream.static "./server/pages") *)
    ; Dream.get  "/:json_page" page_handler
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
