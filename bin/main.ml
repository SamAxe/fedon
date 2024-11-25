
type wiki_server_info =
  { title : string
  ; pages : string list ref
  ; authenticated : bool ref
  ; owned : bool
  ; isOwner : bool
  ; ownedBy : string
  ; seedNeighbors : string
  ; user : string
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
  let%lwt body = Dream.body request in

  let message_object =
    body
    |> Yojson.Safe.from_string
    |> message_object_of_yojson
  in

  `String message_object.message
  |> Yojson.Safe.to_string
  |> Dream.json

*)

let () =
  Dream.run
(*     ~tls:true *)
    ~port:8081
    ~error_handler:Dream.debug_error_handler
    @@ Dream.origin_referrer_check
    @@ Dream.logger
    @@ Dream.memory_sessions
    @@ Dream.router
    [ Dream.get "/" handler
    ; Dream.get "/dialog/style.css" (Dream.from_filesystem "./server/dialog" "dialog.css")   (* This is for debugging only *)
    ; Dream.get "/dialog/" dialog_handler
    ; Dream.get "/logout" logout_handler
    ; Dream.get "/favicon.png" (Dream.from_filesystem "./server/" "favicon.png")
    ; Dream.get "/client.js" (Dream.from_filesystem "./server/" "client.js")
    ; Dream.get "/client.js.map" (Dream.from_filesystem "./server/" "client.js.map")   (* This is for debugging only *)
    ; Dream.get "/security/**" (Dream.static "./server/security")
    ; Dream.get "/system/**" (Dream.static "./server/system")
    ; Dream.get "/images/**" (Dream.static "./server/images")
    ; Dream.get "/plugins/**" (Dream.static "./server/plugins")
    ; Dream.get "/js/**" (Dream.static "./server/js")
    ; Dream.get "/style/**" (Dream.static "./server/style")
    ; Dream.get "/theme/**" (Dream.static "./server/theme")
    ; Dream.get "/view/**" dynamic_view_url
    ; Dream.post "/auth/reclaim/" reclaim_handler

    ; Dream.get "/**" (Dream.static "./server/pages")
    ; Dream.get "/fail"
        (fun _ ->
          raise (Failure "The Web app failed!"))
    ; Dream.get "/bad"
        (fun _ ->
          Dream.empty `Bad_Request)
    ]
