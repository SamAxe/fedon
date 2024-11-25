
type page_info =
  { title : string
  ; pages : string list
  ; authenticated : bool
  ; owned : bool
  ; isOwner : bool
  ; ownedBy : string
  ; seedNeighbors : string
  ; user : string
  }

let static_page (info:page_info) _req =
  let open Dream_html in
  let open HTML in
    html [class_ "no-js"]
      [ head []
          [ title [] "%s" info.title
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
            ) info.pages
          )
        ; footer []
          [ div [ id "site-owner"; class_ "footer-item"]
            [ (* txt "{{#owned}}" *)
              txt "Site Owned by: "
            ; span [id "site-owner"; style_ "text-transform:capitalize;"] [ txt "%s" info.ownedBy ]
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
          info.authenticated
          info.owned
          info.isOwner
          info.ownedBy
          info.seedNeighbors
          info.user
        ]
      ]

let make_page_info () =
  { title = "Welcome Visitors"
  ; pages  = [ "welcome-visitors" ]
  ; authenticated = true
  ; owned = true
  ; isOwner = true
  ; ownedBy = "two bits"
  ; seedNeighbors = ""
  ; user = "two bits"
  }



let handler page_info req = Dream_html.respond (static_page page_info req)

let dynamic_view_url req =
  let page_info = make_page_info () in
  let target = Dream.target req in
  let fields = String.split_on_char '/' (String.trim target) |> List.filter ( function f -> (f = "view" || (String.trim f) = "") |> not ) in
  let page_info' = { page_info with pages = fields } in
    Dream.log "Requesting %d pages in: '%s'" (List.length fields) (String.concat "|" fields)
    ; handler page_info' req


let () =
  let info = make_page_info () in
  Dream.run
(*     ~tls:true *)
    ~port:8081
    ~error_handler:Dream.debug_error_handler
    @@ Dream.origin_referrer_check
    @@ Dream.logger
    @@ Dream.memory_sessions
    @@ Dream.router
    [ Dream.get "/" (handler info)
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
    ; Dream.get "/**" (Dream.static "./server/pages")
    ; Dream.get "/fail"
        (fun _ ->
          raise (Failure "The Web app failed!"))
    ; Dream.get "/bad"
        (fun _ ->
          Dream.empty `Bad_Request)


    ]
