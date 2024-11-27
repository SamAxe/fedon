open Fedwiki.Types

let () =
  let usage_msg = "./clitest [-verbose] <file1> [<file2>] ... -o <output>" in
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
    ; !input_files |> List.iter ( function in_file ->

    let channel = Stdlib.open_in in_file in
      try
        channel
        |> Yojson.Safe.from_channel
        |> fork_page_of_yojson
        |> yojson_of_fork_page
        |> Yojson.Safe.pretty_to_string
        |> Printf.printf "%s\n"
      with _ -> Printf.eprintf "Caught exception processing %s\n" in_file
    )
