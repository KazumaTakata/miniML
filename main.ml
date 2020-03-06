open Core

let printPrompt () =
  Out_channel.output_string stdout ">>";
  Out_channel.flush stdout

let rec mainLoop () =
  match In_channel.input_line In_channel.stdin with
  | None -> printf ""
  | Some x ->
      Out_channel.output_string stdout (String.concat [ x; "\n" ]);
      Out_channel.flush stdout;
      printPrompt ();
      mainLoop ()

let () =
  printPrompt ();
  mainLoop ()
