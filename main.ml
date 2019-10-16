open Core

let setTermColor =
  Out_channel.output_string stdout "\027[31m" ;
  Out_channel.flush stdout

let rec mainLoop (env : Eval.evalEnvironment) =
  setTermColor ;
  Out_channel.output_string stdout "type in command\n--------\n" ;
  Out_channel.flush stdout ;
  match In_channel.input_line In_channel.stdin with
  | None ->
      failwith "No timezone provided"
  | Some x ->
      let ee = Parse.getTokenList x in
      let parsed = Parse.parseStatement ee in
      let obj, env = Eval.evalStatement parsed env in
      Eval.inspectObject obj ; mainLoop env

let () =
  let env = Eval.genEnvironment in
  mainLoop env
