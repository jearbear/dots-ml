open Core

let ignored_files = [".git"; "README.md"]
let home_dir = Sys.home_directory ()
let default_store_path = Filename.concat home_dir ".dotfiles"

type dotfile_state =
  | Installed
  | Uninstalled
  | Blocked

type dotfile =
  { name : string
  ; store_path : string
  ; target : string
  ; source : string
  ; state : dotfile_state }

let dotfile_from_name store_path name =
  let read_symlink path = Option.try_with (fun () -> Unix.readlink path) in
  let target = Filename.concat home_dir "." ^ name in
  let source = Filename.concat store_path name in
  let state =
    if Sys.file_exists target = `Yes
    then if read_symlink target = Some source then Installed else Blocked
    else Uninstalled
  in
  {name; store_path; target; source; state}
;;

let rec dir_contents dir =
  if List.exists ignored_files ~f:(( = ) (Filename.basename dir))
  then []
  else
    match Sys.is_directory dir with
    | `Yes ->
      Sys.ls_dir dir
      |> List.map ~f:(fun dir' -> dir_contents (Filename.concat dir dir'))
      |> List.concat
    | `No -> [dir]
    | `Unknown -> []
;;

let all_dotfiles store_path =
  let dotfiles =
    dir_contents store_path
    |> List.map ~f:(String.chop_prefix_exn ~prefix:(store_path ^ "/"))
  in
  dotfiles
  |> List.sort ~compare:String.compare
  |> List.map ~f:(dotfile_from_name store_path)
;;

let list_dotfiles store_path =
  let store_path = Option.value store_path ~default:default_store_path in
  let dotfiles = all_dotfiles store_path in
  Printf.printf
    "Printing dotfiles from %s\nLegend: [x] installed, [-] blocked, [ ] uninstalled\n\n"
    store_path;
  List.iter dotfiles ~f:(fun df ->
      let string_state =
        match df.state with
        | Installed -> "[x]"
        | Uninstalled -> "[ ]"
        | Blocked -> "[-]"
      in
      Printf.printf "%s %s\n" string_state df.name )
;;

let fetch_dotfile store_path path =
  let store_path = Option.value store_path ~default:default_store_path in
  let dotfiles = all_dotfiles store_path in
  List.find dotfiles ~f:(fun df -> df.source = path || df.target = path)
;;

let print_dotfile df =
  Printf.printf
    "Dotfile { store: %s, name: %s, source: %s, target: %s }"
    df.store_path
    df.name
    df.source
    df.target
;;

let install_dotfile store_path path =
  match fetch_dotfile store_path path with
  | Some df ->
    (match df.state with
    | Installed -> Ok ()
    | Blocked ->
      Result.failf "Dotfile target `%s` is blocked by an existing file" df.target
    | Uninstalled ->
      df.target |> Filename.dirname |> Unix.mkdir_p;
      Unix.symlink ~src:df.source ~dst:df.target;
      Ok ())
  | None -> Result.failf "Dotfile not found at path `%s`" path
;;

let uninstall_dotfile store_path path =
  match fetch_dotfile store_path path with
  | Some df ->
    (match df.state with
    | Blocked | Uninstalled -> ()
    | Installed -> Sys.remove df.target);
    Ok ()
  | None -> Result.failf "Dotfile not found at path `%s`" path
;;

let manage_dotfile store_path path =
  match fetch_dotfile store_path path with
  | Some df ->
    Result.failf "Dotfile with target `%s` already exists in the store" df.target
  | None ->
    let name = String.chop_prefix_exn ~prefix:(home_dir ^ "/.") path in
    let store_path = Option.value store_path ~default:default_store_path in
    let df = dotfile_from_name store_path name in
    Unix.mkdir_p (Filename.dirname df.source);
    Sys.rename df.target df.source;
    Unix.symlink ~src:df.source ~dst:df.target;
    Ok ()
;;

let unmanage_dotfile store_path path =
  match fetch_dotfile store_path path with
  | Some df ->
    Unix.mkdir_p (Filename.dirname df.target);
    Sys.rename df.source df.target;
    Ok ()
  | None -> Result.failf "Dotfile not found with reference `%s`" path
;;

let () =
  let store_path_param =
    Command.Param.(flag "-store" (optional string) ~doc:"path dotfile store to use")
  and path_param = Command.Param.(anon ("filename" %: string)) in
  let list_command =
    Command.basic
      ~summary:"list all dotfiles in the given store"
      (Command.Param.map store_path_param ~f:(fun path () -> list_dotfiles path))
  and install_command =
    Command.basic
      ~summary:"link dotfile(s) to target in home directory"
      Command.Param.(
        map (both store_path_param path_param) ~f:(fun (store_path, path) () ->
            match install_dotfile store_path path with
            | Ok _ -> ()
            | Error err -> Printf.eprintf "Error: %s\n" err ))
  and uninstall_command =
    Command.basic
      ~summary:"unlink dotfile(s) from target in home directory"
      Command.Param.(
        map (both store_path_param path_param) ~f:(fun (store_path, path) () ->
            match uninstall_dotfile store_path path with
            | Ok _ -> ()
            | Error err -> Printf.eprintf "Error: %s\n" err ))
  and manage_command =
    Command.basic
      ~summary:"move dotfile(s) to the store and link them back to their target"
      Command.Param.(
        map (both store_path_param path_param) ~f:(fun (store_path, path) () ->
            match manage_dotfile store_path path with
            | Ok _ -> ()
            | Error err -> Printf.eprintf "Error: %s\n" err ))
  and unmanage_command =
    Command.basic
      ~summary:"move dotfile(s) out of the store and back to their target"
      Command.Param.(
        map (both store_path_param path_param) ~f:(fun (store_path, path) () ->
            match unmanage_dotfile store_path path with
            | Ok _ -> ()
            | Error err -> Printf.eprintf "Error: %s\n" err ))
  in
  Command.run
    (Command.group
       ~summary:"dots - Dotfile management made less toilesome."
       [ "list", list_command
       ; "install", install_command
       ; "uninstall", uninstall_command
       ; "manage", manage_command
       ; "unmanage", unmanage_command ])
;;
