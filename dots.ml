open Core

type dotfile_state =
  | Installed
  | Uninstalled
  | Blocked

type dotfile =
  { name: string
  ; store_path: string
  ; target: string
  ; source: string
  ; state: dotfile_state }

let ignored_files = [".git"; "README.md"]

let home_dir = Sys.home_directory ()

let rec ls_dir_files dir =
  if List.mem ignored_files (Filename.basename dir) ~equal:String.equal
  then []
  else
    match Sys.is_directory dir with
    | `Yes ->
        Sys.ls_dir dir
        |> List.map ~f:(fun dir' -> ls_dir_files (Filename.concat dir dir'))
        |> List.concat
    | `No -> [dir]
    | `Unknown -> []


let dotfile_from_name store_path name =
  let readlink path = Option.try_with (fun () -> Unix.readlink path)
  and target = Filename.concat home_dir ("." ^ name)
  and source = Filename.concat store_path name in
  let state =
    if Sys.file_exists target = `Yes
    then if readlink target = Some source then Installed else Blocked
    else Uninstalled
  in
  {name; store_path; target; source; state}


let all_dotfiles store_path =
  ls_dir_files store_path
  |> List.map ~f:(String.chop_prefix_exn ~prefix:(store_path ^ "/"))
  |> List.sort ~compare:String.compare
  |> List.map ~f:(dotfile_from_name store_path)


let fetch_dotfile store_path path =
  all_dotfiles store_path |> List.find ~f:(fun df -> df.source = path || df.target = path)


let list_dotfiles store_path =
  let dotfiles = all_dotfiles store_path in
  Printf.printf
    "Printing dotfiles from %s\nLegend: [x] installed, [-] blocked, [ ] uninstalled\n\n"
    store_path ;
  List.iter dotfiles ~f:(fun df ->
      let string_state =
        match df.state with
        | Installed -> "[x]"
        | Uninstalled -> "[ ]"
        | Blocked -> "[-]"
      in
      Printf.printf "%s %s\n" string_state df.name )


let install_dotfile store_path path =
  match fetch_dotfile store_path path with
  | Some df ->
    ( match df.state with
    | Installed -> Ok ()
    | Blocked ->
        Result.failf "Dotfile target '%s' is blocked by an existing file" df.target
    | Uninstalled ->
        Unix.mkdir_p (Filename.dirname df.target) ;
        Unix.symlink ~src:df.source ~dst:df.target ;
        Ok () )
  | None -> Result.failf "Dotfile not found in store with reference '%s'" path


let uninstall_dotfile store_path path =
  match fetch_dotfile store_path path with
  | Some df ->
      ( match df.state with
      | Blocked | Uninstalled -> ()
      | Installed -> Sys.remove df.target ) ;
      Ok ()
  | None -> Result.failf "Dotfile not found in store with reference '%s'" path


let manage_dotfile store_path path =
  match fetch_dotfile store_path path with
  | Some df -> Result.failf "Dotfile '%s' already exists in the store" df.name
  | None ->
    ( match String.chop_prefix path ~prefix:(home_dir ^ "/.") with
    | Some name ->
        let df = dotfile_from_name store_path name in
        Unix.mkdir_p (Filename.dirname df.source) ;
        Sys.rename df.target df.source ;
        Unix.symlink ~src:df.source ~dst:df.target ;
        Ok ()
    | None -> Result.failf "Target '%s' must be a dotfile in the home directory" path )


let unmanage_dotfile store_path path =
  match fetch_dotfile store_path path with
  | Some df ->
      Unix.mkdir_p (Filename.dirname df.target) ;
      Sys.rename df.source df.target ;
      Ok ()
  | None -> Result.failf "Dotfile not found with reference '%s'" path


let directory =
  Command.Arg_type.create (fun path ->
      if Sys.is_directory path = `Yes
      then Filename.realpath path
      else (
        Printf.eprintf "Path '%s' must be an existing directory.\n" path ;
        exit 1 ) )


let regular_file =
  Command.Arg_type.create (fun path ->
      if Sys.is_file path = `Yes
      then Filename.realpath path
      else (
        Printf.eprintf "Path '%s' must be an existing regular file.\n" path ;
        exit 1 ) )


let ok_or_exit = function
  | Ok _ -> ()
  | Error err ->
      Printf.eprintf "Error: %s\n" err ;
      exit 1


let () =
  let store_path_param =
    Command.Param.(
      flag "-store"
        (optional_with_default (Filename.concat home_dir ".dotfiles") directory)
        ~doc:"directory dotfile store to use")
  and path_param = Command.Param.(anon ("file" %: regular_file)) in
  let list_command =
    Command.basic ~summary:"list all dotfiles in the given store"
      (Command.Param.map store_path_param ~f:(fun path () -> list_dotfiles path))
  and install_command =
    Command.basic ~summary:"link dotfile(s) to target in home directory"
      Command.Let_syntax.(
        let%map_open store_path = store_path_param and path = path_param in
        fun () -> install_dotfile store_path path |> ok_or_exit)
  and uninstall_command =
    Command.basic ~summary:"unlink dotfile(s) from target in home directory"
      Command.Let_syntax.(
        let%map_open store_path = store_path_param and path = path_param in
        fun () -> uninstall_dotfile store_path path |> ok_or_exit)
  and manage_command =
    Command.basic
      ~summary:"move dotfile(s) to the store and link them back to their target"
      Command.Let_syntax.(
        let%map_open store_path = store_path_param and path = path_param in
        fun () -> manage_dotfile store_path path |> ok_or_exit)
  and unmanage_command =
    Command.basic ~summary:"move dotfile(s) out of the store and back to their target"
      Command.Let_syntax.(
        let%map_open store_path = store_path_param and path = path_param in
        fun () -> unmanage_dotfile store_path path |> ok_or_exit)
  in
  Command.run
    (Command.group ~summary:"dots - Dotfile management made less toilesome"
       [ ("list", list_command)
       ; ("install", install_command)
       ; ("uninstall", uninstall_command)
       ; ("manage", manage_command)
       ; ("unmanage", unmanage_command) ])
