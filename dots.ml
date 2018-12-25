open Core

let ignored_files = [".git"; "README.md"]

let home_dir = Sys.home_directory ()

type dotfile_state = Installed | Uninstalled | Blocked

let df_state_of_string = function
  | Installed -> "[x]"
  | Uninstalled -> "[ ]"
  | Blocked -> "[-]"

type dotfile =
  { name: string
  ; store_path: string
  ; target: string
  ; source: string
  ; state: dotfile_state }

let dotfile_from_name store_path name =
  let read_symlink path = try Some (Unix.readlink path) with _ -> None in
  let target = Filename.concat home_dir "." ^ name in
  let source = Filename.concat store_path name in
  let state =
    if Sys.file_exists target = `Yes then
      if read_symlink target = Some source then Installed else Blocked
    else Uninstalled
  in
  {name; store_path; target; source; state}

let rec dir_contents dir =
  if List.exists ignored_files ~f:(( = ) @@ Filename.basename dir) then []
  else
    match Sys.is_directory dir with
    | `Yes ->
        Sys.ls_dir dir
        |> List.map ~f:(fun dir' -> dir_contents @@ Filename.concat dir dir')
        |> List.concat
    | `No -> [dir]
    | `Unknown -> []

let () =
  let store_path = Filename.concat home_dir ".dotfiles" in
  let dotfiles =
    dir_contents store_path
    |> List.map ~f:(String.chop_prefix_exn ~prefix:(store_path ^ "/"))
  in
  let () =
    Printf.printf
      "Printing dotfiles from %s\n\
       Legend: [x] installed, [-] blocked, [ ] uninstalled\n\n"
      store_path
  in
  dotfiles
  |> List.sort ~compare:String.compare
  |> List.map ~f:(dotfile_from_name store_path)
  |> List.iter ~f:(fun df ->
         Printf.printf "%s %s\n" (df_state_of_string df.state) df.name )
