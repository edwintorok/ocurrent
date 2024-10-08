type t = No_context

module Key = struct
  include Commit

  let digest t = t |> Commit.id |> Commit_id.digest

end

module Value = struct
  type t = { commit: Commit.t; tree_hash: string } [@@deriving yojson]

  let v ~commit ~tree_hash = {commit; tree_hash}

  let commit t = t.commit

  let tree_hash t = t.tree_hash

  let equal a b = String.equal (tree_hash a) (tree_hash b)

  let compare a b = String.compare (tree_hash a) (tree_hash b)

  let pp = Fmt.using tree_hash Fmt.string

  let digest t = t.tree_hash

  let marshal t = t |> to_yojson |> Yojson.Safe.to_string

  let unmarshal s =
    match s |> Yojson.Safe.from_string |> of_yojson with
    | Ok x -> x
    | Error e -> failwith e
end

let id = "git-rev-parse-for-tree"

let build No_context job commit =
  let open Lwt.Syntax in
  let repo = Commit.repo commit
  and commitish = commit |> Commit.id |> Commit_id.hash in
  let repo_lock = repo |> Fpath.to_string |> Clone.repo_lock in
  Lwt_mutex.with_lock repo_lock @@ fun () ->
  let* () = Current.Job.start job ~level:Current.Level.Harmless in
  let open Lwt_result.Syntax in
  (* we could append [./], but that would depend on where the current dir is. *)
  let+ tree_hash = Cmd.git_rev_parse ~cancellable:true ~job ~repo (commitish ^ ":") in
  Value.v ~commit ~tree_hash

let pp ppf key = Fmt.pf ppf "git rev-parse %a:" Commit.pp key

let auto_cancel = false
