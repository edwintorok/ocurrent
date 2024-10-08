(** Integration with Git. *)

(** [git clone] configuration options.
  [git] will persist these in [.git/config], so subsequent fetches and other operations
  on the repository will also use these.
  Some of these settings are suitable for a CI (e.g. blobless clones), but not for a developer
  (e.g. they slow down [git blame]), so they have to be set per-repository,
  and cannot be set in the global [git] config.
 *)
type clone_config =
{ filter: [`Blobless] option (** use --filter=blob:none for a blobless clone/fetch *)
; only_branch: bool (** enables [--single-branch] and [--no-tags] *)
; features: string list (** enable git {{:https://git-scm.com/docs/git-config#Documentation/git-config.txt-feature}features} *)
; disable_reflog: bool (** disables the {{:https://git-scm.com/docs/git-config#Documentation/git-config.txt-corelogAllRefUpdates}reflog} *)
}

(** git's default configuration *)
val empty_config: clone_config

(** git configuration values optimized for clone/fetch speed.
    May break compatibility with older versions of [git], or 3rdparty
    [git] implementations. *)
val fast_config: clone_config

(** this plugin's default configuration.
  Currently equivalent to {!val:empty_config}. *)
val default_config: clone_config

module Commit_id : sig
  include Set.OrderedType

  val v : repo:string -> gref:string -> hash:string -> t
  (** [v ~repo ~gref ~hash] identifies a commit that can be fetched from [repo]
      using [gref] as the reference name and has hash [hash]. *)

  val repo : t -> string
  (** [repo t] is the Git URI of the repository. *)

  val gref : t -> string

  val hash : t -> string
  (* [hash t] is the Git commit hash. *)

  val equal : t -> t -> bool
  val pp : t Fmt.t

  val pp_user_clone : t Fmt.t
  (** Display a Git command a user could run to get this commit. *)

  val digest : t -> string
end

(** A git tree hash identifies the actual contents of a repository.

  It doesn't include commit messages or history, and should remain unchanged
  over squashing or rewording commits.
  This can be useful to avoid rebuilds when cleaning up commit history before
  or during a PR review.

  Although a git tree hash is independent of filesystem location,
  a build that uses a git tree as input may embed the path in binaries that it produces,
  and therefore we also include the repo path into {!type:Tree.t}.

  Comparison and equality only look at the tree hash and ignore the repo though,
  but {!val:digest} includes it.

  Can be used either as a cache Key or cache Value too.
 *)
module Tree: sig
  include Set.OrderedType

  val v : repo:Fpath.t -> tree_hash:string -> t
  (** [v ~repo ~tree_hash] creates a tree hash from [tree_hash]. *)

  val tree_hash: t -> string
  (** [tree_hash t] tree hash as a hexadecimal string *)

  val repo: t -> Fpath.t
  (** [repo t] is the repository containing [t]. *)

  val pp: t Fmt.t
  (** [pp ppf t] pretty prints the full tree hash of [t] *)

  include Current_cache.S.WITH_MARSHAL with type t := t
  include Current_cache.S.WITH_DIGEST with type t := t
end

module Commit : sig
  include Set.OrderedType

  val v : repo:Fpath.t -> id:Commit_id.t -> t
  val id : t -> Commit_id.t
  val hash : t -> string
  val equal : t -> t -> bool
  val pp : t Fmt.t

  val repo : t -> Fpath.t
  val pp_short : t Fmt.t
  (** [pp_short] shows just the start of the hash. *)

  val marshal : t -> string
  val unmarshal : string -> t

end


val clone : ?clone_config:clone_config -> schedule:Current_cache.Schedule.t -> ?gref:string -> string -> Commit.t Current.t
(** [clone ?clone_config ~schedule ~gref uri] evaluates to the head commit of [uri]'s [gref] branch (default: "master"). *)

val fetch : ?clone_config:clone_config -> Commit_id.t Current.t -> Commit.t Current.t

val tree_hash: Commit.t Current.t -> Tree.t Current.t
(** [tree_hash commit] return the tree hash given a commit hash (cached). *)

val with_checkout :
  ?pool:unit Current.Pool.t ->
  job:Current.Job.t ->
  Commit.t ->
  (Fpath.t -> 'a Current.or_error Lwt.t) ->
  'a Current.or_error Lwt.t
(** [with_checkout ~job c fn] clones [c] to a temporary directory and runs [fn tmpdir].
    When it returns, the directory is deleted.
    @param pool Used to prevent too many clones from happening at once. *)

module Local : sig
  type t
  (** A local Git repository. *)

  val v : Fpath.t -> t
  (** [v path] is the local Git repository at [path]. *)

  val head : t -> [`Commit of Commit_id.t | `Ref of string ] Current.t
  (** [head] is the current branch ref (e.g. "/refs/heads/master"). *)

  val head_commit : t -> Commit.t Current.t
  (** [head_commit] is the commit at the head of the current branch. *)

  val commit_of_ref : t -> string -> Commit.t Current.t
  (** [commit_of_ref t gref] evaluates to the commit at the head of [gref].
      e.g. [commit_of_ref t "/refs/heads/master"] *)

  val repo : t -> Fpath.t
end
