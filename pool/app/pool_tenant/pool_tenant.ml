open CCFun.Infix
include Entity
include Event
module LogoMapping = LogoMapping
module Guard = Entity_guard

type handle_list_recruiters = unit -> Pool_user.t list Lwt.t
type handle_list_tenants = unit -> t list Lwt.t

module Url = struct
  include Entity.Url

  let of_pool = Repo_entity.Url.of_pool
end

let find id =
  Database.(connection_ctx Pool.Root.label) @@ fun db_ctx ->
  Repo.find db_ctx id
let find_full id =
  Database.(connection_ctx Pool.Root.label) @@ fun db_ctx ->
  Repo.find_full db_ctx id
let find_by_label label =
  Database.(connection_ctx Pool.Root.label) @@ fun db_ctx ->
  Repo.find_by_label db_ctx label
let find_by_db_ctx db_ctx = find_by_label (Database.label_of_ctx db_ctx)
let find_by_url ?should_cache url =
  Database.(connection_ctx Pool.Root.label) @@ fun db_ctx ->
  Repo.find_by_url ?should_cache db_ctx url
let find_all () =
  Database.(connection_ctx Pool.Root.label) @@ fun db_ctx ->
  Repo.find_all db_ctx ()

let create_public_url pool_url =
  Sihl.Web.externalize_path %> Format.asprintf "https://%s%s" (Url.value pool_url)
;;

let clear_cache = Repo.Cache.clear

module Repo = struct
  module Id = Repo_entity.Id
end
