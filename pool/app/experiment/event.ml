open Entity
module Reminder = Pool_common.Reminder

type event =
  | Created of t
  | Updated of t * t
  | ResetInvitations of t
  | Deleted of Pool_common.Id.t
[@@deriving eq, show, variants]

let handle_event ?user_uuid db_ctx : event -> unit Lwt.t =
  let open Utils.Lwt_result.Infix in
  let ctx = Database.to_ctx db_ctx in
  let create_changelog before after =
    let open Version_history in
    insert db_ctx ?user_uuid ~entity_uuid:before.id ~before ~after ()
  in
  function
  | Created t ->
    let%lwt () = Repo.insert db_ctx t in
    Entity_guard.Target.to_authorizable ~ctx t
    ||> Pool_common.Utils.get_or_failwith
    ||> fun (_ : Guard.Target.t) -> ()
  | Updated (experiment, updated) ->
    let%lwt () = create_changelog experiment updated in
    Repo.update db_ctx updated
  | ResetInvitations m -> Repo_invitation_reset.insert db_ctx m
  | Deleted experiment_id -> Repo.delete db_ctx experiment_id
[@@deriving eq, show]
;;
