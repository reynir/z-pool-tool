open Entity

type event =
  | SignedUp of Code.t
  | Verified of Code.t
[@@deriving eq, show, variants]

let handle_event db_ctx : event -> unit Lwt.t = function
  | SignedUp code -> Repo.insert db_ctx `Signup code
  | Verified code -> Repo.insert db_ctx `Verification code
;;
