include Entity
include Event

let label = "reset OTP (authentication codes)"
let src = Logs.Src.create "authentication.service"

let create ?(id = Id.create ()) ?(token = Token.generate ()) ~user ~channel () =
  { id; user_uuid = Pool_user.id user; channel; token }
;;

let find_valid_by_id = Repo.find_valid_by_id

let start () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s (5 * 60) in
  let periodic_fcn () =
    Database.Pool.Tenant.all ()
    |> Lwt_list.iter_s (fun pool ->
      Database.transaction_ctx pool @@ fun db_ctx ->
      Logs.debug ~src (fun m ->
        m
          ~tags:Database.(Logger.Tags.create db_ctx)
          "Reset expired authentication tokens in pool");
      Repo.reset_expired db_ctx ())
  in
  let schedule =
    create label (Every (interval |> ScheduledTimeSpan.of_span)) None periodic_fcn
  in
  Schedule.add_and_start schedule
;;

let lifecycle =
  Sihl.Container.create_lifecycle
    label
    ~dependencies:(fun () -> [ Schedule.lifecycle ])
    ~start
;;

let register () =
  let configuration = Sihl.Configuration.make () in
  Sihl.Container.Service.create ~configuration lifecycle
;;
