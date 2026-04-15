let src = Logs.Src.create "user_import.service"
let get_or_failwith = Pool_common.Utils.get_or_failwith
let interval_s = 5

let run db_ctx user_uuid =
  let open Utils.Lwt_result.Infix in
  let tags = Database.Logger.Tags.create db_ctx in
  Logs.info ~src (fun m ->
    m ~tags "Find possible duplicates of Contact '%s'" (Pool_common.Id.value user_uuid));
  let%lwt fields = Custom_field.find_for_duplicate_check db_ctx in
  Repo.find_similars db_ctx ~user_uuid fields >|> Repo.insert db_ctx
;;

let run_by_tenant database_label =
  Database.transaction_ctx database_label @@ fun db_ctx ->
  let%lwt contact = Repo.find_to_check db_ctx in
  match contact with
  | None -> Lwt.return_unit
  | Some contact ->
    let%lwt () = Contact.id contact |> Contact.Id.to_common |> run db_ctx in
    Repo.mark_as_checked db_ctx contact
;;

let run_all () = Database.(Pool.Tenant.all ()) |> Lwt_list.iter_s run_by_tenant

let start () =
  let open Schedule in
  let interval = Ptime.Span.of_int_s interval_s in
  create
    "possible_duplicates"
    (Every (interval |> ScheduledTimeSpan.of_span))
    None
    run_all
  |> Schedule.add_and_start
;;

let stop () = Lwt.return_unit

let lifecycle =
  Sihl.Container.create_lifecycle
    "Import users"
    ~dependencies:(fun () -> [ Pool_database.lifecycle; Pool_queue.lifecycle_service ])
    ~start
    ~stop
;;

let register () = Sihl.Container.Service.create lifecycle
