open CCFun.Infix
open Entity

module Tags = struct
  let add_label : string Logs.Tag.def =
    Logs.Tag.def "database_label" ~doc:"Database Label" CCString.pp
  ;;

  let add_by_label = Label.value %> Logs.Tag.add add_label
  let create_by_label label = Logs.Tag.empty |> add_by_label label
  let extend_by_label label =
    CCOption.map_or ~default:(create_by_label label) (add_by_label label)

  let add ctx = label_of_ctx ctx |> add_by_label
  let create ctx = label_of_ctx ctx |> create_by_label
  let extend ctx = label_of_ctx ctx |> extend_by_label
end
