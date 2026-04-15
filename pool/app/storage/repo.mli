val register_migration : unit -> unit
val register_cleaner : unit -> unit
val insert_file : Database.ctx -> Sihl.Contract.Storage.stored -> unit Lwt.t
val insert_blob : Database.ctx -> id:string -> string -> unit Lwt.t
val get_file : Database.ctx -> string -> Sihl.Contract.Storage.stored option Lwt.t
val get_blob : Database.ctx -> string -> string option Lwt.t
val update_file : Database.ctx -> Sihl.Contract.Storage.stored -> unit Lwt.t
val update_blob : Database.ctx -> id:string -> string -> unit Lwt.t
val delete_file : Database.ctx -> string -> unit Lwt.t
val delete_blob : Database.ctx -> string -> unit Lwt.t
