include module type of Iso3166_base

val info : t -> info
val of_name : string -> t option
val of_name_exn : string -> t
val of_alpha3 : string -> t option
val of_alpha3_exn : string -> t
val to_alpha3 : t -> string
val to_name : t -> string
