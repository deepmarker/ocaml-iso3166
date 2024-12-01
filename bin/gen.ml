open Json_encoding

let pipe = Pla.(string "|")

let int_as_string =
  conv
    Int.to_string
    (function
      | "" -> 0
      | x -> int_of_string x)
    string
;;

let basic =
  obj5
    (req "name" string)
    (req "alpha-2" string)
    (req "alpha-3" string)
    (req "country-code" int_as_string)
    (req "iso_3166-2" string)
;;

let region =
  obj6
    (req "region" string)
    (req "sub-region" string)
    (req "intermediate-region" string)
    (req "region-code" int_as_string)
    (req "sub-region-code" int_as_string)
    (req "intermediate-region-code" int_as_string)
;;

let info_record =
  {|type info = {name: string; alpha2: string; alpha3: string;
  country_code: int; iso_3166_2: string; region: string;
  sub_region: string; intermediate_region: string; region_code: int;
  sub_region_code: int; intermediate_region_code: int} [@@deriving sexp_of]|}
;;

let encoding = merge_objs basic region

let genRecord
  ( (name, alpha2, alpha3, cc, iso)
  , (region, sregion, iregion, regionc, sregionc, iregionc) )
  =
  [%pla
    {|{name="<#name#s>"; alpha2="<#alpha2#s>"; alpha3="<#alpha3#s>"; country_code=<#cc#i>;
    iso_3166_2="<#iso#s>"; region="<#region#s>"; sub_region="<#sregion#s>";
    intermediate_region="<#iregion#s>"; region_code=<#regionc#i>;
    sub_region_code=<#sregionc#i>; intermediate_region_code=<#iregionc#i>}|}]
;;

let withJSON ic oc =
  let json = Ezjsonm.from_channel ic in
  let records = destruct (array encoding) json in
  Array.sort (fun ((_, x, _, _, _), _) ((_, y, _, _, _), _) -> String.compare x y) records;
  let records = Array.to_list records in
  let alpha2 ((_, x, _, _, _), _) = Pla.string x in
  let open Pla in
  let typeT =
    string "type t = " ++ map_sep pipe alpha2 records ++ string "[@@deriving sexp]"
  in
  let infos =
    string "let infos = " ++ string "[|" ++ map_sep semi genRecord records ++ string "|]"
  in
  write
    oc
    (string "open Sexplib.Std"
     ++ newline
     ++ typeT
     ++ newline
     ++ string info_record
     ++ newline
     ++ infos
     ++ newline)
;;

let () =
  let ic = open_in Sys.argv.(1) in
  let oc = Sys.argv.(2) in
  Fun.protect ~finally:(fun () -> close_in ic) (fun () -> withJSON ic oc)
;;
