let escape s =
  let buf = Buffer.create (String.length s) in
  let rec escape_char i c =
    match c with
      '\"' | '\\' ->
        Buffer.add_char buf '\\';
        Buffer.add_char buf c
    | '\000' .. '\127' ->
        Buffer.add_char buf c
    |  _ ->
        Buffer.add_string buf (Printf.sprintf "\\u%.4X" (int_of_char c)) in
  String.iteri escape_char s;
  Buffer.contents buf

let format_sequence list fmt =
  match list with
    [] -> ()
  | head :: tail ->
      Format.fprintf fmt "@[%t@]" head;
      List.iter (fun item -> Format.fprintf fmt "; @[%t@]" item) tail

let format_list list fmt =
  Format.fprintf fmt "@[[%t]@]" (format_sequence list)

let format_escaped_string s fmt =
  Format.fprintf fmt "\"%s\"" (escape s)

let format_marked_list mark list =
  format_list (format_escaped_string mark :: list)

let rec format_path path =
  match path with
    Path.Pident ident ->
      format_marked_list "Pident" [format_escaped_string ident.Ident.name]
  | Path.Pdot (p, field, _) ->
      format_marked_list "Pdot" [format_path p; format_escaped_string field]
  | Path.Papply (p, p') ->
      format_marked_list "Papply" [format_path p; format_path p']

(* Non-generalizable variables are indexed. The index is stored using the name
   format " %d" where %d is replaced by the index, given that no OCaml type name
   can begin with a space. *)

let fresh_count = ref 0

let format_var index =
  format_marked_list "Tvar" [fun fmt -> Format.fprintf fmt "%d" index]

(* Object types are represented by the token "Tobject". All other fancy
   types are represented by the token "Tunknown". *)

let rec format_type ty =
  match ty.Types.desc with
    Types.Tvar (Some x) ->
      if x.[0] = ' ' then
        format_var (int_of_string (String.sub x 1 (pred (String.length x))))
      else
        format_marked_list "Tpoly" [format_escaped_string x]
  | Types.Tvar None ->
      let index = !fresh_count in
      fresh_count := succ index;
      ty.Types.desc <- Types.Tvar (Some (Printf.sprintf " %d" index));
      format_var index
  | Types.Tarrow (_, a, b, _) ->
      format_marked_list "Tarrow" [format_type a; format_type b]
  | Types.Tconstr (c, args, _) ->
      format_marked_list "Tconstr" (format_path c :: List.map format_type args)
  | Types.Ttuple args ->
      format_marked_list "Ttuple" (List.map format_type args)
  | Types.Tobject _ ->
      format_marked_list "Tobject" []
  | _ ->
      format_marked_list "Tunknown" []

let format_constructor_field field =
  format_marked_list "field"
    [format_escaped_string field.Types.ld_id.Ident.name;
     format_type field.Types.ld_type]

let format_constructor_args args =
  match args with
    Types.Cstr_tuple l ->
      format_marked_list "tuple" (List.map format_type l)
  | Types.Cstr_record l ->
      format_marked_list "record" (List.map format_constructor_field l)

let format_constructor decl =
  format_marked_list "constructor"
    [format_escaped_string decl.Types.cd_id.Ident.name;
     format_constructor_args decl.Types.cd_args]

let format_label l =
  format_marked_list "label"
    [format_escaped_string l.Types.ld_id.Ident.name;
     format_type l.Types.ld_type]

let format_type_kind type_kind =
  match type_kind.Types.type_kind with
    Types.Type_variant constructors ->
      format_marked_list "Type_variant"
        (List.map format_constructor constructors)
  | Types.Type_record (labels, _) ->
      format_marked_list "Type_record" (List.map format_label labels)
  | Types.Type_open ->
      format_marked_list "Type_open" []
  | Types.Type_abstract ->
      match type_kind.Types.type_manifest with
        None -> format_marked_list "Type_abstract" []
      | Some ty -> format_marked_list "Type_alias" [format_type ty]

let format_option f o =
  match o with
    None ->
      format_marked_list "None" []
  | Some v ->
      format_marked_list "Some" [f v]

let rec format_item item =
  match item with
    Types.Sig_value (name, descr) ->
      format_marked_list "Sig_value"
        [format_escaped_string name.Ident.name;
         format_type descr.Types.val_type]
  | Types.Sig_type (name, decl, _) ->
      format_marked_list "Sig_type"
        [format_escaped_string name.Ident.name;
         (fun fmt -> Format.fprintf fmt "%d" decl.Types.type_arity);
         format_type_kind decl]
  | Types.Sig_module (name, decl, _) ->
      format_marked_list "Sig_module"
        [format_escaped_string name.Ident.name;
         format_module_signature decl.Types.md_type]
  | _ ->
      format_marked_list "Sig_unknown" []
and format_signature s =
  format_marked_list "Mty_signature" (List.map format_item s)
and format_module_signature ty =
  match ty with
    Types.Mty_signature s -> format_signature s
  | Types.Mty_functor (id, ty', ty'') ->
      format_marked_list "Mty_functor"
        [format_escaped_string id.Ident.name;
         format_option format_module_signature ty';
         format_module_signature ty'']
  | Types.Mty_alias (_, p) ->
      format_marked_list "Mty_alias" [format_path p]
  | _ ->
      format_marked_list "Mty_unknown" []

let dump_file filename =
  let cmi = Cmt_format.read_cmi filename in
  Format.printf "@[%t@]@."
    (format_marked_list "module"
       [format_escaped_string cmi.Cmi_format.cmi_name;
        format_signature cmi.Cmi_format.cmi_sign])

let main () =
   Arg.parse [] dump_file "Dump .cmi files in JSON syntax"

let () =
  if not !Sys.interactive then
    main ()
