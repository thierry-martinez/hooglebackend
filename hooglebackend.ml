(* By convention, all type names have a "T" prepended to them, to make them
   begin with a capital letter. (Note that "_t" is a valid OCaml type name, so
   capitalize the first letter is not enough.) *)

let haskellize_type_name s = Printf.sprintf "T%s" s

(* All non alphanumeric characters are escaped (to handle identifiers like
   "|>" and "#foo"): the convention is to replace every non alphanumeric
   character "c" by "__XX" where "XX" is the hexadecimal character code.
   "_" characters are preserved if they are isolated (i.e. if they are not
   followed by another "_"). If a "_" is followed by another "_", then it
   is replaced as other special characters. *)

let escape s =
  let buf = Buffer.create (String.length s) in
  let rec escape_char i c =
    match c with
      'a' .. 'z' | 'A' .. 'Z' | '0' .. '9' ->
        Buffer.add_char buf c
    | '_' when i + 1 >= String.length s || s.[succ i] != '_' ->
        Buffer.add_char buf '_'
    |  _ ->
        Buffer.add_string buf (Printf.sprintf "__%.2X" (int_of_char c)) in
  String.iteri escape_char s;
  Buffer.contents buf

(* Module paths are flattened by considering "." as a special character:
   "A.B" is printed as "A__2EB", where 2E is the character code for ".". *)

let rec print_path chan path =
  match path with
    Path.Pident ident ->
      output_string chan (escape ident.Ident.name)
  | Path.Pdot (p, field, _) ->
      Printf.fprintf chan "%a__2E%s" print_path p field
  | Path.Papply (p, _) ->
      print_path chan p

let rec print_haskell_type_path chan path =
  match path with
    Path.Pident ident ->
      output_string chan (haskellize_type_name (escape ident.Ident.name))
  | Path.Pdot (p, field, _) ->
      let type_name = haskellize_type_name (escape field) in
      Printf.fprintf chan "%a.%s" print_path p type_name
  | Path.Papply (p, _) ->
      print_haskell_type_path chan p

let print_list fmt print_item chan l =
  List.iter (fun item -> Printf.fprintf chan fmt print_item item) l

let print_comma_separated_list print_item chan l =
  match l with
    [] -> ()
  | head :: tail ->
      print_item chan head;
      print_list ", %a" print_item chan l

(* Non-generalizable variables are replaced with type variables t0, t1, etc. *)

let fresh_count = ref 0

(* Object types are represented by the type variable "object". All other fancy
   types are represented by the type variable "unknown". *)

let rec print_type chan ty =
  match ty.Types.desc with
    Types.Tvar (Some x) ->
      output_string chan x
  | Types.Tvar None ->
      let index = !fresh_count in
      fresh_count := succ index;
      let fresh_name = Printf.sprintf "t%d" index in
      ty.Types.desc <- Types.Tvar (Some fresh_name);
      output_string chan fresh_name
  | Types.Tarrow (_, a, b, _) ->
      Printf.fprintf chan "(%a -> %a)" print_type a print_type b
  | Types.Tconstr (c, args, _) ->
      Printf.fprintf chan "%a%a" print_haskell_type_path c
        (print_list " %a" print_type) args
  | Types.Ttuple items ->
      Printf.fprintf chan "(%a)" (print_comma_separated_list print_type) items
  | Types.Tobject _ ->
      output_string chan "object"
  | _ ->
      output_string chan "unknown"

let print_value chan name ty =
  Printf.fprintf chan "%s :: %a\n" (escape name.Ident.name) print_type ty

let print_constructor_output t chan output =
  match output with
    None -> print_type chan t
  | Some t' -> print_type chan t'

let make_type desc = { Types.desc; level = 0; id = 0 }

let make_constr name args =
  make_type (Types.Tconstr (name, args, ref Types.Mnil))

let make_arrow a b =
  make_type (Types.Tarrow (Asttypes.Nolabel, a, b, Types.Cunknown))

let make_type_arguments arity =
  Array.to_list
    (Array.init arity
       (fun i -> make_type (Types.Tvar (Some (Printf.sprintf "t%d" i)))))

let print_constructor proto chan decl =
  let result =
    match decl.Types.cd_res with
      None -> proto
    | Some ty -> ty in
  let args =
    match decl.Types.cd_args with
      Types.Cstr_tuple l -> l
    | Types.Cstr_record l -> List.map (fun item -> item.Types.ld_type) l in
  let ty = List.fold_right make_arrow args result in
  print_value chan decl.Types.cd_id ty

(* Modules are printed with a module declaration, even the file module.
   The module path is fully printed. *)

let print_module chan name =
  Printf.fprintf chan "module %a where\n" print_path name

let rec print_item module_name chan item =
  match item with
    Types.Sig_value (name, descr) ->
      print_value chan name descr.Types.val_type
  | Types.Sig_type (name, decl, _) ->
      let proto =
        make_constr (Path.Pident name)
          (make_type_arguments decl.Types.type_arity) in
      Printf.fprintf chan "data %a\n" print_type proto;
      begin
        match decl.Types.type_kind with
          Types.Type_variant constructors ->
            List.iter (print_constructor proto chan) constructors
        | _ -> ()
      end
  | Types.Sig_module (name, decl, _) ->
      let rec print_module_signature ty =
        match ty with
          Types.Mty_signature s ->
            print_signature (Path.Pdot (module_name, name.Ident.name, 0)) chan s
        | Types.Mty_functor (_, _, ty') -> print_module_signature ty'
        | _ -> () in
      print_module_signature decl.Types.md_type;
      print_module chan module_name (* Come back to the parent module. *)
  | _ ->
      ()
and print_signature module_name chan s =
  print_module chan module_name;
  List.iter (print_item module_name chan) s

let dump_file filename =
  let cmi = Cmt_format.read_cmi filename in
  print_signature (Path.Pident (Ident.create cmi.Cmi_format.cmi_name))
    stdout cmi.Cmi_format.cmi_sign

let main () =
   Arg.parse [] dump_file "Dump .cmi files in Haskell syntax"

let () =
  if not !Sys.interactive then
    main ()
