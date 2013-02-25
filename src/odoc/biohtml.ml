

open Printf (* don't know how to link Core with ocamldoc  *)
open Odoc_info
(* open Module *)
(* module StringSet = Odoc_html.StringSet *)

let word_re = Str.regexp "[ \t\r\n]+"

let split_args t =
  match t with
  | [] -> []
  | [Odoc_info.Raw arg] -> Str.split word_re arg
  | _ ->
    failwith "Argument too complicated (you should not use markup in captions)"

(*
let read_all f =
  let i = open_in f in
  let b = Buffer.create 1024 in
  let rec loop () =
    try
      Buffer.add_char b (input_char i);
      loop ()
    with e -> ()
  in
  loop ();
  close_in i;
  Buffer.contents b
*)

module Generator (G : Odoc_html.Html_generator) =
struct
  class html =
  object(self)
    inherit G.html as super

    (* method generate module_list = *)
      (* eprintf "generate Called !\n%!" *)

    method private html_of_figure b t =
      let (file, width, caption) =
        let read_width s =
          try `percent (Scanf.sscanf s "%d%%" (fun i -> i))
          with e ->
            ksprintf failwith "in {figure _} expecting width but got %S" s in
        match split_args t with
	| [] -> failwith "{figure ...} needs at least one argument"
        | one :: [] -> (one, `percent 90, "")
        | one :: two :: [] -> (one, read_width two, "")
        | one :: two :: three -> (one, read_width two, String.concat " " three)
      in
      bprintf b "
        <div class=\"figure\" style=\"max-width : %s\" title=%S>
        <a href=\"%s\"><img src=\"%s\" width=\"99%%\"/></a>
        <div class=\"caption\">%s</div>
        </div>"
        (match width with `percent s -> sprintf "%d%%" s) file
        (Filename.basename file) (Filename.basename file)
        caption

    val mutable hide_show_id = 0
    method html_of_included_module b im =   (* overridden! *)
      let with_button f =
        bprintf b "<script>function hide_show (vid) {
                     var v = document.getElementById(vid);
                     if (v.style.display == 'none') {
                       v.style.display = 'block'
                     } else {
                       v.style.display = 'none'
                     }
                   }</script>";
        bprintf b "<div>";
        bprintf b "<button type=\"button\" style=\"float: left; padding: 1px\"
                  onclick=\"hide_show('hs%d')\">+</button>"
          hide_show_id;
        super#html_of_included_module b im;
        bprintf b "</div>";
	bprintf b "<div class=\"included-module\" id=\"hs%d\"
                    style=\"display: none\" >\n" hide_show_id;
        hide_show_id <- hide_show_id + 1;
        f ();
	bprintf b "</div>\n"
      in
      let open Module in
      begin match im.im_module with
      | None -> (* case module is unknown *)
        super#html_of_included_module b im;
      | Some (Mod m) ->
        with_button (fun () ->
	  List.iter
            (self#html_of_module_element b (Name.father m.m_name))
            (Module.module_elements m);
        )
      | Some (Modtype mt) ->
        with_button (fun () ->
	  List.iter
            (self#html_of_module_element b (Name.father mt.mt_name))
            (Module.module_type_elements mt);
        )
      end

    method html_of_module_type_kind b father ?modu ?mt kind =
      let open Module in
      begin match kind with
      | Module_type_struct eles ->
        bprintf b "</pre>";
        self#html_of_text b [Code "sig"];
        bprintf b "<div class=\"sig_block\">";
        List.iter (self#html_of_module_element b father) eles;
        bprintf b "</div>";
        self#html_of_text b [Code "end"];
        (* super#html_of_module_type_kind b father ?modu ?mt kind *)
      | _ ->
        eprintf "other\n%!";
        super#html_of_module_type_kind b father ?modu ?mt kind
      end

    method html_of_module_kind b father ?modu kind =
      let open Module in
      begin match kind with
      | Module_struct eles ->
        bprintf b "</pre>";
        self#html_of_text b [Code "sig"];
        bprintf b "<div class=\"sig_block\">";
        List.iter (self#html_of_module_element b father) eles;
        bprintf b "</div>";
        self#html_of_text b [Code "end"]
      | _ ->
        eprintf "html_of_module_kind\n%!";
        super#html_of_module_kind b father ?modu kind
      end

    method html_of_module b ?(info=true) ?(complete=true) ?(with_link=true) m =
      let open Module in
      bprintf b "<p>";
      self#html_of_info ~indent:false b m.m_info;
      let father = Name.father m.m_name in
      bprintf b "<pre>";
      bprintf b "%s" ((self#keyword "module")^" ");
      bprintf b "%s" (Name.simple m.m_name);
      bprintf b ": ";
      self#html_of_module_kind b father ~modu: m m.m_kind;
      bprintf b "</pre>";
      ()

    method generate_for_module_type pre post mt =
      let open Module in
      let open Odoc_html in
      try
        let (html_file, _) = Naming.html_files mt.mt_name in
        let type_file = Naming.file_type_module_complete_target mt.mt_name in
        let chanout = open_out (Filename.concat !Global.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun mt -> mt.mt_name) pre in
        let post_name = opt (fun mt -> mt.mt_name) post in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b
          ~nav: (Some (pre_name, post_name, mt.mt_name))
          ~comments: (Module.module_type_comments mt)
          (self#inner_title mt.mt_name);
        bs b "<body>\n";
        self#print_navbar b pre_name post_name mt.mt_name;
        bp b "<h1>";
        bs b (Odoc_messages.module_type^" ");
        (match mt.mt_type with
        | Some _ -> bp b "<a href=\"%s\">%s</a>" type_file mt.mt_name
        | None-> bs b mt.mt_name);
        bs b "</h1>\n" ;
        self#html_of_modtype b ~with_link: false mt;
        (* parameters for functors *)
        self#html_of_module_parameter_list b
          (Name.father mt.mt_name)
          (Module.module_type_parameters mt);
        (* a horizontal line *)
        bs b "</body></html>";
        Buffer.output_buffer chanout b;
        close_out chanout;
        (* generate html files for submodules *)
        (* self#generate_elements self#generate_for_module (Module.module_type_modules mt); *)
        (* generate html files for module types *)
        (* self#generate_elements self#generate_for_module_type (Module.module_type_module_types mt); *)
        (* generate html files for classes *)
        self#generate_elements self#generate_for_class (Module.module_type_classes mt);
        (* generate html files for class types *)
        self#generate_elements self#generate_for_class_type (Module.module_type_class_types mt);
        (* generate the file with the complete module type *)
        (match mt.mt_type with
        | None -> ()
        | Some mty ->
          self#output_module_type mt.mt_name
            (Filename.concat !Global.target_dir type_file) mty)
      with
        Sys_error s ->
          raise (Failure s)

    method generate_for_module pre post modu =
      let open Module in
      let open Odoc_html in
      try
        Odoc_info.verbose ("Generate for module "^modu.m_name);
        let (html_file, _) = Naming.html_files modu.m_name in
        let type_file = Naming.file_type_module_complete_target modu.m_name in
        let code_file = Naming.file_code_module_complete_target modu.m_name in
        let chanout = open_out (Filename.concat !Global.target_dir html_file) in
        let b = new_buf () in
        let pre_name = opt (fun m -> m.m_name) pre in
        let post_name = opt (fun m -> m.m_name) post in
        bs b doctype ;
        bs b "<html>\n";
        self#print_header b
          ~nav: (Some (pre_name, post_name, modu.m_name))
          ~comments: (Module.module_comments modu)
          (self#inner_title modu.m_name);
        bs b "<body>\n" ;
        self#print_navbar b pre_name post_name modu.m_name ;
        bs b "<h1>";
        if modu.m_text_only then
          bs b modu.m_name
        else
          (
            bs b
              (
                if Module.module_is_functor modu then
                  Odoc_messages.functo
                else
                  Odoc_messages.modul
              );
            bp b " <a href=\"%s\">%s</a>" type_file modu.m_name;
            (
              match modu.m_code with
                None -> ()
              | Some _ -> bp b " (<a href=\"%s\">.ml</a>)" code_file
            )
          );
        bs b "</h1>\n";
        if not modu.m_text_only then self#html_of_module b ~with_link: false modu;
        (* parameters for functors *)
        self#html_of_module_parameter_list b
          (Name.father modu.m_name)
          (Module.module_parameters modu);
        (* a horizontal line *)
        if not modu.m_text_only then bs b "<hr width=\"100%\">\n";
        (* module elements *)
        (*
          List.iter
          (self#html_of_module_element b (Name.father modu.m_name))
          (Module.module_elements modu);
        *)
        Buffer.output_buffer chanout b;
        close_out chanout;
        (* generate html files for submodules *)
        (* self#generate_elements self#generate_for_module (Module.module_modules modu); *)
        (* generate html files for module types *)
        (* self#generate_elements self#generate_for_module_type (Module.module_module_types modu); *)
        (* generate html files for classes *)
        self#generate_elements self#generate_for_class (Module.module_classes modu);
        (* generate html files for class types *)
        self#generate_elements self#generate_for_class_type (Module.module_class_types modu);
        (* generate the file with the complete module type *)
        self#output_module_type
          modu.m_name
          (Filename.concat !Global.target_dir type_file)
          modu.m_type;
        match modu.m_code with
          None -> ()
        | Some code ->
          self#output_code
            modu.m_name
            (Filename.concat !Global.target_dir code_file)
            code
      with
        Sys_error s ->
          raise (Failure s)

    method html_of_custom_text b s t =
      match s with
      | "figure" -> self#html_of_figure b t
      | _ -> ()
  end
end

let _ = Odoc_args.extend_html_generator (module Generator : Odoc_gen.Html_functor)
