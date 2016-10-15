(* TODO save state player in file *)

let player = new Player.player (* init player *)

let window = GWindow.window ~resizable: false ~position: `CENTER ~title:"Rush 01" () (* init window *)

(* main layout *)
let mainPck = GPack.vbox
                ~spacing: 3
                ~packing: (window#add) ()

(* Action button *)
let btn = ["EAT"; "THUNDER"; "BATH"; "KILL"](* available button *)

let btnPck =  GPack.button_box `HORIZONTAL
                        ~spacing: 3
                        ~layout: `DEFAULT_STYLE
                        ~packing: (mainPck#add) ()

let genBtn =
        let rec init btnList = match btnList with
            | hd::tl -> print_endline (hd ^ " init");
            let btn  = GButton.button ~label:hd ~packing:btnPck#add () in
            let f () = match hd with
                        | "EAT" -> player#eat
                        | "THUNDER" -> player#thunder
                        | "BATH" -> player#bath
                        | "KILL" -> player#kill
                        | _ -> () in
                ignore(btn#connect#clicked ~callback:f);
                init tl
            | [] -> ()
        in init btn

(* player image TODO *)
let getFilename x = "sprites/" ^ ( string_of_int x ) ^ ".png"
let getPixbuf x = GdkPixbuf.from_file (getFilename x)

let image = GMisc.image ~packing: mainPck#add ()

let x = ref 0
let setX () = if !x > 9 then x := 0 else x := !x

let updateImage () =
    setX ();
    image#set_pixbuf (getPixbuf !x);
    x := !x + 1

(* Progress bar *)
let barPck =  GPack.hbox ~packing: (mainPck#add) ()
let bar =
        let rec init lst i =
            if i = 0 then lst
            else init (lst @ [GRange.progress_bar ~packing: (barPck#add) ()]) (i -1)
        in init [] (List.length player#getStats)

let updateBar () =
    let rec loop progr stats= match progr, stats with
        | hd::tl , (name, value)::tl2 ->
                hd#set_text name;
                hd#set_fraction (value /. 100.); (* convert % *)
                loop tl tl2
        | _ , _ -> ()
    in loop bar player#getStats

(* game over *)
let updateGameOver () =
    if player#isAlive = false then
        begin
            (* TODO implem game over here *)
            print_endline "u lose, u can't win!!";
            window#destroy ()
        end

(* main update *)
let update () =
    print_endline "update";
    updateBar ();
    updateImage ();
    updateGameOver ();
    true

let destroy () =
    GMain.Main.quit ()

let main () =
      ignore(GtkMain.Main.init ());
      ignore(GMain.Timeout.add ~ms:420 ~callback:(update));
      ignore(window#connect#destroy ~callback:destroy);
      window#show ();
      GMain.Main.main ()


let () = main ()
