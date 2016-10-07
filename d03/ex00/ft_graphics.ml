type 'a tree = Nil | Node of 'a * 'a tree * 'a tree

let draw_sq x y size =
    Graphics.moveto x y;
    let l1 = (x + size) in
    Graphics.lineto l1 y;
    let l2 = (y + size) in
    Graphics.lineto l1 l2;
    let l3 = (l1 - size) in
        Graphics.lineto l3 l2;
        Graphics.lineto x y

let draw_link x y size offset =
    Graphics.moveto  (x + size) y;
    Graphics.lineto (x + offset) (y - (size / 2));
    Graphics.moveto  (x + size) (y + size);
    Graphics.lineto (x + offset) (y + (size + (size / 2)))

let draw_string x y str =
	let pos_str = 20 in
        Graphics.moveto (x + pos_str) (y + pos_str);
	    Graphics.draw_string str

let draw_tree_node  node =
	let base_x = 50 in
	let base_y = 300 in
	let size = 50 in
	let offset = 100 in
	let rec draw_tree node x y =
	match node with
    | Node (v, l, r) ->
                            draw_sq x y size;
							draw_string x y v;
                            draw_link x y size offset;
                            draw_tree l (x + offset) (y + size);
                            draw_tree r (x + offset) (y - size)

    | Nil ->
                    draw_sq x y size;
					draw_string x y "Nil";
	in draw_tree node base_x base_y


let draw_square x y size =
    let x = (x - (size / 2)) in
    let y = (y - (size / 2)) in
		draw_sq x y size

let main () =
    Graphics.open_graph " 800x600";
    Graphics.set_window_title "ft_graphics";
    draw_square 400 300 30;
    draw_tree_node (Node
		("first", Nil, Nil));
    Graphics.read_key()


let _ = main ()
