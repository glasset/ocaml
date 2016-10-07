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

let draw_tree  node =
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
					draw_string x y "";
	in draw_tree node base_x base_y

let size node =
    let rec count_node c_node s = match c_node with
            | Node (v, l, r) -> let add = s in
                (count_node l add) + (count_node r add)
            | Nil -> (s + 1)
    in (count_node node 0) - 1

let height node =
    let rec count_node c_node s = match c_node with
            | Node (v, l, r) -> let add = (s + 1) in
                let l_res = (count_node l add) in
                let r_res = (count_node r add) in
                    if  l_res < r_res then
                        r_res
                    else
                        l_res
            | Nil -> s
    in (count_node node 0)

let main () =
    Graphics.open_graph " 800x600";
    Graphics.set_window_title "ft_graphics";

    print_newline (print_int
            (size (Node(1,
                            Node(1, Nil, Node(1, Nil, Nil)),
                            Node(1, Nil, Node(1, Node(1, Nil, Nil), Nil))
                            ))
    ));

    print_newline (print_int
            (height (Node(1,
                            Node(1, Nil, Node(1, Nil, Nil)),
                            Node(1, Nil, Node(1, Node(1, Nil, Nil), Nil))
                            ))
    ));

    draw_tree (Node("start",
                            Node("second", Nil, Node("third", Nil, Nil)),
                            Node("max", Nil, Node("height", Node("here", Nil, Nil), Nil))
    ));
    Graphics.read_key()


let _ = main ()

