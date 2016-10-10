let () =
    Random.self_init ();
    let x = [| "A ham sandwich walks into a bar and orders a beer. Bartender says, ‘Sorry we don’t serve food here."; "C'est l'histoire d'une blague vaseuse...Mets tes bottes."; "Comment appelle t-on un bébé éléphant prématuré ?...Un éléphant tôt." ; "C'est un mec qui entre dans un bar et qui dit 'Salut c'est moi !' mais en fait c'était pas lui."; "Comment ramasse-t-on la papaye ?...Avec une foufourche."; "Comment s'appelle un boomerang qui ne revient pas ???...Un bout de bois !!!" |] in
    print_endline x.(Random.int 5);

