type ('a,'b,'c) t = {
  push: 'a -> 'b -> 'c;
  cont: 'c -> ('b -> 'c) -> 'c;
  term: 'b -> 'c;
}

let sync_action push = {
  push;
  cont = (fun x f -> f x);
  term = (fun x -> x);
}

let lwt_action push = {
  push;
  cont = Lwt.bind;
  term = Lwt.return;
}

let cap_with capped red = {
  red with
  cont = (fun xp f -> red.cont xp (fun x ->
    if capped x
    then red.term x
    else f x
  ));
}
