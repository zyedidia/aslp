
module type S = sig
  type r
  type w (* assumes w is inside a list *)
  type s

  val mempty : w
  val mappend : w -> w -> w
end

module Make (T : S) = struct
  include T

  type 'a rws = r -> s -> 'a * s * w

  (* monad definition for rws *)

  let pure (a: 'a): 'a rws =
    fun _ s -> (a, s, T.mempty)

  let bind (x: 'a rws) (f: 'a -> 'b rws): 'b rws =
    fun r s -> 
      let (a, s', w) = x r s in
      let (b, s'', w') = f a r s' in
      (b, s'', T.mappend w w')

  let (>>=) = bind

  (* monad and applicative syntax for rws *)

  module Let = struct
    let (let*) = bind
    let (and*) x y = 
      let* x in 
      let* y in 
      pure (x, y)

    let (let+) x f = let* x in pure (f x)
    let (and+) = (and*)
  end

  open Let

  (* higher-order functions and transformations *)

  let fmap (f: 'a -> 'b) (x: 'a rws): 'b rws =
    let+ x in f x

  let rec sequence (xs: 'a rws list): 'a list rws =
    match xs with
    | (x::xs) -> 
      let+ x = x
      and+ xs = sequence xs in
        (x :: xs)
    | [] -> pure []

    
  let sequence_ (xs : 'a rws list): unit rws =
    let+ _ = sequence xs in ()

  let traverse (f: 'a -> 'b rws) (x: 'a list): 'b list rws =
    sequence (List.map f x)

  (* rws-specific utility functions *)

  let unit: unit rws = pure ()

  (* reader monad *)

  (** A computation returning the immutable reader environment. *)
  let read: r rws = fun r s -> (r, s, mempty)
  let reads (f: r -> 'a): 'a rws = fun r s -> (f r, s, mempty)

  let write (w: w): unit rws = fun _ s -> ((), s, w)


  (* state monad *)
  let gets (f: s -> 'a): 'a rws = fun _ s -> (f s, s, mempty)
  let get: s rws = fun _ s -> (s, s, mempty)
  let put (s: s): unit rws = fun _ _ -> ((), s, mempty)

  let modify (f: s -> s): unit rws = fun _ s -> ((), f s, mempty)


end


module Test = struct
  module Hi = Make(struct
    type r = unit
    type w = unit list
    type s = unit
    let mempty = []
    let mappend = (@)
  end);;

  open Hi;;
  open Hi.Let;;

  let test: int Hi.rws = fun x y -> (100, y, [(); (); (); ()])

  let a = let* x = test and* y = pure "a" in pure x;;

  let main = 
    let* xx =
      try reads (fun e -> (* raise (Invalid_argument "a"); *) 99999)
      with Invalid_argument _ -> pure 55 in 
    let* a = Hi.pure "asdf"
    and+ b = test
    and+ c = test in
      Printf.printf "%s %d\n" a b;
      Printf.printf "xx = %d\n" xx;
      pure a;;

  List.iter (fun x -> Printf.printf "() ") (match (main () ()) with (a, s, w) -> w);;

  Printf.printf "asdfdasf";
end;;
