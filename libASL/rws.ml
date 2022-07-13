
module type S = sig
  type r
  type w
  type s

  val mempty : w
  val mappend : w -> w -> w
end

module Make (T : S) = struct
  include T

  (** A reader-writer-state monad. 
      This is a function which takes an immutable "environment",
      a local mutable "state", and produces a value of some type 'a, 
      along with a new local state and side-effects of writes. *)
  type 'a rws = r -> s -> 'a * s * w

  (* monad definition for rws *)

  let fmap (f: 'a -> 'b) (x: 'a rws): 'b rws =
    fun r s -> 
      let (a,s',w) = x r s in 
      (f a, s', w)

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

    let (let+) x f = fmap f x
    let (and+) = (and*)
  end

  open Let

  (* higher-order functions and transformations *)
  
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

  let traverse_ (f: 'a -> 'b rws) (x: 'a list): unit rws =
    let+ _ = sequence (List.map f x) in ()

  (* rws-specific utility functions *)

  (** A nil computation. Does nothing and returns nothing of interest. *)
  let unit: unit rws = pure ()

  (* READER *)

  (** A computation returning the immutable reader environment. *)
  let read: r rws = fun r s -> (r, s, mempty)

  (** A computation reading the environment and applying the given function to it. *)
  let reads (f: r -> 'a): 'a rws = fun r s -> (f r, s, mempty)

  (* WRITER *)

  (** A computation writing the given values. *)
  let write (w: w): unit rws = fun _ s -> ((), s, w)


  (* STATE *)

  (** A computation returning the value of the local mutable state.  *)
  let get: s rws = fun _ s -> (s, s, mempty)

  (** A computation reading the local state and applying the given function.  *)
  let gets (f: s -> 'a): 'a rws = fun _ s -> (f s, s, mempty)

  (** A computation setting the local state to the given value. *)
  let put (s: s): unit rws = fun _ _ -> ((), s, mempty)

  (** A computation modifying the local state using the given function.  *)
  let modify (f: s -> s): unit rws = fun _ s -> ((), f s, mempty)

  (** A computation returning some value and also modifying state using the given function.  *)
  let stateful (f: s -> 'a * s): 'a rws =
    fun _ s -> let (a,s') = f s in (a, s', mempty)


  (** Runs a computation transiently without modifying the state or writer.
      Instead, returns the final state and writer values. *)
  let locally (x: 'a rws): ('a * s * w) rws =
    fun r s -> 
      let (a,s',w) = x r s in 
      ((a,s',w),s,T.mempty)

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
