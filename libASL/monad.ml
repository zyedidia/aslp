(** Primitive monad definition. *)
module type S = sig
  (** Monad type parameter. *)
  type 'a m

  (** Lifts a function over values into a function over the monad. *)
  val fmap : ('a -> 'b) -> 'a m -> 'b m

  (** Injects a value into the monad context with no side effects. *)
  val pure : 'a -> 'a m

  (** Sequentially compose two monad actions, passing the value
      from the first into the second. *)
  val bind : 'a m -> ('a -> 'b m) -> 'b m
end

(** Constructs monad functions from the given primitive monad definition. *)
module Make (M : S) = struct

  open M

  (** Operator for sequencing two actions with bind. *)
  let (>>=) = bind

  (** Operator for sequencing two actions and discarding the first's result. *)
  let (>>) x y = bind x (fun _ -> y)

  let (<$>) = fmap

  let (<*>) f x = bind f (fun f' -> bind x (fun x' -> pure (f' x')))

  (** Let syntactic sugar for monadic bind and map operations. *)
  module Let = struct
    let (let+) x f = fmap f x
    let (let*) = bind

    let (and+) x y =
      let* x' = x in
      let+ y' = y in
      (x',y')

    let (and*) = (and+)
  end

  (** A nil computation. Does nothing and returns nothing of interest. *)
  let unit: unit m = pure ()

  (** Executes the given action if the given boolean is true,
      otherwise does nothing. *)
  let if_ (b: bool) (x: unit m): unit m =
    if b then x else unit

end
