(* see: http://okmij.org/ftp/ML/first-class-modules/generics.ml *)

open Series

module type Interpretation =
sig
  type 'a tc

  (* Primitive types *)
  val unit   : unit tc
  val int    : int tc
  val int64  : int64 tc
  val float  : float tc
  val bool   : bool tc
  val string : string tc

  (* Tuples *)
  val pair : 'a tc -> 'b tc -> ('a * 'b) tc
  val tuple : 'a tc -> 'a tc

  (* Records *)
  val empty_record : unit tc
  val field : string -> 'a tc -> 'a tc
  val (&) : 'a tc -> 'row tc -> ('a * 'row) tc
  val record : 'row tc -> 'row tc

  (* Lists *)
  val list: 'a tc -> 'a Bounded.producer tc

  (* Maps *)
  val mapping: 'a tc -> 'b tc -> ('a,'b) Mapping.t tc
end

module type Repr = sig
  type a
  module Interpret (I : Interpretation) :
  sig
    val result : a I.tc
  end
end

type 'a repr = (module Repr with type a = 'a)

let unit : unit repr = 
  (module struct
        type a = unit
        module Interpret (I : Interpretation) =
        struct
          let result = I.unit
        end
      end : Repr with type a = unit)

let int : int repr =
  (module struct
      type a = int
      module Interpret (I : Interpretation) =
      struct
        let result = I.int
      end
    end : Repr with type a = int)

let int64 : int64 repr =
  (module struct
      type a = int64
      module Interpret (I : Interpretation) =
      struct
        let result = I.int64
      end
    end : Repr with type a = int64)

let float : float repr =
  (module struct
      type a = float
      module Interpret (I : Interpretation) =
      struct
        let result = I.float
      end
    end : Repr with type a = float)

let bool : bool repr =
  (module struct
      type a = bool
      module Interpret (I : Interpretation) =
      struct
        let result = I.bool
      end
    end : Repr with type a = bool)

let string : string repr =
  (module struct
        type a = string
        module Interpret (I : Interpretation) =
        struct
          let result = I.string
        end
      end : Repr with type a = string)

(* Tuples *)
let pair : 'a 'b. 'a repr -> 'b repr -> ('a * 'b) repr
  = fun (type a') (type b') arepr brepr ->
  (module struct
        type a = a' * b'
        module A = (val arepr : Repr with type a = a')
        module B = (val brepr : Repr with type a = b')
        module Interpret (I : Interpretation) =
        struct
          module AI = A.Interpret(I)
          module BI = B.Interpret(I)
          open I
          let result = pair AI.result BI.result
        end
      end : Repr with type a = a' * b')

let tuple : 'a. 'a repr -> 'a repr
  = fun (type t) pair_rep ->
  (module struct
        type a = t
        module A = (val pair_rep : Repr with type a = t)
        module Interpret (I : Interpretation) =
        struct
          module AI = A.Interpret(I)
          let result = I.tuple AI.result
        end
      end : Repr with type a = t)
    
(* Records *)
let empty_record : unit repr = 
  (module struct
        type a = unit
        module Interpret (I : Interpretation) =
        struct
          let result = I.empty_record
        end
      end : Repr with type a = unit)

let field : 'a. string -> 'a repr -> 'a repr =
  fun (type t) label arepr ->
  (module struct
        type a = t
        module A = (val arepr : Repr with type a = t)
        module Interpret (I : Interpretation) =
        struct
          module AI = A.Interpret(I)
          let result = I.field label AI.result
        end
      end : Repr with type a = t)

let (&) : 'a 'row. 'a repr -> 'row repr -> ('a * 'row) repr =
  fun (type a') (type row') arepr brepr ->
  (module struct
        type a = a' * row'
        module A = (val arepr : Repr with type a = a')
        module R = (val brepr : Repr with type a = row')
        module Interpret (I : Interpretation) =
        struct
          module AI = A.Interpret(I)
          module RI = R.Interpret(I)
          open I
          let result = AI.result & RI.result
        end
      end : Repr with type a = a' * row')

let record : 'row. 'row repr -> 'row repr =
  fun (type r) rec_rep ->
  (module struct
        type a = r
        module A = (val rec_rep : Repr with type a = r)
        module Interpret (I : Interpretation) =
        struct
          module AI = A.Interpret(I)
          let result = I.record AI.result
        end
      end : Repr with type a = r)

(* Lists *)

let list : 'a. 'a repr -> 'a Bounded.producer repr =
  fun (type i) item_rep ->
  (module struct
        type a = i Bounded.producer
        module A = (val item_rep : Repr with type a = i)
        module Interpret (I : Interpretation) =
        struct
          module AI = A.Interpret(I)
          let result = I.list AI.result
        end
      end : Repr with type a = i Bounded.producer)

(* Mappings *)
let mapping : 'a 'b. 'a repr -> 'b repr -> ('a,'b) Mapping.t repr =
  fun (type a') (type b') arepr brepr ->
  (module struct
        type a = (a',b') Mapping.t
        module A = (val arepr : Repr with type a = a')
        module B = (val brepr : Repr with type a = b')
        module Interpret (I : Interpretation) =
        struct
          module AI = A.Interpret(I)
          module BI = B.Interpret(I)
          let result = I.mapping AI.result BI.result
        end
      end : Repr with type a = (a',b') Mapping.t)

