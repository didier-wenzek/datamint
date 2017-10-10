open Series

module type S = sig
  type a
  module Interpret (I : Interpretation.S) :
  sig
    val result : a I.tc
  end
end

type 'a t = (module S with type a = 'a)

let unit : unit t = 
  (module struct
        type a = unit
        module Interpret (I : Interpretation.S) =
        struct
          let result = I.unit
        end
      end : S with type a = unit)

let int : int t =
  (module struct
      type a = int
      module Interpret (I : Interpretation.S) =
      struct
        let result = I.int
      end
    end : S with type a = int)

let int64 : int64 t =
  (module struct
      type a = int64
      module Interpret (I : Interpretation.S) =
      struct
        let result = I.int64
      end
    end : S with type a = int64)

let float : float t =
  (module struct
      type a = float
      module Interpret (I : Interpretation.S) =
      struct
        let result = I.float
      end
    end : S with type a = float)

let bool : bool t =
  (module struct
      type a = bool
      module Interpret (I : Interpretation.S) =
      struct
        let result = I.bool
      end
    end : S with type a = bool)

let string : string t =
  (module struct
        type a = string
        module Interpret (I : Interpretation.S) =
        struct
          let result = I.string
        end
      end : S with type a = string)

(* Tuples *)
let pair : 'a 'b. 'a t -> 'b t -> ('a * 'b) t
  = fun (type a') (type b') arepr brepr ->
  (module struct
        type a = a' * b'
        module A = (val arepr : S with type a = a')
        module B = (val brepr : S with type a = b')
        module Interpret (I : Interpretation.S) =
        struct
          module AI = A.Interpret(I)
          module BI = B.Interpret(I)
          open I
          let result = pair AI.result BI.result
        end
      end : S with type a = a' * b')

let tuple : 'a. 'a t -> 'a t
  = fun (type t) pair_rep ->
  (module struct
        type a = t
        module A = (val pair_rep : S with type a = t)
        module Interpret (I : Interpretation.S) =
        struct
          module AI = A.Interpret(I)
          let result = I.tuple AI.result
        end
      end : S with type a = t)
    
(* Records *)
let empty_record : unit t = 
  (module struct
        type a = unit
        module Interpret (I : Interpretation.S) =
        struct
          let result = I.empty_record
        end
      end : S with type a = unit)

let field : 'a. string -> 'a t -> 'a t =
  fun (type t) label arepr ->
  (module struct
        type a = t
        module A = (val arepr : S with type a = t)
        module Interpret (I : Interpretation.S) =
        struct
          module AI = A.Interpret(I)
          let result = I.field label AI.result
        end
      end : S with type a = t)

let (&) : 'a 'row. 'a t -> 'row t -> ('a * 'row) t =
  fun (type a') (type row') arepr brepr ->
  (module struct
        type a = a' * row'
        module A = (val arepr : S with type a = a')
        module R = (val brepr : S with type a = row')
        module Interpret (I : Interpretation.S) =
        struct
          module AI = A.Interpret(I)
          module RI = R.Interpret(I)
          open I
          let result = AI.result & RI.result
        end
      end : S with type a = a' * row')

let record : 'row. 'row t -> 'row t =
  fun (type r) rec_rep ->
  (module struct
        type a = r
        module A = (val rec_rep : S with type a = r)
        module Interpret (I : Interpretation.S) =
        struct
          module AI = A.Interpret(I)
          let result = I.record AI.result
        end
      end : S with type a = r)

(* Lists *)

let list : 'a. 'a t -> 'a Bounded.producer t =
  fun (type i) item_rep ->
  (module struct
        type a = i Bounded.producer
        module A = (val item_rep : S with type a = i)
        module Interpret (I : Interpretation.S) =
        struct
          module AI = A.Interpret(I)
          let result = I.list AI.result
        end
      end : S with type a = i Bounded.producer)

(* Mappings *)
let mapping : 'a 'b. 'a t -> 'b t -> ('a,'b) Mapping.t t =
  fun (type a') (type b') arepr brepr ->
  (module struct
        type a = (a',b') Mapping.t
        module A = (val arepr : S with type a = a')
        module B = (val brepr : S with type a = b')
        module Interpret (I : Interpretation.S) =
        struct
          module AI = A.Interpret(I)
          module BI = B.Interpret(I)
          let result = I.mapping AI.result BI.result
        end
      end : S with type a = (a',b') Mapping.t)

