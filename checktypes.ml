(******************************************************************)
(* TODO                                        *)
(******************************************************************)

(* 
module type TEST_DATA = sig
  module type Tests = sig
    val tests : OUnit2.test list
  end
  module DictTester (M:Data.DictionaryMaker) : Tests
  val tests : OUnit2.test list
end

module type TEST_ENGINE = sig
  val tests : OUnit2.test list
end *)

module type PREPROCESSING =  sig
  val keywords_list : string-> string list
  val remove_noise : string -> string list -> string
  val k_grams : string -> int -> string list
  val hash : string -> int
  val winnow : int list -> int -> int list
end

module type COMPARISON = sig
  type file_dict
  val dir_to_dict : string -> file_dict
  type pair_comparison
  val compare : file_dict -> pair_comparison list
end

module type WINNOWING = sig
  module type BoundedQueueWithCounter = sig
    type 'a t
    val create : int -> 'a t
    val is_empty : 'a t -> bool
    val size : 'a t -> int
    val enqueue : 'a -> 'a t -> 'a t
    val dequeue : 'a t -> 'a t
    val count : 'a t -> int
    val fold : ('b -> 'a -> 'b) -> 'b -> 'a t -> 'b
  end
  module Window : BoundedQueueWithCounter
  val winnow: int list -> int -> int list 
end

module type DICTIONARY = sig
  module type Comparable = sig
    type t
    val compare: t -> t -> int
  end

  module type Dictionary = sig
    module Key : Comparable
    type key = Key.t
    type value
    type t
    val empty : t
    val member : key -> t -> bool
    val find : key -> t -> value option
    val insert : key -> int -> t -> t
    val remove : key -> t -> t
    val to_list : t -> (key * value) list
  end

end

module CheckComparison : COMPARISON = Comparison
module CheckPreprocessing : PREPROCESSING = Preprocessing
module CheckWinnowing : WINNOWING = Winnowing
module CheckDictionary : DICTIONARY = Dictionary
