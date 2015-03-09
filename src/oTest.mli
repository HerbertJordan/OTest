type test_context = {
  warnings : int ref;
  failed : string list ref;
  success : bool ref;
}
val init_test_context : unit -> test_context
val global_context : test_context ref
val register_error : unit -> unit
val with_unit : int -> string -> string
type test_case = string * (unit -> unit)
type test = TestCase of test_case | TestSuite of string * test list
val count_tests : test -> int
val count_suits : test -> int
exception Failed
val run_test : string -> test -> unit
val ( >: ) : string -> (unit -> unit) -> test
val ( >:: ) : string -> test list -> test
val run_test_main : test -> 'a
val test_expectation : bool -> string -> unit
val test_assertion : bool -> string -> unit
val fail : string -> unit
val test_true : 'a -> ('a -> string -> 'b) -> 'b
val test_false : bool -> (bool -> string -> 'a) -> 'a
val test_pred2 :
  ('a -> 'b -> 'c) -> 'a -> 'b -> string -> ('c -> string -> 'd) -> 'd
val test_eq : 'a -> 'a -> (bool -> string -> 'b) -> 'b
val test_ne : 'a -> 'a -> (bool -> string -> 'b) -> 'b
val test_lt : 'a -> 'a -> (bool -> string -> 'b) -> 'b
val test_le : 'a -> 'a -> (bool -> string -> 'b) -> 'b
val test_ge : 'a -> 'a -> (bool -> string -> 'b) -> 'b
val test_gt : 'a -> 'a -> (bool -> string -> 'b) -> 'b
val assert_true : bool -> unit
val assert_false : bool -> unit
val assert_eq : 'a -> 'a -> unit
val assert_ne : 'a -> 'a -> unit
val assert_lt : 'a -> 'a -> unit
val assert_le : 'a -> 'a -> unit
val assert_ge : 'a -> 'a -> unit
val assert_gt : 'a -> 'a -> unit
val expect_true : bool -> unit
val expect_false : bool -> unit
val expect_eq : 'a -> 'a -> unit
val expect_ne : 'a -> 'a -> unit
val expect_lt : 'a -> 'a -> unit
val expect_le : 'a -> 'a -> unit
val expect_ge : 'a -> 'a -> unit
val expect_gt : 'a -> 'a -> unit
