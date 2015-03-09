(* *)
(* General docu:*)
(* http://caml.inria.fr/pub/docs/manual-ocaml/libref*)
(* *)

open Colors


(** The context information forwarded between tests *)
type test_context = {
		warnings : int ref ;
		failed : string list ref;
		success : bool ref ;
	}

(** Creates a new test context *)
let init_test_context = fun () -> {
		warnings = ref 0;
		failed = ref [] ;
		success = ref true;
	}

(** A global context to trace the progresses of tests *)
let global_context = ref ( init_test_context () )

(** registers a new error in the global context *)
let register_error = fun () ->
	(!global_context).success := false


let with_unit n s = 
	string_of_int(n) ^ " " ^ s ^ (if n > 1 then "s" else "" )

(** A test case consists of a name and a test function *)
type test_case = string * ( unit -> unit ) 

(** A hierarchy of test cases *)
type test =
    TestCase of test_case
  | TestSuite of string * test list 

(** Counts the number of tests in a suite *)
let rec count_tests test = match test with
	| TestCase case -> 1
	| TestSuite ( _ , list ) -> (List.fold_left (fun a b -> a + (count_tests b)) 0 list) 

(** Counts the number of test suits in a test hierarchy *)
let rec count_suits test = match test with
	| TestCase case -> 0
	| TestSuite ( _ , list ) -> (List.fold_left (fun a b -> a + (count_suits b)) 0 list) + 1 


(** An exception to be thrown when aborting a test case  *)
exception Failed

(** Runs a hierarch of tests *)
let rec run_test path test = match test with
	| TestCase ( case_name , case ) ->
		let name = path ^ case_name in 
		let success = (!global_context).success in 
  		
			(* print initial line *)
			success := true ;
  		print_string ( green "[ RUN      ] " ) ;
  		print_endline name ; 

			(* run test case *)			
			let start = Time.now () in
			(
  			try
    			case () ;
  			with Failed -> () 
			) ;
			let duration = (Time.now () - start) in
  		
			(* print summary line *)
			if (!success) then
  			print_string ( green "[       OK ] " ) 
  		else (
  		  print_string ( red "[  FAILED  ] " ) ;
				(!global_context).failed := ( List.append (!((!global_context).failed)) [ name ] ) 
			) ;
  		print_endline ( name ^ " (" ^ (string_of_int duration) ^ " ms)") 
			
	| TestSuite ( suite_name, list ) -> ( 
			print_string ( green "[----------] " ) ;
			let n = count_tests test in 
			print_endline ( (with_unit n "test") ^ " from " ^ path ^ suite_name ) ;
			let start = Time.now() in
			List.iter (fun t -> ( 
  			run_test ( path ^ suite_name ^ "." ) t )
  		) list ;
			let duration = (Time.now() - start) in
			print_string ( green "[----------] " ) ;
			print_endline ( (with_unit n "test") ^ " from " ^ path ^ suite_name ^ " (" ^ (string_of_int duration) ^ " ms)") 
		)


(** A constructor for a test case *)
let (>:) name body = TestCase (name , body )

(** A constructor for a (nested) test suite *)
let (>::) name cases = TestSuite ( name, cases )

(** A test runner to be utilized as a main entry point *)
let run_test_main t = 
	
	(* print info line *)
	print_endline "Running run_test_main from GoTest" ;
	
	(* setup infrastructure *)
	global_context := (init_test_context () ) ;
	
	let n = count_tests t in
	let s = count_suits t in 
	
  	print_string ( green "[==========] " ) ;
		print_endline ("Running " ^ (with_unit n "test") ^ " from " ^ (with_unit s "test case") ^ ".") ; 
	
		(* run actual tests *)
  	run_test "" t ;
  	
  	print_string ( green "[==========] " ) ;
		print_endline ( (with_unit n "test") ^ " from " ^ (with_unit s "test case") ^ " ran.") ; 
  	print_string ( green "[  PASSED  ] " ) ;
		
		(* print summary *)
  	let errors = List.length !((!global_context).failed) in 
    	
  		print_endline ( ( with_unit ( n - errors ) "test" ) ^ "." ) ;
    	
    	if ( errors > 0 ) then (
      	print_string ( red "[  FAILED  ] " ) ;
				print_endline ( ( with_unit ( errors ) "test" ) ^ ", listed below:" ) ;
			  List.iter (fun s -> 
					print_endline ( (red "[  FAILED  ] ") ^ s) 
				) (!((!global_context).failed))
  		) else () ;
    	
			(* terminate with exit code *)
    	if ( errors > 0 ) then (
				print_newline () ;
    		print_string " " ;
    		print_int errors ;
    		print_string " FAILED TEST" ;
    		if ( errors > 1 ) then
    			print_endline "S"
    		else 
    			print_newline () ;
    		exit 1 
    	) else (
    		exit 0
    	)
  	
(* should work with -pp "camlp5o pa_macro.cmo" DEFINE EXPECT_FAIL t =      *)

(*
 * The following constitutes a list of expect / assert functions to implement
 * the actual unit testing functions.
 *
 * At the moment no source locatoin information for individual checks can be
 * obtained. A future fix might be to use macros introduced by an optional 
 * pre-processor using the flag
 *
 *  							-pp "camlp5o pa_macro.cmo" 
 *
 * and macro definitions similar to
 * 
 * 								DEFINE EXPECT_EQ t = expect_eq __LOC__ t  
 * 
 * to make the location available. Those features, however, are only supported
 * since OCaml 4.02 and camlp5
 *)


(** An internal method for the actual evaluatoin of an expectation *)
let test_expectation b msg =
	if (not b) then (
		print_string "Failure: " ;
  	print_endline msg;
  	(* -- the call stack does not include inlined functions --
  	print_endline "Call Stack:" ;
  	print_endline ( Printexc.raw_backtrace_to_string ( Printexc.get_callstack 20 ) ) ;
  	*)
  	register_error ()	
	) else ()

(** The internal foundation for all assertions *)
let test_assertion b msg =
	test_expectation b msg ;
	if (not b) then ( raise Failed ) else ()

(** A condition-less assertion *)
let fail msg = test_assertion false msg

(** raw tests - neither expect nor assert *)
let test_true b test = test b "Expected value to be true."
let test_false b test = test (not b) "Expected value to be false."

let test_pred2 p a b p_name test = test ( p a b ) 
		( 
			"Assumption a " ^ p_name ^ " b failed\n" ^
			" a: " ^ (Std.dump a ) ^ "\n" ^
			" b: " ^ (Std.dump b )
		)

let test_eq a b test = test_pred2  (=) a b  "=" test
let test_ne a b test = test_pred2 (<>) a b "<>" test
let test_lt a b test = test_pred2  (<) a b  "<" test
let test_le a b test = test_pred2 (<=) a b "<=" test
let test_ge a b test = test_pred2 (>=) a b ">=" test
let test_gt a b test = test_pred2  (>) a b  ">" test

(** assertions *)
let assert_true b = test_true b test_assertion
let assert_false b = test_false b test_assertion
let assert_eq a b = test_eq a b test_assertion
let assert_ne a b = test_ne a b test_assertion
let assert_lt a b = test_lt a b test_assertion
let assert_le a b = test_le a b test_assertion
let assert_ge a b = test_ge a b test_assertion
let assert_gt a b = test_gt a b test_assertion 

(** expectations *)
let expect_true b = test_true b test_expectation
let expect_false b = test_false b test_expectation
let expect_eq a b = test_eq a b test_expectation
let expect_ne a b = test_ne a b test_expectation
let expect_lt a b = test_lt a b test_expectation
let expect_le a b = test_le a b test_expectation
let expect_ge a b = test_ge a b test_expectation
let expect_gt a b = test_gt a b test_expectation 

