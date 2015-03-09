(* This is an example unit test case *)
open OTest

(* define a test case*)

let test_case = "Test" >:: [

		"Basic" >: (fun () ->
			()		(* an empty test *)
		);
		
		"Advanced" >: (fun () ->
			let x = 2 in
			let y = 3 in
			expect_eq (y + y) ( x * y)
		);
		
		"Failing" >: (fun () ->
			let x = 2 in
			let y = 3 in
			expect_eq (x + x) ( x * y) ;
			expect_gt x y ;
			expect_false ( ( x + y ) == ( y + x ) ) 
		);
		
		"Asserting" >: (fun () ->
			expect_eq 2 3 ;
			fail "Stop here!" ;
			expect_eq 2 3 
		);
		
		"Nested" >:: [
			"First"  >: (fun () -> ()) ;
			"Second" >: (fun () -> ( expect_eq "Hello" "World" )) ;
			"Third"  >: (fun () -> ( fail "User triggered fail!" ) )
		]
			
	]


(* run test suite*)
let _ = run_test_main test_case
