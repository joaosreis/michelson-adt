open! Containers
open Edo_adt
open Typer

let create_stack : unit -> Typ.t Stack.t = fun () -> Stack.create ()
let inst = Alcotest.testable Adt.Inst.pp Adt.Inst.equal
let inst_typed = Alcotest.testable Typed_adt.Inst.pp Typed_adt.Inst.equal

let inst_t =
  let open Adt in
  Alcotest.testable Inst.pp_inst_t (fun x y -> compare_inst_t x y = 0)

let inst_t_typed =
  let open Typed_adt in
  Alcotest.testable Inst.pp_inst_t (fun x y -> compare_inst_t x y = 0)

let typ =
  let open Typ in
  Alcotest.testable pp are_compatible

let dummy_loc = Common_adt.Loc.dummy_loc
let create_adt_typ t = Typed_adt.Typ.create 0 t
let create_inst i = Adt.Inst.create 0 i
let create_inst_typed i = Typed_adt.Inst.create 0 i
let create_data d = Typed_adt.Data.create 0 d
let create_typ (t : Typ.t') = (t, [])

let abs_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_abs ~actual:(type_abs dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let abs_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_abs dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let abs_tests =
  Alcotest.
    [ test_case "abs_ok" `Quick abs_ok; test_case "abs_nok" `Quick abs_nok ]

(****************************************************************************)

let drop_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_drop Z.one)
    ~actual:(type_drop dummy_loc stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let drop_tests = Alcotest.[ test_case "drop_ok" `Quick drop_ok ]

(****************************************************************************)

let dup_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_dup Z.one)
    ~actual:(type_dup dummy_loc stack Z.one);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dup_tests = Alcotest.[ test_case "dup_ok" `Quick dup_ok ]

(****************************************************************************)

let swap_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_swap
    ~actual:(type_swap dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let swap_tests = Alcotest.[ test_case "swap_ok" `Quick swap_ok ]

(****************************************************************************)

let unit_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_unit
    ~actual:(type_unit dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Unit))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let unit_tests = Alcotest.[ test_case "unit_ok" `Quick unit_ok ]

(****************************************************************************)

let eq_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_eq ~actual:(type_eq dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let eq_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_eq dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let eq_tests =
  Alcotest.[ test_case "eq_ok" `Quick eq_ok; test_case "eq_nok" `Quick eq_nok ]

(****************************************************************************)

let neq_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_neq ~actual:(type_neq dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let neq_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_neq dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let neq_tests =
  Alcotest.
    [ test_case "neq_ok" `Quick neq_ok; test_case "neq_nok" `Quick neq_nok ]

(****************************************************************************)

let lt_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_lt ~actual:(type_lt dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let lt_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_lt dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let lt_tests =
  Alcotest.[ test_case "lt_ok" `Quick lt_ok; test_case "lt_nok" `Quick lt_nok ]

(****************************************************************************)

let gt_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_gt ~actual:(type_gt dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let gt_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_gt dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let gt_tests =
  Alcotest.[ test_case "gt_ok" `Quick gt_ok; test_case "gt_nok" `Quick gt_nok ]

(****************************************************************************)

let le_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_le ~actual:(type_le dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let le_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_le dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let le_tests =
  Alcotest.[ test_case "le_ok" `Quick le_ok; test_case "le_nok" `Quick le_nok ]

(****************************************************************************)

let ge_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_ge ~actual:(type_ge dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let ge_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_ge dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let ge_tests =
  Alcotest.[ test_case "ge_ok" `Quick ge_ok; test_case "ge_nok" `Quick ge_nok ]

(****************************************************************************)

let or_bool_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bool) stack;
  Stack.push (create_typ Bool) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_or_bool ~actual:(type_or dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let or_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_or_nat ~actual:(type_or dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let or_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_or dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let or_tests =
  Alcotest.
    [
      test_case "or_bool_ok" `Quick or_bool_ok;
      test_case "or_nat_ok" `Quick or_nat_ok;
      test_case "or_nok" `Quick or_nok;
    ]

(****************************************************************************)

let and_bool_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bool) stack;
  Stack.push (create_typ Bool) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_and_bool ~actual:(type_and dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let and_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_and_nat ~actual:(type_and dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let and_int_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_and_int_nat
    ~actual:(type_and dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let and_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_and dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let and_tests =
  Alcotest.
    [
      test_case "and_bool_ok" `Quick and_bool_ok;
      test_case "and_nat_ok" `Quick and_nat_ok;
      test_case "and_int_nat_ok" `Quick and_int_nat_ok;
      test_case "and_nok" `Quick and_nok;
    ]

(****************************************************************************)

let xor_bool_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bool) stack;
  Stack.push (create_typ Bool) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_xor_bool ~actual:(type_xor dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let xor_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_xor_nat ~actual:(type_xor dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let xor_tests =
  Alcotest.
    [
      test_case "xor_bool_ok" `Quick xor_bool_ok;
      test_case "xor_nat_ok" `Quick xor_nat_ok;
    ]

(****************************************************************************)

let not_bool_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bool) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_not_bool ~actual:(type_not dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let not_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_not_nat ~actual:(type_not dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let not_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_not_int ~actual:(type_not dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let not_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  let error =
    try
      let _ = type_not dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let not_tests =
  Alcotest.
    [
      test_case "not_bool_ok" `Quick not_bool_ok;
      test_case "not_nat_ok" `Quick not_nat_ok;
      test_case "not_int_ok" `Quick not_int_ok;
      test_case "not_nok" `Quick not_nok;
    ]

(****************************************************************************)

let neg_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_neg_int ~actual:(type_neg dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let neg_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_int_nat ~actual:(type_neg dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let neg_bls12_381_fr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_fr) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_neg_bls12_381_fr
    ~actual:(type_neg dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_fr))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let neg_bls12_381_g1_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_g1) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_neg_bls12_381_g1
    ~actual:(type_neg dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_g1))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let neg_bls12_381_g2_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_g2) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_neg_bls12_381_g2
    ~actual:(type_neg dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_g2))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let neg_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  let error =
    try
      let _ = type_neg dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let neg_tests =
  Alcotest.
    [
      test_case "neg_int_ok" `Quick neg_int_ok;
      test_case "neg_nat_ok" `Quick neg_nat_ok;
      test_case "neg_bls12_381_fr_ok" `Quick neg_bls12_381_fr_ok;
      test_case "neg_bls12_381_g1_ok" `Quick neg_bls12_381_g1_ok;
      test_case "neg_bls12_381_g2_ok" `Quick neg_bls12_381_g2_ok;
      test_case "neg_nok" `Quick neg_nok;
    ]

(****************************************************************************)

let isnat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_isnat
    ~actual:(type_isnat dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ (Option (create_typ Nat))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let isnat_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_isnat dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let isnat_tests =
  Alcotest.
    [
      test_case "isnat_ok" `Quick isnat_ok;
      test_case "isnat_nok" `Quick isnat_nok;
    ]

(****************************************************************************)

let int_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_int_nat ~actual:(type_int dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let int_bls12_381_fr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_fr) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_int_bls12_381_fr
    ~actual:(type_int dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let int_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_int dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let int_tests =
  Alcotest.
    [
      test_case "int_nat_ok" `Quick int_nat_ok;
      test_case "int_bls12_381_fr_ok" `Quick int_bls12_381_fr_ok;
      test_case "int_nok" `Quick int_nok;
    ]

(****************************************************************************)

let add_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_nat ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let add_nat_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_nat_int
    ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_nat_int
    ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let add_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_int ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let add_timestamp_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Timestamp) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Timestamp) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_timestamp_int
    ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Timestamp))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_timestamp_int
    ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Timestamp))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let add_mutez_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Mutez) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_mutez ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Mutez))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let add_bls12_381_g1_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_g1) stack;
  Stack.push (create_typ Bls12_381_g1) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_bls12_381_g1
    ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_g1))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let add_bls12_381_g2_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_g2) stack;
  Stack.push (create_typ Bls12_381_g2) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_bls12_381_g2
    ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_g2))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let add_bls12_381_fr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_fr) stack;
  Stack.push (create_typ Bls12_381_fr) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_add_bls12_381_fr
    ~actual:(type_add dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_fr))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let add_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Mutez) stack;
  let error =
    try
      let _ = type_add dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let add_tests =
  Alcotest.
    [
      test_case "add_nat_ok" `Quick add_nat_ok;
      test_case "add_nat_int_ok" `Quick add_nat_int_ok;
      test_case "add_int_ok" `Quick add_int_ok;
      test_case "add_timestamp_int_ok" `Quick add_timestamp_int_ok;
      test_case "add_mutez_ok" `Quick add_mutez_ok;
      test_case "add_bls12_381_g1_ok" `Quick add_bls12_381_g1_ok;
      test_case "add_bls12_381_g2_ok" `Quick add_bls12_381_g2_ok;
      test_case "add_bls12_381_fr_ok" `Quick add_bls12_381_fr_ok;
      test_case "add_nok" `Quick add_nok;
    ]

(****************************************************************)

let sub_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sub_nat ~actual:(type_sub dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sub_nat_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sub_nat_int
    ~actual:(type_sub dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sub_nat_int
    ~actual:(type_sub dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sub_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sub_int ~actual:(type_sub dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sub_timestamp_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Timestamp) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sub_timestamp_int
    ~actual:(type_sub dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Timestamp))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sub_timestamp_int_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Timestamp) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_sub dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let sub_timestamp_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Timestamp) stack;
  Stack.push (create_typ Timestamp) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sub_timestamp
    ~actual:(type_sub dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sub_mutez_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Mutez) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sub_mutez ~actual:(type_sub dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Mutez))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sub_tests =
  Alcotest.
    [
      test_case "sub_nat_ok" `Quick sub_nat_ok;
      test_case "sub_nat_int_ok" `Quick sub_nat_int_ok;
      test_case "sub_int_ok" `Quick sub_int_ok;
      test_case "sub_timestamp_int_ok" `Quick sub_timestamp_int_ok;
      test_case "sub_timestamp_int_nok" `Quick sub_timestamp_int_nok;
      test_case "sub_timestamp_ok" `Quick sub_timestamp_ok;
      test_case "sub_mutez_ok" `Quick sub_mutez_ok;
    ]

(****************************************************************)

let mul_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_nat ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_nat_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_nat_int
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_nat_int
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_int ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_mutez_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Mutez) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_mutez_nat
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Mutez))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_mutez_nat
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Mutez))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_bls12_381_g1_bls12_381_fr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_fr) stack;
  Stack.push (create_typ Bls12_381_g1) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_bls12_381_g1_bls12_381_fr
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_g1))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_bls12_381_g1_bls12_381_fr_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_g1) stack;
  Stack.push (create_typ Bls12_381_fr) stack;
  let error =
    try
      let _ = type_mul dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let mul_bls12_381_g2_bls12_681_fr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_fr) stack;
  Stack.push (create_typ Bls12_381_g2) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_bls12_381_g2_bls12_381_fr
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_g2))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_bls12_381_g2_bls12_381_fr_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_g2) stack;
  Stack.push (create_typ Bls12_381_fr) stack;
  let error =
    try
      let _ = type_mul dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let mul_bls12_381_fr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_fr) stack;
  Stack.push (create_typ Bls12_381_fr) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_bls12_381_fr_bls12_381_fr
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_fr))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_nat_bls12_381_fr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Bls12_381_fr) stack;
  Stack.push (create_typ Bls12_381_fr) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_nat_bls12_381_fr
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_fr))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_nat_bls12_381_fr
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_fr))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_int_bls12_381_fr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Bls12_381_fr) stack;
  Stack.push (create_typ Bls12_381_fr) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_int_bls12_381_fr
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_fr))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mul_int_bls12_381_fr
    ~actual:(type_mul dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bls12_381_fr))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mul_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_mul dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let mul_tests =
  Alcotest.
    [
      test_case "mul_nat_ok" `Quick mul_nat_ok;
      test_case "mul_int_ok" `Quick mul_int_ok;
      test_case "mul_bls12_381_g1_bls12_381_fr_ok" `Quick
        mul_bls12_381_g1_bls12_381_fr_ok;
      test_case "mul_bls12_381_g1_bls12_381_fr_nok" `Quick
        mul_bls12_381_g1_bls12_381_fr_nok;
      test_case "mul_bls12_381_g2_bls12_681_fr_ok" `Quick
        mul_bls12_381_g2_bls12_681_fr_ok;
      test_case "mul_bls12_381_g2_bls12_381_fr_nok" `Quick
        mul_bls12_381_g2_bls12_381_fr_nok;
      test_case "mul_bls12_381_fr_ok" `Quick mul_bls12_381_fr_ok;
      test_case "mul_nat_bls12_381_fr_ok" `Quick mul_nat_bls12_381_fr_ok;
      test_case "mul_int_bls12_381_fr_ok" `Quick mul_int_bls12_381_fr_ok;
      test_case "mul_nok" `Quick mul_nok;
    ]

(****************************************************************)

let ediv_nat_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_ediv_nat_int
    ~actual:(type_ediv dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Option (create_typ (Pair (create_typ Int, create_typ Nat))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_ediv_nat_int
    ~actual:(type_ediv dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Option (create_typ (Pair (create_typ Int, create_typ Nat))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let ediv_int_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_ediv_int
    ~actual:(type_ediv dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Option (create_typ (Pair (create_typ Int, create_typ Nat))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let ediv_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_ediv_nat
    ~actual:(type_ediv dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Option (create_typ (Pair (create_typ Nat, create_typ Nat))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let ediv_mutez_nat_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Mutez) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_ediv_mutez_nat
    ~actual:(type_ediv dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Option (create_typ (Pair (create_typ Mutez, create_typ Mutez))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let ediv_mutez_nat_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_ediv dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let ediv_mutez_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Mutez) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_ediv_mutez
    ~actual:(type_ediv dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Option (create_typ (Pair (create_typ Nat, create_typ Mutez))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let ediv_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_ediv dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let ediv_tests =
  Alcotest.
    [
      test_case "ediv_nat_int_ok" `Quick ediv_nat_int_ok;
      test_case "ediv_int_ok" `Quick ediv_int_ok;
      test_case "ediv_nat_ok" `Quick ediv_nat_ok;
      test_case "ediv_mutez_nat_ok" `Quick ediv_mutez_nat_ok;
      test_case "ediv_mutez_nat_nok" `Quick ediv_mutez_nat_nok;
      test_case "ediv_mutez_ok" `Quick ediv_mutez_ok;
      test_case "ediv_nok" `Quick ediv_nok;
    ]

(****************************************************************************)

let lsl_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_lsl ~actual:(type_lsl dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let lsl_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_lsl dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let lsl_tests =
  Alcotest.
    [ test_case "lsl_ok" `Quick lsl_ok; test_case "lsl_nok" `Quick lsl_nok ]

(****************************************************************************)

let lsr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_lsr ~actual:(type_lsr dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let lsr_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_lsr dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let lsr_tests =
  Alcotest.
    [ test_case "lsr_ok" `Quick lsr_ok; test_case "lsr_nok" `Quick lsr_nok ]

(****************************************************************************)

let compare_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_compare
    ~actual:(type_compare dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let compare_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_compare dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let compare_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ Bls12_381_g1) stack;
  Stack.push (create_typ Bls12_381_g1) stack;
  let error =
    try
      let _ = type_compare dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let compare_tests =
  Alcotest.
    [
      test_case "compare_ok" `Quick compare_ok;
      test_case "compare_nok" `Quick compare_nok;
      test_case "compare_nok2" `Quick compare_nok2;
    ]

(****************************************************************************)

let concat_string_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_concat_string
    ~actual:(type_concat dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let concat_list_string_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (List (create_typ String))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_concat_list_string
    ~actual:(type_concat dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let concat_bytes_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Stack.push (create_typ Bytes) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_concat_bytes
    ~actual:(type_concat dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let concat_list_bytes_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (List (create_typ Bytes))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_concat_list_bytes
    ~actual:(type_concat dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let concat_tests =
  Alcotest.
    [
      test_case "concat_string_ok" `Quick concat_string_ok;
      test_case "concat_list_string_ok" `Quick concat_list_string_ok;
      test_case "concat_bytes_ok" `Quick concat_bytes_ok;
      test_case "concat_list_bytes_ok" `Quick concat_list_bytes_ok;
    ]

(****************************************************************************)

let size_set_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Set (create_typ Bytes))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_size_set
    ~actual:(type_size dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let size_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Map (create_typ Nat, create_typ String))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_size_map
    ~actual:(type_size dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let size_list_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (List (create_typ String))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_size_list
    ~actual:(type_size dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let size_string_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_size_string
    ~actual:(type_size dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let size_bytes_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_size_bytes
    ~actual:(type_size dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let size_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_size dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let size_tests =
  Alcotest.
    [
      test_case "size_set_ok" `Quick size_set_ok;
      test_case "size_map_ok" `Quick size_map_ok;
      test_case "size_list_ok" `Quick size_list_ok;
      test_case "size_string_ok" `Quick size_string_ok;
      test_case "size_bytes_ok" `Quick size_bytes_ok;
      test_case "size_nok" `Quick size_nok;
    ]

(****************************************************************************)

let slice_string_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_slice_string
    ~actual:(type_slice dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Option (create_typ String))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let slice_bytes_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_slice_bytes
    ~actual:(type_slice dummy_loc stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Option (create_typ Bytes))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let slice_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_slice dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let slice_tests =
  Alcotest.
    [
      test_case "slice_string_ok" `Quick slice_string_ok;
      test_case "slice_bytes_ok" `Quick slice_bytes_ok;
      test_case "slice_nok" `Quick slice_nok;
    ]

(****************************************************************************)

let pair_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_pair
    ~actual:(type_pair dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Pair (create_typ String, create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let pair_tests = Alcotest.[ test_case "pair_ok" `Quick pair_ok ]

(****************************************************************************)

let car_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Pair (create_typ String, create_typ Int))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_car ~actual:(type_car dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let car_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_car dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let car_tests =
  Alcotest.
    [ test_case "car_ok" `Quick car_ok; test_case "car_nok" `Quick car_nok ]

(****************************************************************************)

let cdr_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Pair (create_typ String, create_typ Int))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_cdr ~actual:(type_cdr dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let cdr_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_cdr dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let cdr_tests =
  Alcotest.
    [ test_case "cdr_ok" `Quick cdr_ok; test_case "cdr_nok" `Quick cdr_nok ]

(****************************************************************************)

let mem_set_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Set (create_typ String))) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mem_set ~actual:(type_mem dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mem_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Map (create_typ String, create_typ Int))) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mem_map ~actual:(type_mem dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mem_big_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Big_map (create_typ String, create_typ Int))) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_mem_big_map
    ~actual:(type_mem dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let mem_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ String) stack;
  let error =
    try
      let _ = type_mem dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let mem_tests =
  Alcotest.
    [
      test_case "mem_set_ok" `Quick mem_set_ok;
      test_case "mem_map_ok" `Quick mem_map_ok;
      test_case "mem_big_map_ok" `Quick mem_big_map_ok;
      test_case "mem_nok" `Quick mem_nok;
    ]

(****************************************************************************)

let update_set_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Set (create_typ String))) stack;
  Stack.push (create_typ Bool) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_update_set
    ~actual:(type_update dummy_loc stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Set (create_typ String))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let update_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Map (create_typ String, create_typ Int))) stack;
  Stack.push (create_typ (Option (create_typ Int))) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_update_map
    ~actual:(type_update dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Map (create_typ String, create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let update_big_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Big_map (create_typ String, create_typ Int))) stack;
  Stack.push (create_typ (Option (create_typ Int))) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_update_map
    ~actual:(type_update dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Big_map (create_typ String, create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let update_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Bool) stack;
  Stack.push (create_typ String) stack;
  let error =
    try
      let _ = type_update dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let update_tests =
  Alcotest.
    [
      test_case "update_set_ok" `Quick update_set_ok;
      test_case "update_map_ok" `Quick update_map_ok;
      test_case "update_big_map_ok" `Quick update_big_map_ok;
      test_case "update_nok" `Quick update_nok;
    ]

(****************************************************************)

let get_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Map (create_typ String, create_typ Int))) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_get_map ~actual:(type_get dummy_loc stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Option (create_typ Int))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_big_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Big_map (create_typ String, create_typ Int))) stack;
  Stack.push (create_typ String) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_get_big_map
    ~actual:(type_get dummy_loc stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Option (create_typ Int))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ String) stack;
  let error =
    try
      let _ = type_get dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let get_tests =
  Alcotest.
    [
      test_case "get_map_ok" `Quick get_map_ok;
      test_case "get_big_map_ok" `Quick get_big_map_ok;
      test_case "get_nok" `Quick get_nok;
    ]

(****************************************************************)

let some_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_some
    ~actual:(type_some dummy_loc stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Option (create_typ Int))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let some_tests = Alcotest.[ test_case "some_ok" `Quick some_ok ]

(****************************************************************)

let none_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_none (create_adt_typ T_int))
    ~actual:(type_none dummy_loc stack (create_adt_typ T_int));
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Option (create_typ Int))))
    (Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let none_tests = Alcotest.[ test_case "none_ok" `Quick none_ok ]

(****************************************************************)

let cons_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (List (create_typ Int))) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_cons
    ~actual:(type_cons dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ (List (create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let cons_nok () =
  let stack = create_stack () in
  Stack.push (create_typ (List (create_typ Nat))) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_cons dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let cons_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_cons dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let cons_tests =
  Alcotest.
    [
      test_case "cons_ok" `Quick cons_ok;
      test_case "cons_nok" `Quick cons_nok;
      test_case "cons_nok2" `Quick cons_nok2;
    ]

(****************************************************************)

let transfer_tokens_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Contract (create_typ Nat))) stack;
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_transfer_tokens
    ~actual:(type_transfer_tokens dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Operation))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let transfer_tokens_nok () =
  let stack = create_stack () in
  Stack.push (create_typ (Contract (create_typ Nat))) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Mutez) stack;
  let error =
    try
      let _ = type_transfer_tokens dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let transfer_tokens_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ (Contract (create_typ Int))) stack;
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_transfer_tokens dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let transfer_tokens_tests =
  Alcotest.
    [
      test_case "transfer_tokens_ok" `Quick transfer_tokens_ok;
      test_case "transfer_tokens_nok" `Quick transfer_tokens_nok;
      test_case "transfer_tokens_nok2" `Quick transfer_tokens_nok2;
    ]

(****************************************************************)

let set_delegate_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Option (create_typ Key_hash))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_set_delegate
    ~actual:(type_set_delegate dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Operation))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let set_delegate_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Key_hash) stack;
  let error =
    try
      let _ = type_set_delegate dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let set_delegate_tests =
  Alcotest.
    [
      test_case "set_delegate_ok" `Quick set_delegate_ok;
      test_case "set_delegate_nok" `Quick set_delegate_nok;
    ]

(****************************************************************)

let balance_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_balance
    ~actual:(type_balance dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Mutez))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let balance_tests = Alcotest.[ test_case "balance_ok" `Quick balance_ok ]

(****************************************************************)

let address_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Contract (create_typ Nat))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_address
    ~actual:(type_address dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Address))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let address_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_address dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let address_tests =
  Alcotest.
    [
      test_case "address_ok" `Quick address_ok;
      test_case "address_nok" `Quick address_nok;
    ]

(****************************************************************)

let source_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_source
    ~actual:(type_source dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Address))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let source_tests = Alcotest.[ test_case "source_ok" `Quick source_ok ]

(****************************************************************)

let sender_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sender
    ~actual:(type_sender dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Address))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sender_tests = Alcotest.[ test_case "sender_ok" `Quick sender_ok ]

(****************************************************************)

let self_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_self
    ~actual:(type_self dummy_loc stack (create_adt_typ T_nat) None);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Contract (create_typ Nat))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let self_tests = Alcotest.[ test_case "self_ok" `Quick self_ok ]

(****************************************************************)

let amount_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_amount
    ~actual:(type_amount dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Mutez))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let amount_tests = Alcotest.[ test_case "amount_ok" `Quick amount_ok ]

(******************************************************************************)

let implicit_account_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Key_hash) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_implicit_account
    ~actual:(type_implicit_account dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Contract (create_typ Unit))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let implicit_account_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_implicit_account dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let implicit_account_tests =
  Alcotest.
    [
      test_case "implicit_account_ok" `Quick implicit_account_ok;
      test_case "implicit_account_nok" `Quick implicit_account_nok;
    ]

(****************************************************************)

let voting_power_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Key_hash) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_voting_power
    ~actual:(type_voting_power dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let voting_power_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_voting_power dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let voting_power_tests =
  Alcotest.
    [
      test_case "voting_power_ok" `Quick voting_power_ok;
      test_case "voting_power_nok" `Quick voting_power_nok;
    ]

(****************************************************************)

let now_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_now ~actual:(type_now dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Timestamp))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let now_tests = Alcotest.[ test_case "now_ok" `Quick now_ok ]

(****************************************************************)

let chain_id_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_chain_id
    ~actual:(type_chain_id dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Chain_id))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let chain_id_tests = Alcotest.[ test_case "chain_id_ok" `Quick chain_id_ok ]

(****************************************************************)

let pack_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (List (create_typ Nat))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_pack
    ~actual:(type_pack dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let pack_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Operation) stack;
  let error =
    try
      let _ = type_pack dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let pack_tests =
  Alcotest.
    [ test_case "pack_ok" `Quick pack_ok; test_case "pack_nok" `Quick pack_nok ]

(****************************************************************)

let hash_key_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Key) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_hash_key
    ~actual:(type_hash_key dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Key_hash))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let hash_key_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_hash_key dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let hash_key_tests =
  Alcotest.
    [
      test_case "hash_key_ok" `Quick hash_key_ok;
      test_case "hash_key_nok" `Quick hash_key_nok;
    ]

(****************************************************************)

let blake2b_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_blake2b
    ~actual:(type_blake2b dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let blake2b_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_blake2b dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let blake2b_tests =
  Alcotest.
    [
      test_case "blake2b_ok" `Quick blake2b_ok;
      test_case "blake2b_nok" `Quick blake2b_nok;
    ]

(****************************************************************)

let sha3_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sha3
    ~actual:(type_sha3 dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sha3_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_sha3 dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let sha3_tests =
  Alcotest.
    [ test_case "sha3_ok" `Quick sha3_ok; test_case "sha3_nok" `Quick sha3_nok ]

(****************************************************************)

let sha256_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sha256
    ~actual:(type_sha256 dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sha256_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_sha256 dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let sha256_tests =
  Alcotest.
    [
      test_case "sha256_ok" `Quick sha256_ok;
      test_case "sha256_nok" `Quick sha256_nok;
    ]

(****************************************************************)

let sha512_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sha512
    ~actual:(type_sha512 dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sha512_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_sha512 dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let sha512_tests =
  Alcotest.
    [
      test_case "sha512_ok" `Quick sha512_ok;
      test_case "sha512_nok" `Quick sha512_nok;
    ]

(****************************************************************)

let keccak_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_keccak
    ~actual:(type_keccak dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let keccak_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_keccak dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let keccak_tests =
  Alcotest.
    [
      test_case "keccak_ok" `Quick keccak_ok;
      test_case "keccak_nok" `Quick keccak_nok;
    ]

(****************************************************************)

let check_signature_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Stack.push (create_typ Signature) stack;
  Stack.push (create_typ Key) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_check_signature
    ~actual:(type_check_signature dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let check_signature_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Stack.push (create_typ Key) stack;
  Stack.push (create_typ Signature) stack;
  let error =
    try
      let _ = type_check_signature dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let check_signature_tests =
  Alcotest.
    [
      test_case "check_signature_ok" `Quick check_signature_ok;
      test_case "check_signature_nok" `Quick check_signature_nok;
    ]

(****************************************************************)

let total_voting_power_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_total_voting_power
    ~actual:(type_total_voting_power dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let total_voting_power_tests =
  Alcotest.[ test_case "total_voting_power_ok" `Quick total_voting_power_ok ]

(****************************************************************)

let pairing_check_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (List
          (create_typ (Pair (create_typ Bls12_381_g1, create_typ Bls12_381_g2)))))
    stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_pairing_check
    ~actual:(type_pairing_check dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bool))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let pairing_check_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_pairing_check dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let pairing_check_tests =
  Alcotest.
    [
      test_case "pairing_check_ok" `Quick pairing_check_ok;
      test_case "pairing_check_nok" `Quick pairing_check_nok;
    ]

(****************************************************************)

let sapling_verify_update_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Sapling_state Z.one)) stack;
  Stack.push (create_typ (Sapling_transaction Z.one)) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_sapling_verify_update
    ~actual:(type_sapling_verify_update dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Option
               (create_typ
                  (Pair (create_typ Int, create_typ (Sapling_state Z.one)))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sapling_verify_update_nok () =
  let stack = create_stack () in
  Stack.push (create_typ (Sapling_transaction Z.one)) stack;
  Stack.push (create_typ (Sapling_state Z.one)) stack;
  let error =
    try
      let _ = type_sapling_verify_update dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let sapling_verify_update_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ (Sapling_state Z.zero)) stack;
  Stack.push (create_typ (Sapling_transaction Z.one)) stack;
  let error =
    try
      let _ = type_sapling_verify_update dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let sapling_verify_update_tests =
  Alcotest.
    [
      test_case "sapling_verify_update_ok" `Quick sapling_verify_update_ok;
      test_case "sapling_verify_update_nok" `Quick sapling_verify_update_nok;
      test_case "sapling_verify_update_nok2" `Quick sapling_verify_update_nok2;
    ]

(****************************************************************)

let sapling_empty_state_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_sapling_empty_state Z.one)
    ~actual:(type_sapling_empty_state dummy_loc stack Z.one);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Sapling_state Z.one)))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let sapling_empty_state_tests =
  Alcotest.[ test_case "sapling_empty_state_ok" `Quick sapling_empty_state_ok ]

(****************************************************************)

let ticket_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_ticket
    ~actual:(type_ticket dummy_loc stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Ticket (create_typ Int))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let ticket_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_ticket dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let ticket_tests =
  Alcotest.
    [
      test_case "ticket_ok" `Quick ticket_ok;
      test_case "ticket_nok" `Quick ticket_nok;
    ]

(****************************************************************)

let read_ticket_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Ticket (create_typ Int))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_read_ticket
    ~actual:(type_read_ticket dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Pair
               ( create_typ Address,
                 create_typ (Pair (create_typ Int, create_typ Nat)) ))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Ticket (create_typ Int))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let read_ticket_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_read_ticket dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let read_ticket_tests =
  Alcotest.
    [
      test_case "read_ticket_ok" `Quick read_ticket_ok;
      test_case "read_ticket_nok" `Quick read_ticket_nok;
    ]

(****************************************************************)

let split_ticket_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Pair (create_typ Nat, create_typ Nat))) stack;
  Stack.push (create_typ (Ticket (create_typ Int))) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_split_ticket
    ~actual:(type_split_ticket dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Option
               (create_typ
                  (Pair
                     ( create_typ (Ticket (create_typ Int)),
                       create_typ (Ticket (create_typ Int)) ))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let split_ticket_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ (Ticket (create_typ Int))) stack;
  let error =
    try
      let _ = type_split_ticket dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let split_ticket_tests =
  Alcotest.
    [
      test_case "split_ticket_ok" `Quick split_ticket_ok;
      test_case "split_ticket_nok" `Quick split_ticket_nok;
    ]

(****************************************************************)

let join_tickets_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ (Ticket (create_typ Int)),
            create_typ (Ticket (create_typ Int)) )))
    stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_join_tickets
    ~actual:(type_join_tickets dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some (create_typ (Option (create_typ (Ticket (create_typ Int))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let join_tickets_nok () =
  let stack = create_stack () in
  Stack.push (create_typ (Ticket (create_typ Int))) stack;
  let error =
    try
      let _ = type_join_tickets dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let join_tickets_nok2 () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ (Ticket (create_typ Int)),
            create_typ (Ticket (create_typ Nat)) )))
    stack;
  let error =
    try
      let _ = type_join_tickets dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let join_tickets_tests =
  Alcotest.
    [
      test_case "join_tickets_ok" `Quick join_tickets_ok;
      test_case "join_tickets_nok" `Quick join_tickets_nok;
      test_case "join_tickets_nok2" `Quick join_tickets_nok2;
    ]

(****************************************************************)

let self_address_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_self_address
    ~actual:(type_self_address dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Address))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let self_address_tests =
  Alcotest.[ test_case "self_address_ok" `Quick self_address_ok ]

(****************************************************************)

let level_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_level
    ~actual:(type_level dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let level_tests = Alcotest.[ test_case "level_ok" `Quick level_ok ]

(****************************************************************)

let open_chest_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Chest) stack;
  Stack.push (create_typ Chest_key) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_open_chest
    ~actual:(type_open_chest dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Or (create_typ Bytes, create_typ Bool))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let open_chest_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Chest_key) stack;
  let error =
    try
      let _ = type_open_chest dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let open_chest_tests =
  Alcotest.
    [
      test_case "open_chest_ok" `Quick open_chest_ok;
      test_case "open_chest_nok" `Quick open_chest_nok;
    ]

(****************************************************************)

let get_and_update_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Map (create_typ Int, create_typ Nat))) stack;
  Stack.push (create_typ (Option (create_typ Nat))) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_get_and_update_map
    ~actual:(type_get_and_update dummy_loc stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Option (create_typ Nat))))
    (Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Map (create_typ Int, create_typ Nat))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_and_update_big_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Big_map (create_typ Int, create_typ Nat))) stack;
  Stack.push (create_typ (Option (create_typ Nat))) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_get_and_update_big_map
    ~actual:(type_get_and_update dummy_loc stack);
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Option (create_typ Nat))))
    (Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Big_map (create_typ Int, create_typ Nat))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_and_update_nok () =
  let stack = create_stack () in
  Stack.push (create_typ (Map (create_typ Int, create_typ Nat))) stack;
  Stack.push (create_typ (Option (create_typ Nat))) stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_get_and_update dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let get_and_update_tests =
  Alcotest.
    [
      test_case "get_and_update_map_ok" `Quick get_and_update_map_ok;
      test_case "get_and_update_big_map_ok" `Quick get_and_update_big_map_ok;
      test_case "get_and_update_nok" `Quick get_and_update_nok;
    ]

(****************************************************************)

let empty_set_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_empty_set (create_adt_typ T_int))
    ~actual:(type_empty_set dummy_loc stack (create_adt_typ T_int));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ (Set (create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let empty_set_tests = Alcotest.[ test_case "empty_set_ok" `Quick empty_set_ok ]

(****************************************************************)

let nil_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_nil (create_adt_typ T_int))
    ~actual:(type_nil dummy_loc stack (create_adt_typ T_int));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ (List (create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let nil_tests = Alcotest.[ test_case "nil_ok" `Quick nil_ok ]

(****************************************************************)

let empty_map_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_empty_map (create_adt_typ T_int, create_adt_typ T_nat))
    ~actual:
      (type_empty_map dummy_loc stack (create_adt_typ T_int)
         (create_adt_typ T_nat));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Map (create_typ Int, create_typ Nat))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let empty_map_tests = Alcotest.[ test_case "empty_map_ok" `Quick empty_map_ok ]

(****************************************************************)

let empty_big_map_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_empty_big_map (create_adt_typ T_int, create_adt_typ T_nat))
    ~actual:
      (type_empty_big_map dummy_loc stack (create_adt_typ T_int)
         (create_adt_typ T_nat));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Big_map (create_typ Int, create_typ Nat))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let empty_big_map_tests =
  Alcotest.[ test_case "empty_big_map_ok" `Quick empty_big_map_ok ]

(**********************************************************************)

let contract_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Address) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_contract (create_adt_typ T_int))
    ~actual:(type_contract dummy_loc stack (create_adt_typ T_int));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some (create_typ (Option (create_typ (Contract (create_typ Int))))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let contract_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_contract dummy_loc stack (create_adt_typ T_address) in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let contract_tests =
  Alcotest.
    [
      test_case "contract_ok" `Quick contract_ok;
      test_case "contract_nok" `Quick contract_nok;
    ]

(**********************************************************************)

let create_contract_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ (Option (create_typ Key_hash))) stack;

  let p =
    {
      Typed_adt.param = create_adt_typ T_nat;
      storage = create_adt_typ T_int;
      code =
        create_inst_typed
          (I_seq
             [
               create_inst_typed I_cdr;
               create_inst_typed (I_nil (create_adt_typ T_operation));
               create_inst_typed I_pair;
             ]);
    }
  in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_create_contract p)
    ~actual:(type_create_contract dummy_loc stack p);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Operation))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Address))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let create_contract_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (Option (create_typ Key_hash))) stack;
  Stack.push (create_typ Mutez) stack;
  let p =
    {
      Typed_adt.param = create_adt_typ T_nat;
      storage = create_adt_typ T_int;
      code =
        create_inst_typed
          (I_seq
             [
               create_inst_typed I_cdr;
               create_inst_typed (I_nil (create_adt_typ T_operation));
               create_inst_typed I_pair;
             ]);
    }
  in
  let error =
    try
      let _ = type_create_contract dummy_loc stack p in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let create_contract_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ (Option (create_typ Key_hash))) stack;
  let p =
    {
      Typed_adt.param = create_adt_typ T_nat;
      storage = create_adt_typ T_string;
      code =
        create_inst_typed
          (I_seq
             [
               create_inst_typed I_cdr;
               create_inst_typed (I_nil (create_adt_typ T_operation));
               create_inst_typed I_pair;
             ]);
    }
  in
  let error =
    try
      let _ = type_create_contract dummy_loc stack p in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let create_contract_tests =
  Alcotest.
    [
      test_case "create_contract_ok" `Quick create_contract_ok;
      test_case "create_contract_nok" `Quick create_contract_nok;
      test_case "create_contract_nok2" `Quick create_contract_nok2;
    ]

(**********************************************************************)

let unpack_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Bytes) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_unpack (create_adt_typ T_int))
    ~actual:(type_unpack dummy_loc stack (create_adt_typ T_int));
  Alcotest.(check (option typ))
    "stack"
    (Some (create_typ (Option (create_typ Int))))
    (Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let unpack_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_unpack dummy_loc stack (create_adt_typ T_bytes) in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let unpack_tests =
  Alcotest.
    [
      test_case "unpack_ok" `Quick unpack_ok;
      test_case "unpack_nok" `Quick unpack_nok;
    ]

(**********************************************************************)

let cast_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_cast (create_adt_typ T_int))
    ~actual:(type_cast dummy_loc stack (create_adt_typ T_int));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let cast_tests = Alcotest.[ test_case "cast_ok" `Quick cast_ok ]

(**********************************************************************)

let create_account_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Bool) stack;
  Stack.push (create_typ (Option (create_typ Key_hash))) stack;
  Stack.push (create_typ Key_hash) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_create_account
    ~actual:(type_create_account dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Operation))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Address))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let create_account_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Mutez) stack;
  Stack.push (create_typ Bool) stack;
  Stack.push (create_typ (Option (create_typ Key_hash))) stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_create_account dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let create_account_tests =
  Alcotest.
    [
      test_case "create_account_ok" `Quick create_account_ok;
      test_case "create_account_nok" `Quick create_account_nok;
    ]

(**********************************************************************)

let exec_ok () =
  let stack = create_stack () in
  Stack.push (create_typ (Lambda (create_typ Int, create_typ Nat))) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_exec
    ~actual:(type_exec dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let exec_nok () =
  let stack = create_stack () in
  Stack.push (create_typ (Lambda (create_typ Int, create_typ Nat))) stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_exec dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let exec_tests =
  Alcotest.
    [ test_case "exec_ok" `Quick exec_ok; test_case "exec_nok" `Quick exec_nok ]

(**********************************************************************)

let apply_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Lambda
          (create_typ (Pair (create_typ Nat, create_typ Int)), create_typ String)))
    stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:I_apply
    ~actual:(type_apply dummy_loc stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Lambda (create_typ Int, create_typ String))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let apply_nok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Lambda
          (create_typ (Pair (create_typ Nat, create_typ Int)), create_typ String)))
    stack;
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_apply dummy_loc stack in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let apply_tests =
  Alcotest.
    [
      test_case "apply_ok" `Quick apply_ok;
      test_case "apply_nok" `Quick apply_nok;
    ]

(**********************************************************************)

let drop_0_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_drop Z.zero)
    ~actual:(type_drop_n dummy_loc stack Z.zero);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let drop_1_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_drop Z.one)
    ~actual:(type_drop_n dummy_loc stack Z.one);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let drop_2_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_drop Z.(one + one))
    ~actual:(type_drop_n dummy_loc stack Z.(one + one));
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let drop_n_tests =
  Alcotest.
    [
      test_case "drop_0_ok" `Quick drop_0_ok;
      test_case "drop_1_ok" `Quick drop_1_ok;
      test_case "drop_2_ok" `Quick drop_2_ok;
    ]

(**********************************************************************)

let push_ok () =
  let stack = create_stack () in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_push (create_data (create_adt_typ T_int) (D_int Z.zero)))
    ~actual:
      (type_push dummy_loc stack
         (create_data (create_adt_typ T_int) (D_int Z.zero)));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let push_tests = Alcotest.[ test_case "push_ok" `Quick push_ok ]

(**********************************************************************)

let left_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_left (create_adt_typ T_nat))
    ~actual:(type_left dummy_loc stack (create_adt_typ T_nat));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Or (create_typ Int, create_typ Nat))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let left_tests = Alcotest.[ test_case "left_ok" `Quick left_ok ]

(**********************************************************************)

let right_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_right (create_adt_typ T_nat))
    ~actual:(type_right dummy_loc stack (create_adt_typ T_nat));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Or (create_typ Nat, create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let right_tests = Alcotest.[ test_case "right_ok" `Quick right_ok ]

(**********************************************************************)

let unpair_2_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ Int,
            create_typ
              (Pair
                 ( create_typ Nat,
                   create_typ
                     (Pair
                        ( create_typ String,
                          create_typ (Pair (create_typ Bool, create_typ Bytes))
                        )) )) )))
    stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_unpair Z.(one + one))
    ~actual:(type_unpair dummy_loc stack Z.(one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Pair
               ( create_typ Nat,
                 create_typ
                   (Pair
                      ( create_typ String,
                        create_typ (Pair (create_typ Bool, create_typ Bytes)) ))
               ))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let unpair_3_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ Int,
            create_typ
              (Pair
                 ( create_typ Nat,
                   create_typ
                     (Pair
                        ( create_typ String,
                          create_typ (Pair (create_typ Bool, create_typ Bytes))
                        )) )) )))
    stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_unpair Z.(one + one + one))
    ~actual:(type_unpair dummy_loc stack Z.(one + one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Pair
               ( create_typ String,
                 create_typ (Pair (create_typ Bool, create_typ Bytes)) ))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let unpair_nok () =
  let stack = create_stack () in
  Stack.push (create_typ (Pair (create_typ Int, create_typ Nat))) stack;
  let error =
    try
      let _ = type_unpair dummy_loc stack Z.(one + one + one) in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let unpair_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  let error =
    try
      let _ = type_unpair dummy_loc stack Z.(one + one) in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let unpair_tests =
  Alcotest.
    [
      test_case "unpair_2_ok" `Quick unpair_2_ok;
      test_case "unpair_3_ok" `Quick unpair_3_ok;
      test_case "unpair_nok" `Quick unpair_nok;
      test_case "unpair_nok2" `Quick unpair_nok2;
    ]

(**********************************************************************)

let dup_1_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_dup Z.one)
    ~actual:(type_dup dummy_loc stack Z.one);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dup_2_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dup Z.(one + one))
    ~actual:(type_dup dummy_loc stack Z.(one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dup_3_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dup Z.(one + one))
    ~actual:(type_dup dummy_loc stack Z.(one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dup_n_tests =
  Alcotest.
    [
      test_case "dup_1_ok" `Quick dup_1_ok;
      test_case "dup_2_ok" `Quick dup_2_ok;
      test_case "dup_3_ok" `Quick dup_3_ok;
    ]

(**********************************************************************)

let dig_0_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_dig Z.zero)
    ~actual:(type_dig dummy_loc stack Z.zero);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dig_1_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_dig Z.one)
    ~actual:(type_dig dummy_loc stack Z.one);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dig_2_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dig Z.(one + one))
    ~actual:(type_dig dummy_loc stack Z.(one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dig_tests =
  Alcotest.
    [
      test_case "dig_0_ok" `Quick dig_0_ok;
      test_case "dig_1_ok" `Quick dig_1_ok;
      test_case "dig_2_ok" `Quick dig_2_ok;
    ]

(**********************************************************************)

let dug_0_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_dug Z.zero)
    ~actual:(type_dug dummy_loc stack Z.zero);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dug_1_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_dug Z.one)
    ~actual:(type_dug dummy_loc stack Z.one);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dug_2_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dug Z.(one + one))
    ~actual:(type_dug dummy_loc stack Z.(one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dug_tests =
  Alcotest.
    [
      test_case "dug_0_ok" `Quick dug_0_ok;
      test_case "dug_1_ok" `Quick dug_1_ok;
      test_case "dug_2_ok" `Quick dug_2_ok;
    ]

(**********************************************************************)

let pair_n_2_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_pair_n Z.(one + one))
    ~actual:(type_pair_n dummy_loc stack Z.(one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Pair (create_typ Nat, create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let pair_n_3_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_pair_n Z.(one + one + one))
    ~actual:(type_pair_n dummy_loc stack Z.(one + one + one));

  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Pair
               ( create_typ Nat,
                 create_typ (Pair (create_typ Int, create_typ String)) ))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let pair_n_tests =
  Alcotest.
    [
      test_case "pair_n_2_ok" `Quick pair_n_2_ok;
      test_case "pair_n_3_ok" `Quick pair_n_3_ok;
    ]

(**********************************************************************)

let get_0_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_get_n Z.zero)
    ~actual:(type_get_n dummy_loc stack Z.zero);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_1_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          (create_typ Nat, create_typ (Pair (create_typ Int, create_typ String)))))
    stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_get_n Z.one)
    ~actual:(type_get_n dummy_loc stack Z.one);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_2_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          (create_typ Nat, create_typ (Pair (create_typ Int, create_typ String)))))
    stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_get_n Z.(one + one))
    ~actual:(type_get_n dummy_loc stack Z.(one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Pair (create_typ Int, create_typ String))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_3_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          (create_typ Nat, create_typ (Pair (create_typ Int, create_typ String)))))
    stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_get_n Z.(one + one + one))
    ~actual:(type_get_n dummy_loc stack Z.(one + one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_4_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          (create_typ Nat, create_typ (Pair (create_typ Int, create_typ String)))))
    stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_get_n Z.(one + one + one + one))
    ~actual:(type_get_n dummy_loc stack Z.(one + one + one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let get_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_get_n dummy_loc stack Z.one in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let get_n_tests =
  Alcotest.
    [
      test_case "get_0_ok" `Quick get_0_ok;
      test_case "get_1_ok" `Quick get_1_ok;
      test_case "get_2_ok" `Quick get_2_ok;
      test_case "get_3_ok" `Quick get_3_ok;
      test_case "get_4_ok" `Quick get_4_ok;
      test_case "get_nok" `Quick get_nok;
    ]

(**********************************************************************)

let update_0_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_update_n Z.zero)
    ~actual:(type_update_n dummy_loc stack Z.zero);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);

  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let update_1_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ Bool,
            create_typ (Pair (create_typ Int, create_typ String)) )))
    stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:(I_update_n Z.one)
    ~actual:(type_update_n dummy_loc stack Z.one);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Pair
               ( create_typ Nat,
                 create_typ (Pair (create_typ Int, create_typ String)) ))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let update_2_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ Bool,
            create_typ (Pair (create_typ Int, create_typ String)) )))
    stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_update_n Z.(one + one))
    ~actual:(type_update_n dummy_loc stack Z.(one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Pair (create_typ Bool, create_typ Nat))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let update_3_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ Bool,
            create_typ (Pair (create_typ Int, create_typ String)) )))
    stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_update_n Z.(one + one + one))
    ~actual:(type_update_n dummy_loc stack Z.(one + one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Pair
               ( create_typ Bool,
                 create_typ (Pair (create_typ Nat, create_typ String)) ))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let update_4_ok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ Bool,
            create_typ (Pair (create_typ Int, create_typ String)) )))
    stack;
  Stack.push (create_typ Nat) stack;
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_update_n Z.(one + one + one + one))
    ~actual:(type_update_n dummy_loc stack Z.(one + one + one + one));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:
      (Some
         (create_typ
            (Pair
               ( create_typ Bool,
                 create_typ (Pair (create_typ Int, create_typ Nat)) ))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let update_nok () =
  let stack = create_stack () in
  Stack.push
    (create_typ
       (Pair
          ( create_typ Bool,
            create_typ (Pair (create_typ Int, create_typ String)) )))
    stack;
  Stack.push (create_typ Nat) stack;
  let error =
    try
      let _ = type_update_n dummy_loc stack Z.(one + one + one + one + one) in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let update_n_tests =
  Alcotest.
    [
      test_case "update_0_ok" `Quick update_0_ok;
      test_case "update_1_ok" `Quick update_1_ok;
      test_case "update_2_ok" `Quick update_2_ok;
      test_case "update_3_ok" `Quick update_3_ok;
      test_case "update_4_ok" `Quick update_4_ok;
      test_case "update_nok" `Quick update_nok;
    ]

(**********************************************************************)

let seq_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  let i =
    create_inst
      (I_seq [ create_inst I_add; create_inst I_mul; create_inst I_slice ])
  in
  let i_typed =
    Typed_adt.I_seq
      [
        create_inst_typed I_add_nat;
        create_inst_typed I_mul_nat;
        create_inst_typed I_slice_string;
      ]
  in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:i_typed
    ~actual:(fst (snd (type_inst (create_adt_typ T_unit) stack i)).value);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ (Option (create_typ String))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let seq_tests = Alcotest.[ test_case "seq_ok" `Quick seq_ok ]

(****************************************************************************)

let if_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Bool) stack;
  let i_t = create_inst Adt.I_add in
  let i_f =
    create_inst (Adt.I_seq [ create_inst Adt.I_swap; create_inst Adt.I_drop ])
  in

  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:
      (I_if
         ( create_inst_typed I_add_nat_int,
           create_inst_typed
             (I_seq
                [ create_inst_typed I_swap; create_inst_typed (I_drop Z.one) ])
         ))
    ~actual:(snd (type_if dummy_loc (create_adt_typ T_unit) stack i_t i_f));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let if_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  let i_t = create_inst Adt.I_add in
  let i_f =
    create_inst (Adt.I_seq [ create_inst Adt.I_swap; create_inst Adt.I_drop ])
  in
  let error =
    try
      let _ = type_if dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Bool) stack;
  let i_t = create_inst Adt.I_add in
  let i_f = create_inst Adt.I_swap in
  let error =
    try
      let _ = type_if dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_nok3 () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Bool) stack;
  let i_t = create_inst Adt.I_add in
  let i_f = create_inst Adt.I_drop in
  let error =
    try
      let _ = type_if dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_tests =
  Alcotest.
    [
      test_case "if_ok" `Quick if_ok;
      test_case "if_nok" `Quick if_nok;
      test_case "if_nok2" `Quick if_nok2;
      test_case "if_nok3" `Quick if_nok3;
    ]

(****************************************************************************)

let if_none_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ (Option (create_typ Nat))) stack;
  let i_t =
    create_inst (Adt.I_seq [ create_inst Adt.I_drop; create_inst Adt.I_some ])
  in
  let i_f = create_inst Adt.I_slice in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:
      (I_if_none
         ( create_inst_typed
             (I_seq
                [ create_inst_typed (I_drop Z.one); create_inst_typed I_some ]),
           create_inst_typed I_slice_string ))
    ~actual:(snd (type_if_none dummy_loc (create_adt_typ T_unit) stack i_t i_f));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Option (create_typ String))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let if_none_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Nat) stack;
  let i_t =
    create_inst (Adt.I_seq [ create_inst Adt.I_drop; create_inst Adt.I_some ])
  in
  let i_f = create_inst Adt.I_slice in
  let error =
    try
      let _ = type_if_none dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_none_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ (Option (create_typ Nat))) stack;
  let i_t = create_inst Adt.I_drop in
  let i_f = create_inst Adt.I_slice in
  let error =
    try
      let _ = type_if_none dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_none_tests =
  Alcotest.
    [
      test_case "if_none_ok" `Quick if_none_ok;
      test_case "if_none_nok" `Quick if_none_nok;
      test_case "if_none_nok2" `Quick if_none_nok2;
    ]

(****************************************************************************)

let if_left_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push
    (create_typ (Or (create_typ (List (create_typ String)), create_typ String)))
    stack;
  let i_t =
    create_inst
      (Adt.I_seq [ create_inst Adt.I_concat; create_inst Adt.I_concat ])
  in
  let i_f = create_inst Adt.I_concat in
  Alcotest.(check' inst_t_typed)
    ~msg:"stack value"
    ~expected:
      (I_if_left
         ( create_inst_typed
             (I_seq
                [
                  create_inst_typed I_concat_list_string;
                  create_inst_typed I_concat_string;
                ]),
           create_inst_typed I_concat_string ))
    ~actual:(snd (type_if_left dummy_loc (create_adt_typ T_unit) stack i_t i_f));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let if_left_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ (List (create_typ String))) stack;
  let i_t =
    create_inst
      (Adt.I_seq [ create_inst Adt.I_concat; create_inst Adt.I_concat ])
  in
  let i_f = create_inst Adt.I_concat in
  let error =
    try
      let _ = type_if_left dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_left_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push
    (create_typ (Or (create_typ (List (create_typ String)), create_typ String)))
    stack;
  let i_t = create_inst Adt.I_concat in
  let i_f = create_inst Adt.I_concat in
  let error =
    try
      let _ = type_if_left dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_left_tests =
  Alcotest.
    [
      test_case "if_left_ok" `Quick if_left_ok;
      test_case "if_left_nok" `Quick if_left_nok;
      test_case "if_left_nok2" `Quick if_left_nok2;
    ]

(****************************************************************************)

let if_cons_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ (List (create_typ String))) stack;
  let i_t =
    create_inst
      (Adt.I_seq
         [
           create_inst Adt.I_swap;
           create_inst Adt.I_concat;
           create_inst Adt.I_concat;
           create_inst Adt.I_concat;
         ])
  in
  let i_f = create_inst Adt.I_noop in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:
      (I_if_cons
         ( create_inst_typed
             (I_seq
                [
                  create_inst_typed I_swap;
                  create_inst_typed I_concat_list_string;
                  create_inst_typed I_concat_string;
                  create_inst_typed I_concat_string;
                ]),
           create_inst_typed I_noop ))
    ~actual:(snd (type_if_cons dummy_loc (create_adt_typ T_unit) stack i_t i_f));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let if_cons_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ String) stack;
  let i_t =
    create_inst
      (Adt.I_seq
         [
           create_inst Adt.I_swap;
           create_inst Adt.I_concat;
           create_inst Adt.I_concat;
           create_inst Adt.I_concat;
         ])
  in
  let i_f = create_inst Adt.I_noop in
  let error =
    try
      let _ = type_if_cons dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_cons_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ (List (create_typ String))) stack;
  let i_t =
    create_inst
      (Adt.I_seq
         [
           create_inst Adt.I_swap;
           create_inst Adt.I_concat;
           create_inst Adt.I_concat;
         ])
  in
  let i_f = create_inst Adt.I_noop in
  let error =
    try
      let _ = type_if_cons dummy_loc (create_adt_typ T_unit) stack i_t i_f in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let if_cons_tests =
  Alcotest.
    [
      test_case "if_cons_ok" `Quick if_cons_ok;
      test_case "if_cons_nok" `Quick if_cons_nok;
      test_case "if_cons_nok2" `Quick if_cons_nok2;
    ]

(****************************************************************************)

let loop_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Bool) stack;
  let i =
    create_inst
      (Adt.I_seq
         [
           create_inst (Adt.I_dup Z.one);
           create_inst Adt.I_level;
           create_inst Adt.I_compare;
           create_inst Adt.I_eq;
         ])
  in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:
      (I_loop
         (create_inst_typed
            (I_seq
               [
                 create_inst_typed (I_dup Z.one);
                 create_inst_typed I_level;
                 create_inst_typed I_compare;
                 create_inst_typed I_eq;
               ])))
    ~actual:(snd (type_loop dummy_loc (create_adt_typ T_unit) stack i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Nat))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let loop_nok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ String) stack;
  let i =
    create_inst (Adt.I_seq [ create_inst Adt.I_level; create_inst Adt.I_add ])
  in
  let error =
    try
      let _ = type_loop dummy_loc (create_adt_typ T_unit) stack i in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let loop_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Bool) stack;
  let i = create_inst Adt.I_level in
  let error =
    try
      let _ = type_loop dummy_loc (create_adt_typ T_unit) stack i in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let loop_tests =
  Alcotest.
    [
      test_case "loop_ok" `Quick loop_ok;
      test_case "loop_nok" `Quick loop_nok;
      test_case "loop_nok2" `Quick loop_nok2;
    ]

(****************************************************************************)

let loop_left_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ (Or (create_typ Nat, create_typ Timestamp))) stack;
  let i =
    create_inst
      (Adt.I_seq
         [
           create_inst Adt.I_level;
           create_inst Adt.I_compare;
           create_inst Adt.I_eq;
           create_inst
             (Adt.I_if
                ( create_inst
                    (Adt.I_seq
                       [
                         create_inst Adt.I_level;
                         create_inst (Adt.I_left (create_adt_typ T_timestamp));
                       ]),
                  create_inst
                    (Adt.I_seq
                       [
                         create_inst Adt.I_now;
                         create_inst (Adt.I_right (create_adt_typ T_nat));
                       ]) ));
         ])
  in
  let i_typed =
    Typed_adt.I_loop_left
      (create_inst_typed
         (I_seq
            [
              create_inst_typed I_level;
              create_inst_typed I_compare;
              create_inst_typed I_eq;
              create_inst_typed
                (I_if
                   ( create_inst_typed
                       (I_seq
                          [
                            create_inst_typed I_level;
                            create_inst_typed
                              (I_left (create_adt_typ T_timestamp));
                          ]),
                     create_inst_typed
                       (I_seq
                          [
                            create_inst_typed I_now;
                            create_inst_typed (I_right (create_adt_typ T_nat));
                          ]) ));
            ]))
  in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction" ~expected:i_typed
    ~actual:(snd (type_loop_left dummy_loc (create_adt_typ T_unit) stack i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Timestamp))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let loop_left_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Nat) stack;
  let i =
    create_inst
      (Adt.I_seq
         [
           create_inst Adt.I_level;
           create_inst Adt.I_compare;
           create_inst Adt.I_eq;
           create_inst
             (Adt.I_if
                ( create_inst
                    (Adt.I_seq
                       [
                         create_inst Adt.I_level;
                         create_inst (Adt.I_left (create_adt_typ T_timestamp));
                       ]),
                  create_inst
                    (Adt.I_seq
                       [
                         create_inst Adt.I_now;
                         create_inst (Adt.I_right (create_adt_typ T_nat));
                       ]) ));
         ])
  in
  let error =
    try
      let _ = type_loop_left dummy_loc (create_adt_typ T_unit) stack i in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let loop_left_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ (Or (create_typ Nat, create_typ Timestamp))) stack;
  let i =
    create_inst
      (Adt.I_seq
         [
           create_inst Adt.I_level;
           create_inst Adt.I_compare;
           create_inst Adt.I_eq;
           create_inst
             (Adt.I_if
                ( create_inst
                    (Adt.I_seq
                       [
                         create_inst Adt.I_level;
                         create_inst (Adt.I_left (create_adt_typ T_string));
                       ]),
                  create_inst
                    (Adt.I_seq
                       [
                         create_inst Adt.I_now;
                         create_inst (Adt.I_right (create_adt_typ T_nat));
                       ]) ));
         ])
  in
  let error =
    try
      let _ = type_loop_left dummy_loc (create_adt_typ T_unit) stack i in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let loop_left_tests =
  Alcotest.
    [
      test_case "loop_left_ok" `Quick loop_left_ok;
      test_case "loop_left_nok" `Quick loop_left_nok;
      test_case "loop_left_nok2" `Quick loop_left_nok2;
    ]

(****************************************************************************)

let iter_list_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (List (create_typ Nat))) stack;
  let i = create_inst Adt.I_add in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_iter_list (create_inst_typed I_add_nat_int))
    ~actual:(snd (type_iter dummy_loc (create_adt_typ T_unit) stack i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let iter_set_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (Set (create_typ Nat))) stack;
  let i = create_inst Adt.I_add in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_iter_set (create_inst_typed I_add_nat_int))
    ~actual:(snd (type_iter dummy_loc (create_adt_typ T_unit) stack i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let iter_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (Map (create_typ Nat, create_typ Int))) stack;
  let i =
    create_inst
      (Adt.I_seq
         [
           create_inst Adt.I_unpair;
           create_inst Adt.I_add;
           create_inst Adt.I_add;
         ])
  in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:
      (I_iter_map
         (create_inst_typed
            (I_seq
               [
                 create_inst_typed (I_unpair Z.(one + one));
                 create_inst_typed I_add_nat_int;
                 create_inst_typed I_add_int;
               ])))
    ~actual:(snd (type_iter dummy_loc (create_adt_typ T_unit) stack i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let iter_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (List (create_typ Nat))) stack;
  let i =
    create_inst (Adt.I_seq [ create_inst Adt.I_add; create_inst Adt.I_drop ])
  in
  let error =
    try
      let _ = type_iter dummy_loc (create_adt_typ T_unit) stack i in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let iter_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ Nat) stack;
  let i =
    create_inst (Adt.I_seq [ create_inst Adt.I_add; create_inst Adt.I_drop ])
  in
  let error =
    try
      let _ = type_iter dummy_loc (create_adt_typ T_unit) stack i in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let iter_tests =
  Alcotest.
    [
      test_case "iter_list_ok" `Quick iter_list_ok;
      test_case "iter_set_ok" `Quick iter_set_ok;
      test_case "iter_map_ok" `Quick iter_map_ok;
      test_case "iter_nok" `Quick iter_nok;
      test_case "iter_nok2" `Quick iter_nok2;
    ]

(****************************************************************************)

let map_list_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (List (create_typ Nat))) stack;
  let i =
    create_inst (Adt.I_seq [ create_inst Adt.I_drop; create_inst Adt.I_now ])
  in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:
      (I_map_list
         (create_inst_typed
            (I_seq [ create_inst_typed (I_drop Z.one); create_inst_typed I_now ])))
    ~actual:(snd (type_map dummy_loc (create_adt_typ T_unit) stack i));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (List (create_typ Timestamp))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let map_map_ok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (Map (create_typ Nat, create_typ Bool))) stack;
  let i =
    create_inst (Adt.I_seq [ create_inst Adt.I_drop; create_inst Adt.I_now ])
  in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:
      (I_map_map
         (create_inst_typed
            (I_seq [ create_inst_typed (I_drop Z.one); create_inst_typed I_now ])))
    ~actual:(snd (type_map dummy_loc (create_adt_typ T_unit) stack i));
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Map (create_typ Nat, create_typ Timestamp))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let map_nok () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (Set (create_typ Nat))) stack;
  let i =
    create_inst (Adt.I_seq [ create_inst Adt.I_drop; create_inst Adt.I_now ])
  in
  let error =
    try
      let _ = type_map dummy_loc (create_adt_typ T_unit) stack i in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let map_nok2 () =
  let stack = create_stack () in
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ (List (create_typ Nat))) stack;
  let i = create_inst Adt.I_drop in
  let error =
    try
      let _ = type_map dummy_loc (create_adt_typ T_unit) stack i in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let map_tests =
  Alcotest.
    [
      test_case "map_list_ok" `Quick map_list_ok;
      test_case "map_map_ok" `Quick map_map_ok;
      test_case "map_nok" `Quick map_nok;
      test_case "map_nok2" `Quick map_nok2;
    ]

(****************************************************************************)

let lambda_ok () =
  let stack = create_stack () in
  let i =
    create_inst
      (Adt.I_seq [ create_inst Adt.I_level; create_inst Adt.I_compare ])
  in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:
      (I_lambda
         ( create_adt_typ T_nat,
           create_adt_typ T_int,
           create_inst_typed
             (I_seq [ create_inst_typed I_level; create_inst_typed I_compare ])
         ))
    ~actual:
      (type_lambda dummy_loc stack (create_adt_typ T_nat) (create_adt_typ T_int)
         i);
  Alcotest.(check' (option typ))
    ~msg:"stack value"
    ~expected:(Some (create_typ (Lambda (create_typ Nat, create_typ Int))))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let lambda_nok () =
  let stack = create_stack () in
  let i = create_inst Adt.I_level in
  let error =
    try
      let _ =
        type_lambda dummy_loc stack (create_adt_typ T_nat)
          (create_adt_typ T_int) i
      in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let lambda_nok2 () =
  let stack = create_stack () in
  let i =
    create_inst
      (Adt.I_seq [ create_inst Adt.I_level; create_inst Adt.I_compare ])
  in
  let error =
    try
      let _ =
        type_lambda dummy_loc stack (create_adt_typ T_nat)
          (create_adt_typ T_string) i
      in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let lambda_nok3 () =
  let stack = create_stack () in
  let i =
    create_inst
      (Adt.I_seq [ create_inst Adt.I_level; create_inst Adt.I_compare ])
  in
  let error =
    try
      let _ =
        type_lambda dummy_loc stack (create_adt_typ T_string)
          (create_adt_typ T_int) i
      in
      false
    with Type_error _ -> true
  in
  Alcotest.(check' bool) ~msg:"type error" ~expected:true ~actual:error

let lambda_tests =
  Alcotest.
    [
      test_case "lambda_ok" `Quick lambda_ok;
      test_case "lambda_nok" `Quick lambda_nok;
      test_case "lambda_nok2" `Quick lambda_nok2;
      test_case "lambda_nok3" `Quick lambda_nok3;
    ]

(****************************************************************************)

let dip_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ String) stack;
  let i = create_inst Adt.I_add in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dip (create_inst_typed I_add_nat_int))
    ~actual:(snd (type_dip dummy_loc (create_adt_typ T_unit) stack i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dip_tests = Alcotest.[ test_case "dip_ok" `Quick dip_ok ]

(****************************************************************************)

let dip_0_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  let i = create_inst Adt.I_add in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dip_n (Z.zero, create_inst_typed I_add_nat_int))
    ~actual:(snd (type_dip_n dummy_loc (create_adt_typ T_unit) stack Z.zero i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dip_1_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ String) stack;
  let i = create_inst Adt.I_add in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dip_n (Z.one, create_inst_typed I_add_nat_int))
    ~actual:(snd (type_dip_n dummy_loc (create_adt_typ T_unit) stack Z.one i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dip_2_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ String) stack;
  let i = create_inst Adt.I_int in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dip_n (Z.(one + one), create_inst_typed I_int_nat))
    ~actual:
      (snd (type_dip_n dummy_loc (create_adt_typ T_unit) stack Z.(one + one) i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dip_3_ok () =
  let stack = create_stack () in
  Stack.push (create_typ Nat) stack;
  Stack.push (create_typ Int) stack;
  Stack.push (create_typ String) stack;
  Stack.push (create_typ Bytes) stack;
  let i = create_inst I_int in
  Alcotest.(check' inst_t_typed)
    ~msg:"instruction"
    ~expected:(I_dip_n (Z.(one + one + one), create_inst_typed I_int_nat))
    ~actual:
      (snd
         (type_dip_n dummy_loc (create_adt_typ T_unit) stack
            Z.(one + one + one)
            i));
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Bytes))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ String))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' (option typ))
    ~msg:"stack"
    ~expected:(Some (create_typ Int))
    ~actual:(Stack.pop_opt stack);
  Alcotest.(check' bool)
    ~msg:"empty stack" ~expected:true ~actual:(Stack.is_empty stack)

let dip_n_tests =
  Alcotest.
    [
      test_case "dip_0_ok" `Quick dip_0_ok;
      test_case "dip_1_ok" `Quick dip_1_ok;
      test_case "dip_2_ok" `Quick dip_2_ok;
      test_case "dip_3_ok" `Quick dip_3_ok;
    ]

(****************************************************************************)

let () =
  let open Alcotest in
  run "typing"
    [
      ("abs", abs_tests);
      ("drop", drop_tests);
      ("dup", dup_tests);
      ("swap", swap_tests);
      ("unit", unit_tests);
      ("eq", eq_tests);
      ("neq", neq_tests);
      ("lt", lt_tests);
      ("le", le_tests);
      ("gt", gt_tests);
      ("ge", ge_tests);
      ("add", add_tests);
      ("sub", sub_tests);
      ("mul", mul_tests);
      ("ediv", ediv_tests);
      ("neg", neg_tests);
      ("not", not_tests);
      ("and", and_tests);
      ("or", or_tests);
      ("xor", xor_tests);
      ("lsl", lsl_tests);
      ("lsr", lsr_tests);
      ("compare", compare_tests);
      ("concat", concat_tests);
      ("size", size_tests);
      ("slice", slice_tests);
      ("pair", pair_tests);
      ("car", car_tests);
      ("cdr", cdr_tests);
      ("mem", mem_tests);
      ("update", update_tests);
      ("get", get_tests);
      ("some", some_tests);
      ("none", none_tests);
      ("cons", cons_tests);
      ("transfer_tokens", transfer_tokens_tests);
      ("set_delegate", set_delegate_tests);
      ("balance", balance_tests);
      ("address", address_tests);
      ("source", source_tests);
      ("sender", sender_tests);
      ("self", self_tests);
      ("implicit_account", implicit_account_tests);
      ("voting_power", voting_power_tests);
      ("now", now_tests);
      ("chain_id", chain_id_tests);
      ("pack", pack_tests);
      ("hash_key", hash_key_tests);
      ("blake2b", blake2b_tests);
      ("sha3", sha3_tests);
      ("sha256", sha256_tests);
      ("sha512", sha512_tests);
      ("keccak", keccak_tests);
      ("check_signature", check_signature_tests);
      ("total_voting_power", total_voting_power_tests);
      ("pairing_check", pairing_check_tests);
      ("sapling_verify_update", sapling_verify_update_tests);
      ("sapling_empty_state", sapling_empty_state_tests);
      ("ticket", ticket_tests);
      ("read_ticket", read_ticket_tests);
      ("split_ticket", split_ticket_tests);
      ("join_tickets", join_tickets_tests);
      ("self_address", self_address_tests);
      ("level", level_tests);
      ("open_chest", open_chest_tests);
      ("get_and_update", get_and_update_tests);
      ("empty_set", empty_set_tests);
      ("empty_map", empty_map_tests);
      ("empty_big_map", empty_big_map_tests);
      ("nil", nil_tests);
      ("contract", contract_tests);
      ("create_contract", create_contract_tests);
      ("unpack", unpack_tests);
      ("cast", cast_tests);
      ("create_account", create_account_tests);
      ("exec", exec_tests);
      ("apply", apply_tests);
      ("push", push_tests);
      ("left", left_tests);
      ("right", right_tests);
      ("unpair", unpair_tests);
      ("dig", dig_tests);
      ("dug", dug_tests);
      ("pair_n", pair_n_tests);
      ("seq", seq_tests);
      ("dip_n_tests", dip_n_tests);
      ("if", if_tests);
      ("if_left", if_left_tests);
      ("if_none", if_none_tests);
      ("if_cons", if_cons_tests);
      ("loop", loop_tests);
      ("loop_left", loop_left_tests);
      ("iter", iter_tests);
      ("map", map_tests);
      ("lambda", lambda_tests);
      ("dip", dip_tests);
      ("dip_n", dip_n_tests);
      ("get_n", get_n_tests);
      ("update_n", update_n_tests);
      ("dup_n", dup_n_tests);
      ("drop_n", drop_n_tests);
    ]
