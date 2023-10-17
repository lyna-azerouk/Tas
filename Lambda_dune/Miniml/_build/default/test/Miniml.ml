

(**let test_alpha_conversion () =
  let ex_id : pterm = Miniml.Typeur.Abs ("x", Typeur.Var "x") in
  Alcotest.(check int) "alpha_conv test" 4 (alpha_conv ex_id)

let () =
  let open Alcotest in
  run "Test Suite" [
    "alpha_conv", [
      test_case "test_alpha_conversion" `Quick test_alpha_conversion;
    ];
  ]
  **)