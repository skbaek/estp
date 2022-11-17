fof(f92, plain, $false, inference(avatar_sat_refutation, [], [f50, f79, f86, f90])).
fof(f90, plain, spl2_3, inference(avatar_contradiction_clause, [], [f89])).
fof(f89, plain, ($false | spl2_3), inference(subsumption_resolution, [], [f88, f33])).
fof(f33, plain, ! [X0, X1] : (mult(X1, mult(op_c, X0)) = mult(mult(X1, op_c), X0)), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ! [B, A] : (mult(A, mult(op_c, B)) = mult(mult(A, op_c), B)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP702+1.p', f10)).
fof(f88, plain, (~ (mult(sK0, mult(op_c, sK1)) = mult(mult(sK0, op_c), sK1)) | spl2_3), inference(forward_demodulation, [], [f49, f75])).
fof(f75, plain, (op_c = op_d), inference(forward_demodulation, [], [f68, f51])).
fof(f51, plain, ! [X2] : (ld(unit, X2) = X2), inference(superposition, [], [f24, f29])).
fof(f29, plain, ! [X0] : (mult(unit, X0) = X0), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (mult(unit, X0) = X0), inference(rectify, [], [f6])).
fof(f6, plain, ! [X1] : (mult(unit, X1) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP702+1.p', f06)).
fof(f24, plain, ! [X0, X1] : (mult(X1, ld(X1, X0)) = X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (mult(X1, ld(X1, X0)) = X0), file('/home/ubuntu/library/tptp/Problems/GRP/GRP702+1.p', f01)).
fof(f68, plain, (op_d = ld(unit, op_c)), inference(superposition, [], [f34, f28])).
fof(f28, plain, ! [X0] : (mult(X0, unit) = X0), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (mult(X0, unit) = X0), inference(rectify, [], [f5])).
fof(f5, plain, ! [X1] : (mult(X1, unit) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP702+1.p', f05)).
fof(f34, plain, ! [X0] : (op_d = ld(X0, mult(op_c, X0))), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ! [X0] : (op_d = ld(X0, mult(op_c, X0))), inference(rectify, [], [f11])).
fof(f11, plain, ! [A] : (op_d = ld(A, mult(op_c, A))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP702+1.p', f11)).
fof(f49, plain, (~ (mult(sK0, mult(op_d, sK1)) = mult(mult(sK0, op_d), sK1)) | spl2_3), inference(avatar_component_clause, [], [f47])).
fof(f47, plain, (spl2_3 <=> (mult(sK0, mult(op_d, sK1)) = mult(mult(sK0, op_d), sK1))), introduced(avatar_definition, [new_symbols(naming, [spl2_3])])).
fof(f86, plain, spl2_2, inference(avatar_contradiction_clause, [], [f85])).
fof(f85, plain, ($false | spl2_2), inference(subsumption_resolution, [], [f84, f32])).
fof(f32, plain, ! [X0, X1] : (mult(X1, mult(X0, op_c)) = mult(mult(X1, X0), op_c)), inference(cnf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : (mult(X1, mult(X0, op_c)) = mult(mult(X1, X0), op_c)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP702+1.p', f09)).
fof(f84, plain, (~ (mult(sK0, mult(sK1, op_c)) = mult(mult(sK0, sK1), op_c)) | spl2_2), inference(forward_demodulation, [], [f45, f75])).
fof(f45, plain, (~ (mult(sK0, mult(sK1, op_d)) = mult(mult(sK0, sK1), op_d)) | spl2_2), inference(avatar_component_clause, [], [f43])).
fof(f43, plain, (spl2_2 <=> (mult(sK0, mult(sK1, op_d)) = mult(mult(sK0, sK1), op_d))), introduced(avatar_definition, [new_symbols(naming, [spl2_2])])).
fof(f79, plain, spl2_1, inference(avatar_contradiction_clause, [], [f78])).
fof(f78, plain, ($false | spl2_1), inference(subsumption_resolution, [], [f76, f31])).
fof(f31, plain, ! [X0, X1] : (mult(op_c, mult(X1, X0)) = mult(mult(op_c, X1), X0)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (mult(op_c, mult(X1, X0)) = mult(mult(op_c, X1), X0)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP702+1.p', f08)).
fof(f76, plain, (~ (mult(op_c, mult(sK0, sK1)) = mult(mult(op_c, sK0), sK1)) | spl2_1), inference(backward_demodulation, [], [f41, f75])).
fof(f41, plain, (~ (mult(op_d, mult(sK0, sK1)) = mult(mult(op_d, sK0), sK1)) | spl2_1), inference(avatar_component_clause, [], [f39])).
fof(f39, plain, (spl2_1 <=> (mult(op_d, mult(sK0, sK1)) = mult(mult(op_d, sK0), sK1))), introduced(avatar_definition, [new_symbols(naming, [spl2_1])])).
fof(f50, plain, (~ spl2_1 | ~ spl2_2 | ~ spl2_3), inference(avatar_split_clause, [], [f37, f47, f43, f39])).
fof(f37, plain, (~ (mult(sK0, mult(op_d, sK1)) = mult(mult(sK0, op_d), sK1)) | ~ (mult(sK0, mult(sK1, op_d)) = mult(mult(sK0, sK1), op_d)) | ~ (mult(op_d, mult(sK0, sK1)) = mult(mult(op_d, sK0), sK1))), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (~ (mult(sK0, mult(op_d, sK1)) = mult(mult(sK0, op_d), sK1)) | ~ (mult(sK0, mult(sK1, op_d)) = mult(mult(sK0, sK1), op_d)) | ~ (mult(op_d, mult(sK0, sK1)) = mult(mult(op_d, sK0), sK1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK0, sK1])], [f21, f22])).
fof(f22, plain, (? [X0, X1] : (~ (mult(X0, mult(op_d, X1)) = mult(mult(X0, op_d), X1)) | ~ (mult(X0, mult(X1, op_d)) = mult(mult(X0, X1), op_d)) | ~ (mult(op_d, mult(X0, X1)) = mult(mult(op_d, X0), X1))) => (~ (mult(sK0, mult(op_d, sK1)) = mult(mult(sK0, op_d), sK1)) | ~ (mult(sK0, mult(sK1, op_d)) = mult(mult(sK0, sK1), op_d)) | ~ (mult(op_d, mult(sK0, sK1)) = mult(mult(op_d, sK0), sK1)))), introduced(choice_axiom, [])).
fof(f21, plain, ? [X0, X1] : (~ (mult(X0, mult(op_d, X1)) = mult(mult(X0, op_d), X1)) | ~ (mult(X0, mult(X1, op_d)) = mult(mult(X0, X1), op_d)) | ~ (mult(op_d, mult(X0, X1)) = mult(mult(op_d, X0), X1))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ! [X0, X1] : ((mult(X0, mult(op_d, X1)) = mult(mult(X0, op_d), X1)) & (mult(X0, mult(X1, op_d)) = mult(mult(X0, X1), op_d)) & (mult(op_d, mult(X0, X1)) = mult(mult(op_d, X0), X1))), inference(rectify, [], [f15])).
fof(f15, plain, ~ ! [X3, X4] : ((mult(X3, mult(op_d, X4)) = mult(mult(X3, op_d), X4)) & (mult(X3, mult(X4, op_d)) = mult(mult(X3, X4), op_d)) & (mult(op_d, mult(X3, X4)) = mult(mult(op_d, X3), X4))), inference(negated_conjecture, [], [f14])).
fof(f14, plain, ~ ! [X3, X4] : ((mult(X3, mult(op_d, X4)) = mult(mult(X3, op_d), X4)) & (mult(X3, mult(X4, op_d)) = mult(mult(X3, X4), op_d)) & (mult(op_d, mult(X3, X4)) = mult(mult(op_d, X3), X4))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP702+1.p', goals)).