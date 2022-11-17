fof(f232, plain, $false, inference(avatar_sat_refutation, [], [f50, f184, f226, f230])).
fof(f230, plain, spl2_3, inference(avatar_contradiction_clause, [], [f229])).
fof(f229, plain, ($false | spl2_3), inference(subsumption_resolution, [], [f228, f33])).
fof(f33, plain, ! [X0, X1] : (mult(X1, mult(op_c, X0)) = mult(mult(X1, op_c), X0)), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ! [B, A] : (mult(A, mult(op_c, B)) = mult(mult(A, op_c), B)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP703+1.p', f10)).
fof(f228, plain, (~ (mult(sK0, mult(op_c, sK1)) = mult(mult(sK0, op_c), sK1)) | spl2_3), inference(forward_demodulation, [], [f49, f180])).
fof(f180, plain, (op_c = op_e), inference(forward_demodulation, [], [f179, f26])).
fof(f26, plain, ! [X0, X1] : (mult(rd(X1, X0), X0) = X1), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ! [X0, X1] : (mult(rd(X1, X0), X0) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP703+1.p', f03)).
fof(f179, plain, ! [X0] : (op_e = mult(rd(op_c, X0), X0)), inference(forward_demodulation, [], [f155, f28])).
fof(f28, plain, ! [X0] : (mult(X0, unit) = X0), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (mult(X0, unit) = X0), inference(rectify, [], [f5])).
fof(f5, plain, ! [X1] : (mult(X1, unit) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP703+1.p', f05)).
fof(f155, plain, ! [X0] : (op_e = mult(mult(rd(op_c, X0), unit), X0)), inference(superposition, [], [f35, f28])).
fof(f35, plain, ! [X0, X1] : (op_e = mult(mult(rd(op_c, mult(X1, X0)), X0), X1)), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ! [B, A] : (op_e = mult(mult(rd(op_c, mult(A, B)), B), A)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP703+1.p', f12)).
fof(f49, plain, (~ (mult(sK0, mult(op_e, sK1)) = mult(mult(sK0, op_e), sK1)) | spl2_3), inference(avatar_component_clause, [], [f47])).
fof(f47, plain, (spl2_3 <=> (mult(sK0, mult(op_e, sK1)) = mult(mult(sK0, op_e), sK1))), introduced(avatar_definition, [new_symbols(naming, [spl2_3])])).
fof(f226, plain, spl2_2, inference(avatar_contradiction_clause, [], [f225])).
fof(f225, plain, ($false | spl2_2), inference(subsumption_resolution, [], [f224, f32])).
fof(f32, plain, ! [X0, X1] : (mult(X1, mult(X0, op_c)) = mult(mult(X1, X0), op_c)), inference(cnf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : (mult(X1, mult(X0, op_c)) = mult(mult(X1, X0), op_c)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP703+1.p', f09)).
fof(f224, plain, (~ (mult(sK0, mult(sK1, op_c)) = mult(mult(sK0, sK1), op_c)) | spl2_2), inference(forward_demodulation, [], [f45, f180])).
fof(f45, plain, (~ (mult(sK0, mult(sK1, op_e)) = mult(mult(sK0, sK1), op_e)) | spl2_2), inference(avatar_component_clause, [], [f43])).
fof(f43, plain, (spl2_2 <=> (mult(sK0, mult(sK1, op_e)) = mult(mult(sK0, sK1), op_e))), introduced(avatar_definition, [new_symbols(naming, [spl2_2])])).
fof(f184, plain, spl2_1, inference(avatar_contradiction_clause, [], [f183])).
fof(f183, plain, ($false | spl2_1), inference(subsumption_resolution, [], [f181, f31])).
fof(f31, plain, ! [X0, X1] : (mult(op_c, mult(X1, X0)) = mult(mult(op_c, X1), X0)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (mult(op_c, mult(X1, X0)) = mult(mult(op_c, X1), X0)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP703+1.p', f08)).
fof(f181, plain, (~ (mult(op_c, mult(sK0, sK1)) = mult(mult(op_c, sK0), sK1)) | spl2_1), inference(backward_demodulation, [], [f41, f180])).
fof(f41, plain, (~ (mult(op_e, mult(sK0, sK1)) = mult(mult(op_e, sK0), sK1)) | spl2_1), inference(avatar_component_clause, [], [f39])).
fof(f39, plain, (spl2_1 <=> (mult(op_e, mult(sK0, sK1)) = mult(mult(op_e, sK0), sK1))), introduced(avatar_definition, [new_symbols(naming, [spl2_1])])).
fof(f50, plain, (~ spl2_1 | ~ spl2_2 | ~ spl2_3), inference(avatar_split_clause, [], [f37, f47, f43, f39])).
fof(f37, plain, (~ (mult(sK0, mult(op_e, sK1)) = mult(mult(sK0, op_e), sK1)) | ~ (mult(sK0, mult(sK1, op_e)) = mult(mult(sK0, sK1), op_e)) | ~ (mult(op_e, mult(sK0, sK1)) = mult(mult(op_e, sK0), sK1))), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (~ (mult(sK0, mult(op_e, sK1)) = mult(mult(sK0, op_e), sK1)) | ~ (mult(sK0, mult(sK1, op_e)) = mult(mult(sK0, sK1), op_e)) | ~ (mult(op_e, mult(sK0, sK1)) = mult(mult(op_e, sK0), sK1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK0, sK1])], [f21, f22])).
fof(f22, plain, (? [X0, X1] : (~ (mult(X0, mult(op_e, X1)) = mult(mult(X0, op_e), X1)) | ~ (mult(X0, mult(X1, op_e)) = mult(mult(X0, X1), op_e)) | ~ (mult(op_e, mult(X0, X1)) = mult(mult(op_e, X0), X1))) => (~ (mult(sK0, mult(op_e, sK1)) = mult(mult(sK0, op_e), sK1)) | ~ (mult(sK0, mult(sK1, op_e)) = mult(mult(sK0, sK1), op_e)) | ~ (mult(op_e, mult(sK0, sK1)) = mult(mult(op_e, sK0), sK1)))), introduced(choice_axiom, [])).
fof(f21, plain, ? [X0, X1] : (~ (mult(X0, mult(op_e, X1)) = mult(mult(X0, op_e), X1)) | ~ (mult(X0, mult(X1, op_e)) = mult(mult(X0, X1), op_e)) | ~ (mult(op_e, mult(X0, X1)) = mult(mult(op_e, X0), X1))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ! [X0, X1] : ((mult(X0, mult(op_e, X1)) = mult(mult(X0, op_e), X1)) & (mult(X0, mult(X1, op_e)) = mult(mult(X0, X1), op_e)) & (mult(op_e, mult(X0, X1)) = mult(mult(op_e, X0), X1))), inference(rectify, [], [f15])).
fof(f15, plain, ~ ! [X3, X4] : ((mult(X3, mult(op_e, X4)) = mult(mult(X3, op_e), X4)) & (mult(X3, mult(X4, op_e)) = mult(mult(X3, X4), op_e)) & (mult(op_e, mult(X3, X4)) = mult(mult(op_e, X3), X4))), inference(negated_conjecture, [], [f14])).
fof(f14, plain, ~ ! [X3, X4] : ((mult(X3, mult(op_e, X4)) = mult(mult(X3, op_e), X4)) & (mult(X3, mult(X4, op_e)) = mult(mult(X3, X4), op_e)) & (mult(op_e, mult(X3, X4)) = mult(mult(op_e, X3), X4))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP703+1.p', goals)).