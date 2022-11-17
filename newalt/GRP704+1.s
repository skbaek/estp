fof(f299, plain, $false, inference(avatar_sat_refutation, [], [f50, f254, f293, f297])).
fof(f297, plain, spl2_3, inference(avatar_contradiction_clause, [], [f296])).
fof(f296, plain, ($false | spl2_3), inference(subsumption_resolution, [], [f295, f33])).
fof(f33, plain, ! [X0, X1] : (mult(X1, mult(op_c, X0)) = mult(mult(X1, op_c), X0)), inference(cnf_transformation, [], [f10])).
fof(f10, plain, ! [B, A] : (mult(A, mult(op_c, B)) = mult(mult(A, op_c), B)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP704+1.p', f10)).
fof(f295, plain, (~ (mult(sK0, mult(op_c, sK1)) = mult(mult(sK0, op_c), sK1)) | spl2_3), inference(forward_demodulation, [], [f49, f246])).
fof(f246, plain, (op_c = op_f), inference(forward_demodulation, [], [f245, f24])).
fof(f24, plain, ! [X0, X1] : (mult(X1, ld(X1, X0)) = X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (mult(X1, ld(X1, X0)) = X0), file('/home/ubuntu/library/tptp/Problems/GRP/GRP704+1.p', f01)).
fof(f245, plain, ! [X4] : (op_f = mult(X4, ld(X4, op_c))), inference(forward_demodulation, [], [f223, f29])).
fof(f29, plain, ! [X0] : (mult(unit, X0) = X0), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (mult(unit, X0) = X0), inference(rectify, [], [f6])).
fof(f6, plain, ! [X1] : (mult(unit, X1) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP704+1.p', f06)).
fof(f223, plain, ! [X4] : (op_f = mult(X4, mult(unit, ld(X4, op_c)))), inference(superposition, [], [f36, f28])).
fof(f28, plain, ! [X0] : (mult(X0, unit) = X0), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (mult(X0, unit) = X0), inference(rectify, [], [f5])).
fof(f5, plain, ! [X1] : (mult(X1, unit) = X1), file('/home/ubuntu/library/tptp/Problems/GRP/GRP704+1.p', f05)).
fof(f36, plain, ! [X0, X1] : (op_f = mult(X1, mult(X0, ld(mult(X1, X0), op_c)))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ! [B, A] : (op_f = mult(A, mult(B, ld(mult(A, B), op_c)))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP704+1.p', f13)).
fof(f49, plain, (~ (mult(sK0, mult(op_f, sK1)) = mult(mult(sK0, op_f), sK1)) | spl2_3), inference(avatar_component_clause, [], [f47])).
fof(f47, plain, (spl2_3 <=> (mult(sK0, mult(op_f, sK1)) = mult(mult(sK0, op_f), sK1))), introduced(avatar_definition, [new_symbols(naming, [spl2_3])])).
fof(f293, plain, spl2_2, inference(avatar_contradiction_clause, [], [f292])).
fof(f292, plain, ($false | spl2_2), inference(subsumption_resolution, [], [f291, f32])).
fof(f32, plain, ! [X0, X1] : (mult(X1, mult(X0, op_c)) = mult(mult(X1, X0), op_c)), inference(cnf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : (mult(X1, mult(X0, op_c)) = mult(mult(X1, X0), op_c)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP704+1.p', f09)).
fof(f291, plain, (~ (mult(sK0, mult(sK1, op_c)) = mult(mult(sK0, sK1), op_c)) | spl2_2), inference(forward_demodulation, [], [f45, f246])).
fof(f45, plain, (~ (mult(sK0, mult(sK1, op_f)) = mult(mult(sK0, sK1), op_f)) | spl2_2), inference(avatar_component_clause, [], [f43])).
fof(f43, plain, (spl2_2 <=> (mult(sK0, mult(sK1, op_f)) = mult(mult(sK0, sK1), op_f))), introduced(avatar_definition, [new_symbols(naming, [spl2_2])])).
fof(f254, plain, spl2_1, inference(avatar_contradiction_clause, [], [f253])).
fof(f253, plain, ($false | spl2_1), inference(subsumption_resolution, [], [f249, f31])).
fof(f31, plain, ! [X0, X1] : (mult(op_c, mult(X1, X0)) = mult(mult(op_c, X1), X0)), inference(cnf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (mult(op_c, mult(X1, X0)) = mult(mult(op_c, X1), X0)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP704+1.p', f08)).
fof(f249, plain, (~ (mult(op_c, mult(sK0, sK1)) = mult(mult(op_c, sK0), sK1)) | spl2_1), inference(backward_demodulation, [], [f41, f246])).
fof(f41, plain, (~ (mult(op_f, mult(sK0, sK1)) = mult(mult(op_f, sK0), sK1)) | spl2_1), inference(avatar_component_clause, [], [f39])).
fof(f39, plain, (spl2_1 <=> (mult(op_f, mult(sK0, sK1)) = mult(mult(op_f, sK0), sK1))), introduced(avatar_definition, [new_symbols(naming, [spl2_1])])).
fof(f50, plain, (~ spl2_1 | ~ spl2_2 | ~ spl2_3), inference(avatar_split_clause, [], [f37, f47, f43, f39])).
fof(f37, plain, (~ (mult(sK0, mult(op_f, sK1)) = mult(mult(sK0, op_f), sK1)) | ~ (mult(sK0, mult(sK1, op_f)) = mult(mult(sK0, sK1), op_f)) | ~ (mult(op_f, mult(sK0, sK1)) = mult(mult(op_f, sK0), sK1))), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (~ (mult(sK0, mult(op_f, sK1)) = mult(mult(sK0, op_f), sK1)) | ~ (mult(sK0, mult(sK1, op_f)) = mult(mult(sK0, sK1), op_f)) | ~ (mult(op_f, mult(sK0, sK1)) = mult(mult(op_f, sK0), sK1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK0, sK1])], [f21, f22])).
fof(f22, plain, (? [X0, X1] : (~ (mult(X0, mult(op_f, X1)) = mult(mult(X0, op_f), X1)) | ~ (mult(X0, mult(X1, op_f)) = mult(mult(X0, X1), op_f)) | ~ (mult(op_f, mult(X0, X1)) = mult(mult(op_f, X0), X1))) => (~ (mult(sK0, mult(op_f, sK1)) = mult(mult(sK0, op_f), sK1)) | ~ (mult(sK0, mult(sK1, op_f)) = mult(mult(sK0, sK1), op_f)) | ~ (mult(op_f, mult(sK0, sK1)) = mult(mult(op_f, sK0), sK1)))), introduced(choice_axiom, [])).
fof(f21, plain, ? [X0, X1] : (~ (mult(X0, mult(op_f, X1)) = mult(mult(X0, op_f), X1)) | ~ (mult(X0, mult(X1, op_f)) = mult(mult(X0, X1), op_f)) | ~ (mult(op_f, mult(X0, X1)) = mult(mult(op_f, X0), X1))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ! [X0, X1] : ((mult(X0, mult(op_f, X1)) = mult(mult(X0, op_f), X1)) & (mult(X0, mult(X1, op_f)) = mult(mult(X0, X1), op_f)) & (mult(op_f, mult(X0, X1)) = mult(mult(op_f, X0), X1))), inference(rectify, [], [f15])).
fof(f15, plain, ~ ! [X3, X4] : ((mult(X3, mult(op_f, X4)) = mult(mult(X3, op_f), X4)) & (mult(X3, mult(X4, op_f)) = mult(mult(X3, X4), op_f)) & (mult(op_f, mult(X3, X4)) = mult(mult(op_f, X3), X4))), inference(negated_conjecture, [], [f14])).
fof(f14, plain, ~ ! [X3, X4] : ((mult(X3, mult(op_f, X4)) = mult(mult(X3, op_f), X4)) & (mult(X3, mult(X4, op_f)) = mult(mult(X3, X4), op_f)) & (mult(op_f, mult(X3, X4)) = mult(mult(op_f, X3), X4))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP704+1.p', goals)).