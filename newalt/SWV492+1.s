fof(f4438, plain, $false, inference(avatar_sat_refutation, [], [f80, f89, f94, f99, f104, f109, f114, f119, f124, f256, f918, f939, f1169, f1175, f1253, f1407, f1634, f1654, f1669, f1741, f4427])).
fof(f4427, plain, (spl6_4 | ~ spl6_6 | ~ spl6_7 | ~ spl6_49), inference(avatar_contradiction_clause, [], [f4426])).
fof(f4426, plain, ($false | (spl6_4 | ~ spl6_6 | ~ spl6_7 | ~ spl6_49)), inference(subsumption_resolution, [], [f4425, f88])).
fof(f88, plain, (~ (real_zero = a(sK2, sK3)) | spl6_4), inference(avatar_component_clause, [], [f86])).
fof(f86, plain, (spl6_4 <=> (real_zero = a(sK2, sK3))), introduced(avatar_definition, [new_symbols(naming, [spl6_4])])).
fof(f4425, plain, ((real_zero = a(sK2, sK3)) | (~ spl6_6 | ~ spl6_7 | ~ spl6_49)), inference(forward_demodulation, [], [f4424, f279])).
fof(f279, plain, ((sK3 = plus(sK2, sK1(sK2, sK3))) | ~ spl6_6), inference(resolution, [], [f98, f52])).
fof(f52, plain, ! [X0, X1] : (~ int_less(X0, X1) | (plus(X0, sK1(X0, X1)) = X1)), inference(cnf_transformation, [], [f34])).
fof(f34, plain, ! [X0, X1] : ((int_less(X0, X1) | ! [X2] : (~ int_less(int_zero, X2) | ~ (plus(X0, X2) = X1))) & ((int_less(int_zero, sK1(X0, X1)) & (plus(X0, sK1(X0, X1)) = X1)) | ~ int_less(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f32, f33])).
fof(f33, plain, ! [X1, X0] : (? [X3] : (int_less(int_zero, X3) & (plus(X0, X3) = X1)) => (int_less(int_zero, sK1(X0, X1)) & (plus(X0, sK1(X0, X1)) = X1))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X0, X1] : ((int_less(X0, X1) | ! [X2] : (~ int_less(int_zero, X2) | ~ (plus(X0, X2) = X1))) & (? [X3] : (int_less(int_zero, X3) & (plus(X0, X3) = X1)) | ~ int_less(X0, X1))), inference(rectify, [], [f31])).
fof(f31, plain, ! [X0, X1] : ((int_less(X0, X1) | ! [X2] : (~ int_less(int_zero, X2) | ~ (plus(X0, X2) = X1))) & (? [X2] : (int_less(int_zero, X2) & (plus(X0, X2) = X1)) | ~ int_less(X0, X1))), inference(nnf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : (int_less(X0, X1) <=> ? [X2] : (int_less(int_zero, X2) & (plus(X0, X2) = X1))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', plus_and_inverse)).
fof(f98, plain, (int_less(sK2, sK3) | ~ spl6_6), inference(avatar_component_clause, [], [f96])).
fof(f96, plain, (spl6_6 <=> int_less(sK2, sK3)), introduced(avatar_definition, [new_symbols(naming, [spl6_6])])).
fof(f4424, plain, ((real_zero = a(sK2, plus(sK2, sK1(sK2, sK3)))) | (~ spl6_7 | ~ spl6_49)), inference(subsumption_resolution, [], [f4364, f69])).
fof(f69, plain, ! [X1] : int_leq(X1, X1), inference(equality_resolution, [], [f44])).
fof(f44, plain, ! [X0, X1] : (int_leq(X0, X1) | ~ (X0 = X1)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((int_leq(X0, X1) | (~ (X0 = X1) & ~ int_less(X0, X1))) & ((X0 = X1) | int_less(X0, X1) | ~ int_leq(X0, X1))), inference(flattening, [], [f29])).
fof(f29, plain, ! [X0, X1] : ((int_leq(X0, X1) | (~ (X0 = X1) & ~ int_less(X0, X1))) & (((X0 = X1) | int_less(X0, X1)) | ~ int_leq(X0, X1))), inference(nnf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (int_leq(X0, X1) <=> ((X0 = X1) | int_less(X0, X1))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', int_leq)).
fof(f4364, plain, ((real_zero = a(sK2, plus(sK2, sK1(sK2, sK3)))) | ~ int_leq(sK2, sK2) | (~ spl6_7 | ~ spl6_49)), inference(resolution, [], [f784, f103])).
fof(f103, plain, (int_leq(int_one, sK2) | ~ spl6_7), inference(avatar_component_clause, [], [f101])).
fof(f101, plain, (spl6_7 <=> int_leq(int_one, sK2)), introduced(avatar_definition, [new_symbols(naming, [spl6_7])])).
fof(f784, plain, (! [X10] : (~ int_leq(int_one, X10) | (real_zero = a(X10, plus(X10, sK1(sK2, sK3)))) | ~ int_leq(X10, sK2)) | ~ spl6_49), inference(avatar_component_clause, [], [f783])).
fof(f783, plain, (spl6_49 <=> ! [X10] : (~ int_leq(X10, sK2) | (real_zero = a(X10, plus(X10, sK1(sK2, sK3)))) | ~ int_leq(int_one, X10))), introduced(avatar_definition, [new_symbols(naming, [spl6_49])])).
fof(f1741, plain, (~ spl6_2 | ~ spl6_8 | ~ spl6_9 | ~ spl6_10 | ~ spl6_11), inference(avatar_contradiction_clause, [], [f1740])).
fof(f1740, plain, ($false | (~ spl6_2 | ~ spl6_8 | ~ spl6_9 | ~ spl6_10 | ~ spl6_11)), inference(subsumption_resolution, [], [f1739, f123])).
fof(f123, plain, (int_leq(int_one, sK4) | ~ spl6_11), inference(avatar_component_clause, [], [f121])).
fof(f121, plain, (spl6_11 <=> int_leq(int_one, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl6_11])])).
fof(f1739, plain, (~ int_leq(int_one, sK4) | (~ spl6_2 | ~ spl6_8 | ~ spl6_9 | ~ spl6_10 | ~ spl6_11)), inference(subsumption_resolution, [], [f1729, f250])).
fof(f250, plain, (int_leq(sK4, n) | (~ spl6_9 | ~ spl6_10)), inference(backward_demodulation, [], [f118, f113])).
fof(f113, plain, ((sK4 = sK5) | ~ spl6_9), inference(avatar_component_clause, [], [f111])).
fof(f111, plain, (spl6_9 <=> (sK4 = sK5)), introduced(avatar_definition, [new_symbols(naming, [spl6_9])])).
fof(f118, plain, (int_leq(sK5, n) | ~ spl6_10), inference(avatar_component_clause, [], [f116])).
fof(f116, plain, (spl6_10 <=> int_leq(sK5, n)), introduced(avatar_definition, [new_symbols(naming, [spl6_10])])).
fof(f1729, plain, (~ int_leq(sK4, n) | ~ int_leq(int_one, sK4) | (~ spl6_2 | ~ spl6_8 | ~ spl6_9 | ~ spl6_11)), inference(resolution, [], [f1675, f69])).
fof(f1675, plain, (! [X0] : (~ int_leq(sK4, X0) | ~ int_leq(X0, n) | ~ int_leq(int_one, X0)) | (~ spl6_2 | ~ spl6_8 | ~ spl6_9 | ~ spl6_11)), inference(subsumption_resolution, [], [f1674, f57])).
fof(f57, plain, ~ (real_zero = real_one), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ~ (real_zero = real_one), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', real_constants)).
fof(f1674, plain, (! [X0] : ((real_zero = real_one) | ~ int_leq(int_one, X0) | ~ int_leq(X0, n) | ~ int_leq(sK4, X0)) | (~ spl6_2 | ~ spl6_8 | ~ spl6_9 | ~ spl6_11)), inference(forward_demodulation, [], [f1671, f251])).
fof(f251, plain, ((real_zero = a(sK4, sK4)) | (~ spl6_8 | ~ spl6_9)), inference(forward_demodulation, [], [f108, f113])).
fof(f108, plain, ((real_zero = a(sK4, sK5)) | ~ spl6_8), inference(avatar_component_clause, [], [f106])).
fof(f106, plain, (spl6_8 <=> (real_zero = a(sK4, sK5))), introduced(avatar_definition, [new_symbols(naming, [spl6_8])])).
fof(f1671, plain, (! [X0] : (~ int_leq(int_one, X0) | ~ int_leq(X0, n) | (real_one = a(sK4, sK4)) | ~ int_leq(sK4, X0)) | (~ spl6_2 | ~ spl6_11)), inference(resolution, [], [f123, f79])).
fof(f79, plain, (! [X4, X1] : (~ int_leq(int_one, X4) | ~ int_leq(int_one, X1) | ~ int_leq(X1, n) | (real_one = a(X4, X4)) | ~ int_leq(X4, X1)) | ~ spl6_2), inference(avatar_component_clause, [], [f78])).
fof(f78, plain, (spl6_2 <=> ! [X1, X4] : ((real_one = a(X4, X4)) | ~ int_leq(int_one, X1) | ~ int_leq(X1, n) | ~ int_leq(int_one, X4) | ~ int_leq(X4, X1))), introduced(avatar_definition, [new_symbols(naming, [spl6_2])])).
fof(f1669, plain, (spl6_65 | ~ spl6_6 | ~ spl6_7), inference(avatar_split_clause, [], [f556, f101, f96, f1638])).
fof(f1638, plain, (spl6_65 <=> int_leq(int_one, sK3)), introduced(avatar_definition, [new_symbols(naming, [spl6_65])])).
fof(f556, plain, (int_leq(int_one, sK3) | (~ spl6_6 | ~ spl6_7)), inference(resolution, [], [f354, f55])).
fof(f55, plain, ! [X0] : (~ int_less(int_zero, X0) | int_leq(int_one, X0)), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : ((int_less(int_zero, X0) | ~ int_leq(int_one, X0)) & (int_leq(int_one, X0) | ~ int_less(int_zero, X0))), inference(nnf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (int_less(int_zero, X0) <=> int_leq(int_one, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', one_successor_of_zero)).
fof(f354, plain, (int_less(int_zero, sK3) | (~ spl6_6 | ~ spl6_7)), inference(resolution, [], [f280, f283])).
fof(f283, plain, (int_less(int_zero, sK2) | ~ spl6_7), inference(resolution, [], [f103, f56])).
fof(f56, plain, ! [X0] : (~ int_leq(int_one, X0) | int_less(int_zero, X0)), inference(cnf_transformation, [], [f35])).
fof(f280, plain, (! [X0] : (~ int_less(X0, sK2) | int_less(X0, sK3)) | ~ spl6_6), inference(resolution, [], [f98, f45])).
fof(f45, plain, ! [X2, X0, X1] : (~ int_less(X1, X2) | int_less(X0, X2) | ~ int_less(X0, X1)), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ! [X0, X1, X2] : (int_less(X0, X2) | ~ int_less(X1, X2) | ~ int_less(X0, X1)), inference(flattening, [], [f18])).
fof(f18, plain, ! [X0, X1, X2] : (int_less(X0, X2) | (~ int_less(X1, X2) | ~ int_less(X0, X1))), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ! [X0, X1, X2] : ((int_less(X1, X2) & int_less(X0, X1)) => int_less(X0, X2)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', int_less_transitive)).
fof(f1654, plain, (~ spl6_65 | ~ spl6_48 | spl6_49 | ~ spl6_5 | ~ spl6_6 | ~ spl6_7 | ~ spl6_47), inference(avatar_split_clause, [], [f1653, f775, f101, f96, f91, f783, f779, f1638])).
fof(f779, plain, (spl6_48 <=> int_less(int_zero, sK1(sK2, sK3))), introduced(avatar_definition, [new_symbols(naming, [spl6_48])])).
fof(f91, plain, (spl6_5 <=> int_leq(sK3, n)), introduced(avatar_definition, [new_symbols(naming, [spl6_5])])).
fof(f775, plain, (spl6_47 <=> int_leq(sK2, n)), introduced(avatar_definition, [new_symbols(naming, [spl6_47])])).
fof(f1653, plain, (! [X10] : (~ int_leq(sK3, n) | ~ int_leq(X10, sK2) | ~ int_leq(int_one, X10) | ~ int_less(int_zero, sK1(sK2, sK3)) | (real_zero = a(X10, plus(X10, sK1(sK2, sK3)))) | ~ int_leq(int_one, sK3)) | (~ spl6_6 | ~ spl6_7 | ~ spl6_47)), inference(subsumption_resolution, [], [f1652, f103])).
fof(f1652, plain, (! [X10] : (~ int_leq(sK3, n) | ~ int_leq(X10, sK2) | ~ int_leq(int_one, X10) | ~ int_less(int_zero, sK1(sK2, sK3)) | (real_zero = a(X10, plus(X10, sK1(sK2, sK3)))) | ~ int_leq(int_one, sK3) | ~ int_leq(int_one, sK2)) | (~ spl6_6 | ~ spl6_47)), inference(subsumption_resolution, [], [f768, f776])).
fof(f776, plain, (int_leq(sK2, n) | ~ spl6_47), inference(avatar_component_clause, [], [f775])).
fof(f768, plain, (! [X10] : (~ int_leq(sK3, n) | ~ int_leq(X10, sK2) | ~ int_leq(int_one, X10) | ~ int_less(int_zero, sK1(sK2, sK3)) | (real_zero = a(X10, plus(X10, sK1(sK2, sK3)))) | ~ int_leq(int_one, sK3) | ~ int_leq(sK2, n) | ~ int_leq(int_one, sK2)) | ~ spl6_6), inference(superposition, [], [f72, f279])).
fof(f72, plain, ! [X2, X0, X3] : (~ int_leq(plus(X0, X2), n) | ~ int_leq(X3, X0) | ~ int_leq(int_one, X3) | ~ int_less(int_zero, X2) | (real_zero = a(X3, plus(X3, X2))) | ~ int_leq(int_one, plus(X0, X2)) | ~ int_leq(X0, n) | ~ int_leq(int_one, X0)), inference(equality_resolution, [], [f60])).
fof(f60, plain, ! [X2, X0, X3, X1] : ((real_zero = a(X3, plus(X3, X2))) | ~ int_leq(X3, X0) | ~ int_leq(int_one, X3) | ~ (plus(X0, X2) = X1) | ~ int_less(int_zero, X2) | ~ int_leq(X1, n) | ~ int_leq(int_one, X1) | ~ int_leq(X0, n) | ~ int_leq(int_one, X0)), inference(cnf_transformation, [], [f24])).
fof(f24, plain, ! [X0, X1] : ((! [X2] : (! [X3] : ((real_zero = a(X3, plus(X3, X2))) | ~ int_leq(X3, X0) | ~ int_leq(int_one, X3)) | ~ (plus(X0, X2) = X1) | ~ int_less(int_zero, X2)) & ! [X4] : ((real_one = a(X4, X4)) | ~ int_leq(X4, X1) | ~ int_leq(int_one, X4)) & ! [X5] : (! [X6] : ((a(plus(X6, X5), X6) = qr(plus(X6, X5), X6)) | ~ int_leq(X6, X1) | ~ int_leq(int_one, X6)) | ~ (plus(X1, X5) = X0) | ~ int_less(int_zero, X5))) | ~ int_leq(X1, n) | ~ int_leq(int_one, X1) | ~ int_leq(X0, n) | ~ int_leq(int_one, X0)), inference(flattening, [], [f23])).
fof(f23, plain, ! [X0, X1] : ((! [X2] : (! [X3] : ((real_zero = a(X3, plus(X3, X2))) | (~ int_leq(X3, X0) | ~ int_leq(int_one, X3))) | (~ (plus(X0, X2) = X1) | ~ int_less(int_zero, X2))) & ! [X4] : ((real_one = a(X4, X4)) | (~ int_leq(X4, X1) | ~ int_leq(int_one, X4))) & ! [X5] : (! [X6] : ((a(plus(X6, X5), X6) = qr(plus(X6, X5), X6)) | (~ int_leq(X6, X1) | ~ int_leq(int_one, X6))) | (~ (plus(X1, X5) = X0) | ~ int_less(int_zero, X5)))) | (~ int_leq(X1, n) | ~ int_leq(int_one, X1) | ~ int_leq(X0, n) | ~ int_leq(int_one, X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((int_leq(X1, n) & int_leq(int_one, X1) & int_leq(X0, n) & int_leq(int_one, X0)) => (! [X2] : (((plus(X0, X2) = X1) & int_less(int_zero, X2)) => ! [X3] : ((int_leq(X3, X0) & int_leq(int_one, X3)) => (real_zero = a(X3, plus(X3, X2))))) & ! [X4] : ((int_leq(X4, X1) & int_leq(int_one, X4)) => (real_one = a(X4, X4))) & ! [X5] : (((plus(X1, X5) = X0) & int_less(int_zero, X5)) => ! [X6] : ((int_leq(X6, X1) & int_leq(int_one, X6)) => (a(plus(X6, X5), X6) = qr(plus(X6, X5), X6)))))), inference(rectify, [], [f12])).
fof(f12, plain, ! [X0, X1] : ((int_leq(X1, n) & int_leq(int_one, X1) & int_leq(X0, n) & int_leq(int_one, X0)) => (! [X7] : (((plus(X0, X7) = X1) & int_less(int_zero, X7)) => ! [X2] : ((int_leq(X2, X0) & int_leq(int_one, X2)) => (real_zero = a(X2, plus(X2, X7))))) & ! [X2] : ((int_leq(X2, X1) & int_leq(int_one, X2)) => (real_one = a(X2, X2))) & ! [X7] : (((plus(X1, X7) = X0) & int_less(int_zero, X7)) => ! [X2] : ((int_leq(X2, X1) & int_leq(int_one, X2)) => (a(plus(X2, X7), X2) = qr(plus(X2, X7), X2)))))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', qih)).
fof(f1634, plain, (~ spl6_6 | ~ spl6_14 | ~ spl6_29), inference(avatar_contradiction_clause, [], [f1633])).
fof(f1633, plain, ($false | (~ spl6_6 | ~ spl6_14 | ~ spl6_29)), inference(subsumption_resolution, [], [f1627, f70])).
fof(f70, plain, ! [X1] : ~ int_less(X1, X1), inference(equality_resolution, [], [f46])).
fof(f46, plain, ! [X0, X1] : (~ (X0 = X1) | ~ int_less(X0, X1)), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ! [X0, X1] : (~ (X0 = X1) | ~ int_less(X0, X1)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0, X1] : (int_less(X0, X1) => ~ (X0 = X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', int_less_irreflexive)).
fof(f1627, plain, (int_less(n, n) | (~ spl6_6 | ~ spl6_14 | ~ spl6_29)), inference(resolution, [], [f443, f1550])).
fof(f1550, plain, (! [X0] : (~ int_less(X0, sK2) | int_less(X0, n)) | (~ spl6_6 | ~ spl6_14)), inference(forward_demodulation, [], [f280, f175])).
fof(f175, plain, ((n = sK3) | ~ spl6_14), inference(avatar_component_clause, [], [f173])).
fof(f173, plain, (spl6_14 <=> (n = sK3)), introduced(avatar_definition, [new_symbols(naming, [spl6_14])])).
fof(f443, plain, (int_less(n, sK2) | ~ spl6_29), inference(avatar_component_clause, [], [f441])).
fof(f441, plain, (spl6_29 <=> int_less(n, sK2)), introduced(avatar_definition, [new_symbols(naming, [spl6_29])])).
fof(f1407, plain, (spl6_14 | spl6_15 | ~ spl6_5), inference(avatar_split_clause, [], [f278, f91, f177, f173])).
fof(f177, plain, (spl6_15 <=> int_less(sK3, n)), introduced(avatar_definition, [new_symbols(naming, [spl6_15])])).
fof(f278, plain, (int_less(sK3, n) | (n = sK3) | ~ spl6_5), inference(resolution, [], [f93, f42])).
fof(f42, plain, ! [X0, X1] : (~ int_leq(X0, X1) | int_less(X0, X1) | (X0 = X1)), inference(cnf_transformation, [], [f30])).
fof(f93, plain, (int_leq(sK3, n) | ~ spl6_5), inference(avatar_component_clause, [], [f91])).
fof(f1253, plain, (~ spl6_28 | spl6_47), inference(avatar_contradiction_clause, [], [f1252])).
fof(f1252, plain, ($false | (~ spl6_28 | spl6_47)), inference(subsumption_resolution, [], [f1205, f69])).
fof(f1205, plain, (~ int_leq(n, n) | (~ spl6_28 | spl6_47)), inference(backward_demodulation, [], [f777, f439])).
fof(f439, plain, ((n = sK2) | ~ spl6_28), inference(avatar_component_clause, [], [f437])).
fof(f437, plain, (spl6_28 <=> (n = sK2)), introduced(avatar_definition, [new_symbols(naming, [spl6_28])])).
fof(f777, plain, (~ int_leq(sK2, n) | spl6_47), inference(avatar_component_clause, [], [f775])).
fof(f1175, plain, (spl6_28 | spl6_29 | spl6_47), inference(avatar_split_clause, [], [f1174, f775, f441, f437])).
fof(f1174, plain, ((n = sK2) | (spl6_29 | spl6_47)), inference(subsumption_resolution, [], [f911, f442])).
fof(f442, plain, (~ int_less(n, sK2) | spl6_29), inference(avatar_component_clause, [], [f441])).
fof(f911, plain, (int_less(n, sK2) | (n = sK2) | spl6_47), inference(resolution, [], [f809, f42])).
fof(f809, plain, (int_leq(n, sK2) | spl6_47), inference(resolution, [], [f777, f128])).
fof(f128, plain, ! [X2, X1] : (int_leq(X1, X2) | int_leq(X2, X1)), inference(resolution, [], [f47, f43])).
fof(f43, plain, ! [X0, X1] : (~ int_less(X0, X1) | int_leq(X0, X1)), inference(cnf_transformation, [], [f30])).
fof(f47, plain, ! [X0, X1] : (int_less(X0, X1) | int_leq(X1, X0)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1] : (int_leq(X1, X0) | int_less(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', int_less_total)).
fof(f1169, plain, (~ spl6_6 | ~ spl6_15 | ~ spl6_29), inference(avatar_contradiction_clause, [], [f1168])).
fof(f1168, plain, ($false | (~ spl6_6 | ~ spl6_15 | ~ spl6_29)), inference(subsumption_resolution, [], [f1163, f70])).
fof(f1163, plain, (int_less(n, n) | (~ spl6_6 | ~ spl6_15 | ~ spl6_29)), inference(resolution, [], [f311, f849])).
fof(f849, plain, (int_less(n, sK3) | (~ spl6_6 | ~ spl6_29)), inference(resolution, [], [f443, f280])).
fof(f311, plain, (! [X0] : (~ int_less(X0, sK3) | int_less(X0, n)) | ~ spl6_15), inference(resolution, [], [f179, f45])).
fof(f179, plain, (int_less(sK3, n) | ~ spl6_15), inference(avatar_component_clause, [], [f177])).
fof(f939, plain, (spl6_48 | ~ spl6_50), inference(avatar_split_clause, [], [f929, f884, f779])).
fof(f884, plain, (spl6_50 <=> int_leq(int_one, sK1(sK2, sK3))), introduced(avatar_definition, [new_symbols(naming, [spl6_50])])).
fof(f929, plain, (int_less(int_zero, sK1(sK2, sK3)) | ~ spl6_50), inference(resolution, [], [f885, f56])).
fof(f885, plain, (int_leq(int_one, sK1(sK2, sK3)) | ~ spl6_50), inference(avatar_component_clause, [], [f884])).
fof(f918, plain, (~ spl6_6 | spl6_50), inference(avatar_contradiction_clause, [], [f917])).
fof(f917, plain, ($false | (~ spl6_6 | spl6_50)), inference(subsumption_resolution, [], [f912, f98])).
fof(f912, plain, (~ int_less(sK2, sK3) | spl6_50), inference(resolution, [], [f886, f139])).
fof(f139, plain, ! [X0, X1] : (int_leq(int_one, sK1(X0, X1)) | ~ int_less(X0, X1)), inference(resolution, [], [f53, f55])).
fof(f53, plain, ! [X0, X1] : (int_less(int_zero, sK1(X0, X1)) | ~ int_less(X0, X1)), inference(cnf_transformation, [], [f34])).
fof(f886, plain, (~ int_leq(int_one, sK1(sK2, sK3)) | spl6_50), inference(avatar_component_clause, [], [f884])).
fof(f256, plain, (~ spl6_1 | ~ spl6_9 | ~ spl6_10 | ~ spl6_11), inference(avatar_contradiction_clause, [], [f255])).
fof(f255, plain, ($false | (~ spl6_1 | ~ spl6_9 | ~ spl6_10 | ~ spl6_11)), inference(subsumption_resolution, [], [f252, f250])).
fof(f252, plain, (~ int_leq(sK4, n) | (~ spl6_1 | ~ spl6_11)), inference(resolution, [], [f123, f76])).
fof(f76, plain, (! [X0] : (~ int_leq(int_one, X0) | ~ int_leq(X0, n)) | ~ spl6_1), inference(avatar_component_clause, [], [f75])).
fof(f75, plain, (spl6_1 <=> ! [X0] : (~ int_leq(X0, n) | ~ int_leq(int_one, X0))), introduced(avatar_definition, [new_symbols(naming, [spl6_1])])).
fof(f124, plain, (spl6_3 | spl6_11), inference(avatar_split_clause, [], [f65, f121, f82])).
fof(f82, plain, (spl6_3 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl6_3])])).
fof(f65, plain, (int_leq(int_one, sK4) | sP0), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (((real_zero = a(sK4, sK5)) & (sK4 = sK5) & int_leq(sK5, n) & int_leq(int_one, sK4)) | sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5])], [f28, f40])).
fof(f40, plain, (? [X0, X1] : ((real_zero = a(X0, X1)) & (X0 = X1) & int_leq(X1, n) & int_leq(int_one, X0)) => ((real_zero = a(sK4, sK5)) & (sK4 = sK5) & int_leq(sK5, n) & int_leq(int_one, sK4))), introduced(choice_axiom, [])).
fof(f28, plain, (? [X0, X1] : ((real_zero = a(X0, X1)) & (X0 = X1) & int_leq(X1, n) & int_leq(int_one, X0)) | sP0), inference(definition_folding, [], [f26, e27])).
fof(f27, plain, (? [X2, X3] : (~ (real_zero = a(X2, X3)) & int_leq(X3, n) & int_less(X2, X3) & int_leq(int_one, X2)) | ~ sP0), inference(usedef, [], [e27])).
fof(e27, plain, (sP0 <=> ? [X2, X3] : (~ (real_zero = a(X2, X3)) & int_leq(X3, n) & int_less(X2, X3) & int_leq(int_one, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f26, plain, (? [X0, X1] : ((real_zero = a(X0, X1)) & (X0 = X1) & int_leq(X1, n) & int_leq(int_one, X0)) | ? [X2, X3] : (~ (real_zero = a(X2, X3)) & int_leq(X3, n) & int_less(X2, X3) & int_leq(int_one, X2))), inference(flattening, [], [f25])).
fof(f25, plain, (? [X0, X1] : ((real_zero = a(X0, X1)) & ((X0 = X1) & int_leq(X1, n) & int_leq(int_one, X0))) | ? [X2, X3] : (~ (real_zero = a(X2, X3)) & (int_leq(X3, n) & int_less(X2, X3) & int_leq(int_one, X2)))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ~ (! [X0, X1] : (((X0 = X1) & int_leq(X1, n) & int_leq(int_one, X0)) => ~ (real_zero = a(X0, X1))) & ! [X2, X3] : ((int_leq(X3, n) & int_less(X2, X3) & int_leq(int_one, X2)) => (real_zero = a(X2, X3)))), inference(rectify, [], [f14])).
fof(f14, plain, ~ (! [X0, X1] : (((X0 = X1) & int_leq(X1, n) & int_leq(int_one, X0)) => ~ (real_zero = a(X0, X1))) & ! [X0, X1] : ((int_leq(X1, n) & int_less(X0, X1) & int_leq(int_one, X0)) => (real_zero = a(X0, X1)))), inference(negated_conjecture, [], [f13])).
fof(f13, plain, ~ (! [X0, X1] : (((X0 = X1) & int_leq(X1, n) & int_leq(int_one, X0)) => ~ (real_zero = a(X0, X1))) & ! [X0, X1] : ((int_leq(X1, n) & int_less(X0, X1) & int_leq(int_one, X0)) => (real_zero = a(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV492+1.p', lti)).
fof(f119, plain, (spl6_3 | spl6_10), inference(avatar_split_clause, [], [f66, f116, f82])).
fof(f66, plain, (int_leq(sK5, n) | sP0), inference(cnf_transformation, [], [f41])).
fof(f114, plain, (spl6_3 | spl6_9), inference(avatar_split_clause, [], [f67, f111, f82])).
fof(f67, plain, ((sK4 = sK5) | sP0), inference(cnf_transformation, [], [f41])).
fof(f109, plain, (spl6_3 | spl6_8), inference(avatar_split_clause, [], [f68, f106, f82])).
fof(f68, plain, ((real_zero = a(sK4, sK5)) | sP0), inference(cnf_transformation, [], [f41])).
fof(f104, plain, (~ spl6_3 | spl6_7), inference(avatar_split_clause, [], [f61, f101, f82])).
fof(f61, plain, (int_leq(int_one, sK2) | ~ sP0), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ((~ (real_zero = a(sK2, sK3)) & int_leq(sK3, n) & int_less(sK2, sK3) & int_leq(int_one, sK2)) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3])], [f37, f38])).
fof(f38, plain, (? [X0, X1] : (~ (real_zero = a(X0, X1)) & int_leq(X1, n) & int_less(X0, X1) & int_leq(int_one, X0)) => (~ (real_zero = a(sK2, sK3)) & int_leq(sK3, n) & int_less(sK2, sK3) & int_leq(int_one, sK2))), introduced(choice_axiom, [])).
fof(f37, plain, (? [X0, X1] : (~ (real_zero = a(X0, X1)) & int_leq(X1, n) & int_less(X0, X1) & int_leq(int_one, X0)) | ~ sP0), inference(rectify, [], [f36])).
fof(f36, plain, (? [X2, X3] : (~ (real_zero = a(X2, X3)) & int_leq(X3, n) & int_less(X2, X3) & int_leq(int_one, X2)) | ~ sP0), inference(nnf_transformation, [], [f27])).
fof(f99, plain, (~ spl6_3 | spl6_6), inference(avatar_split_clause, [], [f62, f96, f82])).
fof(f62, plain, (int_less(sK2, sK3) | ~ sP0), inference(cnf_transformation, [], [f39])).
fof(f94, plain, (~ spl6_3 | spl6_5), inference(avatar_split_clause, [], [f63, f91, f82])).
fof(f63, plain, (int_leq(sK3, n) | ~ sP0), inference(cnf_transformation, [], [f39])).
fof(f89, plain, (~ spl6_3 | ~ spl6_4), inference(avatar_split_clause, [], [f64, f86, f82])).
fof(f64, plain, (~ (real_zero = a(sK2, sK3)) | ~ sP0), inference(cnf_transformation, [], [f39])).
fof(f80, plain, (spl6_1 | spl6_2), inference(avatar_split_clause, [], [f59, f78, f75])).
fof(f59, plain, ! [X4, X0, X1] : ((real_one = a(X4, X4)) | ~ int_leq(X4, X1) | ~ int_leq(int_one, X4) | ~ int_leq(X1, n) | ~ int_leq(int_one, X1) | ~ int_leq(X0, n) | ~ int_leq(int_one, X0)), inference(cnf_transformation, [], [f24])).