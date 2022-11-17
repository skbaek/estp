fof(f54610, plain, $false, inference(avatar_sat_refutation, [], [f44, f53132, f54604])).
fof(f54604, plain, spl1_2, inference(avatar_contradiction_clause, [], [f54603])).
fof(f54603, plain, ($false | spl1_2), inference(trivial_inequality_removal, [], [f54581])).
fof(f54581, plain, (~ (sK0 = sK0) | spl1_2), inference(superposition, [], [f43, f54191])).
fof(f54191, plain, ! [X5] : (mult(mult(X5, op_b), op_a) = X5), inference(forward_demodulation, [], [f54179, f52789])).
fof(f52789, plain, ! [X1] : (mult(mult(X1, op_a), op_b) = X1), inference(forward_demodulation, [], [f52788, f31])).
fof(f31, plain, ! [X0] : (mult(X0, unit) = X0), inference(cnf_transformation, [], [f18])).
fof(f18, plain, ! [X0] : (mult(X0, unit) = X0), inference(rectify, [], [f8])).
fof(f8, plain, ! [X2] : (mult(X2, unit) = X2), file('/home/ubuntu/library/tptp/Problems/GRP/GRP715+1.p', f08)).
fof(f52788, plain, ! [X1] : (mult(X1, unit) = mult(mult(X1, op_a), op_b)), inference(forward_demodulation, [], [f52787, f33])).
fof(f33, plain, (unit = mult(op_a, op_b)), inference(cnf_transformation, [], [f10])).
fof(f10, plain, (mult(op_a, op_b) = unit), file('/home/ubuntu/library/tptp/Problems/GRP/GRP715+1.p', f10)).
fof(f52787, plain, ! [X1] : (mult(mult(X1, op_a), op_b) = mult(X1, mult(op_a, op_b))), inference(forward_demodulation, [], [f52777, f52716])).
fof(f52716, plain, ! [X18] : (mult(X18, op_b) = mult(mult(X18, mult(op_b, op_b)), op_a)), inference(forward_demodulation, [], [f52715, f80])).
fof(f80, plain, (op_b = mult(op_a, mult(op_b, op_b))), inference(forward_demodulation, [], [f76, f32])).
fof(f32, plain, ! [X0] : (mult(unit, X0) = X0), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ! [X0] : (mult(unit, X0) = X0), inference(rectify, [], [f9])).
fof(f9, plain, ! [X2] : (mult(unit, X2) = X2), file('/home/ubuntu/library/tptp/Problems/GRP/GRP715+1.p', f09)).
fof(f76, plain, (mult(op_a, mult(op_b, op_b)) = mult(unit, op_b)), inference(superposition, [], [f30, f33])).
fof(f30, plain, ! [X0, X1] : (mult(X1, mult(X0, X0)) = mult(mult(X1, X0), X0)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ! [X0, X1] : (mult(X1, mult(X0, X0)) = mult(mult(X1, X0), X0)), inference(rectify, [], [f7])).
fof(f7, plain, ! [X1, X2] : (mult(X2, mult(X1, X1)) = mult(mult(X2, X1), X1)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP715+1.p', f07)).
fof(f52715, plain, ! [X18] : (mult(X18, mult(op_a, mult(op_b, op_b))) = mult(mult(X18, mult(op_b, op_b)), op_a)), inference(forward_demodulation, [], [f52628, f1927])).
fof(f1927, plain, ! [X39] : (mult(X39, mult(op_b, op_b)) = mult(mult(X39, mult(op_b, mult(op_b, op_b))), op_a)), inference(forward_demodulation, [], [f1926, f31])).
fof(f1926, plain, ! [X39] : (mult(mult(X39, mult(op_b, mult(op_b, op_b))), op_a) = mult(mult(X39, mult(op_b, op_b)), unit)), inference(forward_demodulation, [], [f1925, f34])).
fof(f34, plain, (unit = mult(op_b, op_a)), inference(cnf_transformation, [], [f11])).
fof(f11, plain, (mult(op_b, op_a) = unit), file('/home/ubuntu/library/tptp/Problems/GRP/GRP715+1.p', f11)).
fof(f1925, plain, ! [X39] : (mult(mult(X39, mult(op_b, mult(op_b, op_b))), op_a) = mult(mult(X39, mult(op_b, op_b)), mult(op_b, op_a))), inference(forward_demodulation, [], [f1845, f80])).
fof(f1845, plain, ! [X39] : (mult(mult(X39, mult(op_b, op_b)), mult(mult(op_a, mult(op_b, op_b)), op_a)) = mult(mult(X39, mult(op_b, mult(op_b, op_b))), op_a)), inference(superposition, [], [f118, f783])).
fof(f783, plain, (op_b = mult(mult(op_b, op_b), op_a)), inference(forward_demodulation, [], [f782, f31])).
fof(f782, plain, (mult(op_b, unit) = mult(mult(op_b, op_b), op_a)), inference(forward_demodulation, [], [f768, f34])).
fof(f768, plain, (mult(mult(op_b, op_b), op_a) = mult(op_b, mult(op_b, op_a))), inference(superposition, [], [f130, f80])).
fof(f130, plain, ! [X13] : (mult(op_b, mult(mult(op_a, X13), op_a)) = mult(X13, op_a)), inference(forward_demodulation, [], [f115, f32])).
fof(f115, plain, ! [X13] : (mult(op_b, mult(mult(op_a, X13), op_a)) = mult(mult(unit, X13), op_a)), inference(superposition, [], [f29, f34])).
fof(f29, plain, ! [X2, X0, X1] : (mult(mult(mult(X2, X1), X0), X1) = mult(X2, mult(mult(X1, X0), X1))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ! [X0, X1, X2] : (mult(mult(mult(X2, X1), X0), X1) = mult(X2, mult(mult(X1, X0), X1))), file('/home/ubuntu/library/tptp/Problems/GRP/GRP715+1.p', f06)).
fof(f118, plain, ! [X4, X2, X3] : (mult(mult(X2, X3), mult(mult(X4, X3), X4)) = mult(mult(X2, mult(mult(X3, X4), X3)), X4)), inference(superposition, [], [f29, f29])).
fof(f52628, plain, ! [X18] : (mult(X18, mult(mult(op_a, mult(op_b, mult(op_b, op_b))), op_a)) = mult(mult(X18, mult(op_b, op_b)), op_a)), inference(superposition, [], [f29, f1935])).
fof(f1935, plain, ! [X48] : (mult(X48, mult(op_b, op_b)) = mult(mult(X48, op_a), mult(op_b, mult(op_b, op_b)))), inference(forward_demodulation, [], [f1934, f783])).
fof(f1934, plain, ! [X48] : (mult(mult(X48, op_a), mult(mult(mult(op_b, op_b), op_a), mult(op_b, op_b))) = mult(X48, mult(op_b, op_b))), inference(forward_demodulation, [], [f1933, f31])).
fof(f1933, plain, ! [X48] : (mult(mult(X48, op_a), mult(mult(mult(op_b, op_b), op_a), mult(op_b, op_b))) = mult(mult(X48, unit), mult(op_b, op_b))), inference(forward_demodulation, [], [f1851, f34])).
fof(f1851, plain, ! [X48] : (mult(mult(X48, op_a), mult(mult(mult(op_b, op_b), op_a), mult(op_b, op_b))) = mult(mult(X48, mult(op_b, op_a)), mult(op_b, op_b))), inference(superposition, [], [f118, f80])).
fof(f52777, plain, ! [X1] : (mult(mult(X1, op_a), op_b) = mult(X1, mult(mult(op_a, mult(op_b, op_b)), op_a))), inference(superposition, [], [f52716, f29])).
fof(f54179, plain, ! [X5] : (mult(mult(X5, op_b), op_a) = mult(mult(X5, op_a), op_b)), inference(superposition, [], [f53042, f53156])).
fof(f53156, plain, ! [X52] : (mult(X52, op_a) = mult(mult(X52, op_b), mult(op_a, op_a))), inference(backward_demodulation, [], [f2125, f53122])).
fof(f53122, plain, ! [X28] : (mult(mult(X28, op_a), mult(op_b, op_b)) = mult(X28, op_b)), inference(superposition, [], [f30, f52789])).
fof(f2125, plain, ! [X52] : (mult(X52, op_a) = mult(mult(mult(X52, op_a), mult(op_b, op_b)), mult(op_a, op_a))), inference(forward_demodulation, [], [f2124, f31])).
fof(f2124, plain, ! [X52] : (mult(mult(mult(X52, op_a), mult(op_b, op_b)), mult(op_a, op_a)) = mult(mult(X52, unit), op_a)), inference(forward_demodulation, [], [f2037, f34])).
fof(f2037, plain, ! [X52] : (mult(mult(mult(X52, op_a), mult(op_b, op_b)), mult(op_a, op_a)) = mult(mult(X52, mult(op_b, op_a)), op_a)), inference(superposition, [], [f126, f80])).
fof(f126, plain, ! [X14, X12, X13] : (mult(mult(mult(X12, X13), X14), mult(X13, X13)) = mult(mult(X12, mult(mult(X13, X14), X13)), X13)), inference(superposition, [], [f30, f29])).
fof(f53042, plain, ! [X18] : (mult(X18, op_a) = mult(mult(X18, mult(op_a, op_a)), op_b)), inference(forward_demodulation, [], [f53041, f81])).
fof(f81, plain, (op_a = mult(op_b, mult(op_a, op_a))), inference(forward_demodulation, [], [f77, f32])).
fof(f77, plain, (mult(op_b, mult(op_a, op_a)) = mult(unit, op_a)), inference(superposition, [], [f30, f34])).
fof(f53041, plain, ! [X18] : (mult(X18, mult(op_b, mult(op_a, op_a))) = mult(mult(X18, mult(op_a, op_a)), op_b)), inference(forward_demodulation, [], [f52955, f1930])).
fof(f1930, plain, ! [X40] : (mult(X40, mult(op_a, op_a)) = mult(mult(X40, mult(op_a, mult(op_a, op_a))), op_b)), inference(forward_demodulation, [], [f1929, f31])).
fof(f1929, plain, ! [X40] : (mult(mult(X40, mult(op_a, mult(op_a, op_a))), op_b) = mult(mult(X40, mult(op_a, op_a)), unit)), inference(forward_demodulation, [], [f1928, f33])).
fof(f1928, plain, ! [X40] : (mult(mult(X40, mult(op_a, mult(op_a, op_a))), op_b) = mult(mult(X40, mult(op_a, op_a)), mult(op_a, op_b))), inference(forward_demodulation, [], [f1846, f81])).
fof(f1846, plain, ! [X40] : (mult(mult(X40, mult(op_a, op_a)), mult(mult(op_b, mult(op_a, op_a)), op_b)) = mult(mult(X40, mult(op_a, mult(op_a, op_a))), op_b)), inference(superposition, [], [f118, f754])).
fof(f754, plain, (op_a = mult(mult(op_a, op_a), op_b)), inference(forward_demodulation, [], [f753, f31])).
fof(f753, plain, (mult(op_a, unit) = mult(mult(op_a, op_a), op_b)), inference(forward_demodulation, [], [f741, f33])).
fof(f741, plain, (mult(mult(op_a, op_a), op_b) = mult(op_a, mult(op_a, op_b))), inference(superposition, [], [f129, f81])).
fof(f129, plain, ! [X11] : (mult(op_a, mult(mult(op_b, X11), op_b)) = mult(X11, op_b)), inference(forward_demodulation, [], [f113, f32])).
fof(f113, plain, ! [X11] : (mult(op_a, mult(mult(op_b, X11), op_b)) = mult(mult(unit, X11), op_b)), inference(superposition, [], [f29, f33])).
fof(f52955, plain, ! [X18] : (mult(X18, mult(mult(op_b, mult(op_a, mult(op_a, op_a))), op_b)) = mult(mult(X18, mult(op_a, op_a)), op_b)), inference(superposition, [], [f29, f1943])).
fof(f1943, plain, ! [X53] : (mult(X53, mult(op_a, op_a)) = mult(mult(X53, op_b), mult(op_a, mult(op_a, op_a)))), inference(forward_demodulation, [], [f1942, f754])).
fof(f1942, plain, ! [X53] : (mult(mult(X53, op_b), mult(mult(mult(op_a, op_a), op_b), mult(op_a, op_a))) = mult(X53, mult(op_a, op_a))), inference(forward_demodulation, [], [f1941, f31])).
fof(f1941, plain, ! [X53] : (mult(mult(X53, op_b), mult(mult(mult(op_a, op_a), op_b), mult(op_a, op_a))) = mult(mult(X53, unit), mult(op_a, op_a))), inference(forward_demodulation, [], [f1855, f33])).
fof(f1855, plain, ! [X53] : (mult(mult(X53, op_b), mult(mult(mult(op_a, op_a), op_b), mult(op_a, op_a))) = mult(mult(X53, mult(op_a, op_b)), mult(op_a, op_a))), inference(superposition, [], [f118, f81])).
fof(f43, plain, (~ (sK0 = mult(mult(sK0, op_b), op_a)) | spl1_2), inference(avatar_component_clause, [], [f41])).
fof(f41, plain, (spl1_2 <=> (sK0 = mult(mult(sK0, op_b), op_a))), introduced(avatar_definition, [new_symbols(naming, [spl1_2])])).
fof(f53132, plain, spl1_1, inference(avatar_contradiction_clause, [], [f53131])).
fof(f53131, plain, ($false | spl1_1), inference(trivial_inequality_removal, [], [f53110])).
fof(f53110, plain, (~ (sK0 = sK0) | spl1_1), inference(superposition, [], [f39, f52789])).
fof(f39, plain, (~ (sK0 = mult(mult(sK0, op_a), op_b)) | spl1_1), inference(avatar_component_clause, [], [f37])).
fof(f37, plain, (spl1_1 <=> (sK0 = mult(mult(sK0, op_a), op_b))), introduced(avatar_definition, [new_symbols(naming, [spl1_1])])).
fof(f44, plain, (~ spl1_1 | ~ spl1_2), inference(avatar_split_clause, [], [f35, f41, f37])).
fof(f35, plain, (~ (sK0 = mult(mult(sK0, op_b), op_a)) | ~ (sK0 = mult(mult(sK0, op_a), op_b))), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (~ (sK0 = mult(mult(sK0, op_b), op_a)) | ~ (sK0 = mult(mult(sK0, op_a), op_b))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK0])], [f21, f22])).
fof(f22, plain, (? [X0] : (~ (mult(mult(X0, op_b), op_a) = X0) | ~ (mult(mult(X0, op_a), op_b) = X0)) => (~ (sK0 = mult(mult(sK0, op_b), op_a)) | ~ (sK0 = mult(mult(sK0, op_a), op_b)))), introduced(choice_axiom, [])).
fof(f21, plain, ? [X0] : (~ (mult(mult(X0, op_b), op_a) = X0) | ~ (mult(mult(X0, op_a), op_b) = X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ~ ! [X0] : ((mult(mult(X0, op_b), op_a) = X0) & (mult(mult(X0, op_a), op_b) = X0)), inference(rectify, [], [f13])).
fof(f13, plain, ~ ! [X3] : ((mult(mult(X3, op_b), op_a) = X3) & (mult(mult(X3, op_a), op_b) = X3)), inference(negated_conjecture, [], [f12])).
fof(f12, plain, ~ ! [X3] : ((mult(mult(X3, op_b), op_a) = X3) & (mult(mult(X3, op_a), op_b) = X3)), file('/home/ubuntu/library/tptp/Problems/GRP/GRP715+1.p', goals)).