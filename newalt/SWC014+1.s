fof(f1070808, plain, $false, inference(avatar_sat_refutation, [], [f613, f619, f1399, f2637, f3138, f4301, f9966, f799860, f1070807])).
fof(f1070807, plain, (~ spl60_1 | ~ spl60_70 | ~ spl60_152 | ~ spl60_392 | ~ spl60_18739), inference(avatar_contradiction_clause, [], [f1070806])).
fof(f1070806, plain, ($false | (~ spl60_1 | ~ spl60_70 | ~ spl60_152 | ~ spl60_392 | ~ spl60_18739)), inference(resolution, [], [f1070731, f622])).
fof(f622, plain, (sP6(sK57, sK56, sK57, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f621, f565])).
fof(f565, plain, (sK57 = sK59), inference(cnf_transformation, [], [f355])).
fof(f355, plain, ((((((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK59, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK56, sK57, sK58, sK59])], [f234, f354, f353, f352, f351])).
fof(f351, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, X0)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, sK56)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, sK56)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, X2, sK57, sK56)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, X2, sK57, sK56)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) => (((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK59, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, X0)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X3, X2, X1, X0] : ((? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ neq(nil, X1) | ~ (app(X0, X7) = X6) | ~ (tl(X1) = X7) | ~ ssList(X7)) | ~ (X1 = X6) | ~ ssList(X6)) & neq(X1, nil)) | ~ sP6(X3, X2, X1, X0)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2, X1, X0] : (sP6(X3, X2, X1, X0) <=> (? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ neq(nil, X1) | ~ (app(X0, X7) = X6) | ~ (tl(X1) = X7) | ~ ssList(X7)) | ~ (X1 = X6) | ~ ssList(X6)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ neq(nil, X1) | ~ (app(X0, X7) = X6) | ~ (tl(X1) = X7) | ~ ssList(X7)) | ~ (X1 = X6) | ~ ssList(X6)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : (((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2)) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ neq(nil, X1) | ~ (app(X0, X7) = X6) | ~ (tl(X1) = X7) | ~ ssList(X7)) | ~ (X1 = X6) | ~ ssList(X6)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(cons(X4, nil), X5) = X3) | ~ (cons(X4, nil) = X2)))) | ? [X6] : (? [X7] : (neq(nil, X1) & (app(X0, X7) = X6) & (tl(X1) = X7) & ssList(X7)) & (X1 = X6) & ssList(X6)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X6] : (ssItem(X6) => ! [X7] : (ssList(X7) => (~ (app(cons(X6, nil), X7) = X3) | ~ (cons(X6, nil) = X2)))) | ? [X4] : (? [X5] : (neq(nil, X1) & (app(X0, X5) = X4) & (tl(X1) = X5) & ssList(X5)) & (X1 = X4) & ssList(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X6] : (ssItem(X6) => ! [X7] : (ssList(X7) => (~ (app(cons(X6, nil), X7) = X3) | ~ (cons(X6, nil) = X2)))) | ? [X4] : (? [X5] : (neq(nil, X1) & (app(X0, X5) = X4) & (tl(X1) = X5) & ssList(X5)) & (X1 = X4) & ssList(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', co1)).
fof(f621, plain, (sP6(sK59, sK56, sK57, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f608, f566])).
fof(f566, plain, (sK56 = sK58), inference(cnf_transformation, [], [f355])).
fof(f608, plain, (sP6(sK59, sK58, sK57, sK56) | ~ spl60_1), inference(avatar_component_clause, [], [f606])).
fof(f606, plain, (spl60_1 <=> sP6(sK59, sK58, sK57, sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_1])])).
fof(f1070731, plain, (! [X8, X7] : ~ sP6(X7, X8, sK57, sK56) | (~ spl60_1 | ~ spl60_70 | ~ spl60_152 | ~ spl60_392 | ~ spl60_18739)), inference(subsumption_resolution, [], [f1070730, f794588])).
fof(f794588, plain, (ssList(sK57) | ~ spl60_18739), inference(avatar_component_clause, [], [f794587])).
fof(f794587, plain, (spl60_18739 <=> ssList(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_18739])])).
fof(f1070730, plain, (! [X8, X7] : (~ ssList(sK57) | ~ sP6(X7, X8, sK57, sK56)) | (~ spl60_1 | ~ spl60_70 | ~ spl60_152 | ~ spl60_392)), inference(subsumption_resolution, [], [f1070729, f9916])).
fof(f9916, plain, (neq(nil, sK57) | ~ spl60_392), inference(avatar_component_clause, [], [f9915])).
fof(f9915, plain, (spl60_392 <=> neq(nil, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_392])])).
fof(f1070729, plain, (! [X8, X7] : (~ neq(nil, sK57) | ~ ssList(sK57) | ~ sP6(X7, X8, sK57, sK56)) | (~ spl60_1 | ~ spl60_70 | ~ spl60_152)), inference(subsumption_resolution, [], [f1070728, f2533])).
fof(f2533, plain, (ssList(tl(sK57)) | ~ spl60_70), inference(avatar_component_clause, [], [f2532])).
fof(f2532, plain, (spl60_70 <=> ssList(tl(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl60_70])])).
fof(f1070728, plain, (! [X8, X7] : (~ ssList(tl(sK57)) | ~ neq(nil, sK57) | ~ ssList(sK57) | ~ sP6(X7, X8, sK57, sK56)) | (~ spl60_1 | ~ spl60_152)), inference(trivial_inequality_removal, [], [f1070546])).
fof(f1070546, plain, (! [X8, X7] : (~ (sK57 = sK57) | ~ ssList(tl(sK57)) | ~ neq(nil, sK57) | ~ ssList(sK57) | ~ sP6(X7, X8, sK57, sK56)) | (~ spl60_1 | ~ spl60_152)), inference(superposition, [], [f597, f1069088])).
fof(f1069088, plain, ((sK57 = app(sK56, tl(sK57))) | (~ spl60_1 | ~ spl60_152)), inference(backward_demodulation, [], [f1001556, f1066757])).
fof(f1066757, plain, ((sK55(sK57, sK56) = tl(sK57)) | (~ spl60_1 | ~ spl60_152)), inference(backward_demodulation, [], [f1044840, f1066755])).
fof(f1066755, plain, ((sK57 = cons(hd(sK56), sK55(sK57, sK56))) | (~ spl60_1 | ~ spl60_152)), inference(forward_demodulation, [], [f1066754, f1001556])).
fof(f1066754, plain, ((app(sK56, sK55(sK57, sK56)) = cons(hd(sK56), sK55(sK57, sK56))) | (~ spl60_1 | ~ spl60_152)), inference(forward_demodulation, [], [f1066729, f1001293])).
fof(f1001293, plain, ((sK56 = cons(hd(sK56), nil)) | ~ spl60_1), inference(backward_demodulation, [], [f814806, f4677])).
fof(f4677, plain, ((sK54(sK57, sK56) = hd(sK56)) | ~ spl60_1), inference(forward_demodulation, [], [f4676, f3233])).
fof(f3233, plain, ((sK56 = cons(sK54(sK57, sK56), nil)) | ~ spl60_1), inference(resolution, [], [f622, f559])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f350])).
fof(f350, plain, ! [X0, X1, X2, X3] : (((((app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) & ! [X6] : (! [X7] : (~ neq(nil, X2) | ~ (app(X3, X7) = X6) | ~ (tl(X2) = X7) | ~ ssList(X7)) | ~ (X2 = X6) | ~ ssList(X6)) & neq(X2, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55])], [f347, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X0) & (cons(X4, nil) = X1) & ssList(X5)) & ssItem(X4)) => (? [X5] : ((app(cons(sK54(X0, X1), nil), X5) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X5)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X5] : ((app(cons(sK54(X0, X1), nil), X5) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X5)) => ((app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1, X2, X3] : ((? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X0) & (cons(X4, nil) = X1) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ neq(nil, X2) | ~ (app(X3, X7) = X6) | ~ (tl(X2) = X7) | ~ ssList(X7)) | ~ (X2 = X6) | ~ ssList(X6)) & neq(X2, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2, X1, X0] : ((? [X4] : (? [X5] : ((app(cons(X4, nil), X5) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ neq(nil, X1) | ~ (app(X0, X7) = X6) | ~ (tl(X1) = X7) | ~ ssList(X7)) | ~ (X1 = X6) | ~ ssList(X6)) & neq(X1, nil)) | ~ sP6(X3, X2, X1, X0)), inference(nnf_transformation, [], [f233])).
fof(f4676, plain, ((sK54(sK57, sK56) = hd(cons(sK54(sK57, sK56), nil))) | ~ spl60_1), inference(resolution, [], [f1052, f989])).
fof(f989, plain, (ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f557, f622])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X0, X1))), inference(cnf_transformation, [], [f350])).
fof(f1052, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f457, f448])).
fof(f448, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', ax17)).
fof(f457, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', ax23)).
fof(f814806, plain, ((sK56 = cons(sK54(sK57, sK56), nil)) | ~ spl60_1), inference(resolution, [], [f622, f559])).
fof(f1066729, plain, ((app(cons(hd(sK56), nil), sK55(sK57, sK56)) = cons(hd(sK56), sK55(sK57, sK56))) | (~ spl60_1 | ~ spl60_152)), inference(resolution, [], [f3258, f3594])).
fof(f3594, plain, (ssItem(hd(sK56)) | ~ spl60_152), inference(avatar_component_clause, [], [f3593])).
fof(f3593, plain, (spl60_152 <=> ssItem(hd(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_152])])).
fof(f3258, plain, (! [X4] : (~ ssItem(X4) | (cons(X4, sK55(sK57, sK56)) = app(cons(X4, nil), sK55(sK57, sK56)))) | ~ spl60_1), inference(resolution, [], [f990, f536])).
fof(f536, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', ax81)).
fof(f990, plain, (ssList(sK55(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f558, f622])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK55(X0, X1))), inference(cnf_transformation, [], [f350])).
fof(f1044840, plain, ((sK55(sK57, sK56) = tl(cons(hd(sK56), sK55(sK57, sK56)))) | (~ spl60_1 | ~ spl60_152)), inference(resolution, [], [f1105, f3594])).
fof(f1105, plain, (! [X53] : (~ ssItem(X53) | (sK55(sK57, sK56) = tl(cons(X53, sK55(sK57, sK56))))) | ~ spl60_1), inference(resolution, [], [f459, f990])).
fof(f459, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', ax25)).
fof(f1001556, plain, ((sK57 = app(sK56, sK55(sK57, sK56))) | ~ spl60_1), inference(forward_demodulation, [], [f1001555, f1001293])).
fof(f1001555, plain, ((sK57 = app(cons(hd(sK56), nil), sK55(sK57, sK56))) | ~ spl60_1), inference(forward_demodulation, [], [f43483, f4677])).
fof(f43483, plain, ((sK57 = app(cons(sK54(sK57, sK56), nil), sK55(sK57, sK56))) | ~ spl60_1), inference(resolution, [], [f622, f560])).
fof(f560, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X0)), inference(cnf_transformation, [], [f350])).
fof(f597, plain, ! [X2, X0, X3, X1] : (~ (app(X3, tl(X2)) = X2) | ~ ssList(tl(X2)) | ~ neq(nil, X2) | ~ ssList(app(X3, tl(X2))) | ~ sP6(X0, X1, X2, X3)), inference(equality_resolution, [], [f596])).
fof(f596, plain, ! [X2, X0, X7, X3, X1] : (~ neq(nil, X2) | ~ (tl(X2) = X7) | ~ ssList(X7) | ~ (app(X3, X7) = X2) | ~ ssList(app(X3, X7)) | ~ sP6(X0, X1, X2, X3)), inference(equality_resolution, [], [f556])).
fof(f556, plain, ! [X6, X2, X0, X7, X3, X1] : (~ neq(nil, X2) | ~ (app(X3, X7) = X6) | ~ (tl(X2) = X7) | ~ ssList(X7) | ~ (X2 = X6) | ~ ssList(X6) | ~ sP6(X0, X1, X2, X3)), inference(cnf_transformation, [], [f350])).
fof(f799860, plain, spl60_18739, inference(avatar_split_clause, [], [f615, f794587])).
fof(f615, plain, ssList(sK57), inference(forward_demodulation, [], [f564, f565])).
fof(f564, plain, ssList(sK59), inference(cnf_transformation, [], [f355])).
fof(f9966, plain, (spl60_32 | spl60_392), inference(avatar_contradiction_clause, [], [f9965])).
fof(f9965, plain, ($false | (spl60_32 | spl60_392)), inference(subsumption_resolution, [], [f9964, f448])).
fof(f9964, plain, (~ ssList(nil) | (spl60_32 | spl60_392)), inference(subsumption_resolution, [], [f9963, f615])).
fof(f9963, plain, (~ ssList(sK57) | ~ ssList(nil) | (spl60_32 | spl60_392)), inference(subsumption_resolution, [], [f9961, f1174])).
fof(f1174, plain, (~ (nil = sK57) | spl60_32), inference(avatar_component_clause, [], [f1173])).
fof(f1173, plain, (spl60_32 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_32])])).
fof(f9961, plain, ((nil = sK57) | ~ ssList(sK57) | ~ ssList(nil) | spl60_392), inference(resolution, [], [f9917, f446])).
fof(f446, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', ax15)).
fof(f9917, plain, (~ neq(nil, sK57) | spl60_392), inference(avatar_component_clause, [], [f9915])).
fof(f4301, plain, (spl60_30 | spl60_152), inference(avatar_contradiction_clause, [], [f4300])).
fof(f4300, plain, ($false | (spl60_30 | spl60_152)), inference(subsumption_resolution, [], [f4299, f616])).
fof(f616, plain, ssList(sK56), inference(forward_demodulation, [], [f563, f566])).
fof(f563, plain, ssList(sK58), inference(cnf_transformation, [], [f355])).
fof(f4299, plain, (~ ssList(sK56) | (spl60_30 | spl60_152)), inference(subsumption_resolution, [], [f4298, f1165])).
fof(f1165, plain, (~ (nil = sK56) | spl60_30), inference(avatar_component_clause, [], [f1164])).
fof(f1164, plain, (spl60_30 <=> (nil = sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_30])])).
fof(f4298, plain, ((nil = sK56) | ~ ssList(sK56) | spl60_152), inference(resolution, [], [f3595, f456])).
fof(f456, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', ax22)).
fof(f3595, plain, (~ ssItem(hd(sK56)) | spl60_152), inference(avatar_component_clause, [], [f3593])).
fof(f3138, plain, (~ spl60_1 | ~ spl60_30), inference(avatar_contradiction_clause, [], [f3137])).
fof(f3137, plain, ($false | (~ spl60_1 | ~ spl60_30)), inference(subsumption_resolution, [], [f3136, f448])).
fof(f3136, plain, (~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(subsumption_resolution, [], [f3124, f1426])).
fof(f1426, plain, (ssItem(sK54(sK57, nil)) | (~ spl60_1 | ~ spl60_30)), inference(forward_demodulation, [], [f989, f1166])).
fof(f1166, plain, ((nil = sK56) | ~ spl60_30), inference(avatar_component_clause, [], [f1164])).
fof(f3124, plain, (~ ssItem(sK54(sK57, nil)) | ~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(trivial_inequality_removal, [], [f3096])).
fof(f3096, plain, (~ (nil = nil) | ~ ssItem(sK54(sK57, nil)) | ~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(superposition, [], [f455, f1591])).
fof(f1591, plain, ((nil = cons(sK54(sK57, nil), nil)) | (~ spl60_1 | ~ spl60_30)), inference(resolution, [], [f559, f1427])).
fof(f1427, plain, (sP6(sK57, nil, sK57, nil) | (~ spl60_1 | ~ spl60_30)), inference(forward_demodulation, [], [f622, f1166])).
fof(f455, plain, ! [X0, X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : (! [X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (nil = cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', ax21)).
fof(f2637, plain, (spl60_32 | spl60_70), inference(avatar_contradiction_clause, [], [f2636])).
fof(f2636, plain, ($false | (spl60_32 | spl60_70)), inference(subsumption_resolution, [], [f2635, f615])).
fof(f2635, plain, (~ ssList(sK57) | (spl60_32 | spl60_70)), inference(subsumption_resolution, [], [f2634, f1174])).
fof(f2634, plain, ((nil = sK57) | ~ ssList(sK57) | spl60_70), inference(resolution, [], [f2534, f458])).
fof(f458, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC014+1.p', ax24)).
fof(f2534, plain, (~ ssList(tl(sK57)) | spl60_70), inference(avatar_component_clause, [], [f2532])).
fof(f1399, plain, ~ spl60_32, inference(avatar_contradiction_clause, [], [f1398])).
fof(f1398, plain, ($false | ~ spl60_32), inference(subsumption_resolution, [], [f1397, f448])).
fof(f1397, plain, (~ ssList(nil) | ~ spl60_32), inference(resolution, [], [f1224, f602])).
fof(f602, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f583])).
fof(f583, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1) | ~ ssList(X1)), inference(equality_resolution, [], [f445])).
fof(f445, plain, ! [X0, X1] : (~ (X0 = X1) | ~ neq(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f1224, plain, (neq(nil, nil) | ~ spl60_32), inference(backward_demodulation, [], [f614, f1175])).
fof(f1175, plain, ((nil = sK57) | ~ spl60_32), inference(avatar_component_clause, [], [f1173])).
fof(f614, plain, neq(sK57, nil), inference(subsumption_resolution, [], [f567, f555])).
fof(f555, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X2, nil)), inference(cnf_transformation, [], [f350])).
fof(f567, plain, (neq(sK57, nil) | sP6(sK59, sK58, sK57, sK56)), inference(cnf_transformation, [], [f355])).
fof(f619, plain, spl60_2, inference(avatar_contradiction_clause, [], [f618])).
fof(f618, plain, ($false | spl60_2), inference(subsumption_resolution, [], [f617, f614])).
fof(f617, plain, (~ neq(sK57, nil) | spl60_2), inference(forward_demodulation, [], [f612, f565])).
fof(f612, plain, (~ neq(sK59, nil) | spl60_2), inference(avatar_component_clause, [], [f610])).
fof(f610, plain, (spl60_2 <=> neq(sK59, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_2])])).
fof(f613, plain, (spl60_1 | ~ spl60_2), inference(avatar_split_clause, [], [f568, f610, f606])).
fof(f568, plain, (~ neq(sK59, nil) | sP6(sK59, sK58, sK57, sK56)), inference(cnf_transformation, [], [f355])).