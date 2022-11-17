fof(f4485, plain, $false, inference(avatar_sat_refutation, [], [f446, f451, f464, f469, f474, f475, f476, f2764, f3150, f3464, f4484])).
fof(f4484, plain, (~ spl36_3 | ~ spl36_4 | spl36_19), inference(avatar_contradiction_clause, [], [f4483])).
fof(f4483, plain, ($false | (~ spl36_3 | ~ spl36_4 | spl36_19)), inference(subsumption_resolution, [], [f4482, f3539])).
fof(f3539, plain, (~ leq(sK32, n0) | (~ spl36_4 | spl36_19)), inference(subsumption_resolution, [], [f3499, f822])).
fof(f822, plain, (~ (n0 = sK32) | spl36_19), inference(avatar_component_clause, [], [f821])).
fof(f821, plain, (spl36_19 <=> (n0 = sK32)), introduced(avatar_definition, [new_symbols(naming, [spl36_19])])).
fof(f3499, plain, (~ leq(sK32, n0) | (n0 = sK32) | ~ spl36_4), inference(resolution, [], [f450, f382])).
fof(f382, plain, ! [X0] : (~ leq(n0, X0) | ~ leq(X0, n0) | (n0 = X0)), inference(cnf_transformation, [], [f163])).
fof(f163, plain, ! [X0] : ((n0 = X0) | ~ leq(X0, n0) | ~ leq(n0, X0)), inference(flattening, [], [f162])).
fof(f162, plain, ! [X0] : ((n0 = X0) | (~ leq(X0, n0) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : ((leq(X0, n0) & leq(n0, X0)) => (n0 = X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', finite_domain_0)).
fof(f450, plain, (leq(n0, sK32) | ~ spl36_4), inference(avatar_component_clause, [], [f448])).
fof(f448, plain, (spl36_4 <=> leq(n0, sK32)), introduced(avatar_definition, [new_symbols(naming, [spl36_4])])).
fof(f4482, plain, (leq(sK32, n0) | ~ spl36_3), inference(resolution, [], [f568, f3474])).
fof(f3474, plain, (leq(sK32, tptp_minus_1) | ~ spl36_3), inference(forward_demodulation, [], [f445, f537])).
fof(f537, plain, (tptp_minus_1 = minus(n0, n1)), inference(superposition, [], [f407, f397])).
fof(f397, plain, (n0 = plus(tptp_minus_1, n1)), inference(definition_unfolding, [], [f312, f313])).
fof(f313, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', succ_plus_1_r)).
fof(f312, plain, (n0 = succ(tptp_minus_1)), inference(cnf_transformation, [], [f28])).
fof(f28, plain, (n0 = succ(tptp_minus_1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', succ_tptp_minus_1)).
fof(f407, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f324, f323, f313])).
fof(f323, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', pred_minus_1)).
fof(f324, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', pred_succ)).
fof(f445, plain, (leq(sK32, minus(n0, n1)) | ~ spl36_3), inference(avatar_component_clause, [], [f443])).
fof(f443, plain, (spl36_3 <=> leq(sK32, minus(n0, n1))), introduced(avatar_definition, [new_symbols(naming, [spl36_3])])).
fof(f568, plain, ! [X3] : (~ leq(X3, tptp_minus_1) | leq(X3, n0)), inference(superposition, [], [f394, f397])).
fof(f394, plain, ! [X0, X1] : (leq(X0, plus(X1, n1)) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f243, f313])).
fof(f243, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0, X1] : (leq(X0, X1) => leq(X0, succ(X1))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', leq_succ)).
fof(f3464, plain, (~ spl36_3 | ~ spl36_19 | spl36_21), inference(avatar_contradiction_clause, [], [f3463])).
fof(f3463, plain, ($false | (~ spl36_3 | ~ spl36_19 | spl36_21)), inference(subsumption_resolution, [], [f3462, f3324])).
fof(f3324, plain, (~ (n0 = tptp_minus_1) | (~ spl36_19 | spl36_21)), inference(forward_demodulation, [], [f835, f823])).
fof(f823, plain, ((n0 = sK32) | ~ spl36_19), inference(avatar_component_clause, [], [f821])).
fof(f835, plain, (~ (tptp_minus_1 = sK32) | spl36_21), inference(avatar_component_clause, [], [f834])).
fof(f834, plain, (spl36_21 <=> (tptp_minus_1 = sK32)), introduced(avatar_definition, [new_symbols(naming, [spl36_21])])).
fof(f3462, plain, ((n0 = tptp_minus_1) | (~ spl36_3 | ~ spl36_19)), inference(subsumption_resolution, [], [f3428, f501])).
fof(f501, plain, leq(tptp_minus_1, n0), inference(resolution, [], [f238, f358])).
fof(f358, plain, gt(n0, tptp_minus_1), inference(cnf_transformation, [], [f61])).
fof(f61, plain, gt(n0, tptp_minus_1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', gt_0_tptp_minus_1)).
fof(f238, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', leq_gt1)).
fof(f3428, plain, (~ leq(tptp_minus_1, n0) | (n0 = tptp_minus_1) | (~ spl36_3 | ~ spl36_19)), inference(resolution, [], [f3162, f382])).
fof(f3162, plain, (leq(n0, tptp_minus_1) | (~ spl36_3 | ~ spl36_19)), inference(backward_demodulation, [], [f3161, f537])).
fof(f3161, plain, (leq(n0, minus(n0, n1)) | (~ spl36_3 | ~ spl36_19)), inference(forward_demodulation, [], [f445, f823])).
fof(f3150, plain, (~ spl36_19 | ~ spl36_21), inference(avatar_contradiction_clause, [], [f3149])).
fof(f3149, plain, ($false | (~ spl36_19 | ~ spl36_21)), inference(subsumption_resolution, [], [f3098, f235])).
fof(f235, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', irreflexivity_gt)).
fof(f3098, plain, (gt(n0, n0) | (~ spl36_19 | ~ spl36_21)), inference(backward_demodulation, [], [f358, f3093])).
fof(f3093, plain, ((n0 = tptp_minus_1) | (~ spl36_19 | ~ spl36_21)), inference(backward_demodulation, [], [f836, f823])).
fof(f836, plain, ((tptp_minus_1 = sK32) | ~ spl36_21), inference(avatar_component_clause, [], [f834])).
fof(f2764, plain, (spl36_7 | ~ spl36_8 | ~ spl36_9), inference(avatar_contradiction_clause, [], [f2763])).
fof(f2763, plain, ($false | (spl36_7 | ~ spl36_8 | ~ spl36_9)), inference(subsumption_resolution, [], [f542, f1470])).
fof(f1470, plain, (! [X21] : (n1 = sum(n0, n4, a_select3(q, sK34, X21))) | (~ spl36_8 | ~ spl36_9)), inference(subsumption_resolution, [], [f1448, f473])).
fof(f473, plain, (leq(n0, sK34) | ~ spl36_9), inference(avatar_component_clause, [], [f471])).
fof(f471, plain, (spl36_9 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl36_9])])).
fof(f1448, plain, (! [X21] : ((n1 = sum(n0, n4, a_select3(q, sK34, X21))) | ~ leq(n0, sK34)) | ~ spl36_8), inference(resolution, [], [f543, f468])).
fof(f468, plain, (leq(sK34, minus(pv10, n1)) | ~ spl36_8), inference(avatar_component_clause, [], [f466])).
fof(f466, plain, (spl36_8 <=> leq(sK34, minus(pv10, n1))), introduced(avatar_definition, [new_symbols(naming, [spl36_8])])).
fof(f543, plain, ! [X2, X3] : (~ leq(X2, minus(pv10, n1)) | (n1 = sum(n0, n4, a_select3(q, X2, X3))) | ~ leq(n0, X2)), inference(backward_demodulation, [], [f348, f541])).
fof(f541, plain, (n4 = minus(n5, n1)), inference(superposition, [], [f407, f531])).
fof(f531, plain, (n5 = plus(n4, n1)), inference(backward_demodulation, [], [f526, f528])).
fof(f528, plain, (n4 = plus(n1, n3)), inference(superposition, [], [f527, f398])).
fof(f398, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f314, f313])).
fof(f314, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', succ_plus_1_l)).
fof(f527, plain, (n4 = plus(n3, n1)), inference(backward_demodulation, [], [f485, f523])).
fof(f523, plain, (n3 = plus(n1, n2)), inference(superposition, [], [f398, f489])).
fof(f489, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f488, f484])).
fof(f484, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f415, f414])).
fof(f414, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f388, f313])).
fof(f388, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f91])).
fof(f91, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', successor_1)).
fof(f415, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f389, f313, f313])).
fof(f389, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f92])).
fof(f92, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', successor_2)).
fof(f488, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f487, f414])).
fof(f487, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f416, f398])).
fof(f416, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f390, f313, f313, f313])).
fof(f390, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f93])).
fof(f93, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', successor_3)).
fof(f485, plain, (n4 = plus(plus(n1, n2), n1)), inference(backward_demodulation, [], [f483, f484])).
fof(f483, plain, (n4 = plus(plus(n1, plus(n1, n1)), n1)), inference(backward_demodulation, [], [f478, f414])).
fof(f478, plain, (n4 = plus(plus(n1, plus(n1, plus(n0, n1))), n1)), inference(forward_demodulation, [], [f477, f398])).
fof(f477, plain, (n4 = plus(plus(n1, plus(plus(n0, n1), n1)), n1)), inference(forward_demodulation, [], [f412, f398])).
fof(f412, plain, (n4 = plus(plus(plus(plus(n0, n1), n1), n1), n1)), inference(definition_unfolding, [], [f386, f313, f313, f313, f313])).
fof(f386, plain, (n4 = succ(succ(succ(succ(n0))))), inference(cnf_transformation, [], [f89])).
fof(f89, plain, (n4 = succ(succ(succ(succ(n0))))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', successor_4)).
fof(f526, plain, (n5 = plus(plus(n1, n3), n1)), inference(backward_demodulation, [], [f486, f523])).
fof(f486, plain, (n5 = plus(plus(n1, plus(n1, n2)), n1)), inference(backward_demodulation, [], [f482, f484])).
fof(f482, plain, (n5 = plus(plus(n1, plus(n1, plus(n1, n1))), n1)), inference(backward_demodulation, [], [f481, f414])).
fof(f481, plain, (n5 = plus(plus(n1, plus(n1, plus(n1, plus(n0, n1)))), n1)), inference(forward_demodulation, [], [f480, f398])).
fof(f480, plain, (n5 = plus(plus(n1, plus(n1, plus(plus(n0, n1), n1))), n1)), inference(forward_demodulation, [], [f479, f398])).
fof(f479, plain, (n5 = plus(plus(n1, plus(plus(plus(n0, n1), n1), n1)), n1)), inference(forward_demodulation, [], [f413, f398])).
fof(f413, plain, (n5 = plus(plus(plus(plus(plus(n0, n1), n1), n1), n1), n1)), inference(definition_unfolding, [], [f387, f313, f313, f313, f313, f313])).
fof(f387, plain, (n5 = succ(succ(succ(succ(succ(n0)))))), inference(cnf_transformation, [], [f90])).
fof(f90, plain, (n5 = succ(succ(succ(succ(succ(n0)))))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', successor_5)).
fof(f348, plain, ! [X2, X3] : ((n1 = sum(n0, minus(n5, n1), a_select3(q, X2, X3))) | ~ leq(X2, minus(pv10, n1)) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f232])).
fof(f232, plain, (((~ (n1 = sum(n0, minus(n5, n1), a_select3(q, sK34, sK35))) & leq(sK34, minus(pv10, n1)) & leq(n0, sK34)) | sP4 | ~ leq(pv10, minus(n135300, n1)) | ~ leq(n0, pv10)) & ! [X2, X3] : ((n1 = sum(n0, minus(n5, n1), a_select3(q, X2, X3))) | ~ leq(X2, minus(pv10, n1)) | ~ leq(n0, X2)) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34, sK35])], [f230, f231])).
fof(f231, plain, (? [X0, X1] : (~ (n1 = sum(n0, minus(n5, n1), a_select3(q, X0, X1))) & leq(X0, minus(pv10, n1)) & leq(n0, X0)) => (~ (n1 = sum(n0, minus(n5, n1), a_select3(q, sK34, sK35))) & leq(sK34, minus(pv10, n1)) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f230, plain, ((? [X0, X1] : (~ (n1 = sum(n0, minus(n5, n1), a_select3(q, X0, X1))) & leq(X0, minus(pv10, n1)) & leq(n0, X0)) | sP4 | ~ leq(pv10, minus(n135300, n1)) | ~ leq(n0, pv10)) & ! [X2, X3] : ((n1 = sum(n0, minus(n5, n1), a_select3(q, X2, X3))) | ~ leq(X2, minus(pv10, n1)) | ~ leq(n0, X2)) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10)), inference(rectify, [], [f178])).
fof(f178, plain, ((? [X2, X3] : (~ (n1 = sum(n0, minus(n5, n1), a_select3(q, X2, X3))) & leq(X2, minus(pv10, n1)) & leq(n0, X2)) | sP4 | ~ leq(pv10, minus(n135300, n1)) | ~ leq(n0, pv10)) & ! [X0, X1] : ((n1 = sum(n0, minus(n5, n1), a_select3(q, X0, X1))) | ~ leq(X0, minus(pv10, n1)) | ~ leq(n0, X0)) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10)), inference(definition_folding, [], [f157, e177])).
fof(f177, plain, (? [X4, X5] : (~ (a_select3(q, pv10, X4) = divide(sqrt(times(minus(a_select3(center, X4, n0), a_select2(x, pv10)), minus(a_select3(center, X4, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X5, n0), a_select2(x, pv10)), minus(a_select3(center, X5, n0), a_select2(x, pv10))))))) & leq(X4, minus(n0, n1)) & leq(n0, X4)) | ~ sP4), inference(usedef, [], [e177])).
fof(e177, plain, (sP4 <=> ? [X4, X5] : (~ (a_select3(q, pv10, X4) = divide(sqrt(times(minus(a_select3(center, X4, n0), a_select2(x, pv10)), minus(a_select3(center, X4, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X5, n0), a_select2(x, pv10)), minus(a_select3(center, X5, n0), a_select2(x, pv10))))))) & leq(X4, minus(n0, n1)) & leq(n0, X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f157, plain, ((? [X2, X3] : (~ (n1 = sum(n0, minus(n5, n1), a_select3(q, X2, X3))) & leq(X2, minus(pv10, n1)) & leq(n0, X2)) | ? [X4, X5] : (~ (a_select3(q, pv10, X4) = divide(sqrt(times(minus(a_select3(center, X4, n0), a_select2(x, pv10)), minus(a_select3(center, X4, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X5, n0), a_select2(x, pv10)), minus(a_select3(center, X5, n0), a_select2(x, pv10))))))) & leq(X4, minus(n0, n1)) & leq(n0, X4)) | ~ leq(pv10, minus(n135300, n1)) | ~ leq(n0, pv10)) & ! [X0, X1] : ((n1 = sum(n0, minus(n5, n1), a_select3(q, X0, X1))) | ~ leq(X0, minus(pv10, n1)) | ~ leq(n0, X0)) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10)), inference(flattening, [], [f156])).
fof(f156, plain, ((? [X2, X3] : (~ (n1 = sum(n0, minus(n5, n1), a_select3(q, X2, X3))) & (leq(X2, minus(pv10, n1)) & leq(n0, X2))) | ? [X4, X5] : (~ (a_select3(q, pv10, X4) = divide(sqrt(times(minus(a_select3(center, X4, n0), a_select2(x, pv10)), minus(a_select3(center, X4, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X5, n0), a_select2(x, pv10)), minus(a_select3(center, X5, n0), a_select2(x, pv10))))))) & (leq(X4, minus(n0, n1)) & leq(n0, X4))) | ~ leq(pv10, minus(n135300, n1)) | ~ leq(n0, pv10)) & (! [X0, X1] : ((n1 = sum(n0, minus(n5, n1), a_select3(q, X0, X1))) | (~ leq(X0, minus(pv10, n1)) | ~ leq(n0, X0))) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10))), inference(ennf_transformation, [], [f114])).
fof(f114, plain, ~ ((! [X0, X1] : ((leq(X0, minus(pv10, n1)) & leq(n0, X0)) => (n1 = sum(n0, minus(n5, n1), a_select3(q, X0, X1)))) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10)) => (! [X2, X3] : ((leq(X2, minus(pv10, n1)) & leq(n0, X2)) => (n1 = sum(n0, minus(n5, n1), a_select3(q, X2, X3)))) & ! [X4, X5] : ((leq(X4, minus(n0, n1)) & leq(n0, X4)) => (a_select3(q, pv10, X4) = divide(sqrt(times(minus(a_select3(center, X4, n0), a_select2(x, pv10)), minus(a_select3(center, X4, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X5, n0), a_select2(x, pv10)), minus(a_select3(center, X5, n0), a_select2(x, pv10)))))))) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10))), inference(rectify, [], [f54])).
fof(f54, plain, ~ ((! [X13, X17] : ((leq(X13, minus(pv10, n1)) & leq(n0, X13)) => (n1 = sum(n0, minus(n5, n1), a_select3(q, X13, X17)))) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10)) => (! [X20, X21] : ((leq(X20, minus(pv10, n1)) & leq(n0, X20)) => (n1 = sum(n0, minus(n5, n1), a_select3(q, X20, X21)))) & ! [X3, X19] : ((leq(X3, minus(n0, n1)) & leq(n0, X3)) => (a_select3(q, pv10, X3) = divide(sqrt(times(minus(a_select3(center, X3, n0), a_select2(x, pv10)), minus(a_select3(center, X3, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X19, n0), a_select2(x, pv10)), minus(a_select3(center, X19, n0), a_select2(x, pv10)))))))) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ ((! [X13, X17] : ((leq(X13, minus(pv10, n1)) & leq(n0, X13)) => (n1 = sum(n0, minus(n5, n1), a_select3(q, X13, X17)))) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10)) => (! [X20, X21] : ((leq(X20, minus(pv10, n1)) & leq(n0, X20)) => (n1 = sum(n0, minus(n5, n1), a_select3(q, X20, X21)))) & ! [X3, X19] : ((leq(X3, minus(n0, n1)) & leq(n0, X3)) => (a_select3(q, pv10, X3) = divide(sqrt(times(minus(a_select3(center, X3, n0), a_select2(x, pv10)), minus(a_select3(center, X3, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X19, n0), a_select2(x, pv10)), minus(a_select3(center, X19, n0), a_select2(x, pv10)))))))) & leq(pv10, minus(n135300, n1)) & leq(n0, pv10))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV055+1.p', cl5_nebula_norm_0037)).
fof(f542, plain, (~ (n1 = sum(n0, n4, a_select3(q, sK34, sK35))) | spl36_7), inference(backward_demodulation, [], [f463, f541])).
fof(f463, plain, (~ (n1 = sum(n0, minus(n5, n1), a_select3(q, sK34, sK35))) | spl36_7), inference(avatar_component_clause, [], [f461])).
fof(f461, plain, (spl36_7 <=> (n1 = sum(n0, minus(n5, n1), a_select3(q, sK34, sK35)))), introduced(avatar_definition, [new_symbols(naming, [spl36_7])])).
fof(f476, plain, spl36_5, inference(avatar_split_clause, [], [f346, f453])).
fof(f453, plain, (spl36_5 <=> leq(n0, pv10)), introduced(avatar_definition, [new_symbols(naming, [spl36_5])])).
fof(f346, plain, leq(n0, pv10), inference(cnf_transformation, [], [f232])).
fof(f475, plain, spl36_6, inference(avatar_split_clause, [], [f347, f457])).
fof(f457, plain, (spl36_6 <=> leq(pv10, minus(n135300, n1))), introduced(avatar_definition, [new_symbols(naming, [spl36_6])])).
fof(f347, plain, leq(pv10, minus(n135300, n1)), inference(cnf_transformation, [], [f232])).
fof(f474, plain, (~ spl36_5 | ~ spl36_6 | spl36_1 | spl36_9), inference(avatar_split_clause, [], [f349, f471, f434, f457, f453])).
fof(f434, plain, (spl36_1 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl36_1])])).
fof(f349, plain, (leq(n0, sK34) | sP4 | ~ leq(pv10, minus(n135300, n1)) | ~ leq(n0, pv10)), inference(cnf_transformation, [], [f232])).
fof(f469, plain, (~ spl36_5 | ~ spl36_6 | spl36_1 | spl36_8), inference(avatar_split_clause, [], [f350, f466, f434, f457, f453])).
fof(f350, plain, (leq(sK34, minus(pv10, n1)) | sP4 | ~ leq(pv10, minus(n135300, n1)) | ~ leq(n0, pv10)), inference(cnf_transformation, [], [f232])).
fof(f464, plain, (~ spl36_5 | ~ spl36_6 | spl36_1 | ~ spl36_7), inference(avatar_split_clause, [], [f351, f461, f434, f457, f453])).
fof(f351, plain, (~ (n1 = sum(n0, minus(n5, n1), a_select3(q, sK34, sK35))) | sP4 | ~ leq(pv10, minus(n135300, n1)) | ~ leq(n0, pv10)), inference(cnf_transformation, [], [f232])).
fof(f451, plain, (~ spl36_1 | spl36_4), inference(avatar_split_clause, [], [f343, f448, f434])).
fof(f343, plain, (leq(n0, sK32) | ~ sP4), inference(cnf_transformation, [], [f229])).
fof(f229, plain, ((~ (a_select3(q, pv10, sK32) = divide(sqrt(times(minus(a_select3(center, sK32, n0), a_select2(x, pv10)), minus(a_select3(center, sK32, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, sK33, n0), a_select2(x, pv10)), minus(a_select3(center, sK33, n0), a_select2(x, pv10))))))) & leq(sK32, minus(n0, n1)) & leq(n0, sK32)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK32, sK33])], [f227, f228])).
fof(f228, plain, (? [X0, X1] : (~ (a_select3(q, pv10, X0) = divide(sqrt(times(minus(a_select3(center, X0, n0), a_select2(x, pv10)), minus(a_select3(center, X0, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X1, n0), a_select2(x, pv10)), minus(a_select3(center, X1, n0), a_select2(x, pv10))))))) & leq(X0, minus(n0, n1)) & leq(n0, X0)) => (~ (a_select3(q, pv10, sK32) = divide(sqrt(times(minus(a_select3(center, sK32, n0), a_select2(x, pv10)), minus(a_select3(center, sK32, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, sK33, n0), a_select2(x, pv10)), minus(a_select3(center, sK33, n0), a_select2(x, pv10))))))) & leq(sK32, minus(n0, n1)) & leq(n0, sK32))), introduced(choice_axiom, [])).
fof(f227, plain, (? [X0, X1] : (~ (a_select3(q, pv10, X0) = divide(sqrt(times(minus(a_select3(center, X0, n0), a_select2(x, pv10)), minus(a_select3(center, X0, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X1, n0), a_select2(x, pv10)), minus(a_select3(center, X1, n0), a_select2(x, pv10))))))) & leq(X0, minus(n0, n1)) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f226])).
fof(f226, plain, (? [X4, X5] : (~ (a_select3(q, pv10, X4) = divide(sqrt(times(minus(a_select3(center, X4, n0), a_select2(x, pv10)), minus(a_select3(center, X4, n0), a_select2(x, pv10)))), sum(n0, minus(n5, n1), sqrt(times(minus(a_select3(center, X5, n0), a_select2(x, pv10)), minus(a_select3(center, X5, n0), a_select2(x, pv10))))))) & leq(X4, minus(n0, n1)) & leq(n0, X4)) | ~ sP4), inference(nnf_transformation, [], [f177])).
fof(f446, plain, (~ spl36_1 | spl36_3), inference(avatar_split_clause, [], [f344, f443, f434])).
fof(f344, plain, (leq(sK32, minus(n0, n1)) | ~ sP4), inference(cnf_transformation, [], [f229])).