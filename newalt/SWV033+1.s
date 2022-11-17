fof(f304036, plain, $false, inference(avatar_sat_refutation, [], [f435, f440, f445, f450, f455, f460, f465, f470, f814, f934, f1311, f1316, f1332, f1436, f1504, f1540, f1606, f1611, f1675, f1678, f1738, f1741, f1854, f1891, f301495, f301565, f301816, f302057, f302621, f302693, f302726, f302915, f302993, f303007, f303016, f303222, f303358, f303447, f303764, f303938, f303978])).
fof(f303978, plain, (~ spl35_13 | ~ spl35_42 | ~ spl35_48 | spl35_49), inference(avatar_contradiction_clause, [], [f303977])).
fof(f303977, plain, ($false | (~ spl35_13 | ~ spl35_42 | ~ spl35_48 | spl35_49)), inference(subsumption_resolution, [], [f303976, f303457])).
fof(f303457, plain, (~ (init = a_select2(s_values7_init, n0)) | (~ spl35_13 | spl35_49)), inference(backward_demodulation, [], [f1331, f795])).
fof(f795, plain, ((n0 = sK34) | ~ spl35_13), inference(avatar_component_clause, [], [f793])).
fof(f793, plain, (spl35_13 <=> (n0 = sK34)), introduced(avatar_definition, [new_symbols(naming, [spl35_13])])).
fof(f1331, plain, (~ (init = a_select2(s_values7_init, sK34)) | spl35_49), inference(avatar_component_clause, [], [f1329])).
fof(f1329, plain, (spl35_49 <=> (init = a_select2(s_values7_init, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl35_49])])).
fof(f303976, plain, ((init = a_select2(s_values7_init, n0)) | (~ spl35_42 | ~ spl35_48)), inference(forward_demodulation, [], [f303975, f19026])).
fof(f19026, plain, ! [X0] : (n0 = uniform_int_rnd(X0, n0)), inference(subsumption_resolution, [], [f19021, f230])).
fof(f230, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', reflexivity_leq)).
fof(f19021, plain, ! [X0] : ((n0 = uniform_int_rnd(X0, n0)) | ~ leq(n0, n0)), inference(resolution, [], [f869, f13167])).
fof(f13167, plain, ! [X0] : leq(uniform_int_rnd(X0, n0), n0), inference(resolution, [], [f13148, f552])).
fof(f552, plain, ! [X2] : (~ gt(n1, X2) | leq(X2, n0)), inference(superposition, [], [f382, f523])).
fof(f523, plain, (n0 = minus(n1, n1)), inference(superposition, [], [f398, f405])).
fof(f405, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f379, f307])).
fof(f307, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', succ_plus_1_r)).
fof(f379, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', successor_1)).
fof(f398, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f318, f317, f307])).
fof(f317, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', pred_minus_1)).
fof(f318, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', pred_succ)).
fof(f382, plain, ! [X0, X1] : (leq(X0, minus(X1, n1)) | ~ gt(X1, X0)), inference(definition_unfolding, [], [f235, f317])).
fof(f235, plain, ! [X0, X1] : (leq(X0, pred(X1)) | ~ gt(X1, X0)), inference(cnf_transformation, [], [f172])).
fof(f172, plain, ! [X0, X1] : ((leq(X0, pred(X1)) | ~ gt(X1, X0)) & (gt(X1, X0) | ~ leq(X0, pred(X1)))), inference(nnf_transformation, [], [f10])).
fof(f10, plain, ! [X0, X1] : (leq(X0, pred(X1)) <=> gt(X1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', leq_gt_pred)).
fof(f13148, plain, ! [X1] : gt(n1, uniform_int_rnd(X1, n0)), inference(subsumption_resolution, [], [f13135, f230])).
fof(f13135, plain, ! [X1] : (gt(n1, uniform_int_rnd(X1, n0)) | ~ leq(n0, n0)), inference(resolution, [], [f568, f240])).
fof(f240, plain, ! [X0, X1] : (leq(uniform_int_rnd(X1, X0), X0) | ~ leq(n0, X0)), inference(cnf_transformation, [], [f117])).
fof(f117, plain, ! [X0, X1] : (leq(uniform_int_rnd(X1, X0), X0) | ~ leq(n0, X0)), inference(ennf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1] : (leq(n0, X0) => leq(uniform_int_rnd(X1, X0), X0)), inference(rectify, [], [f14])).
fof(f14, plain, ! [X0, X3] : (leq(n0, X0) => leq(uniform_int_rnd(X3, X0), X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', uniform_int_rand_ranges_hi)).
fof(f568, plain, ! [X2] : (~ leq(X2, n0) | gt(n1, X2)), inference(superposition, [], [f383, f523])).
fof(f383, plain, ! [X0, X1] : (~ leq(X0, minus(X1, n1)) | gt(X1, X0)), inference(definition_unfolding, [], [f234, f317])).
fof(f234, plain, ! [X0, X1] : (gt(X1, X0) | ~ leq(X0, pred(X1))), inference(cnf_transformation, [], [f172])).
fof(f869, plain, ! [X0, X1] : (~ leq(uniform_int_rnd(X0, X1), n0) | (n0 = uniform_int_rnd(X0, X1)) | ~ leq(n0, X1)), inference(resolution, [], [f373, f241])).
fof(f241, plain, ! [X0, X1] : (leq(n0, uniform_int_rnd(X1, X0)) | ~ leq(n0, X0)), inference(cnf_transformation, [], [f118])).
fof(f118, plain, ! [X0, X1] : (leq(n0, uniform_int_rnd(X1, X0)) | ~ leq(n0, X0)), inference(ennf_transformation, [], [f88])).
fof(f88, plain, ! [X0, X1] : (leq(n0, X0) => leq(n0, uniform_int_rnd(X1, X0))), inference(rectify, [], [f15])).
fof(f15, plain, ! [X0, X3] : (leq(n0, X0) => leq(n0, uniform_int_rnd(X3, X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', uniform_int_rand_ranges_lo)).
fof(f373, plain, ! [X0] : (~ leq(n0, X0) | ~ leq(X0, n0) | (n0 = X0)), inference(cnf_transformation, [], [f156])).
fof(f156, plain, ! [X0] : ((n0 = X0) | ~ leq(X0, n0) | ~ leq(n0, X0)), inference(flattening, [], [f155])).
fof(f155, plain, ! [X0] : ((n0 = X0) | (~ leq(X0, n0) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : ((leq(X0, n0) & leq(n0, X0)) => (n0 = X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', finite_domain_0)).
fof(f303975, plain, (! [X1] : (init = a_select2(s_values7_init, uniform_int_rnd(X1, n0))) | (~ spl35_42 | ~ spl35_48)), inference(forward_demodulation, [], [f303953, f523])).
fof(f303953, plain, (! [X1] : (init = a_select2(s_values7_init, uniform_int_rnd(X1, minus(n1, n1)))) | (~ spl35_42 | ~ spl35_48)), inference(backward_demodulation, [], [f1315, f1277])).
fof(f1277, plain, ((n1 = pv1376) | ~ spl35_42), inference(avatar_component_clause, [], [f1275])).
fof(f1275, plain, (spl35_42 <=> (n1 = pv1376)), introduced(avatar_definition, [new_symbols(naming, [spl35_42])])).
fof(f1315, plain, (! [X1] : (init = a_select2(s_values7_init, uniform_int_rnd(X1, minus(pv1376, n1)))) | ~ spl35_48), inference(avatar_component_clause, [], [f1314])).
fof(f1314, plain, (spl35_48 <=> ! [X1] : (init = a_select2(s_values7_init, uniform_int_rnd(X1, minus(pv1376, n1))))), introduced(avatar_definition, [new_symbols(naming, [spl35_48])])).
fof(f303938, plain, (~ spl35_13 | spl35_49 | ~ spl35_54), inference(avatar_contradiction_clause, [], [f303937])).
fof(f303937, plain, ($false | (~ spl35_13 | spl35_49 | ~ spl35_54)), inference(subsumption_resolution, [], [f303936, f490])).
fof(f490, plain, leq(n0, n1), inference(resolution, [], [f232, f359])).
fof(f359, plain, gt(n1, n0), inference(cnf_transformation, [], [f64])).
fof(f64, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', gt_1_0)).
fof(f232, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', leq_gt1)).
fof(f303936, plain, (~ leq(n0, n1) | (~ spl35_13 | spl35_49 | ~ spl35_54)), inference(subsumption_resolution, [], [f303874, f303457])).
fof(f303874, plain, ((init = a_select2(s_values7_init, n0)) | ~ leq(n0, n1) | ~ spl35_54), inference(resolution, [], [f303802, f230])).
fof(f303802, plain, (! [X1] : (~ leq(n0, X1) | (init = a_select2(s_values7_init, X1)) | ~ leq(X1, n1)) | ~ spl35_54), inference(forward_demodulation, [], [f303778, f525])).
fof(f525, plain, (n1 = minus(n2, n1)), inference(superposition, [], [f398, f478])).
fof(f478, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f406, f405])).
fof(f406, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f380, f307, f307])).
fof(f380, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', successor_2)).
fof(f303778, plain, (! [X1] : (~ leq(X1, minus(n2, n1)) | (init = a_select2(s_values7_init, X1)) | ~ leq(n0, X1)) | ~ spl35_54), inference(backward_demodulation, [], [f346, f1422])).
fof(f1422, plain, ((n2 = pv1376) | ~ spl35_54), inference(avatar_component_clause, [], [f1420])).
fof(f1420, plain, (spl35_54 <=> (n2 = pv1376)), introduced(avatar_definition, [new_symbols(naming, [spl35_54])])).
fof(f346, plain, ! [X1] : (~ leq(X1, minus(pv1376, n1)) | (init = a_select2(s_values7_init, X1)) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f226])).
fof(f226, plain, (((~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK34)) & leq(sK34, minus(plus(n1, pv1376), n1)) & leq(n0, sK34)) | sP4 | ~ (init = init)) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | ~ leq(X1, minus(pv1376, n1)) | ~ leq(n0, X1)) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & leq(pv1376, n3) & leq(n0, pv1376) & (init = init)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34])], [f224, f225])).
fof(f225, plain, (? [X0] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X0)) & leq(X0, minus(plus(n1, pv1376), n1)) & leq(n0, X0)) => (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK34)) & leq(sK34, minus(plus(n1, pv1376), n1)) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f224, plain, ((? [X0] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X0)) & leq(X0, minus(plus(n1, pv1376), n1)) & leq(n0, X0)) | sP4 | ~ (init = init)) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | ~ leq(X1, minus(pv1376, n1)) | ~ leq(n0, X1)) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & leq(pv1376, n3) & leq(n0, pv1376) & (init = init)), inference(rectify, [], [f171])).
fof(f171, plain, ((? [X3] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X3)) & leq(X3, minus(plus(n1, pv1376), n1)) & leq(n0, X3)) | sP4 | ~ (init = init)) & ! [X0] : ((init = a_select2(s_values7_init, X0)) | ~ leq(X0, minus(pv1376, n1)) | ~ leq(n0, X0)) & ! [X1] : (! [X2] : ((init = a_select3(simplex7_init, X2, X1)) | ~ leq(X2, n3) | ~ leq(n0, X2)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & leq(pv1376, n3) & leq(n0, pv1376) & (init = init)), inference(definition_folding, [], [f150, e170])).
fof(f170, plain, (? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & leq(X5, n3) & leq(n0, X5)) & leq(X4, n2) & leq(n0, X4)) | ~ sP4), inference(usedef, [], [e170])).
fof(e170, plain, (sP4 <=> ? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & leq(X5, n3) & leq(n0, X5)) & leq(X4, n2) & leq(n0, X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f150, plain, ((? [X3] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X3)) & leq(X3, minus(plus(n1, pv1376), n1)) & leq(n0, X3)) | ? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & leq(X5, n3) & leq(n0, X5)) & leq(X4, n2) & leq(n0, X4)) | ~ (init = init)) & ! [X0] : ((init = a_select2(s_values7_init, X0)) | ~ leq(X0, minus(pv1376, n1)) | ~ leq(n0, X0)) & ! [X1] : (! [X2] : ((init = a_select3(simplex7_init, X2, X1)) | ~ leq(X2, n3) | ~ leq(n0, X2)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & leq(pv1376, n3) & leq(n0, pv1376) & (init = init)), inference(flattening, [], [f149])).
fof(f149, plain, ((? [X3] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X3)) & (leq(X3, minus(plus(n1, pv1376), n1)) & leq(n0, X3))) | ? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & (leq(X5, n3) & leq(n0, X5))) & (leq(X4, n2) & leq(n0, X4))) | ~ (init = init)) & (! [X0] : ((init = a_select2(s_values7_init, X0)) | (~ leq(X0, minus(pv1376, n1)) | ~ leq(n0, X0))) & ! [X1] : (! [X2] : ((init = a_select3(simplex7_init, X2, X1)) | (~ leq(X2, n3) | ~ leq(n0, X2))) | (~ leq(X1, n2) | ~ leq(n0, X1))) & leq(pv1376, n3) & leq(n0, pv1376) & (init = init))), inference(ennf_transformation, [], [f107])).
fof(f107, plain, ~ ((! [X0] : ((leq(X0, minus(pv1376, n1)) & leq(n0, X0)) => (init = a_select2(s_values7_init, X0))) & ! [X1] : ((leq(X1, n2) & leq(n0, X1)) => ! [X2] : ((leq(X2, n3) & leq(n0, X2)) => (init = a_select3(simplex7_init, X2, X1)))) & leq(pv1376, n3) & leq(n0, pv1376) & (init = init)) => (! [X3] : ((leq(X3, minus(plus(n1, pv1376), n1)) & leq(n0, X3)) => (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X3))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => ! [X5] : ((leq(X5, n3) & leq(n0, X5)) => (init = a_select3(simplex7_init, X5, X4)))) & (init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ ((! [X3] : ((leq(X3, minus(pv1376, n1)) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(n0, pv1376) & (init = init)) => (! [X21] : ((leq(X21, minus(plus(n1, pv1376), n1)) & leq(n0, X21)) => (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X21))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => ! [X20] : ((leq(X20, n3) & leq(n0, X20)) => (init = a_select3(simplex7_init, X20, X19)))) & (init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ ((! [X3] : ((leq(X3, minus(pv1376, n1)) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(n0, pv1376) & (init = init)) => (! [X21] : ((leq(X21, minus(plus(n1, pv1376), n1)) & leq(n0, X21)) => (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X21))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => ! [X20] : ((leq(X20, n3) & leq(n0, X20)) => (init = a_select3(simplex7_init, X20, X19)))) & (init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', gauss_init_0045)).
fof(f303764, plain, (~ spl35_13 | ~ spl35_15 | spl35_49), inference(avatar_contradiction_clause, [], [f303763])).
fof(f303763, plain, ($false | (~ spl35_13 | ~ spl35_15 | spl35_49)), inference(subsumption_resolution, [], [f303762, f493])).
fof(f493, plain, leq(n0, n2), inference(resolution, [], [f232, f360])).
fof(f360, plain, gt(n2, n0), inference(cnf_transformation, [], [f65])).
fof(f65, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', gt_2_0)).
fof(f303762, plain, (~ leq(n0, n2) | (~ spl35_13 | ~ spl35_15 | spl35_49)), inference(subsumption_resolution, [], [f303700, f303457])).
fof(f303700, plain, ((init = a_select2(s_values7_init, n0)) | ~ leq(n0, n2) | ~ spl35_15), inference(resolution, [], [f303052, f230])).
fof(f303052, plain, (! [X1] : (~ leq(n0, X1) | (init = a_select2(s_values7_init, X1)) | ~ leq(X1, n2)) | ~ spl35_15), inference(forward_demodulation, [], [f303029, f526])).
fof(f526, plain, (n2 = minus(n3, n1)), inference(superposition, [], [f398, f483])).
fof(f483, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f482, f478])).
fof(f482, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f481, f405])).
fof(f481, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f407, f389])).
fof(f389, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f308, f307])).
fof(f308, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', succ_plus_1_l)).
fof(f407, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f381, f307, f307, f307])).
fof(f381, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', successor_3)).
fof(f303029, plain, (! [X1] : (~ leq(X1, minus(n3, n1)) | (init = a_select2(s_values7_init, X1)) | ~ leq(n0, X1)) | ~ spl35_15), inference(backward_demodulation, [], [f346, f804])).
fof(f804, plain, ((n3 = pv1376) | ~ spl35_15), inference(avatar_component_clause, [], [f802])).
fof(f802, plain, (spl35_15 <=> (n3 = pv1376)), introduced(avatar_definition, [new_symbols(naming, [spl35_15])])).
fof(f303447, plain, (~ spl35_45 | spl35_13 | ~ spl35_9 | spl35_44), inference(avatar_split_clause, [], [f303446, f1284, f467, f793, f1288])).
fof(f1288, plain, (spl35_45 <=> leq(sK34, n1)), introduced(avatar_definition, [new_symbols(naming, [spl35_45])])).
fof(f467, plain, (spl35_9 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl35_9])])).
fof(f1284, plain, (spl35_44 <=> (n1 = sK34)), introduced(avatar_definition, [new_symbols(naming, [spl35_44])])).
fof(f303446, plain, ((n0 = sK34) | ~ leq(sK34, n1) | (~ spl35_9 | spl35_44)), inference(subsumption_resolution, [], [f303131, f1285])).
fof(f1285, plain, (~ (n1 = sK34) | spl35_44), inference(avatar_component_clause, [], [f1284])).
fof(f303131, plain, ((n0 = sK34) | ~ leq(sK34, n1) | (n1 = sK34) | ~ spl35_9), inference(resolution, [], [f469, f374])).
fof(f374, plain, ! [X0] : (~ leq(n0, X0) | (n0 = X0) | ~ leq(X0, n1) | (n1 = X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : ((n1 = X0) | (n0 = X0) | ~ leq(X0, n1) | ~ leq(n0, X0)), inference(flattening, [], [f157])).
fof(f157, plain, ! [X0] : (((n1 = X0) | (n0 = X0)) | (~ leq(X0, n1) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : ((leq(X0, n1) & leq(n0, X0)) => ((n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', finite_domain_1)).
fof(f469, plain, (leq(n0, sK34) | ~ spl35_9), inference(avatar_component_clause, [], [f467])).
fof(f303358, plain, (spl35_49 | ~ spl35_56 | ~ spl35_70), inference(avatar_contradiction_clause, [], [f303357])).
fof(f303357, plain, ($false | (spl35_49 | ~ spl35_56 | ~ spl35_70)), inference(subsumption_resolution, [], [f303260, f1955])).
fof(f1955, plain, ((init = a_select2(s_values7_init, n2)) | ~ spl35_70), inference(avatar_component_clause, [], [f1953])).
fof(f1953, plain, (spl35_70 <=> (init = a_select2(s_values7_init, n2))), introduced(avatar_definition, [new_symbols(naming, [spl35_70])])).
fof(f303260, plain, (~ (init = a_select2(s_values7_init, n2)) | (spl35_49 | ~ spl35_56)), inference(backward_demodulation, [], [f1331, f1431])).
fof(f1431, plain, ((n2 = sK34) | ~ spl35_56), inference(avatar_component_clause, [], [f1429])).
fof(f1429, plain, (spl35_56 <=> (n2 = sK34)), introduced(avatar_definition, [new_symbols(naming, [spl35_56])])).
fof(f303222, plain, (spl35_56 | ~ spl35_9 | spl35_13 | ~ spl35_15 | spl35_17 | spl35_44 | ~ spl35_61), inference(avatar_split_clause, [], [f303221, f1475, f1284, f811, f802, f793, f467, f1429])).
fof(f811, plain, (spl35_17 <=> (pv1376 = sK34)), introduced(avatar_definition, [new_symbols(naming, [spl35_17])])).
fof(f1475, plain, (spl35_61 <=> leq(sK34, n3)), introduced(avatar_definition, [new_symbols(naming, [spl35_61])])).
fof(f303221, plain, ((n2 = sK34) | (~ spl35_9 | spl35_13 | ~ spl35_15 | spl35_17 | spl35_44 | ~ spl35_61)), inference(subsumption_resolution, [], [f303220, f303088])).
fof(f303088, plain, (~ (n3 = sK34) | (~ spl35_15 | spl35_17)), inference(forward_demodulation, [], [f812, f804])).
fof(f812, plain, (~ (pv1376 = sK34) | spl35_17), inference(avatar_component_clause, [], [f811])).
fof(f303220, plain, ((n2 = sK34) | (n3 = sK34) | (~ spl35_9 | spl35_13 | spl35_44 | ~ spl35_61)), inference(subsumption_resolution, [], [f303219, f1476])).
fof(f1476, plain, (leq(sK34, n3) | ~ spl35_61), inference(avatar_component_clause, [], [f1475])).
fof(f303219, plain, ((n2 = sK34) | ~ leq(sK34, n3) | (n3 = sK34) | (~ spl35_9 | spl35_13 | spl35_44)), inference(subsumption_resolution, [], [f303218, f794])).
fof(f794, plain, (~ (n0 = sK34) | spl35_13), inference(avatar_component_clause, [], [f793])).
fof(f303218, plain, ((n2 = sK34) | (n0 = sK34) | ~ leq(sK34, n3) | (n3 = sK34) | (~ spl35_9 | spl35_44)), inference(subsumption_resolution, [], [f303133, f1285])).
fof(f303133, plain, ((n2 = sK34) | (n1 = sK34) | (n0 = sK34) | ~ leq(sK34, n3) | (n3 = sK34) | ~ spl35_9), inference(resolution, [], [f469, f376])).
fof(f376, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f162])).
fof(f162, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f161])).
fof(f161, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', finite_domain_3)).
fof(f303016, plain, (spl35_7 | ~ spl35_17), inference(avatar_contradiction_clause, [], [f303015])).
fof(f303015, plain, ($false | (spl35_7 | ~ spl35_17)), inference(subsumption_resolution, [], [f303010, f331])).
fof(f331, plain, ! [X2, X0, X1] : (a_select2(tptp_update2(X0, X1, X2), X1) = X2), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ! [X0, X1, X2] : (a_select2(tptp_update2(X0, X1, X2), X1) = X2), inference(rectify, [], [f48])).
fof(f48, plain, ! [X0, X6, X16] : (a_select2(tptp_update2(X0, X6, X16), X6) = X16), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', sel2_update_1)).
fof(f303010, plain, (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), pv1376)) | (spl35_7 | ~ spl35_17)), inference(backward_demodulation, [], [f459, f813])).
fof(f813, plain, ((pv1376 = sK34) | ~ spl35_17), inference(avatar_component_clause, [], [f811])).
fof(f459, plain, (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK34)) | spl35_7), inference(avatar_component_clause, [], [f457])).
fof(f457, plain, (spl35_7 <=> (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK34))), introduced(avatar_definition, [new_symbols(naming, [spl35_7])])).
fof(f303007, plain, (~ spl35_16 | ~ spl35_54 | ~ spl35_56), inference(avatar_contradiction_clause, [], [f303006])).
fof(f303006, plain, ($false | (~ spl35_16 | ~ spl35_54 | ~ spl35_56)), inference(subsumption_resolution, [], [f303003, f229])).
fof(f229, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', irreflexivity_gt)).
fof(f303003, plain, (gt(n2, n2) | (~ spl35_16 | ~ spl35_54 | ~ spl35_56)), inference(backward_demodulation, [], [f302992, f1431])).
fof(f302992, plain, (gt(n2, sK34) | (~ spl35_16 | ~ spl35_54)), inference(forward_demodulation, [], [f809, f1422])).
fof(f809, plain, (gt(pv1376, sK34) | ~ spl35_16), inference(avatar_component_clause, [], [f807])).
fof(f807, plain, (spl35_16 <=> gt(pv1376, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl35_16])])).
fof(f302993, plain, (spl35_68 | ~ spl35_47 | ~ spl35_54), inference(avatar_split_clause, [], [f302953, f1420, f1308, f1944])).
fof(f1944, plain, (spl35_68 <=> (init = a_select2(s_values7_init, n1))), introduced(avatar_definition, [new_symbols(naming, [spl35_68])])).
fof(f1308, plain, (spl35_47 <=> (init = a_select2(s_values7_init, minus(pv1376, n1)))), introduced(avatar_definition, [new_symbols(naming, [spl35_47])])).
fof(f302953, plain, ((init = a_select2(s_values7_init, n1)) | (~ spl35_47 | ~ spl35_54)), inference(forward_demodulation, [], [f302932, f525])).
fof(f302932, plain, ((init = a_select2(s_values7_init, minus(n2, n1))) | (~ spl35_47 | ~ spl35_54)), inference(backward_demodulation, [], [f1310, f1422])).
fof(f1310, plain, ((init = a_select2(s_values7_init, minus(pv1376, n1))) | ~ spl35_47), inference(avatar_component_clause, [], [f1308])).
fof(f302915, plain, (spl35_68 | ~ spl35_15), inference(avatar_split_clause, [], [f302914, f802, f1944])).
fof(f302914, plain, ((init = a_select2(s_values7_init, n1)) | ~ spl35_15), inference(subsumption_resolution, [], [f302792, f494])).
fof(f494, plain, leq(n1, n2), inference(resolution, [], [f232, f364])).
fof(f364, plain, gt(n2, n1), inference(cnf_transformation, [], [f69])).
fof(f69, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', gt_2_1)).
fof(f302792, plain, ((init = a_select2(s_values7_init, n1)) | ~ leq(n1, n2) | ~ spl35_15), inference(resolution, [], [f302723, f490])).
fof(f302723, plain, (! [X1] : (~ leq(n0, X1) | (init = a_select2(s_values7_init, X1)) | ~ leq(X1, n2)) | ~ spl35_15), inference(forward_demodulation, [], [f302696, f526])).
fof(f302696, plain, (! [X1] : (~ leq(X1, minus(n3, n1)) | (init = a_select2(s_values7_init, X1)) | ~ leq(n0, X1)) | ~ spl35_15), inference(backward_demodulation, [], [f346, f804])).
fof(f302726, plain, (spl35_70 | ~ spl35_15 | ~ spl35_47), inference(avatar_split_clause, [], [f302725, f1308, f802, f1953])).
fof(f302725, plain, ((init = a_select2(s_values7_init, n2)) | (~ spl35_15 | ~ spl35_47)), inference(forward_demodulation, [], [f302704, f526])).
fof(f302704, plain, ((init = a_select2(s_values7_init, minus(n3, n1))) | (~ spl35_15 | ~ spl35_47)), inference(backward_demodulation, [], [f1310, f804])).
fof(f302693, plain, (spl35_7 | ~ spl35_42 | ~ spl35_44), inference(avatar_contradiction_clause, [], [f302692])).
fof(f302692, plain, ($false | (spl35_7 | ~ spl35_42 | ~ spl35_44)), inference(subsumption_resolution, [], [f302691, f331])).
fof(f302691, plain, (~ (init = a_select2(tptp_update2(s_values7_init, n1, init), n1)) | (spl35_7 | ~ spl35_42 | ~ spl35_44)), inference(forward_demodulation, [], [f302690, f1277])).
fof(f302690, plain, (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), n1)) | (spl35_7 | ~ spl35_44)), inference(forward_demodulation, [], [f459, f1286])).
fof(f1286, plain, ((n1 = sK34) | ~ spl35_44), inference(avatar_component_clause, [], [f1284])).
fof(f302621, plain, (~ spl35_68 | ~ spl35_44 | spl35_49), inference(avatar_split_clause, [], [f302615, f1329, f1284, f1944])).
fof(f302615, plain, (~ (init = a_select2(s_values7_init, n1)) | (~ spl35_44 | spl35_49)), inference(backward_demodulation, [], [f1331, f1286])).
fof(f302057, plain, (spl35_2 | ~ spl35_5 | ~ spl35_6 | ~ spl35_62), inference(avatar_contradiction_clause, [], [f302056])).
fof(f302056, plain, ($false | (spl35_2 | ~ spl35_5 | ~ spl35_6 | ~ spl35_62)), inference(subsumption_resolution, [], [f301491, f302055])).
fof(f302055, plain, (~ (init = a_select3(simplex7_init, n1, sK32)) | (spl35_2 | ~ spl35_62)), inference(forward_demodulation, [], [f434, f1886])).
fof(f1886, plain, ((n1 = sK33) | ~ spl35_62), inference(avatar_component_clause, [], [f1884])).
fof(f1884, plain, (spl35_62 <=> (n1 = sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_62])])).
fof(f434, plain, (~ (init = a_select3(simplex7_init, sK33, sK32)) | spl35_2), inference(avatar_component_clause, [], [f432])).
fof(f432, plain, (spl35_2 <=> (init = a_select3(simplex7_init, sK33, sK32))), introduced(avatar_definition, [new_symbols(naming, [spl35_2])])).
fof(f301491, plain, ((init = a_select3(simplex7_init, n1, sK32)) | (~ spl35_5 | ~ spl35_6)), inference(subsumption_resolution, [], [f301392, f449])).
fof(f449, plain, (leq(sK32, n2) | ~ spl35_5), inference(avatar_component_clause, [], [f447])).
fof(f447, plain, (spl35_5 <=> leq(sK32, n2)), introduced(avatar_definition, [new_symbols(naming, [spl35_5])])).
fof(f301392, plain, (~ leq(sK32, n2) | (init = a_select3(simplex7_init, n1, sK32)) | ~ spl35_6), inference(resolution, [], [f454, f1459])).
fof(f1459, plain, ! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (init = a_select3(simplex7_init, n1, X3))), inference(subsumption_resolution, [], [f1440, f497])).
fof(f497, plain, leq(n1, n3), inference(resolution, [], [f232, f365])).
fof(f365, plain, gt(n3, n1), inference(cnf_transformation, [], [f70])).
fof(f70, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', gt_3_1)).
fof(f1440, plain, ! [X3] : (~ leq(n1, n3) | (init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)), inference(resolution, [], [f345, f490])).
fof(f345, plain, ! [X2, X3] : (~ leq(n0, X3) | ~ leq(X3, n3) | (init = a_select3(simplex7_init, X3, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f226])).
fof(f454, plain, (leq(n0, sK32) | ~ spl35_6), inference(avatar_component_clause, [], [f452])).
fof(f452, plain, (spl35_6 <=> leq(n0, sK32)), introduced(avatar_definition, [new_symbols(naming, [spl35_6])])).
fof(f301816, plain, (spl35_2 | ~ spl35_5 | ~ spl35_6 | ~ spl35_21), inference(avatar_contradiction_clause, [], [f301815])).
fof(f301815, plain, ($false | (spl35_2 | ~ spl35_5 | ~ spl35_6 | ~ spl35_21)), inference(subsumption_resolution, [], [f301496, f301811])).
fof(f301811, plain, (~ (init = a_select3(simplex7_init, n0, sK32)) | (spl35_2 | ~ spl35_21)), inference(forward_demodulation, [], [f434, f841])).
fof(f841, plain, ((n0 = sK33) | ~ spl35_21), inference(avatar_component_clause, [], [f839])).
fof(f839, plain, (spl35_21 <=> (n0 = sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_21])])).
fof(f301496, plain, ((init = a_select3(simplex7_init, n0, sK32)) | (~ spl35_5 | ~ spl35_6)), inference(subsumption_resolution, [], [f301395, f449])).
fof(f301395, plain, (~ leq(sK32, n2) | (init = a_select3(simplex7_init, n0, sK32)) | ~ spl35_6), inference(resolution, [], [f454, f1479])).
fof(f1479, plain, ! [X34] : (~ leq(n0, X34) | ~ leq(X34, n2) | (init = a_select3(simplex7_init, n0, X34))), inference(subsumption_resolution, [], [f1455, f495])).
fof(f495, plain, leq(n0, n3), inference(resolution, [], [f232, f361])).
fof(f361, plain, gt(n3, n0), inference(cnf_transformation, [], [f66])).
fof(f66, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', gt_3_0)).
fof(f1455, plain, ! [X34] : (~ leq(n0, n3) | (init = a_select3(simplex7_init, n0, X34)) | ~ leq(X34, n2) | ~ leq(n0, X34)), inference(resolution, [], [f345, f230])).
fof(f301565, plain, (spl35_2 | ~ spl35_5 | ~ spl35_6 | ~ spl35_63), inference(avatar_contradiction_clause, [], [f301564])).
fof(f301564, plain, ($false | (spl35_2 | ~ spl35_5 | ~ spl35_6 | ~ spl35_63)), inference(subsumption_resolution, [], [f301552, f301492])).
fof(f301492, plain, ((init = a_select3(simplex7_init, n2, sK32)) | (~ spl35_5 | ~ spl35_6)), inference(subsumption_resolution, [], [f301393, f449])).
fof(f301393, plain, (~ leq(sK32, n2) | (init = a_select3(simplex7_init, n2, sK32)) | ~ spl35_6), inference(resolution, [], [f454, f1460])).
fof(f1460, plain, ! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (init = a_select3(simplex7_init, n2, X4))), inference(subsumption_resolution, [], [f1441, f498])).
fof(f498, plain, leq(n2, n3), inference(resolution, [], [f232, f368])).
fof(f368, plain, gt(n3, n2), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', gt_3_2)).
fof(f1441, plain, ! [X4] : (~ leq(n2, n3) | (init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(resolution, [], [f345, f493])).
fof(f301552, plain, (~ (init = a_select3(simplex7_init, n2, sK32)) | (spl35_2 | ~ spl35_63)), inference(backward_demodulation, [], [f434, f1890])).
fof(f1890, plain, ((n2 = sK33) | ~ spl35_63), inference(avatar_component_clause, [], [f1888])).
fof(f1888, plain, (spl35_63 <=> (n2 = sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_63])])).
fof(f301495, plain, (spl35_2 | ~ spl35_5 | ~ spl35_6 | ~ spl35_19), inference(avatar_contradiction_clause, [], [f301494])).
fof(f301494, plain, ($false | (spl35_2 | ~ spl35_5 | ~ spl35_6 | ~ spl35_19)), inference(subsumption_resolution, [], [f301493, f301321])).
fof(f301321, plain, (~ (init = a_select3(simplex7_init, n3, sK32)) | (spl35_2 | ~ spl35_19)), inference(forward_demodulation, [], [f434, f825])).
fof(f825, plain, ((n3 = sK33) | ~ spl35_19), inference(avatar_component_clause, [], [f823])).
fof(f823, plain, (spl35_19 <=> (n3 = sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_19])])).
fof(f301493, plain, ((init = a_select3(simplex7_init, n3, sK32)) | (~ spl35_5 | ~ spl35_6)), inference(subsumption_resolution, [], [f301394, f449])).
fof(f301394, plain, (~ leq(sK32, n2) | (init = a_select3(simplex7_init, n3, sK32)) | ~ spl35_6), inference(resolution, [], [f454, f1461])).
fof(f1461, plain, ! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (init = a_select3(simplex7_init, n3, X5))), inference(subsumption_resolution, [], [f1442, f230])).
fof(f1442, plain, ! [X5] : (~ leq(n3, n3) | (init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)), inference(resolution, [], [f345, f495])).
fof(f1891, plain, (spl35_19 | spl35_21 | spl35_62 | spl35_63 | ~ spl35_3 | ~ spl35_4), inference(avatar_split_clause, [], [f1882, f442, f437, f1888, f1884, f839, f823])).
fof(f437, plain, (spl35_3 <=> leq(sK33, n3)), introduced(avatar_definition, [new_symbols(naming, [spl35_3])])).
fof(f442, plain, (spl35_4 <=> leq(n0, sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_4])])).
fof(f1882, plain, ((n2 = sK33) | (n1 = sK33) | (n0 = sK33) | (n3 = sK33) | (~ spl35_3 | ~ spl35_4)), inference(subsumption_resolution, [], [f1874, f439])).
fof(f439, plain, (leq(sK33, n3) | ~ spl35_3), inference(avatar_component_clause, [], [f437])).
fof(f1874, plain, ((n2 = sK33) | (n1 = sK33) | (n0 = sK33) | ~ leq(sK33, n3) | (n3 = sK33) | ~ spl35_4), inference(resolution, [], [f444, f376])).
fof(f444, plain, (leq(n0, sK33) | ~ spl35_4), inference(avatar_component_clause, [], [f442])).
fof(f1854, plain, (spl35_7 | ~ spl35_11 | ~ spl35_13), inference(avatar_contradiction_clause, [], [f1853])).
fof(f1853, plain, ($false | (spl35_7 | ~ spl35_11 | ~ spl35_13)), inference(subsumption_resolution, [], [f1823, f331])).
fof(f1823, plain, (~ (init = a_select2(tptp_update2(s_values7_init, n0, init), n0)) | (spl35_7 | ~ spl35_11 | ~ spl35_13)), inference(backward_demodulation, [], [f1771, f786])).
fof(f786, plain, ((n0 = pv1376) | ~ spl35_11), inference(avatar_component_clause, [], [f784])).
fof(f784, plain, (spl35_11 <=> (n0 = pv1376)), introduced(avatar_definition, [new_symbols(naming, [spl35_11])])).
fof(f1771, plain, (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), n0)) | (spl35_7 | ~ spl35_13)), inference(backward_demodulation, [], [f459, f795])).
fof(f1741, plain, (spl35_46 | ~ spl35_54), inference(avatar_contradiction_clause, [], [f1740])).
fof(f1740, plain, ($false | (spl35_46 | ~ spl35_54)), inference(subsumption_resolution, [], [f1739, f490])).
fof(f1739, plain, (~ leq(n0, n1) | (spl35_46 | ~ spl35_54)), inference(forward_demodulation, [], [f1722, f525])).
fof(f1722, plain, (~ leq(n0, minus(n2, n1)) | (spl35_46 | ~ spl35_54)), inference(backward_demodulation, [], [f1306, f1422])).
fof(f1306, plain, (~ leq(n0, minus(pv1376, n1)) | spl35_46), inference(avatar_component_clause, [], [f1304])).
fof(f1304, plain, (spl35_46 <=> leq(n0, minus(pv1376, n1))), introduced(avatar_definition, [new_symbols(naming, [spl35_46])])).
fof(f1738, plain, (~ spl35_8 | ~ spl35_54 | spl35_57), inference(avatar_contradiction_clause, [], [f1737])).
fof(f1737, plain, ($false | (~ spl35_8 | ~ spl35_54 | spl35_57)), inference(subsumption_resolution, [], [f1714, f1435])).
fof(f1435, plain, (~ leq(sK34, n2) | spl35_57), inference(avatar_component_clause, [], [f1433])).
fof(f1433, plain, (spl35_57 <=> leq(sK34, n2)), introduced(avatar_definition, [new_symbols(naming, [spl35_57])])).
fof(f1714, plain, (leq(sK34, n2) | (~ spl35_8 | ~ spl35_54)), inference(backward_demodulation, [], [f827, f1422])).
fof(f827, plain, (leq(sK34, pv1376) | ~ spl35_8), inference(forward_demodulation, [], [f464, f522])).
fof(f522, plain, ! [X0] : (minus(plus(n1, X0), n1) = X0), inference(superposition, [], [f398, f389])).
fof(f464, plain, (leq(sK34, minus(plus(n1, pv1376), n1)) | ~ spl35_8), inference(avatar_component_clause, [], [f462])).
fof(f462, plain, (spl35_8 <=> leq(sK34, minus(plus(n1, pv1376), n1))), introduced(avatar_definition, [new_symbols(naming, [spl35_8])])).
fof(f1678, plain, (~ spl35_15 | spl35_46), inference(avatar_contradiction_clause, [], [f1677])).
fof(f1677, plain, ($false | (~ spl35_15 | spl35_46)), inference(subsumption_resolution, [], [f1676, f493])).
fof(f1676, plain, (~ leq(n0, n2) | (~ spl35_15 | spl35_46)), inference(forward_demodulation, [], [f1657, f526])).
fof(f1657, plain, (~ leq(n0, minus(n3, n1)) | (~ spl35_15 | spl35_46)), inference(backward_demodulation, [], [f1306, f804])).
fof(f1675, plain, (~ spl35_8 | ~ spl35_15 | spl35_61), inference(avatar_contradiction_clause, [], [f1674])).
fof(f1674, plain, ($false | (~ spl35_8 | ~ spl35_15 | spl35_61)), inference(subsumption_resolution, [], [f1649, f1477])).
fof(f1477, plain, (~ leq(sK34, n3) | spl35_61), inference(avatar_component_clause, [], [f1475])).
fof(f1649, plain, (leq(sK34, n3) | (~ spl35_8 | ~ spl35_15)), inference(backward_demodulation, [], [f827, f804])).
fof(f1611, plain, (~ spl35_42 | spl35_46), inference(avatar_contradiction_clause, [], [f1610])).
fof(f1610, plain, ($false | (~ spl35_42 | spl35_46)), inference(subsumption_resolution, [], [f1609, f230])).
fof(f1609, plain, (~ leq(n0, n0) | (~ spl35_42 | spl35_46)), inference(forward_demodulation, [], [f1590, f523])).
fof(f1590, plain, (~ leq(n0, minus(n1, n1)) | (~ spl35_42 | spl35_46)), inference(backward_demodulation, [], [f1306, f1277])).
fof(f1606, plain, (~ spl35_8 | ~ spl35_42 | spl35_45), inference(avatar_contradiction_clause, [], [f1605])).
fof(f1605, plain, ($false | (~ spl35_8 | ~ spl35_42 | spl35_45)), inference(subsumption_resolution, [], [f1582, f1290])).
fof(f1290, plain, (~ leq(sK34, n1) | spl35_45), inference(avatar_component_clause, [], [f1288])).
fof(f1582, plain, (leq(sK34, n1) | (~ spl35_8 | ~ spl35_42)), inference(backward_demodulation, [], [f827, f1277])).
fof(f1540, plain, (~ spl35_8 | ~ spl35_11 | spl35_35), inference(avatar_contradiction_clause, [], [f1539])).
fof(f1539, plain, ($false | (~ spl35_8 | ~ spl35_11 | spl35_35)), inference(subsumption_resolution, [], [f1514, f933])).
fof(f933, plain, (~ leq(sK34, n0) | spl35_35), inference(avatar_component_clause, [], [f931])).
fof(f931, plain, (spl35_35 <=> leq(sK34, n0)), introduced(avatar_definition, [new_symbols(naming, [spl35_35])])).
fof(f1514, plain, (leq(sK34, n0) | (~ spl35_8 | ~ spl35_11)), inference(backward_demodulation, [], [f827, f786])).
fof(f1504, plain, (spl35_15 | spl35_11 | spl35_42 | spl35_54), inference(avatar_split_clause, [], [f1503, f1420, f1275, f784, f802])).
fof(f1503, plain, ((n2 = pv1376) | (n1 = pv1376) | (n0 = pv1376) | (n3 = pv1376)), inference(subsumption_resolution, [], [f1489, f344])).
fof(f344, plain, leq(pv1376, n3), inference(cnf_transformation, [], [f226])).
fof(f1489, plain, ((n2 = pv1376) | (n1 = pv1376) | (n0 = pv1376) | ~ leq(pv1376, n3) | (n3 = pv1376)), inference(resolution, [], [f376, f343])).
fof(f343, plain, leq(n0, pv1376), inference(cnf_transformation, [], [f226])).
fof(f1436, plain, (spl35_56 | ~ spl35_57 | spl35_13 | spl35_44 | ~ spl35_9), inference(avatar_split_clause, [], [f1396, f467, f1284, f793, f1433, f1429])).
fof(f1396, plain, ((n1 = sK34) | (n0 = sK34) | ~ leq(sK34, n2) | (n2 = sK34) | ~ spl35_9), inference(resolution, [], [f375, f469])).
fof(f375, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f159])).
fof(f159, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', finite_domain_2)).
fof(f1332, plain, (spl35_17 | ~ spl35_49 | spl35_7), inference(avatar_split_clause, [], [f1326, f457, f1329, f811])).
fof(f1326, plain, (~ (init = a_select2(s_values7_init, sK34)) | (pv1376 = sK34) | spl35_7), inference(superposition, [], [f459, f410])).
fof(f410, plain, ! [X4, X2, X0, X1] : ((a_select2(X2, X1) = a_select2(tptp_update2(X2, X0, X4), X1)) | (X0 = X1)), inference(equality_resolution, [], [f332])).
fof(f332, plain, ! [X4, X2, X0, X3, X1] : ((a_select2(tptp_update2(X2, X0, X4), X1) = X3) | ~ (a_select2(X2, X1) = X3) | (X0 = X1)), inference(cnf_transformation, [], [f146])).
fof(f146, plain, ! [X0, X1, X2, X3, X4] : ((a_select2(tptp_update2(X2, X0, X4), X1) = X3) | ~ (a_select2(X2, X1) = X3) | (X0 = X1)), inference(flattening, [], [f145])).
fof(f145, plain, ! [X0, X1, X2, X3, X4] : ((a_select2(tptp_update2(X2, X0, X4), X1) = X3) | (~ (a_select2(X2, X1) = X3) | (X0 = X1))), inference(ennf_transformation, [], [f105])).
fof(f105, plain, ! [X0, X1, X2, X3, X4] : (((a_select2(X2, X1) = X3) & ~ (X0 = X1)) => (a_select2(tptp_update2(X2, X0, X4), X1) = X3)), inference(rectify, [], [f49])).
fof(f49, plain, ! [X4, X6, X0, X16, X24] : (((a_select2(X0, X6) = X16) & ~ (X4 = X6)) => (a_select2(tptp_update2(X0, X4, X24), X6) = X16)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', sel2_update_2)).
fof(f1316, plain, (~ spl35_46 | spl35_48), inference(avatar_split_clause, [], [f1312, f1314, f1304])).
fof(f1312, plain, ! [X1] : ((init = a_select2(s_values7_init, uniform_int_rnd(X1, minus(pv1376, n1)))) | ~ leq(n0, minus(pv1376, n1))), inference(subsumption_resolution, [], [f1294, f241])).
fof(f1294, plain, ! [X1] : ((init = a_select2(s_values7_init, uniform_int_rnd(X1, minus(pv1376, n1)))) | ~ leq(n0, uniform_int_rnd(X1, minus(pv1376, n1))) | ~ leq(n0, minus(pv1376, n1))), inference(resolution, [], [f346, f240])).
fof(f1311, plain, (~ spl35_46 | spl35_47), inference(avatar_split_clause, [], [f1293, f1308, f1304])).
fof(f1293, plain, ((init = a_select2(s_values7_init, minus(pv1376, n1))) | ~ leq(n0, minus(pv1376, n1))), inference(resolution, [], [f346, f230])).
fof(f934, plain, (spl35_13 | ~ spl35_35 | ~ spl35_9), inference(avatar_split_clause, [], [f884, f467, f931, f793])).
fof(f884, plain, (~ leq(sK34, n0) | (n0 = sK34) | ~ spl35_9), inference(resolution, [], [f373, f469])).
fof(f814, plain, (spl35_16 | spl35_17 | ~ spl35_8), inference(avatar_split_clause, [], [f778, f462, f811, f807])).
fof(f778, plain, ((pv1376 = sK34) | gt(pv1376, sK34) | ~ spl35_8), inference(resolution, [], [f233, f529])).
fof(f529, plain, (leq(sK34, pv1376) | ~ spl35_8), inference(backward_demodulation, [], [f464, f522])).
fof(f233, plain, ! [X0, X1] : (~ leq(X0, X1) | (X0 = X1) | gt(X1, X0)), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ! [X0, X1] : (gt(X1, X0) | (X0 = X1) | ~ leq(X0, X1)), inference(flattening, [], [f114])).
fof(f114, plain, ! [X0, X1] : (gt(X1, X0) | ((X0 = X1) | ~ leq(X0, X1))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : ((~ (X0 = X1) & leq(X0, X1)) => gt(X1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV033+1.p', leq_gt2)).
fof(f470, plain, (spl35_1 | spl35_9), inference(avatar_split_clause, [], [f411, f467, f428])).
fof(f428, plain, (spl35_1 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl35_1])])).
fof(f411, plain, (leq(n0, sK34) | sP4), inference(trivial_inequality_removal, [], [f347])).
fof(f347, plain, (leq(n0, sK34) | sP4 | ~ (init = init)), inference(cnf_transformation, [], [f226])).
fof(f465, plain, (spl35_1 | spl35_8), inference(avatar_split_clause, [], [f412, f462, f428])).
fof(f412, plain, (leq(sK34, minus(plus(n1, pv1376), n1)) | sP4), inference(trivial_inequality_removal, [], [f348])).
fof(f348, plain, (leq(sK34, minus(plus(n1, pv1376), n1)) | sP4 | ~ (init = init)), inference(cnf_transformation, [], [f226])).
fof(f460, plain, (spl35_1 | ~ spl35_7), inference(avatar_split_clause, [], [f413, f457, f428])).
fof(f413, plain, (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK34)) | sP4), inference(trivial_inequality_removal, [], [f349])).
fof(f349, plain, (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK34)) | sP4 | ~ (init = init)), inference(cnf_transformation, [], [f226])).
fof(f455, plain, (~ spl35_1 | spl35_6), inference(avatar_split_clause, [], [f337, f452, f428])).
fof(f337, plain, (leq(n0, sK32) | ~ sP4), inference(cnf_transformation, [], [f223])).
fof(f223, plain, (((~ (init = a_select3(simplex7_init, sK33, sK32)) & leq(sK33, n3) & leq(n0, sK33)) & leq(sK32, n2) & leq(n0, sK32)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK32, sK33])], [f220, f222, f221])).
fof(f221, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK32)) & leq(X1, n3) & leq(n0, X1)) & leq(sK32, n2) & leq(n0, sK32))), introduced(choice_axiom, [])).
fof(f222, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK32)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK33, sK32)) & leq(sK33, n3) & leq(n0, sK33))), introduced(choice_axiom, [])).
fof(f220, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f219])).
fof(f219, plain, (? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & leq(X5, n3) & leq(n0, X5)) & leq(X4, n2) & leq(n0, X4)) | ~ sP4), inference(nnf_transformation, [], [f170])).
fof(f450, plain, (~ spl35_1 | spl35_5), inference(avatar_split_clause, [], [f338, f447, f428])).
fof(f338, plain, (leq(sK32, n2) | ~ sP4), inference(cnf_transformation, [], [f223])).
fof(f445, plain, (~ spl35_1 | spl35_4), inference(avatar_split_clause, [], [f339, f442, f428])).
fof(f339, plain, (leq(n0, sK33) | ~ sP4), inference(cnf_transformation, [], [f223])).
fof(f440, plain, (~ spl35_1 | spl35_3), inference(avatar_split_clause, [], [f340, f437, f428])).
fof(f340, plain, (leq(sK33, n3) | ~ sP4), inference(cnf_transformation, [], [f223])).
fof(f435, plain, (~ spl35_1 | ~ spl35_2), inference(avatar_split_clause, [], [f341, f432, f428])).
fof(f341, plain, (~ (init = a_select3(simplex7_init, sK33, sK32)) | ~ sP4), inference(cnf_transformation, [], [f223])).