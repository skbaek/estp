fof(f585115, plain, $false, inference(avatar_sat_refutation, [], [f530, f535, f540, f549, f554, f559, f568, f573, f578, f583, f588, f653, f658, f663, f668, f669, f670, f671, f672, f673, f674, f675, f676, f677, f678, f679, f680, f681, f682, f683, f684, f685, f1901, f1974, f2017, f2338, f567616, f567726, f567747, f567790, f567809, f567830, f567865, f567887, f567907, f581641, f582330, f582432, f582438, f582449, f585071, f585090, f585112])).
fof(f585112, plain, (spl39_10 | ~ spl39_15 | ~ spl39_177 | ~ spl39_179 | ~ spl39_3144), inference(avatar_contradiction_clause, [], [f585111])).
fof(f585111, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_177 | ~ spl39_179 | ~ spl39_3144)), inference(subsumption_resolution, [], [f585107, f582429])).
fof(f582429, plain, ((s_best7_init = a_select3(simplex7_init, n1, n1)) | ~ spl39_3144), inference(avatar_component_clause, [], [f582427])).
fof(f582427, plain, (spl39_3144 <=> (s_best7_init = a_select3(simplex7_init, n1, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_3144])])).
fof(f585107, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_177 | ~ spl39_179)), inference(backward_demodulation, [], [f584886, f2220])).
fof(f2220, plain, ((n1 = sK37) | ~ spl39_179), inference(avatar_component_clause, [], [f2218])).
fof(f2218, plain, (spl39_179 <=> (n1 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_179])])).
fof(f584886, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_177)), inference(forward_demodulation, [], [f1161, f2210])).
fof(f2210, plain, ((n1 = sK36) | ~ spl39_177), inference(avatar_component_clause, [], [f2208])).
fof(f2208, plain, (spl39_177 <=> (n1 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_177])])).
fof(f1161, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, sK36)) | (spl39_10 | ~ spl39_15)), inference(forward_demodulation, [], [f567, f591])).
fof(f591, plain, ((s_best7_init = s_worst7_init) | ~ spl39_15), inference(avatar_component_clause, [], [f590])).
fof(f590, plain, (spl39_15 <=> (s_best7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_15])])).
fof(f567, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | spl39_10), inference(avatar_component_clause, [], [f565])).
fof(f565, plain, (spl39_10 <=> (s_worst7_init = a_select3(simplex7_init, sK37, sK36))), introduced(avatar_definition, [new_symbols(naming, [spl39_10])])).
fof(f585090, plain, (spl39_10 | ~ spl39_15 | ~ spl39_65 | ~ spl39_177 | ~ spl39_3145), inference(avatar_contradiction_clause, [], [f585089])).
fof(f585089, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_65 | ~ spl39_177 | ~ spl39_3145)), inference(subsumption_resolution, [], [f585087, f582444])).
fof(f582444, plain, ((s_best7_init = a_select3(simplex7_init, n3, n1)) | ~ spl39_3145), inference(avatar_component_clause, [], [f582442])).
fof(f582442, plain, (spl39_3145 <=> (s_best7_init = a_select3(simplex7_init, n3, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_3145])])).
fof(f585087, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_65 | ~ spl39_177)), inference(backward_demodulation, [], [f584886, f1171])).
fof(f1171, plain, ((n3 = sK37) | ~ spl39_65), inference(avatar_component_clause, [], [f1169])).
fof(f1169, plain, (spl39_65 <=> (n3 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_65])])).
fof(f585071, plain, (spl39_10 | ~ spl39_15 | ~ spl39_71 | ~ spl39_177 | ~ spl39_3135), inference(avatar_contradiction_clause, [], [f585070])).
fof(f585070, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_71 | ~ spl39_177 | ~ spl39_3135)), inference(subsumption_resolution, [], [f585066, f581528])).
fof(f581528, plain, ((s_best7_init = a_select3(simplex7_init, n0, n1)) | ~ spl39_3135), inference(avatar_component_clause, [], [f581526])).
fof(f581526, plain, (spl39_3135 <=> (s_best7_init = a_select3(simplex7_init, n0, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_3135])])).
fof(f585066, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_71 | ~ spl39_177)), inference(backward_demodulation, [], [f584886, f1217])).
fof(f1217, plain, ((n0 = sK37) | ~ spl39_71), inference(avatar_component_clause, [], [f1215])).
fof(f1215, plain, (spl39_71 <=> (n0 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_71])])).
fof(f582449, plain, (~ spl39_3133 | spl39_3145 | ~ spl39_15 | ~ spl39_16), inference(avatar_split_clause, [], [f582448, f594, f590, f582442, f581479])).
fof(f581479, plain, (spl39_3133 <=> leq(n1, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_3133])])).
fof(f594, plain, (spl39_16 <=> (s_sworst7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_16])])).
fof(f582448, plain, ((s_best7_init = a_select3(simplex7_init, n3, n1)) | ~ leq(n1, n2) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f567606, f491])).
fof(f491, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f446, f332])).
fof(f332, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', succ_plus_1_r)).
fof(f446, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', successor_1)).
fof(f567606, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f567546, f491])).
fof(f567546, plain, (~ leq(plus(n0, n1), n2) | (s_best7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2430, f713])).
fof(f713, plain, ! [X0] : leq(X0, plus(X0, n1)), inference(resolution, [], [f257, f451])).
fof(f451, plain, ! [X0] : gt(plus(X0, n1), X0), inference(definition_unfolding, [], [f261, f332])).
fof(f261, plain, ! [X0] : gt(succ(X0), X0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : gt(succ(X0), X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', gt_succ)).
fof(f257, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', leq_gt1)).
fof(f2430, plain, (! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (s_best7_init = a_select3(simplex7_init, n3, X5))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2404, f255])).
fof(f255, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', reflexivity_leq)).
fof(f2404, plain, (! [X5] : (~ leq(n3, n3) | (s_best7_init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f707, f716])).
fof(f716, plain, leq(n0, n3), inference(resolution, [], [f257, f422])).
fof(f422, plain, gt(n3, n0), inference(cnf_transformation, [], [f75])).
fof(f75, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', gt_3_0)).
fof(f707, plain, (! [X4, X5] : (~ leq(n0, X5) | ~ leq(X5, n3) | (s_best7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f699, f703])).
fof(f703, plain, ((s_best7_init = s_sworst7_init) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f595, f591])).
fof(f595, plain, ((s_sworst7_init = s_worst7_init) | ~ spl39_16), inference(avatar_component_clause, [], [f594])).
fof(f699, plain, (! [X4, X5] : ((s_sworst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)) | ~ spl39_16), inference(backward_demodulation, [], [f485, f595])).
fof(f485, plain, ! [X4, X5] : ((s_worst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(definition_unfolding, [], [f389, f376])).
fof(f376, plain, (init = s_worst7_init), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK38])], [f249, f250])).
fof(f250, plain, (? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) => (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38))), introduced(choice_axiom, [])).
fof(f249, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(rectify, [], [f188])).
fof(f188, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(definition_folding, [], [f165, e187, e186, e185])).
fof(f185, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(usedef, [], [e185])).
fof(e185, plain, (sP4 <=> ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f186, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(usedef, [], [e186])).
fof(e186, plain, (sP5 <=> ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f187, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(usedef, [], [e187])).
fof(e187, plain, (sP6 <=> ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f165, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(flattening, [], [f164])).
fof(f164, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & (leq(X5, minus(n3, n1)) & leq(n0, X5))) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & (leq(X6, n2) & leq(n0, X6))) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & (leq(X7, n3) & leq(n0, X7))) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & (leq(X9, n3) & leq(n0, X9))) & (leq(X8, n2) & leq(n0, X8))) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & ((((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | (~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0))) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | (~ leq(X1, n2) | ~ leq(n0, X1))) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | (~ leq(X2, n3) | ~ leq(n0, X2))) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | (~ leq(X4, n3) | ~ leq(n0, X4))) | (~ leq(X3, n2) | ~ leq(n0, X3))) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(ennf_transformation, [], [f122])).
fof(f122, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X0] : ((leq(X0, minus(n3, n1)) & leq(n0, X0)) => (init = a_select2(s_try7_init, X0))) & ! [X1] : ((leq(X1, n2) & leq(n0, X1)) => (init = a_select2(s_center7_init, X1))) & ! [X2] : ((leq(X2, n3) & leq(n0, X2)) => (init = a_select2(s_values7_init, X2))) & ! [X3] : ((leq(X3, n2) & leq(n0, X3)) => ! [X4] : ((leq(X4, n3) & leq(n0, X4)) => (init = a_select3(simplex7_init, X4, X3)))) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X5] : ((leq(X5, minus(n3, n1)) & leq(n0, X5)) => (init = a_select2(s_try7_init, X5))) & ! [X6] : ((leq(X6, n2) & leq(n0, X6)) => (init = a_select2(s_center7_init, X6))) & ! [X7] : ((leq(X7, n3) & leq(n0, X7)) => (init = a_select2(s_values7_init, X7))) & ! [X8] : ((leq(X8, n2) & leq(n0, X8)) => ! [X9] : ((leq(X9, n3) & leq(n0, X9)) => (init = a_select3(simplex7_init, X9, X8)))) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', gauss_init_0013)).
fof(f389, plain, ! [X4, X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(cnf_transformation, [], [f251])).
fof(f582438, plain, (~ spl39_3133 | spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_177 | ~ spl39_186), inference(avatar_split_clause, [], [f582437, f2484, f2208, f594, f590, f565, f581479])).
fof(f2484, plain, (spl39_186 <=> (n2 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_186])])).
fof(f582437, plain, (~ leq(n1, n2) | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_177 | ~ spl39_186)), inference(subsumption_resolution, [], [f582436, f573010])).
fof(f573010, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_177 | ~ spl39_186)), inference(backward_demodulation, [], [f572982, f2210])).
fof(f572982, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_186)), inference(backward_demodulation, [], [f1161, f2486])).
fof(f2486, plain, ((n2 = sK37) | ~ spl39_186), inference(avatar_component_clause, [], [f2484])).
fof(f582436, plain, ((s_best7_init = a_select3(simplex7_init, n2, n1)) | ~ leq(n1, n2) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f567481, f491])).
fof(f567481, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f567430, f491])).
fof(f567430, plain, (~ leq(plus(n0, n1), n2) | (s_best7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2429, f713])).
fof(f2429, plain, (! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (s_best7_init = a_select3(simplex7_init, n2, X4))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2403, f735])).
fof(f735, plain, leq(n2, n3), inference(resolution, [], [f257, f433])).
fof(f433, plain, gt(n3, n2), inference(cnf_transformation, [], [f86])).
fof(f86, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', gt_3_2)).
fof(f2403, plain, (! [X4] : (~ leq(n2, n3) | (s_best7_init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f707, f715])).
fof(f715, plain, leq(n0, n2), inference(resolution, [], [f257, f421])).
fof(f421, plain, gt(n2, n0), inference(cnf_transformation, [], [f74])).
fof(f74, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', gt_2_0)).
fof(f582432, plain, (~ spl39_3133 | spl39_3144 | ~ spl39_15 | ~ spl39_16), inference(avatar_split_clause, [], [f582431, f594, f590, f582427, f581479])).
fof(f582431, plain, ((s_best7_init = a_select3(simplex7_init, n1, n1)) | ~ leq(n1, n2) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f567365, f491])).
fof(f567365, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f567314, f491])).
fof(f567314, plain, (~ leq(plus(n0, n1), n2) | (s_best7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2428, f713])).
fof(f2428, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (s_best7_init = a_select3(simplex7_init, n1, X3))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2402, f730])).
fof(f730, plain, leq(n1, n3), inference(resolution, [], [f257, f428])).
fof(f428, plain, gt(n3, n1), inference(cnf_transformation, [], [f81])).
fof(f81, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', gt_3_1)).
fof(f2402, plain, (! [X3] : (~ leq(n1, n3) | (s_best7_init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f707, f714])).
fof(f714, plain, leq(n0, n1), inference(resolution, [], [f257, f420])).
fof(f420, plain, gt(n1, n0), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', gt_1_0)).
fof(f582330, plain, spl39_3133, inference(avatar_split_clause, [], [f522589, f581479])).
fof(f522589, plain, leq(n1, n2), inference(forward_demodulation, [], [f522588, f766])).
fof(f766, plain, (n2 = minus(n3, n1)), inference(superposition, [], [f465, f698])).
fof(f698, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f697, f693])).
fof(f693, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f492, f491])).
fof(f492, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f447, f332, f332])).
fof(f447, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f100])).
fof(f100, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', successor_2)).
fof(f697, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f696, f491])).
fof(f696, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f493, f456])).
fof(f456, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f333, f332])).
fof(f333, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', succ_plus_1_l)).
fof(f493, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f448, f332, f332, f332])).
fof(f448, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f101])).
fof(f101, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', successor_3)).
fof(f465, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f343, f342, f332])).
fof(f342, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', pred_minus_1)).
fof(f343, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', pred_succ)).
fof(f522588, plain, leq(n1, minus(n3, n1)), inference(subsumption_resolution, [], [f522423, f255])).
fof(f522423, plain, (leq(n1, minus(n3, n1)) | ~ leq(n1, n1)), inference(resolution, [], [f29165, f7734])).
fof(f7734, plain, ! [X2] : (leq(plus(X2, n1), n3) | ~ leq(X2, n1)), inference(resolution, [], [f1609, f797])).
fof(f797, plain, ! [X5] : (~ leq(X5, n2) | leq(X5, n3)), inference(superposition, [], [f452, f698])).
fof(f452, plain, ! [X0, X1] : (leq(X0, plus(X1, n1)) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f262, f332])).
fof(f262, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0, X1] : (leq(X0, X1) => leq(X0, succ(X1))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', leq_succ)).
fof(f1609, plain, ! [X4] : (leq(plus(X4, n1), n2) | ~ leq(X4, n1)), inference(superposition, [], [f467, f693])).
fof(f467, plain, ! [X0, X1] : (leq(plus(X0, n1), plus(X1, n1)) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f346, f332, f332])).
fof(f346, plain, ! [X0, X1] : (leq(succ(X0), succ(X1)) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ! [X0, X1] : ((leq(succ(X0), succ(X1)) | ~ leq(X0, X1)) & (leq(X0, X1) | ~ leq(succ(X0), succ(X1)))), inference(nnf_transformation, [], [f42])).
fof(f42, plain, ! [X0, X1] : (leq(succ(X0), succ(X1)) <=> leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', leq_succ_succ)).
fof(f29165, plain, ! [X28, X29] : (~ leq(plus(n1, X29), X28) | leq(X29, minus(X28, n1))), inference(superposition, [], [f1625, f466])).
fof(f466, plain, ! [X0] : (plus(minus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f344, f332, f342])).
fof(f344, plain, ! [X0] : (succ(pred(X0)) = X0), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (succ(pred(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', succ_pred)).
fof(f1625, plain, ! [X0, X1] : (~ leq(plus(n1, X0), plus(X1, n1)) | leq(X0, X1)), inference(superposition, [], [f468, f456])).
fof(f468, plain, ! [X0, X1] : (~ leq(plus(X0, n1), plus(X1, n1)) | leq(X0, X1)), inference(definition_unfolding, [], [f345, f332, f332])).
fof(f345, plain, ! [X0, X1] : (leq(X0, X1) | ~ leq(succ(X0), succ(X1))), inference(cnf_transformation, [], [f231])).
fof(f581641, plain, (~ spl39_3133 | spl39_3135 | ~ spl39_15 | ~ spl39_16), inference(avatar_split_clause, [], [f581640, f594, f590, f581526, f581479])).
fof(f581640, plain, ((s_best7_init = a_select3(simplex7_init, n0, n1)) | ~ leq(n1, n2) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f358895, f491])).
fof(f358895, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n0, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f358877, f491])).
fof(f358877, plain, (~ leq(plus(n0, n1), n2) | (s_best7_init = a_select3(simplex7_init, n0, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2435, f713])).
fof(f2435, plain, (! [X42] : (~ leq(n0, X42) | ~ leq(X42, n2) | (s_best7_init = a_select3(simplex7_init, n0, X42))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2425, f716])).
fof(f2425, plain, (! [X42] : (~ leq(n0, n3) | (s_best7_init = a_select3(simplex7_init, n0, X42)) | ~ leq(X42, n2) | ~ leq(n0, X42)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f707, f255])).
fof(f567907, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_73), inference(avatar_contradiction_clause, [], [f567906])).
fof(f567906, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_73)), inference(subsumption_resolution, [], [f567904, f31391])).
fof(f31391, plain, ((s_best7_init = a_select3(simplex7_init, n0, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f31351, f255])).
fof(f31351, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n0, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2435, f715])).
fof(f567904, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_71 | ~ spl39_73)), inference(backward_demodulation, [], [f567869, f1217])).
fof(f567869, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_73)), inference(forward_demodulation, [], [f1161, f1234])).
fof(f1234, plain, ((n2 = sK36) | ~ spl39_73), inference(avatar_component_clause, [], [f1232])).
fof(f1232, plain, (spl39_73 <=> (n2 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_73])])).
fof(f567887, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_65 | ~ spl39_73), inference(avatar_contradiction_clause, [], [f567886])).
fof(f567886, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_65 | ~ spl39_73)), inference(subsumption_resolution, [], [f567884, f567590])).
fof(f567590, plain, ((s_best7_init = a_select3(simplex7_init, n3, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f567493, f255])).
fof(f567493, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n3, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2430, f715])).
fof(f567884, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_65 | ~ spl39_73)), inference(backward_demodulation, [], [f567869, f1171])).
fof(f567865, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_186), inference(avatar_contradiction_clause, [], [f567864])).
fof(f567864, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_186)), inference(subsumption_resolution, [], [f567862, f567472])).
fof(f567472, plain, ((s_best7_init = a_select3(simplex7_init, n2, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f567377, f255])).
fof(f567377, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n2, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2429, f715])).
fof(f567862, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_73 | ~ spl39_186)), inference(backward_demodulation, [], [f567835, f1234])).
fof(f567835, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_186)), inference(backward_demodulation, [], [f1161, f2486])).
fof(f567830, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_65 | ~ spl39_75), inference(avatar_contradiction_clause, [], [f567829])).
fof(f567829, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_65 | ~ spl39_75)), inference(subsumption_resolution, [], [f567826, f567591])).
fof(f567591, plain, ((s_best7_init = a_select3(simplex7_init, n3, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f567520, f715])).
fof(f567520, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n3, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2430, f255])).
fof(f567826, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_65 | ~ spl39_75)), inference(backward_demodulation, [], [f567753, f1171])).
fof(f567753, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_75)), inference(backward_demodulation, [], [f1161, f1245])).
fof(f1245, plain, ((n0 = sK36) | ~ spl39_75), inference(avatar_component_clause, [], [f1243])).
fof(f1243, plain, (spl39_75 <=> (n0 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_75])])).
fof(f567809, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_75 | ~ spl39_186), inference(avatar_contradiction_clause, [], [f567808])).
fof(f567808, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_75 | ~ spl39_186)), inference(subsumption_resolution, [], [f567806, f567473])).
fof(f567473, plain, ((s_best7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f567404, f715])).
fof(f567404, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2429, f255])).
fof(f567806, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_75 | ~ spl39_186)), inference(backward_demodulation, [], [f567753, f2486])).
fof(f567790, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_75), inference(avatar_contradiction_clause, [], [f567789])).
fof(f567789, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_75)), inference(subsumption_resolution, [], [f567785, f31392])).
fof(f31392, plain, ((s_best7_init = a_select3(simplex7_init, n0, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f31369, f715])).
fof(f31369, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n0, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2435, f255])).
fof(f567785, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_71 | ~ spl39_75)), inference(backward_demodulation, [], [f567753, f1217])).
fof(f567747, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_179), inference(avatar_contradiction_clause, [], [f567746])).
fof(f567746, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_179)), inference(subsumption_resolution, [], [f567742, f567356])).
fof(f567356, plain, ((s_best7_init = a_select3(simplex7_init, n1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f567261, f255])).
fof(f567261, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2428, f715])).
fof(f567742, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_73 | ~ spl39_179)), inference(backward_demodulation, [], [f567693, f1234])).
fof(f567693, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_179)), inference(backward_demodulation, [], [f1161, f2220])).
fof(f567726, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_75 | ~ spl39_179), inference(avatar_contradiction_clause, [], [f567725])).
fof(f567725, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_75 | ~ spl39_179)), inference(subsumption_resolution, [], [f567723, f567357])).
fof(f567357, plain, ((s_best7_init = a_select3(simplex7_init, n1, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f567288, f715])).
fof(f567288, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n1, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2428, f255])).
fof(f567723, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_75 | ~ spl39_179)), inference(backward_demodulation, [], [f567693, f1245])).
fof(f567616, plain, (spl39_65 | spl39_71 | spl39_179 | spl39_186 | ~ spl39_11 | ~ spl39_12), inference(avatar_split_clause, [], [f567615, f575, f570, f2484, f2218, f1215, f1169])).
fof(f570, plain, (spl39_11 <=> leq(sK37, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_11])])).
fof(f575, plain, (spl39_12 <=> leq(n0, sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_12])])).
fof(f567615, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | (n3 = sK37) | (~ spl39_11 | ~ spl39_12)), inference(subsumption_resolution, [], [f2458, f572])).
fof(f572, plain, (leq(sK37, n3) | ~ spl39_11), inference(avatar_component_clause, [], [f570])).
fof(f2458, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | ~ leq(sK37, n3) | (n3 = sK37) | ~ spl39_12), inference(resolution, [], [f443, f577])).
fof(f577, plain, (leq(n0, sK37) | ~ spl39_12), inference(avatar_component_clause, [], [f575])).
fof(f443, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f176])).
fof(f176, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f96])).
fof(f96, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', finite_domain_3)).
fof(f2338, plain, (spl39_73 | spl39_75 | spl39_177 | ~ spl39_13 | ~ spl39_14), inference(avatar_split_clause, [], [f2337, f585, f580, f2208, f1243, f1232])).
fof(f580, plain, (spl39_13 <=> leq(sK36, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_13])])).
fof(f585, plain, (spl39_14 <=> leq(n0, sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_14])])).
fof(f2337, plain, ((n1 = sK36) | (n0 = sK36) | (n2 = sK36) | (~ spl39_13 | ~ spl39_14)), inference(subsumption_resolution, [], [f2331, f582])).
fof(f582, plain, (leq(sK36, n2) | ~ spl39_13), inference(avatar_component_clause, [], [f580])).
fof(f2331, plain, ((n1 = sK36) | (n0 = sK36) | ~ leq(sK36, n2) | (n2 = sK36) | ~ spl39_14), inference(resolution, [], [f442, f587])).
fof(f587, plain, (leq(n0, sK36) | ~ spl39_14), inference(avatar_component_clause, [], [f585])).
fof(f442, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f174])).
fof(f174, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f95])).
fof(f95, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV025+1.p', finite_domain_2)).
fof(f2017, plain, (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16), inference(avatar_contradiction_clause, [], [f2016])).
fof(f2016, plain, ($false | (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2015, f1979])).
fof(f1979, plain, (~ (s_best7_init = a_select2(s_center7_init, sK34)) | (spl39_2 | ~ spl39_15)), inference(forward_demodulation, [], [f529, f591])).
fof(f529, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | spl39_2), inference(avatar_component_clause, [], [f527])).
fof(f527, plain, (spl39_2 <=> (s_worst7_init = a_select2(s_center7_init, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl39_2])])).
fof(f2015, plain, ((s_best7_init = a_select2(s_center7_init, sK34)) | (~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2001, f534])).
fof(f534, plain, (leq(sK34, n2) | ~ spl39_3), inference(avatar_component_clause, [], [f532])).
fof(f532, plain, (spl39_3 <=> leq(sK34, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_3])])).
fof(f2001, plain, (~ leq(sK34, n2) | (s_best7_init = a_select2(s_center7_init, sK34)) | (~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f539, f705])).
fof(f705, plain, (! [X2] : (~ leq(n0, X2) | ~ leq(X2, n2) | (s_best7_init = a_select2(s_center7_init, X2))) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f701, f703])).
fof(f701, plain, (! [X2] : ((s_sworst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) | ~ spl39_16), inference(backward_demodulation, [], [f483, f595])).
fof(f483, plain, ! [X2] : ((s_worst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(definition_unfolding, [], [f391, f376])).
fof(f391, plain, ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f251])).
fof(f539, plain, (leq(n0, sK34) | ~ spl39_4), inference(avatar_component_clause, [], [f537])).
fof(f537, plain, (spl39_4 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl39_4])])).
fof(f1974, plain, (~ spl39_15 | ~ spl39_16 | spl39_27 | ~ spl39_31 | ~ spl39_32), inference(avatar_contradiction_clause, [], [f1973])).
fof(f1973, plain, ($false | (~ spl39_15 | ~ spl39_16 | spl39_27 | ~ spl39_31 | ~ spl39_32)), inference(subsumption_resolution, [], [f1972, f1916])).
fof(f1916, plain, (leq(sK38, n2) | ~ spl39_31), inference(forward_demodulation, [], [f657, f766])).
fof(f657, plain, (leq(sK38, minus(n3, n1)) | ~ spl39_31), inference(avatar_component_clause, [], [f655])).
fof(f655, plain, (spl39_31 <=> leq(sK38, minus(n3, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_31])])).
fof(f1972, plain, (~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_16 | spl39_27 | ~ spl39_32)), inference(subsumption_resolution, [], [f1966, f1174])).
fof(f1174, plain, (~ (s_best7_init = a_select2(s_try7_init, sK38)) | (~ spl39_15 | spl39_27)), inference(forward_demodulation, [], [f640, f591])).
fof(f640, plain, (~ (s_worst7_init = a_select2(s_try7_init, sK38)) | spl39_27), inference(avatar_component_clause, [], [f638])).
fof(f638, plain, (spl39_27 <=> (s_worst7_init = a_select2(s_try7_init, sK38))), introduced(avatar_definition, [new_symbols(naming, [spl39_27])])).
fof(f1966, plain, ((s_best7_init = a_select2(s_try7_init, sK38)) | ~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_16 | ~ spl39_32)), inference(resolution, [], [f769, f662])).
fof(f662, plain, (leq(n0, sK38) | ~ spl39_32), inference(avatar_component_clause, [], [f660])).
fof(f660, plain, (spl39_32 <=> leq(n0, sK38)), introduced(avatar_definition, [new_symbols(naming, [spl39_32])])).
fof(f769, plain, (! [X1] : (~ leq(n0, X1) | (s_best7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f704, f766])).
fof(f704, plain, (! [X1] : ((s_best7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f702, f703])).
fof(f702, plain, (! [X1] : ((s_sworst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | ~ spl39_16), inference(backward_demodulation, [], [f482, f595])).
fof(f482, plain, ! [X1] : ((s_worst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(definition_unfolding, [], [f392, f376])).
fof(f392, plain, ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f251])).
fof(f1901, plain, (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16), inference(avatar_contradiction_clause, [], [f1900])).
fof(f1900, plain, ($false | (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1899, f708])).
fof(f708, plain, (~ (s_best7_init = a_select2(s_values7_init, sK35)) | (spl39_6 | ~ spl39_15)), inference(forward_demodulation, [], [f548, f591])).
fof(f548, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | spl39_6), inference(avatar_component_clause, [], [f546])).
fof(f546, plain, (spl39_6 <=> (s_worst7_init = a_select2(s_values7_init, sK35))), introduced(avatar_definition, [new_symbols(naming, [spl39_6])])).
fof(f1899, plain, ((s_best7_init = a_select2(s_values7_init, sK35)) | (~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1894, f553])).
fof(f553, plain, (leq(sK35, n3) | ~ spl39_7), inference(avatar_component_clause, [], [f551])).
fof(f551, plain, (spl39_7 <=> leq(sK35, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_7])])).
fof(f1894, plain, (~ leq(sK35, n3) | (s_best7_init = a_select2(s_values7_init, sK35)) | (~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f558, f706])).
fof(f706, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n3) | (s_best7_init = a_select2(s_values7_init, X3))) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f700, f703])).
fof(f700, plain, (! [X3] : ((s_sworst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ spl39_16), inference(backward_demodulation, [], [f484, f595])).
fof(f484, plain, ! [X3] : ((s_worst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(definition_unfolding, [], [f390, f376])).
fof(f390, plain, ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(cnf_transformation, [], [f251])).
fof(f558, plain, (leq(n0, sK35) | ~ spl39_8), inference(avatar_component_clause, [], [f556])).
fof(f556, plain, (spl39_8 <=> leq(n0, sK35)), introduced(avatar_definition, [new_symbols(naming, [spl39_8])])).
fof(f685, plain, spl39_15, inference(avatar_split_clause, [], [f487, f590])).
fof(f487, plain, (s_best7_init = s_worst7_init), inference(definition_unfolding, [], [f374, f376])).
fof(f374, plain, (init = s_best7_init), inference(cnf_transformation, [], [f251])).
fof(f684, plain, spl39_16, inference(avatar_split_clause, [], [f486, f594])).
fof(f486, plain, (s_sworst7_init = s_worst7_init), inference(definition_unfolding, [], [f375, f376])).
fof(f375, plain, (init = s_sworst7_init), inference(cnf_transformation, [], [f251])).
fof(f683, plain, spl39_17, inference(avatar_split_clause, [], [f377, f598])).
fof(f598, plain, (spl39_17 <=> leq(n0, s_best7)), introduced(avatar_definition, [new_symbols(naming, [spl39_17])])).
fof(f377, plain, leq(n0, s_best7), inference(cnf_transformation, [], [f251])).
fof(f682, plain, spl39_18, inference(avatar_split_clause, [], [f378, f602])).
fof(f602, plain, (spl39_18 <=> leq(n0, s_sworst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_18])])).
fof(f378, plain, leq(n0, s_sworst7), inference(cnf_transformation, [], [f251])).
fof(f681, plain, spl39_19, inference(avatar_split_clause, [], [f379, f606])).
fof(f606, plain, (spl39_19 <=> leq(n0, s_worst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_19])])).
fof(f379, plain, leq(n0, s_worst7), inference(cnf_transformation, [], [f251])).
fof(f680, plain, spl39_20, inference(avatar_split_clause, [], [f381, f610])).
fof(f610, plain, (spl39_20 <=> leq(n0, pv19)), introduced(avatar_definition, [new_symbols(naming, [spl39_20])])).
fof(f381, plain, leq(n0, pv19), inference(cnf_transformation, [], [f251])).
fof(f679, plain, spl39_21, inference(avatar_split_clause, [], [f382, f614])).
fof(f614, plain, (spl39_21 <=> leq(n0, pv20)), introduced(avatar_definition, [new_symbols(naming, [spl39_21])])).
fof(f382, plain, leq(n0, pv20), inference(cnf_transformation, [], [f251])).
fof(f678, plain, spl39_22, inference(avatar_split_clause, [], [f383, f618])).
fof(f618, plain, (spl39_22 <=> leq(s_best7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_22])])).
fof(f383, plain, leq(s_best7, n3), inference(cnf_transformation, [], [f251])).
fof(f677, plain, spl39_23, inference(avatar_split_clause, [], [f384, f622])).
fof(f622, plain, (spl39_23 <=> leq(s_sworst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_23])])).
fof(f384, plain, leq(s_sworst7, n3), inference(cnf_transformation, [], [f251])).
fof(f676, plain, spl39_24, inference(avatar_split_clause, [], [f385, f626])).
fof(f626, plain, (spl39_24 <=> leq(s_worst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_24])])).
fof(f385, plain, leq(s_worst7, n3), inference(cnf_transformation, [], [f251])).
fof(f675, plain, spl39_25, inference(avatar_split_clause, [], [f387, f630])).
fof(f630, plain, (spl39_25 <=> leq(pv19, minus(n410, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_25])])).
fof(f387, plain, leq(pv19, minus(n410, n1)), inference(cnf_transformation, [], [f251])).
fof(f674, plain, spl39_26, inference(avatar_split_clause, [], [f388, f634])).
fof(f634, plain, (spl39_26 <=> leq(pv20, minus(n330, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_26])])).
fof(f388, plain, leq(pv20, minus(n330, n1)), inference(cnf_transformation, [], [f251])).
fof(f673, plain, (~ spl39_33 | spl39_28), inference(avatar_split_clause, [], [f481, f642, f665])).
fof(f665, plain, (spl39_33 <=> gt(loopcounter, n1)), introduced(avatar_definition, [new_symbols(naming, [spl39_33])])).
fof(f642, plain, (spl39_28 <=> (s_worst7_init = pvar1400_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_28])])).
fof(f481, plain, ((s_worst7_init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f393, f376])).
fof(f393, plain, ((init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f672, plain, (~ spl39_33 | spl39_29), inference(avatar_split_clause, [], [f480, f646, f665])).
fof(f646, plain, (spl39_29 <=> (s_worst7_init = pvar1401_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_29])])).
fof(f480, plain, ((s_worst7_init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f394, f376])).
fof(f394, plain, ((init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f671, plain, (~ spl39_33 | spl39_30), inference(avatar_split_clause, [], [f479, f650, f665])).
fof(f650, plain, (spl39_30 <=> (s_worst7_init = pvar1402_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_30])])).
fof(f479, plain, ((s_worst7_init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f395, f376])).
fof(f395, plain, ((init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f670, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | spl39_9 | spl39_5 | spl39_1 | spl39_32 | spl39_33), inference(avatar_split_clause, [], [f498, f665, f660, f523, f542, f561, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f561, plain, (spl39_9 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl39_9])])).
fof(f542, plain, (spl39_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl39_5])])).
fof(f523, plain, (spl39_1 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl39_1])])).
fof(f498, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f497])).
fof(f497, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f478])).
fof(f478, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f396, f376, f376, f376, f376, f376])).
fof(f396, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f669, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | spl39_9 | spl39_5 | spl39_1 | spl39_31 | spl39_33), inference(avatar_split_clause, [], [f500, f665, f655, f523, f542, f561, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f500, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f499])).
fof(f499, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f477])).
fof(f477, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f397, f376, f376, f376, f376, f376])).
fof(f397, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f668, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_27 | spl39_33), inference(avatar_split_clause, [], [f502, f665, f638, f523, f542, f561, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f502, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f501])).
fof(f501, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f476])).
fof(f476, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f398, f376, f376, f376, f376, f376, f376])).
fof(f398, plain, (gt(loopcounter, n1) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f663, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | spl39_9 | spl39_5 | spl39_1 | spl39_32 | ~ spl39_28 | ~ spl39_29 | ~ spl39_30), inference(avatar_split_clause, [], [f504, f650, f646, f642, f660, f523, f542, f561, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f504, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f503])).
fof(f503, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f475])).
fof(f475, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f399, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f399, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f658, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | spl39_9 | spl39_5 | spl39_1 | spl39_31 | ~ spl39_28 | ~ spl39_29 | ~ spl39_30), inference(avatar_split_clause, [], [f506, f650, f646, f642, f655, f523, f542, f561, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f506, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f505])).
fof(f505, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f474])).
fof(f474, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f400, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f400, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f653, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_27 | ~ spl39_28 | ~ spl39_29 | ~ spl39_30), inference(avatar_split_clause, [], [f508, f650, f646, f642, f638, f523, f542, f561, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f508, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f507])).
fof(f507, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f473])).
fof(f473, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f401, f376, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f401, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f588, plain, (~ spl39_9 | spl39_14), inference(avatar_split_clause, [], [f368, f585, f561])).
fof(f368, plain, (leq(n0, sK36) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f248, plain, (((~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37)) & leq(sK36, n2) & leq(n0, sK36)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK36, sK37])], [f245, f247, f246])).
fof(f246, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) & leq(sK36, n2) & leq(n0, sK36))), introduced(choice_axiom, [])).
fof(f247, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37))), introduced(choice_axiom, [])).
fof(f245, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f244])).
fof(f244, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(nnf_transformation, [], [f185])).
fof(f583, plain, (~ spl39_9 | spl39_13), inference(avatar_split_clause, [], [f369, f580, f561])).
fof(f369, plain, (leq(sK36, n2) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f578, plain, (~ spl39_9 | spl39_12), inference(avatar_split_clause, [], [f370, f575, f561])).
fof(f370, plain, (leq(n0, sK37) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f573, plain, (~ spl39_9 | spl39_11), inference(avatar_split_clause, [], [f371, f570, f561])).
fof(f371, plain, (leq(sK37, n3) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f568, plain, (~ spl39_9 | ~ spl39_10), inference(avatar_split_clause, [], [f472, f565, f561])).
fof(f472, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(definition_unfolding, [], [f372, f376])).
fof(f372, plain, (~ (init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f559, plain, (~ spl39_5 | spl39_8), inference(avatar_split_clause, [], [f365, f556, f542])).
fof(f365, plain, (leq(n0, sK35) | ~ sP5), inference(cnf_transformation, [], [f243])).
fof(f243, plain, ((~ (init = a_select2(s_values7_init, sK35)) & leq(sK35, n3) & leq(n0, sK35)) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK35])], [f241, f242])).
fof(f242, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) => (~ (init = a_select2(s_values7_init, sK35)) & leq(sK35, n3) & leq(n0, sK35))), introduced(choice_axiom, [])).
fof(f241, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) | ~ sP5), inference(rectify, [], [f240])).
fof(f240, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(nnf_transformation, [], [f186])).
fof(f554, plain, (~ spl39_5 | spl39_7), inference(avatar_split_clause, [], [f366, f551, f542])).
fof(f366, plain, (leq(sK35, n3) | ~ sP5), inference(cnf_transformation, [], [f243])).
fof(f549, plain, (~ spl39_5 | ~ spl39_6), inference(avatar_split_clause, [], [f471, f546, f542])).
fof(f471, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | ~ sP5), inference(definition_unfolding, [], [f367, f376])).
fof(f367, plain, (~ (init = a_select2(s_values7_init, sK35)) | ~ sP5), inference(cnf_transformation, [], [f243])).
fof(f540, plain, (~ spl39_1 | spl39_4), inference(avatar_split_clause, [], [f362, f537, f523])).
fof(f362, plain, (leq(n0, sK34) | ~ sP6), inference(cnf_transformation, [], [f239])).
fof(f239, plain, ((~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34)) | ~ sP6), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34])], [f237, f238])).
fof(f238, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) => (~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f237, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) | ~ sP6), inference(rectify, [], [f236])).
fof(f236, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(nnf_transformation, [], [f187])).
fof(f535, plain, (~ spl39_1 | spl39_3), inference(avatar_split_clause, [], [f363, f532, f523])).
fof(f363, plain, (leq(sK34, n2) | ~ sP6), inference(cnf_transformation, [], [f239])).
fof(f530, plain, (~ spl39_1 | ~ spl39_2), inference(avatar_split_clause, [], [f470, f527, f523])).
fof(f470, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(definition_unfolding, [], [f364, f376])).
fof(f364, plain, (~ (init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(cnf_transformation, [], [f239])).