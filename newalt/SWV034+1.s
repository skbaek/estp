fof(f556352, plain, $false, inference(avatar_sat_refutation, [], [f508, f513, f518, f527, f532, f537, f546, f551, f556, f561, f566, f619, f624, f629, f634, f635, f636, f637, f638, f639, f640, f641, f642, f643, f644, f645, f646, f647, f648, f1633, f1819, f1845, f2108, f2223, f2371, f539529, f539557, f539872, f539913, f539937, f540002, f540020, f540078, f540097, f541347, f552535, f556329, f556350])).
fof(f556350, plain, (spl39_10 | ~ spl39_15 | ~ spl39_64 | spl39_65 | ~ spl39_150), inference(avatar_contradiction_clause, [], [f556349])).
fof(f556349, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_64 | spl39_65 | ~ spl39_150)), inference(subsumption_resolution, [], [f556347, f540143])).
fof(f540143, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | spl39_65)), inference(subsumption_resolution, [], [f540142, f1183])).
fof(f1183, plain, (~ (n0 = n1) | spl39_65), inference(avatar_component_clause, [], [f1182])).
fof(f1182, plain, (spl39_65 <=> (n0 = n1)), introduced(avatar_definition, [new_symbols(naming, [spl39_65])])).
fof(f540142, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n0)) | (n0 = n1) | ~ spl39_15), inference(subsumption_resolution, [], [f540141, f13660])).
fof(f13660, plain, ~ gt(n0, n1), inference(superposition, [], [f5691, f469])).
fof(f469, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f425, f324])).
fof(f324, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', succ_plus_1_r)).
fof(f425, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f91])).
fof(f91, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', successor_1)).
fof(f5691, plain, ! [X0] : ~ gt(X0, plus(X0, n1)), inference(forward_demodulation, [], [f5680, f1280])).
fof(f1280, plain, ! [X2] : (plus(X2, n1) = plus(minus(X2, n1), n2)), inference(superposition, [], [f437, f446])).
fof(f446, plain, ! [X0] : (plus(minus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f336, f324, f334])).
fof(f334, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', pred_minus_1)).
fof(f336, plain, ! [X0] : (succ(pred(X0)) = X0), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (succ(pred(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', succ_pred)).
fof(f437, plain, ! [X0] : (plus(X0, n2) = plus(plus(X0, n1), n1)), inference(definition_unfolding, [], [f326, f324, f324])).
fof(f326, plain, ! [X0] : (plus(X0, n2) = succ(succ(X0))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (plus(X0, n2) = succ(succ(X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', succ_plus_2_r)).
fof(f5680, plain, ! [X0] : ~ gt(X0, plus(minus(X0, n1), n2)), inference(resolution, [], [f3666, f429])).
fof(f429, plain, ! [X0, X1] : (leq(X0, minus(X1, n1)) | ~ gt(X1, X0)), inference(definition_unfolding, [], [f252, f334])).
fof(f252, plain, ! [X0, X1] : (leq(X0, pred(X1)) | ~ gt(X1, X0)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0, X1] : ((leq(X0, pred(X1)) | ~ gt(X1, X0)) & (gt(X1, X0) | ~ leq(X0, pred(X1)))), inference(nnf_transformation, [], [f10])).
fof(f10, plain, ! [X0, X1] : (leq(X0, pred(X1)) <=> gt(X1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', leq_gt_pred)).
fof(f3666, plain, ! [X2] : ~ leq(plus(X2, n2), X2), inference(forward_demodulation, [], [f3648, f437])).
fof(f3648, plain, ! [X2] : ~ leq(plus(plus(X2, n1), n1), X2), inference(resolution, [], [f767, f432])).
fof(f432, plain, ! [X0, X1] : (leq(X0, plus(X1, n1)) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f254, f324])).
fof(f254, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0, X1] : (leq(X0, X1) => leq(X0, succ(X1))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', leq_succ)).
fof(f767, plain, ! [X2] : ~ leq(plus(X2, n1), X2), inference(resolution, [], [f434, f246])).
fof(f246, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', irreflexivity_gt)).
fof(f434, plain, ! [X0, X1] : (gt(plus(X1, n1), X0) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f255, f324])).
fof(f255, plain, ! [X0, X1] : (gt(succ(X1), X0) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f182])).
fof(f182, plain, ! [X0, X1] : ((leq(X0, X1) | ~ gt(succ(X1), X0)) & (gt(succ(X1), X0) | ~ leq(X0, X1))), inference(nnf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : (leq(X0, X1) <=> gt(succ(X1), X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', leq_succ_gt_equiv)).
fof(f540141, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n0)) | gt(n0, n1) | (n0 = n1) | ~ spl39_15), inference(subsumption_resolution, [], [f539650, f673])).
fof(f673, plain, leq(n0, n2), inference(resolution, [], [f249, f403])).
fof(f403, plain, gt(n2, n0), inference(cnf_transformation, [], [f69])).
fof(f69, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', gt_2_0)).
fof(f249, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', leq_gt1)).
fof(f539650, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, n0)) | gt(n0, n1) | (n0 = n1) | ~ spl39_15), inference(resolution, [], [f2314, f3959])).
fof(f3959, plain, ! [X0] : (leq(X0, n0) | gt(X0, n1) | (n1 = X0)), inference(resolution, [], [f751, f244])).
fof(f244, plain, ! [X0, X1] : (gt(X1, X0) | gt(X0, X1) | (X0 = X1)), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : ((X0 = X1) | gt(X1, X0) | gt(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', totality)).
fof(f751, plain, ! [X2] : (~ gt(n1, X2) | leq(X2, n0)), inference(superposition, [], [f433, f469])).
fof(f433, plain, ! [X0, X1] : (~ gt(plus(X1, n1), X0) | leq(X0, X1)), inference(definition_unfolding, [], [f256, f324])).
fof(f256, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(succ(X1), X0)), inference(cnf_transformation, [], [f182])).
fof(f2314, plain, (! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, X4))) | ~ spl39_15), inference(subsumption_resolution, [], [f2291, f690])).
fof(f690, plain, leq(n2, n3), inference(resolution, [], [f249, f413])).
fof(f413, plain, gt(n3, n2), inference(cnf_transformation, [], [f79])).
fof(f79, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', gt_3_2)).
fof(f2291, plain, (! [X4] : (~ leq(n2, n3) | (s_sworst7_init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | ~ spl39_15), inference(resolution, [], [f662, f673])).
fof(f662, plain, (! [X4, X5] : (~ leq(n0, X5) | ~ leq(X5, n3) | (s_sworst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | ~ spl39_15), inference(backward_demodulation, [], [f465, f569])).
fof(f569, plain, ((s_sworst7_init = s_worst7_init) | ~ spl39_15), inference(avatar_component_clause, [], [f568])).
fof(f568, plain, (spl39_15 <=> (s_sworst7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_15])])).
fof(f465, plain, ! [X4, X5] : ((s_worst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(definition_unfolding, [], [f376, f367])).
fof(f367, plain, (init = s_worst7_init), inference(cnf_transformation, [], [f243])).
fof(f243, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK38])], [f241, f242])).
fof(f242, plain, (? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) => (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38))), introduced(choice_axiom, [])).
fof(f241, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(rectify, [], [f180])).
fof(f180, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(definition_folding, [], [f157, e179, e178, e177])).
fof(f177, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(usedef, [], [e177])).
fof(e177, plain, (sP4 <=> ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f178, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(usedef, [], [e178])).
fof(e178, plain, (sP5 <=> ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f179, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(usedef, [], [e179])).
fof(e179, plain, (sP6 <=> ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f157, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(flattening, [], [f156])).
fof(f156, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & (leq(X5, minus(n3, n1)) & leq(n0, X5))) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & (leq(X6, n2) & leq(n0, X6))) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & (leq(X7, n3) & leq(n0, X7))) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & (leq(X9, n3) & leq(n0, X9))) & (leq(X8, n2) & leq(n0, X8))) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & ((((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | (~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0))) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | (~ leq(X1, n2) | ~ leq(n0, X1))) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | (~ leq(X2, n3) | ~ leq(n0, X2))) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | (~ leq(X4, n3) | ~ leq(n0, X4))) | (~ leq(X3, n2) | ~ leq(n0, X3))) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init))), inference(ennf_transformation, [], [f114])).
fof(f114, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X0] : ((leq(X0, minus(n3, n1)) & leq(n0, X0)) => (init = a_select2(s_try7_init, X0))) & ! [X1] : ((leq(X1, n2) & leq(n0, X1)) => (init = a_select2(s_center7_init, X1))) & ! [X2] : ((leq(X2, n3) & leq(n0, X2)) => (init = a_select2(s_values7_init, X2))) & ! [X3] : ((leq(X3, n2) & leq(n0, X3)) => ! [X4] : ((leq(X4, n3) & leq(n0, X4)) => (init = a_select3(simplex7_init, X4, X3)))) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X5] : ((leq(X5, minus(n3, n1)) & leq(n0, X5)) => (init = a_select2(s_try7_init, X5))) & ! [X6] : ((leq(X6, n2) & leq(n0, X6)) => (init = a_select2(s_center7_init, X6))) & ! [X7] : ((leq(X7, n3) & leq(n0, X7)) => (init = a_select2(s_values7_init, X7))) & ! [X8] : ((leq(X8, n2) & leq(n0, X8)) => ! [X9] : ((leq(X9, n3) & leq(n0, X9)) => (init = a_select3(simplex7_init, X9, X8)))) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init) & (init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init) & (init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init) & (init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', gauss_init_0049)).
fof(f376, plain, ! [X4, X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(cnf_transformation, [], [f243])).
fof(f556347, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_64 | ~ spl39_150)), inference(backward_demodulation, [], [f556312, f1147])).
fof(f1147, plain, ((n0 = sK36) | ~ spl39_64), inference(avatar_component_clause, [], [f1145])).
fof(f1145, plain, (spl39_64 <=> (n0 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_64])])).
fof(f556312, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_150)), inference(backward_demodulation, [], [f1069, f2370])).
fof(f2370, plain, ((n2 = sK37) | ~ spl39_150), inference(avatar_component_clause, [], [f2368])).
fof(f2368, plain, (spl39_150 <=> (n2 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_150])])).
fof(f1069, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, sK36)) | (spl39_10 | ~ spl39_15)), inference(forward_demodulation, [], [f545, f569])).
fof(f545, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | spl39_10), inference(avatar_component_clause, [], [f543])).
fof(f543, plain, (spl39_10 <=> (s_worst7_init = a_select3(simplex7_init, sK37, sK36))), introduced(avatar_definition, [new_symbols(naming, [spl39_10])])).
fof(f556329, plain, (spl39_10 | ~ spl39_15 | ~ spl39_141 | ~ spl39_150), inference(avatar_contradiction_clause, [], [f556328])).
fof(f556328, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_141 | ~ spl39_150)), inference(subsumption_resolution, [], [f556326, f539733])).
fof(f539733, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n1)) | ~ spl39_15), inference(forward_demodulation, [], [f539732, f469])).
fof(f539732, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | ~ spl39_15), inference(subsumption_resolution, [], [f539731, f685])).
fof(f685, plain, leq(n1, n2), inference(resolution, [], [f249, f408])).
fof(f408, plain, gt(n2, n1), inference(cnf_transformation, [], [f74])).
fof(f74, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', gt_2_1)).
fof(f539731, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | ~ spl39_15), inference(forward_demodulation, [], [f539675, f469])).
fof(f539675, plain, (~ leq(plus(n0, n1), n2) | (s_sworst7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | ~ spl39_15), inference(resolution, [], [f2314, f671])).
fof(f671, plain, ! [X0] : leq(X0, plus(X0, n1)), inference(resolution, [], [f249, f431])).
fof(f431, plain, ! [X0] : gt(plus(X0, n1), X0), inference(definition_unfolding, [], [f253, f324])).
fof(f253, plain, ! [X0] : gt(succ(X0), X0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : gt(succ(X0), X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', gt_succ)).
fof(f556326, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_141 | ~ spl39_150)), inference(backward_demodulation, [], [f556312, f1986])).
fof(f1986, plain, ((n1 = sK36) | ~ spl39_141), inference(avatar_component_clause, [], [f1984])).
fof(f1984, plain, (spl39_141 <=> (n1 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_141])])).
fof(f552535, plain, ~ spl39_69, inference(avatar_contradiction_clause, [], [f552534])).
fof(f552534, plain, ($false | ~ spl39_69), inference(subsumption_resolution, [], [f542702, f402])).
fof(f402, plain, gt(n1, n0), inference(cnf_transformation, [], [f68])).
fof(f68, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', gt_1_0)).
fof(f542702, plain, (~ gt(n1, n0) | ~ spl39_69), inference(backward_demodulation, [], [f20846, f1202])).
fof(f1202, plain, ((n0 = n3) | ~ spl39_69), inference(avatar_component_clause, [], [f1200])).
fof(f1200, plain, (spl39_69 <=> (n0 = n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_69])])).
fof(f20846, plain, ~ gt(n1, n3), inference(superposition, [], [f13819, f700])).
fof(f700, plain, (n3 = plus(n1, n2)), inference(superposition, [], [f436, f661])).
fof(f661, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f660, f656])).
fof(f656, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f470, f469])).
fof(f470, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f426, f324, f324])).
fof(f426, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f92])).
fof(f92, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', successor_2)).
fof(f660, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f659, f469])).
fof(f659, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f471, f436])).
fof(f471, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f427, f324, f324, f324])).
fof(f427, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f93])).
fof(f93, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', successor_3)).
fof(f436, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f325, f324])).
fof(f325, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', succ_plus_1_l)).
fof(f13819, plain, ! [X5] : ~ gt(X5, plus(X5, n2)), inference(forward_demodulation, [], [f13800, f8644])).
fof(f8644, plain, ! [X2] : (plus(X2, n2) = plus(n3, minus(X2, n1))), inference(forward_demodulation, [], [f8632, f437])).
fof(f8632, plain, ! [X2] : (plus(plus(X2, n1), n1) = plus(n3, minus(X2, n1))), inference(superposition, [], [f1300, f1280])).
fof(f1300, plain, ! [X0] : (plus(n3, X0) = plus(plus(X0, n2), n1)), inference(backward_demodulation, [], [f489, f1281])).
fof(f1281, plain, ! [X1] : (plus(X1, n2) = plus(n1, plus(X1, n1))), inference(superposition, [], [f437, f436])).
fof(f489, plain, ! [X0] : (plus(n3, X0) = plus(plus(n1, plus(X0, n1)), n1)), inference(forward_demodulation, [], [f440, f436])).
fof(f440, plain, ! [X0] : (plus(n3, X0) = plus(plus(plus(X0, n1), n1), n1)), inference(definition_unfolding, [], [f329, f324, f324, f324])).
fof(f329, plain, ! [X0] : (succ(succ(succ(X0))) = plus(n3, X0)), inference(cnf_transformation, [], [f34])).
fof(f34, plain, ! [X0] : (succ(succ(succ(X0))) = plus(n3, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', succ_plus_3_l)).
fof(f13800, plain, ! [X5] : ~ gt(X5, plus(n3, minus(X5, n1))), inference(resolution, [], [f5692, f429])).
fof(f5692, plain, ! [X1] : ~ leq(plus(n3, X1), X1), inference(forward_demodulation, [], [f5681, f1334])).
fof(f1334, plain, ! [X9] : (plus(plus(X9, n1), n2) = plus(n3, X9)), inference(forward_demodulation, [], [f1333, f1329])).
fof(f1329, plain, ! [X1] : (plus(n1, plus(n2, X1)) = plus(n3, X1)), inference(backward_demodulation, [], [f1327, f1328])).
fof(f1328, plain, ! [X2] : (plus(n2, plus(X2, n1)) = plus(n3, X2)), inference(forward_demodulation, [], [f1308, f1300])).
fof(f1308, plain, ! [X2] : (plus(n2, plus(X2, n1)) = plus(plus(X2, n2), n1)), inference(superposition, [], [f438, f437])).
fof(f438, plain, ! [X0] : (plus(n2, X0) = plus(plus(X0, n1), n1)), inference(definition_unfolding, [], [f327, f324, f324])).
fof(f327, plain, ! [X0] : (succ(succ(X0)) = plus(n2, X0)), inference(cnf_transformation, [], [f32])).
fof(f32, plain, ! [X0] : (succ(succ(X0)) = plus(n2, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', succ_plus_2_l)).
fof(f1327, plain, ! [X1] : (plus(n2, plus(X1, n1)) = plus(n1, plus(n2, X1))), inference(forward_demodulation, [], [f1307, f436])).
fof(f1307, plain, ! [X1] : (plus(n2, plus(X1, n1)) = plus(plus(n2, X1), n1)), inference(superposition, [], [f438, f438])).
fof(f1333, plain, ! [X9] : (plus(plus(X9, n1), n2) = plus(n1, plus(n2, X9))), inference(forward_demodulation, [], [f1321, f436])).
fof(f1321, plain, ! [X9] : (plus(plus(X9, n1), n2) = plus(plus(n2, X9), n1)), inference(superposition, [], [f437, f438])).
fof(f5681, plain, ! [X1] : ~ leq(plus(plus(X1, n1), n2), X1), inference(resolution, [], [f3666, f432])).
fof(f541347, plain, (spl39_69 | spl39_10 | ~ spl39_15 | ~ spl39_62 | ~ spl39_150), inference(avatar_split_clause, [], [f541346, f2368, f1134, f568, f543, f1200])).
fof(f1134, plain, (spl39_62 <=> (n2 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_62])])).
fof(f541346, plain, ((n0 = n3) | (spl39_10 | ~ spl39_15 | ~ spl39_62 | ~ spl39_150)), inference(subsumption_resolution, [], [f540136, f540841])).
fof(f540841, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_62 | ~ spl39_150)), inference(backward_demodulation, [], [f540060, f2370])).
fof(f540060, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_62)), inference(forward_demodulation, [], [f1069, f1136])).
fof(f1136, plain, ((n2 = sK36) | ~ spl39_62), inference(avatar_component_clause, [], [f1134])).
fof(f540136, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n2)) | (n0 = n3) | ~ spl39_15), inference(subsumption_resolution, [], [f540135, f27752])).
fof(f27752, plain, ~ gt(n0, n3), inference(superposition, [], [f20825, f10597])).
fof(f10597, plain, (n3 = plus(n3, n0)), inference(forward_demodulation, [], [f10588, f717])).
fof(f717, plain, (n3 = minus(n4, n1)), inference(superposition, [], [f445, f704])).
fof(f704, plain, (n4 = plus(n3, n1)), inference(backward_demodulation, [], [f657, f700])).
fof(f657, plain, (n4 = plus(plus(n1, n2), n1)), inference(backward_demodulation, [], [f655, f656])).
fof(f655, plain, (n4 = plus(plus(n1, plus(n1, n1)), n1)), inference(backward_demodulation, [], [f650, f469])).
fof(f650, plain, (n4 = plus(plus(n1, plus(n1, plus(n0, n1))), n1)), inference(forward_demodulation, [], [f649, f436])).
fof(f649, plain, (n4 = plus(plus(n1, plus(plus(n0, n1), n1)), n1)), inference(forward_demodulation, [], [f467, f436])).
fof(f467, plain, (n4 = plus(plus(plus(plus(n0, n1), n1), n1), n1)), inference(definition_unfolding, [], [f423, f324, f324, f324, f324])).
fof(f423, plain, (n4 = succ(succ(succ(succ(n0))))), inference(cnf_transformation, [], [f89])).
fof(f89, plain, (n4 = succ(succ(succ(succ(n0))))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', successor_4)).
fof(f445, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f335, f334, f324])).
fof(f335, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', pred_succ)).
fof(f10588, plain, (plus(n3, n0) = minus(n4, n1)), inference(superposition, [], [f1393, f9193])).
fof(f9193, plain, (n4 = plus(n0, n4)), inference(forward_demodulation, [], [f9170, f1293])).
fof(f1293, plain, (n4 = plus(n2, n2)), inference(forward_demodulation, [], [f1277, f704])).
fof(f1277, plain, (plus(n3, n1) = plus(n2, n2)), inference(superposition, [], [f437, f661])).
fof(f9170, plain, (plus(n0, n4) = plus(n2, n2)), inference(superposition, [], [f1353, f1290])).
fof(f1290, plain, (n2 = plus(n0, n2)), inference(forward_demodulation, [], [f1273, f656])).
fof(f1273, plain, (plus(n1, n1) = plus(n0, n2)), inference(superposition, [], [f437, f469])).
fof(f1353, plain, ! [X8] : (plus(plus(X8, n2), n2) = plus(X8, n4)), inference(forward_demodulation, [], [f1348, f1336])).
fof(f1336, plain, ! [X0] : (plus(X0, n4) = plus(plus(n3, X0), n1)), inference(backward_demodulation, [], [f1299, f1335])).
fof(f1335, plain, ! [X1] : (plus(n1, plus(X1, n2)) = plus(n3, X1)), inference(backward_demodulation, [], [f1292, f1334])).
fof(f1292, plain, ! [X1] : (plus(plus(X1, n1), n2) = plus(n1, plus(X1, n2))), inference(forward_demodulation, [], [f1276, f436])).
fof(f1276, plain, ! [X1] : (plus(plus(X1, n1), n2) = plus(plus(X1, n2), n1)), inference(superposition, [], [f437, f437])).
fof(f1299, plain, ! [X0] : (plus(X0, n4) = plus(plus(n1, plus(X0, n2)), n1)), inference(backward_demodulation, [], [f491, f1281])).
fof(f491, plain, ! [X0] : (plus(X0, n4) = plus(plus(n1, plus(n1, plus(X0, n1))), n1)), inference(forward_demodulation, [], [f490, f436])).
fof(f490, plain, ! [X0] : (plus(X0, n4) = plus(plus(n1, plus(plus(X0, n1), n1)), n1)), inference(forward_demodulation, [], [f441, f436])).
fof(f441, plain, ! [X0] : (plus(X0, n4) = plus(plus(plus(plus(X0, n1), n1), n1), n1)), inference(definition_unfolding, [], [f330, f324, f324, f324, f324])).
fof(f330, plain, ! [X0] : (plus(X0, n4) = succ(succ(succ(succ(X0))))), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : (plus(X0, n4) = succ(succ(succ(succ(X0))))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', succ_plus_4_r)).
fof(f1348, plain, ! [X8] : (plus(plus(X8, n2), n2) = plus(plus(n3, X8), n1)), inference(superposition, [], [f437, f1300])).
fof(f1393, plain, ! [X10] : (plus(n3, X10) = minus(plus(X10, n4), n1)), inference(superposition, [], [f445, f1336])).
fof(f20825, plain, ! [X6] : ~ gt(X6, plus(n3, X6)), inference(forward_demodulation, [], [f20804, f9234])).
fof(f9234, plain, ! [X3] : (plus(n3, X3) = plus(minus(X3, n1), n4)), inference(forward_demodulation, [], [f9221, f1328])).
fof(f9221, plain, ! [X3] : (plus(minus(X3, n1), n4) = plus(n2, plus(X3, n1))), inference(superposition, [], [f1354, f1280])).
fof(f1354, plain, ! [X9] : (plus(n2, plus(X9, n2)) = plus(X9, n4)), inference(forward_demodulation, [], [f1349, f1336])).
fof(f1349, plain, ! [X9] : (plus(n2, plus(X9, n2)) = plus(plus(n3, X9), n1)), inference(superposition, [], [f438, f1300])).
fof(f20804, plain, ! [X6] : ~ gt(X6, plus(minus(X6, n1), n4)), inference(resolution, [], [f13817, f429])).
fof(f13817, plain, ! [X3] : ~ leq(plus(X3, n4), X3), inference(forward_demodulation, [], [f13798, f8968])).
fof(f8968, plain, ! [X9] : (plus(X9, n4) = plus(n3, plus(n1, X9))), inference(forward_demodulation, [], [f8945, f8963])).
fof(f8963, plain, ! [X1] : (plus(X1, n4) = plus(n2, plus(n2, X1))), inference(backward_demodulation, [], [f8937, f8962])).
fof(f8962, plain, ! [X2] : (plus(X2, n4) = plus(n3, plus(X2, n1))), inference(forward_demodulation, [], [f8938, f1354])).
fof(f8938, plain, ! [X2] : (plus(n3, plus(X2, n1)) = plus(n2, plus(X2, n2))), inference(superposition, [], [f1328, f437])).
fof(f8937, plain, ! [X1] : (plus(n3, plus(X1, n1)) = plus(n2, plus(n2, X1))), inference(superposition, [], [f1328, f438])).
fof(f8945, plain, ! [X9] : (plus(n3, plus(n1, X9)) = plus(n2, plus(n2, X9))), inference(superposition, [], [f1328, f1304])).
fof(f1304, plain, ! [X0] : (plus(n2, X0) = plus(plus(n1, X0), n1)), inference(superposition, [], [f438, f436])).
fof(f13798, plain, ! [X3] : ~ leq(plus(n3, plus(n1, X3)), X3), inference(resolution, [], [f5692, f741])).
fof(f741, plain, ! [X0, X1] : (leq(X1, plus(n1, X0)) | ~ leq(X1, X0)), inference(superposition, [], [f432, f436])).
fof(f540135, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n2)) | gt(n0, n3) | (n0 = n3) | ~ spl39_15), inference(subsumption_resolution, [], [f539706, f247])).
fof(f247, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', reflexivity_leq)).
fof(f539706, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, n2)) | gt(n0, n3) | (n0 = n3) | ~ spl39_15), inference(resolution, [], [f2314, f4003])).
fof(f4003, plain, ! [X0] : (leq(X0, n2) | gt(X0, n3) | (n3 = X0)), inference(resolution, [], [f754, f244])).
fof(f754, plain, ! [X5] : (~ gt(n3, X5) | leq(X5, n2)), inference(superposition, [], [f433, f661])).
fof(f540097, plain, (spl39_10 | ~ spl39_15 | ~ spl39_62 | ~ spl39_143), inference(avatar_contradiction_clause, [], [f540096])).
fof(f540096, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_62 | ~ spl39_143)), inference(subsumption_resolution, [], [f540094, f539509])).
fof(f539509, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n2)) | ~ spl39_15), inference(subsumption_resolution, [], [f539407, f247])).
fof(f539407, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, n2)) | ~ spl39_15), inference(resolution, [], [f2313, f673])).
fof(f2313, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, X3))) | ~ spl39_15), inference(subsumption_resolution, [], [f2290, f686])).
fof(f686, plain, leq(n1, n3), inference(resolution, [], [f249, f409])).
fof(f409, plain, gt(n3, n1), inference(cnf_transformation, [], [f75])).
fof(f75, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', gt_3_1)).
fof(f2290, plain, (! [X3] : (~ leq(n1, n3) | (s_sworst7_init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | ~ spl39_15), inference(resolution, [], [f662, f672])).
fof(f672, plain, leq(n0, n1), inference(resolution, [], [f249, f402])).
fof(f540094, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_62 | ~ spl39_143)), inference(backward_demodulation, [], [f540060, f1995])).
fof(f1995, plain, ((n1 = sK37) | ~ spl39_143), inference(avatar_component_clause, [], [f1993])).
fof(f1993, plain, (spl39_143 <=> (n1 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_143])])).
fof(f540078, plain, (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_62), inference(avatar_contradiction_clause, [], [f540077])).
fof(f540077, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_62)), inference(subsumption_resolution, [], [f540075, f539852])).
fof(f539852, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n2)) | ~ spl39_15), inference(subsumption_resolution, [], [f539750, f247])).
fof(f539750, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, n2)) | ~ spl39_15), inference(resolution, [], [f2315, f673])).
fof(f2315, plain, (! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, X5))) | ~ spl39_15), inference(subsumption_resolution, [], [f2292, f247])).
fof(f2292, plain, (! [X5] : (~ leq(n3, n3) | (s_sworst7_init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)) | ~ spl39_15), inference(resolution, [], [f662, f674])).
fof(f674, plain, leq(n0, n3), inference(resolution, [], [f249, f404])).
fof(f404, plain, gt(n3, n0), inference(cnf_transformation, [], [f70])).
fof(f70, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', gt_3_0)).
fof(f540075, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_62)), inference(backward_demodulation, [], [f540060, f1079])).
fof(f1079, plain, ((n3 = sK37) | ~ spl39_54), inference(avatar_component_clause, [], [f1077])).
fof(f1077, plain, (spl39_54 <=> (n3 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_54])])).
fof(f540020, plain, (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_64), inference(avatar_contradiction_clause, [], [f540019])).
fof(f540019, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_64)), inference(subsumption_resolution, [], [f540017, f539853])).
fof(f539853, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n0)) | ~ spl39_15), inference(subsumption_resolution, [], [f539771, f673])).
fof(f539771, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, n0)) | ~ spl39_15), inference(resolution, [], [f2315, f247])).
fof(f540017, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_64)), inference(backward_demodulation, [], [f539981, f1079])).
fof(f539981, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_64)), inference(forward_demodulation, [], [f1069, f1147])).
fof(f540002, plain, (spl39_10 | ~ spl39_15 | ~ spl39_64 | ~ spl39_143), inference(avatar_contradiction_clause, [], [f540001])).
fof(f540001, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_64 | ~ spl39_143)), inference(subsumption_resolution, [], [f539999, f539510])).
fof(f539510, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n0)) | ~ spl39_15), inference(subsumption_resolution, [], [f539428, f673])).
fof(f539428, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, n0)) | ~ spl39_15), inference(resolution, [], [f2313, f247])).
fof(f539999, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_64 | ~ spl39_143)), inference(backward_demodulation, [], [f539981, f1995])).
fof(f539937, plain, (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_62), inference(avatar_contradiction_clause, [], [f539936])).
fof(f539936, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_62)), inference(subsumption_resolution, [], [f539934, f14184])).
fof(f14184, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n2)) | ~ spl39_15), inference(subsumption_resolution, [], [f14149, f247])).
fof(f14149, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, n2)) | ~ spl39_15), inference(resolution, [], [f2320, f673])).
fof(f2320, plain, (! [X38] : (~ leq(n0, X38) | ~ leq(X38, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, X38))) | ~ spl39_15), inference(subsumption_resolution, [], [f2309, f674])).
fof(f2309, plain, (! [X38] : (~ leq(n0, n3) | (s_sworst7_init = a_select3(simplex7_init, n0, X38)) | ~ leq(X38, n2) | ~ leq(n0, X38)) | ~ spl39_15), inference(resolution, [], [f662, f247])).
fof(f539934, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_62)), inference(backward_demodulation, [], [f539917, f1136])).
fof(f539917, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_60)), inference(forward_demodulation, [], [f1069, f1125])).
fof(f1125, plain, ((n0 = sK37) | ~ spl39_60), inference(avatar_component_clause, [], [f1123])).
fof(f1123, plain, (spl39_60 <=> (n0 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_60])])).
fof(f539913, plain, (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_64), inference(avatar_contradiction_clause, [], [f539912])).
fof(f539912, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_64)), inference(subsumption_resolution, [], [f539906, f14185])).
fof(f14185, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n0)) | ~ spl39_15), inference(subsumption_resolution, [], [f14164, f673])).
fof(f14164, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, n0)) | ~ spl39_15), inference(resolution, [], [f2320, f247])).
fof(f539906, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_64)), inference(backward_demodulation, [], [f539876, f1125])).
fof(f539876, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_64)), inference(backward_demodulation, [], [f1069, f1147])).
fof(f539872, plain, (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_141), inference(avatar_contradiction_clause, [], [f539871])).
fof(f539871, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_141)), inference(subsumption_resolution, [], [f539870, f539600])).
fof(f539600, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_141)), inference(backward_demodulation, [], [f2268, f1079])).
fof(f2268, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_141)), inference(backward_demodulation, [], [f1069, f1986])).
fof(f539870, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n1)) | ~ spl39_15), inference(forward_demodulation, [], [f539869, f469])).
fof(f539869, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl39_15), inference(subsumption_resolution, [], [f539868, f685])).
fof(f539868, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl39_15), inference(forward_demodulation, [], [f539800, f469])).
fof(f539800, plain, (~ leq(plus(n0, n1), n2) | (s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl39_15), inference(resolution, [], [f2315, f671])).
fof(f539557, plain, (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_141), inference(avatar_contradiction_clause, [], [f539556])).
fof(f539556, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_141)), inference(subsumption_resolution, [], [f539548, f14183])).
fof(f14183, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n1)) | ~ spl39_15), inference(subsumption_resolution, [], [f14147, f685])).
fof(f14147, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, n1)) | ~ spl39_15), inference(resolution, [], [f2320, f672])).
fof(f539548, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_60 | ~ spl39_141)), inference(backward_demodulation, [], [f2268, f1125])).
fof(f539529, plain, (spl39_10 | ~ spl39_15 | ~ spl39_141 | ~ spl39_143), inference(avatar_contradiction_clause, [], [f539528])).
fof(f539528, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_141 | ~ spl39_143)), inference(subsumption_resolution, [], [f539527, f2385])).
fof(f2385, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_141 | ~ spl39_143)), inference(backward_demodulation, [], [f2268, f1995])).
fof(f539527, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n1)) | ~ spl39_15), inference(forward_demodulation, [], [f539526, f469])).
fof(f539526, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | ~ spl39_15), inference(subsumption_resolution, [], [f539525, f685])).
fof(f539525, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | ~ spl39_15), inference(forward_demodulation, [], [f539457, f469])).
fof(f539457, plain, (~ leq(plus(n0, n1), n2) | (s_sworst7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | ~ spl39_15), inference(resolution, [], [f2313, f671])).
fof(f2371, plain, (spl39_54 | spl39_60 | spl39_143 | spl39_150 | ~ spl39_11 | ~ spl39_12), inference(avatar_split_clause, [], [f2366, f553, f548, f2368, f1993, f1123, f1077])).
fof(f548, plain, (spl39_11 <=> leq(sK37, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_11])])).
fof(f553, plain, (spl39_12 <=> leq(n0, sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_12])])).
fof(f2366, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | (n3 = sK37) | (~ spl39_11 | ~ spl39_12)), inference(subsumption_resolution, [], [f2343, f550])).
fof(f550, plain, (leq(sK37, n3) | ~ spl39_11), inference(avatar_component_clause, [], [f548])).
fof(f2343, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | ~ leq(sK37, n3) | (n3 = sK37) | ~ spl39_12), inference(resolution, [], [f422, f555])).
fof(f555, plain, (leq(n0, sK37) | ~ spl39_12), inference(avatar_component_clause, [], [f553])).
fof(f422, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f169])).
fof(f169, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f168])).
fof(f168, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f88])).
fof(f88, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', finite_domain_3)).
fof(f2223, plain, (~ spl39_65 | ~ spl39_64 | spl39_141), inference(avatar_split_clause, [], [f2213, f1984, f1145, f1182])).
fof(f2213, plain, (~ (n0 = n1) | (~ spl39_64 | spl39_141)), inference(backward_demodulation, [], [f1985, f1147])).
fof(f1985, plain, (~ (n1 = sK36) | spl39_141), inference(avatar_component_clause, [], [f1984])).
fof(f2108, plain, (spl39_62 | spl39_64 | spl39_141 | ~ spl39_13 | ~ spl39_14), inference(avatar_split_clause, [], [f2107, f563, f558, f1984, f1145, f1134])).
fof(f558, plain, (spl39_13 <=> leq(sK36, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_13])])).
fof(f563, plain, (spl39_14 <=> leq(n0, sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_14])])).
fof(f2107, plain, ((n1 = sK36) | (n0 = sK36) | (n2 = sK36) | (~ spl39_13 | ~ spl39_14)), inference(subsumption_resolution, [], [f2100, f560])).
fof(f560, plain, (leq(sK36, n2) | ~ spl39_13), inference(avatar_component_clause, [], [f558])).
fof(f2100, plain, ((n1 = sK36) | (n0 = sK36) | ~ leq(sK36, n2) | (n2 = sK36) | ~ spl39_14), inference(resolution, [], [f421, f565])).
fof(f565, plain, (leq(n0, sK36) | ~ spl39_14), inference(avatar_component_clause, [], [f563])).
fof(f421, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f166])).
fof(f166, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f87])).
fof(f87, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV034+1.p', finite_domain_2)).
fof(f1845, plain, (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15), inference(avatar_contradiction_clause, [], [f1844])).
fof(f1844, plain, ($false | (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15)), inference(subsumption_resolution, [], [f1843, f1824])).
fof(f1824, plain, (~ (s_sworst7_init = a_select2(s_center7_init, sK34)) | (spl39_2 | ~ spl39_15)), inference(forward_demodulation, [], [f507, f569])).
fof(f507, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | spl39_2), inference(avatar_component_clause, [], [f505])).
fof(f505, plain, (spl39_2 <=> (s_worst7_init = a_select2(s_center7_init, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl39_2])])).
fof(f1843, plain, ((s_sworst7_init = a_select2(s_center7_init, sK34)) | (~ spl39_3 | ~ spl39_4 | ~ spl39_15)), inference(subsumption_resolution, [], [f1837, f512])).
fof(f512, plain, (leq(sK34, n2) | ~ spl39_3), inference(avatar_component_clause, [], [f510])).
fof(f510, plain, (spl39_3 <=> leq(sK34, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_3])])).
fof(f1837, plain, (~ leq(sK34, n2) | (s_sworst7_init = a_select2(s_center7_init, sK34)) | (~ spl39_4 | ~ spl39_15)), inference(resolution, [], [f517, f664])).
fof(f664, plain, (! [X2] : (~ leq(n0, X2) | ~ leq(X2, n2) | (s_sworst7_init = a_select2(s_center7_init, X2))) | ~ spl39_15), inference(backward_demodulation, [], [f463, f569])).
fof(f463, plain, ! [X2] : ((s_worst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(definition_unfolding, [], [f378, f367])).
fof(f378, plain, ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f243])).
fof(f517, plain, (leq(n0, sK34) | ~ spl39_4), inference(avatar_component_clause, [], [f515])).
fof(f515, plain, (spl39_4 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl39_4])])).
fof(f1819, plain, (~ spl39_15 | spl39_24 | ~ spl39_28 | ~ spl39_29), inference(avatar_contradiction_clause, [], [f1818])).
fof(f1818, plain, ($false | (~ spl39_15 | spl39_24 | ~ spl39_28 | ~ spl39_29)), inference(subsumption_resolution, [], [f1817, f1648])).
fof(f1648, plain, (leq(sK38, n2) | ~ spl39_28), inference(forward_demodulation, [], [f623, f716])).
fof(f716, plain, (n2 = minus(n3, n1)), inference(superposition, [], [f445, f661])).
fof(f623, plain, (leq(sK38, minus(n3, n1)) | ~ spl39_28), inference(avatar_component_clause, [], [f621])).
fof(f621, plain, (spl39_28 <=> leq(sK38, minus(n3, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_28])])).
fof(f1817, plain, (~ leq(sK38, n2) | (~ spl39_15 | spl39_24 | ~ spl39_29)), inference(subsumption_resolution, [], [f1809, f1082])).
fof(f1082, plain, (~ (s_sworst7_init = a_select2(s_try7_init, sK38)) | (~ spl39_15 | spl39_24)), inference(forward_demodulation, [], [f606, f569])).
fof(f606, plain, (~ (s_worst7_init = a_select2(s_try7_init, sK38)) | spl39_24), inference(avatar_component_clause, [], [f604])).
fof(f604, plain, (spl39_24 <=> (s_worst7_init = a_select2(s_try7_init, sK38))), introduced(avatar_definition, [new_symbols(naming, [spl39_24])])).
fof(f1809, plain, ((s_sworst7_init = a_select2(s_try7_init, sK38)) | ~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_29)), inference(resolution, [], [f719, f628])).
fof(f628, plain, (leq(n0, sK38) | ~ spl39_29), inference(avatar_component_clause, [], [f626])).
fof(f626, plain, (spl39_29 <=> leq(n0, sK38)), introduced(avatar_definition, [new_symbols(naming, [spl39_29])])).
fof(f719, plain, (! [X1] : (~ leq(n0, X1) | (s_sworst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, n2)) | ~ spl39_15), inference(backward_demodulation, [], [f665, f716])).
fof(f665, plain, (! [X1] : ((s_sworst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | ~ spl39_15), inference(backward_demodulation, [], [f462, f569])).
fof(f462, plain, ! [X1] : ((s_worst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(definition_unfolding, [], [f379, f367])).
fof(f379, plain, ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f243])).
fof(f1633, plain, (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15), inference(avatar_contradiction_clause, [], [f1632])).
fof(f1632, plain, ($false | (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15)), inference(subsumption_resolution, [], [f1631, f666])).
fof(f666, plain, (~ (s_sworst7_init = a_select2(s_values7_init, sK35)) | (spl39_6 | ~ spl39_15)), inference(forward_demodulation, [], [f526, f569])).
fof(f526, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | spl39_6), inference(avatar_component_clause, [], [f524])).
fof(f524, plain, (spl39_6 <=> (s_worst7_init = a_select2(s_values7_init, sK35))), introduced(avatar_definition, [new_symbols(naming, [spl39_6])])).
fof(f1631, plain, ((s_sworst7_init = a_select2(s_values7_init, sK35)) | (~ spl39_7 | ~ spl39_8 | ~ spl39_15)), inference(subsumption_resolution, [], [f1582, f531])).
fof(f531, plain, (leq(sK35, n3) | ~ spl39_7), inference(avatar_component_clause, [], [f529])).
fof(f529, plain, (spl39_7 <=> leq(sK35, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_7])])).
fof(f1582, plain, (~ leq(sK35, n3) | (s_sworst7_init = a_select2(s_values7_init, sK35)) | (~ spl39_8 | ~ spl39_15)), inference(resolution, [], [f663, f536])).
fof(f536, plain, (leq(n0, sK35) | ~ spl39_8), inference(avatar_component_clause, [], [f534])).
fof(f534, plain, (spl39_8 <=> leq(n0, sK35)), introduced(avatar_definition, [new_symbols(naming, [spl39_8])])).
fof(f663, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n3) | (s_sworst7_init = a_select2(s_values7_init, X3))) | ~ spl39_15), inference(backward_demodulation, [], [f464, f569])).
fof(f464, plain, ! [X3] : ((s_worst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(definition_unfolding, [], [f377, f367])).
fof(f377, plain, ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(cnf_transformation, [], [f243])).
fof(f648, plain, spl39_15, inference(avatar_split_clause, [], [f466, f568])).
fof(f466, plain, (s_sworst7_init = s_worst7_init), inference(definition_unfolding, [], [f366, f367])).
fof(f366, plain, (init = s_sworst7_init), inference(cnf_transformation, [], [f243])).
fof(f647, plain, spl39_16, inference(avatar_split_clause, [], [f368, f572])).
fof(f572, plain, (spl39_16 <=> leq(n0, s_best7)), introduced(avatar_definition, [new_symbols(naming, [spl39_16])])).
fof(f368, plain, leq(n0, s_best7), inference(cnf_transformation, [], [f243])).
fof(f646, plain, spl39_17, inference(avatar_split_clause, [], [f369, f576])).
fof(f576, plain, (spl39_17 <=> leq(n0, s_sworst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_17])])).
fof(f369, plain, leq(n0, s_sworst7), inference(cnf_transformation, [], [f243])).
fof(f645, plain, spl39_18, inference(avatar_split_clause, [], [f370, f580])).
fof(f580, plain, (spl39_18 <=> leq(n0, s_worst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_18])])).
fof(f370, plain, leq(n0, s_worst7), inference(cnf_transformation, [], [f243])).
fof(f644, plain, spl39_19, inference(avatar_split_clause, [], [f371, f584])).
fof(f584, plain, (spl39_19 <=> leq(n0, pv19)), introduced(avatar_definition, [new_symbols(naming, [spl39_19])])).
fof(f371, plain, leq(n0, pv19), inference(cnf_transformation, [], [f243])).
fof(f643, plain, spl39_20, inference(avatar_split_clause, [], [f372, f588])).
fof(f588, plain, (spl39_20 <=> leq(s_best7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_20])])).
fof(f372, plain, leq(s_best7, n3), inference(cnf_transformation, [], [f243])).
fof(f642, plain, spl39_21, inference(avatar_split_clause, [], [f373, f592])).
fof(f592, plain, (spl39_21 <=> leq(s_sworst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_21])])).
fof(f373, plain, leq(s_sworst7, n3), inference(cnf_transformation, [], [f243])).
fof(f641, plain, spl39_22, inference(avatar_split_clause, [], [f374, f596])).
fof(f596, plain, (spl39_22 <=> leq(s_worst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_22])])).
fof(f374, plain, leq(s_worst7, n3), inference(cnf_transformation, [], [f243])).
fof(f640, plain, spl39_23, inference(avatar_split_clause, [], [f375, f600])).
fof(f600, plain, (spl39_23 <=> leq(pv19, minus(n410, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_23])])).
fof(f375, plain, leq(pv19, minus(n410, n1)), inference(cnf_transformation, [], [f243])).
fof(f639, plain, (~ spl39_30 | spl39_25), inference(avatar_split_clause, [], [f461, f608, f631])).
fof(f631, plain, (spl39_30 <=> gt(loopcounter, n1)), introduced(avatar_definition, [new_symbols(naming, [spl39_30])])).
fof(f608, plain, (spl39_25 <=> (s_worst7_init = pvar1400_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_25])])).
fof(f461, plain, ((s_worst7_init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f380, f367])).
fof(f380, plain, ((init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f243])).
fof(f638, plain, (~ spl39_30 | spl39_26), inference(avatar_split_clause, [], [f460, f612, f631])).
fof(f612, plain, (spl39_26 <=> (s_worst7_init = pvar1401_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_26])])).
fof(f460, plain, ((s_worst7_init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f381, f367])).
fof(f381, plain, ((init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f243])).
fof(f637, plain, (~ spl39_30 | spl39_27), inference(avatar_split_clause, [], [f459, f616, f631])).
fof(f616, plain, (spl39_27 <=> (s_worst7_init = pvar1402_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_27])])).
fof(f459, plain, ((s_worst7_init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f382, f367])).
fof(f382, plain, ((init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f243])).
fof(f636, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | spl39_9 | spl39_5 | spl39_1 | spl39_29 | spl39_30), inference(avatar_split_clause, [], [f476, f631, f626, f501, f520, f539, f600, f596, f592, f588, f584, f580, f576, f572, f568])).
fof(f539, plain, (spl39_9 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl39_9])])).
fof(f520, plain, (spl39_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl39_5])])).
fof(f501, plain, (spl39_1 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl39_1])])).
fof(f476, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f475])).
fof(f475, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f458])).
fof(f458, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f383, f367, f367, f428, f367, f367, f367])).
fof(f428, plain, (s_best7_init = s_worst7_init), inference(definition_unfolding, [], [f365, f367])).
fof(f365, plain, (s_best7_init = init), inference(cnf_transformation, [], [f243])).
fof(f383, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)), inference(cnf_transformation, [], [f243])).
fof(f635, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | spl39_9 | spl39_5 | spl39_1 | spl39_28 | spl39_30), inference(avatar_split_clause, [], [f478, f631, f621, f501, f520, f539, f600, f596, f592, f588, f584, f580, f576, f572, f568])).
fof(f478, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f477])).
fof(f477, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f457])).
fof(f457, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f384, f367, f367, f428, f367, f367, f367])).
fof(f384, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)), inference(cnf_transformation, [], [f243])).
fof(f634, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_24 | spl39_30), inference(avatar_split_clause, [], [f480, f631, f604, f501, f520, f539, f600, f596, f592, f588, f584, f580, f576, f572, f568])).
fof(f480, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f479])).
fof(f479, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f456])).
fof(f456, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f385, f367, f367, f367, f428, f367, f367, f367])).
fof(f385, plain, (gt(loopcounter, n1) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)), inference(cnf_transformation, [], [f243])).
fof(f629, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | spl39_9 | spl39_5 | spl39_1 | spl39_29 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27), inference(avatar_split_clause, [], [f482, f616, f612, f608, f626, f501, f520, f539, f600, f596, f592, f588, f584, f580, f576, f572, f568])).
fof(f482, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f481])).
fof(f481, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f455])).
fof(f455, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f386, f367, f367, f367, f367, f367, f428, f367, f367, f367])).
fof(f386, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)), inference(cnf_transformation, [], [f243])).
fof(f624, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | spl39_9 | spl39_5 | spl39_1 | spl39_28 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27), inference(avatar_split_clause, [], [f484, f616, f612, f608, f621, f501, f520, f539, f600, f596, f592, f588, f584, f580, f576, f572, f568])).
fof(f484, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f483])).
fof(f483, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f454])).
fof(f454, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f387, f367, f367, f367, f367, f367, f428, f367, f367, f367])).
fof(f387, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)), inference(cnf_transformation, [], [f243])).
fof(f619, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27), inference(avatar_split_clause, [], [f486, f616, f612, f608, f604, f501, f520, f539, f600, f596, f592, f588, f584, f580, f576, f572, f568])).
fof(f486, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f485])).
fof(f485, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f453])).
fof(f453, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f388, f367, f367, f367, f367, f367, f367, f428, f367, f367, f367])).
fof(f388, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)), inference(cnf_transformation, [], [f243])).
fof(f566, plain, (~ spl39_9 | spl39_14), inference(avatar_split_clause, [], [f360, f563, f539])).
fof(f360, plain, (leq(n0, sK36) | ~ sP4), inference(cnf_transformation, [], [f240])).
fof(f240, plain, (((~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37)) & leq(sK36, n2) & leq(n0, sK36)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK36, sK37])], [f237, f239, f238])).
fof(f238, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) & leq(sK36, n2) & leq(n0, sK36))), introduced(choice_axiom, [])).
fof(f239, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37))), introduced(choice_axiom, [])).
fof(f237, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f236])).
fof(f236, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(nnf_transformation, [], [f177])).
fof(f561, plain, (~ spl39_9 | spl39_13), inference(avatar_split_clause, [], [f361, f558, f539])).
fof(f361, plain, (leq(sK36, n2) | ~ sP4), inference(cnf_transformation, [], [f240])).
fof(f556, plain, (~ spl39_9 | spl39_12), inference(avatar_split_clause, [], [f362, f553, f539])).
fof(f362, plain, (leq(n0, sK37) | ~ sP4), inference(cnf_transformation, [], [f240])).
fof(f551, plain, (~ spl39_9 | spl39_11), inference(avatar_split_clause, [], [f363, f548, f539])).
fof(f363, plain, (leq(sK37, n3) | ~ sP4), inference(cnf_transformation, [], [f240])).
fof(f546, plain, (~ spl39_9 | ~ spl39_10), inference(avatar_split_clause, [], [f452, f543, f539])).
fof(f452, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(definition_unfolding, [], [f364, f367])).
fof(f364, plain, (~ (init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(cnf_transformation, [], [f240])).
fof(f537, plain, (~ spl39_5 | spl39_8), inference(avatar_split_clause, [], [f357, f534, f520])).
fof(f357, plain, (leq(n0, sK35) | ~ sP5), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ((~ (init = a_select2(s_values7_init, sK35)) & leq(sK35, n3) & leq(n0, sK35)) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK35])], [f233, f234])).
fof(f234, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) => (~ (init = a_select2(s_values7_init, sK35)) & leq(sK35, n3) & leq(n0, sK35))), introduced(choice_axiom, [])).
fof(f233, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) | ~ sP5), inference(rectify, [], [f232])).
fof(f232, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(nnf_transformation, [], [f178])).
fof(f532, plain, (~ spl39_5 | spl39_7), inference(avatar_split_clause, [], [f358, f529, f520])).
fof(f358, plain, (leq(sK35, n3) | ~ sP5), inference(cnf_transformation, [], [f235])).
fof(f527, plain, (~ spl39_5 | ~ spl39_6), inference(avatar_split_clause, [], [f451, f524, f520])).
fof(f451, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | ~ sP5), inference(definition_unfolding, [], [f359, f367])).
fof(f359, plain, (~ (init = a_select2(s_values7_init, sK35)) | ~ sP5), inference(cnf_transformation, [], [f235])).
fof(f518, plain, (~ spl39_1 | spl39_4), inference(avatar_split_clause, [], [f354, f515, f501])).
fof(f354, plain, (leq(n0, sK34) | ~ sP6), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ((~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34)) | ~ sP6), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34])], [f229, f230])).
fof(f230, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) => (~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f229, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) | ~ sP6), inference(rectify, [], [f228])).
fof(f228, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(nnf_transformation, [], [f179])).
fof(f513, plain, (~ spl39_1 | spl39_3), inference(avatar_split_clause, [], [f355, f510, f501])).
fof(f355, plain, (leq(sK34, n2) | ~ sP6), inference(cnf_transformation, [], [f231])).
fof(f508, plain, (~ spl39_1 | ~ spl39_2), inference(avatar_split_clause, [], [f450, f505, f501])).
fof(f450, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(definition_unfolding, [], [f356, f367])).
fof(f356, plain, (~ (init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(cnf_transformation, [], [f231])).