fof(f475987, plain, $false, inference(avatar_sat_refutation, [], [f553, f558, f563, f568, f569, f570, f580, f585, f590, f595, f600, f605, f610, f615, f620, f625, f630, f632, f634, f635, f636, f637, f638, f639, f640, f641, f642, f643, f1707, f1724, f1754, f24920, f35099, f35935, f472504, f472646, f472859, f473001, f473215, f473221, f473364, f475279, f475523, f475633, f475772, f475985])).
fof(f475985, plain, (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_62), inference(avatar_contradiction_clause, [], [f475984])).
fof(f475984, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_62)), inference(subsumption_resolution, [], [f475942, f475262])).
fof(f475262, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n2)) | ~ spl40_2), inference(subsumption_resolution, [], [f475172, f241])).
fof(f241, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', reflexivity_leq)).
fof(f475172, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, n2)) | ~ spl40_2), inference(resolution, [], [f1993, f672])).
fof(f672, plain, leq(n0, n2), inference(resolution, [], [f243, f395])).
fof(f395, plain, gt(n2, n0), inference(cnf_transformation, [], [f65])).
fof(f65, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', gt_2_0)).
fof(f243, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', leq_gt1)).
fof(f1993, plain, (! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, X5))) | ~ spl40_2), inference(subsumption_resolution, [], [f1972, f241])).
fof(f1972, plain, (! [X5] : (~ leq(n3, n3) | (s_sworst7_init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)) | ~ spl40_2), inference(resolution, [], [f659, f674])).
fof(f674, plain, leq(n0, n3), inference(resolution, [], [f243, f396])).
fof(f396, plain, gt(n3, n0), inference(cnf_transformation, [], [f66])).
fof(f66, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', gt_3_0)).
fof(f659, plain, (! [X4, X3] : (~ leq(n0, X4) | ~ leq(X4, n3) | (s_sworst7_init = a_select3(simplex7_init, X4, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | ~ spl40_2), inference(backward_demodulation, [], [f456, f499])).
fof(f499, plain, ((s_sworst7_init = s_worst7_init) | ~ spl40_2), inference(avatar_component_clause, [], [f498])).
fof(f498, plain, (spl40_2 <=> (s_sworst7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl40_2])])).
fof(f456, plain, ! [X4, X3] : ((s_worst7_init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4) | ~ leq(X3, n2) | ~ leq(n0, X3)), inference(definition_unfolding, [], [f376, f369])).
fof(f369, plain, (init = s_worst7_init), inference(cnf_transformation, [], [f173])).
fof(f173, plain, (((~ true & (n0 = pv1413)) | sP7 | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(definition_folding, [], [f149, e172, e171, e170, e169])).
fof(f169, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(usedef, [], [e169])).
fof(e169, plain, (sP4 <=> ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f170, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(usedef, [], [e170])).
fof(e170, plain, (sP5 <=> ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f171, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(usedef, [], [e171])).
fof(e171, plain, (sP6 <=> ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f172, plain, (((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & ~ (n0 = pv1413)) | ~ sP7), inference(usedef, [], [e172])).
fof(e172, plain, (sP7 <=> ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & ~ (n0 = pv1413))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f149, plain, (((~ true & (n0 = pv1413)) | ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & ~ (n0 = pv1413)) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(flattening, [], [f148])).
fof(f148, plain, (((~ true & (n0 = pv1413)) | ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & (leq(X5, minus(n3, n1)) & leq(n0, X5))) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & (leq(X6, n2) & leq(n0, X6))) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & (leq(X7, n3) & leq(n0, X7))) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & (leq(X9, n3) & leq(n0, X9))) & (leq(X8, n2) & leq(n0, X8))) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & ~ (n0 = pv1413)) | ~ (init = init)) & ((((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | (~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0))) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | (~ leq(X1, n2) | ~ leq(n0, X1))) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | (~ leq(X2, n3) | ~ leq(n0, X2))) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | (~ leq(X4, n3) | ~ leq(n0, X4))) | (~ leq(X3, n2) | ~ leq(n0, X3))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init))), inference(ennf_transformation, [], [f107])).
fof(f107, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X0] : ((leq(X0, minus(n3, n1)) & leq(n0, X0)) => (init = a_select2(s_try7_init, X0))) & ! [X1] : ((leq(X1, n2) & leq(n0, X1)) => (init = a_select2(s_center7_init, X1))) & ! [X2] : ((leq(X2, n3) & leq(n0, X2)) => (init = a_select2(s_values7_init, X2))) & ! [X3] : ((leq(X3, n2) & leq(n0, X3)) => ! [X4] : ((leq(X4, n3) & leq(n0, X4)) => (init = a_select3(simplex7_init, X4, X3)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => (((n0 = pv1413) => true) & (~ (n0 = pv1413) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X5] : ((leq(X5, minus(n3, n1)) & leq(n0, X5)) => (init = a_select2(s_try7_init, X5))) & ! [X6] : ((leq(X6, n2) & leq(n0, X6)) => (init = a_select2(s_center7_init, X6))) & ! [X7] : ((leq(X7, n3) & leq(n0, X7)) => (init = a_select2(s_values7_init, X7))) & ! [X8] : ((leq(X8, n2) & leq(n0, X8)) => ! [X9] : ((leq(X9, n3) & leq(n0, X9)) => (init = a_select3(simplex7_init, X9, X8)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init) & (init = init))) & (init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => (((n0 = pv1413) => true) & (~ (n0 = pv1413) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init) & (init = init))) & (init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => (((n0 = pv1413) => true) & (~ (n0 = pv1413) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init) & (init = init))) & (init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', gauss_init_0065)).
fof(f376, plain, ! [X4, X3] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4) | ~ leq(X3, n2) | ~ leq(n0, X3)), inference(cnf_transformation, [], [f173])).
fof(f475942, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n2)) | (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_62)), inference(backward_demodulation, [], [f475778, f1097])).
fof(f1097, plain, ((n3 = sK39) | ~ spl40_58), inference(avatar_component_clause, [], [f1095])).
fof(f1095, plain, (spl40_58 <=> (n3 = sK39)), introduced(avatar_definition, [new_symbols(naming, [spl40_58])])).
fof(f475778, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK39, n2)) | (~ spl40_2 | spl40_26 | ~ spl40_62)), inference(backward_demodulation, [], [f1076, f1124])).
fof(f1124, plain, ((n2 = sK38) | ~ spl40_62), inference(avatar_component_clause, [], [f1122])).
fof(f1122, plain, (spl40_62 <=> (n2 = sK38)), introduced(avatar_definition, [new_symbols(naming, [spl40_62])])).
fof(f1076, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK39, sK38)) | (~ spl40_2 | spl40_26)), inference(forward_demodulation, [], [f609, f499])).
fof(f609, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK39, sK38)) | spl40_26), inference(avatar_component_clause, [], [f607])).
fof(f607, plain, (spl40_26 <=> (s_worst7_init = a_select3(simplex7_init, sK39, sK38))), introduced(avatar_definition, [new_symbols(naming, [spl40_26])])).
fof(f475772, plain, (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_64), inference(avatar_contradiction_clause, [], [f475771])).
fof(f475771, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_64)), inference(subsumption_resolution, [], [f475698, f35079])).
fof(f35079, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n0)) | ~ spl40_2), inference(subsumption_resolution, [], [f35053, f672])).
fof(f35053, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, n0)) | ~ spl40_2), inference(resolution, [], [f1997, f241])).
fof(f1997, plain, (! [X36] : (~ leq(n0, X36) | ~ leq(X36, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, X36))) | ~ spl40_2), inference(subsumption_resolution, [], [f1987, f674])).
fof(f1987, plain, (! [X36] : (~ leq(n0, n3) | (s_sworst7_init = a_select3(simplex7_init, n0, X36)) | ~ leq(X36, n2) | ~ leq(n0, X36)) | ~ spl40_2), inference(resolution, [], [f659, f241])).
fof(f475698, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n0)) | (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_64)), inference(backward_demodulation, [], [f475527, f1113])).
fof(f1113, plain, ((n0 = sK39) | ~ spl40_60), inference(avatar_component_clause, [], [f1111])).
fof(f1111, plain, (spl40_60 <=> (n0 = sK39)), introduced(avatar_definition, [new_symbols(naming, [spl40_60])])).
fof(f475527, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK39, n0)) | (~ spl40_2 | spl40_26 | ~ spl40_64)), inference(forward_demodulation, [], [f1076, f1135])).
fof(f1135, plain, ((n0 = sK38) | ~ spl40_64), inference(avatar_component_clause, [], [f1133])).
fof(f1133, plain, (spl40_64 <=> (n0 = sK38)), introduced(avatar_definition, [new_symbols(naming, [spl40_64])])).
fof(f475633, plain, (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_64), inference(avatar_contradiction_clause, [], [f475632])).
fof(f475632, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_64)), inference(subsumption_resolution, [], [f475590, f475263])).
fof(f475263, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n0)) | ~ spl40_2), inference(subsumption_resolution, [], [f475191, f672])).
fof(f475191, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, n0)) | ~ spl40_2), inference(resolution, [], [f1993, f241])).
fof(f475590, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n0)) | (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_64)), inference(backward_demodulation, [], [f475527, f1097])).
fof(f475523, plain, (~ spl40_2 | spl40_26 | ~ spl40_64 | ~ spl40_206), inference(avatar_contradiction_clause, [], [f475522])).
fof(f475522, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_64 | ~ spl40_206)), inference(subsumption_resolution, [], [f475449, f472370])).
fof(f472370, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n0)) | ~ spl40_2), inference(subsumption_resolution, [], [f472300, f672])).
fof(f472300, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, n0)) | ~ spl40_2), inference(resolution, [], [f1991, f241])).
fof(f1991, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, X3))) | ~ spl40_2), inference(subsumption_resolution, [], [f1970, f676])).
fof(f676, plain, leq(n1, n3), inference(resolution, [], [f243, f400])).
fof(f400, plain, gt(n3, n1), inference(cnf_transformation, [], [f70])).
fof(f70, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', gt_3_1)).
fof(f1970, plain, (! [X3] : (~ leq(n1, n3) | (s_sworst7_init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | ~ spl40_2), inference(resolution, [], [f659, f669])).
fof(f669, plain, leq(n0, n1), inference(resolution, [], [f243, f394])).
fof(f394, plain, gt(n1, n0), inference(cnf_transformation, [], [f64])).
fof(f64, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', gt_1_0)).
fof(f475449, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n0)) | (~ spl40_2 | spl40_26 | ~ spl40_64 | ~ spl40_206)), inference(backward_demodulation, [], [f475283, f1135])).
fof(f475283, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, sK38)) | (~ spl40_2 | spl40_26 | ~ spl40_206)), inference(backward_demodulation, [], [f1076, f35176])).
fof(f35176, plain, ((n1 = sK39) | ~ spl40_206), inference(avatar_component_clause, [], [f35174])).
fof(f35174, plain, (spl40_206 <=> (n1 = sK39)), introduced(avatar_definition, [new_symbols(naming, [spl40_206])])).
fof(f475279, plain, (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_218), inference(avatar_contradiction_clause, [], [f475278])).
fof(f475278, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_218)), inference(subsumption_resolution, [], [f475277, f473431])).
fof(f473431, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n1)) | (~ spl40_2 | spl40_26 | ~ spl40_58 | ~ spl40_218)), inference(backward_demodulation, [], [f473005, f1097])).
fof(f473005, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK39, n1)) | (~ spl40_2 | spl40_26 | ~ spl40_218)), inference(backward_demodulation, [], [f1076, f35895])).
fof(f35895, plain, ((n1 = sK38) | ~ spl40_218), inference(avatar_component_clause, [], [f35893])).
fof(f35893, plain, (spl40_218 <=> (n1 = sK38)), introduced(avatar_definition, [new_symbols(naming, [spl40_218])])).
fof(f475277, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n1)) | ~ spl40_2), inference(forward_demodulation, [], [f475276, f460])).
fof(f460, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f414, f318])).
fof(f318, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', succ_plus_1_r)).
fof(f414, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', successor_1)).
fof(f475276, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl40_2), inference(subsumption_resolution, [], [f475275, f673])).
fof(f673, plain, leq(n1, n2), inference(resolution, [], [f243, f399])).
fof(f399, plain, gt(n2, n1), inference(cnf_transformation, [], [f69])).
fof(f69, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', gt_2_1)).
fof(f475275, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl40_2), inference(forward_demodulation, [], [f475220, f460])).
fof(f475220, plain, (~ leq(plus(n0, n1), n2) | (s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl40_2), inference(resolution, [], [f1993, f670])).
fof(f670, plain, ! [X0] : leq(X0, plus(X0, n1)), inference(resolution, [], [f243, f420])).
fof(f420, plain, ! [X0] : gt(plus(X0, n1), X0), inference(definition_unfolding, [], [f247, f318])).
fof(f247, plain, ! [X0] : gt(succ(X0), X0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : gt(succ(X0), X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', gt_succ)).
fof(f473364, plain, (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_218), inference(avatar_contradiction_clause, [], [f473363])).
fof(f473363, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_218)), inference(subsumption_resolution, [], [f473288, f35075])).
fof(f35075, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n1)) | ~ spl40_2), inference(subsumption_resolution, [], [f35036, f673])).
fof(f35036, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, n1)) | ~ spl40_2), inference(resolution, [], [f1997, f669])).
fof(f473288, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n1)) | (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_218)), inference(backward_demodulation, [], [f473005, f1113])).
fof(f473221, plain, (spl40_58 | ~ spl40_27 | spl40_60 | spl40_206 | spl40_207 | ~ spl40_28), inference(avatar_split_clause, [], [f35154, f617, f35178, f35174, f1111, f612, f1095])).
fof(f612, plain, (spl40_27 <=> leq(sK39, n3)), introduced(avatar_definition, [new_symbols(naming, [spl40_27])])).
fof(f35178, plain, (spl40_207 <=> (n2 = sK39)), introduced(avatar_definition, [new_symbols(naming, [spl40_207])])).
fof(f617, plain, (spl40_28 <=> leq(n0, sK39)), introduced(avatar_definition, [new_symbols(naming, [spl40_28])])).
fof(f35154, plain, ((n2 = sK39) | (n1 = sK39) | (n0 = sK39) | ~ leq(sK39, n3) | (n3 = sK39) | ~ spl40_28), inference(resolution, [], [f619, f411])).
fof(f411, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f161])).
fof(f161, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f160])).
fof(f160, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', finite_domain_3)).
fof(f619, plain, (leq(n0, sK39) | ~ spl40_28), inference(avatar_component_clause, [], [f617])).
fof(f473215, plain, (~ spl40_2 | spl40_26 | ~ spl40_206 | ~ spl40_218), inference(avatar_contradiction_clause, [], [f473214])).
fof(f473214, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_206 | ~ spl40_218)), inference(subsumption_resolution, [], [f473172, f472368])).
fof(f472368, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n1)) | ~ spl40_2), inference(subsumption_resolution, [], [f472266, f673])).
fof(f472266, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, n1)) | ~ spl40_2), inference(resolution, [], [f1991, f669])).
fof(f473172, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n1)) | (~ spl40_2 | spl40_26 | ~ spl40_206 | ~ spl40_218)), inference(backward_demodulation, [], [f473005, f35176])).
fof(f473001, plain, (~ spl40_2 | spl40_26 | ~ spl40_64 | ~ spl40_207), inference(avatar_contradiction_clause, [], [f473000])).
fof(f473000, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_64 | ~ spl40_207)), inference(subsumption_resolution, [], [f472927, f472486])).
fof(f472486, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n0)) | ~ spl40_2), inference(subsumption_resolution, [], [f472414, f672])).
fof(f472414, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, n0)) | ~ spl40_2), inference(resolution, [], [f1992, f241])).
fof(f1992, plain, (! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, X4))) | ~ spl40_2), inference(subsumption_resolution, [], [f1971, f677])).
fof(f677, plain, leq(n2, n3), inference(resolution, [], [f243, f403])).
fof(f403, plain, gt(n3, n2), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', gt_3_2)).
fof(f1971, plain, (! [X4] : (~ leq(n2, n3) | (s_sworst7_init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | ~ spl40_2), inference(resolution, [], [f659, f672])).
fof(f472927, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n0)) | (~ spl40_2 | spl40_26 | ~ spl40_64 | ~ spl40_207)), inference(backward_demodulation, [], [f472863, f1135])).
fof(f472863, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, sK38)) | (~ spl40_2 | spl40_26 | ~ spl40_207)), inference(forward_demodulation, [], [f1076, f35180])).
fof(f35180, plain, ((n2 = sK39) | ~ spl40_207), inference(avatar_component_clause, [], [f35178])).
fof(f472859, plain, (~ spl40_2 | spl40_26 | ~ spl40_207 | ~ spl40_218), inference(avatar_contradiction_clause, [], [f472858])).
fof(f472858, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_207 | ~ spl40_218)), inference(subsumption_resolution, [], [f472816, f472482])).
fof(f472482, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n1)) | ~ spl40_2), inference(subsumption_resolution, [], [f472380, f673])).
fof(f472380, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, n1)) | ~ spl40_2), inference(resolution, [], [f1992, f669])).
fof(f472816, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n1)) | (~ spl40_2 | spl40_26 | ~ spl40_207 | ~ spl40_218)), inference(backward_demodulation, [], [f472649, f35180])).
fof(f472649, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK39, n1)) | (~ spl40_2 | spl40_26 | ~ spl40_218)), inference(backward_demodulation, [], [f1076, f35895])).
fof(f472646, plain, (~ spl40_2 | spl40_26 | ~ spl40_62 | ~ spl40_206), inference(avatar_contradiction_clause, [], [f472645])).
fof(f472645, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_62 | ~ spl40_206)), inference(subsumption_resolution, [], [f472603, f472369])).
fof(f472369, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n2)) | ~ spl40_2), inference(subsumption_resolution, [], [f472281, f241])).
fof(f472281, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, n2)) | ~ spl40_2), inference(resolution, [], [f1991, f672])).
fof(f472603, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n2)) | (~ spl40_2 | spl40_26 | ~ spl40_62 | ~ spl40_206)), inference(backward_demodulation, [], [f472536, f35176])).
fof(f472536, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK39, n2)) | (~ spl40_2 | spl40_26 | ~ spl40_62)), inference(forward_demodulation, [], [f1076, f1124])).
fof(f472504, plain, (~ spl40_2 | spl40_26 | ~ spl40_62 | spl40_65 | ~ spl40_207), inference(avatar_contradiction_clause, [], [f472503])).
fof(f472503, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_62 | spl40_65 | ~ spl40_207)), inference(subsumption_resolution, [], [f472502, f1168])).
fof(f1168, plain, (~ (n0 = n1) | spl40_65), inference(avatar_component_clause, [], [f1167])).
fof(f1167, plain, (spl40_65 <=> (n0 = n1)), introduced(avatar_definition, [new_symbols(naming, [spl40_65])])).
fof(f472502, plain, ((n0 = n1) | (~ spl40_2 | spl40_26 | ~ spl40_62 | ~ spl40_207)), inference(subsumption_resolution, [], [f472501, f5671])).
fof(f5671, plain, ~ gt(n0, n1), inference(superposition, [], [f4404, f460])).
fof(f4404, plain, ! [X1] : ~ gt(X1, plus(X1, n1)), inference(forward_demodulation, [], [f4397, f1246])).
fof(f1246, plain, ! [X2] : (plus(X2, n1) = plus(minus(X2, n1), n2)), inference(superposition, [], [f426, f435])).
fof(f435, plain, ! [X0] : (plus(minus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f330, f318, f328])).
fof(f328, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', pred_minus_1)).
fof(f330, plain, ! [X0] : (succ(pred(X0)) = X0), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (succ(pred(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', succ_pred)).
fof(f426, plain, ! [X0] : (plus(X0, n2) = plus(plus(X0, n1), n1)), inference(definition_unfolding, [], [f320, f318, f318])).
fof(f320, plain, ! [X0] : (plus(X0, n2) = succ(succ(X0))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (plus(X0, n2) = succ(succ(X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', succ_plus_2_r)).
fof(f4397, plain, ! [X1] : ~ gt(X1, plus(minus(X1, n1), n2)), inference(resolution, [], [f3212, f418])).
fof(f418, plain, ! [X0, X1] : (leq(X0, minus(X1, n1)) | ~ gt(X1, X0)), inference(definition_unfolding, [], [f246, f328])).
fof(f246, plain, ! [X0, X1] : (leq(X0, pred(X1)) | ~ gt(X1, X0)), inference(cnf_transformation, [], [f174])).
fof(f174, plain, ! [X0, X1] : ((leq(X0, pred(X1)) | ~ gt(X1, X0)) & (gt(X1, X0) | ~ leq(X0, pred(X1)))), inference(nnf_transformation, [], [f10])).
fof(f10, plain, ! [X0, X1] : (leq(X0, pred(X1)) <=> gt(X1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', leq_gt_pred)).
fof(f3212, plain, ! [X1] : ~ leq(plus(X1, n2), X1), inference(forward_demodulation, [], [f3194, f426])).
fof(f3194, plain, ! [X1] : ~ leq(plus(plus(X1, n1), n1), X1), inference(resolution, [], [f774, f421])).
fof(f421, plain, ! [X0, X1] : (leq(X0, plus(X1, n1)) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f248, f318])).
fof(f248, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0, X1] : (leq(X0, X1) => leq(X0, succ(X1))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', leq_succ)).
fof(f774, plain, ! [X2] : ~ leq(plus(X2, n1), X2), inference(resolution, [], [f423, f240])).
fof(f240, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', irreflexivity_gt)).
fof(f423, plain, ! [X0, X1] : (gt(plus(X1, n1), X0) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f249, f318])).
fof(f249, plain, ! [X0, X1] : (gt(succ(X1), X0) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0, X1] : ((leq(X0, X1) | ~ gt(succ(X1), X0)) & (gt(succ(X1), X0) | ~ leq(X0, X1))), inference(nnf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : (leq(X0, X1) <=> gt(succ(X1), X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', leq_succ_gt_equiv)).
fof(f472501, plain, (gt(n0, n1) | (n0 = n1) | (~ spl40_2 | spl40_26 | ~ spl40_62 | ~ spl40_207)), inference(subsumption_resolution, [], [f472500, f36190])).
fof(f36190, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n2)) | (~ spl40_2 | spl40_26 | ~ spl40_62 | ~ spl40_207)), inference(backward_demodulation, [], [f35713, f1124])).
fof(f35713, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, sK38)) | (~ spl40_2 | spl40_26 | ~ spl40_207)), inference(backward_demodulation, [], [f1076, f35180])).
fof(f472500, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n2)) | gt(n0, n1) | (n0 = n1) | ~ spl40_2), inference(subsumption_resolution, [], [f472466, f241])).
fof(f472466, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, n2)) | gt(n0, n1) | (n0 = n1) | ~ spl40_2), inference(resolution, [], [f1992, f6183])).
fof(f6183, plain, ! [X19] : (leq(X19, n2) | gt(X19, n1) | (n1 = X19)), inference(resolution, [], [f807, f750])).
fof(f750, plain, ! [X4] : (~ leq(X4, n1) | leq(X4, n2)), inference(superposition, [], [f421, f651])).
fof(f651, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f461, f460])).
fof(f461, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f415, f318, f318])).
fof(f415, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', successor_2)).
fof(f807, plain, ! [X2, X1] : (leq(X1, X2) | (X1 = X2) | gt(X1, X2)), inference(resolution, [], [f238, f243])).
fof(f238, plain, ! [X0, X1] : (gt(X1, X0) | gt(X0, X1) | (X0 = X1)), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : ((X0 = X1) | gt(X1, X0) | gt(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', totality)).
fof(f35935, plain, (spl40_62 | spl40_64 | spl40_218 | ~ spl40_29 | ~ spl40_30), inference(avatar_split_clause, [], [f35921, f627, f622, f35893, f1133, f1122])).
fof(f622, plain, (spl40_29 <=> leq(sK38, n2)), introduced(avatar_definition, [new_symbols(naming, [spl40_29])])).
fof(f627, plain, (spl40_30 <=> leq(n0, sK38)), introduced(avatar_definition, [new_symbols(naming, [spl40_30])])).
fof(f35921, plain, ((n1 = sK38) | (n0 = sK38) | (n2 = sK38) | (~ spl40_29 | ~ spl40_30)), inference(subsumption_resolution, [], [f35869, f624])).
fof(f624, plain, (leq(sK38, n2) | ~ spl40_29), inference(avatar_component_clause, [], [f622])).
fof(f35869, plain, ((n1 = sK38) | (n0 = sK38) | ~ leq(sK38, n2) | (n2 = sK38) | ~ spl40_30), inference(resolution, [], [f629, f410])).
fof(f410, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f159])).
fof(f159, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f158])).
fof(f158, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', finite_domain_2)).
fof(f629, plain, (leq(n0, sK38) | ~ spl40_30), inference(avatar_component_clause, [], [f627])).
fof(f35099, plain, (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_62), inference(avatar_contradiction_clause, [], [f35098])).
fof(f35098, plain, ($false | (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_62)), inference(subsumption_resolution, [], [f35097, f1760])).
fof(f1760, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n2)) | (~ spl40_2 | spl40_26 | ~ spl40_60 | ~ spl40_62)), inference(backward_demodulation, [], [f1759, f1113])).
fof(f1759, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK39, n2)) | (~ spl40_2 | spl40_26 | ~ spl40_62)), inference(backward_demodulation, [], [f1076, f1124])).
fof(f35097, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n2)) | ~ spl40_2), inference(forward_demodulation, [], [f35096, f1256])).
fof(f1256, plain, (n2 = plus(n0, n2)), inference(forward_demodulation, [], [f1239, f651])).
fof(f1239, plain, (plus(n1, n1) = plus(n0, n2)), inference(superposition, [], [f426, f460])).
fof(f35096, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, plus(n0, n2))) | ~ spl40_2), inference(subsumption_resolution, [], [f35095, f241])).
fof(f35095, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, plus(n0, n2))) | ~ spl40_2), inference(forward_demodulation, [], [f35063, f1256])).
fof(f35063, plain, (~ leq(plus(n0, n2), n2) | (s_sworst7_init = a_select3(simplex7_init, n0, plus(n0, n2))) | ~ spl40_2), inference(resolution, [], [f1997, f4313])).
fof(f4313, plain, ! [X2] : leq(X2, plus(X2, n2)), inference(resolution, [], [f800, f243])).
fof(f800, plain, ! [X1] : gt(plus(X1, n2), X1), inference(forward_demodulation, [], [f789, f426])).
fof(f789, plain, ! [X1] : gt(plus(plus(X1, n1), n1), X1), inference(resolution, [], [f438, f670])).
fof(f438, plain, ! [X0, X1] : (~ leq(plus(X0, n1), X1) | gt(X1, X0)), inference(definition_unfolding, [], [f333, f318])).
fof(f333, plain, ! [X0, X1] : (gt(X1, X0) | ~ leq(succ(X0), X1)), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ! [X0, X1] : (gt(X1, X0) | ~ leq(succ(X0), X1)), inference(ennf_transformation, [], [f43])).
fof(f43, plain, ! [X0, X1] : (leq(succ(X0), X1) => gt(X1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', leq_succ_gt)).
fof(f24920, plain, ~ spl40_65, inference(avatar_contradiction_clause, [], [f24919])).
fof(f24919, plain, ($false | ~ spl40_65), inference(subsumption_resolution, [], [f24918, f240])).
fof(f24918, plain, (! [X192] : gt(X192, X192) | ~ spl40_65), inference(forward_demodulation, [], [f24917, f20537])).
fof(f20537, plain, (! [X0] : (plus(n2, X0) = X0) | ~ spl40_65), inference(forward_demodulation, [], [f20455, f20449])).
fof(f20449, plain, (! [X8] : (plus(X8, n0) = X8) | ~ spl40_65), inference(forward_demodulation, [], [f20319, f16590])).
fof(f16590, plain, (! [X0] : (minus(plus(X0, n0), n0) = X0) | ~ spl40_65), inference(backward_demodulation, [], [f434, f1169])).
fof(f1169, plain, ((n0 = n1) | ~ spl40_65), inference(avatar_component_clause, [], [f1167])).
fof(f434, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f329, f328, f318])).
fof(f329, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', pred_succ)).
fof(f20319, plain, (! [X8] : (plus(X8, n0) = minus(plus(X8, n0), n0)) | ~ spl40_65), inference(backward_demodulation, [], [f16679, f20300])).
fof(f20300, plain, (! [X10] : (plus(X10, n2) = plus(X10, n0)) | ~ spl40_65), inference(forward_demodulation, [], [f20299, f16693])).
fof(f16693, plain, (! [X10] : (plus(X10, n0) = minus(plus(n2, X10), n0)) | ~ spl40_65), inference(backward_demodulation, [], [f1288, f1169])).
fof(f1288, plain, ! [X10] : (plus(X10, n1) = minus(plus(n2, X10), n1)), inference(superposition, [], [f434, f427])).
fof(f427, plain, ! [X0] : (plus(n2, X0) = plus(plus(X0, n1), n1)), inference(definition_unfolding, [], [f321, f318, f318])).
fof(f321, plain, ! [X0] : (succ(succ(X0)) = plus(n2, X0)), inference(cnf_transformation, [], [f32])).
fof(f32, plain, ! [X0] : (succ(succ(X0)) = plus(n2, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', succ_plus_2_l)).
fof(f20299, plain, (! [X10] : (plus(X10, n2) = minus(plus(n2, X10), n0)) | ~ spl40_65), inference(forward_demodulation, [], [f16704, f18273])).
fof(f18273, plain, ((n2 = n3) | ~ spl40_65), inference(forward_demodulation, [], [f16597, f1291])).
fof(f1291, plain, (n2 = plus(n2, n0)), inference(forward_demodulation, [], [f1271, f651])).
fof(f1271, plain, (plus(n1, n1) = plus(n2, n0)), inference(superposition, [], [f427, f460])).
fof(f16597, plain, ((n3 = plus(n2, n0)) | ~ spl40_65), inference(backward_demodulation, [], [f656, f1169])).
fof(f656, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f655, f651])).
fof(f655, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f654, f460])).
fof(f654, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f462, f425])).
fof(f425, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f319, f318])).
fof(f319, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', succ_plus_1_l)).
fof(f462, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f416, f318, f318, f318])).
fof(f416, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', successor_3)).
fof(f16704, plain, (! [X10] : (plus(X10, n2) = minus(plus(n3, X10), n0)) | ~ spl40_65), inference(backward_demodulation, [], [f1316, f1169])).
fof(f1316, plain, ! [X10] : (plus(X10, n2) = minus(plus(n3, X10), n1)), inference(superposition, [], [f434, f1266])).
fof(f1266, plain, ! [X0] : (plus(n3, X0) = plus(plus(X0, n2), n1)), inference(backward_demodulation, [], [f482, f1247])).
fof(f1247, plain, ! [X1] : (plus(X1, n2) = plus(n1, plus(X1, n1))), inference(superposition, [], [f426, f425])).
fof(f482, plain, ! [X0] : (plus(n3, X0) = plus(plus(n1, plus(X0, n1)), n1)), inference(forward_demodulation, [], [f429, f425])).
fof(f429, plain, ! [X0] : (plus(n3, X0) = plus(plus(plus(X0, n1), n1), n1)), inference(definition_unfolding, [], [f323, f318, f318, f318])).
fof(f323, plain, ! [X0] : (succ(succ(succ(X0))) = plus(n3, X0)), inference(cnf_transformation, [], [f34])).
fof(f34, plain, ! [X0] : (succ(succ(succ(X0))) = plus(n3, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', succ_plus_3_l)).
fof(f16679, plain, (! [X8] : (plus(X8, n0) = minus(plus(X8, n2), n0)) | ~ spl40_65), inference(backward_demodulation, [], [f1253, f1169])).
fof(f1253, plain, ! [X8] : (plus(X8, n1) = minus(plus(X8, n2), n1)), inference(superposition, [], [f434, f426])).
fof(f20455, plain, (! [X0] : (plus(n2, X0) = plus(X0, n0)) | ~ spl40_65), inference(backward_demodulation, [], [f16589, f20449])).
fof(f16589, plain, (! [X0] : (plus(n2, X0) = plus(plus(X0, n0), n0)) | ~ spl40_65), inference(backward_demodulation, [], [f427, f1169])).
fof(f24917, plain, (! [X192] : gt(plus(n2, X192), X192) | ~ spl40_65), inference(forward_demodulation, [], [f24916, f19312])).
fof(f19312, plain, ((n2 = n4) | ~ spl40_65), inference(forward_demodulation, [], [f19311, f1291])).
fof(f19311, plain, ((n4 = plus(n2, n0)) | ~ spl40_65), inference(forward_demodulation, [], [f16607, f18273])).
fof(f16607, plain, ((n4 = plus(n3, n0)) | ~ spl40_65), inference(backward_demodulation, [], [f693, f1169])).
fof(f693, plain, (n4 = plus(n3, n1)), inference(backward_demodulation, [], [f652, f689])).
fof(f689, plain, (n3 = plus(n1, n2)), inference(superposition, [], [f425, f656])).
fof(f652, plain, (n4 = plus(plus(n1, n2), n1)), inference(backward_demodulation, [], [f650, f651])).
fof(f650, plain, (n4 = plus(plus(n1, plus(n1, n1)), n1)), inference(backward_demodulation, [], [f645, f460])).
fof(f645, plain, (n4 = plus(plus(n1, plus(n1, plus(n0, n1))), n1)), inference(forward_demodulation, [], [f644, f425])).
fof(f644, plain, (n4 = plus(plus(n1, plus(plus(n0, n1), n1)), n1)), inference(forward_demodulation, [], [f458, f425])).
fof(f458, plain, (n4 = plus(plus(plus(plus(n0, n1), n1), n1), n1)), inference(definition_unfolding, [], [f412, f318, f318, f318, f318])).
fof(f412, plain, (n4 = succ(succ(succ(succ(n0))))), inference(cnf_transformation, [], [f82])).
fof(f82, plain, (n4 = succ(succ(succ(succ(n0))))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', successor_4)).
fof(f24916, plain, (! [X192] : gt(plus(n4, X192), X192) | ~ spl40_65), inference(forward_demodulation, [], [f18185, f20449])).
fof(f18185, plain, (! [X192] : gt(plus(n4, plus(X192, n0)), X192) | ~ spl40_65), inference(backward_demodulation, [], [f12020, f1169])).
fof(f12020, plain, ! [X192] : gt(plus(n4, plus(X192, n1)), X192), inference(forward_demodulation, [], [f11897, f4665])).
fof(f4665, plain, ! [X1] : (plus(n4, X1) = plus(plus(X1, n3), n1)), inference(superposition, [], [f1303, f1324])).
fof(f1324, plain, ! [X1] : (plus(n3, X1) = plus(X1, n3)), inference(superposition, [], [f1267, f1266])).
fof(f1267, plain, ! [X0] : (plus(X0, n3) = plus(plus(X0, n2), n1)), inference(backward_demodulation, [], [f481, f1247])).
fof(f481, plain, ! [X0] : (plus(X0, n3) = plus(plus(n1, plus(X0, n1)), n1)), inference(forward_demodulation, [], [f428, f425])).
fof(f428, plain, ! [X0] : (plus(X0, n3) = plus(plus(plus(X0, n1), n1), n1)), inference(definition_unfolding, [], [f322, f318, f318, f318])).
fof(f322, plain, ! [X0] : (plus(X0, n3) = succ(succ(succ(X0)))), inference(cnf_transformation, [], [f33])).
fof(f33, plain, ! [X0] : (plus(X0, n3) = succ(succ(succ(X0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', succ_plus_3_r)).
fof(f1303, plain, ! [X0] : (plus(n4, X0) = plus(plus(n3, X0), n1)), inference(backward_demodulation, [], [f1264, f1301])).
fof(f1301, plain, ! [X1] : (plus(n1, plus(X1, n2)) = plus(n3, X1)), inference(backward_demodulation, [], [f1258, f1300])).
fof(f1300, plain, ! [X9] : (plus(plus(X9, n1), n2) = plus(n3, X9)), inference(forward_demodulation, [], [f1299, f1295])).
fof(f1295, plain, ! [X1] : (plus(n1, plus(n2, X1)) = plus(n3, X1)), inference(backward_demodulation, [], [f1293, f1294])).
fof(f1294, plain, ! [X2] : (plus(n2, plus(X2, n1)) = plus(n3, X2)), inference(forward_demodulation, [], [f1274, f1266])).
fof(f1274, plain, ! [X2] : (plus(n2, plus(X2, n1)) = plus(plus(X2, n2), n1)), inference(superposition, [], [f427, f426])).
fof(f1293, plain, ! [X1] : (plus(n2, plus(X1, n1)) = plus(n1, plus(n2, X1))), inference(forward_demodulation, [], [f1273, f425])).
fof(f1273, plain, ! [X1] : (plus(n2, plus(X1, n1)) = plus(plus(n2, X1), n1)), inference(superposition, [], [f427, f427])).
fof(f1299, plain, ! [X9] : (plus(plus(X9, n1), n2) = plus(n1, plus(n2, X9))), inference(forward_demodulation, [], [f1287, f425])).
fof(f1287, plain, ! [X9] : (plus(plus(X9, n1), n2) = plus(plus(n2, X9), n1)), inference(superposition, [], [f426, f427])).
fof(f1258, plain, ! [X1] : (plus(plus(X1, n1), n2) = plus(n1, plus(X1, n2))), inference(forward_demodulation, [], [f1242, f425])).
fof(f1242, plain, ! [X1] : (plus(plus(X1, n1), n2) = plus(plus(X1, n2), n1)), inference(superposition, [], [f426, f426])).
fof(f1264, plain, ! [X0] : (plus(n4, X0) = plus(plus(n1, plus(X0, n2)), n1)), inference(backward_demodulation, [], [f486, f1247])).
fof(f486, plain, ! [X0] : (plus(n4, X0) = plus(plus(n1, plus(n1, plus(X0, n1))), n1)), inference(forward_demodulation, [], [f485, f425])).
fof(f485, plain, ! [X0] : (plus(n4, X0) = plus(plus(n1, plus(plus(X0, n1), n1)), n1)), inference(forward_demodulation, [], [f431, f425])).
fof(f431, plain, ! [X0] : (plus(n4, X0) = plus(plus(plus(plus(X0, n1), n1), n1), n1)), inference(definition_unfolding, [], [f325, f318, f318, f318, f318])).
fof(f325, plain, ! [X0] : (succ(succ(succ(succ(X0)))) = plus(n4, X0)), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (succ(succ(succ(succ(X0)))) = plus(n4, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', succ_plus_4_l)).
fof(f11897, plain, ! [X192] : gt(plus(plus(plus(X192, n1), n3), n1), X192), inference(resolution, [], [f9325, f790])).
fof(f790, plain, ! [X2, X3] : (~ leq(plus(X3, n1), X2) | gt(plus(X2, n1), X3)), inference(resolution, [], [f438, f421])).
fof(f9325, plain, ! [X0] : leq(X0, plus(X0, n3)), inference(resolution, [], [f1328, f4313])).
fof(f1328, plain, ! [X2, X3] : (~ leq(X3, plus(X2, n2)) | leq(X3, plus(X2, n3))), inference(superposition, [], [f421, f1267])).
fof(f1754, plain, (~ spl40_24 | ~ spl40_2 | spl40_23 | ~ spl40_25), inference(avatar_split_clause, [], [f1753, f602, f592, f498, f597])).
fof(f597, plain, (spl40_24 <=> leq(sK37, n3)), introduced(avatar_definition, [new_symbols(naming, [spl40_24])])).
fof(f592, plain, (spl40_23 <=> (s_worst7_init = a_select2(s_values7_init, sK37))), introduced(avatar_definition, [new_symbols(naming, [spl40_23])])).
fof(f602, plain, (spl40_25 <=> leq(n0, sK37)), introduced(avatar_definition, [new_symbols(naming, [spl40_25])])).
fof(f1753, plain, (~ leq(sK37, n3) | (~ spl40_2 | spl40_23 | ~ spl40_25)), inference(subsumption_resolution, [], [f1545, f1053])).
fof(f1053, plain, (~ (s_sworst7_init = a_select2(s_values7_init, sK37)) | (~ spl40_2 | spl40_23)), inference(forward_demodulation, [], [f594, f499])).
fof(f594, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK37)) | spl40_23), inference(avatar_component_clause, [], [f592])).
fof(f1545, plain, (~ leq(sK37, n3) | (s_sworst7_init = a_select2(s_values7_init, sK37)) | (~ spl40_2 | ~ spl40_25)), inference(resolution, [], [f660, f604])).
fof(f604, plain, (leq(n0, sK37) | ~ spl40_25), inference(avatar_component_clause, [], [f602])).
fof(f660, plain, (! [X2] : (~ leq(n0, X2) | ~ leq(X2, n3) | (s_sworst7_init = a_select2(s_values7_init, X2))) | ~ spl40_2), inference(backward_demodulation, [], [f455, f499])).
fof(f455, plain, ! [X2] : ((s_worst7_init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)), inference(definition_unfolding, [], [f377, f369])).
fof(f377, plain, ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f173])).
fof(f1724, plain, (~ spl40_2 | spl40_20 | ~ spl40_21 | ~ spl40_22), inference(avatar_contradiction_clause, [], [f1723])).
fof(f1723, plain, ($false | (~ spl40_2 | spl40_20 | ~ spl40_21 | ~ spl40_22)), inference(subsumption_resolution, [], [f1722, f1040])).
fof(f1040, plain, (~ (s_sworst7_init = a_select2(s_center7_init, sK36)) | (~ spl40_2 | spl40_20)), inference(forward_demodulation, [], [f579, f499])).
fof(f579, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK36)) | spl40_20), inference(avatar_component_clause, [], [f577])).
fof(f577, plain, (spl40_20 <=> (s_worst7_init = a_select2(s_center7_init, sK36))), introduced(avatar_definition, [new_symbols(naming, [spl40_20])])).
fof(f1722, plain, ((s_sworst7_init = a_select2(s_center7_init, sK36)) | (~ spl40_2 | ~ spl40_21 | ~ spl40_22)), inference(subsumption_resolution, [], [f1716, f584])).
fof(f584, plain, (leq(sK36, n2) | ~ spl40_21), inference(avatar_component_clause, [], [f582])).
fof(f582, plain, (spl40_21 <=> leq(sK36, n2)), introduced(avatar_definition, [new_symbols(naming, [spl40_21])])).
fof(f1716, plain, (~ leq(sK36, n2) | (s_sworst7_init = a_select2(s_center7_init, sK36)) | (~ spl40_2 | ~ spl40_22)), inference(resolution, [], [f589, f661])).
fof(f661, plain, (! [X1] : (~ leq(n0, X1) | ~ leq(X1, n2) | (s_sworst7_init = a_select2(s_center7_init, X1))) | ~ spl40_2), inference(backward_demodulation, [], [f454, f499])).
fof(f454, plain, ! [X1] : ((s_worst7_init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)), inference(definition_unfolding, [], [f378, f369])).
fof(f378, plain, ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f173])).
fof(f589, plain, (leq(n0, sK36) | ~ spl40_22), inference(avatar_component_clause, [], [f587])).
fof(f587, plain, (spl40_22 <=> leq(n0, sK36)), introduced(avatar_definition, [new_symbols(naming, [spl40_22])])).
fof(f1707, plain, (~ spl40_2 | spl40_12 | ~ spl40_16 | ~ spl40_17), inference(avatar_contradiction_clause, [], [f1706])).
fof(f1706, plain, ($false | (~ spl40_2 | spl40_12 | ~ spl40_16 | ~ spl40_17)), inference(subsumption_resolution, [], [f1705, f1099])).
fof(f1099, plain, (leq(sK35, n2) | ~ spl40_16), inference(forward_demodulation, [], [f557, f705])).
fof(f705, plain, (n2 = minus(n3, n1)), inference(superposition, [], [f434, f656])).
fof(f557, plain, (leq(sK35, minus(n3, n1)) | ~ spl40_16), inference(avatar_component_clause, [], [f555])).
fof(f555, plain, (spl40_16 <=> leq(sK35, minus(n3, n1))), introduced(avatar_definition, [new_symbols(naming, [spl40_16])])).
fof(f1705, plain, (~ leq(sK35, n2) | (~ spl40_2 | spl40_12 | ~ spl40_17)), inference(subsumption_resolution, [], [f1697, f657])).
fof(f657, plain, (~ (s_sworst7_init = a_select2(s_try7_init, sK35)) | (~ spl40_2 | spl40_12)), inference(backward_demodulation, [], [f540, f499])).
fof(f540, plain, (~ (s_worst7_init = a_select2(s_try7_init, sK35)) | spl40_12), inference(avatar_component_clause, [], [f538])).
fof(f538, plain, (spl40_12 <=> (s_worst7_init = a_select2(s_try7_init, sK35))), introduced(avatar_definition, [new_symbols(naming, [spl40_12])])).
fof(f1697, plain, ((s_sworst7_init = a_select2(s_try7_init, sK35)) | ~ leq(sK35, n2) | (~ spl40_2 | ~ spl40_17)), inference(resolution, [], [f708, f562])).
fof(f562, plain, (leq(n0, sK35) | ~ spl40_17), inference(avatar_component_clause, [], [f560])).
fof(f560, plain, (spl40_17 <=> leq(n0, sK35)), introduced(avatar_definition, [new_symbols(naming, [spl40_17])])).
fof(f708, plain, (! [X0] : (~ leq(n0, X0) | (s_sworst7_init = a_select2(s_try7_init, X0)) | ~ leq(X0, n2)) | ~ spl40_2), inference(backward_demodulation, [], [f662, f705])).
fof(f662, plain, (! [X0] : ((s_sworst7_init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) | ~ spl40_2), inference(backward_demodulation, [], [f453, f499])).
fof(f453, plain, ! [X0] : ((s_worst7_init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)), inference(definition_unfolding, [], [f379, f369])).
fof(f379, plain, ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)), inference(cnf_transformation, [], [f173])).
fof(f643, plain, spl40_2, inference(avatar_split_clause, [], [f457, f498])).
fof(f457, plain, (s_sworst7_init = s_worst7_init), inference(definition_unfolding, [], [f368, f369])).
fof(f368, plain, (init = s_sworst7_init), inference(cnf_transformation, [], [f173])).
fof(f642, plain, spl40_3, inference(avatar_split_clause, [], [f370, f502])).
fof(f502, plain, (spl40_3 <=> leq(n0, s_best7)), introduced(avatar_definition, [new_symbols(naming, [spl40_3])])).
fof(f370, plain, leq(n0, s_best7), inference(cnf_transformation, [], [f173])).
fof(f641, plain, spl40_4, inference(avatar_split_clause, [], [f371, f506])).
fof(f506, plain, (spl40_4 <=> leq(n0, s_sworst7)), introduced(avatar_definition, [new_symbols(naming, [spl40_4])])).
fof(f371, plain, leq(n0, s_sworst7), inference(cnf_transformation, [], [f173])).
fof(f640, plain, spl40_5, inference(avatar_split_clause, [], [f372, f510])).
fof(f510, plain, (spl40_5 <=> leq(n0, s_worst7)), introduced(avatar_definition, [new_symbols(naming, [spl40_5])])).
fof(f372, plain, leq(n0, s_worst7), inference(cnf_transformation, [], [f173])).
fof(f639, plain, spl40_6, inference(avatar_split_clause, [], [f373, f514])).
fof(f514, plain, (spl40_6 <=> leq(s_best7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl40_6])])).
fof(f373, plain, leq(s_best7, n3), inference(cnf_transformation, [], [f173])).
fof(f638, plain, spl40_7, inference(avatar_split_clause, [], [f374, f518])).
fof(f518, plain, (spl40_7 <=> leq(s_sworst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl40_7])])).
fof(f374, plain, leq(s_sworst7, n3), inference(cnf_transformation, [], [f173])).
fof(f637, plain, spl40_8, inference(avatar_split_clause, [], [f375, f522])).
fof(f522, plain, (spl40_8 <=> leq(s_worst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl40_8])])).
fof(f375, plain, leq(s_worst7, n3), inference(cnf_transformation, [], [f173])).
fof(f636, plain, (~ spl40_18 | spl40_13), inference(avatar_split_clause, [], [f452, f542, f565])).
fof(f565, plain, (spl40_18 <=> gt(loopcounter, n1)), introduced(avatar_definition, [new_symbols(naming, [spl40_18])])).
fof(f542, plain, (spl40_13 <=> (s_worst7_init = pvar1400_init)), introduced(avatar_definition, [new_symbols(naming, [spl40_13])])).
fof(f452, plain, ((s_worst7_init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f380, f369])).
fof(f380, plain, ((init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f173])).
fof(f635, plain, (~ spl40_18 | spl40_14), inference(avatar_split_clause, [], [f451, f546, f565])).
fof(f546, plain, (spl40_14 <=> (s_worst7_init = pvar1401_init)), introduced(avatar_definition, [new_symbols(naming, [spl40_14])])).
fof(f451, plain, ((s_worst7_init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f381, f369])).
fof(f381, plain, ((init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f173])).
fof(f634, plain, (~ spl40_18 | spl40_15), inference(avatar_split_clause, [], [f450, f550, f565])).
fof(f550, plain, (spl40_15 <=> (s_worst7_init = pvar1402_init)), introduced(avatar_definition, [new_symbols(naming, [spl40_15])])).
fof(f450, plain, ((s_worst7_init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f382, f369])).
fof(f382, plain, ((init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f173])).
fof(f632, plain, spl40_1, inference(avatar_split_clause, [], [f631, f494])).
fof(f494, plain, (spl40_1 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl40_1])])).
fof(f631, plain, sP7, inference(subsumption_resolution, [], [f467, f347])).
fof(f347, plain, true, inference(cnf_transformation, [], [f51])).
fof(f51, plain, true, file('/home/ubuntu/library/tptp/Problems/SWV/SWV038+1.p', ttrue)).
fof(f467, plain, (~ true | sP7), inference(trivial_inequality_removal, [], [f448])).
fof(f448, plain, (~ true | sP7 | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f384, f369, f369])).
fof(f384, plain, (~ true | sP7 | ~ (init = init)), inference(cnf_transformation, [], [f173])).
fof(f630, plain, (~ spl40_9 | spl40_30), inference(avatar_split_clause, [], [f362, f627, f526])).
fof(f526, plain, (spl40_9 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl40_9])])).
fof(f362, plain, (leq(n0, sK38) | ~ sP4), inference(cnf_transformation, [], [f237])).
fof(f237, plain, (((~ (init = a_select3(simplex7_init, sK39, sK38)) & leq(sK39, n3) & leq(n0, sK39)) & leq(sK38, n2) & leq(n0, sK38)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK38, sK39])], [f234, f236, f235])).
fof(f235, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK38)) & leq(X1, n3) & leq(n0, X1)) & leq(sK38, n2) & leq(n0, sK38))), introduced(choice_axiom, [])).
fof(f236, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK38)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK39, sK38)) & leq(sK39, n3) & leq(n0, sK39))), introduced(choice_axiom, [])).
fof(f234, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f233])).
fof(f233, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(nnf_transformation, [], [f169])).
fof(f625, plain, (~ spl40_9 | spl40_29), inference(avatar_split_clause, [], [f363, f622, f526])).
fof(f363, plain, (leq(sK38, n2) | ~ sP4), inference(cnf_transformation, [], [f237])).
fof(f620, plain, (~ spl40_9 | spl40_28), inference(avatar_split_clause, [], [f364, f617, f526])).
fof(f364, plain, (leq(n0, sK39) | ~ sP4), inference(cnf_transformation, [], [f237])).
fof(f615, plain, (~ spl40_9 | spl40_27), inference(avatar_split_clause, [], [f365, f612, f526])).
fof(f365, plain, (leq(sK39, n3) | ~ sP4), inference(cnf_transformation, [], [f237])).
fof(f610, plain, (~ spl40_9 | ~ spl40_26), inference(avatar_split_clause, [], [f447, f607, f526])).
fof(f447, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK39, sK38)) | ~ sP4), inference(definition_unfolding, [], [f366, f369])).
fof(f366, plain, (~ (init = a_select3(simplex7_init, sK39, sK38)) | ~ sP4), inference(cnf_transformation, [], [f237])).
fof(f605, plain, (~ spl40_10 | spl40_25), inference(avatar_split_clause, [], [f359, f602, f530])).
fof(f530, plain, (spl40_10 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl40_10])])).
fof(f359, plain, (leq(n0, sK37) | ~ sP5), inference(cnf_transformation, [], [f232])).
fof(f232, plain, ((~ (init = a_select2(s_values7_init, sK37)) & leq(sK37, n3) & leq(n0, sK37)) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK37])], [f230, f231])).
fof(f231, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) => (~ (init = a_select2(s_values7_init, sK37)) & leq(sK37, n3) & leq(n0, sK37))), introduced(choice_axiom, [])).
fof(f230, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) | ~ sP5), inference(rectify, [], [f229])).
fof(f229, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(nnf_transformation, [], [f170])).
fof(f600, plain, (~ spl40_10 | spl40_24), inference(avatar_split_clause, [], [f360, f597, f530])).
fof(f360, plain, (leq(sK37, n3) | ~ sP5), inference(cnf_transformation, [], [f232])).
fof(f595, plain, (~ spl40_10 | ~ spl40_23), inference(avatar_split_clause, [], [f446, f592, f530])).
fof(f446, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK37)) | ~ sP5), inference(definition_unfolding, [], [f361, f369])).
fof(f361, plain, (~ (init = a_select2(s_values7_init, sK37)) | ~ sP5), inference(cnf_transformation, [], [f232])).
fof(f590, plain, (~ spl40_11 | spl40_22), inference(avatar_split_clause, [], [f356, f587, f534])).
fof(f534, plain, (spl40_11 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl40_11])])).
fof(f356, plain, (leq(n0, sK36) | ~ sP6), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ((~ (init = a_select2(s_center7_init, sK36)) & leq(sK36, n2) & leq(n0, sK36)) | ~ sP6), inference(skolemisation, [status(esa), new_symbols(skolem, [sK36])], [f226, f227])).
fof(f227, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) => (~ (init = a_select2(s_center7_init, sK36)) & leq(sK36, n2) & leq(n0, sK36))), introduced(choice_axiom, [])).
fof(f226, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) | ~ sP6), inference(rectify, [], [f225])).
fof(f225, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(nnf_transformation, [], [f171])).
fof(f585, plain, (~ spl40_11 | spl40_21), inference(avatar_split_clause, [], [f357, f582, f534])).
fof(f357, plain, (leq(sK36, n2) | ~ sP6), inference(cnf_transformation, [], [f228])).
fof(f580, plain, (~ spl40_11 | ~ spl40_20), inference(avatar_split_clause, [], [f445, f577, f534])).
fof(f445, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK36)) | ~ sP6), inference(definition_unfolding, [], [f358, f369])).
fof(f358, plain, (~ (init = a_select2(s_center7_init, sK36)) | ~ sP6), inference(cnf_transformation, [], [f228])).
fof(f570, plain, (~ spl40_1 | ~ spl40_2 | ~ spl40_3 | ~ spl40_4 | ~ spl40_5 | ~ spl40_6 | ~ spl40_7 | ~ spl40_8 | spl40_9 | spl40_10 | spl40_11 | spl40_17 | spl40_18), inference(avatar_split_clause, [], [f469, f565, f560, f534, f530, f526, f522, f518, f514, f510, f506, f502, f498, f494])).
fof(f469, plain, (gt(loopcounter, n1) | leq(n0, sK35) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(trivial_inequality_removal, [], [f468])).
fof(f468, plain, (gt(loopcounter, n1) | leq(n0, sK35) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(duplicate_literal_removal, [], [f444])).
fof(f444, plain, (gt(loopcounter, n1) | leq(n0, sK35) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ sP7), inference(definition_unfolding, [], [f350, f369, f369, f417, f369, f369, f369])).
fof(f417, plain, (s_best7_init = s_worst7_init), inference(definition_unfolding, [], [f367, f369])).
fof(f367, plain, (s_best7_init = init), inference(cnf_transformation, [], [f173])).
fof(f350, plain, (gt(loopcounter, n1) | leq(n0, sK35) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init) | ~ sP7), inference(cnf_transformation, [], [f224])).
fof(f224, plain, (((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | (~ (init = a_select2(s_try7_init, sK35)) & leq(sK35, minus(n3, n1)) & leq(n0, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & ~ (n0 = pv1413)) | ~ sP7), inference(skolemisation, [status(esa), new_symbols(skolem, [sK35])], [f222, f223])).
fof(f223, plain, (? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) => (~ (init = a_select2(s_try7_init, sK35)) & leq(sK35, minus(n3, n1)) & leq(n0, sK35))), introduced(choice_axiom, [])).
fof(f222, plain, (((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & ~ (n0 = pv1413)) | ~ sP7), inference(rectify, [], [f221])).
fof(f221, plain, (((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init)) & ~ (n0 = pv1413)) | ~ sP7), inference(nnf_transformation, [], [f172])).
fof(f569, plain, (~ spl40_1 | ~ spl40_2 | ~ spl40_3 | ~ spl40_4 | ~ spl40_5 | ~ spl40_6 | ~ spl40_7 | ~ spl40_8 | spl40_9 | spl40_10 | spl40_11 | spl40_16 | spl40_18), inference(avatar_split_clause, [], [f471, f565, f555, f534, f530, f526, f522, f518, f514, f510, f506, f502, f498, f494])).
fof(f471, plain, (gt(loopcounter, n1) | leq(sK35, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(trivial_inequality_removal, [], [f470])).
fof(f470, plain, (gt(loopcounter, n1) | leq(sK35, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(duplicate_literal_removal, [], [f443])).
fof(f443, plain, (gt(loopcounter, n1) | leq(sK35, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ sP7), inference(definition_unfolding, [], [f351, f369, f369, f417, f369, f369, f369])).
fof(f351, plain, (gt(loopcounter, n1) | leq(sK35, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init) | ~ sP7), inference(cnf_transformation, [], [f224])).
fof(f568, plain, (~ spl40_1 | ~ spl40_2 | ~ spl40_3 | ~ spl40_4 | ~ spl40_5 | ~ spl40_6 | ~ spl40_7 | ~ spl40_8 | spl40_9 | spl40_10 | spl40_11 | ~ spl40_12 | spl40_18), inference(avatar_split_clause, [], [f473, f565, f538, f534, f530, f526, f522, f518, f514, f510, f506, f502, f498, f494])).
fof(f473, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(trivial_inequality_removal, [], [f472])).
fof(f472, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(duplicate_literal_removal, [], [f442])).
fof(f442, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ sP7), inference(definition_unfolding, [], [f352, f369, f369, f369, f417, f369, f369, f369])).
fof(f352, plain, (gt(loopcounter, n1) | ~ (init = a_select2(s_try7_init, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init) | ~ sP7), inference(cnf_transformation, [], [f224])).
fof(f563, plain, (~ spl40_1 | ~ spl40_2 | ~ spl40_3 | ~ spl40_4 | ~ spl40_5 | ~ spl40_6 | ~ spl40_7 | ~ spl40_8 | spl40_9 | spl40_10 | spl40_11 | spl40_17 | ~ spl40_13 | ~ spl40_14 | ~ spl40_15), inference(avatar_split_clause, [], [f475, f550, f546, f542, f560, f534, f530, f526, f522, f518, f514, f510, f506, f502, f498, f494])).
fof(f475, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK35) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(trivial_inequality_removal, [], [f474])).
fof(f474, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK35) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(duplicate_literal_removal, [], [f441])).
fof(f441, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK35) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ sP7), inference(definition_unfolding, [], [f353, f369, f369, f369, f369, f369, f417, f369, f369, f369])).
fof(f353, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(n0, sK35) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init) | ~ sP7), inference(cnf_transformation, [], [f224])).
fof(f558, plain, (~ spl40_1 | ~ spl40_2 | ~ spl40_3 | ~ spl40_4 | ~ spl40_5 | ~ spl40_6 | ~ spl40_7 | ~ spl40_8 | spl40_9 | spl40_10 | spl40_11 | spl40_16 | ~ spl40_13 | ~ spl40_14 | ~ spl40_15), inference(avatar_split_clause, [], [f477, f550, f546, f542, f555, f534, f530, f526, f522, f518, f514, f510, f506, f502, f498, f494])).
fof(f477, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK35, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(trivial_inequality_removal, [], [f476])).
fof(f476, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK35, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(duplicate_literal_removal, [], [f440])).
fof(f440, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK35, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ sP7), inference(definition_unfolding, [], [f354, f369, f369, f369, f369, f369, f417, f369, f369, f369])).
fof(f354, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(sK35, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init) | ~ sP7), inference(cnf_transformation, [], [f224])).
fof(f553, plain, (~ spl40_1 | ~ spl40_2 | ~ spl40_3 | ~ spl40_4 | ~ spl40_5 | ~ spl40_6 | ~ spl40_7 | ~ spl40_8 | spl40_9 | spl40_10 | spl40_11 | ~ spl40_12 | ~ spl40_13 | ~ spl40_14 | ~ spl40_15), inference(avatar_split_clause, [], [f479, f550, f546, f542, f538, f534, f530, f526, f522, f518, f514, f510, f506, f502, f498, f494])).
fof(f479, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(trivial_inequality_removal, [], [f478])).
fof(f478, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ sP7), inference(duplicate_literal_removal, [], [f439])).
fof(f439, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init) | ~ sP7), inference(definition_unfolding, [], [f355, f369, f369, f369, f369, f369, f369, f417, f369, f369, f369])).
fof(f355, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | ~ (init = a_select2(s_try7_init, sK35)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init) | ~ (init = init) | ~ sP7), inference(cnf_transformation, [], [f224])).