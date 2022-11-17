fof(f505614, plain, $false, inference(avatar_sat_refutation, [], [f496, f501, f506, f515, f520, f525, f534, f539, f544, f549, f554, f603, f608, f613, f618, f619, f620, f621, f622, f623, f624, f625, f626, f627, f628, f629, f630, f631, f1858, f1877, f1907, f1987, f2103, f2328, f2370, f2518, f504201, f504972, f504990, f505384, f505414, f505439, f505474, f505493, f505530, f505552, f505591, f505611])).
fof(f505611, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_115 | ~ spl39_117), inference(avatar_contradiction_clause, [], [f505610])).
fof(f505610, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_115 | ~ spl39_117)), inference(subsumption_resolution, [], [f505608, f504063])).
fof(f504063, plain, ((s_best7_init = a_select3(simplex7_init, n1, n1)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f503963, f665])).
fof(f665, plain, leq(n1, n2), inference(resolution, [], [f242, f397])).
fof(f397, plain, gt(n2, n1), inference(cnf_transformation, [], [f69])).
fof(f69, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', gt_2_1)).
fof(f242, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', leq_gt1)).
fof(f503963, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n1, n1)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2264, f661])).
fof(f661, plain, leq(n0, n1), inference(resolution, [], [f242, f392])).
fof(f392, plain, gt(n1, n0), inference(cnf_transformation, [], [f64])).
fof(f64, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', gt_1_0)).
fof(f2264, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (s_best7_init = a_select3(simplex7_init, n1, X3))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2242, f668])).
fof(f668, plain, leq(n1, n3), inference(resolution, [], [f242, f398])).
fof(f398, plain, gt(n3, n1), inference(cnf_transformation, [], [f70])).
fof(f70, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', gt_3_1)).
fof(f2242, plain, (! [X3] : (~ leq(n1, n3) | (s_best7_init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f653, f661])).
fof(f653, plain, (! [X4, X5] : (~ leq(n0, X5) | ~ leq(X5, n3) | (s_best7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f645, f649])).
fof(f649, plain, ((s_best7_init = s_sworst7_init) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f561, f557])).
fof(f557, plain, ((s_best7_init = s_worst7_init) | ~ spl39_15), inference(avatar_component_clause, [], [f556])).
fof(f556, plain, (spl39_15 <=> (s_best7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_15])])).
fof(f561, plain, ((s_sworst7_init = s_worst7_init) | ~ spl39_16), inference(avatar_component_clause, [], [f560])).
fof(f560, plain, (spl39_16 <=> (s_sworst7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_16])])).
fof(f645, plain, (! [X4, X5] : ((s_sworst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)) | ~ spl39_16), inference(backward_demodulation, [], [f451, f561])).
fof(f451, plain, ! [X4, X5] : ((s_worst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(definition_unfolding, [], [f370, f361])).
fof(f361, plain, (init = s_worst7_init), inference(cnf_transformation, [], [f236])).
fof(f236, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv1376, n3) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK38])], [f234, f235])).
fof(f235, plain, (? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) => (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38))), introduced(choice_axiom, [])).
fof(f234, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv1376, n3) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(rectify, [], [f173])).
fof(f173, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv1376, n3) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(definition_folding, [], [f150, e172, e171, e170])).
fof(f170, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(usedef, [], [e170])).
fof(e170, plain, (sP4 <=> ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f171, plain, (? [X7] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(usedef, [], [e171])).
fof(e171, plain, (sP5 <=> ? [X7] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X7)) & leq(X7, n3) & leq(n0, X7))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f172, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(usedef, [], [e172])).
fof(e172, plain, (sP6 <=> ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f150, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ? [X7] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X7)) & leq(X7, n3) & leq(n0, X7)) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv1376, n3) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(flattening, [], [f149])).
fof(f149, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & (leq(X5, minus(n3, n1)) & leq(n0, X5))) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & (leq(X6, n2) & leq(n0, X6))) | ? [X7] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X7)) & (leq(X7, n3) & leq(n0, X7))) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & (leq(X9, n3) & leq(n0, X9))) & (leq(X8, n2) & leq(n0, X8))) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & ((((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | (~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0))) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | (~ leq(X1, n2) | ~ leq(n0, X1))) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | (~ leq(X2, n3) | ~ leq(n0, X2))) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | (~ leq(X4, n3) | ~ leq(n0, X4))) | (~ leq(X3, n2) | ~ leq(n0, X3))) & leq(pv1376, n3) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(ennf_transformation, [], [f107])).
fof(f107, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X0] : ((leq(X0, minus(n3, n1)) & leq(n0, X0)) => (init = a_select2(s_try7_init, X0))) & ! [X1] : ((leq(X1, n2) & leq(n0, X1)) => (init = a_select2(s_center7_init, X1))) & ! [X2] : ((leq(X2, n3) & leq(n0, X2)) => (init = a_select2(s_values7_init, X2))) & ! [X3] : ((leq(X3, n2) & leq(n0, X3)) => ! [X4] : ((leq(X4, n3) & leq(n0, X4)) => (init = a_select3(simplex7_init, X4, X3)))) & leq(pv1376, n3) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X5] : ((leq(X5, minus(n3, n1)) & leq(n0, X5)) => (init = a_select2(s_try7_init, X5))) & ! [X6] : ((leq(X6, n2) & leq(n0, X6)) => (init = a_select2(s_center7_init, X6))) & ! [X7] : ((leq(X7, n3) & leq(n0, X7)) => (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X7))) & ! [X8] : ((leq(X8, n2) & leq(n0, X8)) => ! [X9] : ((leq(X9, n3) & leq(n0, X9)) => (init = a_select3(simplex7_init, X9, X8)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', gauss_init_0029)).
fof(f370, plain, ! [X4, X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(cnf_transformation, [], [f236])).
fof(f505608, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_115 | ~ spl39_117)), inference(backward_demodulation, [], [f505557, f1853])).
fof(f1853, plain, ((n1 = sK37) | ~ spl39_117), inference(avatar_component_clause, [], [f1851])).
fof(f1851, plain, (spl39_117 <=> (n1 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_117])])).
fof(f505557, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_115)), inference(backward_demodulation, [], [f1071, f1844])).
fof(f1844, plain, ((n1 = sK36) | ~ spl39_115), inference(avatar_component_clause, [], [f1842])).
fof(f1842, plain, (spl39_115 <=> (n1 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_115])])).
fof(f1071, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, sK36)) | (spl39_10 | ~ spl39_15)), inference(forward_demodulation, [], [f533, f557])).
fof(f533, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | spl39_10), inference(avatar_component_clause, [], [f531])).
fof(f531, plain, (spl39_10 <=> (s_worst7_init = a_select3(simplex7_init, sK37, sK36))), introduced(avatar_definition, [new_symbols(naming, [spl39_10])])).
fof(f505591, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_59 | ~ spl39_115), inference(avatar_contradiction_clause, [], [f505590])).
fof(f505590, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_59 | ~ spl39_115)), inference(subsumption_resolution, [], [f505588, f37795])).
fof(f37795, plain, ((s_best7_init = a_select3(simplex7_init, n0, n1)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f37755, f665])).
fof(f37755, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n0, n1)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2272, f661])).
fof(f2272, plain, (! [X37] : (~ leq(n0, X37) | ~ leq(X37, n2) | (s_best7_init = a_select3(simplex7_init, n0, X37))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2260, f666])).
fof(f666, plain, leq(n0, n3), inference(resolution, [], [f242, f394])).
fof(f394, plain, gt(n3, n0), inference(cnf_transformation, [], [f66])).
fof(f66, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', gt_3_0)).
fof(f2260, plain, (! [X37] : (~ leq(n0, n3) | (s_best7_init = a_select3(simplex7_init, n0, X37)) | ~ leq(X37, n2) | ~ leq(n0, X37)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f653, f240])).
fof(f240, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', reflexivity_leq)).
fof(f505588, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_59 | ~ spl39_115)), inference(backward_demodulation, [], [f505557, f1127])).
fof(f1127, plain, ((n0 = sK37) | ~ spl39_59), inference(avatar_component_clause, [], [f1125])).
fof(f1125, plain, (spl39_59 <=> (n0 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_59])])).
fof(f505552, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_61 | ~ spl39_117), inference(avatar_contradiction_clause, [], [f505551])).
fof(f505551, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_61 | ~ spl39_117)), inference(subsumption_resolution, [], [f505549, f504064])).
fof(f504064, plain, ((s_best7_init = a_select3(simplex7_init, n1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f503978, f240])).
fof(f503978, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2264, f664])).
fof(f664, plain, leq(n0, n2), inference(resolution, [], [f242, f393])).
fof(f393, plain, gt(n2, n0), inference(cnf_transformation, [], [f65])).
fof(f65, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', gt_2_0)).
fof(f505549, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_61 | ~ spl39_117)), inference(backward_demodulation, [], [f505498, f1853])).
fof(f505498, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_61)), inference(backward_demodulation, [], [f1071, f1138])).
fof(f1138, plain, ((n2 = sK36) | ~ spl39_61), inference(avatar_component_clause, [], [f1136])).
fof(f1136, plain, (spl39_61 <=> (n2 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_61])])).
fof(f505530, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_59 | ~ spl39_61), inference(avatar_contradiction_clause, [], [f505529])).
fof(f505529, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_59 | ~ spl39_61)), inference(subsumption_resolution, [], [f505527, f37796])).
fof(f37796, plain, ((s_best7_init = a_select3(simplex7_init, n0, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f37760, f240])).
fof(f37760, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n0, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2272, f664])).
fof(f505527, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_59 | ~ spl39_61)), inference(backward_demodulation, [], [f505498, f1127])).
fof(f505493, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_53 | ~ spl39_115), inference(avatar_contradiction_clause, [], [f505492])).
fof(f505492, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_53 | ~ spl39_115)), inference(subsumption_resolution, [], [f505490, f505375])).
fof(f505375, plain, ((s_best7_init = a_select3(simplex7_init, n3, n1)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f505275, f665])).
fof(f505275, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n3, n1)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2266, f661])).
fof(f2266, plain, (! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (s_best7_init = a_select3(simplex7_init, n3, X5))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2244, f240])).
fof(f2244, plain, (! [X5] : (~ leq(n3, n3) | (s_best7_init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f653, f666])).
fof(f505490, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_53 | ~ spl39_115)), inference(backward_demodulation, [], [f505476, f1844])).
fof(f505476, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_53)), inference(forward_demodulation, [], [f1071, f1081])).
fof(f1081, plain, ((n3 = sK37) | ~ spl39_53), inference(avatar_component_clause, [], [f1079])).
fof(f1079, plain, (spl39_53 <=> (n3 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_53])])).
fof(f505474, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_53 | ~ spl39_61), inference(avatar_contradiction_clause, [], [f505473])).
fof(f505473, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_53 | ~ spl39_61)), inference(subsumption_resolution, [], [f505471, f505376])).
fof(f505376, plain, ((s_best7_init = a_select3(simplex7_init, n3, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f505290, f240])).
fof(f505290, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n3, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2266, f664])).
fof(f505471, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_53 | ~ spl39_61)), inference(backward_demodulation, [], [f505443, f1081])).
fof(f505443, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_61)), inference(backward_demodulation, [], [f1071, f1138])).
fof(f505439, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_59 | ~ spl39_63), inference(avatar_contradiction_clause, [], [f505438])).
fof(f505438, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_59 | ~ spl39_63)), inference(subsumption_resolution, [], [f505436, f37797])).
fof(f37797, plain, ((s_best7_init = a_select3(simplex7_init, n0, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f37772, f664])).
fof(f37772, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n0, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2272, f240])).
fof(f505436, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_59 | ~ spl39_63)), inference(backward_demodulation, [], [f504994, f1127])).
fof(f504994, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_63)), inference(forward_demodulation, [], [f1071, f1149])).
fof(f1149, plain, ((n0 = sK36) | ~ spl39_63), inference(avatar_component_clause, [], [f1147])).
fof(f1147, plain, (spl39_63 <=> (n0 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_63])])).
fof(f505414, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_63 | ~ spl39_117), inference(avatar_contradiction_clause, [], [f505413])).
fof(f505413, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_63 | ~ spl39_117)), inference(subsumption_resolution, [], [f505411, f504065])).
fof(f504065, plain, ((s_best7_init = a_select3(simplex7_init, n1, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f503997, f664])).
fof(f503997, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n1, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2264, f240])).
fof(f505411, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_63 | ~ spl39_117)), inference(backward_demodulation, [], [f504994, f1853])).
fof(f505384, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_53 | ~ spl39_63 | spl39_115), inference(avatar_contradiction_clause, [], [f505383])).
fof(f505383, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_53 | ~ spl39_63 | spl39_115)), inference(subsumption_resolution, [], [f505382, f5422])).
fof(f5422, plain, ~ gt(n0, n1), inference(superposition, [], [f4496, f457])).
fof(f457, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f412, f317])).
fof(f317, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', succ_plus_1_r)).
fof(f412, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', successor_1)).
fof(f4496, plain, ! [X1] : ~ gt(X1, plus(X1, n1)), inference(forward_demodulation, [], [f4489, f1263])).
fof(f1263, plain, ! [X2] : (plus(X2, n1) = plus(minus(X2, n1), n2)), inference(superposition, [], [f423, f432])).
fof(f432, plain, ! [X0] : (plus(minus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f329, f317, f327])).
fof(f327, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', pred_minus_1)).
fof(f329, plain, ! [X0] : (succ(pred(X0)) = X0), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (succ(pred(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', succ_pred)).
fof(f423, plain, ! [X0] : (plus(X0, n2) = plus(plus(X0, n1), n1)), inference(definition_unfolding, [], [f319, f317, f317])).
fof(f319, plain, ! [X0] : (plus(X0, n2) = succ(succ(X0))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (plus(X0, n2) = succ(succ(X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', succ_plus_2_r)).
fof(f4489, plain, ! [X1] : ~ gt(X1, plus(minus(X1, n1), n2)), inference(resolution, [], [f871, f415])).
fof(f415, plain, ! [X0, X1] : (leq(X0, minus(X1, n1)) | ~ gt(X1, X0)), inference(definition_unfolding, [], [f245, f327])).
fof(f245, plain, ! [X0, X1] : (leq(X0, pred(X1)) | ~ gt(X1, X0)), inference(cnf_transformation, [], [f174])).
fof(f174, plain, ! [X0, X1] : ((leq(X0, pred(X1)) | ~ gt(X1, X0)) & (gt(X1, X0) | ~ leq(X0, pred(X1)))), inference(nnf_transformation, [], [f10])).
fof(f10, plain, ! [X0, X1] : (leq(X0, pred(X1)) <=> gt(X1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', leq_gt_pred)).
fof(f871, plain, ! [X0] : ~ leq(plus(X0, n2), X0), inference(forward_demodulation, [], [f861, f423])).
fof(f861, plain, ! [X0] : ~ leq(plus(plus(X0, n1), n1), X0), inference(resolution, [], [f773, f418])).
fof(f418, plain, ! [X0, X1] : (leq(X0, plus(X1, n1)) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f247, f317])).
fof(f247, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f116])).
fof(f116, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0, X1] : (leq(X0, X1) => leq(X0, succ(X1))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', leq_succ)).
fof(f773, plain, ! [X2] : ~ leq(plus(X2, n1), X2), inference(resolution, [], [f420, f239])).
fof(f239, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', irreflexivity_gt)).
fof(f420, plain, ! [X0, X1] : (gt(plus(X1, n1), X0) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f248, f317])).
fof(f248, plain, ! [X0, X1] : (gt(succ(X1), X0) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0, X1] : ((leq(X0, X1) | ~ gt(succ(X1), X0)) & (gt(succ(X1), X0) | ~ leq(X0, X1))), inference(nnf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : (leq(X0, X1) <=> gt(succ(X1), X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', leq_succ_gt_equiv)).
fof(f505382, plain, (gt(n0, n1) | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_53 | ~ spl39_63 | spl39_115)), inference(subsumption_resolution, [], [f505381, f504983])).
fof(f504983, plain, (~ (n0 = n1) | (~ spl39_63 | spl39_115)), inference(backward_demodulation, [], [f1843, f1149])).
fof(f1843, plain, (~ (n1 = sK36) | spl39_115), inference(avatar_component_clause, [], [f1842])).
fof(f505381, plain, ((n0 = n1) | gt(n0, n1) | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_53 | ~ spl39_63)), inference(subsumption_resolution, [], [f505380, f505010])).
fof(f505010, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_53 | ~ spl39_63)), inference(backward_demodulation, [], [f504994, f1081])).
fof(f505380, plain, ((s_best7_init = a_select3(simplex7_init, n3, n0)) | (n0 = n1) | gt(n0, n1) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f505312, f664])).
fof(f505312, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n3, n0)) | (n0 = n1) | gt(n0, n1) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2266, f42077])).
fof(f42077, plain, ! [X2] : (leq(X2, n0) | (n1 = X2) | gt(X2, n1)), inference(superposition, [], [f813, f457])).
fof(f813, plain, ! [X4, X3] : (gt(X3, plus(X4, n1)) | (plus(X4, n1) = X3) | leq(X3, X4)), inference(resolution, [], [f237, f419])).
fof(f419, plain, ! [X0, X1] : (~ gt(plus(X1, n1), X0) | leq(X0, X1)), inference(definition_unfolding, [], [f249, f317])).
fof(f249, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(succ(X1), X0)), inference(cnf_transformation, [], [f175])).
fof(f237, plain, ! [X0, X1] : (gt(X1, X0) | gt(X0, X1) | (X0 = X1)), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : ((X0 = X1) | gt(X1, X0) | gt(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', totality)).
fof(f504990, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_63 | ~ spl39_142), inference(avatar_contradiction_clause, [], [f504989])).
fof(f504989, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_63 | ~ spl39_142)), inference(subsumption_resolution, [], [f504987, f504184])).
fof(f504184, plain, ((s_best7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f504114, f664])).
fof(f504114, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2265, f240])).
fof(f2265, plain, (! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (s_best7_init = a_select3(simplex7_init, n2, X4))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2243, f669])).
fof(f669, plain, leq(n2, n3), inference(resolution, [], [f242, f401])).
fof(f401, plain, gt(n3, n2), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', gt_3_2)).
fof(f2243, plain, (! [X4] : (~ leq(n2, n3) | (s_best7_init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f653, f664])).
fof(f504987, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_63 | ~ spl39_142)), inference(backward_demodulation, [], [f504955, f1149])).
fof(f504955, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_142)), inference(forward_demodulation, [], [f1071, f2327])).
fof(f2327, plain, ((n2 = sK37) | ~ spl39_142), inference(avatar_component_clause, [], [f2325])).
fof(f2325, plain, (spl39_142 <=> (n2 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_142])])).
fof(f504972, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_61 | ~ spl39_142), inference(avatar_contradiction_clause, [], [f504971])).
fof(f504971, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_61 | ~ spl39_142)), inference(subsumption_resolution, [], [f504969, f504183])).
fof(f504183, plain, ((s_best7_init = a_select3(simplex7_init, n2, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f504095, f240])).
fof(f504095, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n2, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2265, f664])).
fof(f504969, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_61 | ~ spl39_142)), inference(backward_demodulation, [], [f504955, f1138])).
fof(f504201, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_115 | ~ spl39_142), inference(avatar_contradiction_clause, [], [f504200])).
fof(f504200, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_115 | ~ spl39_142)), inference(subsumption_resolution, [], [f504199, f2584])).
fof(f2584, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_115 | ~ spl39_142)), inference(backward_demodulation, [], [f2199, f2327])).
fof(f2199, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_115)), inference(backward_demodulation, [], [f1071, f1844])).
fof(f504199, plain, ((s_best7_init = a_select3(simplex7_init, n2, n1)) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f504198, f457])).
fof(f504198, plain, ((s_best7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f504197, f665])).
fof(f504197, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f504142, f457])).
fof(f504142, plain, (~ leq(plus(n0, n1), n2) | (s_best7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2265, f662])).
fof(f662, plain, ! [X0] : leq(X0, plus(X0, n1)), inference(resolution, [], [f242, f417])).
fof(f417, plain, ! [X0] : gt(plus(X0, n1), X0), inference(definition_unfolding, [], [f246, f317])).
fof(f246, plain, ! [X0] : gt(succ(X0), X0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : gt(succ(X0), X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', gt_succ)).
fof(f2518, plain, (~ spl39_117 | spl39_118), inference(avatar_contradiction_clause, [], [f2517])).
fof(f2517, plain, ($false | (~ spl39_117 | spl39_118)), inference(subsumption_resolution, [], [f2507, f240])).
fof(f2507, plain, (~ leq(n1, n1) | (~ spl39_117 | spl39_118)), inference(backward_demodulation, [], [f1857, f1853])).
fof(f1857, plain, (~ leq(sK37, n1) | spl39_118), inference(avatar_component_clause, [], [f1855])).
fof(f1855, plain, (spl39_118 <=> leq(sK37, n1)), introduced(avatar_definition, [new_symbols(naming, [spl39_118])])).
fof(f2370, plain, (~ spl39_59 | spl39_118), inference(avatar_contradiction_clause, [], [f2369])).
fof(f2369, plain, ($false | (~ spl39_59 | spl39_118)), inference(subsumption_resolution, [], [f2356, f661])).
fof(f2356, plain, (~ leq(n0, n1) | (~ spl39_59 | spl39_118)), inference(backward_demodulation, [], [f1857, f1127])).
fof(f2328, plain, (spl39_53 | spl39_59 | spl39_117 | spl39_142 | ~ spl39_11 | ~ spl39_12), inference(avatar_split_clause, [], [f2323, f541, f536, f2325, f1851, f1125, f1079])).
fof(f536, plain, (spl39_11 <=> leq(sK37, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_11])])).
fof(f541, plain, (spl39_12 <=> leq(n0, sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_12])])).
fof(f2323, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | (n3 = sK37) | (~ spl39_11 | ~ spl39_12)), inference(subsumption_resolution, [], [f2294, f538])).
fof(f538, plain, (leq(sK37, n3) | ~ spl39_11), inference(avatar_component_clause, [], [f536])).
fof(f2294, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | ~ leq(sK37, n3) | (n3 = sK37) | ~ spl39_12), inference(resolution, [], [f409, f543])).
fof(f543, plain, (leq(n0, sK37) | ~ spl39_12), inference(avatar_component_clause, [], [f541])).
fof(f409, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f162])).
fof(f162, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f161])).
fof(f161, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', finite_domain_3)).
fof(f2103, plain, (spl39_61 | spl39_63 | spl39_115 | ~ spl39_13 | ~ spl39_14), inference(avatar_split_clause, [], [f2102, f551, f546, f1842, f1147, f1136])).
fof(f546, plain, (spl39_13 <=> leq(sK36, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_13])])).
fof(f551, plain, (spl39_14 <=> leq(n0, sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_14])])).
fof(f2102, plain, ((n1 = sK36) | (n0 = sK36) | (n2 = sK36) | (~ spl39_13 | ~ spl39_14)), inference(subsumption_resolution, [], [f2095, f548])).
fof(f548, plain, (leq(sK36, n2) | ~ spl39_13), inference(avatar_component_clause, [], [f546])).
fof(f2095, plain, ((n1 = sK36) | (n0 = sK36) | ~ leq(sK36, n2) | (n2 = sK36) | ~ spl39_14), inference(resolution, [], [f408, f553])).
fof(f553, plain, (leq(n0, sK36) | ~ spl39_14), inference(avatar_component_clause, [], [f551])).
fof(f408, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f159])).
fof(f159, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', finite_domain_2)).
fof(f1987, plain, (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16), inference(avatar_contradiction_clause, [], [f1986])).
fof(f1986, plain, ($false | (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1985, f1935])).
fof(f1935, plain, (~ (s_best7_init = a_select2(s_center7_init, sK34)) | (spl39_2 | ~ spl39_15)), inference(forward_demodulation, [], [f495, f557])).
fof(f495, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | spl39_2), inference(avatar_component_clause, [], [f493])).
fof(f493, plain, (spl39_2 <=> (s_worst7_init = a_select2(s_center7_init, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl39_2])])).
fof(f1985, plain, ((s_best7_init = a_select2(s_center7_init, sK34)) | (~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1958, f500])).
fof(f500, plain, (leq(sK34, n2) | ~ spl39_3), inference(avatar_component_clause, [], [f498])).
fof(f498, plain, (spl39_3 <=> leq(sK34, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_3])])).
fof(f1958, plain, (~ leq(sK34, n2) | (s_best7_init = a_select2(s_center7_init, sK34)) | (~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f505, f651])).
fof(f651, plain, (! [X2] : (~ leq(n0, X2) | ~ leq(X2, n2) | (s_best7_init = a_select2(s_center7_init, X2))) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f647, f649])).
fof(f647, plain, (! [X2] : ((s_sworst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) | ~ spl39_16), inference(backward_demodulation, [], [f449, f561])).
fof(f449, plain, ! [X2] : ((s_worst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(definition_unfolding, [], [f372, f361])).
fof(f372, plain, ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f236])).
fof(f505, plain, (leq(n0, sK34) | ~ spl39_4), inference(avatar_component_clause, [], [f503])).
fof(f503, plain, (spl39_4 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl39_4])])).
fof(f1907, plain, (~ spl39_15 | ~ spl39_16 | spl39_23 | ~ spl39_27 | ~ spl39_28), inference(avatar_contradiction_clause, [], [f1906])).
fof(f1906, plain, ($false | (~ spl39_15 | ~ spl39_16 | spl39_23 | ~ spl39_27 | ~ spl39_28)), inference(subsumption_resolution, [], [f1905, f1888])).
fof(f1888, plain, (leq(sK38, n2) | ~ spl39_27), inference(forward_demodulation, [], [f607, f697])).
fof(f697, plain, (n2 = minus(n3, n1)), inference(superposition, [], [f431, f644])).
fof(f644, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f643, f639])).
fof(f639, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f458, f457])).
fof(f458, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f413, f317, f317])).
fof(f413, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', successor_2)).
fof(f643, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f642, f457])).
fof(f642, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f459, f422])).
fof(f422, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f318, f317])).
fof(f318, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', succ_plus_1_l)).
fof(f459, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f414, f317, f317, f317])).
fof(f414, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', successor_3)).
fof(f431, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f328, f327, f317])).
fof(f328, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', pred_succ)).
fof(f607, plain, (leq(sK38, minus(n3, n1)) | ~ spl39_27), inference(avatar_component_clause, [], [f605])).
fof(f605, plain, (spl39_27 <=> leq(sK38, minus(n3, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_27])])).
fof(f1905, plain, (~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_16 | spl39_23 | ~ spl39_28)), inference(subsumption_resolution, [], [f1890, f1084])).
fof(f1084, plain, (~ (s_best7_init = a_select2(s_try7_init, sK38)) | (~ spl39_15 | spl39_23)), inference(forward_demodulation, [], [f590, f557])).
fof(f590, plain, (~ (s_worst7_init = a_select2(s_try7_init, sK38)) | spl39_23), inference(avatar_component_clause, [], [f588])).
fof(f588, plain, (spl39_23 <=> (s_worst7_init = a_select2(s_try7_init, sK38))), introduced(avatar_definition, [new_symbols(naming, [spl39_23])])).
fof(f1890, plain, ((s_best7_init = a_select2(s_try7_init, sK38)) | ~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_16 | ~ spl39_28)), inference(resolution, [], [f612, f700])).
fof(f700, plain, (! [X1] : (~ leq(n0, X1) | (s_best7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f650, f697])).
fof(f650, plain, (! [X1] : ((s_best7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f648, f649])).
fof(f648, plain, (! [X1] : ((s_sworst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | ~ spl39_16), inference(backward_demodulation, [], [f448, f561])).
fof(f448, plain, ! [X1] : ((s_worst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(definition_unfolding, [], [f373, f361])).
fof(f373, plain, ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f236])).
fof(f612, plain, (leq(n0, sK38) | ~ spl39_28), inference(avatar_component_clause, [], [f610])).
fof(f610, plain, (spl39_28 <=> leq(n0, sK38)), introduced(avatar_definition, [new_symbols(naming, [spl39_28])])).
fof(f1877, plain, (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16), inference(avatar_contradiction_clause, [], [f1876])).
fof(f1876, plain, ($false | (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1865, f341])).
fof(f341, plain, ! [X2, X0, X1] : (a_select2(tptp_update2(X0, X1, X2), X1) = X2), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ! [X0, X1, X2] : (a_select2(tptp_update2(X0, X1, X2), X1) = X2), inference(rectify, [], [f48])).
fof(f48, plain, ! [X0, X6, X16] : (a_select2(tptp_update2(X0, X6, X16), X6) = X16), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', sel2_update_1)).
fof(f1865, plain, (~ (s_best7_init = a_select2(tptp_update2(s_values7_init, pv1376, s_best7_init), pv1376)) | (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f654, f1862])).
fof(f1862, plain, ((pv1376 = sK35) | (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1860, f1700])).
fof(f1700, plain, ((s_best7_init = a_select2(s_values7_init, sK35)) | (~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1677, f519])).
fof(f519, plain, (leq(sK35, n3) | ~ spl39_7), inference(avatar_component_clause, [], [f517])).
fof(f517, plain, (spl39_7 <=> leq(sK35, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_7])])).
fof(f1677, plain, (~ leq(sK35, n3) | (s_best7_init = a_select2(s_values7_init, sK35)) | (~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f652, f524])).
fof(f524, plain, (leq(n0, sK35) | ~ spl39_8), inference(avatar_component_clause, [], [f522])).
fof(f522, plain, (spl39_8 <=> leq(n0, sK35)), introduced(avatar_definition, [new_symbols(naming, [spl39_8])])).
fof(f652, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n3) | (s_best7_init = a_select2(s_values7_init, X3))) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f646, f649])).
fof(f646, plain, (! [X3] : ((s_sworst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ spl39_16), inference(backward_demodulation, [], [f450, f561])).
fof(f450, plain, ! [X3] : ((s_worst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(definition_unfolding, [], [f371, f361])).
fof(f371, plain, ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(cnf_transformation, [], [f236])).
fof(f1860, plain, (~ (s_best7_init = a_select2(s_values7_init, sK35)) | (pv1376 = sK35) | (spl39_6 | ~ spl39_15)), inference(superposition, [], [f654, f462])).
fof(f462, plain, ! [X4, X2, X0, X1] : ((a_select2(X2, X1) = a_select2(tptp_update2(X2, X0, X4), X1)) | (X0 = X1)), inference(equality_resolution, [], [f342])).
fof(f342, plain, ! [X4, X2, X0, X3, X1] : ((a_select2(tptp_update2(X2, X0, X4), X1) = X3) | ~ (a_select2(X2, X1) = X3) | (X0 = X1)), inference(cnf_transformation, [], [f146])).
fof(f146, plain, ! [X0, X1, X2, X3, X4] : ((a_select2(tptp_update2(X2, X0, X4), X1) = X3) | ~ (a_select2(X2, X1) = X3) | (X0 = X1)), inference(flattening, [], [f145])).
fof(f145, plain, ! [X0, X1, X2, X3, X4] : ((a_select2(tptp_update2(X2, X0, X4), X1) = X3) | (~ (a_select2(X2, X1) = X3) | (X0 = X1))), inference(ennf_transformation, [], [f105])).
fof(f105, plain, ! [X0, X1, X2, X3, X4] : (((a_select2(X2, X1) = X3) & ~ (X0 = X1)) => (a_select2(tptp_update2(X2, X0, X4), X1) = X3)), inference(rectify, [], [f49])).
fof(f49, plain, ! [X4, X6, X0, X16, X24] : (((a_select2(X0, X6) = X16) & ~ (X4 = X6)) => (a_select2(tptp_update2(X0, X4, X24), X6) = X16)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', sel2_update_2)).
fof(f654, plain, (~ (s_best7_init = a_select2(tptp_update2(s_values7_init, pv1376, s_best7_init), sK35)) | (spl39_6 | ~ spl39_15)), inference(forward_demodulation, [], [f514, f557])).
fof(f514, plain, (~ (s_worst7_init = a_select2(tptp_update2(s_values7_init, pv1376, s_worst7_init), sK35)) | spl39_6), inference(avatar_component_clause, [], [f512])).
fof(f512, plain, (spl39_6 <=> (s_worst7_init = a_select2(tptp_update2(s_values7_init, pv1376, s_worst7_init), sK35))), introduced(avatar_definition, [new_symbols(naming, [spl39_6])])).
fof(f1858, plain, (spl39_117 | ~ spl39_118 | spl39_59 | ~ spl39_12), inference(avatar_split_clause, [], [f1764, f541, f1125, f1855, f1851])).
fof(f1764, plain, ((n0 = sK37) | ~ leq(sK37, n1) | (n1 = sK37) | ~ spl39_12), inference(resolution, [], [f407, f543])).
fof(f407, plain, ! [X0] : (~ leq(n0, X0) | (n0 = X0) | ~ leq(X0, n1) | (n1 = X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : ((n1 = X0) | (n0 = X0) | ~ leq(X0, n1) | ~ leq(n0, X0)), inference(flattening, [], [f157])).
fof(f157, plain, ! [X0] : (((n1 = X0) | (n0 = X0)) | (~ leq(X0, n1) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : ((leq(X0, n1) & leq(n0, X0)) => ((n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV029+1.p', finite_domain_1)).
fof(f631, plain, spl39_15, inference(avatar_split_clause, [], [f453, f556])).
fof(f453, plain, (s_best7_init = s_worst7_init), inference(definition_unfolding, [], [f359, f361])).
fof(f359, plain, (init = s_best7_init), inference(cnf_transformation, [], [f236])).
fof(f630, plain, spl39_16, inference(avatar_split_clause, [], [f452, f560])).
fof(f452, plain, (s_sworst7_init = s_worst7_init), inference(definition_unfolding, [], [f360, f361])).
fof(f360, plain, (init = s_sworst7_init), inference(cnf_transformation, [], [f236])).
fof(f629, plain, spl39_17, inference(avatar_split_clause, [], [f362, f564])).
fof(f564, plain, (spl39_17 <=> leq(n0, s_best7)), introduced(avatar_definition, [new_symbols(naming, [spl39_17])])).
fof(f362, plain, leq(n0, s_best7), inference(cnf_transformation, [], [f236])).
fof(f628, plain, spl39_18, inference(avatar_split_clause, [], [f363, f568])).
fof(f568, plain, (spl39_18 <=> leq(n0, s_sworst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_18])])).
fof(f363, plain, leq(n0, s_sworst7), inference(cnf_transformation, [], [f236])).
fof(f627, plain, spl39_19, inference(avatar_split_clause, [], [f364, f572])).
fof(f572, plain, (spl39_19 <=> leq(n0, s_worst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_19])])).
fof(f364, plain, leq(n0, s_worst7), inference(cnf_transformation, [], [f236])).
fof(f626, plain, spl39_20, inference(avatar_split_clause, [], [f366, f576])).
fof(f576, plain, (spl39_20 <=> leq(s_best7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_20])])).
fof(f366, plain, leq(s_best7, n3), inference(cnf_transformation, [], [f236])).
fof(f625, plain, spl39_21, inference(avatar_split_clause, [], [f367, f580])).
fof(f580, plain, (spl39_21 <=> leq(s_sworst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_21])])).
fof(f367, plain, leq(s_sworst7, n3), inference(cnf_transformation, [], [f236])).
fof(f624, plain, spl39_22, inference(avatar_split_clause, [], [f368, f584])).
fof(f584, plain, (spl39_22 <=> leq(s_worst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_22])])).
fof(f368, plain, leq(s_worst7, n3), inference(cnf_transformation, [], [f236])).
fof(f623, plain, (~ spl39_29 | spl39_24), inference(avatar_split_clause, [], [f447, f592, f615])).
fof(f615, plain, (spl39_29 <=> gt(loopcounter, n1)), introduced(avatar_definition, [new_symbols(naming, [spl39_29])])).
fof(f592, plain, (spl39_24 <=> (s_worst7_init = pvar1400_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_24])])).
fof(f447, plain, ((s_worst7_init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f374, f361])).
fof(f374, plain, ((init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f236])).
fof(f622, plain, (~ spl39_29 | spl39_25), inference(avatar_split_clause, [], [f446, f596, f615])).
fof(f596, plain, (spl39_25 <=> (s_worst7_init = pvar1401_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_25])])).
fof(f446, plain, ((s_worst7_init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f375, f361])).
fof(f375, plain, ((init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f236])).
fof(f621, plain, (~ spl39_29 | spl39_26), inference(avatar_split_clause, [], [f445, f600, f615])).
fof(f600, plain, (spl39_26 <=> (s_worst7_init = pvar1402_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_26])])).
fof(f445, plain, ((s_worst7_init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f376, f361])).
fof(f376, plain, ((init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f236])).
fof(f620, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | spl39_9 | spl39_5 | spl39_1 | spl39_28 | spl39_29), inference(avatar_split_clause, [], [f464, f615, f610, f489, f508, f527, f584, f580, f576, f572, f568, f564, f560, f556])).
fof(f527, plain, (spl39_9 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl39_9])])).
fof(f508, plain, (spl39_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl39_5])])).
fof(f489, plain, (spl39_1 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl39_1])])).
fof(f464, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f463])).
fof(f463, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f444])).
fof(f444, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f377, f361, f361, f361, f361, f361])).
fof(f377, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f236])).
fof(f619, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | spl39_9 | spl39_5 | spl39_1 | spl39_27 | spl39_29), inference(avatar_split_clause, [], [f466, f615, f605, f489, f508, f527, f584, f580, f576, f572, f568, f564, f560, f556])).
fof(f466, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f465])).
fof(f465, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f443])).
fof(f443, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f378, f361, f361, f361, f361, f361])).
fof(f378, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f236])).
fof(f618, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_23 | spl39_29), inference(avatar_split_clause, [], [f468, f615, f588, f489, f508, f527, f584, f580, f576, f572, f568, f564, f560, f556])).
fof(f468, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f467])).
fof(f467, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f442])).
fof(f442, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f379, f361, f361, f361, f361, f361, f361])).
fof(f379, plain, (gt(loopcounter, n1) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f236])).
fof(f613, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | spl39_9 | spl39_5 | spl39_1 | spl39_28 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26), inference(avatar_split_clause, [], [f470, f600, f596, f592, f610, f489, f508, f527, f584, f580, f576, f572, f568, f564, f560, f556])).
fof(f470, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f469])).
fof(f469, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f441])).
fof(f441, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f380, f361, f361, f361, f361, f361, f361, f361, f361])).
fof(f380, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f236])).
fof(f608, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | spl39_9 | spl39_5 | spl39_1 | spl39_27 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26), inference(avatar_split_clause, [], [f472, f600, f596, f592, f605, f489, f508, f527, f584, f580, f576, f572, f568, f564, f560, f556])).
fof(f472, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f471])).
fof(f471, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f440])).
fof(f440, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f381, f361, f361, f361, f361, f361, f361, f361, f361])).
fof(f381, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f236])).
fof(f603, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26), inference(avatar_split_clause, [], [f474, f600, f596, f592, f588, f489, f508, f527, f584, f580, f576, f572, f568, f564, f560, f556])).
fof(f474, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f473])).
fof(f473, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f439])).
fof(f439, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f382, f361, f361, f361, f361, f361, f361, f361, f361, f361])).
fof(f382, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f236])).
fof(f554, plain, (~ spl39_9 | spl39_14), inference(avatar_split_clause, [], [f353, f551, f527])).
fof(f353, plain, (leq(n0, sK36) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f233, plain, (((~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37)) & leq(sK36, n2) & leq(n0, sK36)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK36, sK37])], [f230, f232, f231])).
fof(f231, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) & leq(sK36, n2) & leq(n0, sK36))), introduced(choice_axiom, [])).
fof(f232, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37))), introduced(choice_axiom, [])).
fof(f230, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f229])).
fof(f229, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(nnf_transformation, [], [f170])).
fof(f549, plain, (~ spl39_9 | spl39_13), inference(avatar_split_clause, [], [f354, f546, f527])).
fof(f354, plain, (leq(sK36, n2) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f544, plain, (~ spl39_9 | spl39_12), inference(avatar_split_clause, [], [f355, f541, f527])).
fof(f355, plain, (leq(n0, sK37) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f539, plain, (~ spl39_9 | spl39_11), inference(avatar_split_clause, [], [f356, f536, f527])).
fof(f356, plain, (leq(sK37, n3) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f534, plain, (~ spl39_9 | ~ spl39_10), inference(avatar_split_clause, [], [f438, f531, f527])).
fof(f438, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(definition_unfolding, [], [f357, f361])).
fof(f357, plain, (~ (init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f525, plain, (~ spl39_5 | spl39_8), inference(avatar_split_clause, [], [f350, f522, f508])).
fof(f350, plain, (leq(n0, sK35) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ((~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK35)) & leq(sK35, n3) & leq(n0, sK35)) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK35])], [f226, f227])).
fof(f227, plain, (? [X0] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X0)) & leq(X0, n3) & leq(n0, X0)) => (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK35)) & leq(sK35, n3) & leq(n0, sK35))), introduced(choice_axiom, [])).
fof(f226, plain, (? [X0] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X0)) & leq(X0, n3) & leq(n0, X0)) | ~ sP5), inference(rectify, [], [f225])).
fof(f225, plain, (? [X7] : (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(nnf_transformation, [], [f171])).
fof(f520, plain, (~ spl39_5 | spl39_7), inference(avatar_split_clause, [], [f351, f517, f508])).
fof(f351, plain, (leq(sK35, n3) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f515, plain, (~ spl39_5 | ~ spl39_6), inference(avatar_split_clause, [], [f437, f512, f508])).
fof(f437, plain, (~ (s_worst7_init = a_select2(tptp_update2(s_values7_init, pv1376, s_worst7_init), sK35)) | ~ sP5), inference(definition_unfolding, [], [f352, f361, f361])).
fof(f352, plain, (~ (init = a_select2(tptp_update2(s_values7_init, pv1376, init), sK35)) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f506, plain, (~ spl39_1 | spl39_4), inference(avatar_split_clause, [], [f347, f503, f489])).
fof(f347, plain, (leq(n0, sK34) | ~ sP6), inference(cnf_transformation, [], [f224])).
fof(f224, plain, ((~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34)) | ~ sP6), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34])], [f222, f223])).
fof(f223, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) => (~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f222, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) | ~ sP6), inference(rectify, [], [f221])).
fof(f221, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(nnf_transformation, [], [f172])).
fof(f501, plain, (~ spl39_1 | spl39_3), inference(avatar_split_clause, [], [f348, f498, f489])).
fof(f348, plain, (leq(sK34, n2) | ~ sP6), inference(cnf_transformation, [], [f224])).
fof(f496, plain, (~ spl39_1 | ~ spl39_2), inference(avatar_split_clause, [], [f436, f493, f489])).
fof(f436, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(definition_unfolding, [], [f349, f361])).
fof(f349, plain, (~ (init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(cnf_transformation, [], [f224])).