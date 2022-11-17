fof(f319979, plain, $false, inference(avatar_sat_refutation, [], [f532, f537, f542, f551, f556, f561, f570, f575, f580, f585, f590, f663, f668, f673, f678, f679, f680, f681, f682, f683, f684, f685, f686, f687, f688, f689, f690, f691, f692, f693, f694, f695, f696, f697, f1952, f2021, f319102, f319137, f319163, f319187, f319191, f319220, f319366, f319402, f319420, f319424, f319615, f319910, f319935, f319953, f319977])).
fof(f319977, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_182), inference(avatar_contradiction_clause, [], [f319976])).
fof(f319976, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_182)), inference(subsumption_resolution, [], [f319974, f36500])).
fof(f36500, plain, ((s_best7_init = a_select3(simplex7_init, n0, n1)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f36462, f741])).
fof(f741, plain, leq(n1, n2), inference(resolution, [], [f257, f429])).
fof(f429, plain, gt(n2, n1), inference(cnf_transformation, [], [f80])).
fof(f80, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', gt_2_1)).
fof(f257, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', leq_gt1)).
fof(f36462, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n0, n1)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2446, f726])).
fof(f726, plain, leq(n0, n1), inference(resolution, [], [f257, f422])).
fof(f422, plain, gt(n1, n0), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', gt_1_0)).
fof(f2446, plain, (! [X43] : (~ leq(n0, X43) | ~ leq(X43, n2) | (s_best7_init = a_select3(simplex7_init, n0, X43))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2435, f728])).
fof(f728, plain, leq(n0, n3), inference(resolution, [], [f257, f424])).
fof(f424, plain, gt(n3, n0), inference(cnf_transformation, [], [f75])).
fof(f75, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', gt_3_0)).
fof(f2435, plain, (! [X43] : (~ leq(n0, n3) | (s_best7_init = a_select3(simplex7_init, n0, X43)) | ~ leq(X43, n2) | ~ leq(n0, X43)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f719, f255])).
fof(f255, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', reflexivity_leq)).
fof(f719, plain, (! [X4, X5] : (~ leq(n0, X5) | ~ leq(X5, n3) | (s_best7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f711, f715])).
fof(f715, plain, ((s_best7_init = s_sworst7_init) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f597, f593])).
fof(f593, plain, ((s_best7_init = s_worst7_init) | ~ spl39_15), inference(avatar_component_clause, [], [f592])).
fof(f592, plain, (spl39_15 <=> (s_best7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_15])])).
fof(f597, plain, ((s_sworst7_init = s_worst7_init) | ~ spl39_16), inference(avatar_component_clause, [], [f596])).
fof(f596, plain, (spl39_16 <=> (s_sworst7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_16])])).
fof(f711, plain, (! [X4, X5] : ((s_sworst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)) | ~ spl39_16), inference(backward_demodulation, [], [f487, f597])).
fof(f487, plain, ! [X4, X5] : ((s_worst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(definition_unfolding, [], [f391, f376])).
fof(f376, plain, (init = s_worst7_init), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv9, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv9) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK38])], [f249, f250])).
fof(f250, plain, (? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) => (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38))), introduced(choice_axiom, [])).
fof(f249, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv9, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv9) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(rectify, [], [f188])).
fof(f188, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv9, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv9) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(definition_folding, [], [f165, e187, e186, e185])).
fof(f185, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(usedef, [], [e185])).
fof(e185, plain, (sP4 <=> ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f186, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(usedef, [], [e186])).
fof(e186, plain, (sP5 <=> ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f187, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(usedef, [], [e187])).
fof(e187, plain, (sP6 <=> ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f165, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv9, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv9) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(flattening, [], [f164])).
fof(f164, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & (leq(X5, minus(n3, n1)) & leq(n0, X5))) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & (leq(X6, n2) & leq(n0, X6))) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & (leq(X7, n3) & leq(n0, X7))) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & (leq(X9, n3) & leq(n0, X9))) & (leq(X8, n2) & leq(n0, X8))) | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & ((((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | (~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0))) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | (~ leq(X1, n2) | ~ leq(n0, X1))) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | (~ leq(X2, n3) | ~ leq(n0, X2))) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | (~ leq(X4, n3) | ~ leq(n0, X4))) | (~ leq(X3, n2) | ~ leq(n0, X3))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv9, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv9) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(ennf_transformation, [], [f122])).
fof(f122, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X0] : ((leq(X0, minus(n3, n1)) & leq(n0, X0)) => (init = a_select2(s_try7_init, X0))) & ! [X1] : ((leq(X1, n2) & leq(n0, X1)) => (init = a_select2(s_center7_init, X1))) & ! [X2] : ((leq(X2, n3) & leq(n0, X2)) => (init = a_select2(s_values7_init, X2))) & ! [X3] : ((leq(X3, n2) & leq(n0, X3)) => ! [X4] : ((leq(X4, n3) & leq(n0, X4)) => (init = a_select3(simplex7_init, X4, X3)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv9, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv9) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X5] : ((leq(X5, minus(n3, n1)) & leq(n0, X5)) => (init = a_select2(s_try7_init, X5))) & ! [X6] : ((leq(X6, n2) & leq(n0, X6)) => (init = a_select2(s_center7_init, X6))) & ! [X7] : ((leq(X7, n3) & leq(n0, X7)) => (init = a_select2(s_values7_init, X7))) & ! [X8] : ((leq(X8, n2) & leq(n0, X8)) => ! [X9] : ((leq(X9, n3) & leq(n0, X9)) => (init = a_select3(simplex7_init, X9, X8)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv9, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv9) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv9, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv9) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', gauss_init_0017)).
fof(f391, plain, ! [X4, X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(cnf_transformation, [], [f251])).
fof(f319974, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_77 | ~ spl39_182)), inference(backward_demodulation, [], [f319956, f1244])).
fof(f1244, plain, ((n0 = sK37) | ~ spl39_77), inference(avatar_component_clause, [], [f1242])).
fof(f1242, plain, (spl39_77 <=> (n0 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_77])])).
fof(f319956, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_182)), inference(forward_demodulation, [], [f1195, f2219])).
fof(f2219, plain, ((n1 = sK36) | ~ spl39_182), inference(avatar_component_clause, [], [f2217])).
fof(f2217, plain, (spl39_182 <=> (n1 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_182])])).
fof(f1195, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, sK36)) | (spl39_10 | ~ spl39_15)), inference(forward_demodulation, [], [f569, f593])).
fof(f569, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | spl39_10), inference(avatar_component_clause, [], [f567])).
fof(f567, plain, (spl39_10 <=> (s_worst7_init = a_select3(simplex7_init, sK37, sK36))), introduced(avatar_definition, [new_symbols(naming, [spl39_10])])).
fof(f319953, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_182 | ~ spl39_184), inference(avatar_contradiction_clause, [], [f319952])).
fof(f319952, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_182 | ~ spl39_184)), inference(subsumption_resolution, [], [f319950, f318960])).
fof(f318960, plain, ((s_best7_init = a_select3(simplex7_init, n1, n1)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f318845, f741])).
fof(f318845, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n1, n1)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2438, f726])).
fof(f2438, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (s_best7_init = a_select3(simplex7_init, n1, X3))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2411, f742])).
fof(f742, plain, leq(n1, n3), inference(resolution, [], [f257, f430])).
fof(f430, plain, gt(n3, n1), inference(cnf_transformation, [], [f81])).
fof(f81, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', gt_3_1)).
fof(f2411, plain, (! [X3] : (~ leq(n1, n3) | (s_best7_init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f719, f726])).
fof(f319950, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_182 | ~ spl39_184)), inference(backward_demodulation, [], [f319938, f2219])).
fof(f319938, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_184)), inference(forward_demodulation, [], [f1195, f2229])).
fof(f2229, plain, ((n1 = sK37) | ~ spl39_184), inference(avatar_component_clause, [], [f2227])).
fof(f2227, plain, (spl39_184 <=> (n1 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_184])])).
fof(f319935, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_79 | ~ spl39_184), inference(avatar_contradiction_clause, [], [f319934])).
fof(f319934, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_79 | ~ spl39_184)), inference(subsumption_resolution, [], [f319932, f318961])).
fof(f318961, plain, ((s_best7_init = a_select3(simplex7_init, n1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f318865, f255])).
fof(f318865, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2438, f727])).
fof(f727, plain, leq(n0, n2), inference(resolution, [], [f257, f423])).
fof(f423, plain, gt(n2, n0), inference(cnf_transformation, [], [f74])).
fof(f74, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', gt_2_0)).
fof(f319932, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_79 | ~ spl39_184)), inference(backward_demodulation, [], [f319874, f2229])).
fof(f319874, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_79)), inference(backward_demodulation, [], [f1195, f1261])).
fof(f1261, plain, ((n2 = sK36) | ~ spl39_79), inference(avatar_component_clause, [], [f1259])).
fof(f1259, plain, (spl39_79 <=> (n2 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_79])])).
fof(f319910, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_79), inference(avatar_contradiction_clause, [], [f319909])).
fof(f319909, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_79)), inference(subsumption_resolution, [], [f319901, f36501])).
fof(f36501, plain, ((s_best7_init = a_select3(simplex7_init, n0, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f36464, f255])).
fof(f36464, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n0, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2446, f727])).
fof(f319901, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_77 | ~ spl39_79)), inference(backward_demodulation, [], [f319874, f1244])).
fof(f319615, plain, (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16), inference(avatar_contradiction_clause, [], [f319614])).
fof(f319614, plain, ($false | (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f319613, f319425])).
fof(f319425, plain, (~ (s_best7_init = a_select2(s_center7_init, sK34)) | (spl39_2 | ~ spl39_15)), inference(forward_demodulation, [], [f531, f593])).
fof(f531, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | spl39_2), inference(avatar_component_clause, [], [f529])).
fof(f529, plain, (spl39_2 <=> (s_worst7_init = a_select2(s_center7_init, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl39_2])])).
fof(f319613, plain, ((s_best7_init = a_select2(s_center7_init, sK34)) | (~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f319509, f536])).
fof(f536, plain, (leq(sK34, n2) | ~ spl39_3), inference(avatar_component_clause, [], [f534])).
fof(f534, plain, (spl39_3 <=> leq(sK34, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_3])])).
fof(f319509, plain, (~ leq(sK34, n2) | (s_best7_init = a_select2(s_center7_init, sK34)) | (~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f541, f717])).
fof(f717, plain, (! [X2] : (~ leq(n0, X2) | ~ leq(X2, n2) | (s_best7_init = a_select2(s_center7_init, X2))) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f713, f715])).
fof(f713, plain, (! [X2] : ((s_sworst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) | ~ spl39_16), inference(backward_demodulation, [], [f485, f597])).
fof(f485, plain, ! [X2] : ((s_worst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(definition_unfolding, [], [f393, f376])).
fof(f393, plain, ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f251])).
fof(f541, plain, (leq(n0, sK34) | ~ spl39_4), inference(avatar_component_clause, [], [f539])).
fof(f539, plain, (spl39_4 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl39_4])])).
fof(f319424, plain, (spl39_79 | ~ spl39_13 | spl39_182 | ~ spl39_14 | spl39_81), inference(avatar_split_clause, [], [f319423, f1270, f587, f2217, f582, f1259])).
fof(f582, plain, (spl39_13 <=> leq(sK36, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_13])])).
fof(f587, plain, (spl39_14 <=> leq(n0, sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_14])])).
fof(f1270, plain, (spl39_81 <=> (n0 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_81])])).
fof(f319423, plain, ((n1 = sK36) | ~ leq(sK36, n2) | (n2 = sK36) | (~ spl39_14 | spl39_81)), inference(subsumption_resolution, [], [f2343, f1271])).
fof(f1271, plain, (~ (n0 = sK36) | spl39_81), inference(avatar_component_clause, [], [f1270])).
fof(f2343, plain, ((n1 = sK36) | (n0 = sK36) | ~ leq(sK36, n2) | (n2 = sK36) | ~ spl39_14), inference(resolution, [], [f444, f589])).
fof(f589, plain, (leq(n0, sK36) | ~ spl39_14), inference(avatar_component_clause, [], [f587])).
fof(f444, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f174])).
fof(f174, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f95])).
fof(f95, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', finite_domain_2)).
fof(f319420, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_182), inference(avatar_contradiction_clause, [], [f319419])).
fof(f319419, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_182)), inference(subsumption_resolution, [], [f319417, f319362])).
fof(f319362, plain, ((s_best7_init = a_select3(simplex7_init, n3, n1)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f319247, f741])).
fof(f319247, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n3, n1)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2440, f726])).
fof(f2440, plain, (! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (s_best7_init = a_select3(simplex7_init, n3, X5))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2413, f255])).
fof(f2413, plain, (! [X5] : (~ leq(n3, n3) | (s_best7_init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f719, f728])).
fof(f319417, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_71 | ~ spl39_182)), inference(backward_demodulation, [], [f319385, f2219])).
fof(f319385, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_71)), inference(forward_demodulation, [], [f1195, f1205])).
fof(f1205, plain, ((n3 = sK37) | ~ spl39_71), inference(avatar_component_clause, [], [f1203])).
fof(f1203, plain, (spl39_71 <=> (n3 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_71])])).
fof(f319402, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_79), inference(avatar_contradiction_clause, [], [f319401])).
fof(f319401, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_79)), inference(subsumption_resolution, [], [f319399, f319363])).
fof(f319363, plain, ((s_best7_init = a_select3(simplex7_init, n3, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f319267, f255])).
fof(f319267, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n3, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2440, f727])).
fof(f319399, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_71 | ~ spl39_79)), inference(backward_demodulation, [], [f319385, f1261])).
fof(f319366, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_81), inference(avatar_contradiction_clause, [], [f319365])).
fof(f319365, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_71 | ~ spl39_81)), inference(subsumption_resolution, [], [f319364, f319235])).
fof(f319235, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_71 | ~ spl39_81)), inference(backward_demodulation, [], [f319166, f1205])).
fof(f319166, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_81)), inference(forward_demodulation, [], [f1195, f1272])).
fof(f1272, plain, ((n0 = sK36) | ~ spl39_81), inference(avatar_component_clause, [], [f1270])).
fof(f319364, plain, ((s_best7_init = a_select3(simplex7_init, n3, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f319292, f727])).
fof(f319292, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n3, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2440, f255])).
fof(f319220, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_81), inference(avatar_contradiction_clause, [], [f319219])).
fof(f319219, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_81)), inference(subsumption_resolution, [], [f319215, f36502])).
fof(f36502, plain, ((s_best7_init = a_select3(simplex7_init, n0, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f36481, f727])).
fof(f36481, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n0, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2446, f255])).
fof(f319215, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_77 | ~ spl39_81)), inference(backward_demodulation, [], [f319166, f1244])).
fof(f319191, plain, (spl39_71 | ~ spl39_11 | spl39_77 | spl39_184 | spl39_192 | ~ spl39_12), inference(avatar_split_clause, [], [f2470, f577, f2502, f2227, f1242, f572, f1203])).
fof(f572, plain, (spl39_11 <=> leq(sK37, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_11])])).
fof(f2502, plain, (spl39_192 <=> (n2 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_192])])).
fof(f577, plain, (spl39_12 <=> leq(n0, sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_12])])).
fof(f2470, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | ~ leq(sK37, n3) | (n3 = sK37) | ~ spl39_12), inference(resolution, [], [f445, f579])).
fof(f579, plain, (leq(n0, sK37) | ~ spl39_12), inference(avatar_component_clause, [], [f577])).
fof(f445, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f176])).
fof(f176, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f96])).
fof(f96, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', finite_domain_3)).
fof(f319187, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_81 | ~ spl39_184), inference(avatar_contradiction_clause, [], [f319186])).
fof(f319186, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_81 | ~ spl39_184)), inference(subsumption_resolution, [], [f319184, f318962])).
fof(f318962, plain, ((s_best7_init = a_select3(simplex7_init, n1, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f318890, f727])).
fof(f318890, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n1, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2438, f255])).
fof(f319184, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_81 | ~ spl39_184)), inference(backward_demodulation, [], [f319166, f2229])).
fof(f319163, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_81 | ~ spl39_192), inference(avatar_contradiction_clause, [], [f319162])).
fof(f319162, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_81 | ~ spl39_192)), inference(subsumption_resolution, [], [f319156, f319094])).
fof(f319094, plain, ((s_best7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f319020, f727])).
fof(f319020, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2439, f255])).
fof(f2439, plain, (! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (s_best7_init = a_select3(simplex7_init, n2, X4))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2412, f747])).
fof(f747, plain, leq(n2, n3), inference(resolution, [], [f257, f435])).
fof(f435, plain, gt(n3, n2), inference(cnf_transformation, [], [f86])).
fof(f86, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', gt_3_2)).
fof(f2412, plain, (! [X4] : (~ leq(n2, n3) | (s_best7_init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f719, f727])).
fof(f319156, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_81 | ~ spl39_192)), inference(backward_demodulation, [], [f319120, f1272])).
fof(f319120, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_192)), inference(forward_demodulation, [], [f1195, f2504])).
fof(f2504, plain, ((n2 = sK37) | ~ spl39_192), inference(avatar_component_clause, [], [f2502])).
fof(f319137, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_182 | ~ spl39_192), inference(avatar_contradiction_clause, [], [f319136])).
fof(f319136, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_182 | ~ spl39_192)), inference(subsumption_resolution, [], [f319134, f319090])).
fof(f319090, plain, ((s_best7_init = a_select3(simplex7_init, n2, n1)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f318975, f741])).
fof(f318975, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n2, n1)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2439, f726])).
fof(f319134, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_182 | ~ spl39_192)), inference(backward_demodulation, [], [f319120, f2219])).
fof(f319102, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_79 | ~ spl39_192), inference(avatar_contradiction_clause, [], [f319101])).
fof(f319101, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_79 | ~ spl39_192)), inference(subsumption_resolution, [], [f319100, f2813])).
fof(f2813, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_79 | ~ spl39_192)), inference(backward_demodulation, [], [f2394, f2504])).
fof(f2394, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_79)), inference(backward_demodulation, [], [f1195, f1261])).
fof(f319100, plain, ((s_best7_init = a_select3(simplex7_init, n2, n2)) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f319099, f1463])).
fof(f1463, plain, (n2 = plus(n2, n0)), inference(forward_demodulation, [], [f1444, f705])).
fof(f705, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f494, f493])).
fof(f493, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f448, f332])).
fof(f332, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', succ_plus_1_r)).
fof(f448, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', successor_1)).
fof(f494, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f449, f332, f332])).
fof(f449, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f100])).
fof(f100, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', successor_2)).
fof(f1444, plain, (plus(n1, n1) = plus(n2, n0)), inference(superposition, [], [f460, f493])).
fof(f460, plain, ! [X0] : (plus(n2, X0) = plus(plus(X0, n1), n1)), inference(definition_unfolding, [], [f335, f332, f332])).
fof(f335, plain, ! [X0] : (succ(succ(X0)) = plus(n2, X0)), inference(cnf_transformation, [], [f32])).
fof(f32, plain, ! [X0] : (succ(succ(X0)) = plus(n2, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', succ_plus_2_l)).
fof(f319099, plain, ((s_best7_init = a_select3(simplex7_init, n2, plus(n2, n0))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f319098, f255])).
fof(f319098, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n2, plus(n2, n0))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f319030, f1463])).
fof(f319030, plain, (~ leq(plus(n2, n0), n2) | (s_best7_init = a_select3(simplex7_init, n2, plus(n2, n0))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2439, f28082])).
fof(f28082, plain, ! [X13] : leq(X13, plus(n2, X13)), inference(forward_demodulation, [], [f28058, f460])).
fof(f28058, plain, ! [X13] : leq(X13, plus(plus(X13, n1), n1)), inference(resolution, [], [f1662, f725])).
fof(f725, plain, ! [X0] : leq(X0, plus(X0, n1)), inference(resolution, [], [f257, f453])).
fof(f453, plain, ! [X0] : gt(plus(X0, n1), X0), inference(definition_unfolding, [], [f261, f332])).
fof(f261, plain, ! [X0] : gt(succ(X0), X0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : gt(succ(X0), X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', gt_succ)).
fof(f1662, plain, ! [X2, X3] : (~ leq(plus(X2, n1), X3) | leq(X2, X3)), inference(resolution, [], [f470, f454])).
fof(f454, plain, ! [X0, X1] : (leq(X0, plus(X1, n1)) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f262, f332])).
fof(f262, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0, X1] : (leq(X0, succ(X1)) | ~ leq(X0, X1)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0, X1] : (leq(X0, X1) => leq(X0, succ(X1))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', leq_succ)).
fof(f470, plain, ! [X0, X1] : (~ leq(plus(X0, n1), plus(X1, n1)) | leq(X0, X1)), inference(definition_unfolding, [], [f345, f332, f332])).
fof(f345, plain, ! [X0, X1] : (leq(X0, X1) | ~ leq(succ(X0), succ(X1))), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ! [X0, X1] : ((leq(succ(X0), succ(X1)) | ~ leq(X0, X1)) & (leq(X0, X1) | ~ leq(succ(X0), succ(X1)))), inference(nnf_transformation, [], [f42])).
fof(f42, plain, ! [X0, X1] : (leq(succ(X0), succ(X1)) <=> leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', leq_succ_succ)).
fof(f2021, plain, (~ spl39_15 | ~ spl39_16 | spl39_29 | ~ spl39_33 | ~ spl39_34), inference(avatar_contradiction_clause, [], [f2020])).
fof(f2020, plain, ($false | (~ spl39_15 | ~ spl39_16 | spl39_29 | ~ spl39_33 | ~ spl39_34)), inference(subsumption_resolution, [], [f2019, f1207])).
fof(f1207, plain, (leq(sK38, n2) | ~ spl39_33), inference(forward_demodulation, [], [f667, f778])).
fof(f778, plain, (n2 = minus(n3, n1)), inference(superposition, [], [f467, f710])).
fof(f710, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f709, f705])).
fof(f709, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f708, f493])).
fof(f708, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f495, f458])).
fof(f458, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f333, f332])).
fof(f333, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', succ_plus_1_l)).
fof(f495, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f450, f332, f332, f332])).
fof(f450, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f101])).
fof(f101, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', successor_3)).
fof(f467, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f343, f342, f332])).
fof(f342, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', pred_minus_1)).
fof(f343, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV026+1.p', pred_succ)).
fof(f667, plain, (leq(sK38, minus(n3, n1)) | ~ spl39_33), inference(avatar_component_clause, [], [f665])).
fof(f665, plain, (spl39_33 <=> leq(sK38, minus(n3, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_33])])).
fof(f2019, plain, (~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_16 | spl39_29 | ~ spl39_34)), inference(subsumption_resolution, [], [f2013, f1208])).
fof(f1208, plain, (~ (s_best7_init = a_select2(s_try7_init, sK38)) | (~ spl39_15 | spl39_29)), inference(forward_demodulation, [], [f650, f593])).
fof(f650, plain, (~ (s_worst7_init = a_select2(s_try7_init, sK38)) | spl39_29), inference(avatar_component_clause, [], [f648])).
fof(f648, plain, (spl39_29 <=> (s_worst7_init = a_select2(s_try7_init, sK38))), introduced(avatar_definition, [new_symbols(naming, [spl39_29])])).
fof(f2013, plain, ((s_best7_init = a_select2(s_try7_init, sK38)) | ~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_16 | ~ spl39_34)), inference(resolution, [], [f781, f672])).
fof(f672, plain, (leq(n0, sK38) | ~ spl39_34), inference(avatar_component_clause, [], [f670])).
fof(f670, plain, (spl39_34 <=> leq(n0, sK38)), introduced(avatar_definition, [new_symbols(naming, [spl39_34])])).
fof(f781, plain, (! [X1] : (~ leq(n0, X1) | (s_best7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f716, f778])).
fof(f716, plain, (! [X1] : ((s_best7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f714, f715])).
fof(f714, plain, (! [X1] : ((s_sworst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | ~ spl39_16), inference(backward_demodulation, [], [f484, f597])).
fof(f484, plain, ! [X1] : ((s_worst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(definition_unfolding, [], [f394, f376])).
fof(f394, plain, ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f251])).
fof(f1952, plain, (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16), inference(avatar_contradiction_clause, [], [f1951])).
fof(f1951, plain, ($false | (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1950, f720])).
fof(f720, plain, (~ (s_best7_init = a_select2(s_values7_init, sK35)) | (spl39_6 | ~ spl39_15)), inference(forward_demodulation, [], [f550, f593])).
fof(f550, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | spl39_6), inference(avatar_component_clause, [], [f548])).
fof(f548, plain, (spl39_6 <=> (s_worst7_init = a_select2(s_values7_init, sK35))), introduced(avatar_definition, [new_symbols(naming, [spl39_6])])).
fof(f1950, plain, ((s_best7_init = a_select2(s_values7_init, sK35)) | (~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1945, f555])).
fof(f555, plain, (leq(sK35, n3) | ~ spl39_7), inference(avatar_component_clause, [], [f553])).
fof(f553, plain, (spl39_7 <=> leq(sK35, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_7])])).
fof(f1945, plain, (~ leq(sK35, n3) | (s_best7_init = a_select2(s_values7_init, sK35)) | (~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f560, f718])).
fof(f718, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n3) | (s_best7_init = a_select2(s_values7_init, X3))) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f712, f715])).
fof(f712, plain, (! [X3] : ((s_sworst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ spl39_16), inference(backward_demodulation, [], [f486, f597])).
fof(f486, plain, ! [X3] : ((s_worst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(definition_unfolding, [], [f392, f376])).
fof(f392, plain, ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(cnf_transformation, [], [f251])).
fof(f560, plain, (leq(n0, sK35) | ~ spl39_8), inference(avatar_component_clause, [], [f558])).
fof(f558, plain, (spl39_8 <=> leq(n0, sK35)), introduced(avatar_definition, [new_symbols(naming, [spl39_8])])).
fof(f697, plain, spl39_15, inference(avatar_split_clause, [], [f489, f592])).
fof(f489, plain, (s_best7_init = s_worst7_init), inference(definition_unfolding, [], [f374, f376])).
fof(f374, plain, (init = s_best7_init), inference(cnf_transformation, [], [f251])).
fof(f696, plain, spl39_16, inference(avatar_split_clause, [], [f488, f596])).
fof(f488, plain, (s_sworst7_init = s_worst7_init), inference(definition_unfolding, [], [f375, f376])).
fof(f375, plain, (init = s_sworst7_init), inference(cnf_transformation, [], [f251])).
fof(f695, plain, spl39_17, inference(avatar_split_clause, [], [f377, f600])).
fof(f600, plain, (spl39_17 <=> leq(n0, s_best7)), introduced(avatar_definition, [new_symbols(naming, [spl39_17])])).
fof(f377, plain, leq(n0, s_best7), inference(cnf_transformation, [], [f251])).
fof(f694, plain, spl39_18, inference(avatar_split_clause, [], [f378, f604])).
fof(f604, plain, (spl39_18 <=> leq(n0, s_sworst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_18])])).
fof(f378, plain, leq(n0, s_sworst7), inference(cnf_transformation, [], [f251])).
fof(f693, plain, spl39_19, inference(avatar_split_clause, [], [f379, f608])).
fof(f608, plain, (spl39_19 <=> leq(n0, s_worst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_19])])).
fof(f379, plain, leq(n0, s_worst7), inference(cnf_transformation, [], [f251])).
fof(f692, plain, spl39_20, inference(avatar_split_clause, [], [f381, f612])).
fof(f612, plain, (spl39_20 <=> leq(n0, pv19)), introduced(avatar_definition, [new_symbols(naming, [spl39_20])])).
fof(f381, plain, leq(n0, pv19), inference(cnf_transformation, [], [f251])).
fof(f691, plain, spl39_21, inference(avatar_split_clause, [], [f382, f616])).
fof(f616, plain, (spl39_21 <=> leq(n0, pv20)), introduced(avatar_definition, [new_symbols(naming, [spl39_21])])).
fof(f382, plain, leq(n0, pv20), inference(cnf_transformation, [], [f251])).
fof(f690, plain, spl39_22, inference(avatar_split_clause, [], [f383, f620])).
fof(f620, plain, (spl39_22 <=> leq(n0, pv1376)), introduced(avatar_definition, [new_symbols(naming, [spl39_22])])).
fof(f383, plain, leq(n0, pv1376), inference(cnf_transformation, [], [f251])).
fof(f689, plain, spl39_23, inference(avatar_split_clause, [], [f384, f624])).
fof(f624, plain, (spl39_23 <=> leq(s_best7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_23])])).
fof(f384, plain, leq(s_best7, n3), inference(cnf_transformation, [], [f251])).
fof(f688, plain, spl39_24, inference(avatar_split_clause, [], [f385, f628])).
fof(f628, plain, (spl39_24 <=> leq(s_sworst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_24])])).
fof(f385, plain, leq(s_sworst7, n3), inference(cnf_transformation, [], [f251])).
fof(f687, plain, spl39_25, inference(avatar_split_clause, [], [f386, f632])).
fof(f632, plain, (spl39_25 <=> leq(s_worst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_25])])).
fof(f386, plain, leq(s_worst7, n3), inference(cnf_transformation, [], [f251])).
fof(f686, plain, spl39_26, inference(avatar_split_clause, [], [f388, f636])).
fof(f636, plain, (spl39_26 <=> leq(pv19, minus(n410, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_26])])).
fof(f388, plain, leq(pv19, minus(n410, n1)), inference(cnf_transformation, [], [f251])).
fof(f685, plain, spl39_27, inference(avatar_split_clause, [], [f389, f640])).
fof(f640, plain, (spl39_27 <=> leq(pv20, minus(n330, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_27])])).
fof(f389, plain, leq(pv20, minus(n330, n1)), inference(cnf_transformation, [], [f251])).
fof(f684, plain, spl39_28, inference(avatar_split_clause, [], [f390, f644])).
fof(f644, plain, (spl39_28 <=> leq(pv1376, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_28])])).
fof(f390, plain, leq(pv1376, n3), inference(cnf_transformation, [], [f251])).
fof(f683, plain, (~ spl39_35 | spl39_30), inference(avatar_split_clause, [], [f483, f652, f675])).
fof(f675, plain, (spl39_35 <=> gt(loopcounter, n1)), introduced(avatar_definition, [new_symbols(naming, [spl39_35])])).
fof(f652, plain, (spl39_30 <=> (s_worst7_init = pvar1400_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_30])])).
fof(f483, plain, ((s_worst7_init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f395, f376])).
fof(f395, plain, ((init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f682, plain, (~ spl39_35 | spl39_31), inference(avatar_split_clause, [], [f482, f656, f675])).
fof(f656, plain, (spl39_31 <=> (s_worst7_init = pvar1401_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_31])])).
fof(f482, plain, ((s_worst7_init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f396, f376])).
fof(f396, plain, ((init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f681, plain, (~ spl39_35 | spl39_32), inference(avatar_split_clause, [], [f481, f660, f675])).
fof(f660, plain, (spl39_32 <=> (s_worst7_init = pvar1402_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_32])])).
fof(f481, plain, ((s_worst7_init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f397, f376])).
fof(f397, plain, ((init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f680, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | spl39_34 | spl39_35), inference(avatar_split_clause, [], [f500, f675, f670, f525, f544, f563, f644, f640, f636, f632, f628, f624, f620, f616, f612, f608, f604, f600, f596, f592])).
fof(f563, plain, (spl39_9 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl39_9])])).
fof(f544, plain, (spl39_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl39_5])])).
fof(f525, plain, (spl39_1 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl39_1])])).
fof(f500, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f499])).
fof(f499, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f480])).
fof(f480, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f398, f376, f376, f376, f376, f376])).
fof(f398, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f679, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | spl39_33 | spl39_35), inference(avatar_split_clause, [], [f502, f675, f665, f525, f544, f563, f644, f640, f636, f632, f628, f624, f620, f616, f612, f608, f604, f600, f596, f592])).
fof(f502, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f501])).
fof(f501, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f479])).
fof(f479, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f399, f376, f376, f376, f376, f376])).
fof(f399, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f678, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_29 | spl39_35), inference(avatar_split_clause, [], [f504, f675, f648, f525, f544, f563, f644, f640, f636, f632, f628, f624, f620, f616, f612, f608, f604, f600, f596, f592])).
fof(f504, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f503])).
fof(f503, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f478])).
fof(f478, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f400, f376, f376, f376, f376, f376, f376])).
fof(f400, plain, (gt(loopcounter, n1) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f673, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | spl39_34 | ~ spl39_30 | ~ spl39_31 | ~ spl39_32), inference(avatar_split_clause, [], [f506, f660, f656, f652, f670, f525, f544, f563, f644, f640, f636, f632, f628, f624, f620, f616, f612, f608, f604, f600, f596, f592])).
fof(f506, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f505])).
fof(f505, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f477])).
fof(f477, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f401, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f401, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f668, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | spl39_33 | ~ spl39_30 | ~ spl39_31 | ~ spl39_32), inference(avatar_split_clause, [], [f508, f660, f656, f652, f665, f525, f544, f563, f644, f640, f636, f632, f628, f624, f620, f616, f612, f608, f604, f600, f596, f592])).
fof(f508, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f507])).
fof(f507, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f476])).
fof(f476, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f402, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f402, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f663, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_29 | ~ spl39_30 | ~ spl39_31 | ~ spl39_32), inference(avatar_split_clause, [], [f510, f660, f656, f652, f648, f525, f544, f563, f644, f640, f636, f632, f628, f624, f620, f616, f612, f608, f604, f600, f596, f592])).
fof(f510, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f509])).
fof(f509, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f475])).
fof(f475, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f403, f376, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f403, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f590, plain, (~ spl39_9 | spl39_14), inference(avatar_split_clause, [], [f368, f587, f563])).
fof(f368, plain, (leq(n0, sK36) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f248, plain, (((~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37)) & leq(sK36, n2) & leq(n0, sK36)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK36, sK37])], [f245, f247, f246])).
fof(f246, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) & leq(sK36, n2) & leq(n0, sK36))), introduced(choice_axiom, [])).
fof(f247, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37))), introduced(choice_axiom, [])).
fof(f245, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f244])).
fof(f244, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(nnf_transformation, [], [f185])).
fof(f585, plain, (~ spl39_9 | spl39_13), inference(avatar_split_clause, [], [f369, f582, f563])).
fof(f369, plain, (leq(sK36, n2) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f580, plain, (~ spl39_9 | spl39_12), inference(avatar_split_clause, [], [f370, f577, f563])).
fof(f370, plain, (leq(n0, sK37) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f575, plain, (~ spl39_9 | spl39_11), inference(avatar_split_clause, [], [f371, f572, f563])).
fof(f371, plain, (leq(sK37, n3) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f570, plain, (~ spl39_9 | ~ spl39_10), inference(avatar_split_clause, [], [f474, f567, f563])).
fof(f474, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(definition_unfolding, [], [f372, f376])).
fof(f372, plain, (~ (init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(cnf_transformation, [], [f248])).
fof(f561, plain, (~ spl39_5 | spl39_8), inference(avatar_split_clause, [], [f365, f558, f544])).
fof(f365, plain, (leq(n0, sK35) | ~ sP5), inference(cnf_transformation, [], [f243])).
fof(f243, plain, ((~ (init = a_select2(s_values7_init, sK35)) & leq(sK35, n3) & leq(n0, sK35)) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK35])], [f241, f242])).
fof(f242, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) => (~ (init = a_select2(s_values7_init, sK35)) & leq(sK35, n3) & leq(n0, sK35))), introduced(choice_axiom, [])).
fof(f241, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) | ~ sP5), inference(rectify, [], [f240])).
fof(f240, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(nnf_transformation, [], [f186])).
fof(f556, plain, (~ spl39_5 | spl39_7), inference(avatar_split_clause, [], [f366, f553, f544])).
fof(f366, plain, (leq(sK35, n3) | ~ sP5), inference(cnf_transformation, [], [f243])).
fof(f551, plain, (~ spl39_5 | ~ spl39_6), inference(avatar_split_clause, [], [f473, f548, f544])).
fof(f473, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | ~ sP5), inference(definition_unfolding, [], [f367, f376])).
fof(f367, plain, (~ (init = a_select2(s_values7_init, sK35)) | ~ sP5), inference(cnf_transformation, [], [f243])).
fof(f542, plain, (~ spl39_1 | spl39_4), inference(avatar_split_clause, [], [f362, f539, f525])).
fof(f362, plain, (leq(n0, sK34) | ~ sP6), inference(cnf_transformation, [], [f239])).
fof(f239, plain, ((~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34)) | ~ sP6), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34])], [f237, f238])).
fof(f238, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) => (~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f237, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) | ~ sP6), inference(rectify, [], [f236])).
fof(f236, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(nnf_transformation, [], [f187])).
fof(f537, plain, (~ spl39_1 | spl39_3), inference(avatar_split_clause, [], [f363, f534, f525])).
fof(f363, plain, (leq(sK34, n2) | ~ sP6), inference(cnf_transformation, [], [f239])).
fof(f532, plain, (~ spl39_1 | ~ spl39_2), inference(avatar_split_clause, [], [f472, f529, f525])).
fof(f472, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(definition_unfolding, [], [f364, f376])).
fof(f364, plain, (~ (init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(cnf_transformation, [], [f239])).