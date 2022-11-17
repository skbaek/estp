fof(f622847, plain, $false, inference(avatar_sat_refutation, [], [f530, f535, f540, f549, f554, f559, f568, f573, f578, f583, f588, f661, f666, f671, f676, f677, f678, f679, f680, f681, f682, f683, f684, f685, f686, f687, f688, f689, f690, f691, f692, f693, f694, f695, f1904, f1977, f2289, f621959, f622018, f622163, f622310, f622348, f622369, f622406, f622434, f622441, f622464, f622725, f622808, f622826, f622843])).
fof(f622843, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_172), inference(avatar_contradiction_clause, [], [f622842])).
fof(f622842, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_172)), inference(subsumption_resolution, [], [f622840, f621943])).
fof(f621943, plain, ((s_best7_init = a_select3(simplex7_init, n1, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f621874, f725])).
fof(f725, plain, leq(n0, n2), inference(resolution, [], [f257, f421])).
fof(f421, plain, gt(n2, n0), inference(cnf_transformation, [], [f74])).
fof(f74, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', gt_2_0)).
fof(f257, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', leq_gt1)).
fof(f621874, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n1, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2398, f255])).
fof(f255, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', reflexivity_leq)).
fof(f2398, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (s_best7_init = a_select3(simplex7_init, n1, X3))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2372, f740])).
fof(f740, plain, leq(n1, n3), inference(resolution, [], [f257, f428])).
fof(f428, plain, gt(n3, n1), inference(cnf_transformation, [], [f81])).
fof(f81, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', gt_3_1)).
fof(f2372, plain, (! [X3] : (~ leq(n1, n3) | (s_best7_init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f717, f724])).
fof(f724, plain, leq(n0, n1), inference(resolution, [], [f257, f420])).
fof(f420, plain, gt(n1, n0), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', gt_1_0)).
fof(f717, plain, (! [X4, X5] : (~ leq(n0, X5) | ~ leq(X5, n3) | (s_best7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f709, f713])).
fof(f713, plain, ((s_best7_init = s_sworst7_init) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f595, f591])).
fof(f591, plain, ((s_best7_init = s_worst7_init) | ~ spl39_15), inference(avatar_component_clause, [], [f590])).
fof(f590, plain, (spl39_15 <=> (s_best7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_15])])).
fof(f595, plain, ((s_sworst7_init = s_worst7_init) | ~ spl39_16), inference(avatar_component_clause, [], [f594])).
fof(f594, plain, (spl39_16 <=> (s_sworst7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_16])])).
fof(f709, plain, (! [X4, X5] : ((s_sworst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)) | ~ spl39_16), inference(backward_demodulation, [], [f485, f595])).
fof(f485, plain, ! [X4, X5] : ((s_worst7_init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(definition_unfolding, [], [f389, f376])).
fof(f376, plain, (init = s_worst7_init), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK38])], [f249, f250])).
fof(f250, plain, (? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) => (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n3, n1)) & leq(n0, sK38))), introduced(choice_axiom, [])).
fof(f249, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n3, n1)) & leq(n0, X0)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) & ! [X4] : (! [X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5)) | ~ leq(X4, n2) | ~ leq(n0, X4)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(rectify, [], [f188])).
fof(f188, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(definition_folding, [], [f165, e187, e186, e185])).
fof(f185, plain, (? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ sP4), inference(usedef, [], [e185])).
fof(e185, plain, (sP4 <=> ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f186, plain, (? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ~ sP5), inference(usedef, [], [e186])).
fof(e186, plain, (sP5 <=> ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f187, plain, (? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ~ sP6), inference(usedef, [], [e187])).
fof(e187, plain, (sP6 <=> ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f165, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & leq(X5, minus(n3, n1)) & leq(n0, X5)) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & leq(X6, n2) & leq(n0, X6)) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & leq(X7, n3) & leq(n0, X7)) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & leq(X9, n3) & leq(n0, X9)) & leq(X8, n2) & leq(n0, X8)) | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | ~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)), inference(flattening, [], [f164])).
fof(f164, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X5] : (~ (init = a_select2(s_try7_init, X5)) & (leq(X5, minus(n3, n1)) & leq(n0, X5))) | ? [X6] : (~ (init = a_select2(s_center7_init, X6)) & (leq(X6, n2) & leq(n0, X6))) | ? [X7] : (~ (init = a_select2(s_values7_init, X7)) & (leq(X7, n3) & leq(n0, X7))) | ? [X8] : (? [X9] : (~ (init = a_select3(simplex7_init, X9, X8)) & (leq(X9, n3) & leq(n0, X9))) & (leq(X8, n2) & leq(n0, X8))) | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)) & ((((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_try7_init, X0)) | (~ leq(X0, minus(n3, n1)) | ~ leq(n0, X0))) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | (~ leq(X1, n2) | ~ leq(n0, X1))) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | (~ leq(X2, n3) | ~ leq(n0, X2))) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | (~ leq(X4, n3) | ~ leq(n0, X4))) | (~ leq(X3, n2) | ~ leq(n0, X3))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(ennf_transformation, [], [f122])).
fof(f122, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X0] : ((leq(X0, minus(n3, n1)) & leq(n0, X0)) => (init = a_select2(s_try7_init, X0))) & ! [X1] : ((leq(X1, n2) & leq(n0, X1)) => (init = a_select2(s_center7_init, X1))) & ! [X2] : ((leq(X2, n3) & leq(n0, X2)) => (init = a_select2(s_values7_init, X2))) & ! [X3] : ((leq(X3, n2) & leq(n0, X3)) => ! [X4] : ((leq(X4, n3) & leq(n0, X4)) => (init = a_select3(simplex7_init, X4, X3)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X5] : ((leq(X5, minus(n3, n1)) & leq(n0, X5)) => (init = a_select2(s_try7_init, X5))) & ! [X6] : ((leq(X6, n2) & leq(n0, X6)) => (init = a_select2(s_center7_init, X6))) & ! [X7] : ((leq(X7, n3) & leq(n0, X7)) => (init = a_select2(s_values7_init, X7))) & ! [X8] : ((leq(X8, n2) & leq(n0, X8)) => ! [X9] : ((leq(X9, n3) & leq(n0, X9)) => (init = a_select3(simplex7_init, X9, X8)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X20] : ((leq(X20, minus(n3, n1)) & leq(n0, X20)) => (init = a_select2(s_try7_init, X20))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X10] : ((leq(X10, minus(n3, n1)) & leq(n0, X10)) => (init = a_select2(s_try7_init, X10))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X28] : ((leq(X28, n3) & leq(n0, X28)) => (init = a_select2(s_values7_init, X28))) & ! [X21] : ((leq(X21, n2) & leq(n0, X21)) => ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select3(simplex7_init, X27, X21)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (init = s_best7_init) & (init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', gauss_init_0021)).
fof(f389, plain, ! [X4, X5] : ((init = a_select3(simplex7_init, X5, X4)) | ~ leq(X5, n3) | ~ leq(n0, X5) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(cnf_transformation, [], [f251])).
fof(f622840, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_77 | ~ spl39_172)), inference(backward_demodulation, [], [f622773, f2170])).
fof(f2170, plain, ((n1 = sK37) | ~ spl39_172), inference(avatar_component_clause, [], [f2168])).
fof(f2168, plain, (spl39_172 <=> (n1 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_172])])).
fof(f622773, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_77)), inference(backward_demodulation, [], [f1170, f1254])).
fof(f1254, plain, ((n0 = sK36) | ~ spl39_77), inference(avatar_component_clause, [], [f1252])).
fof(f1252, plain, (spl39_77 <=> (n0 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_77])])).
fof(f1170, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, sK36)) | (spl39_10 | ~ spl39_15)), inference(forward_demodulation, [], [f567, f591])).
fof(f567, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | spl39_10), inference(avatar_component_clause, [], [f565])).
fof(f565, plain, (spl39_10 <=> (s_worst7_init = a_select3(simplex7_init, sK37, sK36))), introduced(avatar_definition, [new_symbols(naming, [spl39_10])])).
fof(f622826, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_67 | ~ spl39_77), inference(avatar_contradiction_clause, [], [f622825])).
fof(f622825, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_67 | ~ spl39_77)), inference(subsumption_resolution, [], [f622823, f622294])).
fof(f622294, plain, ((s_best7_init = a_select3(simplex7_init, n3, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f622225, f725])).
fof(f622225, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n3, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2400, f255])).
fof(f2400, plain, (! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (s_best7_init = a_select3(simplex7_init, n3, X5))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2374, f255])).
fof(f2374, plain, (! [X5] : (~ leq(n3, n3) | (s_best7_init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f717, f726])).
fof(f726, plain, leq(n0, n3), inference(resolution, [], [f257, f422])).
fof(f422, plain, gt(n3, n0), inference(cnf_transformation, [], [f75])).
fof(f75, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', gt_3_0)).
fof(f622823, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_67 | ~ spl39_77)), inference(backward_demodulation, [], [f622773, f1180])).
fof(f1180, plain, ((n3 = sK37) | ~ spl39_67), inference(avatar_component_clause, [], [f1178])).
fof(f1178, plain, (spl39_67 <=> (n3 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_67])])).
fof(f622808, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_77), inference(avatar_contradiction_clause, [], [f622807])).
fof(f622807, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_77)), inference(subsumption_resolution, [], [f622803, f18941])).
fof(f18941, plain, ((s_best7_init = a_select3(simplex7_init, n0, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f18919, f725])).
fof(f18919, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n0, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2406, f255])).
fof(f2406, plain, (! [X42] : (~ leq(n0, X42) | ~ leq(X42, n2) | (s_best7_init = a_select3(simplex7_init, n0, X42))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2395, f726])).
fof(f2395, plain, (! [X42] : (~ leq(n0, n3) | (s_best7_init = a_select3(simplex7_init, n0, X42)) | ~ leq(X42, n2) | ~ leq(n0, X42)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f717, f255])).
fof(f622803, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_73 | ~ spl39_77)), inference(backward_demodulation, [], [f622773, f1226])).
fof(f1226, plain, ((n0 = sK37) | ~ spl39_73), inference(avatar_component_clause, [], [f1224])).
fof(f1224, plain, (spl39_73 <=> (n0 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_73])])).
fof(f622725, plain, (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16), inference(avatar_contradiction_clause, [], [f622724])).
fof(f622724, plain, ($false | (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f622723, f622475])).
fof(f622475, plain, (~ (s_best7_init = a_select2(s_center7_init, sK34)) | (spl39_2 | ~ spl39_15)), inference(forward_demodulation, [], [f529, f591])).
fof(f529, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | spl39_2), inference(avatar_component_clause, [], [f527])).
fof(f527, plain, (spl39_2 <=> (s_worst7_init = a_select2(s_center7_init, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl39_2])])).
fof(f622723, plain, ((s_best7_init = a_select2(s_center7_init, sK34)) | (~ spl39_3 | ~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f622615, f534])).
fof(f534, plain, (leq(sK34, n2) | ~ spl39_3), inference(avatar_component_clause, [], [f532])).
fof(f532, plain, (spl39_3 <=> leq(sK34, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_3])])).
fof(f622615, plain, (~ leq(sK34, n2) | (s_best7_init = a_select2(s_center7_init, sK34)) | (~ spl39_4 | ~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f539, f715])).
fof(f715, plain, (! [X2] : (~ leq(n0, X2) | ~ leq(X2, n2) | (s_best7_init = a_select2(s_center7_init, X2))) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f711, f713])).
fof(f711, plain, (! [X2] : ((s_sworst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)) | ~ spl39_16), inference(backward_demodulation, [], [f483, f595])).
fof(f483, plain, ! [X2] : ((s_worst7_init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(definition_unfolding, [], [f391, f376])).
fof(f391, plain, ! [X2] : ((init = a_select2(s_center7_init, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f251])).
fof(f539, plain, (leq(n0, sK34) | ~ spl39_4), inference(avatar_component_clause, [], [f537])).
fof(f537, plain, (spl39_4 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl39_4])])).
fof(f622464, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_75), inference(avatar_contradiction_clause, [], [f622463])).
fof(f622463, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_75)), inference(subsumption_resolution, [], [f622459, f18940])).
fof(f18940, plain, ((s_best7_init = a_select3(simplex7_init, n0, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f18904, f255])).
fof(f18904, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n0, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2406, f725])).
fof(f622459, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_73 | ~ spl39_75)), inference(backward_demodulation, [], [f622375, f1226])).
fof(f622375, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_75)), inference(backward_demodulation, [], [f1170, f1243])).
fof(f1243, plain, ((n2 = sK36) | ~ spl39_75), inference(avatar_component_clause, [], [f1241])).
fof(f1241, plain, (spl39_75 <=> (n2 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_75])])).
fof(f622441, plain, (spl39_67 | ~ spl39_11 | spl39_73 | spl39_172 | ~ spl39_12 | spl39_180), inference(avatar_split_clause, [], [f622408, f2461, f575, f2168, f1224, f570, f1178])).
fof(f570, plain, (spl39_11 <=> leq(sK37, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_11])])).
fof(f575, plain, (spl39_12 <=> leq(n0, sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_12])])).
fof(f2461, plain, (spl39_180 <=> (n2 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_180])])).
fof(f622408, plain, ((n1 = sK37) | (n0 = sK37) | ~ leq(sK37, n3) | (n3 = sK37) | (~ spl39_12 | spl39_180)), inference(subsumption_resolution, [], [f2429, f2462])).
fof(f2462, plain, (~ (n2 = sK37) | spl39_180), inference(avatar_component_clause, [], [f2461])).
fof(f2429, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | ~ leq(sK37, n3) | (n3 = sK37) | ~ spl39_12), inference(resolution, [], [f443, f577])).
fof(f577, plain, (leq(n0, sK37) | ~ spl39_12), inference(avatar_component_clause, [], [f575])).
fof(f443, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f176])).
fof(f176, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f96])).
fof(f96, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', finite_domain_3)).
fof(f622434, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_75 | ~ spl39_172), inference(avatar_contradiction_clause, [], [f622433])).
fof(f622433, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_75 | ~ spl39_172)), inference(subsumption_resolution, [], [f622431, f621942])).
fof(f621942, plain, ((s_best7_init = a_select3(simplex7_init, n1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f621851, f255])).
fof(f621851, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2398, f725])).
fof(f622431, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_75 | ~ spl39_172)), inference(backward_demodulation, [], [f622375, f2170])).
fof(f622406, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_67 | ~ spl39_75), inference(avatar_contradiction_clause, [], [f622405])).
fof(f622405, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_67 | ~ spl39_75)), inference(subsumption_resolution, [], [f622403, f622293])).
fof(f622293, plain, ((s_best7_init = a_select3(simplex7_init, n3, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f622202, f255])).
fof(f622202, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n3, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2400, f725])).
fof(f622403, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_67 | ~ spl39_75)), inference(backward_demodulation, [], [f622375, f1180])).
fof(f622369, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_180), inference(avatar_contradiction_clause, [], [f622368])).
fof(f622368, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_77 | ~ spl39_180)), inference(subsumption_resolution, [], [f622366, f622147])).
fof(f622147, plain, ((s_best7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f622078, f725])).
fof(f622078, plain, (~ leq(n0, n2) | (s_best7_init = a_select3(simplex7_init, n2, n0)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2399, f255])).
fof(f2399, plain, (! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (s_best7_init = a_select3(simplex7_init, n2, X4))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f2373, f745])).
fof(f745, plain, leq(n2, n3), inference(resolution, [], [f257, f433])).
fof(f433, plain, gt(n3, n2), inference(cnf_transformation, [], [f86])).
fof(f86, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', gt_3_2)).
fof(f2373, plain, (! [X4] : (~ leq(n2, n3) | (s_best7_init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f717, f725])).
fof(f622366, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_77 | ~ spl39_180)), inference(backward_demodulation, [], [f622316, f1254])).
fof(f622316, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_180)), inference(backward_demodulation, [], [f1170, f2463])).
fof(f2463, plain, ((n2 = sK37) | ~ spl39_180), inference(avatar_component_clause, [], [f2461])).
fof(f622348, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_75 | ~ spl39_180), inference(avatar_contradiction_clause, [], [f622347])).
fof(f622347, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_75 | ~ spl39_180)), inference(subsumption_resolution, [], [f622345, f622146])).
fof(f622146, plain, ((s_best7_init = a_select3(simplex7_init, n2, n2)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f622055, f255])).
fof(f622055, plain, (~ leq(n2, n2) | (s_best7_init = a_select3(simplex7_init, n2, n2)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2399, f725])).
fof(f622345, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_75 | ~ spl39_180)), inference(backward_demodulation, [], [f622316, f1243])).
fof(f622310, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_67 | ~ spl39_170), inference(avatar_contradiction_clause, [], [f622309])).
fof(f622309, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_67 | ~ spl39_170)), inference(subsumption_resolution, [], [f622308, f622180])).
fof(f622180, plain, (~ (s_best7_init = a_select3(simplex7_init, n3, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_67 | ~ spl39_170)), inference(backward_demodulation, [], [f2353, f1180])).
fof(f2353, plain, (~ (s_best7_init = a_select3(simplex7_init, sK37, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_170)), inference(backward_demodulation, [], [f1170, f2160])).
fof(f2160, plain, ((n1 = sK36) | ~ spl39_170), inference(avatar_component_clause, [], [f2158])).
fof(f2158, plain, (spl39_170 <=> (n1 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_170])])).
fof(f622308, plain, ((s_best7_init = a_select3(simplex7_init, n3, n1)) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f622307, f491])).
fof(f491, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f446, f332])).
fof(f332, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', succ_plus_1_r)).
fof(f446, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', successor_1)).
fof(f622307, plain, ((s_best7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f622306, f739])).
fof(f739, plain, leq(n1, n2), inference(resolution, [], [f257, f427])).
fof(f427, plain, gt(n2, n1), inference(cnf_transformation, [], [f80])).
fof(f80, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', gt_2_1)).
fof(f622306, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f622254, f491])).
fof(f622254, plain, (~ leq(plus(n0, n1), n2) | (s_best7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2400, f723])).
fof(f723, plain, ! [X0] : leq(X0, plus(X0, n1)), inference(resolution, [], [f257, f451])).
fof(f451, plain, ! [X0] : gt(plus(X0, n1), X0), inference(definition_unfolding, [], [f261, f332])).
fof(f261, plain, ! [X0] : gt(succ(X0), X0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : gt(succ(X0), X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', gt_succ)).
fof(f622163, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_170 | ~ spl39_180), inference(avatar_contradiction_clause, [], [f622162])).
fof(f622162, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_170 | ~ spl39_180)), inference(subsumption_resolution, [], [f622161, f622035])).
fof(f622035, plain, (~ (s_best7_init = a_select3(simplex7_init, n2, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_170 | ~ spl39_180)), inference(backward_demodulation, [], [f2353, f2463])).
fof(f622161, plain, ((s_best7_init = a_select3(simplex7_init, n2, n1)) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f622160, f491])).
fof(f622160, plain, ((s_best7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f622159, f739])).
fof(f622159, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f622107, f491])).
fof(f622107, plain, (~ leq(plus(n0, n1), n2) | (s_best7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2399, f723])).
fof(f622018, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_170), inference(avatar_contradiction_clause, [], [f622017])).
fof(f622017, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_73 | ~ spl39_170)), inference(subsumption_resolution, [], [f622010, f18939])).
fof(f18939, plain, ((s_best7_init = a_select3(simplex7_init, n0, n1)) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f18898, f739])).
fof(f18898, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n0, n1)) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2406, f724])).
fof(f622010, plain, (~ (s_best7_init = a_select3(simplex7_init, n0, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_73 | ~ spl39_170)), inference(backward_demodulation, [], [f2353, f1226])).
fof(f621959, plain, (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_170 | ~ spl39_172), inference(avatar_contradiction_clause, [], [f621958])).
fof(f621958, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_16 | ~ spl39_170 | ~ spl39_172)), inference(subsumption_resolution, [], [f621957, f2590])).
fof(f2590, plain, (~ (s_best7_init = a_select3(simplex7_init, n1, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_170 | ~ spl39_172)), inference(backward_demodulation, [], [f2353, f2170])).
fof(f621957, plain, ((s_best7_init = a_select3(simplex7_init, n1, n1)) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f621956, f491])).
fof(f621956, plain, ((s_best7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f621955, f739])).
fof(f621955, plain, (~ leq(n1, n2) | (s_best7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(forward_demodulation, [], [f621903, f491])).
fof(f621903, plain, (~ leq(plus(n0, n1), n2) | (s_best7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | (~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f2398, f723])).
fof(f2289, plain, (spl39_75 | spl39_77 | spl39_170 | ~ spl39_13 | ~ spl39_14), inference(avatar_split_clause, [], [f2288, f585, f580, f2158, f1252, f1241])).
fof(f580, plain, (spl39_13 <=> leq(sK36, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_13])])).
fof(f585, plain, (spl39_14 <=> leq(n0, sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_14])])).
fof(f2288, plain, ((n1 = sK36) | (n0 = sK36) | (n2 = sK36) | (~ spl39_13 | ~ spl39_14)), inference(subsumption_resolution, [], [f2282, f582])).
fof(f582, plain, (leq(sK36, n2) | ~ spl39_13), inference(avatar_component_clause, [], [f580])).
fof(f2282, plain, ((n1 = sK36) | (n0 = sK36) | ~ leq(sK36, n2) | (n2 = sK36) | ~ spl39_14), inference(resolution, [], [f442, f587])).
fof(f587, plain, (leq(n0, sK36) | ~ spl39_14), inference(avatar_component_clause, [], [f585])).
fof(f442, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f174])).
fof(f174, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f95])).
fof(f95, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', finite_domain_2)).
fof(f1977, plain, (~ spl39_15 | ~ spl39_16 | spl39_29 | ~ spl39_33 | ~ spl39_34), inference(avatar_contradiction_clause, [], [f1976])).
fof(f1976, plain, ($false | (~ spl39_15 | ~ spl39_16 | spl39_29 | ~ spl39_33 | ~ spl39_34)), inference(subsumption_resolution, [], [f1975, f1919])).
fof(f1919, plain, (leq(sK38, n2) | ~ spl39_33), inference(forward_demodulation, [], [f665, f776])).
fof(f776, plain, (n2 = minus(n3, n1)), inference(superposition, [], [f465, f708])).
fof(f708, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f707, f703])).
fof(f703, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f492, f491])).
fof(f492, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f447, f332, f332])).
fof(f447, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f100])).
fof(f100, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', successor_2)).
fof(f707, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f706, f491])).
fof(f706, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f493, f456])).
fof(f456, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f333, f332])).
fof(f333, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', succ_plus_1_l)).
fof(f493, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f448, f332, f332, f332])).
fof(f448, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f101])).
fof(f101, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', successor_3)).
fof(f465, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f343, f342, f332])).
fof(f342, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', pred_minus_1)).
fof(f343, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV027+1.p', pred_succ)).
fof(f665, plain, (leq(sK38, minus(n3, n1)) | ~ spl39_33), inference(avatar_component_clause, [], [f663])).
fof(f663, plain, (spl39_33 <=> leq(sK38, minus(n3, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_33])])).
fof(f1975, plain, (~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_16 | spl39_29 | ~ spl39_34)), inference(subsumption_resolution, [], [f1969, f1183])).
fof(f1183, plain, (~ (s_best7_init = a_select2(s_try7_init, sK38)) | (~ spl39_15 | spl39_29)), inference(forward_demodulation, [], [f648, f591])).
fof(f648, plain, (~ (s_worst7_init = a_select2(s_try7_init, sK38)) | spl39_29), inference(avatar_component_clause, [], [f646])).
fof(f646, plain, (spl39_29 <=> (s_worst7_init = a_select2(s_try7_init, sK38))), introduced(avatar_definition, [new_symbols(naming, [spl39_29])])).
fof(f1969, plain, ((s_best7_init = a_select2(s_try7_init, sK38)) | ~ leq(sK38, n2) | (~ spl39_15 | ~ spl39_16 | ~ spl39_34)), inference(resolution, [], [f779, f670])).
fof(f670, plain, (leq(n0, sK38) | ~ spl39_34), inference(avatar_component_clause, [], [f668])).
fof(f668, plain, (spl39_34 <=> leq(n0, sK38)), introduced(avatar_definition, [new_symbols(naming, [spl39_34])])).
fof(f779, plain, (! [X1] : (~ leq(n0, X1) | (s_best7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, n2)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f714, f776])).
fof(f714, plain, (! [X1] : ((s_best7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f712, f713])).
fof(f712, plain, (! [X1] : ((s_sworst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)) | ~ spl39_16), inference(backward_demodulation, [], [f482, f595])).
fof(f482, plain, ! [X1] : ((s_worst7_init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(definition_unfolding, [], [f392, f376])).
fof(f392, plain, ! [X1] : ((init = a_select2(s_try7_init, X1)) | ~ leq(X1, minus(n3, n1)) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f251])).
fof(f1904, plain, (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16), inference(avatar_contradiction_clause, [], [f1903])).
fof(f1903, plain, ($false | (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1902, f718])).
fof(f718, plain, (~ (s_best7_init = a_select2(s_values7_init, sK35)) | (spl39_6 | ~ spl39_15)), inference(forward_demodulation, [], [f548, f591])).
fof(f548, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | spl39_6), inference(avatar_component_clause, [], [f546])).
fof(f546, plain, (spl39_6 <=> (s_worst7_init = a_select2(s_values7_init, sK35))), introduced(avatar_definition, [new_symbols(naming, [spl39_6])])).
fof(f1902, plain, ((s_best7_init = a_select2(s_values7_init, sK35)) | (~ spl39_7 | ~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(subsumption_resolution, [], [f1897, f553])).
fof(f553, plain, (leq(sK35, n3) | ~ spl39_7), inference(avatar_component_clause, [], [f551])).
fof(f551, plain, (spl39_7 <=> leq(sK35, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_7])])).
fof(f1897, plain, (~ leq(sK35, n3) | (s_best7_init = a_select2(s_values7_init, sK35)) | (~ spl39_8 | ~ spl39_15 | ~ spl39_16)), inference(resolution, [], [f558, f716])).
fof(f716, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n3) | (s_best7_init = a_select2(s_values7_init, X3))) | (~ spl39_15 | ~ spl39_16)), inference(backward_demodulation, [], [f710, f713])).
fof(f710, plain, (! [X3] : ((s_sworst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ spl39_16), inference(backward_demodulation, [], [f484, f595])).
fof(f484, plain, ! [X3] : ((s_worst7_init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(definition_unfolding, [], [f390, f376])).
fof(f390, plain, ! [X3] : ((init = a_select2(s_values7_init, X3)) | ~ leq(X3, n3) | ~ leq(n0, X3)), inference(cnf_transformation, [], [f251])).
fof(f558, plain, (leq(n0, sK35) | ~ spl39_8), inference(avatar_component_clause, [], [f556])).
fof(f556, plain, (spl39_8 <=> leq(n0, sK35)), introduced(avatar_definition, [new_symbols(naming, [spl39_8])])).
fof(f695, plain, spl39_15, inference(avatar_split_clause, [], [f487, f590])).
fof(f487, plain, (s_best7_init = s_worst7_init), inference(definition_unfolding, [], [f374, f376])).
fof(f374, plain, (init = s_best7_init), inference(cnf_transformation, [], [f251])).
fof(f694, plain, spl39_16, inference(avatar_split_clause, [], [f486, f594])).
fof(f486, plain, (s_sworst7_init = s_worst7_init), inference(definition_unfolding, [], [f375, f376])).
fof(f375, plain, (init = s_sworst7_init), inference(cnf_transformation, [], [f251])).
fof(f693, plain, spl39_17, inference(avatar_split_clause, [], [f377, f598])).
fof(f598, plain, (spl39_17 <=> leq(n0, s_best7)), introduced(avatar_definition, [new_symbols(naming, [spl39_17])])).
fof(f377, plain, leq(n0, s_best7), inference(cnf_transformation, [], [f251])).
fof(f692, plain, spl39_18, inference(avatar_split_clause, [], [f378, f602])).
fof(f602, plain, (spl39_18 <=> leq(n0, s_sworst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_18])])).
fof(f378, plain, leq(n0, s_sworst7), inference(cnf_transformation, [], [f251])).
fof(f691, plain, spl39_19, inference(avatar_split_clause, [], [f379, f606])).
fof(f606, plain, (spl39_19 <=> leq(n0, s_worst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_19])])).
fof(f379, plain, leq(n0, s_worst7), inference(cnf_transformation, [], [f251])).
fof(f690, plain, spl39_20, inference(avatar_split_clause, [], [f380, f610])).
fof(f610, plain, (spl39_20 <=> leq(n0, pv19)), introduced(avatar_definition, [new_symbols(naming, [spl39_20])])).
fof(f380, plain, leq(n0, pv19), inference(cnf_transformation, [], [f251])).
fof(f689, plain, spl39_21, inference(avatar_split_clause, [], [f381, f614])).
fof(f614, plain, (spl39_21 <=> leq(n0, pv20)), introduced(avatar_definition, [new_symbols(naming, [spl39_21])])).
fof(f381, plain, leq(n0, pv20), inference(cnf_transformation, [], [f251])).
fof(f688, plain, spl39_22, inference(avatar_split_clause, [], [f382, f618])).
fof(f618, plain, (spl39_22 <=> leq(n0, pv1376)), introduced(avatar_definition, [new_symbols(naming, [spl39_22])])).
fof(f382, plain, leq(n0, pv1376), inference(cnf_transformation, [], [f251])).
fof(f687, plain, spl39_23, inference(avatar_split_clause, [], [f383, f622])).
fof(f622, plain, (spl39_23 <=> leq(s_best7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_23])])).
fof(f383, plain, leq(s_best7, n3), inference(cnf_transformation, [], [f251])).
fof(f686, plain, spl39_24, inference(avatar_split_clause, [], [f384, f626])).
fof(f626, plain, (spl39_24 <=> leq(s_sworst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_24])])).
fof(f384, plain, leq(s_sworst7, n3), inference(cnf_transformation, [], [f251])).
fof(f685, plain, spl39_25, inference(avatar_split_clause, [], [f385, f630])).
fof(f630, plain, (spl39_25 <=> leq(s_worst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_25])])).
fof(f385, plain, leq(s_worst7, n3), inference(cnf_transformation, [], [f251])).
fof(f684, plain, spl39_26, inference(avatar_split_clause, [], [f386, f634])).
fof(f634, plain, (spl39_26 <=> leq(pv19, minus(n410, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_26])])).
fof(f386, plain, leq(pv19, minus(n410, n1)), inference(cnf_transformation, [], [f251])).
fof(f683, plain, spl39_27, inference(avatar_split_clause, [], [f387, f638])).
fof(f638, plain, (spl39_27 <=> leq(pv20, minus(n330, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_27])])).
fof(f387, plain, leq(pv20, minus(n330, n1)), inference(cnf_transformation, [], [f251])).
fof(f682, plain, spl39_28, inference(avatar_split_clause, [], [f388, f642])).
fof(f642, plain, (spl39_28 <=> leq(pv1376, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_28])])).
fof(f388, plain, leq(pv1376, n3), inference(cnf_transformation, [], [f251])).
fof(f681, plain, (~ spl39_35 | spl39_30), inference(avatar_split_clause, [], [f481, f650, f673])).
fof(f673, plain, (spl39_35 <=> gt(loopcounter, n1)), introduced(avatar_definition, [new_symbols(naming, [spl39_35])])).
fof(f650, plain, (spl39_30 <=> (s_worst7_init = pvar1400_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_30])])).
fof(f481, plain, ((s_worst7_init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f393, f376])).
fof(f393, plain, ((init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f680, plain, (~ spl39_35 | spl39_31), inference(avatar_split_clause, [], [f480, f654, f673])).
fof(f654, plain, (spl39_31 <=> (s_worst7_init = pvar1401_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_31])])).
fof(f480, plain, ((s_worst7_init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f394, f376])).
fof(f394, plain, ((init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f679, plain, (~ spl39_35 | spl39_32), inference(avatar_split_clause, [], [f479, f658, f673])).
fof(f658, plain, (spl39_32 <=> (s_worst7_init = pvar1402_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_32])])).
fof(f479, plain, ((s_worst7_init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f395, f376])).
fof(f395, plain, ((init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f251])).
fof(f678, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | spl39_34 | spl39_35), inference(avatar_split_clause, [], [f498, f673, f668, f523, f542, f561, f642, f638, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f561, plain, (spl39_9 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl39_9])])).
fof(f542, plain, (spl39_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl39_5])])).
fof(f523, plain, (spl39_1 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl39_1])])).
fof(f498, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f497])).
fof(f497, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f478])).
fof(f478, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f396, f376, f376, f376, f376, f376])).
fof(f396, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f677, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | spl39_33 | spl39_35), inference(avatar_split_clause, [], [f500, f673, f663, f523, f542, f561, f642, f638, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f500, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f499])).
fof(f499, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f477])).
fof(f477, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f397, f376, f376, f376, f376, f376])).
fof(f397, plain, (gt(loopcounter, n1) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f676, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_29 | spl39_35), inference(avatar_split_clause, [], [f502, f673, f646, f523, f542, f561, f642, f638, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f502, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f501])).
fof(f501, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f476])).
fof(f476, plain, (gt(loopcounter, n1) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f398, f376, f376, f376, f376, f376, f376])).
fof(f398, plain, (gt(loopcounter, n1) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f671, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | spl39_34 | ~ spl39_30 | ~ spl39_31 | ~ spl39_32), inference(avatar_split_clause, [], [f504, f658, f654, f650, f668, f523, f542, f561, f642, f638, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f504, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f503])).
fof(f503, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f475])).
fof(f475, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f399, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f399, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f666, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | spl39_33 | ~ spl39_30 | ~ spl39_31 | ~ spl39_32), inference(avatar_split_clause, [], [f506, f658, f654, f650, f663, f523, f542, f561, f642, f638, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f506, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f505])).
fof(f505, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f474])).
fof(f474, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f400, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f400, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(sK38, minus(n3, n1)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
fof(f661, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | ~ spl39_22 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25 | ~ spl39_26 | ~ spl39_27 | ~ spl39_28 | spl39_9 | spl39_5 | spl39_1 | ~ spl39_29 | ~ spl39_30 | ~ spl39_31 | ~ spl39_32), inference(avatar_split_clause, [], [f508, f658, f654, f650, f646, f523, f542, f561, f642, f638, f634, f630, f626, f622, f618, f614, f610, f606, f602, f598, f594, f590])).
fof(f508, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f507])).
fof(f507, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f473])).
fof(f473, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | ~ (s_worst7_init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_best7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f401, f376, f376, f376, f376, f376, f376, f376, f376, f376])).
fof(f401, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | ~ (init = a_select2(s_try7_init, sK38)) | sP6 | sP5 | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (init = s_best7_init) | ~ (init = init)), inference(cnf_transformation, [], [f251])).
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