fof(f272145, plain, $false, inference(avatar_sat_refutation, [], [f490, f495, f500, f509, f514, f519, f528, f533, f538, f543, f548, f598, f603, f609, f610, f611, f612, f613, f614, f615, f616, f617, f618, f619, f620, f1528, f3384, f3512, f20072, f256276, f256283, f256350, f256455, f256784, f267224, f271305, f271546, f271653, f271658, f271932, f272038, f272143])).
fof(f272143, plain, (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_56), inference(avatar_contradiction_clause, [], [f272142])).
fof(f272142, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_56)), inference(subsumption_resolution, [], [f272100, f271286])).
fof(f271286, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n2)) | ~ spl39_15), inference(subsumption_resolution, [], [f271201, f240])).
fof(f240, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', reflexivity_leq)).
fof(f271201, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, n2)) | ~ spl39_15), inference(resolution, [], [f1825, f647])).
fof(f647, plain, leq(n0, n2), inference(resolution, [], [f242, f389])).
fof(f389, plain, gt(n2, n0), inference(cnf_transformation, [], [f65])).
fof(f65, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', gt_2_0)).
fof(f242, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', leq_gt1)).
fof(f1825, plain, (! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, X5))) | ~ spl39_15), inference(subsumption_resolution, [], [f1804, f240])).
fof(f1804, plain, (! [X5] : (~ leq(n3, n3) | (s_sworst7_init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)) | ~ spl39_15), inference(resolution, [], [f634, f649])).
fof(f649, plain, leq(n0, n3), inference(resolution, [], [f242, f390])).
fof(f390, plain, gt(n3, n0), inference(cnf_transformation, [], [f66])).
fof(f66, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', gt_3_0)).
fof(f634, plain, (! [X4, X3] : (~ leq(n0, X4) | ~ leq(X4, n3) | (s_sworst7_init = a_select3(simplex7_init, X4, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | ~ spl39_15), inference(backward_demodulation, [], [f447, f551])).
fof(f551, plain, ((s_sworst7_init = s_worst7_init) | ~ spl39_15), inference(avatar_component_clause, [], [f550])).
fof(f550, plain, (spl39_15 <=> (s_sworst7_init = s_worst7_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_15])])).
fof(f447, plain, ! [X4, X3] : ((s_worst7_init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4) | ~ leq(X3, n2) | ~ leq(n0, X3)), inference(definition_unfolding, [], [f367, f360])).
fof(f360, plain, (init = s_worst7_init), inference(cnf_transformation, [], [f236])).
fof(f236, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n0, n1)) & leq(n0, sK38)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK38])], [f234, f235])).
fof(f235, plain, (? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n0, n1)) & leq(n0, X0)) => (~ (init = a_select2(s_try7_init, sK38)) & leq(sK38, minus(n0, n1)) & leq(n0, sK38))), introduced(choice_axiom, [])).
fof(f234, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X0] : (~ (init = a_select2(s_try7_init, X0)) & leq(X0, minus(n0, n1)) & leq(n0, X0)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(rectify, [], [f173])).
fof(f173, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X4] : (~ (init = a_select2(s_try7_init, X4)) & leq(X4, minus(n0, n1)) & leq(n0, X4)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_center7_init, X0)) | ~ leq(X0, n2) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | ~ leq(X1, n3) | ~ leq(n0, X1)) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(definition_folding, [], [f150, e172, e171, e170])).
fof(f170, plain, (? [X7] : (? [X8] : (~ (init = a_select3(simplex7_init, X8, X7)) & leq(X8, n3) & leq(n0, X8)) & leq(X7, n2) & leq(n0, X7)) | ~ sP4), inference(usedef, [], [e170])).
fof(e170, plain, (sP4 <=> ? [X7] : (? [X8] : (~ (init = a_select3(simplex7_init, X8, X7)) & leq(X8, n3) & leq(n0, X8)) & leq(X7, n2) & leq(n0, X7))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f171, plain, (? [X6] : (~ (init = a_select2(s_values7_init, X6)) & leq(X6, n3) & leq(n0, X6)) | ~ sP5), inference(usedef, [], [e171])).
fof(e171, plain, (sP5 <=> ? [X6] : (~ (init = a_select2(s_values7_init, X6)) & leq(X6, n3) & leq(n0, X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f172, plain, (? [X5] : (~ (init = a_select2(s_center7_init, X5)) & leq(X5, n2) & leq(n0, X5)) | ~ sP6), inference(usedef, [], [e172])).
fof(e172, plain, (sP6 <=> ? [X5] : (~ (init = a_select2(s_center7_init, X5)) & leq(X5, n2) & leq(n0, X5))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f150, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X4] : (~ (init = a_select2(s_try7_init, X4)) & leq(X4, minus(n0, n1)) & leq(n0, X4)) | ? [X5] : (~ (init = a_select2(s_center7_init, X5)) & leq(X5, n2) & leq(n0, X5)) | ? [X6] : (~ (init = a_select2(s_values7_init, X6)) & leq(X6, n3) & leq(n0, X6)) | ? [X7] : (? [X8] : (~ (init = a_select3(simplex7_init, X8, X7)) & leq(X8, n3) & leq(n0, X8)) & leq(X7, n2) & leq(n0, X7)) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_center7_init, X0)) | ~ leq(X0, n2) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | ~ leq(X1, n3) | ~ leq(n0, X1)) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)), inference(flattening, [], [f149])).
fof(f149, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X4] : (~ (init = a_select2(s_try7_init, X4)) & (leq(X4, minus(n0, n1)) & leq(n0, X4))) | ? [X5] : (~ (init = a_select2(s_center7_init, X5)) & (leq(X5, n2) & leq(n0, X5))) | ? [X6] : (~ (init = a_select2(s_values7_init, X6)) & (leq(X6, n3) & leq(n0, X6))) | ? [X7] : (? [X8] : (~ (init = a_select3(simplex7_init, X8, X7)) & (leq(X8, n3) & leq(n0, X8))) & (leq(X7, n2) & leq(n0, X7))) | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)) & ((((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_center7_init, X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | (~ leq(X1, n3) | ~ leq(n0, X1))) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | (~ leq(X3, n3) | ~ leq(n0, X3))) | (~ leq(X2, n2) | ~ leq(n0, X2))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init))), inference(ennf_transformation, [], [f107])).
fof(f107, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => (init = a_select2(s_center7_init, X0))) & ! [X1] : ((leq(X1, n3) & leq(n0, X1)) => (init = a_select2(s_values7_init, X1))) & ! [X2] : ((leq(X2, n2) & leq(n0, X2)) => ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select3(simplex7_init, X3, X2)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X4] : ((leq(X4, minus(n0, n1)) & leq(n0, X4)) => (init = a_select2(s_try7_init, X4))) & ! [X5] : ((leq(X5, n2) & leq(n0, X5)) => (init = a_select2(s_center7_init, X5))) & ! [X6] : ((leq(X6, n3) & leq(n0, X6)) => (init = a_select2(s_values7_init, X6))) & ! [X7] : ((leq(X7, n2) & leq(n0, X7)) => ! [X8] : ((leq(X8, n3) & leq(n0, X8)) => (init = a_select3(simplex7_init, X8, X7)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X4] : ((leq(X4, minus(n0, n1)) & leq(n0, X4)) => (init = a_select2(s_try7_init, X4))) & ! [X28] : ((leq(X28, n2) & leq(n0, X28)) => (init = a_select2(s_center7_init, X28))) & ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select2(s_values7_init, X27))) & ! [X20] : ((leq(X20, n2) & leq(n0, X20)) => ! [X21] : ((leq(X21, n3) & leq(n0, X21)) => (init = a_select3(simplex7_init, X21, X20)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init)) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X4] : ((leq(X4, minus(n0, n1)) & leq(n0, X4)) => (init = a_select2(s_try7_init, X4))) & ! [X28] : ((leq(X28, n2) & leq(n0, X28)) => (init = a_select2(s_center7_init, X28))) & ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select2(s_values7_init, X27))) & ! [X20] : ((leq(X20, n2) & leq(n0, X20)) => ! [X21] : ((leq(X21, n3) & leq(n0, X21)) => (init = a_select3(simplex7_init, X21, X20)))) & leq(s_worst7, n3) & leq(s_sworst7, n3) & leq(s_best7, n3) & leq(n0, s_worst7) & leq(n0, s_sworst7) & leq(n0, s_best7) & (init = s_worst7_init) & (init = s_sworst7_init) & (s_best7_init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', gauss_init_0069)).
fof(f367, plain, ! [X4, X3] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4) | ~ leq(X3, n2) | ~ leq(n0, X3)), inference(cnf_transformation, [], [f236])).
fof(f272100, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_56)), inference(backward_demodulation, [], [f272040, f1099])).
fof(f1099, plain, ((n2 = sK36) | ~ spl39_56), inference(avatar_component_clause, [], [f1097])).
fof(f1097, plain, (spl39_56 <=> (n2 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_56])])).
fof(f272040, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_48)), inference(forward_demodulation, [], [f1031, f1041])).
fof(f1041, plain, ((n3 = sK37) | ~ spl39_48), inference(avatar_component_clause, [], [f1039])).
fof(f1039, plain, (spl39_48 <=> (n3 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_48])])).
fof(f1031, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, sK36)) | (spl39_10 | ~ spl39_15)), inference(forward_demodulation, [], [f527, f551])).
fof(f527, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | spl39_10), inference(avatar_component_clause, [], [f525])).
fof(f525, plain, (spl39_10 <=> (s_worst7_init = a_select3(simplex7_init, sK37, sK36))), introduced(avatar_definition, [new_symbols(naming, [spl39_10])])).
fof(f272038, plain, (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_240), inference(avatar_contradiction_clause, [], [f272037])).
fof(f272037, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_240)), inference(subsumption_resolution, [], [f271995, f271287])).
fof(f271287, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n0)) | ~ spl39_15), inference(subsumption_resolution, [], [f271220, f647])).
fof(f271220, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, n0)) | ~ spl39_15), inference(resolution, [], [f1825, f240])).
fof(f271995, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_240)), inference(backward_demodulation, [], [f271661, f1041])).
fof(f271661, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_240)), inference(backward_demodulation, [], [f1031, f20857])).
fof(f20857, plain, ((n0 = sK36) | ~ spl39_240), inference(avatar_component_clause, [], [f20855])).
fof(f20855, plain, (spl39_240 <=> (n0 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_240])])).
fof(f271932, plain, (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_240), inference(avatar_contradiction_clause, [], [f271931])).
fof(f271931, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_240)), inference(subsumption_resolution, [], [f271858, f20053])).
fof(f20053, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n0)) | ~ spl39_15), inference(subsumption_resolution, [], [f20029, f647])).
fof(f20029, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, n0)) | ~ spl39_15), inference(resolution, [], [f1829, f240])).
fof(f1829, plain, (! [X36] : (~ leq(n0, X36) | ~ leq(X36, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, X36))) | ~ spl39_15), inference(subsumption_resolution, [], [f1819, f649])).
fof(f1819, plain, (! [X36] : (~ leq(n0, n3) | (s_sworst7_init = a_select3(simplex7_init, n0, X36)) | ~ leq(X36, n2) | ~ leq(n0, X36)) | ~ spl39_15), inference(resolution, [], [f634, f240])).
fof(f271858, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_240)), inference(backward_demodulation, [], [f271661, f1080])).
fof(f1080, plain, ((n0 = sK37) | ~ spl39_54), inference(avatar_component_clause, [], [f1078])).
fof(f1078, plain, (spl39_54 <=> (n0 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_54])])).
fof(f271658, plain, (spl39_48 | spl39_54 | spl39_230 | ~ spl39_11 | ~ spl39_12 | spl39_229), inference(avatar_split_clause, [], [f256797, f20146, f535, f530, f20150, f1078, f1039])).
fof(f20150, plain, (spl39_230 <=> (n2 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_230])])).
fof(f530, plain, (spl39_11 <=> leq(sK37, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_11])])).
fof(f535, plain, (spl39_12 <=> leq(n0, sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_12])])).
fof(f20146, plain, (spl39_229 <=> (n1 = sK37)), introduced(avatar_definition, [new_symbols(naming, [spl39_229])])).
fof(f256797, plain, ((n2 = sK37) | (n0 = sK37) | (n3 = sK37) | (~ spl39_11 | ~ spl39_12 | spl39_229)), inference(subsumption_resolution, [], [f256796, f532])).
fof(f532, plain, (leq(sK37, n3) | ~ spl39_11), inference(avatar_component_clause, [], [f530])).
fof(f256796, plain, ((n2 = sK37) | (n0 = sK37) | ~ leq(sK37, n3) | (n3 = sK37) | (~ spl39_12 | spl39_229)), inference(subsumption_resolution, [], [f20127, f20147])).
fof(f20147, plain, (~ (n1 = sK37) | spl39_229), inference(avatar_component_clause, [], [f20146])).
fof(f20127, plain, ((n2 = sK37) | (n1 = sK37) | (n0 = sK37) | ~ leq(sK37, n3) | (n3 = sK37) | ~ spl39_12), inference(resolution, [], [f537, f405])).
fof(f405, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f162])).
fof(f162, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f161])).
fof(f161, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', finite_domain_3)).
fof(f537, plain, (leq(n0, sK37) | ~ spl39_12), inference(avatar_component_clause, [], [f535])).
fof(f271653, plain, (spl39_10 | ~ spl39_15 | ~ spl39_56 | ~ spl39_230), inference(avatar_contradiction_clause, [], [f271652])).
fof(f271652, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_56 | ~ spl39_230)), inference(subsumption_resolution, [], [f271610, f267205])).
fof(f267205, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n2)) | ~ spl39_15), inference(subsumption_resolution, [], [f267120, f240])).
fof(f267120, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, n2)) | ~ spl39_15), inference(resolution, [], [f1824, f647])).
fof(f1824, plain, (! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, X4))) | ~ spl39_15), inference(subsumption_resolution, [], [f1803, f652])).
fof(f652, plain, leq(n2, n3), inference(resolution, [], [f242, f397])).
fof(f397, plain, gt(n3, n2), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', gt_3_2)).
fof(f1803, plain, (! [X4] : (~ leq(n2, n3) | (s_sworst7_init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)) | ~ spl39_15), inference(resolution, [], [f634, f647])).
fof(f271610, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_56 | ~ spl39_230)), inference(backward_demodulation, [], [f271550, f1099])).
fof(f271550, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_230)), inference(forward_demodulation, [], [f1031, f20152])).
fof(f20152, plain, ((n2 = sK37) | ~ spl39_230), inference(avatar_component_clause, [], [f20150])).
fof(f271546, plain, (spl39_10 | ~ spl39_15 | ~ spl39_230 | ~ spl39_240), inference(avatar_contradiction_clause, [], [f271545])).
fof(f271545, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_230 | ~ spl39_240)), inference(subsumption_resolution, [], [f271503, f267206])).
fof(f267206, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n0)) | ~ spl39_15), inference(subsumption_resolution, [], [f267139, f647])).
fof(f267139, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, n0)) | ~ spl39_15), inference(resolution, [], [f1824, f240])).
fof(f271503, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_230 | ~ spl39_240)), inference(backward_demodulation, [], [f271309, f20152])).
fof(f271309, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_240)), inference(backward_demodulation, [], [f1031, f20857])).
fof(f271305, plain, (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_241), inference(avatar_contradiction_clause, [], [f271304])).
fof(f271304, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_241)), inference(subsumption_resolution, [], [f271303, f267302])).
fof(f267302, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n3, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_48 | ~ spl39_241)), inference(backward_demodulation, [], [f256541, f1041])).
fof(f256541, plain, (~ (s_sworst7_init = a_select3(simplex7_init, sK37, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_241)), inference(backward_demodulation, [], [f1031, f20861])).
fof(f20861, plain, ((n1 = sK36) | ~ spl39_241), inference(avatar_component_clause, [], [f20859])).
fof(f20859, plain, (spl39_241 <=> (n1 = sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_241])])).
fof(f271303, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, n1)) | ~ spl39_15), inference(forward_demodulation, [], [f271302, f451])).
fof(f451, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f408, f317])).
fof(f317, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', succ_plus_1_r)).
fof(f408, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', successor_1)).
fof(f271302, plain, ((s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl39_15), inference(subsumption_resolution, [], [f271301, f648])).
fof(f648, plain, leq(n1, n2), inference(resolution, [], [f242, f393])).
fof(f393, plain, gt(n2, n1), inference(cnf_transformation, [], [f69])).
fof(f69, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', gt_2_1)).
fof(f271301, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl39_15), inference(forward_demodulation, [], [f271241, f451])).
fof(f271241, plain, (~ leq(plus(n0, n1), n2) | (s_sworst7_init = a_select3(simplex7_init, n3, plus(n0, n1))) | ~ spl39_15), inference(resolution, [], [f1825, f645])).
fof(f645, plain, ! [X0] : leq(X0, plus(X0, n1)), inference(resolution, [], [f242, f414])).
fof(f414, plain, ! [X0] : gt(plus(X0, n1), X0), inference(definition_unfolding, [], [f246, f317])).
fof(f246, plain, ! [X0] : gt(succ(X0), X0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : gt(succ(X0), X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', gt_succ)).
fof(f267224, plain, (spl39_10 | ~ spl39_15 | ~ spl39_230 | ~ spl39_241), inference(avatar_contradiction_clause, [], [f267223])).
fof(f267223, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_230 | ~ spl39_241)), inference(subsumption_resolution, [], [f267222, f256863])).
fof(f256863, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n2, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_230 | ~ spl39_241)), inference(backward_demodulation, [], [f256541, f20152])).
fof(f267222, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, n1)) | ~ spl39_15), inference(forward_demodulation, [], [f267221, f451])).
fof(f267221, plain, ((s_sworst7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | ~ spl39_15), inference(subsumption_resolution, [], [f267220, f648])).
fof(f267220, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | ~ spl39_15), inference(forward_demodulation, [], [f267160, f451])).
fof(f267160, plain, (~ leq(plus(n0, n1), n2) | (s_sworst7_init = a_select3(simplex7_init, n2, plus(n0, n1))) | ~ spl39_15), inference(resolution, [], [f1824, f645])).
fof(f256784, plain, (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_241), inference(avatar_contradiction_clause, [], [f256783])).
fof(f256783, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_241)), inference(subsumption_resolution, [], [f256708, f20049])).
fof(f20049, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n1)) | ~ spl39_15), inference(subsumption_resolution, [], [f20013, f648])).
fof(f20013, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, n1)) | ~ spl39_15), inference(resolution, [], [f1829, f644])).
fof(f644, plain, leq(n0, n1), inference(resolution, [], [f242, f388])).
fof(f388, plain, gt(n1, n0), inference(cnf_transformation, [], [f64])).
fof(f64, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', gt_1_0)).
fof(f256708, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_241)), inference(backward_demodulation, [], [f256541, f1080])).
fof(f256455, plain, (spl39_10 | ~ spl39_15 | ~ spl39_229 | ~ spl39_240), inference(avatar_contradiction_clause, [], [f256454])).
fof(f256454, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_229 | ~ spl39_240)), inference(subsumption_resolution, [], [f256396, f256258])).
fof(f256258, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n0)) | ~ spl39_15), inference(subsumption_resolution, [], [f256191, f647])).
fof(f256191, plain, (~ leq(n0, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, n0)) | ~ spl39_15), inference(resolution, [], [f1823, f240])).
fof(f1823, plain, (! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, X3))) | ~ spl39_15), inference(subsumption_resolution, [], [f1802, f651])).
fof(f651, plain, leq(n1, n3), inference(resolution, [], [f242, f394])).
fof(f394, plain, gt(n3, n1), inference(cnf_transformation, [], [f70])).
fof(f70, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', gt_3_1)).
fof(f1802, plain, (! [X3] : (~ leq(n1, n3) | (s_sworst7_init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)) | ~ spl39_15), inference(resolution, [], [f634, f644])).
fof(f256396, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n0)) | (spl39_10 | ~ spl39_15 | ~ spl39_229 | ~ spl39_240)), inference(backward_demodulation, [], [f20678, f20857])).
fof(f20678, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_229)), inference(backward_demodulation, [], [f1031, f20148])).
fof(f20148, plain, ((n1 = sK37) | ~ spl39_229), inference(avatar_component_clause, [], [f20146])).
fof(f256350, plain, (spl39_10 | ~ spl39_15 | ~ spl39_56 | ~ spl39_229), inference(avatar_contradiction_clause, [], [f256349])).
fof(f256349, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_56 | ~ spl39_229)), inference(subsumption_resolution, [], [f256291, f256257])).
fof(f256257, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n2)) | ~ spl39_15), inference(subsumption_resolution, [], [f256172, f240])).
fof(f256172, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, n2)) | ~ spl39_15), inference(resolution, [], [f1823, f647])).
fof(f256291, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_56 | ~ spl39_229)), inference(backward_demodulation, [], [f20678, f1099])).
fof(f256283, plain, (spl39_56 | spl39_240 | spl39_241 | ~ spl39_13 | ~ spl39_14), inference(avatar_split_clause, [], [f20887, f545, f540, f20859, f20855, f1097])).
fof(f540, plain, (spl39_13 <=> leq(sK36, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_13])])).
fof(f545, plain, (spl39_14 <=> leq(n0, sK36)), introduced(avatar_definition, [new_symbols(naming, [spl39_14])])).
fof(f20887, plain, ((n1 = sK36) | (n0 = sK36) | (n2 = sK36) | (~ spl39_13 | ~ spl39_14)), inference(subsumption_resolution, [], [f20832, f542])).
fof(f542, plain, (leq(sK36, n2) | ~ spl39_13), inference(avatar_component_clause, [], [f540])).
fof(f20832, plain, ((n1 = sK36) | (n0 = sK36) | ~ leq(sK36, n2) | (n2 = sK36) | ~ spl39_14), inference(resolution, [], [f547, f404])).
fof(f404, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f159])).
fof(f159, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', finite_domain_2)).
fof(f547, plain, (leq(n0, sK36) | ~ spl39_14), inference(avatar_component_clause, [], [f545])).
fof(f256276, plain, (spl39_10 | ~ spl39_15 | ~ spl39_229 | ~ spl39_241), inference(avatar_contradiction_clause, [], [f256275])).
fof(f256275, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_229 | ~ spl39_241)), inference(subsumption_resolution, [], [f256274, f20908])).
fof(f20908, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n1, n1)) | (spl39_10 | ~ spl39_15 | ~ spl39_229 | ~ spl39_241)), inference(backward_demodulation, [], [f20678, f20861])).
fof(f256274, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, n1)) | ~ spl39_15), inference(forward_demodulation, [], [f256273, f451])).
fof(f256273, plain, ((s_sworst7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | ~ spl39_15), inference(subsumption_resolution, [], [f256272, f648])).
fof(f256272, plain, (~ leq(n1, n2) | (s_sworst7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | ~ spl39_15), inference(forward_demodulation, [], [f256212, f451])).
fof(f256212, plain, (~ leq(plus(n0, n1), n2) | (s_sworst7_init = a_select3(simplex7_init, n1, plus(n0, n1))) | ~ spl39_15), inference(resolution, [], [f1823, f645])).
fof(f20072, plain, (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_56), inference(avatar_contradiction_clause, [], [f20071])).
fof(f20071, plain, ($false | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_56)), inference(subsumption_resolution, [], [f20070, f1102])).
fof(f1102, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, n2)) | (spl39_10 | ~ spl39_15 | ~ spl39_54 | ~ spl39_56)), inference(backward_demodulation, [], [f1084, f1099])).
fof(f1084, plain, (~ (s_sworst7_init = a_select3(simplex7_init, n0, sK36)) | (spl39_10 | ~ spl39_15 | ~ spl39_54)), inference(backward_demodulation, [], [f1031, f1080])).
fof(f20070, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, n2)) | ~ spl39_15), inference(forward_demodulation, [], [f20069, f1208])).
fof(f1208, plain, (n2 = plus(n0, n2)), inference(forward_demodulation, [], [f1190, f628])).
fof(f628, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f452, f451])).
fof(f452, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f409, f317, f317])).
fof(f409, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', successor_2)).
fof(f1190, plain, (plus(n1, n1) = plus(n0, n2)), inference(superposition, [], [f420, f451])).
fof(f420, plain, ! [X0] : (plus(X0, n2) = plus(plus(X0, n1), n1)), inference(definition_unfolding, [], [f319, f317, f317])).
fof(f319, plain, ! [X0] : (plus(X0, n2) = succ(succ(X0))), inference(cnf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (plus(X0, n2) = succ(succ(X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', succ_plus_2_r)).
fof(f20069, plain, ((s_sworst7_init = a_select3(simplex7_init, n0, plus(n0, n2))) | ~ spl39_15), inference(subsumption_resolution, [], [f20068, f240])).
fof(f20068, plain, (~ leq(n2, n2) | (s_sworst7_init = a_select3(simplex7_init, n0, plus(n0, n2))) | ~ spl39_15), inference(forward_demodulation, [], [f20038, f1208])).
fof(f20038, plain, (~ leq(plus(n0, n2), n2) | (s_sworst7_init = a_select3(simplex7_init, n0, plus(n0, n2))) | ~ spl39_15), inference(resolution, [], [f1829, f3355])).
fof(f3355, plain, ! [X2] : leq(X2, plus(X2, n2)), inference(resolution, [], [f777, f242])).
fof(f777, plain, ! [X1] : gt(plus(X1, n2), X1), inference(forward_demodulation, [], [f766, f420])).
fof(f766, plain, ! [X1] : gt(plus(plus(X1, n1), n1), X1), inference(resolution, [], [f432, f645])).
fof(f432, plain, ! [X0, X1] : (~ leq(plus(X0, n1), X1) | gt(X1, X0)), inference(definition_unfolding, [], [f332, f317])).
fof(f332, plain, ! [X0, X1] : (gt(X1, X0) | ~ leq(succ(X0), X1)), inference(cnf_transformation, [], [f139])).
fof(f139, plain, ! [X0, X1] : (gt(X1, X0) | ~ leq(succ(X0), X1)), inference(ennf_transformation, [], [f43])).
fof(f43, plain, ! [X0, X1] : (leq(succ(X0), X1) => gt(X1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', leq_succ_gt)).
fof(f3512, plain, (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15), inference(avatar_contradiction_clause, [], [f3511])).
fof(f3511, plain, ($false | (spl39_2 | ~ spl39_3 | ~ spl39_4 | ~ spl39_15)), inference(subsumption_resolution, [], [f3510, f3389])).
fof(f3389, plain, (~ (s_sworst7_init = a_select2(s_center7_init, sK34)) | (spl39_2 | ~ spl39_15)), inference(forward_demodulation, [], [f489, f551])).
fof(f489, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | spl39_2), inference(avatar_component_clause, [], [f487])).
fof(f487, plain, (spl39_2 <=> (s_worst7_init = a_select2(s_center7_init, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl39_2])])).
fof(f3510, plain, ((s_sworst7_init = a_select2(s_center7_init, sK34)) | (~ spl39_3 | ~ spl39_4 | ~ spl39_15)), inference(subsumption_resolution, [], [f3449, f494])).
fof(f494, plain, (leq(sK34, n2) | ~ spl39_3), inference(avatar_component_clause, [], [f492])).
fof(f492, plain, (spl39_3 <=> leq(sK34, n2)), introduced(avatar_definition, [new_symbols(naming, [spl39_3])])).
fof(f3449, plain, (~ leq(sK34, n2) | (s_sworst7_init = a_select2(s_center7_init, sK34)) | (~ spl39_4 | ~ spl39_15)), inference(resolution, [], [f499, f636])).
fof(f636, plain, (! [X1] : (~ leq(n0, X1) | ~ leq(X1, n2) | (s_sworst7_init = a_select2(s_center7_init, X1))) | ~ spl39_15), inference(backward_demodulation, [], [f445, f551])).
fof(f445, plain, ! [X1] : ((s_worst7_init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)), inference(definition_unfolding, [], [f369, f360])).
fof(f369, plain, ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f236])).
fof(f499, plain, (leq(n0, sK34) | ~ spl39_4), inference(avatar_component_clause, [], [f497])).
fof(f497, plain, (spl39_4 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl39_4])])).
fof(f3384, plain, (~ spl39_26 | ~ spl39_27), inference(avatar_contradiction_clause, [], [f3383])).
fof(f3383, plain, ($false | (~ spl39_26 | ~ spl39_27)), inference(subsumption_resolution, [], [f3372, f899])).
fof(f899, plain, ~ leq(n0, tptp_minus_1), inference(superposition, [], [f751, f418])).
fof(f418, plain, (n0 = plus(tptp_minus_1, n1)), inference(definition_unfolding, [], [f316, f317])).
fof(f316, plain, (n0 = succ(tptp_minus_1)), inference(cnf_transformation, [], [f28])).
fof(f28, plain, (n0 = succ(tptp_minus_1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', succ_tptp_minus_1)).
fof(f751, plain, ! [X2] : ~ leq(plus(X2, n1), X2), inference(resolution, [], [f417, f239])).
fof(f239, plain, ! [X0] : ~ gt(X0, X0), inference(cnf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : ~ gt(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', irreflexivity_gt)).
fof(f417, plain, ! [X0, X1] : (gt(plus(X1, n1), X0) | ~ leq(X0, X1)), inference(definition_unfolding, [], [f248, f317])).
fof(f248, plain, ! [X0, X1] : (gt(succ(X1), X0) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0, X1] : ((leq(X0, X1) | ~ gt(succ(X1), X0)) & (gt(succ(X1), X0) | ~ leq(X0, X1))), inference(nnf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : (leq(X0, X1) <=> gt(succ(X1), X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', leq_succ_gt_equiv)).
fof(f3372, plain, (leq(n0, tptp_minus_1) | (~ spl39_26 | ~ spl39_27)), inference(resolution, [], [f1057, f602])).
fof(f602, plain, (leq(n0, sK38) | ~ spl39_27), inference(avatar_component_clause, [], [f600])).
fof(f600, plain, (spl39_27 <=> leq(n0, sK38)), introduced(avatar_definition, [new_symbols(naming, [spl39_27])])).
fof(f1057, plain, (! [X0] : (~ leq(X0, sK38) | leq(X0, tptp_minus_1)) | ~ spl39_26), inference(resolution, [], [f1043, f241])).
fof(f241, plain, ! [X2, X0, X1] : (~ leq(X1, X2) | leq(X0, X2) | ~ leq(X0, X1)), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ! [X0, X1, X2] : (leq(X0, X2) | ~ leq(X1, X2) | ~ leq(X0, X1)), inference(flattening, [], [f111])).
fof(f111, plain, ! [X0, X1, X2] : (leq(X0, X2) | (~ leq(X1, X2) | ~ leq(X0, X1))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0, X1, X2] : ((leq(X1, X2) & leq(X0, X1)) => leq(X0, X2)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', transitivity_leq)).
fof(f1043, plain, (leq(sK38, tptp_minus_1) | ~ spl39_26), inference(forward_demodulation, [], [f597, f678])).
fof(f678, plain, (tptp_minus_1 = minus(n0, n1)), inference(superposition, [], [f428, f418])).
fof(f428, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f328, f327, f317])).
fof(f327, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', pred_minus_1)).
fof(f328, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV039+1.p', pred_succ)).
fof(f597, plain, (leq(sK38, minus(n0, n1)) | ~ spl39_26), inference(avatar_component_clause, [], [f595])).
fof(f595, plain, (spl39_26 <=> leq(sK38, minus(n0, n1))), introduced(avatar_definition, [new_symbols(naming, [spl39_26])])).
fof(f1528, plain, (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15), inference(avatar_contradiction_clause, [], [f1527])).
fof(f1527, plain, ($false | (spl39_6 | ~ spl39_7 | ~ spl39_8 | ~ spl39_15)), inference(subsumption_resolution, [], [f1526, f637])).
fof(f637, plain, (~ (s_sworst7_init = a_select2(s_values7_init, sK35)) | (spl39_6 | ~ spl39_15)), inference(forward_demodulation, [], [f508, f551])).
fof(f508, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | spl39_6), inference(avatar_component_clause, [], [f506])).
fof(f506, plain, (spl39_6 <=> (s_worst7_init = a_select2(s_values7_init, sK35))), introduced(avatar_definition, [new_symbols(naming, [spl39_6])])).
fof(f1526, plain, ((s_sworst7_init = a_select2(s_values7_init, sK35)) | (~ spl39_7 | ~ spl39_8 | ~ spl39_15)), inference(subsumption_resolution, [], [f1506, f513])).
fof(f513, plain, (leq(sK35, n3) | ~ spl39_7), inference(avatar_component_clause, [], [f511])).
fof(f511, plain, (spl39_7 <=> leq(sK35, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_7])])).
fof(f1506, plain, (~ leq(sK35, n3) | (s_sworst7_init = a_select2(s_values7_init, sK35)) | (~ spl39_8 | ~ spl39_15)), inference(resolution, [], [f635, f518])).
fof(f518, plain, (leq(n0, sK35) | ~ spl39_8), inference(avatar_component_clause, [], [f516])).
fof(f516, plain, (spl39_8 <=> leq(n0, sK35)), introduced(avatar_definition, [new_symbols(naming, [spl39_8])])).
fof(f635, plain, (! [X2] : (~ leq(n0, X2) | ~ leq(X2, n3) | (s_sworst7_init = a_select2(s_values7_init, X2))) | ~ spl39_15), inference(backward_demodulation, [], [f446, f551])).
fof(f446, plain, ! [X2] : ((s_worst7_init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)), inference(definition_unfolding, [], [f368, f360])).
fof(f368, plain, ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f236])).
fof(f620, plain, spl39_15, inference(avatar_split_clause, [], [f448, f550])).
fof(f448, plain, (s_sworst7_init = s_worst7_init), inference(definition_unfolding, [], [f359, f360])).
fof(f359, plain, (init = s_sworst7_init), inference(cnf_transformation, [], [f236])).
fof(f619, plain, spl39_16, inference(avatar_split_clause, [], [f361, f554])).
fof(f554, plain, (spl39_16 <=> leq(n0, s_best7)), introduced(avatar_definition, [new_symbols(naming, [spl39_16])])).
fof(f361, plain, leq(n0, s_best7), inference(cnf_transformation, [], [f236])).
fof(f618, plain, spl39_17, inference(avatar_split_clause, [], [f362, f558])).
fof(f558, plain, (spl39_17 <=> leq(n0, s_sworst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_17])])).
fof(f362, plain, leq(n0, s_sworst7), inference(cnf_transformation, [], [f236])).
fof(f617, plain, spl39_18, inference(avatar_split_clause, [], [f363, f562])).
fof(f562, plain, (spl39_18 <=> leq(n0, s_worst7)), introduced(avatar_definition, [new_symbols(naming, [spl39_18])])).
fof(f363, plain, leq(n0, s_worst7), inference(cnf_transformation, [], [f236])).
fof(f616, plain, spl39_19, inference(avatar_split_clause, [], [f364, f566])).
fof(f566, plain, (spl39_19 <=> leq(s_best7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_19])])).
fof(f364, plain, leq(s_best7, n3), inference(cnf_transformation, [], [f236])).
fof(f615, plain, spl39_20, inference(avatar_split_clause, [], [f365, f570])).
fof(f570, plain, (spl39_20 <=> leq(s_sworst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_20])])).
fof(f365, plain, leq(s_sworst7, n3), inference(cnf_transformation, [], [f236])).
fof(f614, plain, spl39_21, inference(avatar_split_clause, [], [f366, f574])).
fof(f574, plain, (spl39_21 <=> leq(s_worst7, n3)), introduced(avatar_definition, [new_symbols(naming, [spl39_21])])).
fof(f366, plain, leq(s_worst7, n3), inference(cnf_transformation, [], [f236])).
fof(f613, plain, (~ spl39_28 | spl39_23), inference(avatar_split_clause, [], [f444, f582, f605])).
fof(f605, plain, (spl39_28 <=> gt(loopcounter, n1)), introduced(avatar_definition, [new_symbols(naming, [spl39_28])])).
fof(f582, plain, (spl39_23 <=> (s_worst7_init = pvar1400_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_23])])).
fof(f444, plain, ((s_worst7_init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f370, f360])).
fof(f370, plain, ((init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f236])).
fof(f612, plain, (~ spl39_28 | spl39_24), inference(avatar_split_clause, [], [f443, f586, f605])).
fof(f586, plain, (spl39_24 <=> (s_worst7_init = pvar1401_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_24])])).
fof(f443, plain, ((s_worst7_init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f371, f360])).
fof(f371, plain, ((init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f236])).
fof(f611, plain, (~ spl39_28 | spl39_25), inference(avatar_split_clause, [], [f442, f590, f605])).
fof(f590, plain, (spl39_25 <=> (s_worst7_init = pvar1402_init)), introduced(avatar_definition, [new_symbols(naming, [spl39_25])])).
fof(f442, plain, ((s_worst7_init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(definition_unfolding, [], [f372, f360])).
fof(f372, plain, ((init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f236])).
fof(f610, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | spl39_9 | spl39_5 | spl39_1 | spl39_27 | spl39_28), inference(avatar_split_clause, [], [f458, f605, f600, f483, f502, f521, f574, f570, f566, f562, f558, f554, f550])).
fof(f521, plain, (spl39_9 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl39_9])])).
fof(f502, plain, (spl39_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl39_5])])).
fof(f483, plain, (spl39_1 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl39_1])])).
fof(f458, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f457])).
fof(f457, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f441])).
fof(f441, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f373, f360, f360, f411, f360])).
fof(f411, plain, (s_best7_init = s_worst7_init), inference(definition_unfolding, [], [f358, f360])).
fof(f358, plain, (s_best7_init = init), inference(cnf_transformation, [], [f236])).
fof(f373, plain, (gt(loopcounter, n1) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)), inference(cnf_transformation, [], [f236])).
fof(f609, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | spl39_9 | spl39_5 | spl39_1 | spl39_26 | spl39_28), inference(avatar_split_clause, [], [f460, f605, f595, f483, f502, f521, f574, f570, f566, f562, f558, f554, f550])).
fof(f460, plain, (gt(loopcounter, n1) | leq(sK38, minus(n0, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f459])).
fof(f459, plain, (gt(loopcounter, n1) | leq(sK38, minus(n0, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f440])).
fof(f440, plain, (gt(loopcounter, n1) | leq(sK38, minus(n0, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f374, f360, f360, f411, f360])).
fof(f374, plain, (gt(loopcounter, n1) | leq(sK38, minus(n0, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)), inference(cnf_transformation, [], [f236])).
fof(f603, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | spl39_9 | spl39_5 | spl39_1 | spl39_27 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25), inference(avatar_split_clause, [], [f464, f590, f586, f582, f600, f483, f502, f521, f574, f570, f566, f562, f558, f554, f550])).
fof(f464, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f463])).
fof(f463, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f438])).
fof(f438, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f376, f360, f360, f360, f360, f360, f411, f360])).
fof(f376, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(n0, sK38) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)), inference(cnf_transformation, [], [f236])).
fof(f598, plain, (~ spl39_15 | ~ spl39_16 | ~ spl39_17 | ~ spl39_18 | ~ spl39_19 | ~ spl39_20 | ~ spl39_21 | spl39_9 | spl39_5 | spl39_1 | spl39_26 | ~ spl39_23 | ~ spl39_24 | ~ spl39_25), inference(avatar_split_clause, [], [f466, f590, f586, f582, f595, f483, f502, f521, f574, f570, f566, f562, f558, f554, f550])).
fof(f466, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n0, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_sworst7_init = s_worst7_init)), inference(trivial_inequality_removal, [], [f465])).
fof(f465, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n0, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init)), inference(duplicate_literal_removal, [], [f437])).
fof(f437, plain, (~ (s_worst7_init = pvar1402_init) | ~ (s_worst7_init = pvar1401_init) | ~ (s_worst7_init = pvar1400_init) | leq(sK38, minus(n0, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (s_worst7_init = s_worst7_init) | ~ (s_sworst7_init = s_worst7_init) | ~ (s_worst7_init = s_worst7_init)), inference(definition_unfolding, [], [f377, f360, f360, f360, f360, f360, f411, f360])).
fof(f377, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(sK38, minus(n0, n1)) | sP6 | sP5 | sP4 | ~ leq(s_worst7, n3) | ~ leq(s_sworst7, n3) | ~ leq(s_best7, n3) | ~ leq(n0, s_worst7) | ~ leq(n0, s_sworst7) | ~ leq(n0, s_best7) | ~ (init = s_worst7_init) | ~ (init = s_sworst7_init) | ~ (s_best7_init = init)), inference(cnf_transformation, [], [f236])).
fof(f548, plain, (~ spl39_9 | spl39_14), inference(avatar_split_clause, [], [f353, f545, f521])).
fof(f353, plain, (leq(n0, sK36) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f233, plain, (((~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37)) & leq(sK36, n2) & leq(n0, sK36)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK36, sK37])], [f230, f232, f231])).
fof(f231, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) & leq(sK36, n2) & leq(n0, sK36))), introduced(choice_axiom, [])).
fof(f232, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK36)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK37, sK36)) & leq(sK37, n3) & leq(n0, sK37))), introduced(choice_axiom, [])).
fof(f230, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f229])).
fof(f229, plain, (? [X7] : (? [X8] : (~ (init = a_select3(simplex7_init, X8, X7)) & leq(X8, n3) & leq(n0, X8)) & leq(X7, n2) & leq(n0, X7)) | ~ sP4), inference(nnf_transformation, [], [f170])).
fof(f543, plain, (~ spl39_9 | spl39_13), inference(avatar_split_clause, [], [f354, f540, f521])).
fof(f354, plain, (leq(sK36, n2) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f538, plain, (~ spl39_9 | spl39_12), inference(avatar_split_clause, [], [f355, f535, f521])).
fof(f355, plain, (leq(n0, sK37) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f533, plain, (~ spl39_9 | spl39_11), inference(avatar_split_clause, [], [f356, f530, f521])).
fof(f356, plain, (leq(sK37, n3) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f528, plain, (~ spl39_9 | ~ spl39_10), inference(avatar_split_clause, [], [f435, f525, f521])).
fof(f435, plain, (~ (s_worst7_init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(definition_unfolding, [], [f357, f360])).
fof(f357, plain, (~ (init = a_select3(simplex7_init, sK37, sK36)) | ~ sP4), inference(cnf_transformation, [], [f233])).
fof(f519, plain, (~ spl39_5 | spl39_8), inference(avatar_split_clause, [], [f350, f516, f502])).
fof(f350, plain, (leq(n0, sK35) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f228, plain, ((~ (init = a_select2(s_values7_init, sK35)) & leq(sK35, n3) & leq(n0, sK35)) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK35])], [f226, f227])).
fof(f227, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) => (~ (init = a_select2(s_values7_init, sK35)) & leq(sK35, n3) & leq(n0, sK35))), introduced(choice_axiom, [])).
fof(f226, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) | ~ sP5), inference(rectify, [], [f225])).
fof(f225, plain, (? [X6] : (~ (init = a_select2(s_values7_init, X6)) & leq(X6, n3) & leq(n0, X6)) | ~ sP5), inference(nnf_transformation, [], [f171])).
fof(f514, plain, (~ spl39_5 | spl39_7), inference(avatar_split_clause, [], [f351, f511, f502])).
fof(f351, plain, (leq(sK35, n3) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f509, plain, (~ spl39_5 | ~ spl39_6), inference(avatar_split_clause, [], [f434, f506, f502])).
fof(f434, plain, (~ (s_worst7_init = a_select2(s_values7_init, sK35)) | ~ sP5), inference(definition_unfolding, [], [f352, f360])).
fof(f352, plain, (~ (init = a_select2(s_values7_init, sK35)) | ~ sP5), inference(cnf_transformation, [], [f228])).
fof(f500, plain, (~ spl39_1 | spl39_4), inference(avatar_split_clause, [], [f347, f497, f483])).
fof(f347, plain, (leq(n0, sK34) | ~ sP6), inference(cnf_transformation, [], [f224])).
fof(f224, plain, ((~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34)) | ~ sP6), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34])], [f222, f223])).
fof(f223, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) => (~ (init = a_select2(s_center7_init, sK34)) & leq(sK34, n2) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f222, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) | ~ sP6), inference(rectify, [], [f221])).
fof(f221, plain, (? [X5] : (~ (init = a_select2(s_center7_init, X5)) & leq(X5, n2) & leq(n0, X5)) | ~ sP6), inference(nnf_transformation, [], [f172])).
fof(f495, plain, (~ spl39_1 | spl39_3), inference(avatar_split_clause, [], [f348, f492, f483])).
fof(f348, plain, (leq(sK34, n2) | ~ sP6), inference(cnf_transformation, [], [f224])).
fof(f490, plain, (~ spl39_1 | ~ spl39_2), inference(avatar_split_clause, [], [f433, f487, f483])).
fof(f433, plain, (~ (s_worst7_init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(definition_unfolding, [], [f349, f360])).
fof(f349, plain, (~ (init = a_select2(s_center7_init, sK34)) | ~ sP6), inference(cnf_transformation, [], [f224])).