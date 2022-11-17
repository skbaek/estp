fof(f860573, plain, $false, inference(avatar_sat_refutation, [], [f444, f449, f454, f463, f468, f473, f478, f483, f500, f505, f510, f515, f516, f517, f518, f519, f520, f51430, f669763, f669933, f670059, f670539, f708229, f708596, f859309, f859634, f859787, f859793, f860104, f860185, f860570])).
fof(f860570, plain, (spl37_6 | ~ spl37_21 | ~ spl37_27), inference(avatar_contradiction_clause, [], [f860569])).
fof(f860569, plain, ($false | (spl37_6 | ~ spl37_21 | ~ spl37_27)), inference(subsumption_resolution, [], [f860488, f859292])).
fof(f859292, plain, (init = a_select3(simplex7_init, n3, n0)), inference(subsumption_resolution, [], [f859225, f543])).
fof(f543, plain, leq(n0, n2), inference(resolution, [], [f237, f372])).
fof(f372, plain, gt(n2, n0), inference(cnf_transformation, [], [f65])).
fof(f65, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', gt_2_0)).
fof(f237, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', leq_gt1)).
fof(f859225, plain, (~ leq(n0, n2) | (init = a_select3(simplex7_init, n3, n0))), inference(resolution, [], [f1502, f235])).
fof(f235, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', reflexivity_leq)).
fof(f1502, plain, ! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (init = a_select3(simplex7_init, n3, X5))), inference(subsumption_resolution, [], [f1484, f235])).
fof(f1484, plain, ! [X5] : (~ leq(n3, n3) | (init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)), inference(resolution, [], [f350, f545])).
fof(f545, plain, leq(n0, n3), inference(resolution, [], [f237, f373])).
fof(f373, plain, gt(n3, n0), inference(cnf_transformation, [], [f66])).
fof(f66, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', gt_3_0)).
fof(f350, plain, ! [X4, X3] : (~ leq(n0, X4) | ~ leq(X4, n3) | (init = a_select3(simplex7_init, X4, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | (~ (init = a_select2(s_center7_init, sK36)) & leq(sK36, n2) & leq(n0, sK36)) | sP5 | sP4) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, minus(plus(n1, n2), n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK36])], [f229, f230])).
fof(f230, plain, (? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) => (~ (init = a_select2(s_center7_init, sK36)) & leq(sK36, n2) & leq(n0, sK36))), introduced(choice_axiom, [])).
fof(f229, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X0] : (~ (init = a_select2(s_center7_init, X0)) & leq(X0, n2) & leq(n0, X0)) | sP5 | sP4) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, minus(plus(n1, n2), n1)) | ~ leq(n0, X1)) & ! [X2] : ((init = a_select2(s_values7_init, X2)) | ~ leq(X2, n3) | ~ leq(n0, X2)) & ! [X3] : (! [X4] : ((init = a_select3(simplex7_init, X4, X3)) | ~ leq(X4, n3) | ~ leq(n0, X4)) | ~ leq(X3, n2) | ~ leq(n0, X3))), inference(rectify, [], [f172])).
fof(f172, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X4] : (~ (init = a_select2(s_center7_init, X4)) & leq(X4, n2) & leq(n0, X4)) | sP5 | sP4) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_center7_init, X0)) | ~ leq(X0, minus(plus(n1, n2), n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | ~ leq(X1, n3) | ~ leq(n0, X1)) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ leq(X2, n2) | ~ leq(n0, X2))), inference(definition_folding, [], [f150, e171, e170])).
fof(f170, plain, (? [X6] : (? [X7] : (~ (init = a_select3(simplex7_init, X7, X6)) & leq(X7, n3) & leq(n0, X7)) & leq(X6, n2) & leq(n0, X6)) | ~ sP4), inference(usedef, [], [e170])).
fof(e170, plain, (sP4 <=> ? [X6] : (? [X7] : (~ (init = a_select3(simplex7_init, X7, X6)) & leq(X7, n3) & leq(n0, X7)) & leq(X6, n2) & leq(n0, X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f171, plain, (? [X5] : (~ (init = a_select2(s_values7_init, X5)) & leq(X5, n3) & leq(n0, X5)) | ~ sP5), inference(usedef, [], [e171])).
fof(e171, plain, (sP5 <=> ? [X5] : (~ (init = a_select2(s_values7_init, X5)) & leq(X5, n3) & leq(n0, X5))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f150, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X4] : (~ (init = a_select2(s_center7_init, X4)) & leq(X4, n2) & leq(n0, X4)) | ? [X5] : (~ (init = a_select2(s_values7_init, X5)) & leq(X5, n3) & leq(n0, X5)) | ? [X6] : (? [X7] : (~ (init = a_select3(simplex7_init, X7, X6)) & leq(X7, n3) & leq(n0, X7)) & leq(X6, n2) & leq(n0, X6))) & (((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_center7_init, X0)) | ~ leq(X0, minus(plus(n1, n2), n1)) | ~ leq(n0, X0)) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | ~ leq(X1, n3) | ~ leq(n0, X1)) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ leq(X2, n2) | ~ leq(n0, X2))), inference(flattening, [], [f149])).
fof(f149, plain, ((((~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init)) & gt(loopcounter, n1)) | ? [X4] : (~ (init = a_select2(s_center7_init, X4)) & (leq(X4, n2) & leq(n0, X4))) | ? [X5] : (~ (init = a_select2(s_values7_init, X5)) & (leq(X5, n3) & leq(n0, X5))) | ? [X6] : (? [X7] : (~ (init = a_select3(simplex7_init, X7, X6)) & (leq(X7, n3) & leq(n0, X7))) & (leq(X6, n2) & leq(n0, X6)))) & ((((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init)) | ~ gt(loopcounter, n1)) & ! [X0] : ((init = a_select2(s_center7_init, X0)) | (~ leq(X0, minus(plus(n1, n2), n1)) | ~ leq(n0, X0))) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | (~ leq(X1, n3) | ~ leq(n0, X1))) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | (~ leq(X3, n3) | ~ leq(n0, X3))) | (~ leq(X2, n2) | ~ leq(n0, X2))))), inference(ennf_transformation, [], [f107])).
fof(f107, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X0] : ((leq(X0, minus(plus(n1, n2), n1)) & leq(n0, X0)) => (init = a_select2(s_center7_init, X0))) & ! [X1] : ((leq(X1, n3) & leq(n0, X1)) => (init = a_select2(s_values7_init, X1))) & ! [X2] : ((leq(X2, n2) & leq(n0, X2)) => ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select3(simplex7_init, X3, X2))))) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => (init = a_select2(s_center7_init, X4))) & ! [X5] : ((leq(X5, n3) & leq(n0, X5)) => (init = a_select2(s_values7_init, X5))) & ! [X6] : ((leq(X6, n2) & leq(n0, X6)) => ! [X7] : ((leq(X7, n3) & leq(n0, X7)) => (init = a_select3(simplex7_init, X7, X6)))))), inference(rectify, [], [f54])).
fof(f54, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X19] : ((leq(X19, minus(plus(n1, n2), n1)) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (a_select3(simplex7_init, X17, X13) = init)))) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X28] : ((leq(X28, n2) & leq(n0, X28)) => (init = a_select2(s_center7_init, X28))) & ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select2(s_values7_init, X27))) & ! [X20] : ((leq(X20, n2) & leq(n0, X20)) => ! [X21] : ((leq(X21, n3) & leq(n0, X21)) => (init = a_select3(simplex7_init, X21, X20)))))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ (((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X19] : ((leq(X19, minus(plus(n1, n2), n1)) & leq(n0, X19)) => (init = a_select2(s_center7_init, X19))) & ! [X3] : ((leq(X3, n3) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (a_select3(simplex7_init, X17, X13) = init)))) => ((gt(loopcounter, n1) => ((init = pvar1402_init) & (init = pvar1401_init) & (init = pvar1400_init))) & ! [X28] : ((leq(X28, n2) & leq(n0, X28)) => (init = a_select2(s_center7_init, X28))) & ! [X27] : ((leq(X27, n3) & leq(n0, X27)) => (init = a_select2(s_values7_init, X27))) & ! [X20] : ((leq(X20, n2) & leq(n0, X20)) => ! [X21] : ((leq(X21, n3) & leq(n0, X21)) => (init = a_select3(simplex7_init, X21, X20)))))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', gauss_init_0081)).
fof(f860488, plain, (~ (init = a_select3(simplex7_init, n3, n0)) | (spl37_6 | ~ spl37_21 | ~ spl37_27)), inference(backward_demodulation, [], [f860414, f880])).
fof(f880, plain, ((n3 = sK35) | ~ spl37_27), inference(avatar_component_clause, [], [f878])).
fof(f878, plain, (spl37_27 <=> (n3 = sK35)), introduced(avatar_definition, [new_symbols(naming, [spl37_27])])).
fof(f860414, plain, (~ (init = a_select3(simplex7_init, sK35, n0)) | (spl37_6 | ~ spl37_21)), inference(forward_demodulation, [], [f462, f853])).
fof(f853, plain, ((n0 = sK34) | ~ spl37_21), inference(avatar_component_clause, [], [f851])).
fof(f851, plain, (spl37_21 <=> (n0 = sK34)), introduced(avatar_definition, [new_symbols(naming, [spl37_21])])).
fof(f462, plain, (~ (init = a_select3(simplex7_init, sK35, sK34)) | spl37_6), inference(avatar_component_clause, [], [f460])).
fof(f460, plain, (spl37_6 <=> (init = a_select3(simplex7_init, sK35, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl37_6])])).
fof(f860185, plain, (spl37_6 | ~ spl37_9 | ~ spl37_10 | ~ spl37_23), inference(avatar_contradiction_clause, [], [f860184])).
fof(f860184, plain, ($false | (spl37_6 | ~ spl37_9 | ~ spl37_10 | ~ spl37_23)), inference(subsumption_resolution, [], [f860107, f51426])).
fof(f51426, plain, ((init = a_select3(simplex7_init, n0, sK34)) | (~ spl37_9 | ~ spl37_10)), inference(subsumption_resolution, [], [f51362, f477])).
fof(f477, plain, (leq(sK34, n2) | ~ spl37_9), inference(avatar_component_clause, [], [f475])).
fof(f475, plain, (spl37_9 <=> leq(sK34, n2)), introduced(avatar_definition, [new_symbols(naming, [spl37_9])])).
fof(f51362, plain, (~ leq(sK34, n2) | (init = a_select3(simplex7_init, n0, sK34)) | ~ spl37_10), inference(resolution, [], [f482, f1503])).
fof(f1503, plain, ! [X33] : (~ leq(n0, X33) | ~ leq(X33, n2) | (init = a_select3(simplex7_init, n0, X33))), inference(subsumption_resolution, [], [f1496, f545])).
fof(f1496, plain, ! [X33] : (~ leq(n0, n3) | (init = a_select3(simplex7_init, n0, X33)) | ~ leq(X33, n2) | ~ leq(n0, X33)), inference(resolution, [], [f350, f235])).
fof(f482, plain, (leq(n0, sK34) | ~ spl37_10), inference(avatar_component_clause, [], [f480])).
fof(f480, plain, (spl37_10 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl37_10])])).
fof(f860107, plain, (~ (init = a_select3(simplex7_init, n0, sK34)) | (spl37_6 | ~ spl37_23)), inference(backward_demodulation, [], [f462, f862])).
fof(f862, plain, ((n0 = sK35) | ~ spl37_23), inference(avatar_component_clause, [], [f860])).
fof(f860, plain, (spl37_23 <=> (n0 = sK35)), introduced(avatar_definition, [new_symbols(naming, [spl37_23])])).
fof(f860104, plain, (spl37_6 | ~ spl37_25 | ~ spl37_27), inference(avatar_contradiction_clause, [], [f860103])).
fof(f860103, plain, ($false | (spl37_6 | ~ spl37_25 | ~ spl37_27)), inference(subsumption_resolution, [], [f860022, f859291])).
fof(f859291, plain, (init = a_select3(simplex7_init, n3, n2)), inference(subsumption_resolution, [], [f859206, f235])).
fof(f859206, plain, (~ leq(n2, n2) | (init = a_select3(simplex7_init, n3, n2))), inference(resolution, [], [f1502, f543])).
fof(f860022, plain, (~ (init = a_select3(simplex7_init, n3, n2)) | (spl37_6 | ~ spl37_25 | ~ spl37_27)), inference(backward_demodulation, [], [f859794, f880])).
fof(f859794, plain, (~ (init = a_select3(simplex7_init, sK35, n2)) | (spl37_6 | ~ spl37_25)), inference(forward_demodulation, [], [f462, f871])).
fof(f871, plain, ((n2 = sK34) | ~ spl37_25), inference(avatar_component_clause, [], [f869])).
fof(f869, plain, (spl37_25 <=> (n2 = sK34)), introduced(avatar_definition, [new_symbols(naming, [spl37_25])])).
fof(f859793, plain, (spl37_27 | spl37_23 | spl37_266 | spl37_267 | ~ spl37_7 | ~ spl37_8), inference(avatar_split_clause, [], [f55276, f470, f465, f55247, f55243, f860, f878])).
fof(f55243, plain, (spl37_266 <=> (n1 = sK35)), introduced(avatar_definition, [new_symbols(naming, [spl37_266])])).
fof(f55247, plain, (spl37_267 <=> (n2 = sK35)), introduced(avatar_definition, [new_symbols(naming, [spl37_267])])).
fof(f465, plain, (spl37_7 <=> leq(sK35, n3)), introduced(avatar_definition, [new_symbols(naming, [spl37_7])])).
fof(f470, plain, (spl37_8 <=> leq(n0, sK35)), introduced(avatar_definition, [new_symbols(naming, [spl37_8])])).
fof(f55276, plain, ((n2 = sK35) | (n1 = sK35) | (n0 = sK35) | (n3 = sK35) | (~ spl37_7 | ~ spl37_8)), inference(subsumption_resolution, [], [f55213, f467])).
fof(f467, plain, (leq(sK35, n3) | ~ spl37_7), inference(avatar_component_clause, [], [f465])).
fof(f55213, plain, ((n2 = sK35) | (n1 = sK35) | (n0 = sK35) | ~ leq(sK35, n3) | (n3 = sK35) | ~ spl37_8), inference(resolution, [], [f472, f388])).
fof(f388, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f162])).
fof(f162, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f161])).
fof(f161, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', finite_domain_3)).
fof(f472, plain, (leq(n0, sK35) | ~ spl37_8), inference(avatar_component_clause, [], [f470])).
fof(f859787, plain, (spl37_6 | ~ spl37_25 | ~ spl37_266), inference(avatar_contradiction_clause, [], [f859786])).
fof(f859786, plain, ($false | (spl37_6 | ~ spl37_25 | ~ spl37_266)), inference(subsumption_resolution, [], [f859707, f669636])).
fof(f669636, plain, (init = a_select3(simplex7_init, n1, n2)), inference(subsumption_resolution, [], [f669555, f235])).
fof(f669555, plain, (~ leq(n2, n2) | (init = a_select3(simplex7_init, n1, n2))), inference(resolution, [], [f1500, f543])).
fof(f1500, plain, ! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (init = a_select3(simplex7_init, n1, X3))), inference(subsumption_resolution, [], [f1482, f547])).
fof(f547, plain, leq(n1, n3), inference(resolution, [], [f237, f377])).
fof(f377, plain, gt(n3, n1), inference(cnf_transformation, [], [f70])).
fof(f70, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', gt_3_1)).
fof(f1482, plain, ! [X3] : (~ leq(n1, n3) | (init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)), inference(resolution, [], [f350, f540])).
fof(f540, plain, leq(n0, n1), inference(resolution, [], [f237, f371])).
fof(f371, plain, gt(n1, n0), inference(cnf_transformation, [], [f64])).
fof(f64, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', gt_1_0)).
fof(f859707, plain, (~ (init = a_select3(simplex7_init, n1, n2)) | (spl37_6 | ~ spl37_25 | ~ spl37_266)), inference(backward_demodulation, [], [f859326, f871])).
fof(f859326, plain, (~ (init = a_select3(simplex7_init, n1, sK34)) | (spl37_6 | ~ spl37_266)), inference(backward_demodulation, [], [f462, f55245])).
fof(f55245, plain, ((n1 = sK35) | ~ spl37_266), inference(avatar_component_clause, [], [f55243])).
fof(f859634, plain, (spl37_6 | ~ spl37_21 | ~ spl37_266), inference(avatar_contradiction_clause, [], [f859633])).
fof(f859633, plain, ($false | (spl37_6 | ~ spl37_21 | ~ spl37_266)), inference(subsumption_resolution, [], [f859557, f669637])).
fof(f669637, plain, (init = a_select3(simplex7_init, n1, n0)), inference(subsumption_resolution, [], [f669574, f543])).
fof(f669574, plain, (~ leq(n0, n2) | (init = a_select3(simplex7_init, n1, n0))), inference(resolution, [], [f1500, f235])).
fof(f859557, plain, (~ (init = a_select3(simplex7_init, n1, n0)) | (spl37_6 | ~ spl37_21 | ~ spl37_266)), inference(backward_demodulation, [], [f859326, f853])).
fof(f859309, plain, (spl37_6 | ~ spl37_27 | ~ spl37_259), inference(avatar_contradiction_clause, [], [f859308])).
fof(f859308, plain, ($false | (spl37_6 | ~ spl37_27 | ~ spl37_259)), inference(subsumption_resolution, [], [f859307, f708681])).
fof(f708681, plain, (~ (init = a_select3(simplex7_init, n3, n1)) | (spl37_6 | ~ spl37_27 | ~ spl37_259)), inference(backward_demodulation, [], [f708593, f880])).
fof(f708593, plain, (~ (init = a_select3(simplex7_init, sK35, n1)) | (spl37_6 | ~ spl37_259)), inference(forward_demodulation, [], [f462, f51393])).
fof(f51393, plain, ((n1 = sK34) | ~ spl37_259), inference(avatar_component_clause, [], [f51391])).
fof(f51391, plain, (spl37_259 <=> (n1 = sK34)), introduced(avatar_definition, [new_symbols(naming, [spl37_259])])).
fof(f859307, plain, (init = a_select3(simplex7_init, n3, n1)), inference(forward_demodulation, [], [f859306, f417])).
fof(f417, plain, (n1 = plus(n0, n1)), inference(definition_unfolding, [], [f391, f312])).
fof(f312, plain, ! [X0] : (succ(X0) = plus(X0, n1)), inference(cnf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (succ(X0) = plus(X0, n1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', succ_plus_1_r)).
fof(f391, plain, (n1 = succ(n0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, (n1 = succ(n0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', successor_1)).
fof(f859306, plain, (init = a_select3(simplex7_init, n3, plus(n0, n1))), inference(subsumption_resolution, [], [f859305, f544])).
fof(f544, plain, leq(n1, n2), inference(resolution, [], [f237, f376])).
fof(f376, plain, gt(n2, n1), inference(cnf_transformation, [], [f69])).
fof(f69, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', gt_2_1)).
fof(f859305, plain, (~ leq(n1, n2) | (init = a_select3(simplex7_init, n3, plus(n0, n1)))), inference(forward_demodulation, [], [f859253, f417])).
fof(f859253, plain, (~ leq(plus(n0, n1), n2) | (init = a_select3(simplex7_init, n3, plus(n0, n1)))), inference(resolution, [], [f1502, f541])).
fof(f541, plain, ! [X0] : leq(X0, plus(X0, n1)), inference(resolution, [], [f237, f396])).
fof(f396, plain, ! [X0] : gt(plus(X0, n1), X0), inference(definition_unfolding, [], [f241, f312])).
fof(f241, plain, ! [X0] : gt(succ(X0), X0), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : gt(succ(X0), X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', gt_succ)).
fof(f708596, plain, (spl37_6 | ~ spl37_259 | ~ spl37_266), inference(avatar_contradiction_clause, [], [f708595])).
fof(f708595, plain, ($false | (spl37_6 | ~ spl37_259 | ~ spl37_266)), inference(subsumption_resolution, [], [f708594, f669635])).
fof(f669635, plain, (init = a_select3(simplex7_init, n1, n1)), inference(subsumption_resolution, [], [f669540, f544])).
fof(f669540, plain, (~ leq(n1, n2) | (init = a_select3(simplex7_init, n1, n1))), inference(resolution, [], [f1500, f540])).
fof(f708594, plain, (~ (init = a_select3(simplex7_init, n1, n1)) | (spl37_6 | ~ spl37_259 | ~ spl37_266)), inference(forward_demodulation, [], [f708593, f55245])).
fof(f708229, plain, (spl37_2 | ~ spl37_3 | ~ spl37_4), inference(avatar_contradiction_clause, [], [f708228])).
fof(f708228, plain, ($false | (spl37_2 | ~ spl37_3 | ~ spl37_4)), inference(subsumption_resolution, [], [f708227, f443])).
fof(f443, plain, (~ (init = a_select2(s_values7_init, sK33)) | spl37_2), inference(avatar_component_clause, [], [f441])).
fof(f441, plain, (spl37_2 <=> (init = a_select2(s_values7_init, sK33))), introduced(avatar_definition, [new_symbols(naming, [spl37_2])])).
fof(f708227, plain, ((init = a_select2(s_values7_init, sK33)) | (~ spl37_3 | ~ spl37_4)), inference(subsumption_resolution, [], [f706733, f448])).
fof(f448, plain, (leq(sK33, n3) | ~ spl37_3), inference(avatar_component_clause, [], [f446])).
fof(f446, plain, (spl37_3 <=> leq(sK33, n3)), introduced(avatar_definition, [new_symbols(naming, [spl37_3])])).
fof(f706733, plain, (~ leq(sK33, n3) | (init = a_select2(s_values7_init, sK33)) | ~ spl37_4), inference(resolution, [], [f351, f453])).
fof(f453, plain, (leq(n0, sK33) | ~ spl37_4), inference(avatar_component_clause, [], [f451])).
fof(f451, plain, (spl37_4 <=> leq(n0, sK33)), introduced(avatar_definition, [new_symbols(naming, [spl37_4])])).
fof(f351, plain, ! [X2] : (~ leq(n0, X2) | ~ leq(X2, n3) | (init = a_select2(s_values7_init, X2))), inference(cnf_transformation, [], [f231])).
fof(f670539, plain, (spl37_11 | ~ spl37_15 | ~ spl37_16), inference(avatar_split_clause, [], [f670538, f507, f502, f485])).
fof(f485, plain, (spl37_11 <=> (init = a_select2(s_center7_init, sK36))), introduced(avatar_definition, [new_symbols(naming, [spl37_11])])).
fof(f502, plain, (spl37_15 <=> leq(sK36, n2)), introduced(avatar_definition, [new_symbols(naming, [spl37_15])])).
fof(f507, plain, (spl37_16 <=> leq(n0, sK36)), introduced(avatar_definition, [new_symbols(naming, [spl37_16])])).
fof(f670538, plain, ((init = a_select2(s_center7_init, sK36)) | (~ spl37_15 | ~ spl37_16)), inference(subsumption_resolution, [], [f8529, f504])).
fof(f504, plain, (leq(sK36, n2) | ~ spl37_15), inference(avatar_component_clause, [], [f502])).
fof(f8529, plain, ((init = a_select2(s_center7_init, sK36)) | ~ leq(sK36, n2) | ~ spl37_16), inference(resolution, [], [f509, f580])).
fof(f580, plain, ! [X1] : (~ leq(n0, X1) | (init = a_select2(s_center7_init, X1)) | ~ leq(X1, n2)), inference(backward_demodulation, [], [f565, f577])).
fof(f577, plain, (n2 = minus(n3, n1)), inference(superposition, [], [f410, f533])).
fof(f533, plain, (n3 = plus(n2, n1)), inference(forward_demodulation, [], [f532, f528])).
fof(f528, plain, (n2 = plus(n1, n1)), inference(forward_demodulation, [], [f418, f417])).
fof(f418, plain, (n2 = plus(plus(n0, n1), n1)), inference(definition_unfolding, [], [f392, f312, f312])).
fof(f392, plain, (n2 = succ(succ(n0))), inference(cnf_transformation, [], [f85])).
fof(f85, plain, (n2 = succ(succ(n0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', successor_2)).
fof(f532, plain, (n3 = plus(plus(n1, n1), n1)), inference(forward_demodulation, [], [f531, f417])).
fof(f531, plain, (n3 = plus(plus(n1, plus(n0, n1)), n1)), inference(forward_demodulation, [], [f419, f401])).
fof(f401, plain, ! [X0] : (plus(X0, n1) = plus(n1, X0)), inference(definition_unfolding, [], [f313, f312])).
fof(f313, plain, ! [X0] : (succ(X0) = plus(n1, X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (succ(X0) = plus(n1, X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', succ_plus_1_l)).
fof(f419, plain, (n3 = plus(plus(plus(n0, n1), n1), n1)), inference(definition_unfolding, [], [f393, f312, f312, f312])).
fof(f393, plain, (n3 = succ(succ(succ(n0)))), inference(cnf_transformation, [], [f86])).
fof(f86, plain, (n3 = succ(succ(succ(n0)))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', successor_3)).
fof(f410, plain, ! [X0] : (minus(plus(X0, n1), n1) = X0), inference(definition_unfolding, [], [f323, f322, f312])).
fof(f322, plain, ! [X0] : (minus(X0, n1) = pred(X0)), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ! [X0] : (minus(X0, n1) = pred(X0)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', pred_minus_1)).
fof(f323, plain, ! [X0] : (pred(succ(X0)) = X0), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (pred(succ(X0)) = X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', pred_succ)).
fof(f565, plain, ! [X1] : (~ leq(X1, minus(n3, n1)) | (init = a_select2(s_center7_init, X1)) | ~ leq(n0, X1)), inference(backward_demodulation, [], [f352, f560])).
fof(f560, plain, (n3 = plus(n1, n2)), inference(superposition, [], [f401, f533])).
fof(f352, plain, ! [X1] : ((init = a_select2(s_center7_init, X1)) | ~ leq(X1, minus(plus(n1, n2), n1)) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f231])).
fof(f509, plain, (leq(n0, sK36) | ~ spl37_16), inference(avatar_component_clause, [], [f507])).
fof(f670059, plain, (spl37_6 | ~ spl37_25 | ~ spl37_267), inference(avatar_contradiction_clause, [], [f670058])).
fof(f670058, plain, ($false | (spl37_6 | ~ spl37_25 | ~ spl37_267)), inference(subsumption_resolution, [], [f670006, f669745])).
fof(f669745, plain, (init = a_select3(simplex7_init, n2, n2)), inference(subsumption_resolution, [], [f669662, f235])).
fof(f669662, plain, (~ leq(n2, n2) | (init = a_select3(simplex7_init, n2, n2))), inference(resolution, [], [f1501, f543])).
fof(f1501, plain, ! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (init = a_select3(simplex7_init, n2, X4))), inference(subsumption_resolution, [], [f1483, f548])).
fof(f548, plain, leq(n2, n3), inference(resolution, [], [f237, f380])).
fof(f380, plain, gt(n3, n2), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', gt_3_2)).
fof(f1483, plain, ! [X4] : (~ leq(n2, n3) | (init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(resolution, [], [f350, f543])).
fof(f670006, plain, (~ (init = a_select3(simplex7_init, n2, n2)) | (spl37_6 | ~ spl37_25 | ~ spl37_267)), inference(backward_demodulation, [], [f669776, f871])).
fof(f669776, plain, (~ (init = a_select3(simplex7_init, n2, sK34)) | (spl37_6 | ~ spl37_267)), inference(forward_demodulation, [], [f462, f55249])).
fof(f55249, plain, ((n2 = sK35) | ~ spl37_267), inference(avatar_component_clause, [], [f55247])).
fof(f669933, plain, (spl37_6 | ~ spl37_21 | ~ spl37_267), inference(avatar_contradiction_clause, [], [f669932])).
fof(f669932, plain, ($false | (spl37_6 | ~ spl37_21 | ~ spl37_267)), inference(subsumption_resolution, [], [f669852, f669746])).
fof(f669746, plain, (init = a_select3(simplex7_init, n2, n0)), inference(subsumption_resolution, [], [f669681, f543])).
fof(f669681, plain, (~ leq(n0, n2) | (init = a_select3(simplex7_init, n2, n0))), inference(resolution, [], [f1501, f235])).
fof(f669852, plain, (~ (init = a_select3(simplex7_init, n2, n0)) | (spl37_6 | ~ spl37_21 | ~ spl37_267)), inference(backward_demodulation, [], [f669776, f853])).
fof(f669763, plain, (spl37_6 | ~ spl37_259 | ~ spl37_267), inference(avatar_contradiction_clause, [], [f669762])).
fof(f669762, plain, ($false | (spl37_6 | ~ spl37_259 | ~ spl37_267)), inference(subsumption_resolution, [], [f669761, f55714])).
fof(f55714, plain, (~ (init = a_select3(simplex7_init, n2, n1)) | (spl37_6 | ~ spl37_259 | ~ spl37_267)), inference(backward_demodulation, [], [f55139, f55249])).
fof(f55139, plain, (~ (init = a_select3(simplex7_init, sK35, n1)) | (spl37_6 | ~ spl37_259)), inference(forward_demodulation, [], [f462, f51393])).
fof(f669761, plain, (init = a_select3(simplex7_init, n2, n1)), inference(forward_demodulation, [], [f669760, f417])).
fof(f669760, plain, (init = a_select3(simplex7_init, n2, plus(n0, n1))), inference(subsumption_resolution, [], [f669759, f544])).
fof(f669759, plain, (~ leq(n1, n2) | (init = a_select3(simplex7_init, n2, plus(n0, n1)))), inference(forward_demodulation, [], [f669709, f417])).
fof(f669709, plain, (~ leq(plus(n0, n1), n2) | (init = a_select3(simplex7_init, n2, plus(n0, n1)))), inference(resolution, [], [f1501, f541])).
fof(f51430, plain, (spl37_25 | spl37_21 | spl37_259 | ~ spl37_9 | ~ spl37_10), inference(avatar_split_clause, [], [f51419, f480, f475, f51391, f851, f869])).
fof(f51419, plain, ((n1 = sK34) | (n0 = sK34) | (n2 = sK34) | (~ spl37_9 | ~ spl37_10)), inference(subsumption_resolution, [], [f51357, f477])).
fof(f51357, plain, ((n1 = sK34) | (n0 = sK34) | ~ leq(sK34, n2) | (n2 = sK34) | ~ spl37_10), inference(resolution, [], [f482, f387])).
fof(f387, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f159])).
fof(f159, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV042+1.p', finite_domain_2)).
fof(f520, plain, (~ spl37_17 | spl37_12), inference(avatar_split_clause, [], [f353, f489, f512])).
fof(f512, plain, (spl37_17 <=> gt(loopcounter, n1)), introduced(avatar_definition, [new_symbols(naming, [spl37_17])])).
fof(f489, plain, (spl37_12 <=> (init = pvar1400_init)), introduced(avatar_definition, [new_symbols(naming, [spl37_12])])).
fof(f353, plain, ((init = pvar1400_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f231])).
fof(f519, plain, (~ spl37_17 | spl37_13), inference(avatar_split_clause, [], [f354, f493, f512])).
fof(f493, plain, (spl37_13 <=> (init = pvar1401_init)), introduced(avatar_definition, [new_symbols(naming, [spl37_13])])).
fof(f354, plain, ((init = pvar1401_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f231])).
fof(f518, plain, (~ spl37_17 | spl37_14), inference(avatar_split_clause, [], [f355, f497, f512])).
fof(f497, plain, (spl37_14 <=> (init = pvar1402_init)), introduced(avatar_definition, [new_symbols(naming, [spl37_14])])).
fof(f355, plain, ((init = pvar1402_init) | ~ gt(loopcounter, n1)), inference(cnf_transformation, [], [f231])).
fof(f517, plain, (spl37_5 | spl37_1 | spl37_16 | spl37_17), inference(avatar_split_clause, [], [f356, f512, f507, f437, f456])).
fof(f456, plain, (spl37_5 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl37_5])])).
fof(f437, plain, (spl37_1 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl37_1])])).
fof(f356, plain, (gt(loopcounter, n1) | leq(n0, sK36) | sP5 | sP4), inference(cnf_transformation, [], [f231])).
fof(f516, plain, (spl37_5 | spl37_1 | spl37_15 | spl37_17), inference(avatar_split_clause, [], [f357, f512, f502, f437, f456])).
fof(f357, plain, (gt(loopcounter, n1) | leq(sK36, n2) | sP5 | sP4), inference(cnf_transformation, [], [f231])).
fof(f515, plain, (spl37_5 | spl37_1 | ~ spl37_11 | spl37_17), inference(avatar_split_clause, [], [f358, f512, f485, f437, f456])).
fof(f358, plain, (gt(loopcounter, n1) | ~ (init = a_select2(s_center7_init, sK36)) | sP5 | sP4), inference(cnf_transformation, [], [f231])).
fof(f510, plain, (spl37_5 | spl37_1 | spl37_16 | ~ spl37_12 | ~ spl37_13 | ~ spl37_14), inference(avatar_split_clause, [], [f359, f497, f493, f489, f507, f437, f456])).
fof(f359, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(n0, sK36) | sP5 | sP4), inference(cnf_transformation, [], [f231])).
fof(f505, plain, (spl37_5 | spl37_1 | spl37_15 | ~ spl37_12 | ~ spl37_13 | ~ spl37_14), inference(avatar_split_clause, [], [f360, f497, f493, f489, f502, f437, f456])).
fof(f360, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | leq(sK36, n2) | sP5 | sP4), inference(cnf_transformation, [], [f231])).
fof(f500, plain, (spl37_5 | spl37_1 | ~ spl37_11 | ~ spl37_12 | ~ spl37_13 | ~ spl37_14), inference(avatar_split_clause, [], [f361, f497, f493, f489, f485, f437, f456])).
fof(f361, plain, (~ (init = pvar1402_init) | ~ (init = pvar1401_init) | ~ (init = pvar1400_init) | ~ (init = a_select2(s_center7_init, sK36)) | sP5 | sP4), inference(cnf_transformation, [], [f231])).
fof(f483, plain, (~ spl37_5 | spl37_10), inference(avatar_split_clause, [], [f345, f480, f456])).
fof(f345, plain, (leq(n0, sK34) | ~ sP4), inference(cnf_transformation, [], [f228])).
fof(f228, plain, (((~ (init = a_select3(simplex7_init, sK35, sK34)) & leq(sK35, n3) & leq(n0, sK35)) & leq(sK34, n2) & leq(n0, sK34)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34, sK35])], [f225, f227, f226])).
fof(f226, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK34)) & leq(X1, n3) & leq(n0, X1)) & leq(sK34, n2) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f227, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK34)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK35, sK34)) & leq(sK35, n3) & leq(n0, sK35))), introduced(choice_axiom, [])).
fof(f225, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f224])).
fof(f224, plain, (? [X6] : (? [X7] : (~ (init = a_select3(simplex7_init, X7, X6)) & leq(X7, n3) & leq(n0, X7)) & leq(X6, n2) & leq(n0, X6)) | ~ sP4), inference(nnf_transformation, [], [f170])).
fof(f478, plain, (~ spl37_5 | spl37_9), inference(avatar_split_clause, [], [f346, f475, f456])).
fof(f346, plain, (leq(sK34, n2) | ~ sP4), inference(cnf_transformation, [], [f228])).
fof(f473, plain, (~ spl37_5 | spl37_8), inference(avatar_split_clause, [], [f347, f470, f456])).
fof(f347, plain, (leq(n0, sK35) | ~ sP4), inference(cnf_transformation, [], [f228])).
fof(f468, plain, (~ spl37_5 | spl37_7), inference(avatar_split_clause, [], [f348, f465, f456])).
fof(f348, plain, (leq(sK35, n3) | ~ sP4), inference(cnf_transformation, [], [f228])).
fof(f463, plain, (~ spl37_5 | ~ spl37_6), inference(avatar_split_clause, [], [f349, f460, f456])).
fof(f349, plain, (~ (init = a_select3(simplex7_init, sK35, sK34)) | ~ sP4), inference(cnf_transformation, [], [f228])).
fof(f454, plain, (~ spl37_1 | spl37_4), inference(avatar_split_clause, [], [f342, f451, f437])).
fof(f342, plain, (leq(n0, sK33) | ~ sP5), inference(cnf_transformation, [], [f223])).
fof(f223, plain, ((~ (init = a_select2(s_values7_init, sK33)) & leq(sK33, n3) & leq(n0, sK33)) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK33])], [f221, f222])).
fof(f222, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) => (~ (init = a_select2(s_values7_init, sK33)) & leq(sK33, n3) & leq(n0, sK33))), introduced(choice_axiom, [])).
fof(f221, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, n3) & leq(n0, X0)) | ~ sP5), inference(rectify, [], [f220])).
fof(f220, plain, (? [X5] : (~ (init = a_select2(s_values7_init, X5)) & leq(X5, n3) & leq(n0, X5)) | ~ sP5), inference(nnf_transformation, [], [f171])).
fof(f449, plain, (~ spl37_1 | spl37_3), inference(avatar_split_clause, [], [f343, f446, f437])).
fof(f343, plain, (leq(sK33, n3) | ~ sP5), inference(cnf_transformation, [], [f223])).
fof(f444, plain, (~ spl37_1 | ~ spl37_2), inference(avatar_split_clause, [], [f344, f441, f437])).
fof(f344, plain, (~ (init = a_select2(s_values7_init, sK33)) | ~ sP5), inference(cnf_transformation, [], [f223])).