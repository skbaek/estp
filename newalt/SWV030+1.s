fof(f512950, plain, $false, inference(avatar_sat_refutation, [], [f471, f476, f481, f486, f491, f528, f533, f538, f539, f540, f541, f542, f543, f544, f545, f546, f1562, f1791, f1937, f512695, f512724, f512751, f512770, f512786, f512819, f512837, f512853, f512884, f512902, f512932, f512948])).
fof(f512948, plain, (spl35_2 | ~ spl35_90 | ~ spl35_93), inference(avatar_contradiction_clause, [], [f512947])).
fof(f512947, plain, ($false | (spl35_2 | ~ spl35_90 | ~ spl35_93)), inference(subsumption_resolution, [], [f512945, f512439])).
fof(f512439, plain, (init = a_select3(simplex7_init, n1, n1)), inference(subsumption_resolution, [], [f512324, f580])).
fof(f580, plain, leq(n1, n2), inference(resolution, [], [f247, f396])).
fof(f396, plain, gt(n2, n1), inference(cnf_transformation, [], [f80])).
fof(f80, plain, gt(n2, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', gt_2_1)).
fof(f247, plain, ! [X0, X1] : (~ gt(X1, X0) | leq(X0, X1)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0, X1] : (leq(X0, X1) | ~ gt(X1, X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (gt(X1, X0) => leq(X0, X1)), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', leq_gt1)).
fof(f512324, plain, (~ leq(n1, n2) | (init = a_select3(simplex7_init, n1, n1))), inference(resolution, [], [f1848, f565])).
fof(f565, plain, leq(n0, n1), inference(resolution, [], [f247, f389])).
fof(f389, plain, gt(n1, n0), inference(cnf_transformation, [], [f73])).
fof(f73, plain, gt(n1, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', gt_1_0)).
fof(f1848, plain, ! [X3] : (~ leq(n0, X3) | ~ leq(X3, n2) | (init = a_select3(simplex7_init, n1, X3))), inference(subsumption_resolution, [], [f1825, f581])).
fof(f581, plain, leq(n1, n3), inference(resolution, [], [f247, f397])).
fof(f397, plain, gt(n3, n1), inference(cnf_transformation, [], [f81])).
fof(f81, plain, gt(n3, n1), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', gt_3_1)).
fof(f1825, plain, ! [X3] : (~ leq(n1, n3) | (init = a_select3(simplex7_init, n1, X3)) | ~ leq(X3, n2) | ~ leq(n0, X3)), inference(resolution, [], [f366, f565])).
fof(f366, plain, ! [X2, X3] : (~ leq(n0, X3) | ~ leq(X3, n3) | (init = a_select3(simplex7_init, X3, X2)) | ~ leq(X2, n2) | ~ leq(n0, X2)), inference(cnf_transformation, [], [f241])).
fof(f241, plain, (((~ (init = a_select2(s_values7_init, sK34)) & leq(sK34, minus(pv1376, n1)) & leq(n0, sK34)) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7) | ~ (init = init)) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | ~ leq(X1, minus(pv1376, n1)) | ~ leq(n0, X1)) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK34])], [f239, f240])).
fof(f240, plain, (? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, minus(pv1376, n1)) & leq(n0, X0)) => (~ (init = a_select2(s_values7_init, sK34)) & leq(sK34, minus(pv1376, n1)) & leq(n0, sK34))), introduced(choice_axiom, [])).
fof(f239, plain, ((? [X0] : (~ (init = a_select2(s_values7_init, X0)) & leq(X0, minus(pv1376, n1)) & leq(n0, X0)) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7) | ~ (init = init)) & ! [X1] : ((init = a_select2(s_values7_init, X1)) | ~ leq(X1, minus(pv1376, n1)) | ~ leq(n0, X1)) & ! [X2] : (! [X3] : ((init = a_select3(simplex7_init, X3, X2)) | ~ leq(X3, n3) | ~ leq(n0, X3)) | ~ leq(X2, n2) | ~ leq(n0, X2)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init)), inference(rectify, [], [f186])).
fof(f186, plain, ((? [X3] : (~ (init = a_select2(s_values7_init, X3)) & leq(X3, minus(pv1376, n1)) & leq(n0, X3)) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7) | ~ (init = init)) & ! [X0] : ((init = a_select2(s_values7_init, X0)) | ~ leq(X0, minus(pv1376, n1)) | ~ leq(n0, X0)) & ! [X1] : (! [X2] : ((init = a_select3(simplex7_init, X2, X1)) | ~ leq(X2, n3) | ~ leq(n0, X2)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init)), inference(definition_folding, [], [f165, e185])).
fof(f185, plain, (? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & leq(X5, n3) & leq(n0, X5)) & leq(X4, n2) & leq(n0, X4)) | ~ sP4), inference(usedef, [], [e185])).
fof(e185, plain, (sP4 <=> ? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & leq(X5, n3) & leq(n0, X5)) & leq(X4, n2) & leq(n0, X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f165, plain, ((? [X3] : (~ (init = a_select2(s_values7_init, X3)) & leq(X3, minus(pv1376, n1)) & leq(n0, X3)) | ? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & leq(X5, n3) & leq(n0, X5)) & leq(X4, n2) & leq(n0, X4)) | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7) | ~ (init = init)) & ! [X0] : ((init = a_select2(s_values7_init, X0)) | ~ leq(X0, minus(pv1376, n1)) | ~ leq(n0, X0)) & ! [X1] : (! [X2] : ((init = a_select3(simplex7_init, X2, X1)) | ~ leq(X2, n3) | ~ leq(n0, X2)) | ~ leq(X1, n2) | ~ leq(n0, X1)) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init)), inference(flattening, [], [f164])).
fof(f164, plain, ((? [X3] : (~ (init = a_select2(s_values7_init, X3)) & (leq(X3, minus(pv1376, n1)) & leq(n0, X3))) | ? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & (leq(X5, n3) & leq(n0, X5))) & (leq(X4, n2) & leq(n0, X4))) | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7) | ~ (init = init)) & (! [X0] : ((init = a_select2(s_values7_init, X0)) | (~ leq(X0, minus(pv1376, n1)) | ~ leq(n0, X0))) & ! [X1] : (! [X2] : ((init = a_select3(simplex7_init, X2, X1)) | (~ leq(X2, n3) | ~ leq(n0, X2))) | (~ leq(X1, n2) | ~ leq(n0, X1))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init))), inference(ennf_transformation, [], [f122])).
fof(f122, plain, ~ ((! [X0] : ((leq(X0, minus(pv1376, n1)) & leq(n0, X0)) => (init = a_select2(s_values7_init, X0))) & ! [X1] : ((leq(X1, n2) & leq(n0, X1)) => ! [X2] : ((leq(X2, n3) & leq(n0, X2)) => (init = a_select3(simplex7_init, X2, X1)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init)) => (! [X3] : ((leq(X3, minus(pv1376, n1)) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X4] : ((leq(X4, n2) & leq(n0, X4)) => ! [X5] : ((leq(X5, n3) & leq(n0, X5)) => (init = a_select3(simplex7_init, X5, X4)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init))), inference(rectify, [], [f54])).
fof(f54, plain, ~ ((! [X3] : ((leq(X3, minus(pv1376, n1)) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init)) => (! [X21] : ((leq(X21, minus(pv1376, n1)) & leq(n0, X21)) => (init = a_select2(s_values7_init, X21))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => ! [X20] : ((leq(X20, n3) & leq(n0, X20)) => (init = a_select3(simplex7_init, X20, X19)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init))), inference(negated_conjecture, [], [f53])).
fof(f53, plain, ~ ((! [X3] : ((leq(X3, minus(pv1376, n1)) & leq(n0, X3)) => (init = a_select2(s_values7_init, X3))) & ! [X13] : ((leq(X13, n2) & leq(n0, X13)) => ! [X17] : ((leq(X17, n3) & leq(n0, X17)) => (init = a_select3(simplex7_init, X17, X13)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init)) => (! [X21] : ((leq(X21, minus(pv1376, n1)) & leq(n0, X21)) => (init = a_select2(s_values7_init, X21))) & ! [X19] : ((leq(X19, n2) & leq(n0, X19)) => ! [X20] : ((leq(X20, n3) & leq(n0, X20)) => (init = a_select3(simplex7_init, X20, X19)))) & leq(pv1376, n3) & leq(pv20, minus(n330, n1)) & leq(pv19, minus(n410, n1)) & leq(pv7, minus(n410, n1)) & leq(n0, pv1376) & leq(n0, pv20) & leq(n0, pv19) & leq(n0, pv7) & (init = init))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', gauss_init_0033)).
fof(f512945, plain, (~ (init = a_select3(simplex7_init, n1, n1)) | (spl35_2 | ~ spl35_90 | ~ spl35_93)), inference(backward_demodulation, [], [f512904, f1593])).
fof(f1593, plain, ((n1 = sK32) | ~ spl35_93), inference(avatar_component_clause, [], [f1591])).
fof(f1591, plain, (spl35_93 <=> (n1 = sK32)), introduced(avatar_definition, [new_symbols(naming, [spl35_93])])).
fof(f512904, plain, (~ (init = a_select3(simplex7_init, n1, sK32)) | (spl35_2 | ~ spl35_90)), inference(backward_demodulation, [], [f470, f1573])).
fof(f1573, plain, ((n1 = sK33) | ~ spl35_90), inference(avatar_component_clause, [], [f1571])).
fof(f1571, plain, (spl35_90 <=> (n1 = sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_90])])).
fof(f470, plain, (~ (init = a_select3(simplex7_init, sK33, sK32)) | spl35_2), inference(avatar_component_clause, [], [f468])).
fof(f468, plain, (spl35_2 <=> (init = a_select3(simplex7_init, sK33, sK32))), introduced(avatar_definition, [new_symbols(naming, [spl35_2])])).
fof(f512932, plain, (spl35_2 | ~ spl35_45 | ~ spl35_90), inference(avatar_contradiction_clause, [], [f512931])).
fof(f512931, plain, ($false | (spl35_2 | ~ spl35_45 | ~ spl35_90)), inference(subsumption_resolution, [], [f512929, f512441])).
fof(f512441, plain, (init = a_select3(simplex7_init, n1, n0)), inference(subsumption_resolution, [], [f512373, f566])).
fof(f566, plain, leq(n0, n2), inference(resolution, [], [f247, f390])).
fof(f390, plain, gt(n2, n0), inference(cnf_transformation, [], [f74])).
fof(f74, plain, gt(n2, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', gt_2_0)).
fof(f512373, plain, (~ leq(n0, n2) | (init = a_select3(simplex7_init, n1, n0))), inference(resolution, [], [f1848, f245])).
fof(f245, plain, ! [X0] : leq(X0, X0), inference(cnf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : leq(X0, X0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', reflexivity_leq)).
fof(f512929, plain, (~ (init = a_select3(simplex7_init, n1, n0)) | (spl35_2 | ~ spl35_45 | ~ spl35_90)), inference(backward_demodulation, [], [f512904, f997])).
fof(f997, plain, ((n0 = sK32) | ~ spl35_45), inference(avatar_component_clause, [], [f995])).
fof(f995, plain, (spl35_45 <=> (n0 = sK32)), introduced(avatar_definition, [new_symbols(naming, [spl35_45])])).
fof(f512902, plain, (spl35_2 | ~ spl35_41 | ~ spl35_93), inference(avatar_contradiction_clause, [], [f512901])).
fof(f512901, plain, ($false | (spl35_2 | ~ spl35_41 | ~ spl35_93)), inference(subsumption_resolution, [], [f512899, f39957])).
fof(f39957, plain, (init = a_select3(simplex7_init, n0, n1)), inference(subsumption_resolution, [], [f39915, f580])).
fof(f39915, plain, (~ leq(n1, n2) | (init = a_select3(simplex7_init, n0, n1))), inference(resolution, [], [f1909, f565])).
fof(f1909, plain, ! [X39] : (~ leq(n0, X39) | ~ leq(X39, n2) | (init = a_select3(simplex7_init, n0, X39))), inference(subsumption_resolution, [], [f1845, f567])).
fof(f567, plain, leq(n0, n3), inference(resolution, [], [f247, f391])).
fof(f391, plain, gt(n3, n0), inference(cnf_transformation, [], [f75])).
fof(f75, plain, gt(n3, n0), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', gt_3_0)).
fof(f1845, plain, ! [X39] : (~ leq(n0, n3) | (init = a_select3(simplex7_init, n0, X39)) | ~ leq(X39, n2) | ~ leq(n0, X39)), inference(resolution, [], [f366, f245])).
fof(f512899, plain, (~ (init = a_select3(simplex7_init, n0, n1)) | (spl35_2 | ~ spl35_41 | ~ spl35_93)), inference(backward_demodulation, [], [f512888, f1593])).
fof(f512888, plain, (~ (init = a_select3(simplex7_init, n0, sK32)) | (spl35_2 | ~ spl35_41)), inference(forward_demodulation, [], [f470, f975])).
fof(f975, plain, ((n0 = sK33) | ~ spl35_41), inference(avatar_component_clause, [], [f973])).
fof(f973, plain, (spl35_41 <=> (n0 = sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_41])])).
fof(f512884, plain, (spl35_2 | ~ spl35_41 | ~ spl35_45), inference(avatar_contradiction_clause, [], [f512883])).
fof(f512883, plain, ($false | (spl35_2 | ~ spl35_41 | ~ spl35_45)), inference(subsumption_resolution, [], [f512881, f39959])).
fof(f39959, plain, (init = a_select3(simplex7_init, n0, n0)), inference(subsumption_resolution, [], [f39936, f566])).
fof(f39936, plain, (~ leq(n0, n2) | (init = a_select3(simplex7_init, n0, n0))), inference(resolution, [], [f1909, f245])).
fof(f512881, plain, (~ (init = a_select3(simplex7_init, n0, n0)) | (spl35_2 | ~ spl35_41 | ~ spl35_45)), inference(backward_demodulation, [], [f512855, f975])).
fof(f512855, plain, (~ (init = a_select3(simplex7_init, sK33, n0)) | (spl35_2 | ~ spl35_45)), inference(backward_demodulation, [], [f470, f997])).
fof(f512853, plain, (spl35_2 | ~ spl35_39 | ~ spl35_43), inference(avatar_contradiction_clause, [], [f512852])).
fof(f512852, plain, ($false | (spl35_2 | ~ spl35_39 | ~ spl35_43)), inference(subsumption_resolution, [], [f512850, f512692])).
fof(f512692, plain, (init = a_select3(simplex7_init, n3, n2)), inference(subsumption_resolution, [], [f512601, f245])).
fof(f512601, plain, (~ leq(n2, n2) | (init = a_select3(simplex7_init, n3, n2))), inference(resolution, [], [f1850, f566])).
fof(f1850, plain, ! [X5] : (~ leq(n0, X5) | ~ leq(X5, n2) | (init = a_select3(simplex7_init, n3, X5))), inference(subsumption_resolution, [], [f1827, f245])).
fof(f1827, plain, ! [X5] : (~ leq(n3, n3) | (init = a_select3(simplex7_init, n3, X5)) | ~ leq(X5, n2) | ~ leq(n0, X5)), inference(resolution, [], [f366, f567])).
fof(f512850, plain, (~ (init = a_select3(simplex7_init, n3, n2)) | (spl35_2 | ~ spl35_39 | ~ spl35_43)), inference(backward_demodulation, [], [f512790, f962])).
fof(f962, plain, ((n3 = sK33) | ~ spl35_39), inference(avatar_component_clause, [], [f960])).
fof(f960, plain, (spl35_39 <=> (n3 = sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_39])])).
fof(f512790, plain, (~ (init = a_select3(simplex7_init, sK33, n2)) | (spl35_2 | ~ spl35_43)), inference(backward_demodulation, [], [f470, f986])).
fof(f986, plain, ((n2 = sK32) | ~ spl35_43), inference(avatar_component_clause, [], [f984])).
fof(f984, plain, (spl35_43 <=> (n2 = sK32)), introduced(avatar_definition, [new_symbols(naming, [spl35_43])])).
fof(f512837, plain, (spl35_2 | ~ spl35_43 | ~ spl35_90), inference(avatar_contradiction_clause, [], [f512836])).
fof(f512836, plain, ($false | (spl35_2 | ~ spl35_43 | ~ spl35_90)), inference(subsumption_resolution, [], [f512834, f512440])).
fof(f512440, plain, (init = a_select3(simplex7_init, n1, n2)), inference(subsumption_resolution, [], [f512349, f245])).
fof(f512349, plain, (~ leq(n2, n2) | (init = a_select3(simplex7_init, n1, n2))), inference(resolution, [], [f1848, f566])).
fof(f512834, plain, (~ (init = a_select3(simplex7_init, n1, n2)) | (spl35_2 | ~ spl35_43 | ~ spl35_90)), inference(backward_demodulation, [], [f512790, f1573])).
fof(f512819, plain, (spl35_2 | ~ spl35_41 | ~ spl35_43), inference(avatar_contradiction_clause, [], [f512818])).
fof(f512818, plain, ($false | (spl35_2 | ~ spl35_41 | ~ spl35_43)), inference(subsumption_resolution, [], [f512816, f39958])).
fof(f39958, plain, (init = a_select3(simplex7_init, n0, n2)), inference(subsumption_resolution, [], [f39921, f245])).
fof(f39921, plain, (~ leq(n2, n2) | (init = a_select3(simplex7_init, n0, n2))), inference(resolution, [], [f1909, f566])).
fof(f512816, plain, (~ (init = a_select3(simplex7_init, n0, n2)) | (spl35_2 | ~ spl35_41 | ~ spl35_43)), inference(backward_demodulation, [], [f512790, f975])).
fof(f512786, plain, (spl35_2 | ~ spl35_93 | ~ spl35_115), inference(avatar_contradiction_clause, [], [f512785])).
fof(f512785, plain, ($false | (spl35_2 | ~ spl35_93 | ~ spl35_115)), inference(subsumption_resolution, [], [f512781, f512565])).
fof(f512565, plain, (init = a_select3(simplex7_init, n2, n1)), inference(subsumption_resolution, [], [f512450, f580])).
fof(f512450, plain, (~ leq(n1, n2) | (init = a_select3(simplex7_init, n2, n1))), inference(resolution, [], [f1849, f565])).
fof(f1849, plain, ! [X4] : (~ leq(n0, X4) | ~ leq(X4, n2) | (init = a_select3(simplex7_init, n2, X4))), inference(subsumption_resolution, [], [f1826, f586])).
fof(f586, plain, leq(n2, n3), inference(resolution, [], [f247, f402])).
fof(f402, plain, gt(n3, n2), inference(cnf_transformation, [], [f86])).
fof(f86, plain, gt(n3, n2), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', gt_3_2)).
fof(f1826, plain, ! [X4] : (~ leq(n2, n3) | (init = a_select3(simplex7_init, n2, X4)) | ~ leq(X4, n2) | ~ leq(n0, X4)), inference(resolution, [], [f366, f566])).
fof(f512781, plain, (~ (init = a_select3(simplex7_init, n2, n1)) | (spl35_2 | ~ spl35_93 | ~ spl35_115)), inference(backward_demodulation, [], [f512726, f1593])).
fof(f512726, plain, (~ (init = a_select3(simplex7_init, n2, sK32)) | (spl35_2 | ~ spl35_115)), inference(backward_demodulation, [], [f470, f1795])).
fof(f1795, plain, ((n2 = sK33) | ~ spl35_115), inference(avatar_component_clause, [], [f1793])).
fof(f1793, plain, (spl35_115 <=> (n2 = sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_115])])).
fof(f512770, plain, (spl35_2 | ~ spl35_43 | ~ spl35_115), inference(avatar_contradiction_clause, [], [f512769])).
fof(f512769, plain, ($false | (spl35_2 | ~ spl35_43 | ~ spl35_115)), inference(subsumption_resolution, [], [f512765, f512566])).
fof(f512566, plain, (init = a_select3(simplex7_init, n2, n2)), inference(subsumption_resolution, [], [f512475, f245])).
fof(f512475, plain, (~ leq(n2, n2) | (init = a_select3(simplex7_init, n2, n2))), inference(resolution, [], [f1849, f566])).
fof(f512765, plain, (~ (init = a_select3(simplex7_init, n2, n2)) | (spl35_2 | ~ spl35_43 | ~ spl35_115)), inference(backward_demodulation, [], [f512726, f986])).
fof(f512751, plain, (spl35_2 | ~ spl35_45 | ~ spl35_115), inference(avatar_contradiction_clause, [], [f512750])).
fof(f512750, plain, ($false | (spl35_2 | ~ spl35_45 | ~ spl35_115)), inference(subsumption_resolution, [], [f512748, f512567])).
fof(f512567, plain, (init = a_select3(simplex7_init, n2, n0)), inference(subsumption_resolution, [], [f512499, f566])).
fof(f512499, plain, (~ leq(n0, n2) | (init = a_select3(simplex7_init, n2, n0))), inference(resolution, [], [f1849, f245])).
fof(f512748, plain, (~ (init = a_select3(simplex7_init, n2, n0)) | (spl35_2 | ~ spl35_45 | ~ spl35_115)), inference(backward_demodulation, [], [f512726, f997])).
fof(f512724, plain, (spl35_2 | ~ spl35_39 | ~ spl35_93), inference(avatar_contradiction_clause, [], [f512723])).
fof(f512723, plain, ($false | (spl35_2 | ~ spl35_39 | ~ spl35_93)), inference(subsumption_resolution, [], [f512721, f512691])).
fof(f512691, plain, (init = a_select3(simplex7_init, n3, n1)), inference(subsumption_resolution, [], [f512576, f580])).
fof(f512576, plain, (~ leq(n1, n2) | (init = a_select3(simplex7_init, n3, n1))), inference(resolution, [], [f1850, f565])).
fof(f512721, plain, (~ (init = a_select3(simplex7_init, n3, n1)) | (spl35_2 | ~ spl35_39 | ~ spl35_93)), inference(backward_demodulation, [], [f512711, f1593])).
fof(f512711, plain, (~ (init = a_select3(simplex7_init, n3, sK32)) | (spl35_2 | ~ spl35_39)), inference(forward_demodulation, [], [f470, f962])).
fof(f512695, plain, (spl35_2 | ~ spl35_39 | ~ spl35_45), inference(avatar_contradiction_clause, [], [f512694])).
fof(f512694, plain, ($false | (spl35_2 | ~ spl35_39 | ~ spl35_45)), inference(subsumption_resolution, [], [f512693, f17747])).
fof(f17747, plain, (~ (init = a_select3(simplex7_init, n3, n0)) | (spl35_2 | ~ spl35_39 | ~ spl35_45)), inference(backward_demodulation, [], [f1801, f962])).
fof(f1801, plain, (~ (init = a_select3(simplex7_init, sK33, n0)) | (spl35_2 | ~ spl35_45)), inference(backward_demodulation, [], [f470, f997])).
fof(f512693, plain, (init = a_select3(simplex7_init, n3, n0)), inference(subsumption_resolution, [], [f512625, f566])).
fof(f512625, plain, (~ leq(n0, n2) | (init = a_select3(simplex7_init, n3, n0))), inference(resolution, [], [f1850, f245])).
fof(f1937, plain, (spl35_39 | spl35_41 | spl35_90 | spl35_115 | ~ spl35_3 | ~ spl35_4), inference(avatar_split_clause, [], [f1936, f478, f473, f1793, f1571, f973, f960])).
fof(f473, plain, (spl35_3 <=> leq(sK33, n3)), introduced(avatar_definition, [new_symbols(naming, [spl35_3])])).
fof(f478, plain, (spl35_4 <=> leq(n0, sK33)), introduced(avatar_definition, [new_symbols(naming, [spl35_4])])).
fof(f1936, plain, ((n2 = sK33) | (n1 = sK33) | (n0 = sK33) | (n3 = sK33) | (~ spl35_3 | ~ spl35_4)), inference(subsumption_resolution, [], [f1930, f475])).
fof(f475, plain, (leq(sK33, n3) | ~ spl35_3), inference(avatar_component_clause, [], [f473])).
fof(f1930, plain, ((n2 = sK33) | (n1 = sK33) | (n0 = sK33) | ~ leq(sK33, n3) | (n3 = sK33) | ~ spl35_4), inference(resolution, [], [f412, f480])).
fof(f480, plain, (leq(n0, sK33) | ~ spl35_4), inference(avatar_component_clause, [], [f478])).
fof(f412, plain, ! [X0] : (~ leq(n0, X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | (n3 = X0)), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n3) | ~ leq(n0, X0)), inference(flattening, [], [f176])).
fof(f176, plain, ! [X0] : (((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n3) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f96])).
fof(f96, plain, ! [X0] : ((leq(X0, n3) & leq(n0, X0)) => ((n3 = X0) | (n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', finite_domain_3)).
fof(f1791, plain, (spl35_43 | spl35_45 | spl35_93 | ~ spl35_5 | ~ spl35_6), inference(avatar_split_clause, [], [f1790, f488, f483, f1591, f995, f984])).
fof(f483, plain, (spl35_5 <=> leq(sK32, n2)), introduced(avatar_definition, [new_symbols(naming, [spl35_5])])).
fof(f488, plain, (spl35_6 <=> leq(n0, sK32)), introduced(avatar_definition, [new_symbols(naming, [spl35_6])])).
fof(f1790, plain, ((n1 = sK32) | (n0 = sK32) | (n2 = sK32) | (~ spl35_5 | ~ spl35_6)), inference(subsumption_resolution, [], [f1704, f485])).
fof(f485, plain, (leq(sK32, n2) | ~ spl35_5), inference(avatar_component_clause, [], [f483])).
fof(f1704, plain, ((n1 = sK32) | (n0 = sK32) | ~ leq(sK32, n2) | (n2 = sK32) | ~ spl35_6), inference(resolution, [], [f411, f490])).
fof(f490, plain, (leq(n0, sK32) | ~ spl35_6), inference(avatar_component_clause, [], [f488])).
fof(f411, plain, ! [X0] : (~ leq(n0, X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | (n2 = X0)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : ((n2 = X0) | (n1 = X0) | (n0 = X0) | ~ leq(X0, n2) | ~ leq(n0, X0)), inference(flattening, [], [f174])).
fof(f174, plain, ! [X0] : (((n2 = X0) | (n1 = X0) | (n0 = X0)) | (~ leq(X0, n2) | ~ leq(n0, X0))), inference(ennf_transformation, [], [f95])).
fof(f95, plain, ! [X0] : ((leq(X0, n2) & leq(n0, X0)) => ((n2 = X0) | (n1 = X0) | (n0 = X0))), file('/home/ubuntu/library/tptp/Problems/SWV/SWV030+1.p', finite_domain_2)).
fof(f1562, plain, (~ spl35_17 | spl35_15 | ~ spl35_16), inference(avatar_split_clause, [], [f1559, f530, f525, f535])).
fof(f535, plain, (spl35_17 <=> leq(n0, sK34)), introduced(avatar_definition, [new_symbols(naming, [spl35_17])])).
fof(f525, plain, (spl35_15 <=> (init = a_select2(s_values7_init, sK34))), introduced(avatar_definition, [new_symbols(naming, [spl35_15])])).
fof(f530, plain, (spl35_16 <=> leq(sK34, minus(pv1376, n1))), introduced(avatar_definition, [new_symbols(naming, [spl35_16])])).
fof(f1559, plain, (~ leq(n0, sK34) | (spl35_15 | ~ spl35_16)), inference(subsumption_resolution, [], [f1536, f527])).
fof(f527, plain, (~ (init = a_select2(s_values7_init, sK34)) | spl35_15), inference(avatar_component_clause, [], [f525])).
fof(f1536, plain, ((init = a_select2(s_values7_init, sK34)) | ~ leq(n0, sK34) | ~ spl35_16), inference(resolution, [], [f367, f532])).
fof(f532, plain, (leq(sK34, minus(pv1376, n1)) | ~ spl35_16), inference(avatar_component_clause, [], [f530])).
fof(f367, plain, ! [X1] : (~ leq(X1, minus(pv1376, n1)) | (init = a_select2(s_values7_init, X1)) | ~ leq(n0, X1)), inference(cnf_transformation, [], [f241])).
fof(f546, plain, spl35_7, inference(avatar_split_clause, [], [f358, f493])).
fof(f493, plain, (spl35_7 <=> leq(n0, pv7)), introduced(avatar_definition, [new_symbols(naming, [spl35_7])])).
fof(f358, plain, leq(n0, pv7), inference(cnf_transformation, [], [f241])).
fof(f545, plain, spl35_8, inference(avatar_split_clause, [], [f359, f497])).
fof(f497, plain, (spl35_8 <=> leq(n0, pv19)), introduced(avatar_definition, [new_symbols(naming, [spl35_8])])).
fof(f359, plain, leq(n0, pv19), inference(cnf_transformation, [], [f241])).
fof(f544, plain, spl35_9, inference(avatar_split_clause, [], [f360, f501])).
fof(f501, plain, (spl35_9 <=> leq(n0, pv20)), introduced(avatar_definition, [new_symbols(naming, [spl35_9])])).
fof(f360, plain, leq(n0, pv20), inference(cnf_transformation, [], [f241])).
fof(f543, plain, spl35_10, inference(avatar_split_clause, [], [f361, f505])).
fof(f505, plain, (spl35_10 <=> leq(n0, pv1376)), introduced(avatar_definition, [new_symbols(naming, [spl35_10])])).
fof(f361, plain, leq(n0, pv1376), inference(cnf_transformation, [], [f241])).
fof(f542, plain, spl35_11, inference(avatar_split_clause, [], [f362, f509])).
fof(f509, plain, (spl35_11 <=> leq(pv7, minus(n410, n1))), introduced(avatar_definition, [new_symbols(naming, [spl35_11])])).
fof(f362, plain, leq(pv7, minus(n410, n1)), inference(cnf_transformation, [], [f241])).
fof(f541, plain, spl35_12, inference(avatar_split_clause, [], [f363, f513])).
fof(f513, plain, (spl35_12 <=> leq(pv19, minus(n410, n1))), introduced(avatar_definition, [new_symbols(naming, [spl35_12])])).
fof(f363, plain, leq(pv19, minus(n410, n1)), inference(cnf_transformation, [], [f241])).
fof(f540, plain, spl35_13, inference(avatar_split_clause, [], [f364, f517])).
fof(f517, plain, (spl35_13 <=> leq(pv20, minus(n330, n1))), introduced(avatar_definition, [new_symbols(naming, [spl35_13])])).
fof(f364, plain, leq(pv20, minus(n330, n1)), inference(cnf_transformation, [], [f241])).
fof(f539, plain, spl35_14, inference(avatar_split_clause, [], [f365, f521])).
fof(f521, plain, (spl35_14 <=> leq(pv1376, n3)), introduced(avatar_definition, [new_symbols(naming, [spl35_14])])).
fof(f365, plain, leq(pv1376, n3), inference(cnf_transformation, [], [f241])).
fof(f538, plain, (~ spl35_7 | ~ spl35_8 | ~ spl35_9 | ~ spl35_10 | ~ spl35_11 | ~ spl35_12 | ~ spl35_13 | ~ spl35_14 | spl35_1 | spl35_17), inference(avatar_split_clause, [], [f447, f535, f464, f521, f517, f513, f509, f505, f501, f497, f493])).
fof(f464, plain, (spl35_1 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl35_1])])).
fof(f447, plain, (leq(n0, sK34) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7)), inference(trivial_inequality_removal, [], [f368])).
fof(f368, plain, (leq(n0, sK34) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7) | ~ (init = init)), inference(cnf_transformation, [], [f241])).
fof(f533, plain, (~ spl35_7 | ~ spl35_8 | ~ spl35_9 | ~ spl35_10 | ~ spl35_11 | ~ spl35_12 | ~ spl35_13 | ~ spl35_14 | spl35_1 | spl35_16), inference(avatar_split_clause, [], [f448, f530, f464, f521, f517, f513, f509, f505, f501, f497, f493])).
fof(f448, plain, (leq(sK34, minus(pv1376, n1)) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7)), inference(trivial_inequality_removal, [], [f369])).
fof(f369, plain, (leq(sK34, minus(pv1376, n1)) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7) | ~ (init = init)), inference(cnf_transformation, [], [f241])).
fof(f528, plain, (~ spl35_7 | ~ spl35_8 | ~ spl35_9 | ~ spl35_10 | ~ spl35_11 | ~ spl35_12 | ~ spl35_13 | ~ spl35_14 | spl35_1 | ~ spl35_15), inference(avatar_split_clause, [], [f449, f525, f464, f521, f517, f513, f509, f505, f501, f497, f493])).
fof(f449, plain, (~ (init = a_select2(s_values7_init, sK34)) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7)), inference(trivial_inequality_removal, [], [f370])).
fof(f370, plain, (~ (init = a_select2(s_values7_init, sK34)) | sP4 | ~ leq(pv1376, n3) | ~ leq(pv20, minus(n330, n1)) | ~ leq(pv19, minus(n410, n1)) | ~ leq(pv7, minus(n410, n1)) | ~ leq(n0, pv1376) | ~ leq(n0, pv20) | ~ leq(n0, pv19) | ~ leq(n0, pv7) | ~ (init = init)), inference(cnf_transformation, [], [f241])).
fof(f491, plain, (~ spl35_1 | spl35_6), inference(avatar_split_clause, [], [f352, f488, f464])).
fof(f352, plain, (leq(n0, sK32) | ~ sP4), inference(cnf_transformation, [], [f238])).
fof(f238, plain, (((~ (init = a_select3(simplex7_init, sK33, sK32)) & leq(sK33, n3) & leq(n0, sK33)) & leq(sK32, n2) & leq(n0, sK32)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK32, sK33])], [f235, f237, f236])).
fof(f236, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) => (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK32)) & leq(X1, n3) & leq(n0, X1)) & leq(sK32, n2) & leq(n0, sK32))), introduced(choice_axiom, [])).
fof(f237, plain, (? [X1] : (~ (init = a_select3(simplex7_init, X1, sK32)) & leq(X1, n3) & leq(n0, X1)) => (~ (init = a_select3(simplex7_init, sK33, sK32)) & leq(sK33, n3) & leq(n0, sK33))), introduced(choice_axiom, [])).
fof(f235, plain, (? [X0] : (? [X1] : (~ (init = a_select3(simplex7_init, X1, X0)) & leq(X1, n3) & leq(n0, X1)) & leq(X0, n2) & leq(n0, X0)) | ~ sP4), inference(rectify, [], [f234])).
fof(f234, plain, (? [X4] : (? [X5] : (~ (init = a_select3(simplex7_init, X5, X4)) & leq(X5, n3) & leq(n0, X5)) & leq(X4, n2) & leq(n0, X4)) | ~ sP4), inference(nnf_transformation, [], [f185])).
fof(f486, plain, (~ spl35_1 | spl35_5), inference(avatar_split_clause, [], [f353, f483, f464])).
fof(f353, plain, (leq(sK32, n2) | ~ sP4), inference(cnf_transformation, [], [f238])).
fof(f481, plain, (~ spl35_1 | spl35_4), inference(avatar_split_clause, [], [f354, f478, f464])).
fof(f354, plain, (leq(n0, sK33) | ~ sP4), inference(cnf_transformation, [], [f238])).
fof(f476, plain, (~ spl35_1 | spl35_3), inference(avatar_split_clause, [], [f355, f473, f464])).
fof(f355, plain, (leq(sK33, n3) | ~ sP4), inference(cnf_transformation, [], [f238])).
fof(f471, plain, (~ spl35_1 | ~ spl35_2), inference(avatar_split_clause, [], [f356, f468, f464])).
fof(f356, plain, (~ (init = a_select3(simplex7_init, sK33, sK32)) | ~ sP4), inference(cnf_transformation, [], [f238])).