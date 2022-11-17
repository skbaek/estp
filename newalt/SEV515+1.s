fof(f392, plain, $false, inference(avatar_sat_refutation, [], [f80, f88, f92, f100, f123, f286, f311, f322, f334, f339, f350, f351, f364, f389])).
fof(f389, plain, (spl4_18 | ~ spl4_20), inference(avatar_contradiction_clause, [], [f388])).
fof(f388, plain, ($false | (spl4_18 | ~ spl4_20)), inference(subsumption_resolution, [], [f385, f305])).
fof(f305, plain, (~ g_true_only(sK1(sK3), sK3) | spl4_18), inference(avatar_component_clause, [], [f304])).
fof(f304, plain, (spl4_18 <=> g_true_only(sK1(sK3), sK3)), introduced(avatar_definition, [new_symbols(naming, [spl4_18])])).
fof(f385, plain, (g_true_only(sK1(sK3), sK3) | ~ spl4_20), inference(resolution, [], [f363, f165])).
fof(f165, plain, ! [X2] : (~ g_false_only(X2, X2) | g_true_only(X2, sK3)), inference(subsumption_resolution, [], [f159, f39])).
fof(f39, plain, ! [X3] : (~ g_false_only(X3, X3) | ~ g_both(X3, sK3)), inference(cnf_transformation, [], [f22])).
fof(f22, plain, ((! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, sK2)) | (g_false_only(X1, X1) & g_true_only(X1, sK2))) | sP0) & ! [X3] : (((~ g_both(X3, X3) & ~ g_false_only(X3, X3)) | ~ g_false_only(X3, sK3)) & ((~ g_true_only(X3, X3) & ~ g_false_only(X3, X3)) | ~ g_both(X3, sK3)) & ((~ g_true_only(X3, X3) & ~ g_both(X3, X3)) | ~ g_true_only(X3, sK3)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3])], [f19, f21, f20])).
fof(f20, plain, (? [X0] : ! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, X0)) | (g_false_only(X1, X1) & g_true_only(X1, X0))) => ! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, sK2)) | (g_false_only(X1, X1) & g_true_only(X1, sK2)))), introduced(choice_axiom, [])).
fof(f21, plain, (? [X2] : ! [X3] : (((~ g_both(X3, X3) & ~ g_false_only(X3, X3)) | ~ g_false_only(X3, X2)) & ((~ g_true_only(X3, X3) & ~ g_false_only(X3, X3)) | ~ g_both(X3, X2)) & ((~ g_true_only(X3, X3) & ~ g_both(X3, X3)) | ~ g_true_only(X3, X2))) => ! [X3] : (((~ g_both(X3, X3) & ~ g_false_only(X3, X3)) | ~ g_false_only(X3, sK3)) & ((~ g_true_only(X3, X3) & ~ g_false_only(X3, X3)) | ~ g_both(X3, sK3)) & ((~ g_true_only(X3, X3) & ~ g_both(X3, X3)) | ~ g_true_only(X3, sK3)))), introduced(choice_axiom, [])).
fof(f19, plain, ((? [X0] : ! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, X0)) | (g_false_only(X1, X1) & g_true_only(X1, X0))) | sP0) & ? [X2] : ! [X3] : (((~ g_both(X3, X3) & ~ g_false_only(X3, X3)) | ~ g_false_only(X3, X2)) & ((~ g_true_only(X3, X3) & ~ g_false_only(X3, X3)) | ~ g_both(X3, X2)) & ((~ g_true_only(X3, X3) & ~ g_both(X3, X3)) | ~ g_true_only(X3, X2)))), inference(rectify, [], [f14])).
fof(f14, plain, ((? [X0] : ! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, X0)) | (g_false_only(X1, X1) & g_true_only(X1, X0))) | sP0) & ? [X5] : ! [X6] : (((~ g_both(X6, X6) & ~ g_false_only(X6, X6)) | ~ g_false_only(X6, X5)) & ((~ g_true_only(X6, X6) & ~ g_false_only(X6, X6)) | ~ g_both(X6, X5)) & ((~ g_true_only(X6, X6) & ~ g_both(X6, X6)) | ~ g_true_only(X6, X5)))), inference(definition_folding, [], [f12, e13])).
fof(f13, plain, (! [X2] : (? [X3] : (((g_both(X3, X3) | g_false_only(X3, X3)) & g_false_only(X3, X2)) | ((g_true_only(X3, X3) | g_false_only(X3, X3)) & g_both(X3, X2)) | ((g_true_only(X3, X3) | g_both(X3, X3)) & g_true_only(X3, X2))) | ! [X4] : (~ g_both(X4, X4) | ~ g_both(X4, X2))) | ~ sP0), inference(usedef, [], [e13])).
fof(e13, plain, (sP0 <=> ! [X2] : (? [X3] : (((g_both(X3, X3) | g_false_only(X3, X3)) & g_false_only(X3, X2)) | ((g_true_only(X3, X3) | g_false_only(X3, X3)) & g_both(X3, X2)) | ((g_true_only(X3, X3) | g_both(X3, X3)) & g_true_only(X3, X2))) | ! [X4] : (~ g_both(X4, X4) | ~ g_both(X4, X2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f12, plain, ((? [X0] : ! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, X0)) | (g_false_only(X1, X1) & g_true_only(X1, X0))) | ! [X2] : (? [X3] : (((g_both(X3, X3) | g_false_only(X3, X3)) & g_false_only(X3, X2)) | ((g_true_only(X3, X3) | g_false_only(X3, X3)) & g_both(X3, X2)) | ((g_true_only(X3, X3) | g_both(X3, X3)) & g_true_only(X3, X2))) | ! [X4] : (~ g_both(X4, X4) | ~ g_both(X4, X2)))) & ? [X5] : ! [X6] : (((~ g_both(X6, X6) & ~ g_false_only(X6, X6)) | ~ g_false_only(X6, X5)) & ((~ g_true_only(X6, X6) & ~ g_false_only(X6, X6)) | ~ g_both(X6, X5)) & ((~ g_true_only(X6, X6) & ~ g_both(X6, X6)) | ~ g_true_only(X6, X5)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((~ ? [X0] : ! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, X0)) | (g_false_only(X1, X1) & g_true_only(X1, X0))) & ? [X2] : (~ ? [X3] : (((g_both(X3, X3) | g_false_only(X3, X3)) & g_false_only(X3, X2)) | ((g_true_only(X3, X3) | g_false_only(X3, X3)) & g_both(X3, X2)) | ((g_true_only(X3, X3) | g_both(X3, X3)) & g_true_only(X3, X2))) & ? [X4] : (g_both(X4, X4) & g_both(X4, X2)))) | ! [X5] : ? [X6] : (((g_both(X6, X6) | g_false_only(X6, X6)) & g_false_only(X6, X5)) | ((g_true_only(X6, X6) | g_false_only(X6, X6)) & g_both(X6, X5)) | ((g_true_only(X6, X6) | g_both(X6, X6)) & g_true_only(X6, X5)))), inference(rectify, [], [f2])).
fof(f2, plain, ~ ((~ ? [X0] : ! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, X0)) | (g_false_only(X1, X1) & g_true_only(X1, X0))) & ? [X0] : (~ ? [X1] : (((g_both(X1, X1) | g_false_only(X1, X1)) & g_false_only(X1, X0)) | ((g_true_only(X1, X1) | g_false_only(X1, X1)) & g_both(X1, X0)) | ((g_true_only(X1, X1) | g_both(X1, X1)) & g_true_only(X1, X0))) & ? [X1] : (g_both(X1, X1) & g_both(X1, X0)))) | ! [X0] : ? [X1] : (((g_both(X1, X1) | g_false_only(X1, X1)) & g_false_only(X1, X0)) | ((g_true_only(X1, X1) | g_false_only(X1, X1)) & g_both(X1, X0)) | ((g_true_only(X1, X1) | g_both(X1, X1)) & g_true_only(X1, X0)))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ((~ ? [X0] : ! [X1] : ((g_true_only(X1, X1) & g_false_only(X1, X0)) | (g_false_only(X1, X1) & g_true_only(X1, X0))) & ? [X0] : (~ ? [X1] : (((g_both(X1, X1) | g_false_only(X1, X1)) & g_false_only(X1, X0)) | ((g_true_only(X1, X1) | g_false_only(X1, X1)) & g_both(X1, X0)) | ((g_true_only(X1, X1) | g_both(X1, X1)) & g_true_only(X1, X0))) & ? [X1] : (g_both(X1, X1) & g_both(X1, X0)))) | ! [X0] : ? [X1] : (((g_both(X1, X1) | g_false_only(X1, X1)) & g_false_only(X1, X0)) | ((g_true_only(X1, X1) | g_false_only(X1, X1)) & g_both(X1, X0)) | ((g_true_only(X1, X1) | g_both(X1, X1)) & g_true_only(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SEV/SEV515+1.p', nc1)).
fof(f159, plain, ! [X2] : (g_both(X2, sK3) | g_true_only(X2, sK3) | ~ g_false_only(X2, X2)), inference(resolution, [], [f56, f41])).
fof(f41, plain, ! [X3] : (~ g_false_only(X3, sK3) | ~ g_false_only(X3, X3)), inference(cnf_transformation, [], [f22])).
fof(f56, plain, ! [X0, X1] : (g_false_only(X0, X1) | g_both(X0, X1) | g_true_only(X0, X1)), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0, X1] : (g_false_only(X0, X1) | g_both(X0, X1) | g_true_only(X0, X1)), inference(rectify, [], [f6])).
fof(f6, plain, ! [X2, X3] : (g_false_only(X2, X3) | g_both(X2, X3) | g_true_only(X2, X3)), file('/home/ubuntu/library/tptp/Problems/SEV/SEV515+1.p', exhaustion_g)).
fof(f363, plain, (g_false_only(sK1(sK3), sK1(sK3)) | ~ spl4_20), inference(avatar_component_clause, [], [f361])).
fof(f361, plain, (spl4_20 <=> g_false_only(sK1(sK3), sK1(sK3))), introduced(avatar_definition, [new_symbols(naming, [spl4_20])])).
fof(f364, plain, (spl4_20 | spl4_16 | ~ spl4_7 | spl4_14 | spl4_18), inference(avatar_split_clause, [], [f359, f304, f269, f86, f277, f361])).
fof(f277, plain, (spl4_16 <=> g_false_only(sK1(sK3), sK3)), introduced(avatar_definition, [new_symbols(naming, [spl4_16])])).
fof(f86, plain, (spl4_7 <=> ! [X0, X2] : (g_false_only(sK1(X0), X0) | ~ g_both(X2, X2) | ~ g_both(X2, X0) | g_true_only(sK1(X0), X0) | g_false_only(sK1(X0), sK1(X0)) | g_true_only(sK1(X0), sK1(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_7])])).
fof(f269, plain, (spl4_14 <=> g_true_only(sK1(sK3), sK1(sK3))), introduced(avatar_definition, [new_symbols(naming, [spl4_14])])).
fof(f359, plain, (g_false_only(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | (~ spl4_7 | spl4_14 | spl4_18)), inference(subsumption_resolution, [], [f358, f270])).
fof(f270, plain, (~ g_true_only(sK1(sK3), sK1(sK3)) | spl4_14), inference(avatar_component_clause, [], [f269])).
fof(f358, plain, (g_false_only(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK1(sK3)) | (~ spl4_7 | spl4_18)), inference(subsumption_resolution, [], [f357, f305])).
fof(f357, plain, (g_false_only(sK1(sK3), sK3) | g_true_only(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK1(sK3)) | ~ spl4_7), inference(subsumption_resolution, [], [f291, f213])).
fof(f213, plain, ~ g_true_only(sK3, sK3), inference(resolution, [], [f211, f40])).
fof(f40, plain, ! [X3] : (~ g_both(X3, sK3) | ~ g_true_only(X3, X3)), inference(cnf_transformation, [], [f22])).
fof(f211, plain, g_both(sK3, sK3), inference(subsumption_resolution, [], [f210, f38])).
fof(f38, plain, ! [X3] : (~ g_true_only(X3, sK3) | ~ g_true_only(X3, X3)), inference(cnf_transformation, [], [f22])).
fof(f210, plain, (g_true_only(sK3, sK3) | g_both(sK3, sK3)), inference(duplicate_literal_removal, [], [f205])).
fof(f205, plain, (g_true_only(sK3, sK3) | g_true_only(sK3, sK3) | g_both(sK3, sK3)), inference(resolution, [], [f171, f163])).
fof(f163, plain, ! [X0] : (~ g_both(X0, sK3) | g_both(X0, X0)), inference(subsumption_resolution, [], [f157, f40])).
fof(f157, plain, ! [X0] : (g_both(X0, X0) | g_true_only(X0, X0) | ~ g_both(X0, sK3)), inference(resolution, [], [f56, f39])).
fof(f171, plain, ! [X0] : (g_both(X0, X0) | g_true_only(X0, sK3) | g_true_only(X0, X0)), inference(resolution, [], [f165, f56])).
fof(f291, plain, (g_true_only(sK3, sK3) | g_false_only(sK1(sK3), sK3) | g_true_only(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK1(sK3)) | ~ spl4_7), inference(duplicate_literal_removal, [], [f290])).
fof(f290, plain, (g_true_only(sK3, sK3) | g_false_only(sK1(sK3), sK3) | g_true_only(sK3, sK3) | g_true_only(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK1(sK3)) | ~ spl4_7), inference(resolution, [], [f202, f211])).
fof(f202, plain, (! [X4, X3] : (~ g_both(X3, X4) | g_true_only(X3, X3) | g_false_only(sK1(X4), X4) | g_true_only(X3, sK3) | g_true_only(sK1(X4), X4) | g_false_only(sK1(X4), sK1(X4)) | g_true_only(sK1(X4), sK1(X4))) | ~ spl4_7), inference(resolution, [], [f171, f87])).
fof(f87, plain, (! [X2, X0] : (~ g_both(X2, X2) | g_false_only(sK1(X0), X0) | ~ g_both(X2, X0) | g_true_only(sK1(X0), X0) | g_false_only(sK1(X0), sK1(X0)) | g_true_only(sK1(X0), sK1(X0))) | ~ spl4_7), inference(avatar_component_clause, [], [f86])).
fof(f351, plain, (~ spl4_15 | ~ spl4_16), inference(avatar_split_clause, [], [f340, f277, f273])).
fof(f273, plain, (spl4_15 <=> g_both(sK1(sK3), sK1(sK3))), introduced(avatar_definition, [new_symbols(naming, [spl4_15])])).
fof(f340, plain, (~ g_both(sK1(sK3), sK1(sK3)) | ~ spl4_16), inference(resolution, [], [f279, f42])).
fof(f42, plain, ! [X3] : (~ g_false_only(X3, sK3) | ~ g_both(X3, X3)), inference(cnf_transformation, [], [f22])).
fof(f279, plain, (g_false_only(sK1(sK3), sK3) | ~ spl4_16), inference(avatar_component_clause, [], [f277])).
fof(f350, plain, (~ spl4_16 | ~ spl4_18), inference(avatar_contradiction_clause, [], [f349])).
fof(f349, plain, ($false | (~ spl4_16 | ~ spl4_18)), inference(subsumption_resolution, [], [f348, f279])).
fof(f348, plain, (~ g_false_only(sK1(sK3), sK3) | ~ spl4_18), inference(resolution, [], [f336, f54])).
fof(f54, plain, ! [X0, X1] : (~ g_true(X0, X1) | ~ g_false_only(X0, X1)), inference(cnf_transformation, [], [f28])).
fof(f28, plain, ! [X0, X1] : ((g_false_only(X0, X1) | g_true(X0, X1) | ~ g_false(X0, X1)) & ((~ g_true(X0, X1) & g_false(X0, X1)) | ~ g_false_only(X0, X1))), inference(flattening, [], [f27])).
fof(f27, plain, ! [X0, X1] : ((g_false_only(X0, X1) | (g_true(X0, X1) | ~ g_false(X0, X1))) & ((~ g_true(X0, X1) & g_false(X0, X1)) | ~ g_false_only(X0, X1))), inference(nnf_transformation, [], [f10])).
fof(f10, plain, ! [X0, X1] : (g_false_only(X0, X1) <=> (~ g_true(X0, X1) & g_false(X0, X1))), inference(rectify, [], [f5])).
fof(f5, plain, ! [X2, X3] : (g_false_only(X2, X3) <=> (~ g_true(X2, X3) & g_false(X2, X3))), file('/home/ubuntu/library/tptp/Problems/SEV/SEV515+1.p', false_only_g)).
fof(f336, plain, (g_true(sK1(sK3), sK3) | ~ spl4_18), inference(resolution, [], [f306, f47])).
fof(f47, plain, ! [X0, X1] : (~ g_true_only(X0, X1) | g_true(X0, X1)), inference(cnf_transformation, [], [f24])).
fof(f24, plain, ! [X0, X1] : ((g_true_only(X0, X1) | g_false(X0, X1) | ~ g_true(X0, X1)) & ((~ g_false(X0, X1) & g_true(X0, X1)) | ~ g_true_only(X0, X1))), inference(flattening, [], [f23])).
fof(f23, plain, ! [X0, X1] : ((g_true_only(X0, X1) | (g_false(X0, X1) | ~ g_true(X0, X1))) & ((~ g_false(X0, X1) & g_true(X0, X1)) | ~ g_true_only(X0, X1))), inference(nnf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (g_true_only(X0, X1) <=> (~ g_false(X0, X1) & g_true(X0, X1))), inference(rectify, [], [f3])).
fof(f3, plain, ! [X2, X3] : (g_true_only(X2, X3) <=> (~ g_false(X2, X3) & g_true(X2, X3))), file('/home/ubuntu/library/tptp/Problems/SEV/SEV515+1.p', true_only_g)).
fof(f306, plain, (g_true_only(sK1(sK3), sK3) | ~ spl4_18), inference(avatar_component_clause, [], [f304])).
fof(f339, plain, (~ spl4_14 | ~ spl4_18), inference(avatar_split_clause, [], [f335, f304, f269])).
fof(f335, plain, (~ g_true_only(sK1(sK3), sK1(sK3)) | ~ spl4_18), inference(resolution, [], [f306, f38])).
fof(f334, plain, (~ spl4_14 | ~ spl4_15), inference(avatar_contradiction_clause, [], [f333])).
fof(f333, plain, ($false | (~ spl4_14 | ~ spl4_15)), inference(subsumption_resolution, [], [f324, f271])).
fof(f271, plain, (g_true_only(sK1(sK3), sK1(sK3)) | ~ spl4_14), inference(avatar_component_clause, [], [f269])).
fof(f324, plain, (~ g_true_only(sK1(sK3), sK1(sK3)) | ~ spl4_15), inference(resolution, [], [f313, f40])).
fof(f313, plain, (g_both(sK1(sK3), sK3) | ~ spl4_15), inference(resolution, [], [f275, f164])).
fof(f164, plain, ! [X1] : (~ g_both(X1, X1) | g_both(X1, sK3)), inference(subsumption_resolution, [], [f158, f37])).
fof(f37, plain, ! [X3] : (~ g_both(X3, X3) | ~ g_true_only(X3, sK3)), inference(cnf_transformation, [], [f22])).
fof(f158, plain, ! [X1] : (g_both(X1, sK3) | g_true_only(X1, sK3) | ~ g_both(X1, X1)), inference(resolution, [], [f56, f42])).
fof(f275, plain, (g_both(sK1(sK3), sK1(sK3)) | ~ spl4_15), inference(avatar_component_clause, [], [f273])).
fof(f322, plain, (~ spl4_18 | ~ spl4_15), inference(avatar_split_clause, [], [f316, f273, f304])).
fof(f316, plain, (~ g_true_only(sK1(sK3), sK3) | ~ spl4_15), inference(resolution, [], [f275, f37])).
fof(f311, plain, (spl4_18 | spl4_15 | ~ spl4_5), inference(avatar_split_clause, [], [f310, f78, f273, f304])).
fof(f78, plain, (spl4_5 <=> ! [X0, X2] : (g_both(sK1(X0), sK1(X0)) | ~ g_both(X2, X2) | ~ g_both(X2, X0) | g_true_only(sK1(X0), X0) | g_both(sK1(X0), X0) | g_false_only(sK1(X0), sK1(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl4_5])])).
fof(f310, plain, (g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK3) | ~ spl4_5), inference(subsumption_resolution, [], [f309, f165])).
fof(f309, plain, (g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | ~ spl4_5), inference(subsumption_resolution, [], [f308, f163])).
fof(f308, plain, (g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK3) | g_both(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | ~ spl4_5), inference(subsumption_resolution, [], [f298, f213])).
fof(f298, plain, (g_true_only(sK3, sK3) | g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK3) | g_both(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | ~ spl4_5), inference(duplicate_literal_removal, [], [f297])).
fof(f297, plain, (g_true_only(sK3, sK3) | g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK3, sK3) | g_true_only(sK1(sK3), sK3) | g_both(sK1(sK3), sK3) | g_false_only(sK1(sK3), sK1(sK3)) | ~ spl4_5), inference(resolution, [], [f203, f211])).
fof(f203, plain, (! [X6, X5] : (~ g_both(X5, X6) | g_true_only(X5, X5) | g_both(sK1(X6), sK1(X6)) | g_true_only(X5, sK3) | g_true_only(sK1(X6), X6) | g_both(sK1(X6), X6) | g_false_only(sK1(X6), sK1(X6))) | ~ spl4_5), inference(resolution, [], [f171, f79])).
fof(f79, plain, (! [X2, X0] : (~ g_both(X2, X2) | g_both(sK1(X0), sK1(X0)) | ~ g_both(X2, X0) | g_true_only(sK1(X0), X0) | g_both(sK1(X0), X0) | g_false_only(sK1(X0), sK1(X0))) | ~ spl4_5), inference(avatar_component_clause, [], [f78])).
fof(f286, plain, (spl4_14 | spl4_15 | spl4_16 | ~ spl4_8), inference(avatar_split_clause, [], [f285, f90, f277, f273, f269])).
fof(f90, plain, (spl4_8 <=> ! [X0, X2] : (g_false_only(sK1(X0), X0) | ~ g_both(X2, X2) | ~ g_both(X2, X0) | g_both(sK1(X0), sK1(X0)) | g_true_only(sK1(X0), sK1(X0)) | g_both(sK1(X0), X0))), introduced(avatar_definition, [new_symbols(naming, [spl4_8])])).
fof(f285, plain, (g_false_only(sK1(sK3), sK3) | g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK1(sK3)) | ~ spl4_8), inference(subsumption_resolution, [], [f284, f163])).
fof(f284, plain, (g_false_only(sK1(sK3), sK3) | g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK1(sK3)) | g_both(sK1(sK3), sK3) | ~ spl4_8), inference(subsumption_resolution, [], [f264, f213])).
fof(f264, plain, (g_true_only(sK3, sK3) | g_false_only(sK1(sK3), sK3) | g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK1(sK3)) | g_both(sK1(sK3), sK3) | ~ spl4_8), inference(duplicate_literal_removal, [], [f263])).
fof(f263, plain, (g_true_only(sK3, sK3) | g_false_only(sK1(sK3), sK3) | g_true_only(sK3, sK3) | g_both(sK1(sK3), sK1(sK3)) | g_true_only(sK1(sK3), sK1(sK3)) | g_both(sK1(sK3), sK3) | ~ spl4_8), inference(resolution, [], [f200, f211])).
fof(f200, plain, (! [X0, X1] : (~ g_both(X0, X1) | g_true_only(X0, X0) | g_false_only(sK1(X1), X1) | g_true_only(X0, sK3) | g_both(sK1(X1), sK1(X1)) | g_true_only(sK1(X1), sK1(X1)) | g_both(sK1(X1), X1)) | ~ spl4_8), inference(resolution, [], [f171, f91])).
fof(f91, plain, (! [X2, X0] : (~ g_both(X2, X2) | g_false_only(sK1(X0), X0) | ~ g_both(X2, X0) | g_both(sK1(X0), sK1(X0)) | g_true_only(sK1(X0), sK1(X0)) | g_both(sK1(X0), X0)) | ~ spl4_8), inference(avatar_component_clause, [], [f90])).
fof(f123, plain, ~ spl4_10, inference(avatar_contradiction_clause, [], [f122])).
fof(f122, plain, ($false | ~ spl4_10), inference(subsumption_resolution, [], [f121, f38])).
fof(f121, plain, (g_true_only(sK3, sK3) | ~ spl4_10), inference(resolution, [], [f117, f99])).
fof(f99, plain, (! [X1] : (g_false_only(X1, X1) | g_true_only(X1, X1)) | ~ spl4_10), inference(avatar_component_clause, [], [f98])).
fof(f98, plain, (spl4_10 <=> ! [X1] : (g_true_only(X1, X1) | g_false_only(X1, X1))), introduced(avatar_definition, [new_symbols(naming, [spl4_10])])).
fof(f117, plain, (~ g_false_only(sK3, sK3) | ~ spl4_10), inference(subsumption_resolution, [], [f115, f38])).
fof(f115, plain, (g_true_only(sK3, sK3) | ~ g_false_only(sK3, sK3) | ~ spl4_10), inference(resolution, [], [f99, f41])).
fof(f100, plain, (spl4_1 | spl4_10), inference(avatar_split_clause, [], [f46, f98, f62])).
fof(f62, plain, (spl4_1 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl4_1])])).
fof(f46, plain, ! [X1] : (g_true_only(X1, X1) | g_false_only(X1, X1) | sP0), inference(cnf_transformation, [], [f22])).
fof(f92, plain, (~ spl4_1 | spl4_8), inference(avatar_split_clause, [], [f30, f90, f62])).
fof(f30, plain, ! [X2, X0] : (g_false_only(sK1(X0), X0) | g_both(sK1(X0), X0) | g_true_only(sK1(X0), sK1(X0)) | g_both(sK1(X0), sK1(X0)) | ~ g_both(X2, X2) | ~ g_both(X2, X0) | ~ sP0), inference(cnf_transformation, [], [f18])).
fof(f18, plain, (! [X0] : ((((g_both(sK1(X0), sK1(X0)) | g_false_only(sK1(X0), sK1(X0))) & g_false_only(sK1(X0), X0)) | ((g_true_only(sK1(X0), sK1(X0)) | g_false_only(sK1(X0), sK1(X0))) & g_both(sK1(X0), X0)) | ((g_true_only(sK1(X0), sK1(X0)) | g_both(sK1(X0), sK1(X0))) & g_true_only(sK1(X0), X0))) | ! [X2] : (~ g_both(X2, X2) | ~ g_both(X2, X0))) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f16, f17])).
fof(f17, plain, ! [X0] : (? [X1] : (((g_both(X1, X1) | g_false_only(X1, X1)) & g_false_only(X1, X0)) | ((g_true_only(X1, X1) | g_false_only(X1, X1)) & g_both(X1, X0)) | ((g_true_only(X1, X1) | g_both(X1, X1)) & g_true_only(X1, X0))) => (((g_both(sK1(X0), sK1(X0)) | g_false_only(sK1(X0), sK1(X0))) & g_false_only(sK1(X0), X0)) | ((g_true_only(sK1(X0), sK1(X0)) | g_false_only(sK1(X0), sK1(X0))) & g_both(sK1(X0), X0)) | ((g_true_only(sK1(X0), sK1(X0)) | g_both(sK1(X0), sK1(X0))) & g_true_only(sK1(X0), X0)))), introduced(choice_axiom, [])).
fof(f16, plain, (! [X0] : (? [X1] : (((g_both(X1, X1) | g_false_only(X1, X1)) & g_false_only(X1, X0)) | ((g_true_only(X1, X1) | g_false_only(X1, X1)) & g_both(X1, X0)) | ((g_true_only(X1, X1) | g_both(X1, X1)) & g_true_only(X1, X0))) | ! [X2] : (~ g_both(X2, X2) | ~ g_both(X2, X0))) | ~ sP0), inference(rectify, [], [f15])).
fof(f15, plain, (! [X2] : (? [X3] : (((g_both(X3, X3) | g_false_only(X3, X3)) & g_false_only(X3, X2)) | ((g_true_only(X3, X3) | g_false_only(X3, X3)) & g_both(X3, X2)) | ((g_true_only(X3, X3) | g_both(X3, X3)) & g_true_only(X3, X2))) | ! [X4] : (~ g_both(X4, X4) | ~ g_both(X4, X2))) | ~ sP0), inference(nnf_transformation, [], [f13])).
fof(f88, plain, (~ spl4_1 | spl4_7), inference(avatar_split_clause, [], [f31, f86, f62])).
fof(f31, plain, ! [X2, X0] : (g_false_only(sK1(X0), X0) | g_true_only(sK1(X0), sK1(X0)) | g_false_only(sK1(X0), sK1(X0)) | g_true_only(sK1(X0), X0) | ~ g_both(X2, X2) | ~ g_both(X2, X0) | ~ sP0), inference(cnf_transformation, [], [f18])).
fof(f80, plain, (~ spl4_1 | spl4_5), inference(avatar_split_clause, [], [f33, f78, f62])).
fof(f33, plain, ! [X2, X0] : (g_both(sK1(X0), sK1(X0)) | g_false_only(sK1(X0), sK1(X0)) | g_both(sK1(X0), X0) | g_true_only(sK1(X0), X0) | ~ g_both(X2, X2) | ~ g_both(X2, X0) | ~ sP0), inference(cnf_transformation, [], [f18])).