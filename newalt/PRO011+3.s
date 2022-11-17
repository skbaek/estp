fof(f895, plain, $false, inference(avatar_sat_refutation, [], [f662, f666, f745, f757, f894])).
fof(f894, plain, ~ spl23_23, inference(avatar_contradiction_clause, [], [f893])).
fof(f893, plain, ($false | ~ spl23_23), inference(resolution, [], [f879, f359])).
fof(f359, plain, ~ arboreal(sK21), inference(subsumption_resolution, [], [f357, f333])).
fof(f333, plain, ~ atomic(tptp0), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ~ atomic(tptp0), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', sos_51)).
fof(f357, plain, (~ arboreal(sK21) | atomic(tptp0)), inference(resolution, [], [f252, f348])).
fof(f348, plain, occurrence_of(sK21, tptp0), inference(cnf_transformation, [], [f242])).
fof(f242, plain, (! [X1, X2] : (((min_precedes(X1, sK22(X1), tptp0) & subactivity_occurrence(sK22(X1), sK21) & occurrence_of(sK22(X1), tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK21, X2) | ~ leaf_occ(X2, sK21)) & occurrence_of(sK21, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK21, sK22])], [f189, f241, f240])).
fof(f240, plain, (? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)) => (! [X2, X1] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK21) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK21, X2) | ~ leaf_occ(X2, sK21)) & occurrence_of(sK21, tptp0))), introduced(choice_axiom, [])).
fof(f241, plain, ! [X1] : (? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK21) & occurrence_of(X3, tptp1)) => (min_precedes(X1, sK22(X1), tptp0) & subactivity_occurrence(sK22(X1), sK21) & occurrence_of(sK22(X1), tptp1))), introduced(choice_axiom, [])).
fof(f189, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)), inference(definition_folding, [], [f187, e188])).
fof(f188, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(usedef, [], [e188])).
fof(e188, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f187, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f114])).
fof(f114, plain, ~ ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2] : ((occurrence_of(X2, tptp2) => ~ ? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1))) & (occurrence_of(X2, tptp1) => ~ ? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2))) & leaf_occ(X2, X0))), inference(rectify, [], [f64])).
fof(f64, plain, ~ ! [X165] : (occurrence_of(X165, tptp0) => ? [X166, X167] : ((occurrence_of(X167, tptp2) => ~ ? [X169] : (min_precedes(X166, X169, tptp0) & subactivity_occurrence(X169, X165) & occurrence_of(X169, tptp1))) & (occurrence_of(X167, tptp1) => ~ ? [X168] : (min_precedes(X166, X168, tptp0) & subactivity_occurrence(X168, X165) & occurrence_of(X168, tptp2))) & leaf_occ(X167, X165))), inference(negated_conjecture, [], [f63])).
fof(f63, plain, ~ ! [X165] : (occurrence_of(X165, tptp0) => ? [X166, X167] : ((occurrence_of(X167, tptp2) => ~ ? [X169] : (min_precedes(X166, X169, tptp0) & subactivity_occurrence(X169, X165) & occurrence_of(X169, tptp1))) & (occurrence_of(X167, tptp1) => ~ ? [X168] : (min_precedes(X166, X168, tptp0) & subactivity_occurrence(X168, X165) & occurrence_of(X168, tptp2))) & leaf_occ(X167, X165))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', goals)).
fof(f252, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ arboreal(X0) | atomic(X1)), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f125])).
fof(f125, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f8])).
fof(f8, plain, ! [X16, X17] : (occurrence_of(X16, X17) => (arboreal(X16) <=> atomic(X17))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', sos_07)).
fof(f879, plain, (! [X16] : arboreal(X16) | ~ spl23_23), inference(resolution, [], [f756, f322])).
fof(f322, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | arboreal(X0)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0, X1, X2] : (arboreal(X0) | ~ min_precedes(X0, X1, X2)), inference(ennf_transformation, [], [f110])).
fof(f110, plain, ! [X0, X1, X2] : (min_precedes(X0, X1, X2) => arboreal(X0)), inference(rectify, [], [f47])).
fof(f47, plain, ! [X148, X149, X150] : (min_precedes(X148, X149, X150) => arboreal(X148)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', sos_46)).
fof(f756, plain, (! [X0] : min_precedes(X0, sK20(X0, sK21), tptp0) | ~ spl23_23), inference(avatar_component_clause, [], [f755])).
fof(f755, plain, (spl23_23 <=> ! [X0] : min_precedes(X0, sK20(X0, sK21), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl23_23])])).
fof(f757, plain, (spl23_5 | spl23_23), inference(avatar_split_clause, [], [f551, f755, f448])).
fof(f448, plain, (spl23_5 <=> ! [X0] : (occurrence_of(X0, tptp2) | ~ leaf_occ(X0, sK21))), introduced(avatar_definition, [new_symbols(naming, [spl23_5])])).
fof(f551, plain, ! [X0, X1] : (min_precedes(X0, sK20(X0, sK21), tptp0) | occurrence_of(X1, tptp2) | ~ leaf_occ(X1, sK21)), inference(resolution, [], [f347, f349])).
fof(f349, plain, ! [X2, X1] : (sP0(X1, sK21, X2) | occurrence_of(X2, tptp2) | ~ leaf_occ(X2, sK21)), inference(cnf_transformation, [], [f242])).
fof(f347, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | min_precedes(X0, sK20(X0, X1), tptp0)), inference(cnf_transformation, [], [f239])).
fof(f239, plain, ! [X0, X1, X2] : (((min_precedes(X0, sK20(X0, X1), tptp0) & subactivity_occurrence(sK20(X0, X1), X1) & occurrence_of(sK20(X0, X1), tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK20])], [f237, f238])).
fof(f238, plain, ! [X1, X0] : (? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) => (min_precedes(X0, sK20(X0, X1), tptp0) & subactivity_occurrence(sK20(X0, X1), X1) & occurrence_of(sK20(X0, X1), tptp2))), introduced(choice_axiom, [])).
fof(f237, plain, ! [X0, X1, X2] : ((? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(rectify, [], [f236])).
fof(f236, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(nnf_transformation, [], [f188])).
fof(f745, plain, ~ spl23_19, inference(avatar_contradiction_clause, [], [f744])).
fof(f744, plain, ($false | ~ spl23_19), inference(resolution, [], [f734, f359])).
fof(f734, plain, (! [X9] : arboreal(X9) | ~ spl23_19), inference(resolution, [], [f665, f322])).
fof(f665, plain, (! [X6] : min_precedes(X6, sK22(X6), tptp0) | ~ spl23_19), inference(avatar_component_clause, [], [f664])).
fof(f664, plain, (spl23_19 <=> ! [X6] : min_precedes(X6, sK22(X6), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl23_19])])).
fof(f666, plain, (spl23_11 | spl23_19), inference(avatar_split_clause, [], [f634, f664, f572])).
fof(f572, plain, (spl23_11 <=> ! [X7] : (~ leaf_occ(X7, sK21) | occurrence_of(X7, tptp1))), introduced(avatar_definition, [new_symbols(naming, [spl23_11])])).
fof(f634, plain, ! [X6, X7] : (min_precedes(X6, sK22(X6), tptp0) | ~ leaf_occ(X7, sK21) | occurrence_of(X7, tptp1)), inference(resolution, [], [f352, f344])).
fof(f344, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | occurrence_of(X2, tptp1)), inference(cnf_transformation, [], [f239])).
fof(f352, plain, ! [X2, X1] : (min_precedes(X1, sK22(X1), tptp0) | sP0(X1, sK21, X2) | ~ leaf_occ(X2, sK21)), inference(cnf_transformation, [], [f242])).
fof(f662, plain, (~ spl23_5 | ~ spl23_11), inference(avatar_contradiction_clause, [], [f661])).
fof(f661, plain, ($false | (~ spl23_5 | ~ spl23_11)), inference(subsumption_resolution, [], [f660, f343])).
fof(f343, plain, ~ (tptp1 = tptp2), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ~ (tptp1 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', sos_61)).
fof(f660, plain, ((tptp1 = tptp2) | (~ spl23_5 | ~ spl23_11)), inference(backward_demodulation, [], [f656, f657])).
fof(f657, plain, ((tptp1 = sK1(sK19(sK21))) | ~ spl23_11), inference(resolution, [], [f420, f598])).
fof(f598, plain, (occurrence_of(sK19(sK21), tptp1) | ~ spl23_11), inference(subsumption_resolution, [], [f597, f348])).
fof(f597, plain, (occurrence_of(sK19(sK21), tptp1) | ~ occurrence_of(sK21, tptp0) | ~ spl23_11), inference(resolution, [], [f573, f331])).
fof(f331, plain, ! [X0] : (leaf_occ(sK19(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ! [X0] : ((leaf_occ(sK19(X0), X0) & next_subocc(sK18(X0), sK19(X0), tptp0) & (occurrence_of(sK19(X0), tptp2) | occurrence_of(sK19(X0), tptp1)) & next_subocc(sK17(X0), sK18(X0), tptp0) & occurrence_of(sK18(X0), tptp4) & root_occ(sK17(X0), X0) & occurrence_of(sK17(X0), tptp3)) | ~ occurrence_of(X0, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK17, sK18, sK19])], [f186, f234])).
fof(f234, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) => (leaf_occ(sK19(X0), X0) & next_subocc(sK18(X0), sK19(X0), tptp0) & (occurrence_of(sK19(X0), tptp2) | occurrence_of(sK19(X0), tptp1)) & next_subocc(sK17(X0), sK18(X0), tptp0) & occurrence_of(sK18(X0), tptp4) & root_occ(sK17(X0), X0) & occurrence_of(sK17(X0), tptp3))), introduced(choice_axiom, [])).
fof(f186, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) | ~ occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f113])).
fof(f113, plain, ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3))), inference(rectify, [], [f50])).
fof(f50, plain, ! [X161] : (occurrence_of(X161, tptp0) => ? [X162, X163, X164] : (leaf_occ(X164, X161) & next_subocc(X163, X164, tptp0) & (occurrence_of(X164, tptp2) | occurrence_of(X164, tptp1)) & next_subocc(X162, X163, tptp0) & occurrence_of(X163, tptp4) & root_occ(X162, X161) & occurrence_of(X162, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', sos_49)).
fof(f573, plain, (! [X7] : (~ leaf_occ(X7, sK21) | occurrence_of(X7, tptp1)) | ~ spl23_11), inference(avatar_component_clause, [], [f572])).
fof(f420, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | (sK1(X1) = X0)), inference(subsumption_resolution, [], [f413, f244])).
fof(f244, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ! [X0, X1] : ((activity_occurrence(X1) & activity(X0)) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', sos)).
fof(f413, plain, ! [X0, X1] : ((sK1(X1) = X0) | ~ occurrence_of(X1, X0) | ~ activity_occurrence(X1)), inference(resolution, [], [f247, f246])).
fof(f246, plain, ! [X0] : (occurrence_of(X0, sK1(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f191])).
fof(f191, plain, ! [X0] : ((occurrence_of(X0, sK1(X0)) & activity(sK1(X0))) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f116, f190])).
fof(f190, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) => (occurrence_of(X0, sK1(X0)) & activity(sK1(X0)))), introduced(choice_axiom, [])).
fof(f116, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f2])).
fof(f2, plain, ! [X2] : (activity_occurrence(X2) => ? [X3] : (occurrence_of(X2, X3) & activity(X3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', sos_01)).
fof(f247, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f118])).
fof(f118, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f117])).
fof(f117, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f3])).
fof(f3, plain, ! [X4, X5, X6] : ((occurrence_of(X4, X6) & occurrence_of(X4, X5)) => (X5 = X6)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+3.p', sos_02)).
fof(f656, plain, ((tptp2 = sK1(sK19(sK21))) | ~ spl23_5), inference(resolution, [], [f420, f456])).
fof(f456, plain, (occurrence_of(sK19(sK21), tptp2) | ~ spl23_5), inference(subsumption_resolution, [], [f455, f348])).
fof(f455, plain, (occurrence_of(sK19(sK21), tptp2) | ~ occurrence_of(sK21, tptp0) | ~ spl23_5), inference(resolution, [], [f449, f331])).
fof(f449, plain, (! [X0] : (~ leaf_occ(X0, sK21) | occurrence_of(X0, tptp2)) | ~ spl23_5), inference(avatar_component_clause, [], [f448])).