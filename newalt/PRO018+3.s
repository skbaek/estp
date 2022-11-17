fof(f4812, plain, $false, inference(avatar_sat_refutation, [], [f702, f2220, f3669, f3690, f4047, f4662, f4706, f4752, f4810])).
fof(f4810, plain, (~ spl23_5 | ~ spl23_109 | ~ spl23_140), inference(avatar_contradiction_clause, [], [f4809])).
fof(f4809, plain, ($false | (~ spl23_5 | ~ spl23_109 | ~ spl23_140)), inference(subsumption_resolution, [], [f4808, f339])).
fof(f339, plain, ~ (tptp3 = tptp2), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ~ (tptp3 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_60)).
fof(f4808, plain, ((tptp3 = tptp2) | (~ spl23_5 | ~ spl23_109 | ~ spl23_140)), inference(forward_demodulation, [], [f4805, f4764])).
fof(f4764, plain, ((tptp3 = sK1(sK18(sK20))) | (~ spl23_109 | ~ spl23_140)), inference(backward_demodulation, [], [f3722, f4657])).
fof(f4657, plain, ((sK18(sK20) = sK16(sK16(sK20))) | ~ spl23_140), inference(avatar_component_clause, [], [f4655])).
fof(f4655, plain, (spl23_140 <=> (sK18(sK20) = sK16(sK16(sK20)))), introduced(avatar_definition, [new_symbols(naming, [spl23_140])])).
fof(f3722, plain, ((tptp3 = sK1(sK16(sK16(sK20)))) | ~ spl23_109), inference(resolution, [], [f3668, f384])).
fof(f384, plain, ! [X2, X1] : (~ occurrence_of(X2, X1) | (sK1(X2) = X1)), inference(subsumption_resolution, [], [f381, f244])).
fof(f244, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f116])).
fof(f116, plain, ! [X0, X1] : ((activity_occurrence(X1) & activity(X0)) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos)).
fof(f381, plain, ! [X2, X1] : ((sK1(X2) = X1) | ~ occurrence_of(X2, X1) | ~ activity_occurrence(X2)), inference(resolution, [], [f247, f246])).
fof(f246, plain, ! [X0] : (occurrence_of(X0, sK1(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f195])).
fof(f195, plain, ! [X0] : ((occurrence_of(X0, sK1(X0)) & activity(sK1(X0))) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f117, f194])).
fof(f194, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) => (occurrence_of(X0, sK1(X0)) & activity(sK1(X0)))), introduced(choice_axiom, [])).
fof(f117, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f2])).
fof(f2, plain, ! [X2] : (activity_occurrence(X2) => ? [X3] : (occurrence_of(X2, X3) & activity(X3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_01)).
fof(f247, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f118])).
fof(f118, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f3])).
fof(f3, plain, ! [X4, X5, X6] : ((occurrence_of(X4, X6) & occurrence_of(X4, X5)) => (X5 = X6)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_02)).
fof(f3668, plain, (occurrence_of(sK16(sK16(sK20)), tptp3) | ~ spl23_109), inference(avatar_component_clause, [], [f3666])).
fof(f3666, plain, (spl23_109 <=> occurrence_of(sK16(sK16(sK20)), tptp3)), introduced(avatar_definition, [new_symbols(naming, [spl23_109])])).
fof(f4805, plain, ((tptp2 = sK1(sK18(sK20))) | ~ spl23_5), inference(resolution, [], [f697, f384])).
fof(f697, plain, (occurrence_of(sK18(sK20), tptp2) | ~ spl23_5), inference(avatar_component_clause, [], [f695])).
fof(f695, plain, (spl23_5 <=> occurrence_of(sK18(sK20), tptp2)), introduced(avatar_definition, [new_symbols(naming, [spl23_5])])).
fof(f4752, plain, (~ spl23_109 | ~ spl23_141), inference(avatar_contradiction_clause, [], [f4751])).
fof(f4751, plain, ($false | (~ spl23_109 | ~ spl23_141)), inference(subsumption_resolution, [], [f4750, f335])).
fof(f335, plain, ~ (tptp3 = tptp4), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ~ (tptp3 = tptp4), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_56)).
fof(f4750, plain, ((tptp3 = tptp4) | (~ spl23_109 | ~ spl23_141)), inference(backward_demodulation, [], [f645, f4721])).
fof(f4721, plain, ((tptp3 = sK1(sK17(sK20))) | (~ spl23_109 | ~ spl23_141)), inference(backward_demodulation, [], [f3722, f4661])).
fof(f4661, plain, ((sK17(sK20) = sK16(sK16(sK20))) | ~ spl23_141), inference(avatar_component_clause, [], [f4659])).
fof(f4659, plain, (spl23_141 <=> (sK17(sK20) = sK16(sK16(sK20)))), introduced(avatar_definition, [new_symbols(naming, [spl23_141])])).
fof(f645, plain, (tptp4 = sK1(sK17(sK20))), inference(resolution, [], [f616, f384])).
fof(f616, plain, occurrence_of(sK17(sK20), tptp4), inference(subsumption_resolution, [], [f615, f344])).
fof(f344, plain, occurrence_of(sK21, tptp0), inference(cnf_transformation, [], [f242])).
fof(f242, plain, (! [X2, X3] : (((min_precedes(X2, sK22(X2), tptp0) & occurrence_of(sK22(X2), tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK21) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK20, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK20, sK21) & arboreal(sK20) & subactivity_occurrence(sK20, sK21) & occurrence_of(sK21, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK20, sK21, sK22])], [f193, f241, f240])).
fof(f240, plain, (? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => (! [X3, X2] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK21) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK20, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK20, sK21) & arboreal(sK20) & subactivity_occurrence(sK20, sK21) & occurrence_of(sK21, tptp0))), introduced(choice_axiom, [])).
fof(f241, plain, ! [X2] : (? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) => (min_precedes(X2, sK22(X2), tptp0) & occurrence_of(sK22(X2), tptp1))), introduced(choice_axiom, [])).
fof(f193, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(definition_folding, [], [f191, e192])).
fof(f192, plain, ! [X2, X3] : ((? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ sP0(X2, X3)), inference(usedef, [], [e192])).
fof(e192, plain, ! [X2, X3] : (sP0(X2, X3) <=> (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f191, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(flattening, [], [f190])).
fof(f190, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & (~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f114])).
fof(f114, plain, ~ ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3] : ((occurrence_of(X3, tptp2) => ~ ? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1))) & (occurrence_of(X3, tptp1) => ~ ? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2))) & leaf_occ(X3, X1) & min_precedes(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f64])).
fof(f64, plain, ~ ! [X167, X168] : ((~ leaf_occ(X167, X168) & arboreal(X167) & subactivity_occurrence(X167, X168) & occurrence_of(X168, tptp0)) => ? [X169, X170] : ((occurrence_of(X170, tptp2) => ~ ? [X172] : (min_precedes(X169, X172, tptp0) & occurrence_of(X172, tptp1))) & (occurrence_of(X170, tptp1) => ~ ? [X171] : (min_precedes(X169, X171, tptp0) & occurrence_of(X171, tptp2))) & leaf_occ(X170, X168) & min_precedes(X169, X170, tptp0) & (occurrence_of(X170, tptp2) | occurrence_of(X170, tptp1)) & next_subocc(X167, X169, tptp0) & occurrence_of(X169, tptp3))), inference(negated_conjecture, [], [f63])).
fof(f63, plain, ~ ! [X167, X168] : ((~ leaf_occ(X167, X168) & arboreal(X167) & subactivity_occurrence(X167, X168) & occurrence_of(X168, tptp0)) => ? [X169, X170] : ((occurrence_of(X170, tptp2) => ~ ? [X172] : (min_precedes(X169, X172, tptp0) & occurrence_of(X172, tptp1))) & (occurrence_of(X170, tptp1) => ~ ? [X171] : (min_precedes(X169, X171, tptp0) & occurrence_of(X171, tptp2))) & leaf_occ(X170, X168) & min_precedes(X169, X170, tptp0) & (occurrence_of(X170, tptp2) | occurrence_of(X170, tptp1)) & next_subocc(X167, X169, tptp0) & occurrence_of(X169, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', goals)).
fof(f615, plain, (occurrence_of(sK17(sK20), tptp4) | ~ occurrence_of(sK21, tptp0)), inference(subsumption_resolution, [], [f614, f346])).
fof(f346, plain, arboreal(sK20), inference(cnf_transformation, [], [f242])).
fof(f614, plain, (~ arboreal(sK20) | occurrence_of(sK17(sK20), tptp4) | ~ occurrence_of(sK21, tptp0)), inference(subsumption_resolution, [], [f609, f347])).
fof(f347, plain, ~ leaf_occ(sK20, sK21), inference(cnf_transformation, [], [f242])).
fof(f609, plain, (leaf_occ(sK20, sK21) | ~ arboreal(sK20) | occurrence_of(sK17(sK20), tptp4) | ~ occurrence_of(sK21, tptp0)), inference(resolution, [], [f324, f345])).
fof(f345, plain, subactivity_occurrence(sK20, sK21), inference(cnf_transformation, [], [f242])).
fof(f324, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK17(X0), tptp4) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ! [X0, X1] : ((! [X5] : ((sK18(X0) = X5) | (sK17(X0) = X5) | ~ min_precedes(sK16(X0), X5, tptp0)) & min_precedes(sK17(X0), sK18(X0), tptp0) & (occurrence_of(sK18(X0), tptp2) | occurrence_of(sK18(X0), tptp1)) & min_precedes(sK16(X0), sK17(X0), tptp0) & occurrence_of(sK17(X0), tptp4) & next_subocc(X0, sK16(X0), tptp0) & occurrence_of(sK16(X0), tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16, sK17, sK18])], [f189, f234])).
fof(f234, plain, ! [X0] : (? [X2, X3, X4] : (! [X5] : ((X4 = X5) | (X3 = X5) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) => (! [X5] : ((sK18(X0) = X5) | (sK17(X0) = X5) | ~ min_precedes(sK16(X0), X5, tptp0)) & min_precedes(sK17(X0), sK18(X0), tptp0) & (occurrence_of(sK18(X0), tptp2) | occurrence_of(sK18(X0), tptp1)) & min_precedes(sK16(X0), sK17(X0), tptp0) & occurrence_of(sK17(X0), tptp4) & next_subocc(X0, sK16(X0), tptp0) & occurrence_of(sK16(X0), tptp3))), introduced(choice_axiom, [])).
fof(f189, plain, ! [X0, X1] : (? [X2, X3, X4] : (! [X5] : ((X4 = X5) | (X3 = X5) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(flattening, [], [f188])).
fof(f188, plain, ! [X0, X1] : (? [X2, X3, X4] : (! [X5] : (((X4 = X5) | (X3 = X5)) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | (leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3, X4] : (! [X5] : (min_precedes(X2, X5, tptp0) => ((X4 = X5) | (X3 = X5))) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f50])).
fof(f50, plain, ! [X161, X162] : ((~ leaf_occ(X161, X162) & arboreal(X161) & subactivity_occurrence(X161, X162) & occurrence_of(X162, tptp0)) => ? [X163, X164, X165] : (! [X166] : (min_precedes(X163, X166, tptp0) => ((X165 = X166) | (X164 = X166))) & min_precedes(X164, X165, tptp0) & (occurrence_of(X165, tptp2) | occurrence_of(X165, tptp1)) & min_precedes(X163, X164, tptp0) & occurrence_of(X164, tptp4) & next_subocc(X161, X163, tptp0) & occurrence_of(X163, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_49)).
fof(f4706, plain, (~ spl23_6 | ~ spl23_109 | ~ spl23_140), inference(avatar_contradiction_clause, [], [f4705])).
fof(f4705, plain, ($false | (~ spl23_6 | ~ spl23_109 | ~ spl23_140)), inference(subsumption_resolution, [], [f4704, f338])).
fof(f338, plain, ~ (tptp3 = tptp1), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ~ (tptp3 = tptp1), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_59)).
fof(f4704, plain, ((tptp3 = tptp1) | (~ spl23_6 | ~ spl23_109 | ~ spl23_140)), inference(backward_demodulation, [], [f3500, f4677])).
fof(f4677, plain, ((tptp3 = sK1(sK18(sK20))) | (~ spl23_109 | ~ spl23_140)), inference(backward_demodulation, [], [f3722, f4657])).
fof(f3500, plain, ((tptp1 = sK1(sK18(sK20))) | ~ spl23_6), inference(resolution, [], [f701, f384])).
fof(f701, plain, (occurrence_of(sK18(sK20), tptp1) | ~ spl23_6), inference(avatar_component_clause, [], [f699])).
fof(f699, plain, (spl23_6 <=> occurrence_of(sK18(sK20), tptp1)), introduced(avatar_definition, [new_symbols(naming, [spl23_6])])).
fof(f4662, plain, (spl23_31 | spl23_140 | spl23_141 | ~ spl23_120), inference(avatar_split_clause, [], [f4653, f4044, f4659, f4655, f2164])).
fof(f2164, plain, (spl23_31 <=> ! [X0] : (~ subactivity_occurrence(sK20, X0) | ~ occurrence_of(X0, tptp0))), introduced(avatar_definition, [new_symbols(naming, [spl23_31])])).
fof(f4044, plain, (spl23_120 <=> next_subocc(sK16(sK20), sK16(sK16(sK20)), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl23_120])])).
fof(f4653, plain, (! [X1] : ((sK17(sK20) = sK16(sK16(sK20))) | (sK18(sK20) = sK16(sK16(sK20))) | ~ subactivity_occurrence(sK20, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl23_120), inference(subsumption_resolution, [], [f4652, f902])).
fof(f902, plain, ! [X3] : (~ leaf_occ(sK20, X3) | ~ occurrence_of(X3, tptp0)), inference(resolution, [], [f768, f308])).
fof(f308, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(flattening, [], [f166])).
fof(f166, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | (~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2))), inference(ennf_transformation, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : ((leaf_occ(X1, X0) & occurrence_of(X0, X2)) => ~ ? [X3] : min_precedes(X1, X3, X2)), inference(rectify, [], [f38])).
fof(f38, plain, ! [X113, X114, X115] : ((leaf_occ(X114, X113) & occurrence_of(X113, X115)) => ~ ? [X116] : min_precedes(X114, X116, X115)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_37)).
fof(f768, plain, min_precedes(sK20, sK16(sK20), tptp0), inference(resolution, [], [f629, f278])).
fof(f278, plain, ! [X2, X0, X1] : (~ next_subocc(X0, X1, X2) | min_precedes(X0, X1, X2)), inference(cnf_transformation, [], [f215])).
fof(f215, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (min_precedes(sK8(X0, X1, X2), X1, X2) & min_precedes(X0, sK8(X0, X1, X2), X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f213, f214])).
fof(f214, plain, ! [X2, X1, X0] : (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) => (min_precedes(sK8(X0, X1, X2), X1, X2) & min_precedes(X0, sK8(X0, X1, X2), X2))), introduced(choice_axiom, [])).
fof(f213, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(rectify, [], [f212])).
fof(f212, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(flattening, [], [f211])).
fof(f211, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2))) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(nnf_transformation, [], [f144])).
fof(f144, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (~ ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(rectify, [], [f23])).
fof(f23, plain, ! [X60, X61, X62] : (next_subocc(X60, X61, X62) <=> (~ ? [X63] : (min_precedes(X63, X61, X62) & min_precedes(X60, X63, X62)) & min_precedes(X60, X61, X62))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_22)).
fof(f629, plain, next_subocc(sK20, sK16(sK20), tptp0), inference(subsumption_resolution, [], [f628, f344])).
fof(f628, plain, (next_subocc(sK20, sK16(sK20), tptp0) | ~ occurrence_of(sK21, tptp0)), inference(subsumption_resolution, [], [f627, f346])).
fof(f627, plain, (~ arboreal(sK20) | next_subocc(sK20, sK16(sK20), tptp0) | ~ occurrence_of(sK21, tptp0)), inference(subsumption_resolution, [], [f622, f347])).
fof(f622, plain, (leaf_occ(sK20, sK21) | ~ arboreal(sK20) | next_subocc(sK20, sK16(sK20), tptp0) | ~ occurrence_of(sK21, tptp0)), inference(resolution, [], [f323, f345])).
fof(f323, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | next_subocc(X0, sK16(X0), tptp0) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f4652, plain, (! [X1] : ((sK17(sK20) = sK16(sK16(sK20))) | (sK18(sK20) = sK16(sK16(sK20))) | leaf_occ(sK20, X1) | ~ subactivity_occurrence(sK20, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl23_120), inference(subsumption_resolution, [], [f4595, f346])).
fof(f4595, plain, (! [X1] : ((sK17(sK20) = sK16(sK16(sK20))) | (sK18(sK20) = sK16(sK16(sK20))) | leaf_occ(sK20, X1) | ~ arboreal(sK20) | ~ subactivity_occurrence(sK20, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl23_120), inference(resolution, [], [f4455, f328])).
fof(f328, plain, ! [X0, X5, X1] : (~ min_precedes(sK16(X0), X5, tptp0) | (sK17(X0) = X5) | (sK18(X0) = X5) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f4455, plain, (min_precedes(sK16(sK20), sK16(sK16(sK20)), tptp0) | ~ spl23_120), inference(resolution, [], [f4046, f278])).
fof(f4046, plain, (next_subocc(sK16(sK20), sK16(sK16(sK20)), tptp0) | ~ spl23_120), inference(avatar_component_clause, [], [f4044])).
fof(f4047, plain, (spl23_108 | spl23_120), inference(avatar_split_clause, [], [f4042, f4044, f3663])).
fof(f3663, plain, (spl23_108 <=> ! [X44] : ~ min_precedes(sK16(sK20), X44, tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl23_108])])).
fof(f4042, plain, ! [X44] : (next_subocc(sK16(sK20), sK16(sK16(sK20)), tptp0) | ~ min_precedes(sK16(sK20), X44, tptp0)), inference(subsumption_resolution, [], [f4037, f288])).
fof(f288, plain, ! [X2, X0, X1] : (occurrence_of(sK10(X0, X1, X2), X0) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f221])).
fof(f221, plain, ! [X0, X1, X2] : ((subactivity_occurrence(X2, sK10(X0, X1, X2)) & subactivity_occurrence(X1, sK10(X0, X1, X2)) & occurrence_of(sK10(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f146, f220])).
fof(f220, plain, ! [X2, X1, X0] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) => (subactivity_occurrence(X2, sK10(X0, X1, X2)) & subactivity_occurrence(X1, sK10(X0, X1, X2)) & occurrence_of(sK10(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f146, plain, ! [X0, X1, X2] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f89])).
fof(f89, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0))), inference(rectify, [], [f26])).
fof(f26, plain, ! [X69, X70, X71] : (min_precedes(X70, X71, X69) => ? [X72] : (subactivity_occurrence(X71, X72) & subactivity_occurrence(X70, X72) & occurrence_of(X72, X69))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_25)).
fof(f4037, plain, ! [X44] : (next_subocc(sK16(sK20), sK16(sK16(sK20)), tptp0) | ~ min_precedes(sK16(sK20), X44, tptp0) | ~ occurrence_of(sK10(tptp0, sK16(sK20), X44), tptp0)), inference(resolution, [], [f1683, f815])).
fof(f815, plain, ! [X4] : (~ leaf_occ(sK16(sK20), X4) | ~ occurrence_of(X4, tptp0)), inference(resolution, [], [f656, f308])).
fof(f656, plain, min_precedes(sK16(sK20), sK17(sK20), tptp0), inference(subsumption_resolution, [], [f655, f344])).
fof(f655, plain, (min_precedes(sK16(sK20), sK17(sK20), tptp0) | ~ occurrence_of(sK21, tptp0)), inference(subsumption_resolution, [], [f654, f346])).
fof(f654, plain, (~ arboreal(sK20) | min_precedes(sK16(sK20), sK17(sK20), tptp0) | ~ occurrence_of(sK21, tptp0)), inference(subsumption_resolution, [], [f649, f347])).
fof(f649, plain, (leaf_occ(sK20, sK21) | ~ arboreal(sK20) | min_precedes(sK16(sK20), sK17(sK20), tptp0) | ~ occurrence_of(sK21, tptp0)), inference(resolution, [], [f325, f345])).
fof(f325, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | min_precedes(sK16(X0), sK17(X0), tptp0) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f1683, plain, ! [X0, X1] : (leaf_occ(X0, sK10(tptp0, X0, X1)) | next_subocc(X0, sK16(X0), tptp0) | ~ min_precedes(X0, X1, tptp0)), inference(duplicate_literal_removal, [], [f1682])).
fof(f1682, plain, ! [X0, X1] : (next_subocc(X0, sK16(X0), tptp0) | leaf_occ(X0, sK10(tptp0, X0, X1)) | ~ min_precedes(X0, X1, tptp0) | ~ min_precedes(X0, X1, tptp0)), inference(resolution, [], [f630, f288])).
fof(f630, plain, ! [X2, X0, X1] : (~ occurrence_of(sK10(X1, X0, X2), tptp0) | next_subocc(X0, sK16(X0), tptp0) | leaf_occ(X0, sK10(X1, X0, X2)) | ~ min_precedes(X0, X2, X1)), inference(subsumption_resolution, [], [f623, f319])).
fof(f319, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | arboreal(X0)), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ! [X0, X1, X2] : (arboreal(X0) | ~ min_precedes(X0, X1, X2)), inference(ennf_transformation, [], [f110])).
fof(f110, plain, ! [X0, X1, X2] : (min_precedes(X0, X1, X2) => arboreal(X0)), inference(rectify, [], [f47])).
fof(f47, plain, ! [X148, X149, X150] : (min_precedes(X148, X149, X150) => arboreal(X148)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+3.p', sos_46)).
fof(f623, plain, ! [X2, X0, X1] : (leaf_occ(X0, sK10(X1, X0, X2)) | ~ arboreal(X0) | next_subocc(X0, sK16(X0), tptp0) | ~ occurrence_of(sK10(X1, X0, X2), tptp0) | ~ min_precedes(X0, X2, X1)), inference(resolution, [], [f323, f289])).
fof(f289, plain, ! [X2, X0, X1] : (subactivity_occurrence(X1, sK10(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f221])).
fof(f3690, plain, ~ spl23_108, inference(avatar_contradiction_clause, [], [f3679])).
fof(f3679, plain, ($false | ~ spl23_108), inference(resolution, [], [f3664, f656])).
fof(f3664, plain, (! [X44] : ~ min_precedes(sK16(sK20), X44, tptp0) | ~ spl23_108), inference(avatar_component_clause, [], [f3663])).
fof(f3669, plain, (spl23_108 | spl23_109), inference(avatar_split_clause, [], [f3661, f3666, f3663])).
fof(f3661, plain, ! [X44] : (occurrence_of(sK16(sK16(sK20)), tptp3) | ~ min_precedes(sK16(sK20), X44, tptp0)), inference(subsumption_resolution, [], [f3656, f288])).
fof(f3656, plain, ! [X44] : (occurrence_of(sK16(sK16(sK20)), tptp3) | ~ min_precedes(sK16(sK20), X44, tptp0) | ~ occurrence_of(sK10(tptp0, sK16(sK20), X44), tptp0)), inference(resolution, [], [f1625, f815])).
fof(f1625, plain, ! [X0, X1] : (leaf_occ(X0, sK10(tptp0, X0, X1)) | occurrence_of(sK16(X0), tptp3) | ~ min_precedes(X0, X1, tptp0)), inference(duplicate_literal_removal, [], [f1624])).
fof(f1624, plain, ! [X0, X1] : (occurrence_of(sK16(X0), tptp3) | leaf_occ(X0, sK10(tptp0, X0, X1)) | ~ min_precedes(X0, X1, tptp0) | ~ min_precedes(X0, X1, tptp0)), inference(resolution, [], [f596, f288])).
fof(f596, plain, ! [X2, X0, X1] : (~ occurrence_of(sK10(X1, X0, X2), tptp0) | occurrence_of(sK16(X0), tptp3) | leaf_occ(X0, sK10(X1, X0, X2)) | ~ min_precedes(X0, X2, X1)), inference(subsumption_resolution, [], [f589, f319])).
fof(f589, plain, ! [X2, X0, X1] : (leaf_occ(X0, sK10(X1, X0, X2)) | ~ arboreal(X0) | occurrence_of(sK16(X0), tptp3) | ~ occurrence_of(sK10(X1, X0, X2), tptp0) | ~ min_precedes(X0, X2, X1)), inference(resolution, [], [f322, f289])).
fof(f322, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK16(X0), tptp3) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f2220, plain, ~ spl23_31, inference(avatar_contradiction_clause, [], [f2219])).
fof(f2219, plain, ($false | ~ spl23_31), inference(subsumption_resolution, [], [f2211, f344])).
fof(f2211, plain, (~ occurrence_of(sK21, tptp0) | ~ spl23_31), inference(resolution, [], [f2165, f345])).
fof(f2165, plain, (! [X0] : (~ subactivity_occurrence(sK20, X0) | ~ occurrence_of(X0, tptp0)) | ~ spl23_31), inference(avatar_component_clause, [], [f2164])).
fof(f702, plain, (spl23_5 | spl23_6), inference(avatar_split_clause, [], [f693, f699, f695])).
fof(f693, plain, (occurrence_of(sK18(sK20), tptp1) | occurrence_of(sK18(sK20), tptp2)), inference(subsumption_resolution, [], [f692, f344])).
fof(f692, plain, (occurrence_of(sK18(sK20), tptp1) | occurrence_of(sK18(sK20), tptp2) | ~ occurrence_of(sK21, tptp0)), inference(subsumption_resolution, [], [f691, f346])).
fof(f691, plain, (occurrence_of(sK18(sK20), tptp1) | ~ arboreal(sK20) | occurrence_of(sK18(sK20), tptp2) | ~ occurrence_of(sK21, tptp0)), inference(subsumption_resolution, [], [f686, f347])).
fof(f686, plain, (occurrence_of(sK18(sK20), tptp1) | leaf_occ(sK20, sK21) | ~ arboreal(sK20) | occurrence_of(sK18(sK20), tptp2) | ~ occurrence_of(sK21, tptp0)), inference(resolution, [], [f326, f345])).
fof(f326, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | occurrence_of(sK18(X0), tptp1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK18(X0), tptp2) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).