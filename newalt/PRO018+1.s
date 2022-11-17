fof(f87355, plain, $false, inference(avatar_sat_refutation, [], [f2241, f2252, f4615, f40736, f45785, f45946, f46025, f86306, f86322, f86718, f87041, f87240, f87277])).
fof(f87277, plain, (~ spl22_43 | ~ spl22_372 | spl22_565 | spl22_848), inference(avatar_contradiction_clause, [], [f87276])).
fof(f87276, plain, ($false | (~ spl22_43 | ~ spl22_372 | spl22_565 | spl22_848)), inference(subsumption_resolution, [], [f87246, f25397])).
fof(f25397, plain, (~ occurrence_of(sK15(sK15(sK19)), tptp3) | spl22_565), inference(avatar_component_clause, [], [f25396])).
fof(f25396, plain, (spl22_565 <=> occurrence_of(sK15(sK15(sK19)), tptp3)), introduced(avatar_definition, [new_symbols(naming, [spl22_565])])).
fof(f87246, plain, (occurrence_of(sK15(sK15(sK19)), tptp3) | (~ spl22_43 | ~ spl22_372 | spl22_848)), inference(subsumption_resolution, [], [f87245, f2240])).
fof(f2240, plain, (occurrence_of(sK10(tptp0, sK19, sK15(sK19)), tptp0) | ~ spl22_43), inference(avatar_component_clause, [], [f2238])).
fof(f2238, plain, (spl22_43 <=> occurrence_of(sK10(tptp0, sK19, sK15(sK19)), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl22_43])])).
fof(f87245, plain, (occurrence_of(sK15(sK15(sK19)), tptp3) | ~ occurrence_of(sK10(tptp0, sK19, sK15(sK19)), tptp0) | (~ spl22_372 | spl22_848)), inference(subsumption_resolution, [], [f87244, f493])).
fof(f493, plain, arboreal(sK15(sK19)), inference(subsumption_resolution, [], [f490, f259])).
fof(f259, plain, atomic(tptp3), inference(cnf_transformation, [], [f42])).
fof(f42, plain, atomic(tptp3), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_41)).
fof(f490, plain, (~ atomic(tptp3) | arboreal(sK15(sK19))), inference(resolution, [], [f481, f195])).
fof(f195, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ atomic(X1) | arboreal(X0)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f97])).
fof(f97, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f8])).
fof(f8, plain, ! [X16, X17] : (occurrence_of(X16, X17) => (arboreal(X16) <=> atomic(X17))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_07)).
fof(f481, plain, occurrence_of(sK15(sK19), tptp3), inference(subsumption_resolution, [], [f480, f269])).
fof(f269, plain, occurrence_of(sK20, tptp0), inference(cnf_transformation, [], [f184])).
fof(f184, plain, (! [X2, X3] : (((min_precedes(X2, sK21(X2), tptp0) & occurrence_of(sK21(X2), tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK20) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK19, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK19, sK20) & arboreal(sK19) & subactivity_occurrence(sK19, sK20) & occurrence_of(sK20, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20, sK21])], [f137, f183, f182])).
fof(f182, plain, (? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => (! [X3, X2] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK20) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK19, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK19, sK20) & arboreal(sK19) & subactivity_occurrence(sK19, sK20) & occurrence_of(sK20, tptp0))), introduced(choice_axiom, [])).
fof(f183, plain, ! [X2] : (? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) => (min_precedes(X2, sK21(X2), tptp0) & occurrence_of(sK21(X2), tptp1))), introduced(choice_axiom, [])).
fof(f137, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(definition_folding, [], [f135, e136])).
fof(f136, plain, ! [X2, X3] : ((? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ sP0(X2, X3)), inference(usedef, [], [e136])).
fof(e136, plain, ! [X2, X3] : (sP0(X2, X3) <=> (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f135, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(flattening, [], [f134])).
fof(f134, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & (~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ~ ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3] : ((occurrence_of(X3, tptp2) => ~ ? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1))) & (occurrence_of(X3, tptp1) => ~ ? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2))) & leaf_occ(X3, X1) & min_precedes(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f50])).
fof(f50, plain, ~ ! [X111, X112] : ((~ leaf_occ(X111, X112) & arboreal(X111) & subactivity_occurrence(X111, X112) & occurrence_of(X112, tptp0)) => ? [X113, X114] : ((occurrence_of(X114, tptp2) => ~ ? [X116] : (min_precedes(X113, X116, tptp0) & occurrence_of(X116, tptp1))) & (occurrence_of(X114, tptp1) => ~ ? [X115] : (min_precedes(X113, X115, tptp0) & occurrence_of(X115, tptp2))) & leaf_occ(X114, X112) & min_precedes(X113, X114, tptp0) & (occurrence_of(X114, tptp2) | occurrence_of(X114, tptp1)) & next_subocc(X111, X113, tptp0) & occurrence_of(X113, tptp3))), inference(negated_conjecture, [], [f49])).
fof(f49, plain, ~ ! [X111, X112] : ((~ leaf_occ(X111, X112) & arboreal(X111) & subactivity_occurrence(X111, X112) & occurrence_of(X112, tptp0)) => ? [X113, X114] : ((occurrence_of(X114, tptp2) => ~ ? [X116] : (min_precedes(X113, X116, tptp0) & occurrence_of(X116, tptp1))) & (occurrence_of(X114, tptp1) => ~ ? [X115] : (min_precedes(X113, X115, tptp0) & occurrence_of(X115, tptp2))) & leaf_occ(X114, X112) & min_precedes(X113, X114, tptp0) & (occurrence_of(X114, tptp2) | occurrence_of(X114, tptp1)) & next_subocc(X111, X113, tptp0) & occurrence_of(X113, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', goals)).
fof(f480, plain, (occurrence_of(sK15(sK19), tptp3) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f479, f271])).
fof(f271, plain, arboreal(sK19), inference(cnf_transformation, [], [f184])).
fof(f479, plain, (~ arboreal(sK19) | occurrence_of(sK15(sK19), tptp3) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f474, f272])).
fof(f272, plain, ~ leaf_occ(sK19, sK20), inference(cnf_transformation, [], [f184])).
fof(f474, plain, (leaf_occ(sK19, sK20) | ~ arboreal(sK19) | occurrence_of(sK15(sK19), tptp3) | ~ occurrence_of(sK20, tptp0)), inference(resolution, [], [f247, f270])).
fof(f270, plain, subactivity_occurrence(sK19, sK20), inference(cnf_transformation, [], [f184])).
fof(f247, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK15(X0), tptp3) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f177])).
fof(f177, plain, ! [X0, X1] : ((! [X5] : ((sK17(X0) = X5) | (sK16(X0) = X5) | ~ min_precedes(sK15(X0), X5, tptp0)) & min_precedes(sK16(X0), sK17(X0), tptp0) & (occurrence_of(sK17(X0), tptp2) | occurrence_of(sK17(X0), tptp1)) & min_precedes(sK15(X0), sK16(X0), tptp0) & occurrence_of(sK16(X0), tptp4) & next_subocc(X0, sK15(X0), tptp0) & occurrence_of(sK15(X0), tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15, sK16, sK17])], [f133, f176])).
fof(f176, plain, ! [X0] : (? [X2, X3, X4] : (! [X5] : ((X4 = X5) | (X3 = X5) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) => (! [X5] : ((sK17(X0) = X5) | (sK16(X0) = X5) | ~ min_precedes(sK15(X0), X5, tptp0)) & min_precedes(sK16(X0), sK17(X0), tptp0) & (occurrence_of(sK17(X0), tptp2) | occurrence_of(sK17(X0), tptp1)) & min_precedes(sK15(X0), sK16(X0), tptp0) & occurrence_of(sK16(X0), tptp4) & next_subocc(X0, sK15(X0), tptp0) & occurrence_of(sK15(X0), tptp3))), introduced(choice_axiom, [])).
fof(f133, plain, ! [X0, X1] : (? [X2, X3, X4] : (! [X5] : ((X4 = X5) | (X3 = X5) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(flattening, [], [f132])).
fof(f132, plain, ! [X0, X1] : (? [X2, X3, X4] : (! [X5] : (((X4 = X5) | (X3 = X5)) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | (leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3, X4] : (! [X5] : (min_precedes(X2, X5, tptp0) => ((X4 = X5) | (X3 = X5))) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f36])).
fof(f36, plain, ! [X105, X106] : ((~ leaf_occ(X105, X106) & arboreal(X105) & subactivity_occurrence(X105, X106) & occurrence_of(X106, tptp0)) => ? [X107, X108, X109] : (! [X110] : (min_precedes(X107, X110, tptp0) => ((X109 = X110) | (X108 = X110))) & min_precedes(X108, X109, tptp0) & (occurrence_of(X109, tptp2) | occurrence_of(X109, tptp1)) & min_precedes(X107, X108, tptp0) & occurrence_of(X108, tptp4) & next_subocc(X105, X107, tptp0) & occurrence_of(X107, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_35)).
fof(f87244, plain, (~ arboreal(sK15(sK19)) | occurrence_of(sK15(sK15(sK19)), tptp3) | ~ occurrence_of(sK10(tptp0, sK19, sK15(sK19)), tptp0) | (~ spl22_372 | spl22_848)), inference(subsumption_resolution, [], [f40791, f40550])).
fof(f40550, plain, (~ leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | spl22_848), inference(avatar_component_clause, [], [f40549])).
fof(f40549, plain, (spl22_848 <=> leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19)))), introduced(avatar_definition, [new_symbols(naming, [spl22_848])])).
fof(f40791, plain, (leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | ~ arboreal(sK15(sK19)) | occurrence_of(sK15(sK15(sK19)), tptp3) | ~ occurrence_of(sK10(tptp0, sK19, sK15(sK19)), tptp0) | ~ spl22_372), inference(resolution, [], [f15550, f247])).
fof(f15550, plain, (subactivity_occurrence(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | ~ spl22_372), inference(avatar_component_clause, [], [f15548])).
fof(f15548, plain, (spl22_372 <=> subactivity_occurrence(sK15(sK19), sK10(tptp0, sK19, sK15(sK19)))), introduced(avatar_definition, [new_symbols(naming, [spl22_372])])).
fof(f87240, plain, (~ spl22_1 | ~ spl22_565 | ~ spl22_917), inference(avatar_contradiction_clause, [], [f87239])).
fof(f87239, plain, ($false | (~ spl22_1 | ~ spl22_565 | ~ spl22_917)), inference(subsumption_resolution, [], [f87238, f264])).
fof(f264, plain, ~ (tptp3 = tptp2), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ~ (tptp3 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_46)).
fof(f87238, plain, ((tptp3 = tptp2) | (~ spl22_1 | ~ spl22_565 | ~ spl22_917)), inference(forward_demodulation, [], [f87232, f87116])).
fof(f87116, plain, ((tptp3 = sK1(sK17(sK19))) | (~ spl22_565 | ~ spl22_917)), inference(backward_demodulation, [], [f25434, f45780])).
fof(f45780, plain, ((sK17(sK19) = sK15(sK15(sK19))) | ~ spl22_917), inference(avatar_component_clause, [], [f45778])).
fof(f45778, plain, (spl22_917 <=> (sK17(sK19) = sK15(sK15(sK19)))), introduced(avatar_definition, [new_symbols(naming, [spl22_917])])).
fof(f25434, plain, ((tptp3 = sK1(sK15(sK15(sK19)))) | ~ spl22_565), inference(resolution, [], [f25398, f309])).
fof(f309, plain, ! [X2, X1] : (~ occurrence_of(X2, X1) | (sK1(X2) = X1)), inference(subsumption_resolution, [], [f306, f186])).
fof(f186, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1] : ((activity_occurrence(X1) & activity(X0)) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos)).
fof(f306, plain, ! [X2, X1] : ((sK1(X2) = X1) | ~ occurrence_of(X2, X1) | ~ activity_occurrence(X2)), inference(resolution, [], [f189, f188])).
fof(f188, plain, ! [X0] : (occurrence_of(X0, sK1(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f139])).
fof(f139, plain, ! [X0] : ((occurrence_of(X0, sK1(X0)) & activity(sK1(X0))) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f88, f138])).
fof(f138, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) => (occurrence_of(X0, sK1(X0)) & activity(sK1(X0)))), introduced(choice_axiom, [])).
fof(f88, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f2])).
fof(f2, plain, ! [X2] : (activity_occurrence(X2) => ? [X3] : (occurrence_of(X2, X3) & activity(X3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_01)).
fof(f189, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f89])).
fof(f89, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f3])).
fof(f3, plain, ! [X4, X5, X6] : ((occurrence_of(X4, X6) & occurrence_of(X4, X5)) => (X5 = X6)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_02)).
fof(f25398, plain, (occurrence_of(sK15(sK15(sK19)), tptp3) | ~ spl22_565), inference(avatar_component_clause, [], [f25396])).
fof(f87232, plain, ((tptp2 = sK1(sK17(sK19))) | ~ spl22_1), inference(resolution, [], [f569, f309])).
fof(f569, plain, (occurrence_of(sK17(sK19), tptp2) | ~ spl22_1), inference(avatar_component_clause, [], [f567])).
fof(f567, plain, (spl22_1 <=> occurrence_of(sK17(sK19), tptp2)), introduced(avatar_definition, [new_symbols(naming, [spl22_1])])).
fof(f87041, plain, (spl22_728 | spl22_848 | ~ spl22_43 | ~ spl22_372), inference(avatar_split_clause, [], [f87040, f15548, f2238, f40549, f29860])).
fof(f29860, plain, (spl22_728 <=> next_subocc(sK15(sK19), sK15(sK15(sK19)), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl22_728])])).
fof(f87040, plain, (leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | next_subocc(sK15(sK19), sK15(sK15(sK19)), tptp0) | (~ spl22_43 | ~ spl22_372)), inference(subsumption_resolution, [], [f87039, f2240])).
fof(f87039, plain, (leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | next_subocc(sK15(sK19), sK15(sK15(sK19)), tptp0) | ~ occurrence_of(sK10(tptp0, sK19, sK15(sK19)), tptp0) | ~ spl22_372), inference(subsumption_resolution, [], [f40792, f493])).
fof(f40792, plain, (leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | ~ arboreal(sK15(sK19)) | next_subocc(sK15(sK19), sK15(sK15(sK19)), tptp0) | ~ occurrence_of(sK10(tptp0, sK19, sK15(sK19)), tptp0) | ~ spl22_372), inference(resolution, [], [f15550, f248])).
fof(f248, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | next_subocc(X0, sK15(X0), tptp0) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f177])).
fof(f86718, plain, (spl22_1 | spl22_2), inference(avatar_split_clause, [], [f86717, f571, f567])).
fof(f571, plain, (spl22_2 <=> occurrence_of(sK17(sK19), tptp1)), introduced(avatar_definition, [new_symbols(naming, [spl22_2])])).
fof(f86717, plain, (occurrence_of(sK17(sK19), tptp1) | occurrence_of(sK17(sK19), tptp2)), inference(subsumption_resolution, [], [f86716, f269])).
fof(f86716, plain, (occurrence_of(sK17(sK19), tptp1) | occurrence_of(sK17(sK19), tptp2) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f86715, f271])).
fof(f86715, plain, (occurrence_of(sK17(sK19), tptp1) | ~ arboreal(sK19) | occurrence_of(sK17(sK19), tptp2) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f17937, f272])).
fof(f17937, plain, (occurrence_of(sK17(sK19), tptp1) | leaf_occ(sK19, sK20) | ~ arboreal(sK19) | occurrence_of(sK17(sK19), tptp2) | ~ occurrence_of(sK20, tptp0)), inference(resolution, [], [f270, f251])).
fof(f251, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | occurrence_of(sK17(X0), tptp1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK17(X0), tptp2) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f177])).
fof(f86322, plain, (spl22_569 | ~ spl22_848), inference(avatar_split_clause, [], [f86321, f40549, f25416])).
fof(f25416, plain, (spl22_569 <=> (tptp0 = sK14(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))))), introduced(avatar_definition, [new_symbols(naming, [spl22_569])])).
fof(f86321, plain, ((tptp0 = sK14(sK15(sK19), sK10(tptp0, sK19, sK15(sK19)))) | ~ spl22_848), inference(subsumption_resolution, [], [f41264, f630])).
fof(f630, plain, min_precedes(sK19, sK15(sK19), tptp0), inference(resolution, [], [f514, f220])).
fof(f220, plain, ! [X2, X0, X1] : (~ next_subocc(X0, X1, X2) | min_precedes(X0, X1, X2)), inference(cnf_transformation, [], [f159])).
fof(f159, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (min_precedes(sK8(X0, X1, X2), X1, X2) & min_precedes(X0, sK8(X0, X1, X2), X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f157, f158])).
fof(f158, plain, ! [X2, X1, X0] : (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) => (min_precedes(sK8(X0, X1, X2), X1, X2) & min_precedes(X0, sK8(X0, X1, X2), X2))), introduced(choice_axiom, [])).
fof(f157, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(rectify, [], [f156])).
fof(f156, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(flattening, [], [f155])).
fof(f155, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2))) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(nnf_transformation, [], [f115])).
fof(f115, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f72])).
fof(f72, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (~ ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(rectify, [], [f23])).
fof(f23, plain, ! [X60, X61, X62] : (next_subocc(X60, X61, X62) <=> (~ ? [X63] : (min_precedes(X63, X61, X62) & min_precedes(X60, X63, X62)) & min_precedes(X60, X61, X62))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_22)).
fof(f514, plain, next_subocc(sK19, sK15(sK19), tptp0), inference(subsumption_resolution, [], [f513, f269])).
fof(f513, plain, (next_subocc(sK19, sK15(sK19), tptp0) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f512, f271])).
fof(f512, plain, (~ arboreal(sK19) | next_subocc(sK19, sK15(sK19), tptp0) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f507, f272])).
fof(f507, plain, (leaf_occ(sK19, sK20) | ~ arboreal(sK19) | next_subocc(sK19, sK15(sK19), tptp0) | ~ occurrence_of(sK20, tptp0)), inference(resolution, [], [f248, f270])).
fof(f41264, plain, (~ min_precedes(sK19, sK15(sK19), tptp0) | (tptp0 = sK14(sK15(sK19), sK10(tptp0, sK19, sK15(sK19)))) | ~ spl22_848), inference(resolution, [], [f40551, f842])).
fof(f842, plain, ! [X12, X10, X13, X11] : (~ leaf_occ(X11, sK10(X10, X12, X13)) | ~ min_precedes(X12, X13, X10) | (sK14(X11, sK10(X10, X12, X13)) = X10)), inference(resolution, [], [f326, f243])).
fof(f243, plain, ! [X0, X1] : (occurrence_of(X1, sK14(X0, X1)) | ~ leaf_occ(X0, X1)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & ((leaf(X0, sK14(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK14(X0, X1))) | ~ leaf_occ(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f173, f174])).
fof(f174, plain, ! [X1, X0] : (? [X3] : (leaf(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) => (leaf(X0, sK14(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f173, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X3] : (leaf(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) | ~ leaf_occ(X0, X1))), inference(rectify, [], [f172])).
fof(f172, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X2] : (leaf(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2)) | ~ leaf_occ(X0, X1))), inference(nnf_transformation, [], [f84])).
fof(f84, plain, ! [X0, X1] : (leaf_occ(X0, X1) <=> ? [X2] : (leaf(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2))), inference(rectify, [], [f35])).
fof(f35, plain, ! [X102, X103] : (leaf_occ(X102, X103) <=> ? [X104] : (leaf(X102, X104) & subactivity_occurrence(X102, X103) & occurrence_of(X103, X104))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_34)).
fof(f326, plain, ! [X2, X0, X3, X1] : (~ occurrence_of(sK10(X2, X0, X1), X3) | (X2 = X3) | ~ min_precedes(X0, X1, X2)), inference(resolution, [], [f230, f189])).
fof(f230, plain, ! [X2, X0, X1] : (occurrence_of(sK10(X0, X1, X2), X0) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f165])).
fof(f165, plain, ! [X0, X1, X2] : ((subactivity_occurrence(X2, sK10(X0, X1, X2)) & subactivity_occurrence(X1, sK10(X0, X1, X2)) & occurrence_of(sK10(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f117, f164])).
fof(f164, plain, ! [X2, X1, X0] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) => (subactivity_occurrence(X2, sK10(X0, X1, X2)) & subactivity_occurrence(X1, sK10(X0, X1, X2)) & occurrence_of(sK10(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f117, plain, ! [X0, X1, X2] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0))), inference(rectify, [], [f26])).
fof(f26, plain, ! [X69, X70, X71] : (min_precedes(X70, X71, X69) => ? [X72] : (subactivity_occurrence(X71, X72) & subactivity_occurrence(X70, X72) & occurrence_of(X72, X69))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_25)).
fof(f40551, plain, (leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | ~ spl22_848), inference(avatar_component_clause, [], [f40549])).
fof(f86306, plain, (~ spl22_569 | ~ spl22_848), inference(avatar_contradiction_clause, [], [f86305])).
fof(f86305, plain, ($false | (~ spl22_569 | ~ spl22_848)), inference(subsumption_resolution, [], [f86304, f40551])).
fof(f86304, plain, (~ leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | ~ spl22_569), inference(subsumption_resolution, [], [f86291, f662])).
fof(f662, plain, ~ leaf(sK15(sK19), tptp0), inference(resolution, [], [f541, f217])).
fof(f217, plain, ! [X4, X0, X1] : (~ min_precedes(X0, X4, X1) | ~ leaf(X0, X1)), inference(cnf_transformation, [], [f154])).
fof(f154, plain, ! [X0, X1] : ((leaf(X0, X1) | min_precedes(X0, sK6(X0, X1), X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X4] : ~ min_precedes(X0, X4, X1) & (min_precedes(sK7(X0, X1), X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7])], [f151, f153, f152])).
fof(f152, plain, ! [X1, X0] : (? [X2] : min_precedes(X0, X2, X1) => min_precedes(X0, sK6(X0, X1), X1)), introduced(choice_axiom, [])).
fof(f153, plain, ! [X1, X0] : (? [X5] : min_precedes(X5, X0, X1) => min_precedes(sK7(X0, X1), X0, X1)), introduced(choice_axiom, [])).
fof(f151, plain, ! [X0, X1] : ((leaf(X0, X1) | ? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X4] : ~ min_precedes(X0, X4, X1) & (? [X5] : min_precedes(X5, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(rectify, [], [f150])).
fof(f150, plain, ! [X0, X1] : ((leaf(X0, X1) | ? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(flattening, [], [f149])).
fof(f149, plain, ! [X0, X1] : ((leaf(X0, X1) | (? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1)))) & ((! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(nnf_transformation, [], [f114])).
fof(f114, plain, ! [X0, X1] : (leaf(X0, X1) <=> (! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1)))), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1] : (leaf(X0, X1) <=> (~ ? [X2] : min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1)))), inference(rectify, [], [f22])).
fof(f22, plain, ! [X56, X57] : (leaf(X56, X57) <=> (~ ? [X59] : min_precedes(X56, X59, X57) & (? [X58] : min_precedes(X58, X56, X57) | root(X56, X57)))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_21)).
fof(f541, plain, min_precedes(sK15(sK19), sK16(sK19), tptp0), inference(subsumption_resolution, [], [f540, f269])).
fof(f540, plain, (min_precedes(sK15(sK19), sK16(sK19), tptp0) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f539, f271])).
fof(f539, plain, (~ arboreal(sK19) | min_precedes(sK15(sK19), sK16(sK19), tptp0) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f534, f272])).
fof(f534, plain, (leaf_occ(sK19, sK20) | ~ arboreal(sK19) | min_precedes(sK15(sK19), sK16(sK19), tptp0) | ~ occurrence_of(sK20, tptp0)), inference(resolution, [], [f250, f270])).
fof(f250, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | min_precedes(sK15(X0), sK16(X0), tptp0) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f177])).
fof(f86291, plain, (leaf(sK15(sK19), tptp0) | ~ leaf_occ(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | ~ spl22_569), inference(superposition, [], [f245, f25418])).
fof(f25418, plain, ((tptp0 = sK14(sK15(sK19), sK10(tptp0, sK19, sK15(sK19)))) | ~ spl22_569), inference(avatar_component_clause, [], [f25416])).
fof(f245, plain, ! [X0, X1] : (leaf(X0, sK14(X0, X1)) | ~ leaf_occ(X0, X1)), inference(cnf_transformation, [], [f175])).
fof(f46025, plain, (~ spl22_565 | ~ spl22_918), inference(avatar_contradiction_clause, [], [f46024])).
fof(f46024, plain, ($false | (~ spl22_565 | ~ spl22_918)), inference(subsumption_resolution, [], [f46023, f260])).
fof(f260, plain, ~ (tptp3 = tptp4), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ~ (tptp3 = tptp4), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_42)).
fof(f46023, plain, ((tptp3 = tptp4) | (~ spl22_565 | ~ spl22_918)), inference(backward_demodulation, [], [f530, f45973])).
fof(f45973, plain, ((tptp3 = sK1(sK16(sK19))) | (~ spl22_565 | ~ spl22_918)), inference(backward_demodulation, [], [f25434, f45784])).
fof(f45784, plain, ((sK16(sK19) = sK15(sK15(sK19))) | ~ spl22_918), inference(avatar_component_clause, [], [f45782])).
fof(f45782, plain, (spl22_918 <=> (sK16(sK19) = sK15(sK15(sK19)))), introduced(avatar_definition, [new_symbols(naming, [spl22_918])])).
fof(f530, plain, (tptp4 = sK1(sK16(sK19))), inference(resolution, [], [f502, f309])).
fof(f502, plain, occurrence_of(sK16(sK19), tptp4), inference(subsumption_resolution, [], [f501, f269])).
fof(f501, plain, (occurrence_of(sK16(sK19), tptp4) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f500, f271])).
fof(f500, plain, (~ arboreal(sK19) | occurrence_of(sK16(sK19), tptp4) | ~ occurrence_of(sK20, tptp0)), inference(subsumption_resolution, [], [f495, f272])).
fof(f495, plain, (leaf_occ(sK19, sK20) | ~ arboreal(sK19) | occurrence_of(sK16(sK19), tptp4) | ~ occurrence_of(sK20, tptp0)), inference(resolution, [], [f249, f270])).
fof(f249, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK16(X0), tptp4) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f177])).
fof(f45946, plain, (~ spl22_2 | ~ spl22_565 | ~ spl22_917), inference(avatar_contradiction_clause, [], [f45945])).
fof(f45945, plain, ($false | (~ spl22_2 | ~ spl22_565 | ~ spl22_917)), inference(subsumption_resolution, [], [f45944, f263])).
fof(f263, plain, ~ (tptp3 = tptp1), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ~ (tptp3 = tptp1), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+1.p', sos_45)).
fof(f45944, plain, ((tptp3 = tptp1) | (~ spl22_2 | ~ spl22_565 | ~ spl22_917)), inference(backward_demodulation, [], [f8129, f45895])).
fof(f45895, plain, ((tptp3 = sK1(sK17(sK19))) | (~ spl22_565 | ~ spl22_917)), inference(backward_demodulation, [], [f25434, f45780])).
fof(f8129, plain, ((tptp1 = sK1(sK17(sK19))) | ~ spl22_2), inference(resolution, [], [f573, f309])).
fof(f573, plain, (occurrence_of(sK17(sK19), tptp1) | ~ spl22_2), inference(avatar_component_clause, [], [f571])).
fof(f45785, plain, (spl22_27 | spl22_917 | spl22_918 | ~ spl22_728), inference(avatar_split_clause, [], [f45776, f29860, f45782, f45778, f1770])).
fof(f1770, plain, (spl22_27 <=> ! [X0] : (leaf_occ(sK19, X0) | ~ occurrence_of(X0, tptp0) | ~ subactivity_occurrence(sK19, X0))), introduced(avatar_definition, [new_symbols(naming, [spl22_27])])).
fof(f45776, plain, (! [X2] : ((sK16(sK19) = sK15(sK15(sK19))) | (sK17(sK19) = sK15(sK15(sK19))) | leaf_occ(sK19, X2) | ~ subactivity_occurrence(sK19, X2) | ~ occurrence_of(X2, tptp0)) | ~ spl22_728), inference(subsumption_resolution, [], [f45668, f271])).
fof(f45668, plain, (! [X2] : ((sK16(sK19) = sK15(sK15(sK19))) | (sK17(sK19) = sK15(sK15(sK19))) | leaf_occ(sK19, X2) | ~ arboreal(sK19) | ~ subactivity_occurrence(sK19, X2) | ~ occurrence_of(X2, tptp0)) | ~ spl22_728), inference(resolution, [], [f45651, f253])).
fof(f253, plain, ! [X0, X5, X1] : (~ min_precedes(sK15(X0), X5, tptp0) | (sK16(X0) = X5) | (sK17(X0) = X5) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f177])).
fof(f45651, plain, (min_precedes(sK15(sK19), sK15(sK15(sK19)), tptp0) | ~ spl22_728), inference(resolution, [], [f29862, f220])).
fof(f29862, plain, (next_subocc(sK15(sK19), sK15(sK15(sK19)), tptp0) | ~ spl22_728), inference(avatar_component_clause, [], [f29860])).
fof(f40736, plain, spl22_372, inference(avatar_contradiction_clause, [], [f40735])).
fof(f40735, plain, ($false | spl22_372), inference(subsumption_resolution, [], [f40732, f630])).
fof(f40732, plain, (~ min_precedes(sK19, sK15(sK19), tptp0) | spl22_372), inference(resolution, [], [f15549, f232])).
fof(f232, plain, ! [X2, X0, X1] : (subactivity_occurrence(X2, sK10(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f165])).
fof(f15549, plain, (~ subactivity_occurrence(sK15(sK19), sK10(tptp0, sK19, sK15(sK19))) | spl22_372), inference(avatar_component_clause, [], [f15548])).
fof(f4615, plain, ~ spl22_27, inference(avatar_contradiction_clause, [], [f4614])).
fof(f4614, plain, ($false | ~ spl22_27), inference(subsumption_resolution, [], [f4613, f272])).
fof(f4613, plain, (leaf_occ(sK19, sK20) | ~ spl22_27), inference(subsumption_resolution, [], [f4606, f269])).
fof(f4606, plain, (~ occurrence_of(sK20, tptp0) | leaf_occ(sK19, sK20) | ~ spl22_27), inference(resolution, [], [f1771, f270])).
fof(f1771, plain, (! [X0] : (~ subactivity_occurrence(sK19, X0) | ~ occurrence_of(X0, tptp0) | leaf_occ(sK19, X0)) | ~ spl22_27), inference(avatar_component_clause, [], [f1770])).
fof(f2252, plain, spl22_41, inference(avatar_contradiction_clause, [], [f2251])).
fof(f2251, plain, ($false | spl22_41), inference(subsumption_resolution, [], [f2250, f630])).
fof(f2250, plain, (~ min_precedes(sK19, sK15(sK19), tptp0) | spl22_41), inference(resolution, [], [f2231, f329])).
fof(f329, plain, ! [X12, X10, X11] : (activity_occurrence(sK10(X12, X10, X11)) | ~ min_precedes(X10, X11, X12)), inference(resolution, [], [f230, f186])).
fof(f2231, plain, (~ activity_occurrence(sK10(tptp0, sK19, sK15(sK19))) | spl22_41), inference(avatar_component_clause, [], [f2229])).
fof(f2229, plain, (spl22_41 <=> activity_occurrence(sK10(tptp0, sK19, sK15(sK19)))), introduced(avatar_definition, [new_symbols(naming, [spl22_41])])).
fof(f2241, plain, (~ spl22_41 | spl22_43), inference(avatar_split_clause, [], [f2225, f2238, f2229])).
fof(f2225, plain, (occurrence_of(sK10(tptp0, sK19, sK15(sK19)), tptp0) | ~ activity_occurrence(sK10(tptp0, sK19, sK15(sK19)))), inference(superposition, [], [f188, f2051])).
fof(f2051, plain, (tptp0 = sK1(sK10(tptp0, sK19, sK15(sK19)))), inference(resolution, [], [f347, f630])).
fof(f347, plain, ! [X6, X7, X5] : (~ min_precedes(X6, X7, X5) | (sK1(sK10(X5, X6, X7)) = X5)), inference(resolution, [], [f309, f230])).