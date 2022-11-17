fof(f718, plain, $false, inference(avatar_sat_refutation, [], [f606, f610, f672, f684, f717])).
fof(f717, plain, ~ spl21_23, inference(avatar_contradiction_clause, [], [f716])).
fof(f716, plain, ($false | ~ spl21_23), inference(resolution, [], [f710, f280])).
fof(f280, plain, ~ arboreal(sK19), inference(subsumption_resolution, [], [f278, f254])).
fof(f254, plain, ~ atomic(tptp0), inference(cnf_transformation, [], [f38])).
fof(f38, plain, ~ atomic(tptp0), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos_37)).
fof(f278, plain, (~ arboreal(sK19) | atomic(tptp0)), inference(resolution, [], [f192, f269])).
fof(f269, plain, occurrence_of(sK19, tptp0), inference(cnf_transformation, [], [f182])).
fof(f182, plain, (! [X1, X2] : (((min_precedes(X1, sK20(X1), tptp0) & subactivity_occurrence(sK20(X1), sK19) & occurrence_of(sK20(X1), tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK19, X2) | ~ leaf_occ(X2, sK19)) & occurrence_of(sK19, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20])], [f138, f181, f180])).
fof(f180, plain, (? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)) => (! [X2, X1] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK19) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK19, X2) | ~ leaf_occ(X2, sK19)) & occurrence_of(sK19, tptp0))), introduced(choice_axiom, [])).
fof(f181, plain, ! [X1] : (? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK19) & occurrence_of(X3, tptp1)) => (min_precedes(X1, sK20(X1), tptp0) & subactivity_occurrence(sK20(X1), sK19) & occurrence_of(sK20(X1), tptp1))), introduced(choice_axiom, [])).
fof(f138, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)), inference(definition_folding, [], [f136, e137])).
fof(f137, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(usedef, [], [e137])).
fof(e137, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f136, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ~ ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2] : ((occurrence_of(X2, tptp2) => ~ ? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1))) & (occurrence_of(X2, tptp1) => ~ ? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2))) & leaf_occ(X2, X0))), inference(rectify, [], [f50])).
fof(f50, plain, ~ ! [X109] : (occurrence_of(X109, tptp0) => ? [X110, X111] : ((occurrence_of(X111, tptp2) => ~ ? [X113] : (min_precedes(X110, X113, tptp0) & subactivity_occurrence(X113, X109) & occurrence_of(X113, tptp1))) & (occurrence_of(X111, tptp1) => ~ ? [X112] : (min_precedes(X110, X112, tptp0) & subactivity_occurrence(X112, X109) & occurrence_of(X112, tptp2))) & leaf_occ(X111, X109))), inference(negated_conjecture, [], [f49])).
fof(f49, plain, ~ ! [X109] : (occurrence_of(X109, tptp0) => ? [X110, X111] : ((occurrence_of(X111, tptp2) => ~ ? [X113] : (min_precedes(X110, X113, tptp0) & subactivity_occurrence(X113, X109) & occurrence_of(X113, tptp1))) & (occurrence_of(X111, tptp1) => ~ ? [X112] : (min_precedes(X110, X112, tptp0) & subactivity_occurrence(X112, X109) & occurrence_of(X112, tptp2))) & leaf_occ(X111, X109))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', goals)).
fof(f192, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ arboreal(X0) | atomic(X1)), inference(cnf_transformation, [], [f141])).
fof(f141, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f99])).
fof(f99, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f8])).
fof(f8, plain, ! [X16, X17] : (occurrence_of(X16, X17) => (arboreal(X16) <=> atomic(X17))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos_07)).
fof(f710, plain, (! [X12] : arboreal(X12) | ~ spl21_23), inference(resolution, [], [f683, f381])).
fof(f381, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | arboreal(X0)), inference(resolution, [], [f201, f320])).
fof(f320, plain, ! [X0, X1] : (~ atocc(X0, X1) | arboreal(X0)), inference(subsumption_resolution, [], [f316, f221])).
fof(f221, plain, ! [X0, X1] : (atomic(sK8(X0, X1)) | ~ atocc(X0, X1)), inference(cnf_transformation, [], [f159])).
fof(f159, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & ((occurrence_of(X0, sK8(X0, X1)) & atomic(sK8(X0, X1)) & subactivity(X1, sK8(X0, X1))) | ~ atocc(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f157, f158])).
fof(f158, plain, ! [X1, X0] : (? [X3] : (occurrence_of(X0, X3) & atomic(X3) & subactivity(X1, X3)) => (occurrence_of(X0, sK8(X0, X1)) & atomic(sK8(X0, X1)) & subactivity(X1, sK8(X0, X1)))), introduced(choice_axiom, [])).
fof(f157, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & (? [X3] : (occurrence_of(X0, X3) & atomic(X3) & subactivity(X1, X3)) | ~ atocc(X0, X1))), inference(rectify, [], [f156])).
fof(f156, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & (? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2)) | ~ atocc(X0, X1))), inference(nnf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1] : (atocc(X0, X1) <=> ? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X64, X65] : (atocc(X64, X65) <=> ? [X66] : (occurrence_of(X64, X66) & atomic(X66) & subactivity(X65, X66))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos_23)).
fof(f316, plain, ! [X0, X1] : (~ atocc(X0, X1) | ~ atomic(sK8(X0, X1)) | arboreal(X0)), inference(resolution, [], [f222, f193])).
fof(f193, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ atomic(X1) | arboreal(X0)), inference(cnf_transformation, [], [f141])).
fof(f222, plain, ! [X0, X1] : (occurrence_of(X0, sK8(X0, X1)) | ~ atocc(X0, X1)), inference(cnf_transformation, [], [f159])).
fof(f201, plain, ! [X2, X0, X1] : (atocc(X1, sK2(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f145])).
fof(f145, plain, ! [X0, X1, X2] : ((atocc(X2, sK3(X0, X1, X2)) & atocc(X1, sK2(X0, X1, X2)) & subactivity(sK3(X0, X1, X2), X0) & subactivity(sK2(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3])], [f103, f144])).
fof(f144, plain, ! [X2, X1, X0] : (? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0)) => (atocc(X2, sK3(X0, X1, X2)) & atocc(X1, sK2(X0, X1, X2)) & subactivity(sK3(X0, X1, X2), X0) & subactivity(sK2(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f103, plain, ! [X0, X1, X2] : (? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f61])).
fof(f61, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0))), inference(rectify, [], [f12])).
fof(f12, plain, ! [X23, X24, X25] : (min_precedes(X24, X25, X23) => ? [X26, X27] : (atocc(X25, X27) & atocc(X24, X26) & subactivity(X27, X23) & subactivity(X26, X23))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos_11)).
fof(f683, plain, (! [X0] : min_precedes(X0, sK18(X0, sK19), tptp0) | ~ spl21_23), inference(avatar_component_clause, [], [f682])).
fof(f682, plain, (spl21_23 <=> ! [X0] : min_precedes(X0, sK18(X0, sK19), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl21_23])])).
fof(f684, plain, (spl21_5 | spl21_23), inference(avatar_split_clause, [], [f436, f682, f366])).
fof(f366, plain, (spl21_5 <=> ! [X0] : (occurrence_of(X0, tptp2) | ~ leaf_occ(X0, sK19))), introduced(avatar_definition, [new_symbols(naming, [spl21_5])])).
fof(f436, plain, ! [X0, X1] : (min_precedes(X0, sK18(X0, sK19), tptp0) | occurrence_of(X1, tptp2) | ~ leaf_occ(X1, sK19)), inference(resolution, [], [f268, f270])).
fof(f270, plain, ! [X2, X1] : (sP0(X1, sK19, X2) | occurrence_of(X2, tptp2) | ~ leaf_occ(X2, sK19)), inference(cnf_transformation, [], [f182])).
fof(f268, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | min_precedes(X0, sK18(X0, X1), tptp0)), inference(cnf_transformation, [], [f179])).
fof(f179, plain, ! [X0, X1, X2] : (((min_precedes(X0, sK18(X0, X1), tptp0) & subactivity_occurrence(sK18(X0, X1), X1) & occurrence_of(sK18(X0, X1), tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18])], [f177, f178])).
fof(f178, plain, ! [X1, X0] : (? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) => (min_precedes(X0, sK18(X0, X1), tptp0) & subactivity_occurrence(sK18(X0, X1), X1) & occurrence_of(sK18(X0, X1), tptp2))), introduced(choice_axiom, [])).
fof(f177, plain, ! [X0, X1, X2] : ((? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(rectify, [], [f176])).
fof(f176, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(nnf_transformation, [], [f137])).
fof(f672, plain, ~ spl21_19, inference(avatar_contradiction_clause, [], [f671])).
fof(f671, plain, ($false | ~ spl21_19), inference(resolution, [], [f662, f280])).
fof(f662, plain, (! [X12] : arboreal(X12) | ~ spl21_19), inference(resolution, [], [f609, f381])).
fof(f609, plain, (! [X6] : min_precedes(X6, sK20(X6), tptp0) | ~ spl21_19), inference(avatar_component_clause, [], [f608])).
fof(f608, plain, (spl21_19 <=> ! [X6] : min_precedes(X6, sK20(X6), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl21_19])])).
fof(f610, plain, (spl21_11 | spl21_19), inference(avatar_split_clause, [], [f519, f608, f460])).
fof(f460, plain, (spl21_11 <=> ! [X7] : (~ leaf_occ(X7, sK19) | occurrence_of(X7, tptp1))), introduced(avatar_definition, [new_symbols(naming, [spl21_11])])).
fof(f519, plain, ! [X6, X7] : (min_precedes(X6, sK20(X6), tptp0) | ~ leaf_occ(X7, sK19) | occurrence_of(X7, tptp1)), inference(resolution, [], [f273, f265])).
fof(f265, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | occurrence_of(X2, tptp1)), inference(cnf_transformation, [], [f179])).
fof(f273, plain, ! [X2, X1] : (min_precedes(X1, sK20(X1), tptp0) | sP0(X1, sK19, X2) | ~ leaf_occ(X2, sK19)), inference(cnf_transformation, [], [f182])).
fof(f606, plain, (~ spl21_5 | ~ spl21_11), inference(avatar_contradiction_clause, [], [f605])).
fof(f605, plain, ($false | (~ spl21_5 | ~ spl21_11)), inference(subsumption_resolution, [], [f604, f264])).
fof(f264, plain, ~ (tptp1 = tptp2), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ~ (tptp1 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos_47)).
fof(f604, plain, ((tptp1 = tptp2) | (~ spl21_5 | ~ spl21_11)), inference(backward_demodulation, [], [f600, f601])).
fof(f601, plain, ((tptp1 = sK1(sK17(sK19))) | ~ spl21_11), inference(resolution, [], [f342, f486])).
fof(f486, plain, (occurrence_of(sK17(sK19), tptp1) | ~ spl21_11), inference(subsumption_resolution, [], [f485, f269])).
fof(f485, plain, (occurrence_of(sK17(sK19), tptp1) | ~ occurrence_of(sK19, tptp0) | ~ spl21_11), inference(resolution, [], [f461, f252])).
fof(f252, plain, ! [X0] : (leaf_occ(sK17(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ! [X0] : ((leaf_occ(sK17(X0), X0) & next_subocc(sK16(X0), sK17(X0), tptp0) & (occurrence_of(sK17(X0), tptp2) | occurrence_of(sK17(X0), tptp1)) & next_subocc(sK15(X0), sK16(X0), tptp0) & occurrence_of(sK16(X0), tptp4) & root_occ(sK15(X0), X0) & occurrence_of(sK15(X0), tptp3)) | ~ occurrence_of(X0, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15, sK16, sK17])], [f135, f174])).
fof(f174, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) => (leaf_occ(sK17(X0), X0) & next_subocc(sK16(X0), sK17(X0), tptp0) & (occurrence_of(sK17(X0), tptp2) | occurrence_of(sK17(X0), tptp1)) & next_subocc(sK15(X0), sK16(X0), tptp0) & occurrence_of(sK16(X0), tptp4) & root_occ(sK15(X0), X0) & occurrence_of(sK15(X0), tptp3))), introduced(choice_axiom, [])).
fof(f135, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) | ~ occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3))), inference(rectify, [], [f36])).
fof(f36, plain, ! [X105] : (occurrence_of(X105, tptp0) => ? [X106, X107, X108] : (leaf_occ(X108, X105) & next_subocc(X107, X108, tptp0) & (occurrence_of(X108, tptp2) | occurrence_of(X108, tptp1)) & next_subocc(X106, X107, tptp0) & occurrence_of(X107, tptp4) & root_occ(X106, X105) & occurrence_of(X106, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos_35)).
fof(f461, plain, (! [X7] : (~ leaf_occ(X7, sK19) | occurrence_of(X7, tptp1)) | ~ spl21_11), inference(avatar_component_clause, [], [f460])).
fof(f342, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | (sK1(X1) = X0)), inference(subsumption_resolution, [], [f335, f184])).
fof(f184, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ! [X0, X1] : ((activity_occurrence(X1) & activity(X0)) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos)).
fof(f335, plain, ! [X0, X1] : ((sK1(X1) = X0) | ~ occurrence_of(X1, X0) | ~ activity_occurrence(X1)), inference(resolution, [], [f187, f186])).
fof(f186, plain, ! [X0] : (occurrence_of(X0, sK1(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0] : ((occurrence_of(X0, sK1(X0)) & activity(sK1(X0))) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f90, f139])).
fof(f139, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) => (occurrence_of(X0, sK1(X0)) & activity(sK1(X0)))), introduced(choice_axiom, [])).
fof(f90, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f2])).
fof(f2, plain, ! [X2] : (activity_occurrence(X2) => ? [X3] : (occurrence_of(X2, X3) & activity(X3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos_01)).
fof(f187, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f91])).
fof(f91, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f3])).
fof(f3, plain, ! [X4, X5, X6] : ((occurrence_of(X4, X6) & occurrence_of(X4, X5)) => (X5 = X6)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+1.p', sos_02)).
fof(f600, plain, ((tptp2 = sK1(sK17(sK19))) | ~ spl21_5), inference(resolution, [], [f342, f374])).
fof(f374, plain, (occurrence_of(sK17(sK19), tptp2) | ~ spl21_5), inference(subsumption_resolution, [], [f373, f269])).
fof(f373, plain, (occurrence_of(sK17(sK19), tptp2) | ~ occurrence_of(sK19, tptp0) | ~ spl21_5), inference(resolution, [], [f367, f252])).
fof(f367, plain, (! [X0] : (~ leaf_occ(X0, sK19) | occurrence_of(X0, tptp2)) | ~ spl21_5), inference(avatar_component_clause, [], [f366])).