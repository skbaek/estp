fof(f716, plain, $false, inference(avatar_sat_refutation, [], [f456, f489, f609, f613, f715])).
fof(f715, plain, ~ spl19_18, inference(avatar_contradiction_clause, [], [f714])).
fof(f714, plain, ($false | ~ spl19_18), inference(resolution, [], [f698, f255])).
fof(f255, plain, occurrence_of(sK17, tptp0), inference(cnf_transformation, [], [f177])).
fof(f177, plain, (! [X1, X2] : (((min_precedes(X1, sK18(X1), tptp0) & subactivity_occurrence(sK18(X1), sK17) & occurrence_of(sK18(X1), tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17)) & occurrence_of(sK17, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK17, sK18])], [f132, f176, f175])).
fof(f175, plain, (? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)) => (! [X2, X1] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK17) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17)) & occurrence_of(sK17, tptp0))), introduced(choice_axiom, [])).
fof(f176, plain, ! [X1] : (? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK17) & occurrence_of(X3, tptp1)) => (min_precedes(X1, sK18(X1), tptp0) & subactivity_occurrence(sK18(X1), sK17) & occurrence_of(sK18(X1), tptp1))), introduced(choice_axiom, [])).
fof(f132, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)), inference(definition_folding, [], [f130, e131])).
fof(f131, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(usedef, [], [e131])).
fof(e131, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f130, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ~ ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2] : ((occurrence_of(X2, tptp2) => ~ ? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1))) & (occurrence_of(X2, tptp1) => ~ ? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2))) & leaf_occ(X2, X0))), inference(rectify, [], [f47])).
fof(f47, plain, ~ ! [X105] : (occurrence_of(X105, tptp0) => ? [X106, X107] : ((occurrence_of(X107, tptp2) => ~ ? [X109] : (min_precedes(X106, X109, tptp0) & subactivity_occurrence(X109, X105) & occurrence_of(X109, tptp1))) & (occurrence_of(X107, tptp1) => ~ ? [X108] : (min_precedes(X106, X108, tptp0) & subactivity_occurrence(X108, X105) & occurrence_of(X108, tptp2))) & leaf_occ(X107, X105))), inference(negated_conjecture, [], [f46])).
fof(f46, plain, ~ ! [X105] : (occurrence_of(X105, tptp0) => ? [X106, X107] : ((occurrence_of(X107, tptp2) => ~ ? [X109] : (min_precedes(X106, X109, tptp0) & subactivity_occurrence(X109, X105) & occurrence_of(X109, tptp1))) & (occurrence_of(X107, tptp1) => ~ ? [X108] : (min_precedes(X106, X108, tptp0) & subactivity_occurrence(X108, X105) & occurrence_of(X108, tptp2))) & leaf_occ(X107, X105))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+4.p', goals)).
fof(f698, plain, (! [X0] : ~ occurrence_of(X0, tptp0) | ~ spl19_18), inference(duplicate_literal_removal, [], [f696])).
fof(f696, plain, (! [X0] : (~ occurrence_of(X0, tptp0) | ~ occurrence_of(X0, tptp0)) | ~ spl19_18), inference(resolution, [], [f628, f239])).
fof(f239, plain, ! [X0] : (leaf_occ(sK15(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ! [X0] : ((leaf_occ(sK15(X0), X0) & next_subocc(sK14(X0), sK15(X0), tptp0) & (occurrence_of(sK15(X0), tptp2) | occurrence_of(sK15(X0), tptp1)) & next_subocc(sK13(X0), sK14(X0), tptp0) & occurrence_of(sK14(X0), tptp4) & root_occ(sK13(X0), X0) & occurrence_of(sK13(X0), tptp3)) | ~ occurrence_of(X0, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14, sK15])], [f129, f169])).
fof(f169, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) => (leaf_occ(sK15(X0), X0) & next_subocc(sK14(X0), sK15(X0), tptp0) & (occurrence_of(sK15(X0), tptp2) | occurrence_of(sK15(X0), tptp1)) & next_subocc(sK13(X0), sK14(X0), tptp0) & occurrence_of(sK14(X0), tptp4) & root_occ(sK13(X0), X0) & occurrence_of(sK13(X0), tptp3))), introduced(choice_axiom, [])).
fof(f129, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) | ~ occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3))), inference(rectify, [], [f33])).
fof(f33, plain, ! [X101] : (occurrence_of(X101, tptp0) => ? [X102, X103, X104] : (leaf_occ(X104, X101) & next_subocc(X103, X104, tptp0) & (occurrence_of(X104, tptp2) | occurrence_of(X104, tptp1)) & next_subocc(X102, X103, tptp0) & occurrence_of(X103, tptp4) & root_occ(X102, X101) & occurrence_of(X102, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+4.p', sos_32)).
fof(f628, plain, (! [X4, X3] : (~ leaf_occ(X3, X4) | ~ occurrence_of(X4, tptp0)) | ~ spl19_18), inference(resolution, [], [f612, f191])).
fof(f191, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(cnf_transformation, [], [f103])).
fof(f103, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(flattening, [], [f102])).
fof(f102, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | (~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2))), inference(ennf_transformation, [], [f56])).
fof(f56, plain, ! [X0, X1, X2] : ((leaf_occ(X1, X0) & occurrence_of(X0, X2)) => ~ ? [X3] : min_precedes(X1, X3, X2)), inference(rectify, [], [f10])).
fof(f10, plain, ! [X31, X32, X33] : ((leaf_occ(X32, X31) & occurrence_of(X31, X33)) => ~ ? [X34] : min_precedes(X32, X34, X33)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+4.p', sos_09)).
fof(f612, plain, (! [X0] : min_precedes(X0, sK16(X0, sK17), tptp0) | ~ spl19_18), inference(avatar_component_clause, [], [f611])).
fof(f611, plain, (spl19_18 <=> ! [X0] : min_precedes(X0, sK16(X0, sK17), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_18])])).
fof(f613, plain, (spl19_2 | spl19_18), inference(avatar_split_clause, [], [f393, f611, f322])).
fof(f322, plain, (spl19_2 <=> ! [X0] : (occurrence_of(X0, tptp2) | ~ leaf_occ(X0, sK17))), introduced(avatar_definition, [new_symbols(naming, [spl19_2])])).
fof(f393, plain, ! [X0, X1] : (min_precedes(X0, sK16(X0, sK17), tptp0) | occurrence_of(X1, tptp2) | ~ leaf_occ(X1, sK17)), inference(resolution, [], [f254, f256])).
fof(f256, plain, ! [X2, X1] : (sP0(X1, sK17, X2) | occurrence_of(X2, tptp2) | ~ leaf_occ(X2, sK17)), inference(cnf_transformation, [], [f177])).
fof(f254, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | min_precedes(X0, sK16(X0, X1), tptp0)), inference(cnf_transformation, [], [f174])).
fof(f174, plain, ! [X0, X1, X2] : (((min_precedes(X0, sK16(X0, X1), tptp0) & subactivity_occurrence(sK16(X0, X1), X1) & occurrence_of(sK16(X0, X1), tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16])], [f172, f173])).
fof(f173, plain, ! [X1, X0] : (? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) => (min_precedes(X0, sK16(X0, X1), tptp0) & subactivity_occurrence(sK16(X0, X1), X1) & occurrence_of(sK16(X0, X1), tptp2))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X0, X1, X2] : ((? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(rectify, [], [f171])).
fof(f171, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(nnf_transformation, [], [f131])).
fof(f609, plain, ~ spl19_17, inference(avatar_contradiction_clause, [], [f608])).
fof(f608, plain, ($false | ~ spl19_17), inference(resolution, [], [f598, f255])).
fof(f598, plain, (! [X0] : ~ occurrence_of(X0, tptp0) | ~ spl19_17), inference(duplicate_literal_removal, [], [f596])).
fof(f596, plain, (! [X0] : (~ occurrence_of(X0, tptp0) | ~ occurrence_of(X0, tptp0)) | ~ spl19_17), inference(resolution, [], [f523, f239])).
fof(f523, plain, (! [X0, X1] : (~ leaf_occ(X0, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl19_17), inference(resolution, [], [f488, f191])).
fof(f488, plain, (! [X6] : min_precedes(X6, sK18(X6), tptp0) | ~ spl19_17), inference(avatar_component_clause, [], [f487])).
fof(f487, plain, (spl19_17 <=> ! [X6] : min_precedes(X6, sK18(X6), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_17])])).
fof(f489, plain, (spl19_8 | spl19_17), inference(avatar_split_clause, [], [f473, f487, f417])).
fof(f417, plain, (spl19_8 <=> ! [X7] : (~ leaf_occ(X7, sK17) | occurrence_of(X7, tptp1))), introduced(avatar_definition, [new_symbols(naming, [spl19_8])])).
fof(f473, plain, ! [X6, X7] : (min_precedes(X6, sK18(X6), tptp0) | ~ leaf_occ(X7, sK17) | occurrence_of(X7, tptp1)), inference(resolution, [], [f259, f251])).
fof(f251, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | occurrence_of(X2, tptp1)), inference(cnf_transformation, [], [f174])).
fof(f259, plain, ! [X2, X1] : (min_precedes(X1, sK18(X1), tptp0) | sP0(X1, sK17, X2) | ~ leaf_occ(X2, sK17)), inference(cnf_transformation, [], [f177])).
fof(f456, plain, (~ spl19_2 | ~ spl19_8), inference(avatar_contradiction_clause, [], [f455])).
fof(f455, plain, ($false | (~ spl19_2 | ~ spl19_8)), inference(subsumption_resolution, [], [f454, f250])).
fof(f250, plain, ~ (tptp1 = tptp2), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ~ (tptp1 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+4.p', sos_44)).
fof(f454, plain, ((tptp1 = tptp2) | (~ spl19_2 | ~ spl19_8)), inference(backward_demodulation, [], [f391, f449])).
fof(f449, plain, ((tptp1 = sK5(sK15(sK17))) | ~ spl19_8), inference(resolution, [], [f443, f298])).
fof(f298, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | (sK5(X1) = X0)), inference(subsumption_resolution, [], [f292, f182])).
fof(f182, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ! [X0, X1] : (activity_occurrence(X1) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0, X1] : (occurrence_of(X1, X0) => activity_occurrence(X1)), inference(pure_predicate_removal, [], [f50])).
fof(f50, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), inference(rectify, [], [f4])).
fof(f4, plain, ! [X12, X13] : (occurrence_of(X13, X12) => (activity_occurrence(X13) & activity(X12))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+4.p', sos_03)).
fof(f292, plain, ! [X0, X1] : ((sK5(X1) = X0) | ~ occurrence_of(X1, X0) | ~ activity_occurrence(X1)), inference(resolution, [], [f190, f195])).
fof(f195, plain, ! [X0] : (occurrence_of(X0, sK5(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f142])).
fof(f142, plain, ! [X0] : (occurrence_of(X0, sK5(X0)) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f107, f141])).
fof(f141, plain, ! [X0] : (? [X1] : occurrence_of(X0, X1) => occurrence_of(X0, sK5(X0))), introduced(choice_axiom, [])).
fof(f107, plain, ! [X0] : (? [X1] : occurrence_of(X0, X1) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : occurrence_of(X0, X1)), inference(pure_predicate_removal, [], [f59])).
fof(f59, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f13])).
fof(f13, plain, ! [X41] : (activity_occurrence(X41) => ? [X42] : (occurrence_of(X41, X42) & activity(X42))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+4.p', sos_12)).
fof(f190, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f100])).
fof(f100, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X28, X29, X30] : ((occurrence_of(X28, X30) & occurrence_of(X28, X29)) => (X29 = X30)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+4.p', sos_08)).
fof(f443, plain, (occurrence_of(sK15(sK17), tptp1) | ~ spl19_8), inference(subsumption_resolution, [], [f442, f255])).
fof(f442, plain, (occurrence_of(sK15(sK17), tptp1) | ~ occurrence_of(sK17, tptp0) | ~ spl19_8), inference(resolution, [], [f418, f239])).
fof(f418, plain, (! [X7] : (~ leaf_occ(X7, sK17) | occurrence_of(X7, tptp1)) | ~ spl19_8), inference(avatar_component_clause, [], [f417])).
fof(f391, plain, ((tptp2 = sK5(sK15(sK17))) | ~ spl19_2), inference(resolution, [], [f298, f330])).
fof(f330, plain, (occurrence_of(sK15(sK17), tptp2) | ~ spl19_2), inference(subsumption_resolution, [], [f329, f255])).
fof(f329, plain, (occurrence_of(sK15(sK17), tptp2) | ~ occurrence_of(sK17, tptp0) | ~ spl19_2), inference(resolution, [], [f323, f239])).
fof(f323, plain, (! [X0] : (~ leaf_occ(X0, sK17) | occurrence_of(X0, tptp2)) | ~ spl19_2), inference(avatar_component_clause, [], [f322])).