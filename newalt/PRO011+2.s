fof(f728, plain, $false, inference(avatar_sat_refutation, [], [f573, f577, f679, f691, f727])).
fof(f727, plain, ~ spl21_23, inference(avatar_contradiction_clause, [], [f726])).
fof(f726, plain, ($false | ~ spl21_23), inference(resolution, [], [f720, f270])).
fof(f270, plain, ~ arboreal(sK19), inference(subsumption_resolution, [], [f268, f244])).
fof(f244, plain, ~ atomic(tptp0), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ~ atomic(tptp0), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_34)).
fof(f268, plain, (~ arboreal(sK19) | atomic(tptp0)), inference(resolution, [], [f198, f259])).
fof(f259, plain, occurrence_of(sK19, tptp0), inference(cnf_transformation, [], [f173])).
fof(f173, plain, (! [X1, X2] : (((min_precedes(X1, sK20(X1), tptp0) & subactivity_occurrence(sK20(X1), sK19) & occurrence_of(sK20(X1), tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK19, X2) | ~ leaf_occ(X2, sK19)) & occurrence_of(sK19, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20])], [f126, f172, f171])).
fof(f171, plain, (? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)) => (! [X2, X1] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK19) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, sK19, X2) | ~ leaf_occ(X2, sK19)) & occurrence_of(sK19, tptp0))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X1] : (? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, sK19) & occurrence_of(X3, tptp1)) => (min_precedes(X1, sK20(X1), tptp0) & subactivity_occurrence(sK20(X1), sK19) & occurrence_of(sK20(X1), tptp1))), introduced(choice_axiom, [])).
fof(f126, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | sP0(X1, X0, X2) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)), inference(definition_folding, [], [f124, e125])).
fof(f125, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(usedef, [], [e125])).
fof(e125, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f124, plain, ? [X0] : (! [X1, X2] : ((? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1)) & occurrence_of(X2, tptp2)) | (? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ leaf_occ(X2, X0)) & occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ~ ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2] : ((occurrence_of(X2, tptp2) => ~ ? [X3] : (min_precedes(X1, X3, tptp0) & subactivity_occurrence(X3, X0) & occurrence_of(X3, tptp1))) & (occurrence_of(X2, tptp1) => ~ ? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2))) & leaf_occ(X2, X0))), inference(rectify, [], [f47])).
fof(f47, plain, ~ ! [X99] : (occurrence_of(X99, tptp0) => ? [X100, X101] : ((occurrence_of(X101, tptp2) => ~ ? [X103] : (min_precedes(X100, X103, tptp0) & subactivity_occurrence(X103, X99) & occurrence_of(X103, tptp1))) & (occurrence_of(X101, tptp1) => ~ ? [X102] : (min_precedes(X100, X102, tptp0) & subactivity_occurrence(X102, X99) & occurrence_of(X102, tptp2))) & leaf_occ(X101, X99))), inference(negated_conjecture, [], [f46])).
fof(f46, plain, ~ ! [X99] : (occurrence_of(X99, tptp0) => ? [X100, X101] : ((occurrence_of(X101, tptp2) => ~ ? [X103] : (min_precedes(X100, X103, tptp0) & subactivity_occurrence(X103, X99) & occurrence_of(X103, tptp1))) & (occurrence_of(X101, tptp1) => ~ ? [X102] : (min_precedes(X100, X102, tptp0) & subactivity_occurrence(X102, X99) & occurrence_of(X102, tptp2))) & leaf_occ(X101, X99))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', goals)).
fof(f198, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ arboreal(X0) | atomic(X1)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f97])).
fof(f97, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f60])).
fof(f60, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f14])).
fof(f14, plain, ! [X40, X41] : (occurrence_of(X40, X41) => (arboreal(X40) <=> atomic(X41))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_13)).
fof(f720, plain, (! [X12] : arboreal(X12) | ~ spl21_23), inference(resolution, [], [f690, f431])).
fof(f431, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | arboreal(X0)), inference(resolution, [], [f226, f319])).
fof(f319, plain, ! [X0, X1] : (~ atocc(X0, X1) | arboreal(X0)), inference(subsumption_resolution, [], [f315, f205])).
fof(f205, plain, ! [X0, X1] : (atomic(sK6(X0, X1)) | ~ atocc(X0, X1)), inference(cnf_transformation, [], [f150])).
fof(f150, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & ((occurrence_of(X0, sK6(X0, X1)) & atomic(sK6(X0, X1)) & subactivity(X1, sK6(X0, X1))) | ~ atocc(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f148, f149])).
fof(f149, plain, ! [X1, X0] : (? [X3] : (occurrence_of(X0, X3) & atomic(X3) & subactivity(X1, X3)) => (occurrence_of(X0, sK6(X0, X1)) & atomic(sK6(X0, X1)) & subactivity(X1, sK6(X0, X1)))), introduced(choice_axiom, [])).
fof(f148, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & (? [X3] : (occurrence_of(X0, X3) & atomic(X3) & subactivity(X1, X3)) | ~ atocc(X0, X1))), inference(rectify, [], [f147])).
fof(f147, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & (? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2)) | ~ atocc(X0, X1))), inference(nnf_transformation, [], [f62])).
fof(f62, plain, ! [X0, X1] : (atocc(X0, X1) <=> ? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2))), inference(rectify, [], [f16])).
fof(f16, plain, ! [X46, X47] : (atocc(X46, X47) <=> ? [X48] : (occurrence_of(X46, X48) & atomic(X48) & subactivity(X47, X48))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_15)).
fof(f315, plain, ! [X0, X1] : (~ atocc(X0, X1) | ~ atomic(sK6(X0, X1)) | arboreal(X0)), inference(resolution, [], [f206, f199])).
fof(f199, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ atomic(X1) | arboreal(X0)), inference(cnf_transformation, [], [f140])).
fof(f206, plain, ! [X0, X1] : (occurrence_of(X0, sK6(X0, X1)) | ~ atocc(X0, X1)), inference(cnf_transformation, [], [f150])).
fof(f226, plain, ! [X2, X0, X1] : (atocc(X1, sK11(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0, X1, X2] : ((atocc(X2, sK12(X0, X1, X2)) & atocc(X1, sK11(X0, X1, X2)) & subactivity(sK12(X0, X1, X2), X0) & subactivity(sK11(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11, sK12])], [f115, f159])).
fof(f159, plain, ! [X2, X1, X0] : (? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0)) => (atocc(X2, sK12(X0, X1, X2)) & atocc(X1, sK11(X0, X1, X2)) & subactivity(sK12(X0, X1, X2), X0) & subactivity(sK11(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f115, plain, ! [X0, X1, X2] : (? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0))), inference(rectify, [], [f27])).
fof(f27, plain, ! [X77, X78, X79] : (min_precedes(X78, X79, X77) => ? [X80, X81] : (atocc(X79, X81) & atocc(X78, X80) & subactivity(X81, X77) & subactivity(X80, X77))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_26)).
fof(f690, plain, (! [X0] : min_precedes(X0, sK18(X0, sK19), tptp0) | ~ spl21_23), inference(avatar_component_clause, [], [f689])).
fof(f689, plain, (spl21_23 <=> ! [X0] : min_precedes(X0, sK18(X0, sK19), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl21_23])])).
fof(f691, plain, (spl21_5 | spl21_23), inference(avatar_split_clause, [], [f446, f689, f358])).
fof(f358, plain, (spl21_5 <=> ! [X0] : (occurrence_of(X0, tptp2) | ~ leaf_occ(X0, sK19))), introduced(avatar_definition, [new_symbols(naming, [spl21_5])])).
fof(f446, plain, ! [X0, X1] : (min_precedes(X0, sK18(X0, sK19), tptp0) | occurrence_of(X1, tptp2) | ~ leaf_occ(X1, sK19)), inference(resolution, [], [f258, f260])).
fof(f260, plain, ! [X2, X1] : (sP0(X1, sK19, X2) | occurrence_of(X2, tptp2) | ~ leaf_occ(X2, sK19)), inference(cnf_transformation, [], [f173])).
fof(f258, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | min_precedes(X0, sK18(X0, X1), tptp0)), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ! [X0, X1, X2] : (((min_precedes(X0, sK18(X0, X1), tptp0) & subactivity_occurrence(sK18(X0, X1), X1) & occurrence_of(sK18(X0, X1), tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18])], [f168, f169])).
fof(f169, plain, ! [X1, X0] : (? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) => (min_precedes(X0, sK18(X0, X1), tptp0) & subactivity_occurrence(sK18(X0, X1), X1) & occurrence_of(sK18(X0, X1), tptp2))), introduced(choice_axiom, [])).
fof(f168, plain, ! [X0, X1, X2] : ((? [X3] : (min_precedes(X0, X3, tptp0) & subactivity_occurrence(X3, X1) & occurrence_of(X3, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X0, X1, X2)), inference(rectify, [], [f167])).
fof(f167, plain, ! [X1, X0, X2] : ((? [X4] : (min_precedes(X1, X4, tptp0) & subactivity_occurrence(X4, X0) & occurrence_of(X4, tptp2)) & occurrence_of(X2, tptp1)) | ~ sP0(X1, X0, X2)), inference(nnf_transformation, [], [f125])).
fof(f679, plain, ~ spl21_19, inference(avatar_contradiction_clause, [], [f678])).
fof(f678, plain, ($false | ~ spl21_19), inference(resolution, [], [f656, f270])).
fof(f656, plain, (! [X12] : arboreal(X12) | ~ spl21_19), inference(resolution, [], [f576, f431])).
fof(f576, plain, (! [X6] : min_precedes(X6, sK20(X6), tptp0) | ~ spl21_19), inference(avatar_component_clause, [], [f575])).
fof(f575, plain, (spl21_19 <=> ! [X6] : min_precedes(X6, sK20(X6), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl21_19])])).
fof(f577, plain, (spl21_11 | spl21_19), inference(avatar_split_clause, [], [f527, f575, f469])).
fof(f469, plain, (spl21_11 <=> ! [X7] : (~ leaf_occ(X7, sK19) | occurrence_of(X7, tptp1))), introduced(avatar_definition, [new_symbols(naming, [spl21_11])])).
fof(f527, plain, ! [X6, X7] : (min_precedes(X6, sK20(X6), tptp0) | ~ leaf_occ(X7, sK19) | occurrence_of(X7, tptp1)), inference(resolution, [], [f263, f255])).
fof(f255, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | occurrence_of(X2, tptp1)), inference(cnf_transformation, [], [f170])).
fof(f263, plain, ! [X2, X1] : (min_precedes(X1, sK20(X1), tptp0) | sP0(X1, sK19, X2) | ~ leaf_occ(X2, sK19)), inference(cnf_transformation, [], [f173])).
fof(f573, plain, (~ spl21_5 | ~ spl21_11), inference(avatar_contradiction_clause, [], [f572])).
fof(f572, plain, ($false | (~ spl21_5 | ~ spl21_11)), inference(subsumption_resolution, [], [f571, f254])).
fof(f254, plain, ~ (tptp1 = tptp2), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ~ (tptp1 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_44)).
fof(f571, plain, ((tptp1 = tptp2) | (~ spl21_5 | ~ spl21_11)), inference(backward_demodulation, [], [f567, f568])).
fof(f568, plain, ((tptp1 = sK7(sK17(sK19))) | ~ spl21_11), inference(resolution, [], [f333, f495])).
fof(f495, plain, (occurrence_of(sK17(sK19), tptp1) | ~ spl21_11), inference(subsumption_resolution, [], [f494, f259])).
fof(f494, plain, (occurrence_of(sK17(sK19), tptp1) | ~ occurrence_of(sK19, tptp0) | ~ spl21_11), inference(resolution, [], [f470, f242])).
fof(f242, plain, ! [X0] : (leaf_occ(sK17(X0), X0) | ~ occurrence_of(X0, tptp0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : ((leaf_occ(sK17(X0), X0) & next_subocc(sK16(X0), sK17(X0), tptp0) & (occurrence_of(sK17(X0), tptp2) | occurrence_of(sK17(X0), tptp1)) & next_subocc(sK15(X0), sK16(X0), tptp0) & occurrence_of(sK16(X0), tptp4) & root_occ(sK15(X0), X0) & occurrence_of(sK15(X0), tptp3)) | ~ occurrence_of(X0, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15, sK16, sK17])], [f123, f165])).
fof(f165, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) => (leaf_occ(sK17(X0), X0) & next_subocc(sK16(X0), sK17(X0), tptp0) & (occurrence_of(sK17(X0), tptp2) | occurrence_of(sK17(X0), tptp1)) & next_subocc(sK15(X0), sK16(X0), tptp0) & occurrence_of(sK16(X0), tptp4) & root_occ(sK15(X0), X0) & occurrence_of(sK15(X0), tptp3))), introduced(choice_axiom, [])).
fof(f123, plain, ! [X0] : (? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3)) | ~ occurrence_of(X0, tptp0)), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : (occurrence_of(X0, tptp0) => ? [X1, X2, X3] : (leaf_occ(X3, X0) & next_subocc(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X1, X2, tptp0) & occurrence_of(X2, tptp4) & root_occ(X1, X0) & occurrence_of(X1, tptp3))), inference(rectify, [], [f33])).
fof(f33, plain, ! [X95] : (occurrence_of(X95, tptp0) => ? [X96, X97, X98] : (leaf_occ(X98, X95) & next_subocc(X97, X98, tptp0) & (occurrence_of(X98, tptp2) | occurrence_of(X98, tptp1)) & next_subocc(X96, X97, tptp0) & occurrence_of(X97, tptp4) & root_occ(X96, X95) & occurrence_of(X96, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_32)).
fof(f470, plain, (! [X7] : (~ leaf_occ(X7, sK19) | occurrence_of(X7, tptp1)) | ~ spl21_11), inference(avatar_component_clause, [], [f469])).
fof(f333, plain, ! [X10, X9] : (~ occurrence_of(X10, X9) | (sK7(X10) = X9)), inference(subsumption_resolution, [], [f329, f232])).
fof(f232, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ! [X0, X1] : ((activity_occurrence(X1) & activity(X0)) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f76])).
fof(f76, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), inference(rectify, [], [f30])).
fof(f30, plain, ! [X89, X90] : (occurrence_of(X90, X89) => (activity_occurrence(X90) & activity(X89))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_29)).
fof(f329, plain, ! [X10, X9] : ((sK7(X10) = X9) | ~ occurrence_of(X10, X9) | ~ activity_occurrence(X10)), inference(resolution, [], [f216, f211])).
fof(f211, plain, ! [X0] : (occurrence_of(X0, sK7(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f152])).
fof(f152, plain, ! [X0] : ((occurrence_of(X0, sK7(X0)) & activity(sK7(X0))) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f102, f151])).
fof(f151, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) => (occurrence_of(X0, sK7(X0)) & activity(sK7(X0)))), introduced(choice_axiom, [])).
fof(f102, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f19])).
fof(f19, plain, ! [X52] : (activity_occurrence(X52) => ? [X53] : (occurrence_of(X52, X53) & activity(X53))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_18)).
fof(f216, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f69])).
fof(f69, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f23])).
fof(f23, plain, ! [X64, X65, X66] : ((occurrence_of(X64, X66) & occurrence_of(X64, X65)) => (X65 = X66)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO011+2.p', sos_22)).
fof(f567, plain, ((tptp2 = sK7(sK17(sK19))) | ~ spl21_5), inference(resolution, [], [f333, f366])).
fof(f366, plain, (occurrence_of(sK17(sK19), tptp2) | ~ spl21_5), inference(subsumption_resolution, [], [f365, f259])).
fof(f365, plain, (occurrence_of(sK17(sK19), tptp2) | ~ occurrence_of(sK19, tptp0) | ~ spl21_5), inference(resolution, [], [f359, f242])).
fof(f359, plain, (! [X0] : (~ leaf_occ(X0, sK19) | occurrence_of(X0, tptp2)) | ~ spl21_5), inference(avatar_component_clause, [], [f358])).