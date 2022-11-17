fof(f3430, plain, $false, inference(avatar_sat_refutation, [], [f534, f1561, f1715, f1981, f3304, f3362, f3378, f3407, f3425])).
fof(f3425, plain, (~ spl21_51 | ~ spl21_122), inference(avatar_contradiction_clause, [], [f3424])).
fof(f3424, plain, ($false | (~ spl21_51 | ~ spl21_122)), inference(subsumption_resolution, [], [f3423, f246])).
fof(f246, plain, ~ (tptp3 = tptp4), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ~ (tptp3 = tptp4), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_39)).
fof(f3423, plain, ((tptp3 = tptp4) | (~ spl21_51 | ~ spl21_122)), inference(backward_demodulation, [], [f515, f3413])).
fof(f3413, plain, ((tptp3 = sK6(sK15(sK18))) | (~ spl21_51 | ~ spl21_122)), inference(backward_demodulation, [], [f1957, f3303])).
fof(f3303, plain, ((sK15(sK18) = sK14(sK14(sK18))) | ~ spl21_122), inference(avatar_component_clause, [], [f3301])).
fof(f3301, plain, (spl21_122 <=> (sK15(sK18) = sK14(sK14(sK18)))), introduced(avatar_definition, [new_symbols(naming, [spl21_122])])).
fof(f1957, plain, ((tptp3 = sK6(sK14(sK14(sK18)))) | ~ spl21_51), inference(resolution, [], [f1709, f295])).
fof(f295, plain, ! [X2, X1] : (~ occurrence_of(X2, X1) | (sK6(X2) = X1)), inference(subsumption_resolution, [], [f292, f229])).
fof(f229, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0, X1] : ((activity_occurrence(X1) & activity(X0)) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f76])).
fof(f76, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), inference(rectify, [], [f30])).
fof(f30, plain, ! [X89, X90] : (occurrence_of(X90, X89) => (activity_occurrence(X90) & activity(X89))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_29)).
fof(f292, plain, ! [X2, X1] : ((sK6(X2) = X1) | ~ occurrence_of(X2, X1) | ~ activity_occurrence(X2)), inference(resolution, [], [f213, f208])).
fof(f208, plain, ! [X0] : (occurrence_of(X0, sK6(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f152])).
fof(f152, plain, ! [X0] : ((occurrence_of(X0, sK6(X0)) & activity(sK6(X0))) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f104, f151])).
fof(f151, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) => (occurrence_of(X0, sK6(X0)) & activity(sK6(X0)))), introduced(choice_axiom, [])).
fof(f104, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f19])).
fof(f19, plain, ! [X52] : (activity_occurrence(X52) => ? [X53] : (occurrence_of(X52, X53) & activity(X53))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_18)).
fof(f213, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f110])).
fof(f110, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f69])).
fof(f69, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f23])).
fof(f23, plain, ! [X64, X65, X66] : ((occurrence_of(X64, X66) & occurrence_of(X64, X65)) => (X65 = X66)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_22)).
fof(f1709, plain, (occurrence_of(sK14(sK14(sK18)), tptp3) | ~ spl21_51), inference(avatar_component_clause, [], [f1707])).
fof(f1707, plain, (spl21_51 <=> occurrence_of(sK14(sK14(sK18)), tptp3)), introduced(avatar_definition, [new_symbols(naming, [spl21_51])])).
fof(f515, plain, (tptp4 = sK6(sK15(sK18))), inference(resolution, [], [f476, f295])).
fof(f476, plain, occurrence_of(sK15(sK18), tptp4), inference(subsumption_resolution, [], [f475, f255])).
fof(f255, plain, occurrence_of(sK19, tptp0), inference(cnf_transformation, [], [f173])).
fof(f173, plain, (! [X2, X3] : (((min_precedes(X2, sK20(X2), tptp0) & occurrence_of(sK20(X2), tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK19) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK18, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK18, sK19) & arboreal(sK18) & subactivity_occurrence(sK18, sK19) & occurrence_of(sK19, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18, sK19, sK20])], [f130, f172, f171])).
fof(f171, plain, (? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => (! [X3, X2] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK19) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK18, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK18, sK19) & arboreal(sK18) & subactivity_occurrence(sK18, sK19) & occurrence_of(sK19, tptp0))), introduced(choice_axiom, [])).
fof(f172, plain, ! [X2] : (? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) => (min_precedes(X2, sK20(X2), tptp0) & occurrence_of(sK20(X2), tptp1))), introduced(choice_axiom, [])).
fof(f130, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(definition_folding, [], [f128, e129])).
fof(f129, plain, ! [X2, X3] : ((? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ sP0(X2, X3)), inference(usedef, [], [e129])).
fof(e129, plain, ! [X2, X3] : (sP0(X2, X3) <=> (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f128, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(flattening, [], [f127])).
fof(f127, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & (~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ~ ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3] : ((occurrence_of(X3, tptp2) => ~ ? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1))) & (occurrence_of(X3, tptp1) => ~ ? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2))) & leaf_occ(X3, X1) & min_precedes(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f47])).
fof(f47, plain, ~ ! [X101, X102] : ((~ leaf_occ(X101, X102) & arboreal(X101) & subactivity_occurrence(X101, X102) & occurrence_of(X102, tptp0)) => ? [X103, X104] : ((occurrence_of(X104, tptp2) => ~ ? [X106] : (min_precedes(X103, X106, tptp0) & occurrence_of(X106, tptp1))) & (occurrence_of(X104, tptp1) => ~ ? [X105] : (min_precedes(X103, X105, tptp0) & occurrence_of(X105, tptp2))) & leaf_occ(X104, X102) & min_precedes(X103, X104, tptp0) & (occurrence_of(X104, tptp2) | occurrence_of(X104, tptp1)) & next_subocc(X101, X103, tptp0) & occurrence_of(X103, tptp3))), inference(negated_conjecture, [], [f46])).
fof(f46, plain, ~ ! [X101, X102] : ((~ leaf_occ(X101, X102) & arboreal(X101) & subactivity_occurrence(X101, X102) & occurrence_of(X102, tptp0)) => ? [X103, X104] : ((occurrence_of(X104, tptp2) => ~ ? [X106] : (min_precedes(X103, X106, tptp0) & occurrence_of(X106, tptp1))) & (occurrence_of(X104, tptp1) => ~ ? [X105] : (min_precedes(X103, X105, tptp0) & occurrence_of(X105, tptp2))) & leaf_occ(X104, X102) & min_precedes(X103, X104, tptp0) & (occurrence_of(X104, tptp2) | occurrence_of(X104, tptp1)) & next_subocc(X101, X103, tptp0) & occurrence_of(X103, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', goals)).
fof(f475, plain, (occurrence_of(sK15(sK18), tptp4) | ~ occurrence_of(sK19, tptp0)), inference(subsumption_resolution, [], [f474, f257])).
fof(f257, plain, arboreal(sK18), inference(cnf_transformation, [], [f173])).
fof(f474, plain, (~ arboreal(sK18) | occurrence_of(sK15(sK18), tptp4) | ~ occurrence_of(sK19, tptp0)), inference(subsumption_resolution, [], [f470, f258])).
fof(f258, plain, ~ leaf_occ(sK18, sK19), inference(cnf_transformation, [], [f173])).
fof(f470, plain, (leaf_occ(sK18, sK19) | ~ arboreal(sK18) | occurrence_of(sK15(sK18), tptp4) | ~ occurrence_of(sK19, tptp0)), inference(resolution, [], [f235, f256])).
fof(f256, plain, subactivity_occurrence(sK18, sK19), inference(cnf_transformation, [], [f173])).
fof(f235, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK15(X0), tptp4) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0, X1] : ((! [X5] : ((sK16(X0) = X5) | (sK15(X0) = X5) | ~ min_precedes(sK14(X0), X5, tptp0)) & min_precedes(sK15(X0), sK16(X0), tptp0) & (occurrence_of(sK16(X0), tptp2) | occurrence_of(sK16(X0), tptp1)) & min_precedes(sK14(X0), sK15(X0), tptp0) & occurrence_of(sK15(X0), tptp4) & next_subocc(X0, sK14(X0), tptp0) & occurrence_of(sK14(X0), tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15, sK16])], [f126, f165])).
fof(f165, plain, ! [X0] : (? [X2, X3, X4] : (! [X5] : ((X4 = X5) | (X3 = X5) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) => (! [X5] : ((sK16(X0) = X5) | (sK15(X0) = X5) | ~ min_precedes(sK14(X0), X5, tptp0)) & min_precedes(sK15(X0), sK16(X0), tptp0) & (occurrence_of(sK16(X0), tptp2) | occurrence_of(sK16(X0), tptp1)) & min_precedes(sK14(X0), sK15(X0), tptp0) & occurrence_of(sK15(X0), tptp4) & next_subocc(X0, sK14(X0), tptp0) & occurrence_of(sK14(X0), tptp3))), introduced(choice_axiom, [])).
fof(f126, plain, ! [X0, X1] : (? [X2, X3, X4] : (! [X5] : ((X4 = X5) | (X3 = X5) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(flattening, [], [f125])).
fof(f125, plain, ! [X0, X1] : (? [X2, X3, X4] : (! [X5] : (((X4 = X5) | (X3 = X5)) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | (leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3, X4] : (! [X5] : (min_precedes(X2, X5, tptp0) => ((X4 = X5) | (X3 = X5))) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f33])).
fof(f33, plain, ! [X95, X96] : ((~ leaf_occ(X95, X96) & arboreal(X95) & subactivity_occurrence(X95, X96) & occurrence_of(X96, tptp0)) => ? [X97, X98, X99] : (! [X100] : (min_precedes(X97, X100, tptp0) => ((X99 = X100) | (X98 = X100))) & min_precedes(X98, X99, tptp0) & (occurrence_of(X99, tptp2) | occurrence_of(X99, tptp1)) & min_precedes(X97, X98, tptp0) & occurrence_of(X98, tptp4) & next_subocc(X95, X97, tptp0) & occurrence_of(X97, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_32)).
fof(f3407, plain, (~ spl21_3 | ~ spl21_51 | ~ spl21_121), inference(avatar_contradiction_clause, [], [f3406])).
fof(f3406, plain, ($false | (~ spl21_3 | ~ spl21_51 | ~ spl21_121)), inference(subsumption_resolution, [], [f3405, f250])).
fof(f250, plain, ~ (tptp3 = tptp2), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ~ (tptp3 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_43)).
fof(f3405, plain, ((tptp3 = tptp2) | (~ spl21_3 | ~ spl21_51 | ~ spl21_121)), inference(forward_demodulation, [], [f3403, f3369])).
fof(f3369, plain, ((tptp3 = sK6(sK16(sK18))) | (~ spl21_51 | ~ spl21_121)), inference(backward_demodulation, [], [f1957, f3299])).
fof(f3299, plain, ((sK16(sK18) = sK14(sK14(sK18))) | ~ spl21_121), inference(avatar_component_clause, [], [f3297])).
fof(f3297, plain, (spl21_121 <=> (sK16(sK18) = sK14(sK14(sK18)))), introduced(avatar_definition, [new_symbols(naming, [spl21_121])])).
fof(f3403, plain, ((tptp2 = sK6(sK16(sK18))) | ~ spl21_3), inference(resolution, [], [f529, f295])).
fof(f529, plain, (occurrence_of(sK16(sK18), tptp2) | ~ spl21_3), inference(avatar_component_clause, [], [f527])).
fof(f527, plain, (spl21_3 <=> occurrence_of(sK16(sK18), tptp2)), introduced(avatar_definition, [new_symbols(naming, [spl21_3])])).
fof(f3378, plain, (~ spl21_4 | ~ spl21_51 | ~ spl21_121), inference(avatar_contradiction_clause, [], [f3377])).
fof(f3377, plain, ($false | (~ spl21_4 | ~ spl21_51 | ~ spl21_121)), inference(subsumption_resolution, [], [f3376, f249])).
fof(f249, plain, ~ (tptp3 = tptp1), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ~ (tptp3 = tptp1), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_42)).
fof(f3376, plain, ((tptp3 = tptp1) | (~ spl21_4 | ~ spl21_51 | ~ spl21_121)), inference(backward_demodulation, [], [f902, f3369])).
fof(f902, plain, ((tptp1 = sK6(sK16(sK18))) | ~ spl21_4), inference(resolution, [], [f533, f295])).
fof(f533, plain, (occurrence_of(sK16(sK18), tptp1) | ~ spl21_4), inference(avatar_component_clause, [], [f531])).
fof(f531, plain, (spl21_4 <=> occurrence_of(sK16(sK18), tptp1)), introduced(avatar_definition, [new_symbols(naming, [spl21_4])])).
fof(f3362, plain, ~ spl21_52, inference(avatar_contradiction_clause, [], [f3349])).
fof(f3349, plain, ($false | ~ spl21_52), inference(resolution, [], [f1714, f496])).
fof(f496, plain, min_precedes(sK14(sK18), sK15(sK18), tptp0), inference(subsumption_resolution, [], [f495, f255])).
fof(f495, plain, (min_precedes(sK14(sK18), sK15(sK18), tptp0) | ~ occurrence_of(sK19, tptp0)), inference(subsumption_resolution, [], [f494, f257])).
fof(f494, plain, (~ arboreal(sK18) | min_precedes(sK14(sK18), sK15(sK18), tptp0) | ~ occurrence_of(sK19, tptp0)), inference(subsumption_resolution, [], [f490, f258])).
fof(f490, plain, (leaf_occ(sK18, sK19) | ~ arboreal(sK18) | min_precedes(sK14(sK18), sK15(sK18), tptp0) | ~ occurrence_of(sK19, tptp0)), inference(resolution, [], [f236, f256])).
fof(f236, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | min_precedes(sK14(X0), sK15(X0), tptp0) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f166])).
fof(f1714, plain, (! [X27] : ~ min_precedes(sK14(sK18), X27, tptp0) | ~ spl21_52), inference(avatar_component_clause, [], [f1713])).
fof(f1713, plain, (spl21_52 <=> ! [X27] : ~ min_precedes(sK14(sK18), X27, tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl21_52])])).
fof(f3304, plain, (spl21_33 | spl21_121 | spl21_122 | ~ spl21_70), inference(avatar_split_clause, [], [f3295, f1976, f3301, f3297, f1294])).
fof(f1294, plain, (spl21_33 <=> ! [X0] : (~ subactivity_occurrence(sK18, X0) | ~ occurrence_of(X0, tptp0))), introduced(avatar_definition, [new_symbols(naming, [spl21_33])])).
fof(f1976, plain, (spl21_70 <=> next_subocc(sK14(sK18), sK14(sK14(sK18)), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl21_70])])).
fof(f3295, plain, (! [X1] : ((sK15(sK18) = sK14(sK14(sK18))) | (sK16(sK18) = sK14(sK14(sK18))) | ~ subactivity_occurrence(sK18, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl21_70), inference(subsumption_resolution, [], [f3294, f736])).
fof(f736, plain, ! [X2] : (~ leaf_occ(sK18, X2) | ~ occurrence_of(X2, tptp0)), inference(resolution, [], [f575, f212])).
fof(f212, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | (~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2))), inference(ennf_transformation, [], [f68])).
fof(f68, plain, ! [X0, X1, X2] : ((leaf_occ(X1, X0) & occurrence_of(X0, X2)) => ~ ? [X3] : min_precedes(X1, X3, X2)), inference(rectify, [], [f22])).
fof(f22, plain, ! [X60, X61, X62] : ((leaf_occ(X61, X60) & occurrence_of(X60, X62)) => ~ ? [X63] : min_precedes(X61, X63, X62)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_21)).
fof(f575, plain, min_precedes(sK18, sK14(sK18), tptp0), inference(resolution, [], [f486, f178])).
fof(f178, plain, ! [X2, X0, X1] : (~ next_subocc(X0, X1, X2) | min_precedes(X0, X1, X2)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (min_precedes(sK1(X0, X1, X2), X1, X2) & min_precedes(X0, sK1(X0, X1, X2), X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f133, f134])).
fof(f134, plain, ! [X2, X1, X0] : (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) => (min_precedes(sK1(X0, X1, X2), X1, X2) & min_precedes(X0, sK1(X0, X1, X2), X2))), introduced(choice_axiom, [])).
fof(f133, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(rectify, [], [f132])).
fof(f132, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(flattening, [], [f131])).
fof(f131, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2))) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(nnf_transformation, [], [f91])).
fof(f91, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (~ ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(rectify, [], [f5])).
fof(f5, plain, ! [X15, X16, X17] : (next_subocc(X15, X16, X17) <=> (~ ? [X18] : (min_precedes(X18, X16, X17) & min_precedes(X15, X18, X17)) & min_precedes(X15, X16, X17))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_04)).
fof(f486, plain, next_subocc(sK18, sK14(sK18), tptp0), inference(subsumption_resolution, [], [f485, f255])).
fof(f485, plain, (next_subocc(sK18, sK14(sK18), tptp0) | ~ occurrence_of(sK19, tptp0)), inference(subsumption_resolution, [], [f484, f257])).
fof(f484, plain, (~ arboreal(sK18) | next_subocc(sK18, sK14(sK18), tptp0) | ~ occurrence_of(sK19, tptp0)), inference(subsumption_resolution, [], [f480, f258])).
fof(f480, plain, (leaf_occ(sK18, sK19) | ~ arboreal(sK18) | next_subocc(sK18, sK14(sK18), tptp0) | ~ occurrence_of(sK19, tptp0)), inference(resolution, [], [f234, f256])).
fof(f234, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | next_subocc(X0, sK14(X0), tptp0) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f166])).
fof(f3294, plain, (! [X1] : ((sK15(sK18) = sK14(sK14(sK18))) | (sK16(sK18) = sK14(sK14(sK18))) | leaf_occ(sK18, X1) | ~ subactivity_occurrence(sK18, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl21_70), inference(subsumption_resolution, [], [f3255, f257])).
fof(f3255, plain, (! [X1] : ((sK15(sK18) = sK14(sK14(sK18))) | (sK16(sK18) = sK14(sK14(sK18))) | leaf_occ(sK18, X1) | ~ arboreal(sK18) | ~ subactivity_occurrence(sK18, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl21_70), inference(resolution, [], [f3130, f239])).
fof(f239, plain, ! [X0, X5, X1] : (~ min_precedes(sK14(X0), X5, tptp0) | (sK15(X0) = X5) | (sK16(X0) = X5) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f166])).
fof(f3130, plain, (min_precedes(sK14(sK18), sK14(sK14(sK18)), tptp0) | ~ spl21_70), inference(resolution, [], [f1978, f178])).
fof(f1978, plain, (next_subocc(sK14(sK18), sK14(sK14(sK18)), tptp0) | ~ spl21_70), inference(avatar_component_clause, [], [f1976])).
fof(f1981, plain, (spl21_52 | spl21_70), inference(avatar_split_clause, [], [f1980, f1976, f1713])).
fof(f1980, plain, ! [X27] : (next_subocc(sK14(sK18), sK14(sK14(sK18)), tptp0) | ~ min_precedes(sK14(sK18), X27, tptp0)), inference(subsumption_resolution, [], [f1971, f216])).
fof(f216, plain, ! [X2, X0, X1] : (occurrence_of(sK8(X0, X1, X2), X0) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f156])).
fof(f156, plain, ! [X0, X1, X2] : ((subactivity_occurrence(X2, sK8(X0, X1, X2)) & subactivity_occurrence(X1, sK8(X0, X1, X2)) & occurrence_of(sK8(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f114, f155])).
fof(f155, plain, ! [X2, X1, X0] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) => (subactivity_occurrence(X2, sK8(X0, X1, X2)) & subactivity_occurrence(X1, sK8(X0, X1, X2)) & occurrence_of(sK8(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f114, plain, ! [X0, X1, X2] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0))), inference(rectify, [], [f25])).
fof(f25, plain, ! [X70, X71, X72] : (min_precedes(X71, X72, X70) => ? [X73] : (subactivity_occurrence(X72, X73) & subactivity_occurrence(X71, X73) & occurrence_of(X73, X70))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_24)).
fof(f1971, plain, ! [X27] : (next_subocc(sK14(sK18), sK14(sK14(sK18)), tptp0) | ~ min_precedes(sK14(sK18), X27, tptp0) | ~ occurrence_of(sK8(tptp0, sK14(sK18), X27), tptp0)), inference(resolution, [], [f1095, f608])).
fof(f608, plain, ! [X3] : (~ leaf_occ(sK14(sK18), X3) | ~ occurrence_of(X3, tptp0)), inference(resolution, [], [f496, f212])).
fof(f1095, plain, ! [X0, X1] : (leaf_occ(X0, sK8(tptp0, X0, X1)) | next_subocc(X0, sK14(X0), tptp0) | ~ min_precedes(X0, X1, tptp0)), inference(duplicate_literal_removal, [], [f1094])).
fof(f1094, plain, ! [X0, X1] : (next_subocc(X0, sK14(X0), tptp0) | leaf_occ(X0, sK8(tptp0, X0, X1)) | ~ min_precedes(X0, X1, tptp0) | ~ min_precedes(X0, X1, tptp0)), inference(resolution, [], [f487, f216])).
fof(f487, plain, ! [X2, X0, X1] : (~ occurrence_of(sK8(X1, X0, X2), tptp0) | next_subocc(X0, sK14(X0), tptp0) | leaf_occ(X0, sK8(X1, X0, X2)) | ~ min_precedes(X0, X2, X1)), inference(subsumption_resolution, [], [f481, f344])).
fof(f344, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | arboreal(X0)), inference(resolution, [], [f223, f286])).
fof(f286, plain, ! [X0, X1] : (~ atocc(X0, X1) | arboreal(X0)), inference(subsumption_resolution, [], [f282, f202])).
fof(f202, plain, ! [X0, X1] : (atomic(sK5(X0, X1)) | ~ atocc(X0, X1)), inference(cnf_transformation, [], [f150])).
fof(f150, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & ((occurrence_of(X0, sK5(X0, X1)) & atomic(sK5(X0, X1)) & subactivity(X1, sK5(X0, X1))) | ~ atocc(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f148, f149])).
fof(f149, plain, ! [X1, X0] : (? [X3] : (occurrence_of(X0, X3) & atomic(X3) & subactivity(X1, X3)) => (occurrence_of(X0, sK5(X0, X1)) & atomic(sK5(X0, X1)) & subactivity(X1, sK5(X0, X1)))), introduced(choice_axiom, [])).
fof(f148, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & (? [X3] : (occurrence_of(X0, X3) & atomic(X3) & subactivity(X1, X3)) | ~ atocc(X0, X1))), inference(rectify, [], [f147])).
fof(f147, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & (? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2)) | ~ atocc(X0, X1))), inference(nnf_transformation, [], [f62])).
fof(f62, plain, ! [X0, X1] : (atocc(X0, X1) <=> ? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2))), inference(rectify, [], [f16])).
fof(f16, plain, ! [X46, X47] : (atocc(X46, X47) <=> ? [X48] : (occurrence_of(X46, X48) & atomic(X48) & subactivity(X47, X48))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_15)).
fof(f282, plain, ! [X0, X1] : (~ atocc(X0, X1) | ~ atomic(sK5(X0, X1)) | arboreal(X0)), inference(resolution, [], [f203, f196])).
fof(f196, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ atomic(X1) | arboreal(X0)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f99])).
fof(f99, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f60])).
fof(f60, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f14])).
fof(f14, plain, ! [X40, X41] : (occurrence_of(X40, X41) => (arboreal(X40) <=> atomic(X41))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_13)).
fof(f203, plain, ! [X0, X1] : (occurrence_of(X0, sK5(X0, X1)) | ~ atocc(X0, X1)), inference(cnf_transformation, [], [f150])).
fof(f223, plain, ! [X2, X0, X1] : (atocc(X1, sK10(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f160])).
fof(f160, plain, ! [X0, X1, X2] : ((atocc(X2, sK11(X0, X1, X2)) & atocc(X1, sK10(X0, X1, X2)) & subactivity(sK11(X0, X1, X2), X0) & subactivity(sK10(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11])], [f117, f159])).
fof(f159, plain, ! [X2, X1, X0] : (? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0)) => (atocc(X2, sK11(X0, X1, X2)) & atocc(X1, sK10(X0, X1, X2)) & subactivity(sK11(X0, X1, X2), X0) & subactivity(sK10(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f117, plain, ! [X0, X1, X2] : (? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0))), inference(rectify, [], [f27])).
fof(f27, plain, ! [X77, X78, X79] : (min_precedes(X78, X79, X77) => ? [X80, X81] : (atocc(X79, X81) & atocc(X78, X80) & subactivity(X81, X77) & subactivity(X80, X77))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+2.p', sos_26)).
fof(f481, plain, ! [X2, X0, X1] : (leaf_occ(X0, sK8(X1, X0, X2)) | ~ arboreal(X0) | next_subocc(X0, sK14(X0), tptp0) | ~ occurrence_of(sK8(X1, X0, X2), tptp0) | ~ min_precedes(X0, X2, X1)), inference(resolution, [], [f234, f217])).
fof(f217, plain, ! [X2, X0, X1] : (subactivity_occurrence(X1, sK8(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f156])).
fof(f1715, plain, (spl21_52 | spl21_51), inference(avatar_split_clause, [], [f1711, f1707, f1713])).
fof(f1711, plain, ! [X27] : (occurrence_of(sK14(sK14(sK18)), tptp3) | ~ min_precedes(sK14(sK18), X27, tptp0)), inference(subsumption_resolution, [], [f1698, f216])).
fof(f1698, plain, ! [X27] : (occurrence_of(sK14(sK14(sK18)), tptp3) | ~ min_precedes(sK14(sK18), X27, tptp0) | ~ occurrence_of(sK8(tptp0, sK14(sK18), X27), tptp0)), inference(resolution, [], [f1067, f608])).
fof(f1067, plain, ! [X0, X1] : (leaf_occ(X0, sK8(tptp0, X0, X1)) | occurrence_of(sK14(X0), tptp3) | ~ min_precedes(X0, X1, tptp0)), inference(duplicate_literal_removal, [], [f1066])).
fof(f1066, plain, ! [X0, X1] : (occurrence_of(sK14(X0), tptp3) | leaf_occ(X0, sK8(tptp0, X0, X1)) | ~ min_precedes(X0, X1, tptp0) | ~ min_precedes(X0, X1, tptp0)), inference(resolution, [], [f458, f216])).
fof(f458, plain, ! [X2, X0, X1] : (~ occurrence_of(sK8(X1, X0, X2), tptp0) | occurrence_of(sK14(X0), tptp3) | leaf_occ(X0, sK8(X1, X0, X2)) | ~ min_precedes(X0, X2, X1)), inference(subsumption_resolution, [], [f452, f344])).
fof(f452, plain, ! [X2, X0, X1] : (leaf_occ(X0, sK8(X1, X0, X2)) | ~ arboreal(X0) | occurrence_of(sK14(X0), tptp3) | ~ occurrence_of(sK8(X1, X0, X2), tptp0) | ~ min_precedes(X0, X2, X1)), inference(resolution, [], [f233, f217])).
fof(f233, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK14(X0), tptp3) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f166])).
fof(f1561, plain, ~ spl21_33, inference(avatar_contradiction_clause, [], [f1560])).
fof(f1560, plain, ($false | ~ spl21_33), inference(subsumption_resolution, [], [f1555, f255])).
fof(f1555, plain, (~ occurrence_of(sK19, tptp0) | ~ spl21_33), inference(resolution, [], [f1295, f256])).
fof(f1295, plain, (! [X0] : (~ subactivity_occurrence(sK18, X0) | ~ occurrence_of(X0, tptp0)) | ~ spl21_33), inference(avatar_component_clause, [], [f1294])).
fof(f534, plain, (spl21_3 | spl21_4), inference(avatar_split_clause, [], [f525, f531, f527])).
fof(f525, plain, (occurrence_of(sK16(sK18), tptp1) | occurrence_of(sK16(sK18), tptp2)), inference(subsumption_resolution, [], [f524, f255])).
fof(f524, plain, (occurrence_of(sK16(sK18), tptp1) | occurrence_of(sK16(sK18), tptp2) | ~ occurrence_of(sK19, tptp0)), inference(subsumption_resolution, [], [f523, f257])).
fof(f523, plain, (occurrence_of(sK16(sK18), tptp1) | ~ arboreal(sK18) | occurrence_of(sK16(sK18), tptp2) | ~ occurrence_of(sK19, tptp0)), inference(subsumption_resolution, [], [f519, f258])).
fof(f519, plain, (occurrence_of(sK16(sK18), tptp1) | leaf_occ(sK18, sK19) | ~ arboreal(sK18) | occurrence_of(sK16(sK18), tptp2) | ~ occurrence_of(sK19, tptp0)), inference(resolution, [], [f237, f256])).
fof(f237, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | occurrence_of(sK16(X0), tptp1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK16(X0), tptp2) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f166])).