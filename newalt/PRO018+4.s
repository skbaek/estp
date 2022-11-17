fof(f2602, plain, $false, inference(avatar_sat_refutation, [], [f460, f974, f1886, f2112, f2442, f2472, f2523, f2592, f2601])).
fof(f2601, plain, (~ spl19_4 | ~ spl19_82 | ~ spl19_103), inference(avatar_contradiction_clause, [], [f2600])).
fof(f2600, plain, ($false | (~ spl19_4 | ~ spl19_82 | ~ spl19_103)), inference(subsumption_resolution, [], [f2599, f245])).
fof(f245, plain, ~ (tptp3 = tptp1), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ~ (tptp3 = tptp1), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_42)).
fof(f2599, plain, ((tptp3 = tptp1) | (~ spl19_4 | ~ spl19_82 | ~ spl19_103)), inference(forward_demodulation, [], [f2594, f2583])).
fof(f2583, plain, ((tptp3 = sK5(sK14(sK16))) | (~ spl19_82 | ~ spl19_103)), inference(resolution, [], [f2546, f278])).
fof(f278, plain, ! [X2, X1] : (~ occurrence_of(X2, X1) | (sK5(X2) = X1)), inference(subsumption_resolution, [], [f276, f182])).
fof(f182, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ! [X0, X1] : (activity_occurrence(X1) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ! [X0, X1] : (occurrence_of(X1, X0) => activity_occurrence(X1)), inference(pure_predicate_removal, [], [f50])).
fof(f50, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), inference(rectify, [], [f4])).
fof(f4, plain, ! [X12, X13] : (occurrence_of(X13, X12) => (activity_occurrence(X13) & activity(X12))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_03)).
fof(f276, plain, ! [X2, X1] : ((sK5(X2) = X1) | ~ occurrence_of(X2, X1) | ~ activity_occurrence(X2)), inference(resolution, [], [f190, f195])).
fof(f195, plain, ! [X0] : (occurrence_of(X0, sK5(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f146])).
fof(f146, plain, ! [X0] : (occurrence_of(X0, sK5(X0)) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f108, f145])).
fof(f145, plain, ! [X0] : (? [X1] : occurrence_of(X0, X1) => occurrence_of(X0, sK5(X0))), introduced(choice_axiom, [])).
fof(f108, plain, ! [X0] : (? [X1] : occurrence_of(X0, X1) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : occurrence_of(X0, X1)), inference(pure_predicate_removal, [], [f59])).
fof(f59, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f13])).
fof(f13, plain, ! [X41] : (activity_occurrence(X41) => ? [X42] : (occurrence_of(X41, X42) & activity(X42))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_12)).
fof(f190, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f9])).
fof(f9, plain, ! [X28, X29, X30] : ((occurrence_of(X28, X30) & occurrence_of(X28, X29)) => (X29 = X30)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_08)).
fof(f2546, plain, (occurrence_of(sK14(sK16), tptp3) | (~ spl19_82 | ~ spl19_103)), inference(forward_demodulation, [], [f1885, f2437])).
fof(f2437, plain, ((sK14(sK16) = sK12(sK12(sK16))) | ~ spl19_103), inference(avatar_component_clause, [], [f2435])).
fof(f2435, plain, (spl19_103 <=> (sK14(sK16) = sK12(sK12(sK16)))), introduced(avatar_definition, [new_symbols(naming, [spl19_103])])).
fof(f1885, plain, (occurrence_of(sK12(sK12(sK16)), tptp3) | ~ spl19_82), inference(avatar_component_clause, [], [f1883])).
fof(f1883, plain, (spl19_82 <=> occurrence_of(sK12(sK12(sK16)), tptp3)), introduced(avatar_definition, [new_symbols(naming, [spl19_82])])).
fof(f2594, plain, ((tptp1 = sK5(sK14(sK16))) | ~ spl19_4), inference(resolution, [], [f459, f278])).
fof(f459, plain, (occurrence_of(sK14(sK16), tptp1) | ~ spl19_4), inference(avatar_component_clause, [], [f457])).
fof(f457, plain, (spl19_4 <=> occurrence_of(sK14(sK16), tptp1)), introduced(avatar_definition, [new_symbols(naming, [spl19_4])])).
fof(f2592, plain, (~ spl19_3 | ~ spl19_82 | ~ spl19_103), inference(avatar_contradiction_clause, [], [f2591])).
fof(f2591, plain, ($false | (~ spl19_3 | ~ spl19_82 | ~ spl19_103)), inference(subsumption_resolution, [], [f2590, f246])).
fof(f246, plain, ~ (tptp3 = tptp2), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ~ (tptp3 = tptp2), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_43)).
fof(f2590, plain, ((tptp3 = tptp2) | (~ spl19_3 | ~ spl19_82 | ~ spl19_103)), inference(backward_demodulation, [], [f462, f2583])).
fof(f462, plain, ((tptp2 = sK5(sK14(sK16))) | ~ spl19_3), inference(resolution, [], [f455, f278])).
fof(f455, plain, (occurrence_of(sK14(sK16), tptp2) | ~ spl19_3), inference(avatar_component_clause, [], [f453])).
fof(f453, plain, (spl19_3 <=> occurrence_of(sK14(sK16), tptp2)), introduced(avatar_definition, [new_symbols(naming, [spl19_3])])).
fof(f2523, plain, ~ spl19_81, inference(avatar_contradiction_clause, [], [f2511])).
fof(f2511, plain, ($false | ~ spl19_81), inference(resolution, [], [f1881, f426])).
fof(f426, plain, min_precedes(sK12(sK16), sK13(sK16), tptp0), inference(subsumption_resolution, [], [f425, f251])).
fof(f251, plain, occurrence_of(sK17, tptp0), inference(cnf_transformation, [], [f177])).
fof(f177, plain, (! [X2, X3] : (((min_precedes(X2, sK18(X2), tptp0) & occurrence_of(sK18(X2), tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK17) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK16, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK16, sK17) & arboreal(sK16) & subactivity_occurrence(sK16, sK17) & occurrence_of(sK17, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16, sK17, sK18])], [f136, f176, f175])).
fof(f175, plain, (? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => (! [X3, X2] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, sK17) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(sK16, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(sK16, sK17) & arboreal(sK16) & subactivity_occurrence(sK16, sK17) & occurrence_of(sK17, tptp0))), introduced(choice_axiom, [])).
fof(f176, plain, ! [X2] : (? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) => (min_precedes(X2, sK18(X2), tptp0) & occurrence_of(sK18(X2), tptp1))), introduced(choice_axiom, [])).
fof(f136, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | sP0(X2, X3) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(definition_folding, [], [f134, e135])).
fof(f135, plain, ! [X2, X3] : ((? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ sP0(X2, X3)), inference(usedef, [], [e135])).
fof(e135, plain, ! [X2, X3] : (sP0(X2, X3) <=> (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f134, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & ~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)), inference(flattening, [], [f133])).
fof(f133, plain, ? [X0, X1] : (! [X2, X3] : ((? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1)) & occurrence_of(X3, tptp2)) | (? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2)) & occurrence_of(X3, tptp1)) | ~ leaf_occ(X3, X1) | ~ min_precedes(X2, X3, tptp0) | (~ occurrence_of(X3, tptp2) & ~ occurrence_of(X3, tptp1)) | ~ next_subocc(X0, X2, tptp0) | ~ occurrence_of(X2, tptp3)) & (~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f80])).
fof(f80, plain, ~ ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3] : ((occurrence_of(X3, tptp2) => ~ ? [X4] : (min_precedes(X2, X4, tptp0) & occurrence_of(X4, tptp1))) & (occurrence_of(X3, tptp1) => ~ ? [X5] : (min_precedes(X2, X5, tptp0) & occurrence_of(X5, tptp2))) & leaf_occ(X3, X1) & min_precedes(X2, X3, tptp0) & (occurrence_of(X3, tptp2) | occurrence_of(X3, tptp1)) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f47])).
fof(f47, plain, ~ ! [X107, X108] : ((~ leaf_occ(X107, X108) & arboreal(X107) & subactivity_occurrence(X107, X108) & occurrence_of(X108, tptp0)) => ? [X109, X110] : ((occurrence_of(X110, tptp2) => ~ ? [X112] : (min_precedes(X109, X112, tptp0) & occurrence_of(X112, tptp1))) & (occurrence_of(X110, tptp1) => ~ ? [X111] : (min_precedes(X109, X111, tptp0) & occurrence_of(X111, tptp2))) & leaf_occ(X110, X108) & min_precedes(X109, X110, tptp0) & (occurrence_of(X110, tptp2) | occurrence_of(X110, tptp1)) & next_subocc(X107, X109, tptp0) & occurrence_of(X109, tptp3))), inference(negated_conjecture, [], [f46])).
fof(f46, plain, ~ ! [X107, X108] : ((~ leaf_occ(X107, X108) & arboreal(X107) & subactivity_occurrence(X107, X108) & occurrence_of(X108, tptp0)) => ? [X109, X110] : ((occurrence_of(X110, tptp2) => ~ ? [X112] : (min_precedes(X109, X112, tptp0) & occurrence_of(X112, tptp1))) & (occurrence_of(X110, tptp1) => ~ ? [X111] : (min_precedes(X109, X111, tptp0) & occurrence_of(X111, tptp2))) & leaf_occ(X110, X108) & min_precedes(X109, X110, tptp0) & (occurrence_of(X110, tptp2) | occurrence_of(X110, tptp1)) & next_subocc(X107, X109, tptp0) & occurrence_of(X109, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', goals)).
fof(f425, plain, (min_precedes(sK12(sK16), sK13(sK16), tptp0) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f424, f253])).
fof(f253, plain, arboreal(sK16), inference(cnf_transformation, [], [f177])).
fof(f424, plain, (~ arboreal(sK16) | min_precedes(sK12(sK16), sK13(sK16), tptp0) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f419, f254])).
fof(f254, plain, ~ leaf_occ(sK16, sK17), inference(cnf_transformation, [], [f177])).
fof(f419, plain, (leaf_occ(sK16, sK17) | ~ arboreal(sK16) | min_precedes(sK12(sK16), sK13(sK16), tptp0) | ~ occurrence_of(sK17, tptp0)), inference(resolution, [], [f233, f252])).
fof(f252, plain, subactivity_occurrence(sK16, sK17), inference(cnf_transformation, [], [f177])).
fof(f233, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | min_precedes(sK12(X0), sK13(X0), tptp0) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ! [X0, X1] : ((! [X5] : ((sK14(X0) = X5) | (sK13(X0) = X5) | ~ min_precedes(sK12(X0), X5, tptp0)) & min_precedes(sK13(X0), sK14(X0), tptp0) & (occurrence_of(sK14(X0), tptp2) | occurrence_of(sK14(X0), tptp1)) & min_precedes(sK12(X0), sK13(X0), tptp0) & occurrence_of(sK13(X0), tptp4) & next_subocc(X0, sK12(X0), tptp0) & occurrence_of(sK12(X0), tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12, sK13, sK14])], [f132, f169])).
fof(f169, plain, ! [X0] : (? [X2, X3, X4] : (! [X5] : ((X4 = X5) | (X3 = X5) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) => (! [X5] : ((sK14(X0) = X5) | (sK13(X0) = X5) | ~ min_precedes(sK12(X0), X5, tptp0)) & min_precedes(sK13(X0), sK14(X0), tptp0) & (occurrence_of(sK14(X0), tptp2) | occurrence_of(sK14(X0), tptp1)) & min_precedes(sK12(X0), sK13(X0), tptp0) & occurrence_of(sK13(X0), tptp4) & next_subocc(X0, sK12(X0), tptp0) & occurrence_of(sK12(X0), tptp3))), introduced(choice_axiom, [])).
fof(f132, plain, ! [X0, X1] : (? [X2, X3, X4] : (! [X5] : ((X4 = X5) | (X3 = X5) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(flattening, [], [f131])).
fof(f131, plain, ! [X0, X1] : (? [X2, X3, X4] : (! [X5] : (((X4 = X5) | (X3 = X5)) | ~ min_precedes(X2, X5, tptp0)) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | (leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3, X4] : (! [X5] : (min_precedes(X2, X5, tptp0) => ((X4 = X5) | (X3 = X5))) & min_precedes(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & min_precedes(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f33])).
fof(f33, plain, ! [X101, X102] : ((~ leaf_occ(X101, X102) & arboreal(X101) & subactivity_occurrence(X101, X102) & occurrence_of(X102, tptp0)) => ? [X103, X104, X105] : (! [X106] : (min_precedes(X103, X106, tptp0) => ((X105 = X106) | (X104 = X106))) & min_precedes(X104, X105, tptp0) & (occurrence_of(X105, tptp2) | occurrence_of(X105, tptp1)) & min_precedes(X103, X104, tptp0) & occurrence_of(X104, tptp4) & next_subocc(X101, X103, tptp0) & occurrence_of(X103, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_32)).
fof(f1881, plain, (! [X37] : ~ min_precedes(sK12(sK16), X37, tptp0) | ~ spl19_81), inference(avatar_component_clause, [], [f1880])).
fof(f1880, plain, (spl19_81 <=> ! [X37] : ~ min_precedes(sK12(sK16), X37, tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_81])])).
fof(f2472, plain, (~ spl19_82 | ~ spl19_104), inference(avatar_contradiction_clause, [], [f2471])).
fof(f2471, plain, ($false | (~ spl19_82 | ~ spl19_104)), inference(subsumption_resolution, [], [f2470, f242])).
fof(f242, plain, ~ (tptp3 = tptp4), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ~ (tptp3 = tptp4), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_39)).
fof(f2470, plain, ((tptp3 = tptp4) | (~ spl19_82 | ~ spl19_104)), inference(backward_demodulation, [], [f428, f2454])).
fof(f2454, plain, ((tptp3 = sK5(sK13(sK16))) | (~ spl19_82 | ~ spl19_104)), inference(backward_demodulation, [], [f1897, f2441])).
fof(f2441, plain, ((sK13(sK16) = sK12(sK12(sK16))) | ~ spl19_104), inference(avatar_component_clause, [], [f2439])).
fof(f2439, plain, (spl19_104 <=> (sK13(sK16) = sK12(sK12(sK16)))), introduced(avatar_definition, [new_symbols(naming, [spl19_104])])).
fof(f1897, plain, ((tptp3 = sK5(sK12(sK12(sK16)))) | ~ spl19_82), inference(resolution, [], [f1885, f278])).
fof(f428, plain, (tptp4 = sK5(sK13(sK16))), inference(resolution, [], [f408, f278])).
fof(f408, plain, occurrence_of(sK13(sK16), tptp4), inference(subsumption_resolution, [], [f407, f251])).
fof(f407, plain, (occurrence_of(sK13(sK16), tptp4) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f406, f253])).
fof(f406, plain, (~ arboreal(sK16) | occurrence_of(sK13(sK16), tptp4) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f401, f254])).
fof(f401, plain, (leaf_occ(sK16, sK17) | ~ arboreal(sK16) | occurrence_of(sK13(sK16), tptp4) | ~ occurrence_of(sK17, tptp0)), inference(resolution, [], [f232, f252])).
fof(f232, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK13(X0), tptp4) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f2442, plain, (spl19_21 | spl19_103 | spl19_104 | ~ spl19_89), inference(avatar_split_clause, [], [f2433, f2109, f2439, f2435, f898])).
fof(f898, plain, (spl19_21 <=> ! [X0] : (~ subactivity_occurrence(sK16, X0) | ~ occurrence_of(X0, tptp0))), introduced(avatar_definition, [new_symbols(naming, [spl19_21])])).
fof(f2109, plain, (spl19_89 <=> next_subocc(sK12(sK16), sK12(sK12(sK16)), tptp0)), introduced(avatar_definition, [new_symbols(naming, [spl19_89])])).
fof(f2433, plain, (! [X1] : ((sK13(sK16) = sK12(sK12(sK16))) | (sK14(sK16) = sK12(sK12(sK16))) | ~ subactivity_occurrence(sK16, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl19_89), inference(subsumption_resolution, [], [f2432, f516])).
fof(f516, plain, ! [X2] : (~ leaf_occ(sK16, X2) | ~ occurrence_of(X2, tptp0)), inference(resolution, [], [f476, f191])).
fof(f191, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(flattening, [], [f103])).
fof(f103, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | (~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2))), inference(ennf_transformation, [], [f56])).
fof(f56, plain, ! [X0, X1, X2] : ((leaf_occ(X1, X0) & occurrence_of(X0, X2)) => ~ ? [X3] : min_precedes(X1, X3, X2)), inference(rectify, [], [f10])).
fof(f10, plain, ! [X31, X32, X33] : ((leaf_occ(X32, X31) & occurrence_of(X31, X33)) => ~ ? [X34] : min_precedes(X32, X34, X33)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_09)).
fof(f476, plain, min_precedes(sK16, sK12(sK16), tptp0), inference(resolution, [], [f417, f221])).
fof(f221, plain, ! [X2, X0, X1] : (~ next_subocc(X0, X1, X2) | min_precedes(X0, X1, X2)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (min_precedes(sK11(X0, X1, X2), X1, X2) & min_precedes(X0, sK11(X0, X1, X2), X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f166, f167])).
fof(f167, plain, ! [X2, X1, X0] : (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) => (min_precedes(sK11(X0, X1, X2), X1, X2) & min_precedes(X0, sK11(X0, X1, X2), X2))), introduced(choice_axiom, [])).
fof(f166, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(rectify, [], [f165])).
fof(f165, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(flattening, [], [f164])).
fof(f164, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2))) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(nnf_transformation, [], [f120])).
fof(f120, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (~ ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(rectify, [], [f27])).
fof(f27, plain, ! [X78, X79, X80] : (next_subocc(X78, X79, X80) <=> (~ ? [X81] : (min_precedes(X81, X79, X80) & min_precedes(X78, X81, X80)) & min_precedes(X78, X79, X80))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_26)).
fof(f417, plain, next_subocc(sK16, sK12(sK16), tptp0), inference(subsumption_resolution, [], [f416, f251])).
fof(f416, plain, (next_subocc(sK16, sK12(sK16), tptp0) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f415, f253])).
fof(f415, plain, (~ arboreal(sK16) | next_subocc(sK16, sK12(sK16), tptp0) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f410, f254])).
fof(f410, plain, (leaf_occ(sK16, sK17) | ~ arboreal(sK16) | next_subocc(sK16, sK12(sK16), tptp0) | ~ occurrence_of(sK17, tptp0)), inference(resolution, [], [f231, f252])).
fof(f231, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | next_subocc(X0, sK12(X0), tptp0) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f2432, plain, (! [X1] : ((sK13(sK16) = sK12(sK12(sK16))) | (sK14(sK16) = sK12(sK12(sK16))) | leaf_occ(sK16, X1) | ~ subactivity_occurrence(sK16, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl19_89), inference(subsumption_resolution, [], [f2387, f253])).
fof(f2387, plain, (! [X1] : ((sK13(sK16) = sK12(sK12(sK16))) | (sK14(sK16) = sK12(sK12(sK16))) | leaf_occ(sK16, X1) | ~ arboreal(sK16) | ~ subactivity_occurrence(sK16, X1) | ~ occurrence_of(X1, tptp0)) | ~ spl19_89), inference(resolution, [], [f2329, f236])).
fof(f236, plain, ! [X0, X5, X1] : (~ min_precedes(sK12(X0), X5, tptp0) | (sK13(X0) = X5) | (sK14(X0) = X5) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f2329, plain, (min_precedes(sK12(sK16), sK12(sK12(sK16)), tptp0) | ~ spl19_89), inference(resolution, [], [f2111, f221])).
fof(f2111, plain, (next_subocc(sK12(sK16), sK12(sK12(sK16)), tptp0) | ~ spl19_89), inference(avatar_component_clause, [], [f2109])).
fof(f2112, plain, (spl19_81 | spl19_89), inference(avatar_split_clause, [], [f2107, f2109, f1880])).
fof(f2107, plain, ! [X37] : (next_subocc(sK12(sK16), sK12(sK12(sK16)), tptp0) | ~ min_precedes(sK12(sK16), X37, tptp0)), inference(subsumption_resolution, [], [f2106, f185])).
fof(f185, plain, ! [X2, X0, X1] : (occurrence_of(sK3(X0, X1, X2), X0) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f142])).
fof(f142, plain, ! [X0, X1, X2] : ((subactivity_occurrence(X2, sK3(X0, X1, X2)) & subactivity_occurrence(X1, sK3(X0, X1, X2)) & occurrence_of(sK3(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3])], [f98, f141])).
fof(f141, plain, ! [X2, X1, X0] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) => (subactivity_occurrence(X2, sK3(X0, X1, X2)) & subactivity_occurrence(X1, sK3(X0, X1, X2)) & occurrence_of(sK3(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f98, plain, ! [X0, X1, X2] : (? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f53])).
fof(f53, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3] : (subactivity_occurrence(X2, X3) & subactivity_occurrence(X1, X3) & occurrence_of(X3, X0))), inference(rectify, [], [f7])).
fof(f7, plain, ! [X21, X22, X23] : (min_precedes(X22, X23, X21) => ? [X24] : (subactivity_occurrence(X23, X24) & subactivity_occurrence(X22, X24) & occurrence_of(X24, X21))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_06)).
fof(f2106, plain, ! [X37] : (next_subocc(sK12(sK16), sK12(sK12(sK16)), tptp0) | ~ min_precedes(sK12(sK16), X37, tptp0) | ~ occurrence_of(sK3(tptp0, sK12(sK16), X37), tptp0)), inference(subsumption_resolution, [], [f2100, f400])).
fof(f400, plain, arboreal(sK12(sK16)), inference(subsumption_resolution, [], [f397, f241])).
fof(f241, plain, atomic(tptp3), inference(cnf_transformation, [], [f39])).
fof(f39, plain, atomic(tptp3), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_38)).
fof(f397, plain, (~ atomic(tptp3) | arboreal(sK12(sK16))), inference(resolution, [], [f393, f204])).
fof(f204, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ atomic(X1) | arboreal(X0)), inference(cnf_transformation, [], [f155])).
fof(f155, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f112])).
fof(f112, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f63])).
fof(f63, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f17])).
fof(f17, plain, ! [X51, X52] : (occurrence_of(X51, X52) => (arboreal(X51) <=> atomic(X52))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO018+4.p', sos_16)).
fof(f393, plain, occurrence_of(sK12(sK16), tptp3), inference(subsumption_resolution, [], [f392, f251])).
fof(f392, plain, (occurrence_of(sK12(sK16), tptp3) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f391, f253])).
fof(f391, plain, (~ arboreal(sK16) | occurrence_of(sK12(sK16), tptp3) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f386, f254])).
fof(f386, plain, (leaf_occ(sK16, sK17) | ~ arboreal(sK16) | occurrence_of(sK12(sK16), tptp3) | ~ occurrence_of(sK17, tptp0)), inference(resolution, [], [f230, f252])).
fof(f230, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK12(X0), tptp3) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).
fof(f2100, plain, ! [X37] : (next_subocc(sK12(sK16), sK12(sK12(sK16)), tptp0) | ~ arboreal(sK12(sK16)) | ~ min_precedes(sK12(sK16), X37, tptp0) | ~ occurrence_of(sK3(tptp0, sK12(sK16), X37), tptp0)), inference(resolution, [], [f994, f546])).
fof(f546, plain, ! [X3] : (~ leaf_occ(sK12(sK16), X3) | ~ occurrence_of(X3, tptp0)), inference(resolution, [], [f426, f191])).
fof(f994, plain, ! [X0, X1] : (leaf_occ(X0, sK3(tptp0, X0, X1)) | next_subocc(X0, sK12(X0), tptp0) | ~ arboreal(X0) | ~ min_precedes(X0, X1, tptp0)), inference(duplicate_literal_removal, [], [f993])).
fof(f993, plain, ! [X0, X1] : (~ arboreal(X0) | next_subocc(X0, sK12(X0), tptp0) | leaf_occ(X0, sK3(tptp0, X0, X1)) | ~ min_precedes(X0, X1, tptp0) | ~ min_precedes(X0, X1, tptp0)), inference(resolution, [], [f412, f185])).
fof(f412, plain, ! [X4, X2, X3] : (~ occurrence_of(sK3(X3, X2, X4), tptp0) | ~ arboreal(X2) | next_subocc(X2, sK12(X2), tptp0) | leaf_occ(X2, sK3(X3, X2, X4)) | ~ min_precedes(X2, X4, X3)), inference(resolution, [], [f231, f186])).
fof(f186, plain, ! [X2, X0, X1] : (subactivity_occurrence(X1, sK3(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f142])).
fof(f1886, plain, (spl19_81 | spl19_82), inference(avatar_split_clause, [], [f1878, f1883, f1880])).
fof(f1878, plain, ! [X37] : (occurrence_of(sK12(sK12(sK16)), tptp3) | ~ min_precedes(sK12(sK16), X37, tptp0)), inference(subsumption_resolution, [], [f1877, f185])).
fof(f1877, plain, ! [X37] : (occurrence_of(sK12(sK12(sK16)), tptp3) | ~ min_precedes(sK12(sK16), X37, tptp0) | ~ occurrence_of(sK3(tptp0, sK12(sK16), X37), tptp0)), inference(subsumption_resolution, [], [f1871, f400])).
fof(f1871, plain, ! [X37] : (occurrence_of(sK12(sK12(sK16)), tptp3) | ~ arboreal(sK12(sK16)) | ~ min_precedes(sK12(sK16), X37, tptp0) | ~ occurrence_of(sK3(tptp0, sK12(sK16), X37), tptp0)), inference(resolution, [], [f964, f546])).
fof(f964, plain, ! [X0, X1] : (leaf_occ(X0, sK3(tptp0, X0, X1)) | occurrence_of(sK12(X0), tptp3) | ~ arboreal(X0) | ~ min_precedes(X0, X1, tptp0)), inference(duplicate_literal_removal, [], [f963])).
fof(f963, plain, ! [X0, X1] : (~ arboreal(X0) | occurrence_of(sK12(X0), tptp3) | leaf_occ(X0, sK3(tptp0, X0, X1)) | ~ min_precedes(X0, X1, tptp0) | ~ min_precedes(X0, X1, tptp0)), inference(resolution, [], [f388, f185])).
fof(f388, plain, ! [X4, X2, X3] : (~ occurrence_of(sK3(X3, X2, X4), tptp0) | ~ arboreal(X2) | occurrence_of(sK12(X2), tptp3) | leaf_occ(X2, sK3(X3, X2, X4)) | ~ min_precedes(X2, X4, X3)), inference(resolution, [], [f230, f186])).
fof(f974, plain, ~ spl19_21, inference(avatar_contradiction_clause, [], [f973])).
fof(f973, plain, ($false | ~ spl19_21), inference(subsumption_resolution, [], [f969, f251])).
fof(f969, plain, (~ occurrence_of(sK17, tptp0) | ~ spl19_21), inference(resolution, [], [f899, f252])).
fof(f899, plain, (! [X0] : (~ subactivity_occurrence(sK16, X0) | ~ occurrence_of(X0, tptp0)) | ~ spl19_21), inference(avatar_component_clause, [], [f898])).
fof(f460, plain, (spl19_3 | spl19_4), inference(avatar_split_clause, [], [f451, f457, f453])).
fof(f451, plain, (occurrence_of(sK14(sK16), tptp1) | occurrence_of(sK14(sK16), tptp2)), inference(subsumption_resolution, [], [f450, f251])).
fof(f450, plain, (occurrence_of(sK14(sK16), tptp1) | occurrence_of(sK14(sK16), tptp2) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f449, f253])).
fof(f449, plain, (occurrence_of(sK14(sK16), tptp1) | ~ arboreal(sK16) | occurrence_of(sK14(sK16), tptp2) | ~ occurrence_of(sK17, tptp0)), inference(subsumption_resolution, [], [f444, f254])).
fof(f444, plain, (occurrence_of(sK14(sK16), tptp1) | leaf_occ(sK16, sK17) | ~ arboreal(sK16) | occurrence_of(sK14(sK16), tptp2) | ~ occurrence_of(sK17, tptp0)), inference(resolution, [], [f234, f252])).
fof(f234, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | occurrence_of(sK14(X0), tptp1) | leaf_occ(X0, X1) | ~ arboreal(X0) | occurrence_of(sK14(X0), tptp2) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f170])).