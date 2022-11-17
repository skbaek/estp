fof(f100581, plain, $false, inference(avatar_sat_refutation, [], [f2559, f3963, f4001, f6120, f6150, f6168, f6324, f10851, f21444, f24380, f26777, f100580])).
fof(f100580, plain, (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590), inference(avatar_contradiction_clause, [], [f100579])).
fof(f100579, plain, ($false | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f100578, f5799])).
fof(f5799, plain, (subactivity_occurrence(sK17(sK20, sK21), sK21) | ~ spl23_157), inference(avatar_component_clause, [], [f5798])).
fof(f5798, plain, (spl23_157 <=> subactivity_occurrence(sK17(sK20, sK21), sK21)), introduced(avatar_definition, [new_symbols(naming, [spl23_157])])).
fof(f100578, plain, (~ subactivity_occurrence(sK17(sK20, sK21), sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f100577, f3962])).
fof(f3962, plain, (arboreal(sK17(sK20, sK21)) | ~ spl23_78), inference(avatar_component_clause, [], [f3960])).
fof(f3960, plain, (spl23_78 <=> arboreal(sK17(sK20, sK21))), introduced(avatar_definition, [new_symbols(naming, [spl23_78])])).
fof(f100577, plain, (~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f100576, f22422])).
fof(f22422, plain, (~ leaf_occ(sK17(sK20, sK21), sK21) | spl23_523), inference(avatar_component_clause, [], [f22421])).
fof(f22421, plain, (spl23_523 <=> leaf_occ(sK17(sK20, sK21), sK21)), introduced(avatar_definition, [new_symbols(naming, [spl23_523])])).
fof(f100576, plain, (leaf_occ(sK17(sK20, sK21), sK21) | ~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f100575, f344])).
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
fof(f64, plain, ~ ! [X166, X167] : ((~ leaf_occ(X166, X167) & arboreal(X166) & subactivity_occurrence(X166, X167) & occurrence_of(X167, tptp0)) => ? [X168, X169] : ((occurrence_of(X169, tptp2) => ~ ? [X171] : (min_precedes(X168, X171, tptp0) & occurrence_of(X171, tptp1))) & (occurrence_of(X169, tptp1) => ~ ? [X170] : (min_precedes(X168, X170, tptp0) & occurrence_of(X170, tptp2))) & leaf_occ(X169, X167) & min_precedes(X168, X169, tptp0) & (occurrence_of(X169, tptp2) | occurrence_of(X169, tptp1)) & next_subocc(X166, X168, tptp0) & occurrence_of(X168, tptp3))), inference(negated_conjecture, [], [f63])).
fof(f63, plain, ~ ! [X166, X167] : ((~ leaf_occ(X166, X167) & arboreal(X166) & subactivity_occurrence(X166, X167) & occurrence_of(X167, tptp0)) => ? [X168, X169] : ((occurrence_of(X169, tptp2) => ~ ? [X171] : (min_precedes(X168, X171, tptp0) & occurrence_of(X171, tptp1))) & (occurrence_of(X169, tptp1) => ~ ? [X170] : (min_precedes(X168, X170, tptp0) & occurrence_of(X170, tptp2))) & leaf_occ(X169, X167) & min_precedes(X168, X169, tptp0) & (occurrence_of(X169, tptp2) | occurrence_of(X169, tptp1)) & next_subocc(X166, X168, tptp0) & occurrence_of(X168, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', goals)).
fof(f100575, plain, (~ occurrence_of(sK21, tptp0) | leaf_occ(sK17(sK20, sK21), sK21) | ~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(resolution, [], [f100505, f3528])).
fof(f3528, plain, ! [X2, X3] : (earlier(X2, sK16(X2, X3)) | ~ occurrence_of(X3, tptp0) | leaf_occ(X2, X3) | ~ arboreal(X2) | ~ subactivity_occurrence(X2, X3)), inference(resolution, [], [f1235, f256])).
fof(f256, plain, ! [X0, X1] : (~ precedes(X0, X1) | earlier(X0, X1)), inference(cnf_transformation, [], [f198])).
fof(f198, plain, ! [X0, X1] : ((precedes(X0, X1) | ~ legal(X1) | ~ earlier(X0, X1)) & ((legal(X1) & earlier(X0, X1)) | ~ precedes(X0, X1))), inference(flattening, [], [f197])).
fof(f197, plain, ! [X0, X1] : ((precedes(X0, X1) | (~ legal(X1) | ~ earlier(X0, X1))) & ((legal(X1) & earlier(X0, X1)) | ~ precedes(X0, X1))), inference(nnf_transformation, [], [f74])).
fof(f74, plain, ! [X0, X1] : (precedes(X0, X1) <=> (legal(X1) & earlier(X0, X1))), inference(rectify, [], [f11])).
fof(f11, plain, ! [X21, X22] : (precedes(X21, X22) <=> (legal(X22) & earlier(X21, X22))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_10)).
fof(f1235, plain, ! [X8, X9] : (precedes(X8, sK16(X8, X9)) | ~ subactivity_occurrence(X8, X9) | ~ occurrence_of(X9, tptp0) | leaf_occ(X8, X9) | ~ arboreal(X8)), inference(resolution, [], [f649, f268])).
fof(f268, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | precedes(X0, X1)), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ! [X0, X1, X2] : (precedes(X0, X1) | ~ min_precedes(X0, X1, X2)), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1, X2] : (min_precedes(X0, X1, X2) => precedes(X0, X1)), inference(rectify, [], [f16])).
fof(f16, plain, ! [X38, X39, X40] : (min_precedes(X38, X39, X40) => precedes(X38, X39)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_15)).
fof(f649, plain, ! [X6, X7] : (min_precedes(X6, sK16(X6, X7), tptp0) | ~ arboreal(X6) | ~ subactivity_occurrence(X6, X7) | ~ occurrence_of(X7, tptp0) | leaf_occ(X6, X7)), inference(resolution, [], [f323, f278])).
fof(f278, plain, ! [X2, X0, X1] : (~ next_subocc(X0, X1, X2) | min_precedes(X0, X1, X2)), inference(cnf_transformation, [], [f215])).
fof(f215, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (min_precedes(sK8(X0, X1, X2), X1, X2) & min_precedes(X0, sK8(X0, X1, X2), X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f213, f214])).
fof(f214, plain, ! [X2, X1, X0] : (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) => (min_precedes(sK8(X0, X1, X2), X1, X2) & min_precedes(X0, sK8(X0, X1, X2), X2))), introduced(choice_axiom, [])).
fof(f213, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X4] : (~ min_precedes(X4, X1, X2) | ~ min_precedes(X0, X4, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(rectify, [], [f212])).
fof(f212, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2)) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(flattening, [], [f211])).
fof(f211, plain, ! [X0, X1, X2] : ((next_subocc(X0, X1, X2) | (? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) | ~ min_precedes(X0, X1, X2))) & ((! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2)) | ~ next_subocc(X0, X1, X2))), inference(nnf_transformation, [], [f144])).
fof(f144, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (! [X3] : (~ min_precedes(X3, X1, X2) | ~ min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ! [X0, X1, X2] : (next_subocc(X0, X1, X2) <=> (~ ? [X3] : (min_precedes(X3, X1, X2) & min_precedes(X0, X3, X2)) & min_precedes(X0, X1, X2))), inference(rectify, [], [f23])).
fof(f23, plain, ! [X60, X61, X62] : (next_subocc(X60, X61, X62) <=> (~ ? [X63] : (min_precedes(X63, X61, X62) & min_precedes(X60, X63, X62)) & min_precedes(X60, X61, X62))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_22)).
fof(f323, plain, ! [X0, X1] : (next_subocc(X0, sK16(X0, X1), tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f235, plain, ! [X0, X1] : ((leaf_occ(sK18(X0, X1), X1) & next_subocc(sK17(X0, X1), sK18(X0, X1), tptp0) & (occurrence_of(sK18(X0, X1), tptp2) | occurrence_of(sK18(X0, X1), tptp1)) & next_subocc(sK16(X0, X1), sK17(X0, X1), tptp0) & occurrence_of(sK17(X0, X1), tptp4) & next_subocc(X0, sK16(X0, X1), tptp0) & occurrence_of(sK16(X0, X1), tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16, sK17, sK18])], [f189, f234])).
fof(f234, plain, ! [X1, X0] : (? [X2, X3, X4] : (leaf_occ(X4, X1) & next_subocc(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & next_subocc(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) => (leaf_occ(sK18(X0, X1), X1) & next_subocc(sK17(X0, X1), sK18(X0, X1), tptp0) & (occurrence_of(sK18(X0, X1), tptp2) | occurrence_of(sK18(X0, X1), tptp1)) & next_subocc(sK16(X0, X1), sK17(X0, X1), tptp0) & occurrence_of(sK17(X0, X1), tptp4) & next_subocc(X0, sK16(X0, X1), tptp0) & occurrence_of(sK16(X0, X1), tptp3))), introduced(choice_axiom, [])).
fof(f189, plain, ! [X0, X1] : (? [X2, X3, X4] : (leaf_occ(X4, X1) & next_subocc(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & next_subocc(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(flattening, [], [f188])).
fof(f188, plain, ! [X0, X1] : (? [X2, X3, X4] : (leaf_occ(X4, X1) & next_subocc(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & next_subocc(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3)) | (leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0))), inference(ennf_transformation, [], [f113])).
fof(f113, plain, ! [X0, X1] : ((~ leaf_occ(X0, X1) & arboreal(X0) & subactivity_occurrence(X0, X1) & occurrence_of(X1, tptp0)) => ? [X2, X3, X4] : (leaf_occ(X4, X1) & next_subocc(X3, X4, tptp0) & (occurrence_of(X4, tptp2) | occurrence_of(X4, tptp1)) & next_subocc(X2, X3, tptp0) & occurrence_of(X3, tptp4) & next_subocc(X0, X2, tptp0) & occurrence_of(X2, tptp3))), inference(rectify, [], [f50])).
fof(f50, plain, ! [X161, X162] : ((~ leaf_occ(X161, X162) & arboreal(X161) & subactivity_occurrence(X161, X162) & occurrence_of(X162, tptp0)) => ? [X163, X164, X165] : (leaf_occ(X165, X162) & next_subocc(X164, X165, tptp0) & (occurrence_of(X165, tptp2) | occurrence_of(X165, tptp1)) & next_subocc(X163, X164, tptp0) & occurrence_of(X164, tptp4) & next_subocc(X161, X163, tptp0) & occurrence_of(X163, tptp3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_49)).
fof(f100505, plain, (~ earlier(sK17(sK20, sK21), sK16(sK17(sK20, sK21), sK21)) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(resolution, [], [f100109, f249])).
fof(f249, plain, ! [X0, X1] : (~ earlier(X1, X0) | ~ earlier(X0, X1)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0, X1] : (~ earlier(X1, X0) | ~ earlier(X0, X1)), inference(ennf_transformation, [], [f68])).
fof(f68, plain, ! [X0, X1] : (earlier(X0, X1) => ~ earlier(X1, X0)), inference(rectify, [], [f5])).
fof(f5, plain, ! [X8, X9] : (earlier(X8, X9) => ~ earlier(X9, X8)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_04)).
fof(f100109, plain, (earlier(sK16(sK17(sK20, sK21), sK21), sK17(sK20, sK21)) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f100108, f5799])).
fof(f100108, plain, (earlier(sK16(sK17(sK20, sK21), sK21), sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f100107, f3962])).
fof(f100107, plain, (earlier(sK16(sK17(sK20, sK21), sK21), sK17(sK20, sK21)) | ~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f100106, f22422])).
fof(f100106, plain, (earlier(sK16(sK17(sK20, sK21), sK21), sK17(sK20, sK21)) | leaf_occ(sK17(sK20, sK21), sK21) | ~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f99932, f344])).
fof(f99932, plain, (earlier(sK16(sK17(sK20, sK21), sK21), sK17(sK20, sK21)) | ~ occurrence_of(sK21, tptp0) | leaf_occ(sK17(sK20, sK21), sK21) | ~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(superposition, [], [f4696, f95779])).
fof(f95779, plain, ((sK17(sK20, sK21) = sK17(sK17(sK20, sK21), sK21)) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f95778, f347])).
fof(f347, plain, ~ leaf_occ(sK20, sK21), inference(cnf_transformation, [], [f242])).
fof(f95778, plain, ((sK17(sK20, sK21) = sK17(sK17(sK20, sK21), sK21)) | leaf_occ(sK20, sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f95777, f344])).
fof(f95777, plain, (~ occurrence_of(sK21, tptp0) | (sK17(sK20, sK21) = sK17(sK17(sK20, sK21), sK21)) | leaf_occ(sK20, sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f95776, f345])).
fof(f345, plain, subactivity_occurrence(sK20, sK21), inference(cnf_transformation, [], [f242])).
fof(f95776, plain, (~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | (sK17(sK20, sK21) = sK17(sK17(sK20, sK21), sK21)) | leaf_occ(sK20, sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f95769, f346])).
fof(f346, plain, arboreal(sK20), inference(cnf_transformation, [], [f242])).
fof(f95769, plain, (~ arboreal(sK20) | ~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | (sK17(sK20, sK21) = sK17(sK17(sK20, sK21), sK21)) | leaf_occ(sK20, sK21) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(resolution, [], [f27818, f666])).
fof(f666, plain, ! [X4, X5, X3] : (~ next_subocc(X5, sK18(X3, X4), tptp0) | ~ arboreal(X3) | ~ subactivity_occurrence(X3, X4) | ~ occurrence_of(X4, tptp0) | (sK17(X3, X4) = X5) | leaf_occ(X3, X4)), inference(resolution, [], [f327, f311])).
fof(f311, plain, ! [X2, X0, X3, X1] : (~ next_subocc(X2, X0, X3) | (X1 = X2) | ~ next_subocc(X1, X0, X3)), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ! [X0, X1, X2, X3] : ((X1 = X2) | ~ next_subocc(X2, X0, X3) | ~ next_subocc(X1, X0, X3)), inference(flattening, [], [f172])).
fof(f172, plain, ! [X0, X1, X2, X3] : ((X1 = X2) | (~ next_subocc(X2, X0, X3) | ~ next_subocc(X1, X0, X3))), inference(ennf_transformation, [], [f104])).
fof(f104, plain, ! [X0, X1, X2, X3] : ((next_subocc(X2, X0, X3) & next_subocc(X1, X0, X3)) => (X1 = X2)), inference(rectify, [], [f41])).
fof(f41, plain, ! [X126, X127, X128, X129] : ((next_subocc(X128, X126, X129) & next_subocc(X127, X126, X129)) => (X127 = X128)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_40)).
fof(f327, plain, ! [X0, X1] : (next_subocc(sK17(X0, X1), sK18(X0, X1), tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f27818, plain, (next_subocc(sK17(sK17(sK20, sK21), sK21), sK18(sK20, sK21), tptp0) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f27817, f344])).
fof(f27817, plain, (next_subocc(sK17(sK17(sK20, sK21), sK21), sK18(sK20, sK21), tptp0) | ~ occurrence_of(sK21, tptp0) | (~ spl23_78 | ~ spl23_157 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f27816, f5799])).
fof(f27816, plain, (next_subocc(sK17(sK17(sK20, sK21), sK21), sK18(sK20, sK21), tptp0) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | ~ occurrence_of(sK21, tptp0) | (~ spl23_78 | ~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f27815, f3962])).
fof(f27815, plain, (next_subocc(sK17(sK17(sK20, sK21), sK21), sK18(sK20, sK21), tptp0) | ~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | ~ occurrence_of(sK21, tptp0) | (~ spl23_167 | spl23_523 | ~ spl23_590)), inference(subsumption_resolution, [], [f27739, f22422])).
fof(f27739, plain, (next_subocc(sK17(sK17(sK20, sK21), sK21), sK18(sK20, sK21), tptp0) | leaf_occ(sK17(sK20, sK21), sK21) | ~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | ~ occurrence_of(sK21, tptp0) | (~ spl23_167 | ~ spl23_590)), inference(superposition, [], [f327, f26840])).
fof(f26840, plain, ((sK18(sK20, sK21) = sK18(sK17(sK20, sK21), sK21)) | (~ spl23_167 | ~ spl23_590)), inference(resolution, [], [f26742, f6149])).
fof(f6149, plain, (! [X0] : (~ leaf_occ(X0, sK21) | (sK18(sK20, sK21) = X0)) | ~ spl23_167), inference(avatar_component_clause, [], [f6148])).
fof(f6148, plain, (spl23_167 <=> ! [X0] : ((sK18(sK20, sK21) = X0) | ~ leaf_occ(X0, sK21))), introduced(avatar_definition, [new_symbols(naming, [spl23_167])])).
fof(f26742, plain, (leaf_occ(sK18(sK17(sK20, sK21), sK21), sK21) | ~ spl23_590), inference(avatar_component_clause, [], [f26741])).
fof(f26741, plain, (spl23_590 <=> leaf_occ(sK18(sK17(sK20, sK21), sK21), sK21)), introduced(avatar_definition, [new_symbols(naming, [spl23_590])])).
fof(f4696, plain, ! [X0, X1] : (earlier(sK16(X0, X1), sK17(X0, X1)) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1)), inference(resolution, [], [f1601, f256])).
fof(f1601, plain, ! [X6, X7] : (precedes(sK16(X6, X7), sK17(X6, X7)) | ~ subactivity_occurrence(X6, X7) | ~ occurrence_of(X7, tptp0) | leaf_occ(X6, X7) | ~ arboreal(X6)), inference(resolution, [], [f662, f268])).
fof(f662, plain, ! [X6, X7] : (min_precedes(sK16(X6, X7), sK17(X6, X7), tptp0) | ~ arboreal(X6) | ~ subactivity_occurrence(X6, X7) | ~ occurrence_of(X7, tptp0) | leaf_occ(X6, X7)), inference(resolution, [], [f325, f278])).
fof(f325, plain, ! [X0, X1] : (next_subocc(sK16(X0, X1), sK17(X0, X1), tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f26777, plain, (~ spl23_78 | ~ spl23_157 | spl23_523 | spl23_590), inference(avatar_contradiction_clause, [], [f26776])).
fof(f26776, plain, ($false | (~ spl23_78 | ~ spl23_157 | spl23_523 | spl23_590)), inference(subsumption_resolution, [], [f26775, f344])).
fof(f26775, plain, (~ occurrence_of(sK21, tptp0) | (~ spl23_78 | ~ spl23_157 | spl23_523 | spl23_590)), inference(subsumption_resolution, [], [f26774, f5799])).
fof(f26774, plain, (~ subactivity_occurrence(sK17(sK20, sK21), sK21) | ~ occurrence_of(sK21, tptp0) | (~ spl23_78 | spl23_523 | spl23_590)), inference(subsumption_resolution, [], [f26773, f3962])).
fof(f26773, plain, (~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | ~ occurrence_of(sK21, tptp0) | (spl23_523 | spl23_590)), inference(subsumption_resolution, [], [f26772, f22422])).
fof(f26772, plain, (leaf_occ(sK17(sK20, sK21), sK21) | ~ arboreal(sK17(sK20, sK21)) | ~ subactivity_occurrence(sK17(sK20, sK21), sK21) | ~ occurrence_of(sK21, tptp0) | spl23_590), inference(resolution, [], [f26743, f328])).
fof(f328, plain, ! [X0, X1] : (leaf_occ(sK18(X0, X1), X1) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f26743, plain, (~ leaf_occ(sK18(sK17(sK20, sK21), sK21), sK21) | spl23_590), inference(avatar_component_clause, [], [f26741])).
fof(f24380, plain, ~ spl23_523, inference(avatar_contradiction_clause, [], [f24379])).
fof(f24379, plain, ($false | ~ spl23_523), inference(subsumption_resolution, [], [f24378, f346])).
fof(f24378, plain, (~ arboreal(sK20) | ~ spl23_523), inference(subsumption_resolution, [], [f24377, f347])).
fof(f24377, plain, (leaf_occ(sK20, sK21) | ~ arboreal(sK20) | ~ spl23_523), inference(subsumption_resolution, [], [f24376, f344])).
fof(f24376, plain, (~ occurrence_of(sK21, tptp0) | leaf_occ(sK20, sK21) | ~ arboreal(sK20) | ~ spl23_523), inference(subsumption_resolution, [], [f24374, f345])).
fof(f24374, plain, (~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | leaf_occ(sK20, sK21) | ~ arboreal(sK20) | ~ spl23_523), inference(duplicate_literal_removal, [], [f24342])).
fof(f24342, plain, (~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | leaf_occ(sK20, sK21) | ~ arboreal(sK20) | ~ occurrence_of(sK21, tptp0) | ~ spl23_523), inference(resolution, [], [f22423, f1647])).
fof(f1647, plain, ! [X24, X23, X25] : (~ leaf_occ(sK17(X23, X24), X25) | ~ subactivity_occurrence(X23, X24) | ~ occurrence_of(X24, tptp0) | leaf_occ(X23, X24) | ~ arboreal(X23) | ~ occurrence_of(X25, tptp0)), inference(resolution, [], [f667, f308])).
fof(f308, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | ~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2)), inference(flattening, [], [f166])).
fof(f166, plain, ! [X0, X1, X2] : (! [X3] : ~ min_precedes(X1, X3, X2) | (~ leaf_occ(X1, X0) | ~ occurrence_of(X0, X2))), inference(ennf_transformation, [], [f101])).
fof(f101, plain, ! [X0, X1, X2] : ((leaf_occ(X1, X0) & occurrence_of(X0, X2)) => ~ ? [X3] : min_precedes(X1, X3, X2)), inference(rectify, [], [f38])).
fof(f38, plain, ! [X113, X114, X115] : ((leaf_occ(X114, X113) & occurrence_of(X113, X115)) => ~ ? [X116] : min_precedes(X114, X116, X115)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_37)).
fof(f667, plain, ! [X6, X7] : (min_precedes(sK17(X6, X7), sK18(X6, X7), tptp0) | ~ arboreal(X6) | ~ subactivity_occurrence(X6, X7) | ~ occurrence_of(X7, tptp0) | leaf_occ(X6, X7)), inference(resolution, [], [f327, f278])).
fof(f22423, plain, (leaf_occ(sK17(sK20, sK21), sK21) | ~ spl23_523), inference(avatar_component_clause, [], [f22421])).
fof(f21444, plain, spl23_157, inference(avatar_contradiction_clause, [], [f21443])).
fof(f21443, plain, ($false | spl23_157), inference(subsumption_resolution, [], [f21442, f345])).
fof(f21442, plain, (~ subactivity_occurrence(sK20, sK21) | spl23_157), inference(subsumption_resolution, [], [f21441, f346])).
fof(f21441, plain, (~ arboreal(sK20) | ~ subactivity_occurrence(sK20, sK21) | spl23_157), inference(subsumption_resolution, [], [f21440, f347])).
fof(f21440, plain, (leaf_occ(sK20, sK21) | ~ arboreal(sK20) | ~ subactivity_occurrence(sK20, sK21) | spl23_157), inference(subsumption_resolution, [], [f21351, f344])).
fof(f21351, plain, (~ occurrence_of(sK21, tptp0) | leaf_occ(sK20, sK21) | ~ arboreal(sK20) | ~ subactivity_occurrence(sK20, sK21) | spl23_157), inference(resolution, [], [f7369, f5800])).
fof(f5800, plain, (~ subactivity_occurrence(sK17(sK20, sK21), sK21) | spl23_157), inference(avatar_component_clause, [], [f5798])).
fof(f7369, plain, ! [X0, X1] : (subactivity_occurrence(sK17(X0, X1), X1) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1)), inference(duplicate_literal_removal, [], [f7358])).
fof(f7358, plain, ! [X0, X1] : (~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ occurrence_of(X1, tptp0) | subactivity_occurrence(sK17(X0, X1), X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0) | leaf_occ(X0, X1)), inference(resolution, [], [f1646, f625])).
fof(f625, plain, ! [X14, X15] : (subactivity_occurrence(sK18(X14, X15), X15) | ~ arboreal(X14) | ~ subactivity_occurrence(X14, X15) | ~ occurrence_of(X15, tptp0) | leaf_occ(X14, X15)), inference(resolution, [], [f328, f303])).
fof(f303, plain, ! [X0, X1] : (~ leaf_occ(X0, X1) | subactivity_occurrence(X0, X1)), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & ((leaf(X0, sK14(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK14(X0, X1))) | ~ leaf_occ(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f229, f230])).
fof(f230, plain, ! [X1, X0] : (? [X3] : (leaf(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) => (leaf(X0, sK14(X0, X1)) & subactivity_occurrence(X0, X1) & occurrence_of(X1, sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f229, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X3] : (leaf(X0, X3) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X3)) | ~ leaf_occ(X0, X1))), inference(rectify, [], [f228])).
fof(f228, plain, ! [X0, X1] : ((leaf_occ(X0, X1) | ! [X2] : (~ leaf(X0, X2) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, X2))) & (? [X2] : (leaf(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2)) | ~ leaf_occ(X0, X1))), inference(nnf_transformation, [], [f98])).
fof(f98, plain, ! [X0, X1] : (leaf_occ(X0, X1) <=> ? [X2] : (leaf(X0, X2) & subactivity_occurrence(X0, X1) & occurrence_of(X1, X2))), inference(rectify, [], [f35])).
fof(f35, plain, ! [X102, X103] : (leaf_occ(X102, X103) <=> ? [X104] : (leaf(X102, X104) & subactivity_occurrence(X102, X103) & occurrence_of(X103, X104))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_34)).
fof(f1646, plain, ! [X21, X22, X20] : (~ subactivity_occurrence(sK18(X20, X21), X22) | ~ subactivity_occurrence(X20, X21) | ~ occurrence_of(X21, tptp0) | leaf_occ(X20, X21) | ~ arboreal(X20) | ~ occurrence_of(X22, tptp0) | subactivity_occurrence(sK17(X20, X21), X22)), inference(resolution, [], [f667, f296])).
fof(f296, plain, ! [X2, X0, X3, X1] : (~ min_precedes(X0, X1, X2) | ~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | subactivity_occurrence(X0, X3)), inference(cnf_transformation, [], [f154])).
fof(f154, plain, ! [X0, X1, X2, X3] : (subactivity_occurrence(X0, X3) | ~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | ~ min_precedes(X0, X1, X2)), inference(flattening, [], [f153])).
fof(f153, plain, ! [X0, X1, X2, X3] : (subactivity_occurrence(X0, X3) | (~ subactivity_occurrence(X1, X3) | ~ occurrence_of(X3, X2) | ~ min_precedes(X0, X1, X2))), inference(ennf_transformation, [], [f93])).
fof(f93, plain, ! [X0, X1, X2, X3] : ((subactivity_occurrence(X1, X3) & occurrence_of(X3, X2) & min_precedes(X0, X1, X2)) => subactivity_occurrence(X0, X3)), inference(rectify, [], [f30])).
fof(f30, plain, ! [X83, X84, X85, X86] : ((subactivity_occurrence(X84, X86) & occurrence_of(X86, X85) & min_precedes(X83, X84, X85)) => subactivity_occurrence(X83, X86)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_29)).
fof(f10851, plain, (spl23_26 | ~ spl23_169), inference(avatar_split_clause, [], [f7527, f6162, f1475])).
fof(f1475, plain, (spl23_26 <=> ! [X12] : (~ occurrence_of(sK21, X12) | ~ atomic(X12))), introduced(avatar_definition, [new_symbols(naming, [spl23_26])])).
fof(f6162, plain, (spl23_169 <=> ! [X11] : (~ occurrence_of(sK21, X11) | min_precedes(sK20, sK18(sK20, sK21), X11))), introduced(avatar_definition, [new_symbols(naming, [spl23_169])])).
fof(f7527, plain, (! [X6] : (~ occurrence_of(sK21, X6) | ~ atomic(X6)) | ~ spl23_169), inference(resolution, [], [f6163, f272])).
fof(f272, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | ~ atomic(X2)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0, X1, X2] : (~ atomic(X2) | ~ min_precedes(X0, X1, X2)), inference(ennf_transformation, [], [f83])).
fof(f83, plain, ! [X0, X1, X2] : (min_precedes(X0, X1, X2) => ~ atomic(X2)), inference(rectify, [], [f20])).
fof(f20, plain, ! [X49, X50, X51] : (min_precedes(X49, X50, X51) => ~ atomic(X51)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_19)).
fof(f6163, plain, (! [X11] : (min_precedes(sK20, sK18(sK20, sK21), X11) | ~ occurrence_of(sK21, X11)) | ~ spl23_169), inference(avatar_component_clause, [], [f6162])).
fof(f6324, plain, (~ spl23_163 | ~ spl23_170), inference(avatar_contradiction_clause, [], [f6323])).
fof(f6323, plain, ($false | (~ spl23_163 | ~ spl23_170)), inference(subsumption_resolution, [], [f6300, f347])).
fof(f6300, plain, (leaf_occ(sK20, sK21) | (~ spl23_163 | ~ spl23_170)), inference(backward_demodulation, [], [f6097, f6167])).
fof(f6167, plain, ((sK20 = sK18(sK20, sK21)) | ~ spl23_170), inference(avatar_component_clause, [], [f6165])).
fof(f6165, plain, (spl23_170 <=> (sK20 = sK18(sK20, sK21))), introduced(avatar_definition, [new_symbols(naming, [spl23_170])])).
fof(f6097, plain, (leaf_occ(sK18(sK20, sK21), sK21) | ~ spl23_163), inference(avatar_component_clause, [], [f6096])).
fof(f6096, plain, (spl23_163 <=> leaf_occ(sK18(sK20, sK21), sK21)), introduced(avatar_definition, [new_symbols(naming, [spl23_163])])).
fof(f6168, plain, (spl23_169 | spl23_170 | ~ spl23_163), inference(avatar_split_clause, [], [f6160, f6096, f6165, f6162])).
fof(f6160, plain, (! [X11] : ((sK20 = sK18(sK20, sK21)) | ~ occurrence_of(sK21, X11) | min_precedes(sK20, sK18(sK20, sK21), X11)) | ~ spl23_163), inference(subsumption_resolution, [], [f6145, f6129])).
fof(f6129, plain, (subactivity_occurrence(sK18(sK20, sK21), sK21) | ~ spl23_163), inference(resolution, [], [f6097, f303])).
fof(f6145, plain, (! [X11] : ((sK20 = sK18(sK20, sK21)) | ~ subactivity_occurrence(sK18(sK20, sK21), sK21) | ~ occurrence_of(sK21, X11) | min_precedes(sK20, sK18(sK20, sK21), X11)) | ~ spl23_163), inference(duplicate_literal_removal, [], [f6142])).
fof(f6142, plain, (! [X11] : ((sK20 = sK18(sK20, sK21)) | ~ subactivity_occurrence(sK18(sK20, sK21), sK21) | ~ occurrence_of(sK21, X11) | min_precedes(sK20, sK18(sK20, sK21), X11) | ~ occurrence_of(sK21, X11)) | ~ spl23_163), inference(resolution, [], [f6097, f1491])).
fof(f1491, plain, ! [X24, X23, X25] : (~ leaf_occ(X23, X25) | (sK20 = X23) | ~ subactivity_occurrence(X23, sK21) | ~ occurrence_of(sK21, X24) | min_precedes(sK20, X23, X24) | ~ occurrence_of(X25, X24)), inference(subsumption_resolution, [], [f1457, f490])).
fof(f490, plain, ! [X0, X1] : (~ leaf_occ(X0, X1) | arboreal(X0)), inference(resolution, [], [f488, f304])).
fof(f304, plain, ! [X0, X1] : (leaf(X0, sK14(X0, X1)) | ~ leaf_occ(X0, X1)), inference(cnf_transformation, [], [f231])).
fof(f488, plain, ! [X23, X22] : (~ leaf(X22, X23) | arboreal(X22)), inference(subsumption_resolution, [], [f485, f379])).
fof(f379, plain, ! [X0, X1] : (~ root(X0, X1) | arboreal(X0)), inference(resolution, [], [f373, f264])).
fof(f264, plain, ! [X0, X1] : (atocc(X1, sK4(X0, X1)) | ~ root(X1, X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0, X1] : ((atocc(X1, sK4(X0, X1)) & subactivity(sK4(X0, X1), X0)) | ~ root(X1, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f131, f201])).
fof(f201, plain, ! [X1, X0] : (? [X2] : (atocc(X1, X2) & subactivity(X2, X0)) => (atocc(X1, sK4(X0, X1)) & subactivity(sK4(X0, X1), X0))), introduced(choice_axiom, [])).
fof(f131, plain, ! [X0, X1] : (? [X2] : (atocc(X1, X2) & subactivity(X2, X0)) | ~ root(X1, X0)), inference(ennf_transformation, [], [f76])).
fof(f76, plain, ! [X0, X1] : (root(X1, X0) => ? [X2] : (atocc(X1, X2) & subactivity(X2, X0))), inference(rectify, [], [f13])).
fof(f13, plain, ! [X28, X29] : (root(X29, X28) => ? [X30] : (atocc(X29, X30) & subactivity(X30, X28))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_12)).
fof(f373, plain, ! [X0, X1] : (~ atocc(X0, X1) | arboreal(X0)), inference(subsumption_resolution, [], [f369, f283])).
fof(f283, plain, ! [X0, X1] : (atomic(sK9(X0, X1)) | ~ atocc(X0, X1)), inference(cnf_transformation, [], [f219])).
fof(f219, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & ((occurrence_of(X0, sK9(X0, X1)) & atomic(sK9(X0, X1)) & subactivity(X1, sK9(X0, X1))) | ~ atocc(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f217, f218])).
fof(f218, plain, ! [X1, X0] : (? [X3] : (occurrence_of(X0, X3) & atomic(X3) & subactivity(X1, X3)) => (occurrence_of(X0, sK9(X0, X1)) & atomic(sK9(X0, X1)) & subactivity(X1, sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f217, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & (? [X3] : (occurrence_of(X0, X3) & atomic(X3) & subactivity(X1, X3)) | ~ atocc(X0, X1))), inference(rectify, [], [f216])).
fof(f216, plain, ! [X0, X1] : ((atocc(X0, X1) | ! [X2] : (~ occurrence_of(X0, X2) | ~ atomic(X2) | ~ subactivity(X1, X2))) & (? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2)) | ~ atocc(X0, X1))), inference(nnf_transformation, [], [f87])).
fof(f87, plain, ! [X0, X1] : (atocc(X0, X1) <=> ? [X2] : (occurrence_of(X0, X2) & atomic(X2) & subactivity(X1, X2))), inference(rectify, [], [f24])).
fof(f24, plain, ! [X64, X65] : (atocc(X64, X65) <=> ? [X66] : (occurrence_of(X64, X66) & atomic(X66) & subactivity(X65, X66))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_23)).
fof(f369, plain, ! [X0, X1] : (~ atocc(X0, X1) | ~ atomic(sK9(X0, X1)) | arboreal(X0)), inference(resolution, [], [f284, f253])).
fof(f253, plain, ! [X0, X1] : (~ occurrence_of(X0, X1) | ~ atomic(X1) | arboreal(X0)), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ! [X0, X1] : (((arboreal(X0) | ~ atomic(X1)) & (atomic(X1) | ~ arboreal(X0))) | ~ occurrence_of(X0, X1)), inference(nnf_transformation, [], [f126])).
fof(f126, plain, ! [X0, X1] : ((arboreal(X0) <=> atomic(X1)) | ~ occurrence_of(X0, X1)), inference(ennf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1] : (occurrence_of(X0, X1) => (arboreal(X0) <=> atomic(X1))), inference(rectify, [], [f8])).
fof(f8, plain, ! [X16, X17] : (occurrence_of(X16, X17) => (arboreal(X16) <=> atomic(X17))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_07)).
fof(f284, plain, ! [X0, X1] : (occurrence_of(X0, sK9(X0, X1)) | ~ atocc(X0, X1)), inference(cnf_transformation, [], [f219])).
fof(f485, plain, ! [X23, X22] : (root(X22, X23) | ~ leaf(X22, X23) | arboreal(X22)), inference(resolution, [], [f274, f398])).
fof(f398, plain, ! [X2, X0, X1] : (~ min_precedes(X0, X1, X2) | arboreal(X1)), inference(resolution, [], [f262, f373])).
fof(f262, plain, ! [X2, X0, X1] : (atocc(X2, sK3(X0, X1, X2)) | ~ min_precedes(X1, X2, X0)), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ! [X0, X1, X2] : ((atocc(X2, sK3(X0, X1, X2)) & atocc(X1, sK2(X0, X1, X2)) & subactivity(sK3(X0, X1, X2), X0) & subactivity(sK2(X0, X1, X2), X0)) | ~ min_precedes(X1, X2, X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2, sK3])], [f130, f199])).
fof(f199, plain, ! [X2, X1, X0] : (? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0)) => (atocc(X2, sK3(X0, X1, X2)) & atocc(X1, sK2(X0, X1, X2)) & subactivity(sK3(X0, X1, X2), X0) & subactivity(sK2(X0, X1, X2), X0))), introduced(choice_axiom, [])).
fof(f130, plain, ! [X0, X1, X2] : (? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0)) | ~ min_precedes(X1, X2, X0)), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0, X1, X2] : (min_precedes(X1, X2, X0) => ? [X3, X4] : (atocc(X2, X4) & atocc(X1, X3) & subactivity(X4, X0) & subactivity(X3, X0))), inference(rectify, [], [f12])).
fof(f12, plain, ! [X23, X24, X25] : (min_precedes(X24, X25, X23) => ? [X26, X27] : (atocc(X25, X27) & atocc(X24, X26) & subactivity(X27, X23) & subactivity(X26, X23))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_11)).
fof(f274, plain, ! [X0, X1] : (min_precedes(sK7(X0, X1), X0, X1) | root(X0, X1) | ~ leaf(X0, X1)), inference(cnf_transformation, [], [f210])).
fof(f210, plain, ! [X0, X1] : ((leaf(X0, X1) | min_precedes(X0, sK6(X0, X1), X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X4] : ~ min_precedes(X0, X4, X1) & (min_precedes(sK7(X0, X1), X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7])], [f207, f209, f208])).
fof(f208, plain, ! [X1, X0] : (? [X2] : min_precedes(X0, X2, X1) => min_precedes(X0, sK6(X0, X1), X1)), introduced(choice_axiom, [])).
fof(f209, plain, ! [X1, X0] : (? [X5] : min_precedes(X5, X0, X1) => min_precedes(sK7(X0, X1), X0, X1)), introduced(choice_axiom, [])).
fof(f207, plain, ! [X0, X1] : ((leaf(X0, X1) | ? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X4] : ~ min_precedes(X0, X4, X1) & (? [X5] : min_precedes(X5, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(rectify, [], [f206])).
fof(f206, plain, ! [X0, X1] : ((leaf(X0, X1) | ? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1))) & ((! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(flattening, [], [f205])).
fof(f205, plain, ! [X0, X1] : ((leaf(X0, X1) | (? [X2] : min_precedes(X0, X2, X1) | (! [X3] : ~ min_precedes(X3, X0, X1) & ~ root(X0, X1)))) & ((! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1))) | ~ leaf(X0, X1))), inference(nnf_transformation, [], [f143])).
fof(f143, plain, ! [X0, X1] : (leaf(X0, X1) <=> (! [X2] : ~ min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1)))), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0, X1] : (leaf(X0, X1) <=> (~ ? [X2] : min_precedes(X0, X2, X1) & (? [X3] : min_precedes(X3, X0, X1) | root(X0, X1)))), inference(rectify, [], [f22])).
fof(f22, plain, ! [X56, X57] : (leaf(X56, X57) <=> (~ ? [X59] : min_precedes(X56, X59, X57) & (? [X58] : min_precedes(X58, X56, X57) | root(X56, X57)))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_21)).
fof(f1457, plain, ! [X24, X23, X25] : (min_precedes(sK20, X23, X24) | (sK20 = X23) | ~ subactivity_occurrence(X23, sK21) | ~ arboreal(X23) | ~ occurrence_of(sK21, X24) | ~ leaf_occ(X23, X25) | ~ occurrence_of(X25, X24)), inference(resolution, [], [f740, f308])).
fof(f740, plain, ! [X0, X1] : (min_precedes(sK20, X0, X1) | min_precedes(X0, sK20, X1) | (sK20 = X0) | ~ subactivity_occurrence(X0, sK21) | ~ arboreal(X0) | ~ occurrence_of(sK21, X1)), inference(subsumption_resolution, [], [f733, f346])).
fof(f733, plain, ! [X0, X1] : (min_precedes(sK20, X0, X1) | min_precedes(X0, sK20, X1) | (sK20 = X0) | ~ subactivity_occurrence(X0, sK21) | ~ arboreal(sK20) | ~ arboreal(X0) | ~ occurrence_of(sK21, X1)), inference(resolution, [], [f295, f345])).
fof(f295, plain, ! [X2, X0, X3, X1] : (~ subactivity_occurrence(X3, X1) | min_precedes(X3, X2, X0) | min_precedes(X2, X3, X0) | (X2 = X3) | ~ subactivity_occurrence(X2, X1) | ~ arboreal(X3) | ~ arboreal(X2) | ~ occurrence_of(X1, X0)), inference(cnf_transformation, [], [f152])).
fof(f152, plain, ! [X0, X1, X2, X3] : ((X2 = X3) | min_precedes(X3, X2, X0) | min_precedes(X2, X3, X0) | ~ subactivity_occurrence(X3, X1) | ~ subactivity_occurrence(X2, X1) | ~ arboreal(X3) | ~ arboreal(X2) | ~ occurrence_of(X1, X0)), inference(flattening, [], [f151])).
fof(f151, plain, ! [X0, X1, X2, X3] : (((X2 = X3) | min_precedes(X3, X2, X0) | min_precedes(X2, X3, X0)) | (~ subactivity_occurrence(X3, X1) | ~ subactivity_occurrence(X2, X1) | ~ arboreal(X3) | ~ arboreal(X2) | ~ occurrence_of(X1, X0))), inference(ennf_transformation, [], [f92])).
fof(f92, plain, ! [X0, X1, X2, X3] : ((subactivity_occurrence(X3, X1) & subactivity_occurrence(X2, X1) & arboreal(X3) & arboreal(X2) & occurrence_of(X1, X0)) => ((X2 = X3) | min_precedes(X3, X2, X0) | min_precedes(X2, X3, X0))), inference(rectify, [], [f29])).
fof(f29, plain, ! [X79, X80, X81, X82] : ((subactivity_occurrence(X82, X80) & subactivity_occurrence(X81, X80) & arboreal(X82) & arboreal(X81) & occurrence_of(X80, X79)) => ((X81 = X82) | min_precedes(X82, X81, X79) | min_precedes(X81, X82, X79))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_28)).
fof(f6150, plain, (spl23_20 | spl23_167 | ~ spl23_26 | ~ spl23_163), inference(avatar_split_clause, [], [f6146, f6096, f1475, f6148, f1109])).
fof(f1109, plain, (spl23_20 <=> ! [X4] : ~ occurrence_of(sK21, X4)), introduced(avatar_definition, [new_symbols(naming, [spl23_20])])).
fof(f6146, plain, (! [X0, X1] : ((sK18(sK20, sK21) = X0) | ~ leaf_occ(X0, sK21) | ~ occurrence_of(sK21, X1)) | (~ spl23_26 | ~ spl23_163)), inference(subsumption_resolution, [], [f6130, f1476])).
fof(f1476, plain, (! [X12] : (~ occurrence_of(sK21, X12) | ~ atomic(X12)) | ~ spl23_26), inference(avatar_component_clause, [], [f1475])).
fof(f6130, plain, (! [X0, X1] : ((sK18(sK20, sK21) = X0) | ~ leaf_occ(X0, sK21) | atomic(X1) | ~ occurrence_of(sK21, X1)) | ~ spl23_163), inference(resolution, [], [f6097, f307])).
fof(f307, plain, ! [X2, X0, X3, X1] : (~ leaf_occ(X1, X2) | (X0 = X1) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3)), inference(cnf_transformation, [], [f165])).
fof(f165, plain, ! [X0, X1, X2, X3] : ((X0 = X1) | ~ leaf_occ(X1, X2) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3)), inference(flattening, [], [f164])).
fof(f164, plain, ! [X0, X1, X2, X3] : ((X0 = X1) | (~ leaf_occ(X1, X2) | ~ leaf_occ(X0, X2) | atomic(X3) | ~ occurrence_of(X2, X3))), inference(ennf_transformation, [], [f100])).
fof(f100, plain, ! [X0, X1, X2, X3] : ((leaf_occ(X1, X2) & leaf_occ(X0, X2) & ~ atomic(X3) & occurrence_of(X2, X3)) => (X0 = X1)), inference(rectify, [], [f37])).
fof(f37, plain, ! [X109, X110, X111, X112] : ((leaf_occ(X110, X111) & leaf_occ(X109, X111) & ~ atomic(X112) & occurrence_of(X111, X112)) => (X109 = X110)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_36)).
fof(f6120, plain, spl23_163, inference(avatar_contradiction_clause, [], [f6119])).
fof(f6119, plain, ($false | spl23_163), inference(subsumption_resolution, [], [f6118, f344])).
fof(f6118, plain, (~ occurrence_of(sK21, tptp0) | spl23_163), inference(subsumption_resolution, [], [f6117, f345])).
fof(f6117, plain, (~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | spl23_163), inference(subsumption_resolution, [], [f6116, f346])).
fof(f6116, plain, (~ arboreal(sK20) | ~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | spl23_163), inference(subsumption_resolution, [], [f6115, f347])).
fof(f6115, plain, (leaf_occ(sK20, sK21) | ~ arboreal(sK20) | ~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | spl23_163), inference(resolution, [], [f6098, f328])).
fof(f6098, plain, (~ leaf_occ(sK18(sK20, sK21), sK21) | spl23_163), inference(avatar_component_clause, [], [f6096])).
fof(f4001, plain, spl23_76, inference(avatar_contradiction_clause, [], [f4000])).
fof(f4000, plain, ($false | spl23_76), inference(subsumption_resolution, [], [f3999, f347])).
fof(f3999, plain, (leaf_occ(sK20, sK21) | spl23_76), inference(subsumption_resolution, [], [f3998, f344])).
fof(f3998, plain, (~ occurrence_of(sK21, tptp0) | leaf_occ(sK20, sK21) | spl23_76), inference(subsumption_resolution, [], [f3997, f345])).
fof(f3997, plain, (~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | leaf_occ(sK20, sK21) | spl23_76), inference(subsumption_resolution, [], [f3996, f346])).
fof(f3996, plain, (~ arboreal(sK20) | ~ subactivity_occurrence(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | leaf_occ(sK20, sK21) | spl23_76), inference(resolution, [], [f3952, f607])).
fof(f607, plain, ! [X2, X3] : (activity_occurrence(sK17(X2, X3)) | ~ arboreal(X2) | ~ subactivity_occurrence(X2, X3) | ~ occurrence_of(X3, tptp0) | leaf_occ(X2, X3)), inference(resolution, [], [f324, f244])).
fof(f244, plain, ! [X0, X1] : (~ occurrence_of(X1, X0) | activity_occurrence(X1)), inference(cnf_transformation, [], [f116])).
fof(f116, plain, ! [X0, X1] : ((activity_occurrence(X1) & activity(X0)) | ~ occurrence_of(X1, X0)), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0, X1] : (occurrence_of(X1, X0) => (activity_occurrence(X1) & activity(X0))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos)).
fof(f324, plain, ! [X0, X1] : (occurrence_of(sK17(X0, X1), tptp4) | leaf_occ(X0, X1) | ~ arboreal(X0) | ~ subactivity_occurrence(X0, X1) | ~ occurrence_of(X1, tptp0)), inference(cnf_transformation, [], [f235])).
fof(f3952, plain, (~ activity_occurrence(sK17(sK20, sK21)) | spl23_76), inference(avatar_component_clause, [], [f3950])).
fof(f3950, plain, (spl23_76 <=> activity_occurrence(sK17(sK20, sK21))), introduced(avatar_definition, [new_symbols(naming, [spl23_76])])).
fof(f3963, plain, (~ spl23_76 | spl23_78), inference(avatar_split_clause, [], [f3958, f3960, f3950])).
fof(f3958, plain, (arboreal(sK17(sK20, sK21)) | ~ activity_occurrence(sK17(sK20, sK21))), inference(subsumption_resolution, [], [f3943, f331])).
fof(f331, plain, atomic(tptp4), inference(cnf_transformation, [], [f53])).
fof(f53, plain, atomic(tptp4), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_52)).
fof(f3943, plain, (~ atomic(tptp4) | arboreal(sK17(sK20, sK21)) | ~ activity_occurrence(sK17(sK20, sK21))), inference(superposition, [], [f364, f3805])).
fof(f3805, plain, (tptp4 = sK1(sK17(sK20, sK21))), inference(subsumption_resolution, [], [f3804, f344])).
fof(f3804, plain, (~ occurrence_of(sK21, tptp0) | (tptp4 = sK1(sK17(sK20, sK21)))), inference(subsumption_resolution, [], [f3803, f347])).
fof(f3803, plain, (leaf_occ(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | (tptp4 = sK1(sK17(sK20, sK21)))), inference(subsumption_resolution, [], [f3785, f346])).
fof(f3785, plain, (~ arboreal(sK20) | leaf_occ(sK20, sK21) | ~ occurrence_of(sK21, tptp0) | (tptp4 = sK1(sK17(sK20, sK21)))), inference(resolution, [], [f611, f345])).
fof(f611, plain, ! [X12, X11] : (~ subactivity_occurrence(X11, X12) | ~ arboreal(X11) | leaf_occ(X11, X12) | ~ occurrence_of(X12, tptp0) | (tptp4 = sK1(sK17(X11, X12)))), inference(resolution, [], [f324, f384])).
fof(f384, plain, ! [X2, X1] : (~ occurrence_of(X2, X1) | (sK1(X2) = X1)), inference(subsumption_resolution, [], [f381, f244])).
fof(f381, plain, ! [X2, X1] : ((sK1(X2) = X1) | ~ occurrence_of(X2, X1) | ~ activity_occurrence(X2)), inference(resolution, [], [f247, f246])).
fof(f246, plain, ! [X0] : (occurrence_of(X0, sK1(X0)) | ~ activity_occurrence(X0)), inference(cnf_transformation, [], [f195])).
fof(f195, plain, ! [X0] : ((occurrence_of(X0, sK1(X0)) & activity(sK1(X0))) | ~ activity_occurrence(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1])], [f117, f194])).
fof(f194, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) => (occurrence_of(X0, sK1(X0)) & activity(sK1(X0)))), introduced(choice_axiom, [])).
fof(f117, plain, ! [X0] : (? [X1] : (occurrence_of(X0, X1) & activity(X1)) | ~ activity_occurrence(X0)), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (activity_occurrence(X0) => ? [X1] : (occurrence_of(X0, X1) & activity(X1))), inference(rectify, [], [f2])).
fof(f2, plain, ! [X2] : (activity_occurrence(X2) => ? [X3] : (occurrence_of(X2, X3) & activity(X3))), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_01)).
fof(f247, plain, ! [X2, X0, X1] : (~ occurrence_of(X0, X2) | (X1 = X2) | ~ occurrence_of(X0, X1)), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ! [X0, X1, X2] : ((X1 = X2) | ~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1)), inference(flattening, [], [f118])).
fof(f118, plain, ! [X0, X1, X2] : ((X1 = X2) | (~ occurrence_of(X0, X2) | ~ occurrence_of(X0, X1))), inference(ennf_transformation, [], [f66])).
fof(f66, plain, ! [X0, X1, X2] : ((occurrence_of(X0, X2) & occurrence_of(X0, X1)) => (X1 = X2)), inference(rectify, [], [f3])).
fof(f3, plain, ! [X4, X5, X6] : ((occurrence_of(X4, X6) & occurrence_of(X4, X5)) => (X5 = X6)), file('/home/ubuntu/library/tptp/Problems/PRO/PRO016+3.p', sos_02)).
fof(f364, plain, ! [X0] : (~ atomic(sK1(X0)) | arboreal(X0) | ~ activity_occurrence(X0)), inference(resolution, [], [f253, f246])).
fof(f2559, plain, ~ spl23_20, inference(avatar_contradiction_clause, [], [f2558])).
fof(f2558, plain, ($false | ~ spl23_20), inference(subsumption_resolution, [], [f2554, f355])).
fof(f355, plain, activity_occurrence(sK21), inference(resolution, [], [f244, f344])).
fof(f2554, plain, (~ activity_occurrence(sK21) | ~ spl23_20), inference(resolution, [], [f1110, f246])).
fof(f1110, plain, (! [X4] : ~ occurrence_of(sK21, X4) | ~ spl23_20), inference(avatar_component_clause, [], [f1109])).