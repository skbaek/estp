fof(f736, plain, $false, inference(avatar_sat_refutation, [], [f708, f709, f710, f711, f716, f717, f718, f719, f724, f725, f726, f727, f728, f729, f730, f731, f732, f733, f734, f735])).
fof(f735, plain, (~ spl3_61 | ~ spl3_41 | ~ spl3_21 | ~ spl3_1), inference(avatar_split_clause, [], [f342, f344, f429, f514, f599])).
fof(f599, plain, (spl3_61 <=> (unit = op(unit, unit))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f514, plain, (spl3_41 <=> (op(e1, e1) = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f429, plain, (spl3_21 <=> (op(e2, e2) = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f344, plain, (spl3_1 <=> (op(e3, e3) = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f342, plain, (~ (op(e3, e3) = unit) | ~ (op(e2, e2) = unit) | ~ (op(e1, e1) = unit) | ~ (unit = op(unit, unit))), inference(definition_unfolding, [], [f218, f124, f124, f124, f124, f124, f124])).
fof(f124, plain, (e0 = unit), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (e0 = unit), file('/home/ubuntu/library/tptp/Problems/ALG/ALG014+1.p', ax5)).
fof(f218, plain, (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0))), inference(cnf_transformation, [], [f19])).
fof(f19, plain, ((((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | sP2 | sP1 | sP0) & (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0)))), inference(definition_folding, [], [f15, e18, e17, e16])).
fof(f16, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e16])).
fof(e16, plain, (sP0 <=> ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f17, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e17])).
fof(e17, plain, (sP1 <=> ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f18, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e18])).
fof(e18, plain, (sP2 <=> ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f15, plain, ((((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) & (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0)))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ~ (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) | ((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f12])).
fof(f12, plain, ~ (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) | ((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG014+1.p', co1)).
fof(f734, plain, (~ spl3_62 | ~ spl3_42 | ~ spl3_22 | ~ spl3_2), inference(avatar_split_clause, [], [f341, f348, f433, f518, f603])).
fof(f603, plain, (spl3_62 <=> (e1 = op(unit, unit))), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f518, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f433, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f348, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f341, plain, (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (e1 = op(unit, unit))), inference(definition_unfolding, [], [f219, f124, f124])).
fof(f219, plain, (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f19])).
fof(f733, plain, (~ spl3_63 | ~ spl3_43 | ~ spl3_23 | ~ spl3_3), inference(avatar_split_clause, [], [f340, f352, f437, f522, f607])).
fof(f607, plain, (spl3_63 <=> (e2 = op(unit, unit))), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f522, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f437, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f352, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f340, plain, (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (e2 = op(unit, unit))), inference(definition_unfolding, [], [f220, f124, f124])).
fof(f220, plain, (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f19])).
fof(f732, plain, (~ spl3_64 | ~ spl3_44 | ~ spl3_24 | ~ spl3_4), inference(avatar_split_clause, [], [f339, f356, f441, f526, f611])).
fof(f611, plain, (spl3_64 <=> (e3 = op(unit, unit))), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f526, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f441, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f356, plain, (spl3_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f339, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (e3 = op(unit, unit))), inference(definition_unfolding, [], [f221, f124, f124])).
fof(f221, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f19])).
fof(f731, plain, (spl3_83 | spl3_82 | spl3_81 | spl3_64), inference(avatar_split_clause, [], [f338, f611, f705, f713, f721])).
fof(f721, plain, (spl3_83 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_83])])).
fof(f713, plain, (spl3_82 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_82])])).
fof(f705, plain, (spl3_81 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_81])])).
fof(f338, plain, ((e3 = op(unit, unit)) | sP2 | sP1 | sP0), inference(definition_unfolding, [], [f222, f124, f124])).
fof(f222, plain, ((op(e0, e0) = e3) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f19])).
fof(f730, plain, (spl3_83 | spl3_82 | spl3_81 | spl3_44), inference(avatar_split_clause, [], [f223, f526, f705, f713, f721])).
fof(f223, plain, ((e3 = op(e1, e1)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f19])).
fof(f729, plain, (spl3_83 | spl3_82 | spl3_81 | spl3_24), inference(avatar_split_clause, [], [f224, f441, f705, f713, f721])).
fof(f224, plain, ((e3 = op(e2, e2)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f19])).
fof(f728, plain, (spl3_83 | spl3_82 | spl3_81 | spl3_4), inference(avatar_split_clause, [], [f225, f356, f705, f713, f721])).
fof(f225, plain, ((e3 = op(e3, e3)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f19])).
fof(f727, plain, (~ spl3_83 | spl3_61), inference(avatar_split_clause, [], [f337, f599, f721])).
fof(f337, plain, ((unit = op(unit, unit)) | ~ sP0), inference(definition_unfolding, [], [f214, f124, f124, f124])).
fof(f214, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f22])).
fof(f22, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f16])).
fof(f726, plain, (~ spl3_83 | spl3_41), inference(avatar_split_clause, [], [f336, f514, f721])).
fof(f336, plain, ((op(e1, e1) = unit) | ~ sP0), inference(definition_unfolding, [], [f215, f124])).
fof(f215, plain, ((e0 = op(e1, e1)) | ~ sP0), inference(cnf_transformation, [], [f22])).
fof(f725, plain, (~ spl3_83 | spl3_21), inference(avatar_split_clause, [], [f335, f429, f721])).
fof(f335, plain, ((op(e2, e2) = unit) | ~ sP0), inference(definition_unfolding, [], [f216, f124])).
fof(f216, plain, ((e0 = op(e2, e2)) | ~ sP0), inference(cnf_transformation, [], [f22])).
fof(f724, plain, (~ spl3_83 | spl3_1), inference(avatar_split_clause, [], [f334, f344, f721])).
fof(f334, plain, ((op(e3, e3) = unit) | ~ sP0), inference(definition_unfolding, [], [f217, f124])).
fof(f217, plain, ((e0 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f22])).
fof(f719, plain, (~ spl3_82 | spl3_62), inference(avatar_split_clause, [], [f333, f603, f713])).
fof(f333, plain, ((e1 = op(unit, unit)) | ~ sP1), inference(definition_unfolding, [], [f210, f124, f124])).
fof(f210, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f21])).
fof(f21, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f17])).
fof(f718, plain, (~ spl3_82 | spl3_42), inference(avatar_split_clause, [], [f211, f518, f713])).
fof(f211, plain, ((e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f21])).
fof(f717, plain, (~ spl3_82 | spl3_22), inference(avatar_split_clause, [], [f212, f433, f713])).
fof(f212, plain, ((e1 = op(e2, e2)) | ~ sP1), inference(cnf_transformation, [], [f21])).
fof(f716, plain, (~ spl3_82 | spl3_2), inference(avatar_split_clause, [], [f213, f348, f713])).
fof(f213, plain, ((e1 = op(e3, e3)) | ~ sP1), inference(cnf_transformation, [], [f21])).
fof(f711, plain, (~ spl3_81 | spl3_63), inference(avatar_split_clause, [], [f332, f607, f705])).
fof(f332, plain, ((e2 = op(unit, unit)) | ~ sP2), inference(definition_unfolding, [], [f206, f124, f124])).
fof(f206, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f20])).
fof(f20, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f18])).
fof(f710, plain, (~ spl3_81 | spl3_43), inference(avatar_split_clause, [], [f207, f522, f705])).
fof(f207, plain, ((e2 = op(e1, e1)) | ~ sP2), inference(cnf_transformation, [], [f20])).
fof(f709, plain, (~ spl3_81 | spl3_23), inference(avatar_split_clause, [], [f208, f437, f705])).
fof(f208, plain, ((e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f20])).
fof(f708, plain, (~ spl3_81 | spl3_3), inference(avatar_split_clause, [], [f209, f352, f705])).
fof(f209, plain, ((e2 = op(e3, e3)) | ~ sP2), inference(cnf_transformation, [], [f20])).