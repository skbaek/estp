fof(f588, plain, $false, inference(avatar_sat_refutation, [], [f560, f561, f562, f563, f568, f569, f570, f571, f576, f577, f578, f579, f580, f581, f582, f583, f584, f585, f586, f587])).
fof(f587, plain, (~ spl3_61 | ~ spl3_41 | ~ spl3_21 | ~ spl3_1), inference(avatar_split_clause, [], [f243, f245, f330, f415, f500])).
fof(f500, plain, (spl3_61 <=> (unit = op(unit, unit))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f415, plain, (spl3_41 <=> (op(e1, e1) = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f330, plain, (spl3_21 <=> (op(e2, e2) = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f245, plain, (spl3_1 <=> (op(e3, e3) = unit)), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f243, plain, (~ (op(e3, e3) = unit) | ~ (op(e2, e2) = unit) | ~ (op(e1, e1) = unit) | ~ (unit = op(unit, unit))), inference(definition_unfolding, [], [f141, f74, f74, f74, f74, f74, f74])).
fof(f74, plain, (e0 = unit), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (e0 = unit), file('/home/ubuntu/library/tptp/Problems/ALG/ALG036+1.p', ax4)).
fof(f141, plain, (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | sP2 | sP1 | sP0) & (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0)))), inference(definition_folding, [], [f9, e12, e11, e10])).
fof(f10, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e10])).
fof(e10, plain, (sP0 <=> ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f11, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e11])).
fof(e11, plain, (sP1 <=> ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f12, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e12])).
fof(e12, plain, (sP2 <=> ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f9, plain, ((((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) & (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0)))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ~ (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) | ((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) | ((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG036+1.p', co1)).
fof(f586, plain, (~ spl3_62 | ~ spl3_42 | ~ spl3_22 | ~ spl3_2), inference(avatar_split_clause, [], [f242, f249, f334, f419, f504])).
fof(f504, plain, (spl3_62 <=> (e1 = op(unit, unit))), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f419, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f334, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f249, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f242, plain, (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (e1 = op(unit, unit))), inference(definition_unfolding, [], [f142, f74, f74])).
fof(f142, plain, (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f13])).
fof(f585, plain, (~ spl3_63 | ~ spl3_43 | ~ spl3_23 | ~ spl3_3), inference(avatar_split_clause, [], [f241, f253, f338, f423, f508])).
fof(f508, plain, (spl3_63 <=> (e2 = op(unit, unit))), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f423, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f338, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f253, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f241, plain, (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (e2 = op(unit, unit))), inference(definition_unfolding, [], [f143, f74, f74])).
fof(f143, plain, (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f13])).
fof(f584, plain, (~ spl3_64 | ~ spl3_44 | ~ spl3_24 | ~ spl3_4), inference(avatar_split_clause, [], [f240, f257, f342, f427, f512])).
fof(f512, plain, (spl3_64 <=> (e3 = op(unit, unit))), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f427, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f342, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f257, plain, (spl3_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f240, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (e3 = op(unit, unit))), inference(definition_unfolding, [], [f144, f74, f74])).
fof(f144, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f13])).
fof(f583, plain, (spl3_67 | spl3_66 | spl3_65 | spl3_64), inference(avatar_split_clause, [], [f239, f512, f557, f565, f573])).
fof(f573, plain, (spl3_67 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_67])])).
fof(f565, plain, (spl3_66 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_66])])).
fof(f557, plain, (spl3_65 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_65])])).
fof(f239, plain, ((e3 = op(unit, unit)) | sP2 | sP1 | sP0), inference(definition_unfolding, [], [f145, f74, f74])).
fof(f145, plain, ((op(e0, e0) = e3) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f13])).
fof(f582, plain, (spl3_67 | spl3_66 | spl3_65 | spl3_44), inference(avatar_split_clause, [], [f146, f427, f557, f565, f573])).
fof(f146, plain, ((e3 = op(e1, e1)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f13])).
fof(f581, plain, (spl3_67 | spl3_66 | spl3_65 | spl3_24), inference(avatar_split_clause, [], [f147, f342, f557, f565, f573])).
fof(f147, plain, ((e3 = op(e2, e2)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f13])).
fof(f580, plain, (spl3_67 | spl3_66 | spl3_65 | spl3_4), inference(avatar_split_clause, [], [f148, f257, f557, f565, f573])).
fof(f148, plain, ((e3 = op(e3, e3)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f13])).
fof(f579, plain, (~ spl3_67 | spl3_61), inference(avatar_split_clause, [], [f238, f500, f573])).
fof(f238, plain, ((unit = op(unit, unit)) | ~ sP0), inference(definition_unfolding, [], [f137, f74, f74, f74])).
fof(f137, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f16])).
fof(f16, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f578, plain, (~ spl3_67 | spl3_41), inference(avatar_split_clause, [], [f237, f415, f573])).
fof(f237, plain, ((op(e1, e1) = unit) | ~ sP0), inference(definition_unfolding, [], [f138, f74])).
fof(f138, plain, ((e0 = op(e1, e1)) | ~ sP0), inference(cnf_transformation, [], [f16])).
fof(f577, plain, (~ spl3_67 | spl3_21), inference(avatar_split_clause, [], [f236, f330, f573])).
fof(f236, plain, ((op(e2, e2) = unit) | ~ sP0), inference(definition_unfolding, [], [f139, f74])).
fof(f139, plain, ((e0 = op(e2, e2)) | ~ sP0), inference(cnf_transformation, [], [f16])).
fof(f576, plain, (~ spl3_67 | spl3_1), inference(avatar_split_clause, [], [f235, f245, f573])).
fof(f235, plain, ((op(e3, e3) = unit) | ~ sP0), inference(definition_unfolding, [], [f140, f74])).
fof(f140, plain, ((e0 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f16])).
fof(f571, plain, (~ spl3_66 | spl3_62), inference(avatar_split_clause, [], [f234, f504, f565])).
fof(f234, plain, ((e1 = op(unit, unit)) | ~ sP1), inference(definition_unfolding, [], [f133, f74, f74])).
fof(f133, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f15])).
fof(f15, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f570, plain, (~ spl3_66 | spl3_42), inference(avatar_split_clause, [], [f134, f419, f565])).
fof(f134, plain, ((e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f15])).
fof(f569, plain, (~ spl3_66 | spl3_22), inference(avatar_split_clause, [], [f135, f334, f565])).
fof(f135, plain, ((e1 = op(e2, e2)) | ~ sP1), inference(cnf_transformation, [], [f15])).
fof(f568, plain, (~ spl3_66 | spl3_2), inference(avatar_split_clause, [], [f136, f249, f565])).
fof(f136, plain, ((e1 = op(e3, e3)) | ~ sP1), inference(cnf_transformation, [], [f15])).
fof(f563, plain, (~ spl3_65 | spl3_63), inference(avatar_split_clause, [], [f233, f508, f557])).
fof(f233, plain, ((e2 = op(unit, unit)) | ~ sP2), inference(definition_unfolding, [], [f129, f74, f74])).
fof(f129, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f14])).
fof(f14, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f12])).
fof(f562, plain, (~ spl3_65 | spl3_43), inference(avatar_split_clause, [], [f130, f423, f557])).
fof(f130, plain, ((e2 = op(e1, e1)) | ~ sP2), inference(cnf_transformation, [], [f14])).
fof(f561, plain, (~ spl3_65 | spl3_23), inference(avatar_split_clause, [], [f131, f338, f557])).
fof(f131, plain, ((e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f14])).
fof(f560, plain, (~ spl3_65 | spl3_3), inference(avatar_split_clause, [], [f132, f253, f557])).
fof(f132, plain, ((e2 = op(e3, e3)) | ~ sP2), inference(cnf_transformation, [], [f14])).