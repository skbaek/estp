fof(f507, plain, $false, inference(avatar_sat_refutation, [], [f479, f480, f481, f482, f487, f488, f489, f490, f495, f496, f497, f498, f499, f500, f501, f502, f503, f504, f505, f506])).
fof(f506, plain, (spl3_71 | spl3_70 | spl3_69 | spl3_64), inference(avatar_split_clause, [], [f144, f420, f476, f484, f492])).
fof(f492, plain, (spl3_71 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_71])])).
fof(f484, plain, (spl3_70 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_70])])).
fof(f476, plain, (spl3_69 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_69])])).
fof(f420, plain, (spl3_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f144, plain, ((op(e0, e0) = e3) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | sP2 | sP1 | sP0)), inference(definition_folding, [], [f11, e14, e13, e12])).
fof(f12, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e12])).
fof(e12, plain, (sP0 <=> ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f13, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e13])).
fof(e13, plain, (sP1 <=> ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f14, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e14])).
fof(e14, plain, (sP2 <=> ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f11, plain, ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), inference(flattening, [], [f10])).
fof(f10, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG039+1.p', co1)).
fof(f505, plain, (spl3_71 | spl3_70 | spl3_69 | spl3_44), inference(avatar_split_clause, [], [f145, f335, f476, f484, f492])).
fof(f335, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f145, plain, ((e3 = op(e1, e1)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f15])).
fof(f504, plain, (spl3_71 | spl3_70 | spl3_69 | spl3_24), inference(avatar_split_clause, [], [f146, f250, f476, f484, f492])).
fof(f250, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f146, plain, ((e3 = op(e2, e2)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f15])).
fof(f503, plain, (spl3_71 | spl3_70 | spl3_69 | spl3_4), inference(avatar_split_clause, [], [f147, f165, f476, f484, f492])).
fof(f165, plain, (spl3_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f147, plain, ((e3 = op(e3, e3)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f15])).
fof(f502, plain, (~ spl3_61 | ~ spl3_41 | ~ spl3_21 | ~ spl3_1), inference(avatar_split_clause, [], [f148, f153, f238, f323, f408])).
fof(f408, plain, (spl3_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f323, plain, (spl3_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f238, plain, (spl3_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f153, plain, (spl3_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f148, plain, (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0))), inference(cnf_transformation, [], [f15])).
fof(f501, plain, (~ spl3_62 | ~ spl3_42 | ~ spl3_22 | ~ spl3_2), inference(avatar_split_clause, [], [f149, f157, f242, f327, f412])).
fof(f412, plain, (spl3_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f327, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f242, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f157, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f149, plain, (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f15])).
fof(f500, plain, (~ spl3_63 | ~ spl3_43 | ~ spl3_23 | ~ spl3_3), inference(avatar_split_clause, [], [f150, f161, f246, f331, f416])).
fof(f416, plain, (spl3_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f331, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f246, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f161, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f150, plain, (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f15])).
fof(f499, plain, (~ spl3_64 | ~ spl3_44 | ~ spl3_24 | ~ spl3_4), inference(avatar_split_clause, [], [f151, f165, f250, f335, f420])).
fof(f151, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f15])).
fof(f498, plain, (~ spl3_71 | spl3_61), inference(avatar_split_clause, [], [f140, f408, f492])).
fof(f140, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f18])).
fof(f18, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f12])).
fof(f497, plain, (~ spl3_71 | spl3_41), inference(avatar_split_clause, [], [f141, f323, f492])).
fof(f141, plain, ((e0 = op(e1, e1)) | ~ sP0), inference(cnf_transformation, [], [f18])).
fof(f496, plain, (~ spl3_71 | spl3_21), inference(avatar_split_clause, [], [f142, f238, f492])).
fof(f142, plain, ((e0 = op(e2, e2)) | ~ sP0), inference(cnf_transformation, [], [f18])).
fof(f495, plain, (~ spl3_71 | spl3_1), inference(avatar_split_clause, [], [f143, f153, f492])).
fof(f143, plain, ((e0 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f18])).
fof(f490, plain, (~ spl3_70 | spl3_62), inference(avatar_split_clause, [], [f136, f412, f484])).
fof(f136, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f17])).
fof(f17, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f13])).
fof(f489, plain, (~ spl3_70 | spl3_42), inference(avatar_split_clause, [], [f137, f327, f484])).
fof(f137, plain, ((e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f17])).
fof(f488, plain, (~ spl3_70 | spl3_22), inference(avatar_split_clause, [], [f138, f242, f484])).
fof(f138, plain, ((e1 = op(e2, e2)) | ~ sP1), inference(cnf_transformation, [], [f17])).
fof(f487, plain, (~ spl3_70 | spl3_2), inference(avatar_split_clause, [], [f139, f157, f484])).
fof(f139, plain, ((e1 = op(e3, e3)) | ~ sP1), inference(cnf_transformation, [], [f17])).
fof(f482, plain, (~ spl3_69 | spl3_63), inference(avatar_split_clause, [], [f132, f416, f476])).
fof(f132, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f16])).
fof(f16, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f14])).
fof(f481, plain, (~ spl3_69 | spl3_43), inference(avatar_split_clause, [], [f133, f331, f476])).
fof(f133, plain, ((e2 = op(e1, e1)) | ~ sP2), inference(cnf_transformation, [], [f16])).
fof(f480, plain, (~ spl3_69 | spl3_23), inference(avatar_split_clause, [], [f134, f246, f476])).
fof(f134, plain, ((e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f16])).
fof(f479, plain, (~ spl3_69 | spl3_3), inference(avatar_split_clause, [], [f135, f161, f476])).
fof(f135, plain, ((e2 = op(e3, e3)) | ~ sP2), inference(cnf_transformation, [], [f16])).