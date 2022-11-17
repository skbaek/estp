fof(f632, plain, $false, inference(avatar_sat_refutation, [], [f604, f605, f606, f607, f612, f613, f614, f615, f620, f621, f622, f623, f624, f625, f626, f627, f628, f629, f630, f631])).
fof(f631, plain, (spl3_87 | spl3_86 | spl3_85 | spl3_64), inference(avatar_split_clause, [], [f221, f497, f601, f609, f617])).
fof(f617, plain, (spl3_87 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_87])])).
fof(f609, plain, (spl3_86 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl3_86])])).
fof(f601, plain, (spl3_85 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl3_85])])).
fof(f497, plain, (spl3_64 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl3_64])])).
fof(f221, plain, ((op(e0, e0) = e3) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f21])).
fof(f21, plain, ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | sP2 | sP1 | sP0)), inference(definition_folding, [], [f17, e20, e19, e18])).
fof(f18, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e18])).
fof(e18, plain, (sP0 <=> ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f19, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e19])).
fof(e19, plain, (sP1 <=> ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f20, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e20])).
fof(e20, plain, (sP2 <=> ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f17, plain, ((~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), inference(flattening, [], [f16])).
fof(f16, plain, (((~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)) & (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)) & (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)) & (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), inference(flattening, [], [f13])).
fof(f13, plain, ~ ~ (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), inference(negated_conjecture, [], [f12])).
fof(f12, plain, ~ ~ (~ (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0)))) & (((e3 = op(e3, e3)) & (e3 = op(e2, e2)) & (e3 = op(e1, e1)) & (op(e0, e0) = e3)) | ((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG017+1.p', co1)).
fof(f630, plain, (spl3_87 | spl3_86 | spl3_85 | spl3_44), inference(avatar_split_clause, [], [f222, f412, f601, f609, f617])).
fof(f412, plain, (spl3_44 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_44])])).
fof(f222, plain, ((e3 = op(e1, e1)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f21])).
fof(f629, plain, (spl3_87 | spl3_86 | spl3_85 | spl3_24), inference(avatar_split_clause, [], [f223, f327, f601, f609, f617])).
fof(f327, plain, (spl3_24 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_24])])).
fof(f223, plain, ((e3 = op(e2, e2)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f21])).
fof(f628, plain, (spl3_87 | spl3_86 | spl3_85 | spl3_4), inference(avatar_split_clause, [], [f224, f242, f601, f609, f617])).
fof(f242, plain, (spl3_4 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f224, plain, ((e3 = op(e3, e3)) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f21])).
fof(f627, plain, (~ spl3_61 | ~ spl3_41 | ~ spl3_21 | ~ spl3_1), inference(avatar_split_clause, [], [f225, f230, f315, f400, f485])).
fof(f485, plain, (spl3_61 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl3_61])])).
fof(f400, plain, (spl3_41 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_41])])).
fof(f315, plain, (spl3_21 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_21])])).
fof(f230, plain, (spl3_1 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f225, plain, (~ (e0 = op(e3, e3)) | ~ (e0 = op(e2, e2)) | ~ (e0 = op(e1, e1)) | ~ (e0 = op(e0, e0))), inference(cnf_transformation, [], [f21])).
fof(f626, plain, (~ spl3_62 | ~ spl3_42 | ~ spl3_22 | ~ spl3_2), inference(avatar_split_clause, [], [f226, f234, f319, f404, f489])).
fof(f489, plain, (spl3_62 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl3_62])])).
fof(f404, plain, (spl3_42 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f319, plain, (spl3_22 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_22])])).
fof(f234, plain, (spl3_2 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_2])])).
fof(f226, plain, (~ (e1 = op(e3, e3)) | ~ (e1 = op(e2, e2)) | ~ (e1 = op(e1, e1)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f21])).
fof(f625, plain, (~ spl3_63 | ~ spl3_43 | ~ spl3_23 | ~ spl3_3), inference(avatar_split_clause, [], [f227, f238, f323, f408, f493])).
fof(f493, plain, (spl3_63 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl3_63])])).
fof(f408, plain, (spl3_43 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl3_43])])).
fof(f323, plain, (spl3_23 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl3_23])])).
fof(f238, plain, (spl3_3 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl3_3])])).
fof(f227, plain, (~ (e2 = op(e3, e3)) | ~ (e2 = op(e2, e2)) | ~ (e2 = op(e1, e1)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f21])).
fof(f624, plain, (~ spl3_64 | ~ spl3_44 | ~ spl3_24 | ~ spl3_4), inference(avatar_split_clause, [], [f228, f242, f327, f412, f497])).
fof(f228, plain, (~ (e3 = op(e3, e3)) | ~ (e3 = op(e2, e2)) | ~ (e3 = op(e1, e1)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f21])).
fof(f623, plain, (~ spl3_87 | spl3_61), inference(avatar_split_clause, [], [f217, f485, f617])).
fof(f217, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f24])).
fof(f24, plain, (((e0 = op(e3, e3)) & (e0 = op(e2, e2)) & (e0 = op(e1, e1)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f18])).
fof(f622, plain, (~ spl3_87 | spl3_41), inference(avatar_split_clause, [], [f218, f400, f617])).
fof(f218, plain, ((e0 = op(e1, e1)) | ~ sP0), inference(cnf_transformation, [], [f24])).
fof(f621, plain, (~ spl3_87 | spl3_21), inference(avatar_split_clause, [], [f219, f315, f617])).
fof(f219, plain, ((e0 = op(e2, e2)) | ~ sP0), inference(cnf_transformation, [], [f24])).
fof(f620, plain, (~ spl3_87 | spl3_1), inference(avatar_split_clause, [], [f220, f230, f617])).
fof(f220, plain, ((e0 = op(e3, e3)) | ~ sP0), inference(cnf_transformation, [], [f24])).
fof(f615, plain, (~ spl3_86 | spl3_62), inference(avatar_split_clause, [], [f213, f489, f609])).
fof(f213, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (((e1 = op(e3, e3)) & (e1 = op(e2, e2)) & (e1 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f19])).
fof(f614, plain, (~ spl3_86 | spl3_42), inference(avatar_split_clause, [], [f214, f404, f609])).
fof(f214, plain, ((e1 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f23])).
fof(f613, plain, (~ spl3_86 | spl3_22), inference(avatar_split_clause, [], [f215, f319, f609])).
fof(f215, plain, ((e1 = op(e2, e2)) | ~ sP1), inference(cnf_transformation, [], [f23])).
fof(f612, plain, (~ spl3_86 | spl3_2), inference(avatar_split_clause, [], [f216, f234, f609])).
fof(f216, plain, ((e1 = op(e3, e3)) | ~ sP1), inference(cnf_transformation, [], [f23])).
fof(f607, plain, (~ spl3_85 | spl3_63), inference(avatar_split_clause, [], [f209, f493, f601])).
fof(f209, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f22])).
fof(f22, plain, (((e2 = op(e3, e3)) & (e2 = op(e2, e2)) & (e2 = op(e1, e1)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f20])).
fof(f606, plain, (~ spl3_85 | spl3_43), inference(avatar_split_clause, [], [f210, f408, f601])).
fof(f210, plain, ((e2 = op(e1, e1)) | ~ sP2), inference(cnf_transformation, [], [f22])).
fof(f605, plain, (~ spl3_85 | spl3_23), inference(avatar_split_clause, [], [f211, f323, f601])).
fof(f211, plain, ((e2 = op(e2, e2)) | ~ sP2), inference(cnf_transformation, [], [f22])).
fof(f604, plain, (~ spl3_85 | spl3_3), inference(avatar_split_clause, [], [f212, f238, f601])).
fof(f212, plain, ((e2 = op(e3, e3)) | ~ sP2), inference(cnf_transformation, [], [f22])).