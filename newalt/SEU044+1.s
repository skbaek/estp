fof(f908, plain, $false, inference(avatar_sat_refutation, [], [f182, f183, f184, f373, f772, f904])).
fof(f904, plain, (~ spl18_1 | spl18_3), inference(avatar_contradiction_clause, [], [f903])).
fof(f903, plain, ($false | (~ spl18_1 | spl18_3)), inference(subsumption_resolution, [], [f902, f181])).
fof(f181, plain, (~ in(apply(sK15, sK14), sK13) | spl18_3), inference(avatar_component_clause, [], [f179])).
fof(f179, plain, (spl18_3 <=> in(apply(sK15, sK14), sK13)), introduced(avatar_definition, [new_symbols(naming, [spl18_3])])).
fof(f902, plain, (in(apply(sK15, sK14), sK13) | ~ spl18_1), inference(subsumption_resolution, [], [f901, f152])).
fof(f152, plain, function(sK15), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ in(apply(sK15, sK14), sK13) | ~ in(sK14, relation_dom(sK15)) | ~ in(sK14, relation_dom(relation_rng_restriction(sK13, sK15)))) & ((in(apply(sK15, sK14), sK13) & in(sK14, relation_dom(sK15))) | in(sK14, relation_dom(relation_rng_restriction(sK13, sK15)))) & function(sK15) & relation(sK15)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14, sK15])], [f95, f96])).
fof(f96, plain, (? [X0, X1, X2] : ((~ in(apply(X2, X1), X0) | ~ in(X1, relation_dom(X2)) | ~ in(X1, relation_dom(relation_rng_restriction(X0, X2)))) & ((in(apply(X2, X1), X0) & in(X1, relation_dom(X2))) | in(X1, relation_dom(relation_rng_restriction(X0, X2)))) & function(X2) & relation(X2)) => ((~ in(apply(sK15, sK14), sK13) | ~ in(sK14, relation_dom(sK15)) | ~ in(sK14, relation_dom(relation_rng_restriction(sK13, sK15)))) & ((in(apply(sK15, sK14), sK13) & in(sK14, relation_dom(sK15))) | in(sK14, relation_dom(relation_rng_restriction(sK13, sK15)))) & function(sK15) & relation(sK15))), introduced(choice_axiom, [])).
fof(f95, plain, ? [X0, X1, X2] : ((~ in(apply(X2, X1), X0) | ~ in(X1, relation_dom(X2)) | ~ in(X1, relation_dom(relation_rng_restriction(X0, X2)))) & ((in(apply(X2, X1), X0) & in(X1, relation_dom(X2))) | in(X1, relation_dom(relation_rng_restriction(X0, X2)))) & function(X2) & relation(X2)), inference(flattening, [], [f94])).
fof(f94, plain, ? [X0, X1, X2] : ((((~ in(apply(X2, X1), X0) | ~ in(X1, relation_dom(X2))) | ~ in(X1, relation_dom(relation_rng_restriction(X0, X2)))) & ((in(apply(X2, X1), X0) & in(X1, relation_dom(X2))) | in(X1, relation_dom(relation_rng_restriction(X0, X2))))) & function(X2) & relation(X2)), inference(nnf_transformation, [], [f66])).
fof(f66, plain, ? [X0, X1, X2] : (~ (in(X1, relation_dom(relation_rng_restriction(X0, X2))) <=> (in(apply(X2, X1), X0) & in(X1, relation_dom(X2)))) & function(X2) & relation(X2)), inference(flattening, [], [f65])).
fof(f65, plain, ? [X0, X1, X2] : (~ (in(X1, relation_dom(relation_rng_restriction(X0, X2))) <=> (in(apply(X2, X1), X0) & in(X1, relation_dom(X2)))) & (function(X2) & relation(X2))), inference(ennf_transformation, [], [f34])).
fof(f34, plain, ~ ! [X0, X1, X2] : ((function(X2) & relation(X2)) => (in(X1, relation_dom(relation_rng_restriction(X0, X2))) <=> (in(apply(X2, X1), X0) & in(X1, relation_dom(X2))))), inference(negated_conjecture, [], [f33])).
fof(f33, plain, ~ ! [X0, X1, X2] : ((function(X2) & relation(X2)) => (in(X1, relation_dom(relation_rng_restriction(X0, X2))) <=> (in(apply(X2, X1), X0) & in(X1, relation_dom(X2))))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU044+1.p', t86_funct_1)).
fof(f901, plain, (~ function(sK15) | in(apply(sK15, sK14), sK13) | ~ spl18_1), inference(subsumption_resolution, [], [f883, f151])).
fof(f151, plain, relation(sK15), inference(cnf_transformation, [], [f97])).
fof(f883, plain, (~ relation(sK15) | ~ function(sK15) | in(apply(sK15, sK14), sK13) | ~ spl18_1), inference(resolution, [], [f367, f172])).
fof(f172, plain, (in(sK14, relation_dom(relation_rng_restriction(sK13, sK15))) | ~ spl18_1), inference(avatar_component_clause, [], [f171])).
fof(f171, plain, (spl18_1 <=> in(sK14, relation_dom(relation_rng_restriction(sK13, sK15)))), introduced(avatar_definition, [new_symbols(naming, [spl18_1])])).
fof(f367, plain, ! [X4, X5, X3] : (~ in(X4, relation_dom(relation_rng_restriction(X5, X3))) | ~ relation(X3) | ~ function(X3) | in(apply(X3, X4), X5)), inference(resolution, [], [f265, f159])).
fof(f159, plain, ! [X6, X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ in(X6, relation_dom(X0)) | in(apply(X1, X6), X2)), inference(cnf_transformation, [], [f105])).
fof(f105, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | (~ (apply(X1, sK16(X0, X1)) = apply(X0, sK16(X0, X1))) & in(sK16(X0, X1), relation_dom(X0))) | ((~ in(apply(X1, sK17(X0, X1, X2)), X2) | ~ in(sK17(X0, X1, X2), relation_dom(X1)) | ~ in(sK17(X0, X1, X2), relation_dom(X0))) & ((in(apply(X1, sK17(X0, X1, X2)), X2) & in(sK17(X0, X1, X2), relation_dom(X1))) | in(sK17(X0, X1, X2), relation_dom(X0))))) & ((! [X5] : ((apply(X1, X5) = apply(X0, X5)) | ~ in(X5, relation_dom(X0))) & ! [X6] : ((in(X6, relation_dom(X0)) | ~ in(apply(X1, X6), X2) | ~ in(X6, relation_dom(X1))) & ((in(apply(X1, X6), X2) & in(X6, relation_dom(X1))) | ~ in(X6, relation_dom(X0))))) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK16, sK17])], [f102, f104, f103])).
fof(f103, plain, ! [X1, X0] : (? [X3] : (~ (apply(X1, X3) = apply(X0, X3)) & in(X3, relation_dom(X0))) => (~ (apply(X1, sK16(X0, X1)) = apply(X0, sK16(X0, X1))) & in(sK16(X0, X1), relation_dom(X0)))), introduced(choice_axiom, [])).
fof(f104, plain, ! [X2, X1, X0] : (? [X4] : ((~ in(apply(X1, X4), X2) | ~ in(X4, relation_dom(X1)) | ~ in(X4, relation_dom(X0))) & ((in(apply(X1, X4), X2) & in(X4, relation_dom(X1))) | in(X4, relation_dom(X0)))) => ((~ in(apply(X1, sK17(X0, X1, X2)), X2) | ~ in(sK17(X0, X1, X2), relation_dom(X1)) | ~ in(sK17(X0, X1, X2), relation_dom(X0))) & ((in(apply(X1, sK17(X0, X1, X2)), X2) & in(sK17(X0, X1, X2), relation_dom(X1))) | in(sK17(X0, X1, X2), relation_dom(X0))))), introduced(choice_axiom, [])).
fof(f102, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : (~ (apply(X1, X3) = apply(X0, X3)) & in(X3, relation_dom(X0))) | ? [X4] : ((~ in(apply(X1, X4), X2) | ~ in(X4, relation_dom(X1)) | ~ in(X4, relation_dom(X0))) & ((in(apply(X1, X4), X2) & in(X4, relation_dom(X1))) | in(X4, relation_dom(X0))))) & ((! [X5] : ((apply(X1, X5) = apply(X0, X5)) | ~ in(X5, relation_dom(X0))) & ! [X6] : ((in(X6, relation_dom(X0)) | ~ in(apply(X1, X6), X2) | ~ in(X6, relation_dom(X1))) & ((in(apply(X1, X6), X2) & in(X6, relation_dom(X1))) | ~ in(X6, relation_dom(X0))))) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f101])).
fof(f101, plain, ! [X1, X2, X0] : ((sP0(X1, X2, X0) | ? [X3] : (~ (apply(X2, X3) = apply(X1, X3)) & in(X3, relation_dom(X1))) | ? [X4] : ((~ in(apply(X2, X4), X0) | ~ in(X4, relation_dom(X2)) | ~ in(X4, relation_dom(X1))) & ((in(apply(X2, X4), X0) & in(X4, relation_dom(X2))) | in(X4, relation_dom(X1))))) & ((! [X3] : ((apply(X2, X3) = apply(X1, X3)) | ~ in(X3, relation_dom(X1))) & ! [X4] : ((in(X4, relation_dom(X1)) | ~ in(apply(X2, X4), X0) | ~ in(X4, relation_dom(X2))) & ((in(apply(X2, X4), X0) & in(X4, relation_dom(X2))) | ~ in(X4, relation_dom(X1))))) | ~ sP0(X1, X2, X0))), inference(flattening, [], [f100])).
fof(f100, plain, ! [X1, X2, X0] : ((sP0(X1, X2, X0) | (? [X3] : (~ (apply(X2, X3) = apply(X1, X3)) & in(X3, relation_dom(X1))) | ? [X4] : (((~ in(apply(X2, X4), X0) | ~ in(X4, relation_dom(X2))) | ~ in(X4, relation_dom(X1))) & ((in(apply(X2, X4), X0) & in(X4, relation_dom(X2))) | in(X4, relation_dom(X1)))))) & ((! [X3] : ((apply(X2, X3) = apply(X1, X3)) | ~ in(X3, relation_dom(X1))) & ! [X4] : ((in(X4, relation_dom(X1)) | (~ in(apply(X2, X4), X0) | ~ in(X4, relation_dom(X2)))) & ((in(apply(X2, X4), X0) & in(X4, relation_dom(X2))) | ~ in(X4, relation_dom(X1))))) | ~ sP0(X1, X2, X0))), inference(nnf_transformation, [], [f69])).
fof(f69, plain, ! [X1, X2, X0] : (sP0(X1, X2, X0) <=> (! [X3] : ((apply(X2, X3) = apply(X1, X3)) | ~ in(X3, relation_dom(X1))) & ! [X4] : (in(X4, relation_dom(X1)) <=> (in(apply(X2, X4), X0) & in(X4, relation_dom(X2)))))), inference(usedef, [], [e69])).
fof(e69, plain, ! [X1, X2, X0] : (sP0(X1, X2, X0) <=> (! [X3] : ((apply(X2, X3) = apply(X1, X3)) | ~ in(X3, relation_dom(X1))) & ! [X4] : (in(X4, relation_dom(X1)) <=> (in(apply(X2, X4), X0) & in(X4, relation_dom(X2)))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f265, plain, ! [X0, X1] : (sP0(relation_rng_restriction(X0, X1), X1, X0) | ~ function(X1) | ~ relation(X1)), inference(subsumption_resolution, [], [f264, f128])).
fof(f128, plain, ! [X0, X1] : (relation(relation_rng_restriction(X0, X1)) | ~ relation(X1)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ! [X0, X1] : (relation(relation_rng_restriction(X0, X1)) | ~ relation(X1)), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ! [X0, X1] : (relation(X1) => relation(relation_rng_restriction(X0, X1))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU044+1.p', dt_k8_relat_1)).
fof(f264, plain, ! [X0, X1] : (sP0(relation_rng_restriction(X0, X1), X1, X0) | ~ function(X1) | ~ relation(X1) | ~ relation(relation_rng_restriction(X0, X1))), inference(subsumption_resolution, [], [f263, f130])).
fof(f130, plain, ! [X0, X1] : (function(relation_rng_restriction(X0, X1)) | ~ function(X1) | ~ relation(X1)), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ! [X0, X1] : ((function(relation_rng_restriction(X0, X1)) & relation(relation_rng_restriction(X0, X1))) | ~ function(X1) | ~ relation(X1)), inference(flattening, [], [f60])).
fof(f60, plain, ! [X0, X1] : ((function(relation_rng_restriction(X0, X1)) & relation(relation_rng_restriction(X0, X1))) | (~ function(X1) | ~ relation(X1))), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0, X1] : ((function(X1) & relation(X1)) => (function(relation_rng_restriction(X0, X1)) & relation(relation_rng_restriction(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU044+1.p', fc5_funct_1)).
fof(f263, plain, ! [X0, X1] : (sP0(relation_rng_restriction(X0, X1), X1, X0) | ~ function(X1) | ~ relation(X1) | ~ function(relation_rng_restriction(X0, X1)) | ~ relation(relation_rng_restriction(X0, X1))), inference(resolution, [], [f169, f168])).
fof(f168, plain, ! [X2, X0, X1] : (sP1(X0, X2, X1) | ~ function(X2) | ~ relation(X2) | ~ function(X1) | ~ relation(X1)), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ! [X0, X1] : (! [X2] : (sP1(X0, X2, X1) | ~ function(X2) | ~ relation(X2)) | ~ function(X1) | ~ relation(X1)), inference(definition_folding, [], [f68, e70, e69])).
fof(f70, plain, ! [X0, X2, X1] : (((relation_rng_restriction(X0, X2) = X1) <=> sP0(X1, X2, X0)) | ~ sP1(X0, X2, X1)), inference(usedef, [], [e70])).
fof(e70, plain, ! [X0, X2, X1] : (sP1(X0, X2, X1) <=> ((relation_rng_restriction(X0, X2) = X1) <=> sP0(X1, X2, X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f68, plain, ! [X0, X1] : (! [X2] : (((relation_rng_restriction(X0, X2) = X1) <=> (! [X3] : ((apply(X2, X3) = apply(X1, X3)) | ~ in(X3, relation_dom(X1))) & ! [X4] : (in(X4, relation_dom(X1)) <=> (in(apply(X2, X4), X0) & in(X4, relation_dom(X2)))))) | ~ function(X2) | ~ relation(X2)) | ~ function(X1) | ~ relation(X1)), inference(flattening, [], [f67])).
fof(f67, plain, ! [X0, X1] : (! [X2] : (((relation_rng_restriction(X0, X2) = X1) <=> (! [X3] : ((apply(X2, X3) = apply(X1, X3)) | ~ in(X3, relation_dom(X1))) & ! [X4] : (in(X4, relation_dom(X1)) <=> (in(apply(X2, X4), X0) & in(X4, relation_dom(X2)))))) | (~ function(X2) | ~ relation(X2))) | (~ function(X1) | ~ relation(X1))), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0, X1] : ((function(X1) & relation(X1)) => ! [X2] : ((function(X2) & relation(X2)) => ((relation_rng_restriction(X0, X2) = X1) <=> (! [X3] : (in(X3, relation_dom(X1)) => (apply(X2, X3) = apply(X1, X3))) & ! [X4] : (in(X4, relation_dom(X1)) <=> (in(apply(X2, X4), X0) & in(X4, relation_dom(X2)))))))), inference(rectify, [], [f35])).
fof(f35, plain, ! [X0, X1] : ((function(X1) & relation(X1)) => ! [X2] : ((function(X2) & relation(X2)) => ((relation_rng_restriction(X0, X2) = X1) <=> (! [X3] : (in(X3, relation_dom(X1)) => (apply(X2, X3) = apply(X1, X3))) & ! [X3] : (in(X3, relation_dom(X1)) <=> (in(apply(X2, X3), X0) & in(X3, relation_dom(X2)))))))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU044+1.p', t85_funct_1)).
fof(f169, plain, ! [X0, X1] : (~ sP1(X0, X1, relation_rng_restriction(X0, X1)) | sP0(relation_rng_restriction(X0, X1), X1, X0)), inference(equality_resolution, [], [f156])).
fof(f156, plain, ! [X2, X0, X1] : (sP0(X2, X1, X0) | ~ (relation_rng_restriction(X0, X1) = X2) | ~ sP1(X0, X1, X2)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ! [X0, X1, X2] : ((((relation_rng_restriction(X0, X1) = X2) | ~ sP0(X2, X1, X0)) & (sP0(X2, X1, X0) | ~ (relation_rng_restriction(X0, X1) = X2))) | ~ sP1(X0, X1, X2)), inference(rectify, [], [f98])).
fof(f98, plain, ! [X0, X2, X1] : ((((relation_rng_restriction(X0, X2) = X1) | ~ sP0(X1, X2, X0)) & (sP0(X1, X2, X0) | ~ (relation_rng_restriction(X0, X2) = X1))) | ~ sP1(X0, X2, X1)), inference(nnf_transformation, [], [f70])).
fof(f772, plain, (spl18_2 | ~ spl18_1), inference(avatar_split_clause, [], [f769, f171, f175])).
fof(f175, plain, (spl18_2 <=> in(sK14, relation_dom(sK15))), introduced(avatar_definition, [new_symbols(naming, [spl18_2])])).
fof(f769, plain, (in(sK14, relation_dom(sK15)) | ~ spl18_1), inference(subsumption_resolution, [], [f768, f152])).
fof(f768, plain, (~ function(sK15) | in(sK14, relation_dom(sK15)) | ~ spl18_1), inference(subsumption_resolution, [], [f751, f151])).
fof(f751, plain, (~ relation(sK15) | ~ function(sK15) | in(sK14, relation_dom(sK15)) | ~ spl18_1), inference(resolution, [], [f368, f172])).
fof(f368, plain, ! [X6, X8, X7] : (~ in(X7, relation_dom(relation_rng_restriction(X8, X6))) | ~ relation(X6) | ~ function(X6) | in(X7, relation_dom(X6))), inference(resolution, [], [f265, f158])).
fof(f158, plain, ! [X6, X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ in(X6, relation_dom(X0)) | in(X6, relation_dom(X1))), inference(cnf_transformation, [], [f105])).
fof(f373, plain, (spl18_1 | ~ spl18_2 | ~ spl18_3), inference(avatar_contradiction_clause, [], [f372])).
fof(f372, plain, ($false | (spl18_1 | ~ spl18_2 | ~ spl18_3)), inference(subsumption_resolution, [], [f371, f173])).
fof(f173, plain, (~ in(sK14, relation_dom(relation_rng_restriction(sK13, sK15))) | spl18_1), inference(avatar_component_clause, [], [f171])).
fof(f371, plain, (in(sK14, relation_dom(relation_rng_restriction(sK13, sK15))) | (~ spl18_2 | ~ spl18_3)), inference(subsumption_resolution, [], [f370, f151])).
fof(f370, plain, (~ relation(sK15) | in(sK14, relation_dom(relation_rng_restriction(sK13, sK15))) | (~ spl18_2 | ~ spl18_3)), inference(subsumption_resolution, [], [f369, f152])).
fof(f369, plain, (~ function(sK15) | ~ relation(sK15) | in(sK14, relation_dom(relation_rng_restriction(sK13, sK15))) | (~ spl18_2 | ~ spl18_3)), inference(resolution, [], [f265, f276])).
fof(f276, plain, (! [X0] : (~ sP0(X0, sK15, sK13) | in(sK14, relation_dom(X0))) | (~ spl18_2 | ~ spl18_3)), inference(subsumption_resolution, [], [f274, f176])).
fof(f176, plain, (in(sK14, relation_dom(sK15)) | ~ spl18_2), inference(avatar_component_clause, [], [f175])).
fof(f274, plain, (! [X0] : (in(sK14, relation_dom(X0)) | ~ in(sK14, relation_dom(sK15)) | ~ sP0(X0, sK15, sK13)) | ~ spl18_3), inference(resolution, [], [f160, f180])).
fof(f180, plain, (in(apply(sK15, sK14), sK13) | ~ spl18_3), inference(avatar_component_clause, [], [f179])).
fof(f160, plain, ! [X6, X2, X0, X1] : (~ in(apply(X1, X6), X2) | in(X6, relation_dom(X0)) | ~ in(X6, relation_dom(X1)) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f105])).
fof(f184, plain, (spl18_1 | spl18_2), inference(avatar_split_clause, [], [f153, f175, f171])).
fof(f153, plain, (in(sK14, relation_dom(sK15)) | in(sK14, relation_dom(relation_rng_restriction(sK13, sK15)))), inference(cnf_transformation, [], [f97])).
fof(f183, plain, (spl18_1 | spl18_3), inference(avatar_split_clause, [], [f154, f179, f171])).
fof(f154, plain, (in(apply(sK15, sK14), sK13) | in(sK14, relation_dom(relation_rng_restriction(sK13, sK15)))), inference(cnf_transformation, [], [f97])).
fof(f182, plain, (~ spl18_1 | ~ spl18_2 | ~ spl18_3), inference(avatar_split_clause, [], [f155, f179, f175, f171])).
fof(f155, plain, (~ in(apply(sK15, sK14), sK13) | ~ in(sK14, relation_dom(sK15)) | ~ in(sK14, relation_dom(relation_rng_restriction(sK13, sK15)))), inference(cnf_transformation, [], [f97])).