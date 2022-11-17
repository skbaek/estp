fof(f673, plain, $false, inference(avatar_sat_refutation, [], [f376, f386, f390, f410, f430, f625, f634, f640, f666, f672])).
fof(f672, plain, (spl15_13 | spl15_39), inference(avatar_contradiction_clause, [], [f671])).
fof(f671, plain, ($false | (spl15_13 | spl15_39)), inference(subsumption_resolution, [], [f668, f628])).
fof(f628, plain, (~ sP0(sK1) | spl15_39), inference(avatar_component_clause, [], [f627])).
fof(f627, plain, (spl15_39 <=> sP0(sK1)), introduced(avatar_definition, [new_symbols(naming, [spl15_39])])).
fof(f668, plain, (sP0(sK1) | spl15_13), inference(resolution, [], [f371, f116])).
fof(f116, plain, ! [X0] : (function(sK7(X0)) | sP0(X0)), inference(equality_resolution, [], [f86])).
fof(f86, plain, ! [X4, X0] : (function(sK7(X0)) | ~ (singleton(sK8(X0)) = X4) | sP0(X0)), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ! [X0] : ((! [X2] : ((singleton(X2) = apply(sK7(X0), X2)) | ~ in(X2, X0)) & (relation_dom(sK7(X0)) = X0) & function(sK7(X0)) & relation(sK7(X0))) | (! [X4] : ~ (singleton(sK8(X0)) = X4) & in(sK8(X0), X0)) | sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f58, f60, f59])).
fof(f59, plain, ! [X0] : (? [X1] : (! [X2] : ((apply(X1, X2) = singleton(X2)) | ~ in(X2, X0)) & (relation_dom(X1) = X0) & function(X1) & relation(X1)) => (! [X2] : ((singleton(X2) = apply(sK7(X0), X2)) | ~ in(X2, X0)) & (relation_dom(sK7(X0)) = X0) & function(sK7(X0)) & relation(sK7(X0)))), introduced(choice_axiom, [])).
fof(f60, plain, ! [X0] : (? [X3] : (! [X4] : ~ (singleton(X3) = X4) & in(X3, X0)) => (! [X4] : ~ (singleton(sK8(X0)) = X4) & in(sK8(X0), X0))), introduced(choice_axiom, [])).
fof(f58, plain, ! [X0] : (? [X1] : (! [X2] : ((apply(X1, X2) = singleton(X2)) | ~ in(X2, X0)) & (relation_dom(X1) = X0) & function(X1) & relation(X1)) | ? [X3] : (! [X4] : ~ (singleton(X3) = X4) & in(X3, X0)) | sP0(X0)), inference(rectify, [], [f48])).
fof(f48, plain, ! [X0] : (? [X6] : (! [X7] : ((apply(X6, X7) = singleton(X7)) | ~ in(X7, X0)) & (relation_dom(X6) = X0) & function(X6) & relation(X6)) | ? [X1] : (! [X2] : ~ (singleton(X1) = X2) & in(X1, X0)) | sP0(X0)), inference(definition_folding, [], [f35, e47])).
fof(f47, plain, ! [X0] : (? [X3, X4, X5] : (~ (X4 = X5) & (singleton(X3) = X5) & (singleton(X3) = X4) & in(X3, X0)) | ~ sP0(X0)), inference(usedef, [], [e47])).
fof(e47, plain, ! [X0] : (sP0(X0) <=> ? [X3, X4, X5] : (~ (X4 = X5) & (singleton(X3) = X5) & (singleton(X3) = X4) & in(X3, X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f35, plain, ! [X0] : (? [X6] : (! [X7] : ((apply(X6, X7) = singleton(X7)) | ~ in(X7, X0)) & (relation_dom(X6) = X0) & function(X6) & relation(X6)) | ? [X1] : (! [X2] : ~ (singleton(X1) = X2) & in(X1, X0)) | ? [X3, X4, X5] : (~ (X4 = X5) & (singleton(X3) = X5) & (singleton(X3) = X4) & in(X3, X0))), inference(flattening, [], [f34])).
fof(f34, plain, ! [X0] : (? [X6] : (! [X7] : ((apply(X6, X7) = singleton(X7)) | ~ in(X7, X0)) & (relation_dom(X6) = X0) & function(X6) & relation(X6)) | (? [X1] : (! [X2] : ~ (singleton(X1) = X2) & in(X1, X0)) | ? [X3, X4, X5] : (~ (X4 = X5) & ((singleton(X3) = X5) & (singleton(X3) = X4) & in(X3, X0))))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : ((! [X1] : ~ (! [X2] : ~ (singleton(X1) = X2) & in(X1, X0)) & ! [X3, X4, X5] : (((singleton(X3) = X5) & (singleton(X3) = X4) & in(X3, X0)) => (X4 = X5))) => ? [X6] : (! [X7] : (in(X7, X0) => (apply(X6, X7) = singleton(X7))) & (relation_dom(X6) = X0) & function(X6) & relation(X6))), inference(rectify, [], [f8])).
fof(f8, plain, ! [X0] : ((! [X1] : ~ (! [X2] : ~ (singleton(X1) = X2) & in(X1, X0)) & ! [X1, X2, X3] : (((singleton(X1) = X3) & (singleton(X1) = X2) & in(X1, X0)) => (X2 = X3))) => ? [X1] : (! [X2] : (in(X2, X0) => (apply(X1, X2) = singleton(X2))) & (relation_dom(X1) = X0) & function(X1) & relation(X1))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU284+1.p', s2_funct_1__e16_22__wellord2__1)).
fof(f371, plain, (~ function(sK7(sK1)) | spl15_13), inference(avatar_component_clause, [], [f369])).
fof(f369, plain, (spl15_13 <=> function(sK7(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl15_13])])).
fof(f666, plain, (~ spl15_13 | ~ spl15_12 | spl15_39 | ~ spl15_40), inference(avatar_split_clause, [], [f665, f631, f627, f365, f369])).
fof(f365, plain, (spl15_12 <=> relation(sK7(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl15_12])])).
fof(f631, plain, (spl15_40 <=> (singleton(sK2(sK7(sK1))) = apply(sK7(sK1), sK2(sK7(sK1))))), introduced(avatar_definition, [new_symbols(naming, [spl15_40])])).
fof(f665, plain, (~ function(sK7(sK1)) | (~ spl15_12 | spl15_39 | ~ spl15_40)), inference(subsumption_resolution, [], [f661, f366])).
fof(f366, plain, (relation(sK7(sK1)) | ~ spl15_12), inference(avatar_component_clause, [], [f365])).
fof(f661, plain, (~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | (spl15_39 | ~ spl15_40)), inference(subsumption_resolution, [], [f660, f647])).
fof(f647, plain, ((sK1 = relation_dom(sK7(sK1))) | spl15_39), inference(resolution, [], [f628, f115])).
fof(f115, plain, ! [X0] : (sP0(X0) | (relation_dom(sK7(X0)) = X0)), inference(equality_resolution, [], [f88])).
fof(f88, plain, ! [X4, X0] : ((relation_dom(sK7(X0)) = X0) | ~ (singleton(sK8(X0)) = X4) | sP0(X0)), inference(cnf_transformation, [], [f61])).
fof(f660, plain, (~ (sK1 = relation_dom(sK7(sK1))) | ~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | ~ spl15_40), inference(trivial_inequality_removal, [], [f659])).
fof(f659, plain, (~ (singleton(sK2(sK7(sK1))) = singleton(sK2(sK7(sK1)))) | ~ (sK1 = relation_dom(sK7(sK1))) | ~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | ~ spl15_40), inference(superposition, [], [f75, f633])).
fof(f633, plain, ((singleton(sK2(sK7(sK1))) = apply(sK7(sK1), sK2(sK7(sK1)))) | ~ spl15_40), inference(avatar_component_clause, [], [f631])).
fof(f75, plain, ! [X1] : (~ (apply(X1, sK2(X1)) = singleton(sK2(X1))) | ~ (relation_dom(X1) = sK1) | ~ function(X1) | ~ relation(X1)), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ! [X1] : ((~ (apply(X1, sK2(X1)) = singleton(sK2(X1))) & in(sK2(X1), sK1)) | ~ (relation_dom(X1) = sK1) | ~ function(X1) | ~ relation(X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2])], [f32, f50, f49])).
fof(f49, plain, (? [X0] : ! [X1] : (? [X2] : (~ (apply(X1, X2) = singleton(X2)) & in(X2, X0)) | ~ (relation_dom(X1) = X0) | ~ function(X1) | ~ relation(X1)) => ! [X1] : (? [X2] : (~ (apply(X1, X2) = singleton(X2)) & in(X2, sK1)) | ~ (relation_dom(X1) = sK1) | ~ function(X1) | ~ relation(X1))), introduced(choice_axiom, [])).
fof(f50, plain, ! [X1] : (? [X2] : (~ (apply(X1, X2) = singleton(X2)) & in(X2, sK1)) => (~ (apply(X1, sK2(X1)) = singleton(sK2(X1))) & in(sK2(X1), sK1))), introduced(choice_axiom, [])).
fof(f32, plain, ? [X0] : ! [X1] : (? [X2] : (~ (apply(X1, X2) = singleton(X2)) & in(X2, X0)) | ~ (relation_dom(X1) = X0) | ~ function(X1) | ~ relation(X1)), inference(ennf_transformation, [], [f2])).
fof(f2, plain, ~ ! [X0] : ? [X1] : (! [X2] : (in(X2, X0) => (apply(X1, X2) = singleton(X2))) & (relation_dom(X1) = X0) & function(X1) & relation(X1)), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ? [X1] : (! [X2] : (in(X2, X0) => (apply(X1, X2) = singleton(X2))) & (relation_dom(X1) = X0) & function(X1) & relation(X1)), file('/home/ubuntu/library/tptp/Problems/SEU/SEU284+1.p', s3_funct_1__e16_22__wellord2)).
fof(f640, plain, (spl15_11 | ~ spl15_39), inference(avatar_contradiction_clause, [], [f639])).
fof(f639, plain, ($false | (spl15_11 | ~ spl15_39)), inference(subsumption_resolution, [], [f638, f362])).
fof(f362, plain, (~ (sK5(sK1) = sK6(sK1)) | spl15_11), inference(avatar_component_clause, [], [f361])).
fof(f361, plain, (spl15_11 <=> (sK5(sK1) = sK6(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl15_11])])).
fof(f638, plain, ((sK5(sK1) = sK6(sK1)) | ~ spl15_39), inference(backward_demodulation, [], [f635, f636])).
fof(f636, plain, ((sK5(sK1) = singleton(sK4(sK1))) | ~ spl15_39), inference(resolution, [], [f629, f80])).
fof(f80, plain, ! [X0] : (~ sP0(X0) | (sK5(X0) = singleton(sK4(X0)))), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ! [X0] : ((~ (sK5(X0) = sK6(X0)) & (sK6(X0) = singleton(sK4(X0))) & (sK5(X0) = singleton(sK4(X0))) & in(sK4(X0), X0)) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5, sK6])], [f55, f56])).
fof(f56, plain, ! [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & (singleton(X1) = X3) & (singleton(X1) = X2) & in(X1, X0)) => (~ (sK5(X0) = sK6(X0)) & (sK6(X0) = singleton(sK4(X0))) & (sK5(X0) = singleton(sK4(X0))) & in(sK4(X0), X0))), introduced(choice_axiom, [])).
fof(f55, plain, ! [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & (singleton(X1) = X3) & (singleton(X1) = X2) & in(X1, X0)) | ~ sP0(X0)), inference(rectify, [], [f54])).
fof(f54, plain, ! [X0] : (? [X3, X4, X5] : (~ (X4 = X5) & (singleton(X3) = X5) & (singleton(X3) = X4) & in(X3, X0)) | ~ sP0(X0)), inference(nnf_transformation, [], [f47])).
fof(f629, plain, (sP0(sK1) | ~ spl15_39), inference(avatar_component_clause, [], [f627])).
fof(f635, plain, ((sK6(sK1) = singleton(sK4(sK1))) | ~ spl15_39), inference(resolution, [], [f629, f81])).
fof(f81, plain, ! [X0] : (~ sP0(X0) | (sK6(X0) = singleton(sK4(X0)))), inference(cnf_transformation, [], [f57])).
fof(f634, plain, (spl15_39 | spl15_40 | ~ spl15_14), inference(avatar_split_clause, [], [f451, f373, f631, f627])).
fof(f373, plain, (spl15_14 <=> in(sK2(sK7(sK1)), sK1)), introduced(avatar_definition, [new_symbols(naming, [spl15_14])])).
fof(f451, plain, ((singleton(sK2(sK7(sK1))) = apply(sK7(sK1), sK2(sK7(sK1)))) | sP0(sK1) | ~ spl15_14), inference(resolution, [], [f375, f114])).
fof(f114, plain, ! [X2, X0] : (~ in(X2, X0) | (singleton(X2) = apply(sK7(X0), X2)) | sP0(X0)), inference(equality_resolution, [], [f90])).
fof(f90, plain, ! [X4, X2, X0] : ((singleton(X2) = apply(sK7(X0), X2)) | ~ in(X2, X0) | ~ (singleton(sK8(X0)) = X4) | sP0(X0)), inference(cnf_transformation, [], [f61])).
fof(f375, plain, (in(sK2(sK7(sK1)), sK1) | ~ spl15_14), inference(avatar_component_clause, [], [f373])).
fof(f625, plain, (~ spl15_11 | ~ spl15_12 | ~ spl15_13 | ~ spl15_14), inference(avatar_contradiction_clause, [], [f624])).
fof(f624, plain, ($false | (~ spl15_11 | ~ spl15_12 | ~ spl15_13 | ~ spl15_14)), inference(subsumption_resolution, [], [f623, f366])).
fof(f623, plain, (~ relation(sK7(sK1)) | (~ spl15_11 | ~ spl15_13 | ~ spl15_14)), inference(subsumption_resolution, [], [f622, f370])).
fof(f370, plain, (function(sK7(sK1)) | ~ spl15_13), inference(avatar_component_clause, [], [f369])).
fof(f622, plain, (~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | (~ spl15_11 | ~ spl15_14)), inference(subsumption_resolution, [], [f621, f392])).
fof(f392, plain, ((sK1 = relation_dom(sK7(sK1))) | ~ spl15_11), inference(resolution, [], [f388, f115])).
fof(f388, plain, (~ sP0(sK1) | ~ spl15_11), inference(trivial_inequality_removal, [], [f387])).
fof(f387, plain, (~ (sK5(sK1) = sK5(sK1)) | ~ sP0(sK1) | ~ spl15_11), inference(superposition, [], [f82, f363])).
fof(f363, plain, ((sK5(sK1) = sK6(sK1)) | ~ spl15_11), inference(avatar_component_clause, [], [f361])).
fof(f82, plain, ! [X0] : (~ (sK5(X0) = sK6(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f57])).
fof(f621, plain, (~ (sK1 = relation_dom(sK7(sK1))) | ~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | (~ spl15_11 | ~ spl15_14)), inference(trivial_inequality_removal, [], [f620])).
fof(f620, plain, (~ (singleton(sK2(sK7(sK1))) = singleton(sK2(sK7(sK1)))) | ~ (sK1 = relation_dom(sK7(sK1))) | ~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | (~ spl15_11 | ~ spl15_14)), inference(superposition, [], [f75, f455])).
fof(f455, plain, ((singleton(sK2(sK7(sK1))) = apply(sK7(sK1), sK2(sK7(sK1)))) | (~ spl15_11 | ~ spl15_14)), inference(subsumption_resolution, [], [f451, f388])).
fof(f430, plain, (~ spl15_11 | spl15_13), inference(avatar_contradiction_clause, [], [f429])).
fof(f429, plain, ($false | (~ spl15_11 | spl15_13)), inference(subsumption_resolution, [], [f428, f388])).
fof(f428, plain, (sP0(sK1) | spl15_13), inference(resolution, [], [f371, f116])).
fof(f410, plain, (~ spl15_13 | spl15_14 | ~ spl15_11 | ~ spl15_12), inference(avatar_split_clause, [], [f409, f365, f361, f373, f369])).
fof(f409, plain, (in(sK2(sK7(sK1)), sK1) | ~ function(sK7(sK1)) | (~ spl15_11 | ~ spl15_12)), inference(subsumption_resolution, [], [f408, f366])).
fof(f408, plain, (in(sK2(sK7(sK1)), sK1) | ~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | ~ spl15_11), inference(trivial_inequality_removal, [], [f403])).
fof(f403, plain, (~ (sK1 = sK1) | in(sK2(sK7(sK1)), sK1) | ~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | ~ spl15_11), inference(superposition, [], [f74, f392])).
fof(f74, plain, ! [X1] : (~ (relation_dom(X1) = sK1) | in(sK2(X1), sK1) | ~ function(X1) | ~ relation(X1)), inference(cnf_transformation, [], [f51])).
fof(f390, plain, (~ spl15_11 | spl15_12), inference(avatar_contradiction_clause, [], [f389])).
fof(f389, plain, ($false | (~ spl15_11 | spl15_12)), inference(subsumption_resolution, [], [f388, f381])).
fof(f381, plain, (sP0(sK1) | spl15_12), inference(resolution, [], [f367, f117])).
fof(f117, plain, ! [X0] : (relation(sK7(X0)) | sP0(X0)), inference(equality_resolution, [], [f84])).
fof(f84, plain, ! [X4, X0] : (relation(sK7(X0)) | ~ (singleton(sK8(X0)) = X4) | sP0(X0)), inference(cnf_transformation, [], [f61])).
fof(f367, plain, (~ relation(sK7(sK1)) | spl15_12), inference(avatar_component_clause, [], [f365])).
fof(f386, plain, (spl15_11 | spl15_12), inference(avatar_split_clause, [], [f385, f365, f361])).
fof(f385, plain, ((sK5(sK1) = sK6(sK1)) | spl15_12), inference(backward_demodulation, [], [f382, f383])).
fof(f383, plain, ((sK5(sK1) = singleton(sK4(sK1))) | spl15_12), inference(resolution, [], [f381, f80])).
fof(f382, plain, ((sK6(sK1) = singleton(sK4(sK1))) | spl15_12), inference(resolution, [], [f381, f81])).
fof(f376, plain, (spl15_11 | ~ spl15_12 | ~ spl15_13 | spl15_14), inference(avatar_split_clause, [], [f359, f373, f369, f365, f361])).
fof(f359, plain, (in(sK2(sK7(sK1)), sK1) | ~ function(sK7(sK1)) | ~ relation(sK7(sK1)) | (sK5(sK1) = sK6(sK1))), inference(equality_resolution, [], [f236])).
fof(f236, plain, ! [X0] : (~ (sK1 = X0) | in(sK2(sK7(X0)), sK1) | ~ function(sK7(X0)) | ~ relation(sK7(X0)) | (sK5(X0) = sK6(X0))), inference(superposition, [], [f74, f222])).
fof(f222, plain, ! [X1] : ((relation_dom(sK7(X1)) = X1) | (sK5(X1) = sK6(X1))), inference(duplicate_literal_removal, [], [f219])).
fof(f219, plain, ! [X1] : ((sK5(X1) = sK6(X1)) | (relation_dom(sK7(X1)) = X1) | (relation_dom(sK7(X1)) = X1)), inference(superposition, [], [f153, f149])).
fof(f149, plain, ! [X0] : ((sK5(X0) = singleton(sK4(X0))) | (relation_dom(sK7(X0)) = X0)), inference(resolution, [], [f80, f115])).
fof(f153, plain, ! [X0] : ((sK6(X0) = singleton(sK4(X0))) | (relation_dom(sK7(X0)) = X0)), inference(resolution, [], [f81, f115])).