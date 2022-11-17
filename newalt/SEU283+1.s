fof(f11832, plain, $false, inference(avatar_sat_refutation, [], [f633, f1263, f1318, f1327, f11828])).
fof(f11828, plain, (~ spl19_1 | ~ spl19_41 | ~ spl19_42 | ~ spl19_43), inference(avatar_contradiction_clause, [], [f11827])).
fof(f11827, plain, ($false | (~ spl19_1 | ~ spl19_41 | ~ spl19_42 | ~ spl19_43)), inference(subsumption_resolution, [], [f11801, f132])).
fof(f132, plain, empty(empty_set), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (relation(empty_set) & empty(empty_set)), inference(pure_predicate_removal, [], [f20])).
fof(f20, plain, (relation_empty_yielding(empty_set) & relation(empty_set) & empty(empty_set)), file('/home/ubuntu/library/tptp/Problems/SEU/SEU283+1.p', fc12_relat_1)).
fof(f11801, plain, (~ empty(empty_set) | (~ spl19_1 | ~ spl19_41 | ~ spl19_42 | ~ spl19_43)), inference(superposition, [], [f136, f11798])).
fof(f11798, plain, ((empty_set = singleton(sK2(sK5(sK1)))) | (~ spl19_1 | ~ spl19_41 | ~ spl19_42 | ~ spl19_43)), inference(backward_demodulation, [], [f1726, f11797])).
fof(f11797, plain, ((empty_set = apply(sK5(sK1), sK2(sK5(sK1)))) | (~ spl19_1 | ~ spl19_41 | ~ spl19_42)), inference(subsumption_resolution, [], [f11796, f1253])).
fof(f1253, plain, (relation(sK5(sK1)) | ~ spl19_41), inference(avatar_component_clause, [], [f1252])).
fof(f1252, plain, (spl19_41 <=> relation(sK5(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl19_41])])).
fof(f11796, plain, (~ relation(sK5(sK1)) | (empty_set = apply(sK5(sK1), sK2(sK5(sK1)))) | (~ spl19_1 | ~ spl19_42)), inference(subsumption_resolution, [], [f11795, f1257])).
fof(f1257, plain, (function(sK5(sK1)) | ~ spl19_42), inference(avatar_component_clause, [], [f1256])).
fof(f1256, plain, (spl19_42 <=> function(sK5(sK1))), introduced(avatar_definition, [new_symbols(naming, [spl19_42])])).
fof(f11795, plain, (~ function(sK5(sK1)) | ~ relation(sK5(sK1)) | (empty_set = apply(sK5(sK1), sK2(sK5(sK1)))) | ~ spl19_1), inference(subsumption_resolution, [], [f11794, f1200])).
fof(f1200, plain, ((sK1 = relation_dom(sK5(sK1))) | ~ spl19_1), inference(resolution, [], [f1056, f228])).
fof(f228, plain, (sP0(sK1) | ~ spl19_1), inference(avatar_component_clause, [], [f226])).
fof(f226, plain, (spl19_1 <=> sP0(sK1)), introduced(avatar_definition, [new_symbols(naming, [spl19_1])])).
fof(f1056, plain, ! [X0] : (~ sP0(X0) | (relation_dom(sK5(X0)) = X0)), inference(subsumption_resolution, [], [f1047, f385])).
fof(f385, plain, ! [X8, X7] : (~ in(sK9(sK5(X7), X8), X8) | (relation_dom(sK5(X7)) = X8) | ~ in(sK9(sK5(X7), X8), X7) | ~ sP0(X7)), inference(subsumption_resolution, [], [f380, f108])).
fof(f108, plain, ! [X0] : (relation(sK5(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ! [X0] : ((! [X2, X3] : ((in(ordered_pair(X2, X3), sK5(X0)) | ~ (singleton(X2) = X3) | ~ in(X2, X0) | ~ in(X2, X0)) & (((singleton(X2) = X3) & in(X2, X0) & in(X2, X0)) | ~ in(ordered_pair(X2, X3), sK5(X0)))) & function(sK5(X0)) & relation(sK5(X0))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f74, f75])).
fof(f75, plain, ! [X0] : (? [X1] : (! [X2, X3] : ((in(ordered_pair(X2, X3), X1) | ~ (singleton(X2) = X3) | ~ in(X2, X0) | ~ in(X2, X0)) & (((singleton(X2) = X3) & in(X2, X0) & in(X2, X0)) | ~ in(ordered_pair(X2, X3), X1))) & function(X1) & relation(X1)) => (! [X3, X2] : ((in(ordered_pair(X2, X3), sK5(X0)) | ~ (singleton(X2) = X3) | ~ in(X2, X0) | ~ in(X2, X0)) & (((singleton(X2) = X3) & in(X2, X0) & in(X2, X0)) | ~ in(ordered_pair(X2, X3), sK5(X0)))) & function(sK5(X0)) & relation(sK5(X0)))), introduced(choice_axiom, [])).
fof(f74, plain, ! [X0] : (? [X1] : (! [X2, X3] : ((in(ordered_pair(X2, X3), X1) | ~ (singleton(X2) = X3) | ~ in(X2, X0) | ~ in(X2, X0)) & (((singleton(X2) = X3) & in(X2, X0) & in(X2, X0)) | ~ in(ordered_pair(X2, X3), X1))) & function(X1) & relation(X1)) | ~ sP0(X0)), inference(rectify, [], [f73])).
fof(f73, plain, ! [X0] : (? [X4] : (! [X5, X6] : ((in(ordered_pair(X5, X6), X4) | ~ (singleton(X5) = X6) | ~ in(X5, X0) | ~ in(X5, X0)) & (((singleton(X5) = X6) & in(X5, X0) & in(X5, X0)) | ~ in(ordered_pair(X5, X6), X4))) & function(X4) & relation(X4)) | ~ sP0(X0)), inference(flattening, [], [f72])).
fof(f72, plain, ! [X0] : (? [X4] : (! [X5, X6] : ((in(ordered_pair(X5, X6), X4) | (~ (singleton(X5) = X6) | ~ in(X5, X0) | ~ in(X5, X0))) & (((singleton(X5) = X6) & in(X5, X0) & in(X5, X0)) | ~ in(ordered_pair(X5, X6), X4))) & function(X4) & relation(X4)) | ~ sP0(X0)), inference(nnf_transformation, [], [f63])).
fof(f63, plain, ! [X0] : (? [X4] : (! [X5, X6] : (in(ordered_pair(X5, X6), X4) <=> ((singleton(X5) = X6) & in(X5, X0) & in(X5, X0))) & function(X4) & relation(X4)) | ~ sP0(X0)), inference(usedef, [], [e63])).
fof(e63, plain, ! [X0] : (sP0(X0) <=> ? [X4] : (! [X5, X6] : (in(ordered_pair(X5, X6), X4) <=> ((singleton(X5) = X6) & in(X5, X0) & in(X5, X0))) & function(X4) & relation(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f380, plain, ! [X8, X7] : ((relation_dom(sK5(X7)) = X8) | ~ in(sK9(sK5(X7), X8), X8) | ~ relation(sK5(X7)) | ~ in(sK9(sK5(X7), X8), X7) | ~ sP0(X7)), inference(resolution, [], [f182, f177])).
fof(f177, plain, ! [X2, X0] : (in(unordered_pair(singleton(X2), unordered_pair(X2, singleton(X2))), sK5(X0)) | ~ in(X2, X0) | ~ sP0(X0)), inference(backward_demodulation, [], [f176, f121])).
fof(f121, plain, ! [X0, X1] : (unordered_pair(X0, X1) = unordered_pair(X1, X0)), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0, X1] : (unordered_pair(X0, X1) = unordered_pair(X1, X0)), file('/home/ubuntu/library/tptp/Problems/SEU/SEU283+1.p', commutativity_k2_tarski)).
fof(f176, plain, ! [X2, X0] : (in(unordered_pair(unordered_pair(X2, singleton(X2)), singleton(X2)), sK5(X0)) | ~ in(X2, X0) | ~ sP0(X0)), inference(duplicate_literal_removal, [], [f170])).
fof(f170, plain, ! [X2, X0] : (in(unordered_pair(unordered_pair(X2, singleton(X2)), singleton(X2)), sK5(X0)) | ~ in(X2, X0) | ~ in(X2, X0) | ~ sP0(X0)), inference(equality_resolution, [], [f157])).
fof(f157, plain, ! [X2, X0, X3] : (in(unordered_pair(unordered_pair(X2, X3), singleton(X2)), sK5(X0)) | ~ (singleton(X2) = X3) | ~ in(X2, X0) | ~ in(X2, X0) | ~ sP0(X0)), inference(definition_unfolding, [], [f113, f130])).
fof(f130, plain, ! [X0, X1] : (ordered_pair(X0, X1) = unordered_pair(unordered_pair(X0, X1), singleton(X0))), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ! [X0, X1] : (ordered_pair(X0, X1) = unordered_pair(unordered_pair(X0, X1), singleton(X0))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU283+1.p', d5_tarski)).
fof(f113, plain, ! [X2, X0, X3] : (in(ordered_pair(X2, X3), sK5(X0)) | ~ (singleton(X2) = X3) | ~ in(X2, X0) | ~ in(X2, X0) | ~ sP0(X0)), inference(cnf_transformation, [], [f76])).
fof(f182, plain, ! [X0, X3, X1] : (~ in(unordered_pair(singleton(sK9(X0, X1)), unordered_pair(sK9(X0, X1), X3)), X0) | (relation_dom(X0) = X1) | ~ in(sK9(X0, X1), X1) | ~ relation(X0)), inference(forward_demodulation, [], [f163, f121])).
fof(f163, plain, ! [X0, X3, X1] : ((relation_dom(X0) = X1) | ~ in(unordered_pair(unordered_pair(sK9(X0, X1), X3), singleton(sK9(X0, X1))), X0) | ~ in(sK9(X0, X1), X1) | ~ relation(X0)), inference(definition_unfolding, [], [f129, f130])).
fof(f129, plain, ! [X0, X3, X1] : ((relation_dom(X0) = X1) | ~ in(ordered_pair(sK9(X0, X1), X3), X0) | ~ in(sK9(X0, X1), X1) | ~ relation(X0)), inference(cnf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : (! [X1] : (((relation_dom(X0) = X1) | ((! [X3] : ~ in(ordered_pair(sK9(X0, X1), X3), X0) | ~ in(sK9(X0, X1), X1)) & (in(ordered_pair(sK9(X0, X1), sK10(X0, X1)), X0) | in(sK9(X0, X1), X1)))) & (! [X5] : ((in(X5, X1) | ! [X6] : ~ in(ordered_pair(X5, X6), X0)) & (in(ordered_pair(X5, sK11(X0, X5)), X0) | ~ in(X5, X1))) | ~ (relation_dom(X0) = X1))) | ~ relation(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9, sK10, sK11])], [f81, f84, f83, f82])).
fof(f82, plain, ! [X1, X0] : (? [X2] : ((! [X3] : ~ in(ordered_pair(X2, X3), X0) | ~ in(X2, X1)) & (? [X4] : in(ordered_pair(X2, X4), X0) | in(X2, X1))) => ((! [X3] : ~ in(ordered_pair(sK9(X0, X1), X3), X0) | ~ in(sK9(X0, X1), X1)) & (? [X4] : in(ordered_pair(sK9(X0, X1), X4), X0) | in(sK9(X0, X1), X1)))), introduced(choice_axiom, [])).
fof(f83, plain, ! [X1, X0] : (? [X4] : in(ordered_pair(sK9(X0, X1), X4), X0) => in(ordered_pair(sK9(X0, X1), sK10(X0, X1)), X0)), introduced(choice_axiom, [])).
fof(f84, plain, ! [X5, X0] : (? [X7] : in(ordered_pair(X5, X7), X0) => in(ordered_pair(X5, sK11(X0, X5)), X0)), introduced(choice_axiom, [])).
fof(f81, plain, ! [X0] : (! [X1] : (((relation_dom(X0) = X1) | ? [X2] : ((! [X3] : ~ in(ordered_pair(X2, X3), X0) | ~ in(X2, X1)) & (? [X4] : in(ordered_pair(X2, X4), X0) | in(X2, X1)))) & (! [X5] : ((in(X5, X1) | ! [X6] : ~ in(ordered_pair(X5, X6), X0)) & (? [X7] : in(ordered_pair(X5, X7), X0) | ~ in(X5, X1))) | ~ (relation_dom(X0) = X1))) | ~ relation(X0)), inference(rectify, [], [f80])).
fof(f80, plain, ! [X0] : (! [X1] : (((relation_dom(X0) = X1) | ? [X2] : ((! [X3] : ~ in(ordered_pair(X2, X3), X0) | ~ in(X2, X1)) & (? [X3] : in(ordered_pair(X2, X3), X0) | in(X2, X1)))) & (! [X2] : ((in(X2, X1) | ! [X3] : ~ in(ordered_pair(X2, X3), X0)) & (? [X3] : in(ordered_pair(X2, X3), X0) | ~ in(X2, X1))) | ~ (relation_dom(X0) = X1))) | ~ relation(X0)), inference(nnf_transformation, [], [f52])).
fof(f52, plain, ! [X0] : (! [X1] : ((relation_dom(X0) = X1) <=> ! [X2] : (in(X2, X1) <=> ? [X3] : in(ordered_pair(X2, X3), X0))) | ~ relation(X0)), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (relation(X0) => ! [X1] : ((relation_dom(X0) = X1) <=> ! [X2] : (in(X2, X1) <=> ? [X3] : in(ordered_pair(X2, X3), X0)))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU283+1.p', d4_relat_1)).
fof(f1047, plain, ! [X0] : (in(sK9(sK5(X0), X0), X0) | (relation_dom(sK5(X0)) = X0) | ~ sP0(X0)), inference(factoring, [], [f409])).
fof(f409, plain, ! [X4, X5] : (in(sK9(sK5(X4), X5), X5) | in(sK9(sK5(X4), X5), X4) | (relation_dom(sK5(X4)) = X5) | ~ sP0(X4)), inference(subsumption_resolution, [], [f396, f108])).
fof(f396, plain, ! [X4, X5] : ((relation_dom(sK5(X4)) = X5) | in(sK9(sK5(X4), X5), X5) | ~ relation(sK5(X4)) | in(sK9(sK5(X4), X5), X4) | ~ sP0(X4)), inference(resolution, [], [f183, f178])).
fof(f178, plain, ! [X2, X0, X3] : (~ in(unordered_pair(singleton(X2), unordered_pair(X2, X3)), sK5(X0)) | in(X2, X0) | ~ sP0(X0)), inference(backward_demodulation, [], [f159, f121])).
fof(f159, plain, ! [X2, X0, X3] : (in(X2, X0) | ~ in(unordered_pair(unordered_pair(X2, X3), singleton(X2)), sK5(X0)) | ~ sP0(X0)), inference(definition_unfolding, [], [f111, f130])).
fof(f111, plain, ! [X2, X0, X3] : (in(X2, X0) | ~ in(ordered_pair(X2, X3), sK5(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f76])).
fof(f183, plain, ! [X0, X1] : (in(unordered_pair(singleton(sK9(X0, X1)), unordered_pair(sK9(X0, X1), sK10(X0, X1))), X0) | (relation_dom(X0) = X1) | in(sK9(X0, X1), X1) | ~ relation(X0)), inference(forward_demodulation, [], [f164, f121])).
fof(f164, plain, ! [X0, X1] : ((relation_dom(X0) = X1) | in(unordered_pair(unordered_pair(sK9(X0, X1), sK10(X0, X1)), singleton(sK9(X0, X1))), X0) | in(sK9(X0, X1), X1) | ~ relation(X0)), inference(definition_unfolding, [], [f128, f130])).
fof(f128, plain, ! [X0, X1] : ((relation_dom(X0) = X1) | in(ordered_pair(sK9(X0, X1), sK10(X0, X1)), X0) | in(sK9(X0, X1), X1) | ~ relation(X0)), inference(cnf_transformation, [], [f85])).
fof(f11794, plain, (~ (sK1 = relation_dom(sK5(sK1))) | ~ function(sK5(sK1)) | ~ relation(sK5(sK1)) | (empty_set = apply(sK5(sK1), sK2(sK5(sK1)))) | ~ spl19_1), inference(trivial_inequality_removal, [], [f11792])).
fof(f11792, plain, (~ (singleton(sK2(sK5(sK1))) = singleton(sK2(sK5(sK1)))) | ~ (sK1 = relation_dom(sK5(sK1))) | ~ function(sK5(sK1)) | ~ relation(sK5(sK1)) | (empty_set = apply(sK5(sK1), sK2(sK5(sK1)))) | ~ spl19_1), inference(superposition, [], [f104, f2161])).
fof(f2161, plain, (! [X13] : ((singleton(X13) = apply(sK5(sK1), X13)) | (empty_set = apply(sK5(sK1), X13))) | ~ spl19_1), inference(resolution, [], [f569, f228])).
fof(f569, plain, ! [X2, X3] : (~ sP0(X3) | (singleton(X2) = apply(sK5(X3), X2)) | (empty_set = apply(sK5(X3), X2))), inference(subsumption_resolution, [], [f568, f108])).
fof(f568, plain, ! [X2, X3] : ((singleton(X2) = apply(sK5(X3), X2)) | ~ sP0(X3) | (empty_set = apply(sK5(X3), X2)) | ~ relation(sK5(X3))), inference(subsumption_resolution, [], [f554, f109])).
fof(f109, plain, ! [X0] : (function(sK5(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f76])).
fof(f554, plain, ! [X2, X3] : ((singleton(X2) = apply(sK5(X3), X2)) | ~ sP0(X3) | (empty_set = apply(sK5(X3), X2)) | ~ function(sK5(X3)) | ~ relation(sK5(X3))), inference(resolution, [], [f350, f171])).
fof(f171, plain, ! [X0, X1] : (in(X1, relation_dom(X0)) | (apply(X0, X1) = empty_set) | ~ function(X0) | ~ relation(X0)), inference(equality_resolution, [], [f125])).
fof(f125, plain, ! [X2, X0, X1] : ((apply(X0, X1) = X2) | ~ (empty_set = X2) | in(X1, relation_dom(X0)) | ~ function(X0) | ~ relation(X0)), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : (! [X1, X2] : (((((apply(X0, X1) = X2) | ~ (empty_set = X2)) & ((empty_set = X2) | ~ (apply(X0, X1) = X2))) | in(X1, relation_dom(X0))) & ((((apply(X0, X1) = X2) | ~ in(ordered_pair(X1, X2), X0)) & (in(ordered_pair(X1, X2), X0) | ~ (apply(X0, X1) = X2))) | ~ in(X1, relation_dom(X0)))) | ~ function(X0) | ~ relation(X0)), inference(nnf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (! [X1, X2] : ((((apply(X0, X1) = X2) <=> (empty_set = X2)) | in(X1, relation_dom(X0))) & (((apply(X0, X1) = X2) <=> in(ordered_pair(X1, X2), X0)) | ~ in(X1, relation_dom(X0)))) | ~ function(X0) | ~ relation(X0)), inference(flattening, [], [f50])).
fof(f50, plain, ! [X0] : (! [X1, X2] : ((((apply(X0, X1) = X2) <=> (empty_set = X2)) | in(X1, relation_dom(X0))) & (((apply(X0, X1) = X2) <=> in(ordered_pair(X1, X2), X0)) | ~ in(X1, relation_dom(X0)))) | (~ function(X0) | ~ relation(X0))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : ((function(X0) & relation(X0)) => ! [X1, X2] : ((~ in(X1, relation_dom(X0)) => ((apply(X0, X1) = X2) <=> (empty_set = X2))) & (in(X1, relation_dom(X0)) => ((apply(X0, X1) = X2) <=> in(ordered_pair(X1, X2), X0))))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU283+1.p', d4_funct_1)).
fof(f350, plain, ! [X0, X1] : (~ in(X0, relation_dom(sK5(X1))) | (singleton(X0) = apply(sK5(X1), X0)) | ~ sP0(X1)), inference(subsumption_resolution, [], [f349, f108])).
fof(f349, plain, ! [X0, X1] : (~ in(X0, relation_dom(sK5(X1))) | ~ relation(sK5(X1)) | (singleton(X0) = apply(sK5(X1), X0)) | ~ sP0(X1)), inference(subsumption_resolution, [], [f340, f109])).
fof(f340, plain, ! [X0, X1] : (~ in(X0, relation_dom(sK5(X1))) | ~ function(sK5(X1)) | ~ relation(sK5(X1)) | (singleton(X0) = apply(sK5(X1), X0)) | ~ sP0(X1)), inference(resolution, [], [f181, f179])).
fof(f179, plain, ! [X2, X0, X3] : (~ in(unordered_pair(singleton(X2), unordered_pair(X2, X3)), sK5(X0)) | (singleton(X2) = X3) | ~ sP0(X0)), inference(backward_demodulation, [], [f158, f121])).
fof(f158, plain, ! [X2, X0, X3] : ((singleton(X2) = X3) | ~ in(unordered_pair(unordered_pair(X2, X3), singleton(X2)), sK5(X0)) | ~ sP0(X0)), inference(definition_unfolding, [], [f112, f130])).
fof(f112, plain, ! [X2, X0, X3] : ((singleton(X2) = X3) | ~ in(ordered_pair(X2, X3), sK5(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f76])).
fof(f181, plain, ! [X0, X1] : (in(unordered_pair(singleton(X1), unordered_pair(X1, apply(X0, X1))), X0) | ~ in(X1, relation_dom(X0)) | ~ function(X0) | ~ relation(X0)), inference(forward_demodulation, [], [f173, f121])).
fof(f173, plain, ! [X0, X1] : (in(unordered_pair(unordered_pair(X1, apply(X0, X1)), singleton(X1)), X0) | ~ in(X1, relation_dom(X0)) | ~ function(X0) | ~ relation(X0)), inference(equality_resolution, [], [f162])).
fof(f162, plain, ! [X2, X0, X1] : (in(unordered_pair(unordered_pair(X1, X2), singleton(X1)), X0) | ~ (apply(X0, X1) = X2) | ~ in(X1, relation_dom(X0)) | ~ function(X0) | ~ relation(X0)), inference(definition_unfolding, [], [f122, f130])).
fof(f122, plain, ! [X2, X0, X1] : (in(ordered_pair(X1, X2), X0) | ~ (apply(X0, X1) = X2) | ~ in(X1, relation_dom(X0)) | ~ function(X0) | ~ relation(X0)), inference(cnf_transformation, [], [f79])).
fof(f104, plain, ! [X1] : (~ (apply(X1, sK2(X1)) = singleton(sK2(X1))) | ~ (relation_dom(X1) = sK1) | ~ function(X1) | ~ relation(X1)), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (! [X1] : ((~ (apply(X1, sK2(X1)) = singleton(sK2(X1))) & in(sK2(X1), sK1)) | ~ (relation_dom(X1) = sK1) | ~ function(X1) | ~ relation(X1)) & ! [X3] : ((singleton(X3) = sK3(X3)) | ~ in(X3, sK1)) & ! [X5, X6, X7] : ((X6 = X7) | ~ (singleton(X5) = X7) | ~ (singleton(X5) = X6) | ~ in(X5, sK1))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK1, sK2, sK3])], [f65, f68, f67, f66])).
fof(f66, plain, (? [X0] : (! [X1] : (? [X2] : (~ (apply(X1, X2) = singleton(X2)) & in(X2, X0)) | ~ (relation_dom(X1) = X0) | ~ function(X1) | ~ relation(X1)) & ! [X3] : (? [X4] : (singleton(X3) = X4) | ~ in(X3, X0)) & ! [X5, X6, X7] : ((X6 = X7) | ~ (singleton(X5) = X7) | ~ (singleton(X5) = X6) | ~ in(X5, X0))) => (! [X1] : (? [X2] : (~ (apply(X1, X2) = singleton(X2)) & in(X2, sK1)) | ~ (relation_dom(X1) = sK1) | ~ function(X1) | ~ relation(X1)) & ! [X3] : (? [X4] : (singleton(X3) = X4) | ~ in(X3, sK1)) & ! [X7, X6, X5] : ((X6 = X7) | ~ (singleton(X5) = X7) | ~ (singleton(X5) = X6) | ~ in(X5, sK1)))), introduced(choice_axiom, [])).
fof(f67, plain, ! [X1] : (? [X2] : (~ (apply(X1, X2) = singleton(X2)) & in(X2, sK1)) => (~ (apply(X1, sK2(X1)) = singleton(sK2(X1))) & in(sK2(X1), sK1))), introduced(choice_axiom, [])).
fof(f68, plain, ! [X3] : (? [X4] : (singleton(X3) = X4) => (singleton(X3) = sK3(X3))), introduced(choice_axiom, [])).
fof(f65, plain, ? [X0] : (! [X1] : (? [X2] : (~ (apply(X1, X2) = singleton(X2)) & in(X2, X0)) | ~ (relation_dom(X1) = X0) | ~ function(X1) | ~ relation(X1)) & ! [X3] : (? [X4] : (singleton(X3) = X4) | ~ in(X3, X0)) & ! [X5, X6, X7] : ((X6 = X7) | ~ (singleton(X5) = X7) | ~ (singleton(X5) = X6) | ~ in(X5, X0))), inference(rectify, [], [f44])).
fof(f44, plain, ? [X0] : (! [X6] : (? [X7] : (~ (apply(X6, X7) = singleton(X7)) & in(X7, X0)) | ~ (relation_dom(X6) = X0) | ~ function(X6) | ~ relation(X6)) & ! [X1] : (? [X2] : (singleton(X1) = X2) | ~ in(X1, X0)) & ! [X3, X4, X5] : ((X4 = X5) | ~ (singleton(X3) = X5) | ~ (singleton(X3) = X4) | ~ in(X3, X0))), inference(flattening, [], [f43])).
fof(f43, plain, ? [X0] : (! [X6] : (? [X7] : (~ (apply(X6, X7) = singleton(X7)) & in(X7, X0)) | ~ (relation_dom(X6) = X0) | ~ function(X6) | ~ relation(X6)) & (! [X1] : (? [X2] : (singleton(X1) = X2) | ~ in(X1, X0)) & ! [X3, X4, X5] : ((X4 = X5) | (~ (singleton(X3) = X5) | ~ (singleton(X3) = X4) | ~ in(X3, X0))))), inference(ennf_transformation, [], [f39])).
fof(f39, plain, ~ ! [X0] : ((! [X1] : ~ (! [X2] : ~ (singleton(X1) = X2) & in(X1, X0)) & ! [X3, X4, X5] : (((singleton(X3) = X5) & (singleton(X3) = X4) & in(X3, X0)) => (X4 = X5))) => ? [X6] : (! [X7] : (in(X7, X0) => (apply(X6, X7) = singleton(X7))) & (relation_dom(X6) = X0) & function(X6) & relation(X6))), inference(rectify, [], [f2])).
fof(f2, plain, ~ ! [X0] : ((! [X1] : ~ (! [X2] : ~ (singleton(X1) = X2) & in(X1, X0)) & ! [X1, X2, X3] : (((singleton(X1) = X3) & (singleton(X1) = X2) & in(X1, X0)) => (X2 = X3))) => ? [X1] : (! [X2] : (in(X2, X0) => (apply(X1, X2) = singleton(X2))) & (relation_dom(X1) = X0) & function(X1) & relation(X1))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ! [X0] : ((! [X1] : ~ (! [X2] : ~ (singleton(X1) = X2) & in(X1, X0)) & ! [X1, X2, X3] : (((singleton(X1) = X3) & (singleton(X1) = X2) & in(X1, X0)) => (X2 = X3))) => ? [X1] : (! [X2] : (in(X2, X0) => (apply(X1, X2) = singleton(X2))) & (relation_dom(X1) = X0) & function(X1) & relation(X1))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU283+1.p', s2_funct_1__e16_22__wellord2__1)).
fof(f1726, plain, ((singleton(sK2(sK5(sK1))) = apply(sK5(sK1), sK2(sK5(sK1)))) | (~ spl19_1 | ~ spl19_43)), inference(subsumption_resolution, [], [f1719, f228])).
fof(f1719, plain, (~ sP0(sK1) | (singleton(sK2(sK5(sK1))) = apply(sK5(sK1), sK2(sK5(sK1)))) | ~ spl19_43), inference(resolution, [], [f567, f1262])).
fof(f1262, plain, (in(sK2(sK5(sK1)), sK1) | ~ spl19_43), inference(avatar_component_clause, [], [f1260])).
fof(f1260, plain, (spl19_43 <=> in(sK2(sK5(sK1)), sK1)), introduced(avatar_definition, [new_symbols(naming, [spl19_43])])).
fof(f567, plain, ! [X0, X1] : (~ in(X0, X1) | ~ sP0(X1) | (singleton(X0) = apply(sK5(X1), X0))), inference(duplicate_literal_removal, [], [f553])).
fof(f553, plain, ! [X0, X1] : ((singleton(X0) = apply(sK5(X1), X0)) | ~ sP0(X1) | ~ sP0(X1) | ~ in(X0, X1)), inference(resolution, [], [f350, f308])).
fof(f308, plain, ! [X2, X3] : (in(X2, relation_dom(sK5(X3))) | ~ sP0(X3) | ~ in(X2, X3)), inference(subsumption_resolution, [], [f303, f108])).
fof(f303, plain, ! [X2, X3] : (~ in(X2, X3) | ~ sP0(X3) | in(X2, relation_dom(sK5(X3))) | ~ relation(sK5(X3))), inference(resolution, [], [f177, f184])).
fof(f184, plain, ! [X6, X0, X5] : (~ in(unordered_pair(singleton(X5), unordered_pair(X5, X6)), X0) | in(X5, relation_dom(X0)) | ~ relation(X0)), inference(forward_demodulation, [], [f174, f121])).
fof(f174, plain, ! [X6, X0, X5] : (in(X5, relation_dom(X0)) | ~ in(unordered_pair(unordered_pair(X5, X6), singleton(X5)), X0) | ~ relation(X0)), inference(equality_resolution, [], [f165])).
fof(f165, plain, ! [X6, X0, X5, X1] : (in(X5, X1) | ~ in(unordered_pair(unordered_pair(X5, X6), singleton(X5)), X0) | ~ (relation_dom(X0) = X1) | ~ relation(X0)), inference(definition_unfolding, [], [f127, f130])).
fof(f127, plain, ! [X6, X0, X5, X1] : (in(X5, X1) | ~ in(ordered_pair(X5, X6), X0) | ~ (relation_dom(X0) = X1) | ~ relation(X0)), inference(cnf_transformation, [], [f85])).
fof(f136, plain, ! [X0] : ~ empty(singleton(X0)), inference(cnf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : ~ empty(singleton(X0)), file('/home/ubuntu/library/tptp/Problems/SEU/SEU283+1.p', fc2_subset_1)).
fof(f1327, plain, (~ spl19_1 | spl19_42), inference(avatar_contradiction_clause, [], [f1326])).
fof(f1326, plain, ($false | (~ spl19_1 | spl19_42)), inference(subsumption_resolution, [], [f1325, f228])).
fof(f1325, plain, (~ sP0(sK1) | spl19_42), inference(resolution, [], [f1258, f109])).
fof(f1258, plain, (~ function(sK5(sK1)) | spl19_42), inference(avatar_component_clause, [], [f1256])).
fof(f1318, plain, (~ spl19_1 | spl19_41), inference(avatar_contradiction_clause, [], [f1317])).
fof(f1317, plain, ($false | (~ spl19_1 | spl19_41)), inference(subsumption_resolution, [], [f1316, f228])).
fof(f1316, plain, (~ sP0(sK1) | spl19_41), inference(resolution, [], [f1254, f108])).
fof(f1254, plain, (~ relation(sK5(sK1)) | spl19_41), inference(avatar_component_clause, [], [f1252])).
fof(f1263, plain, (~ spl19_41 | ~ spl19_42 | spl19_43 | ~ spl19_1), inference(avatar_split_clause, [], [f1250, f226, f1260, f1256, f1252])).
fof(f1250, plain, (in(sK2(sK5(sK1)), sK1) | ~ function(sK5(sK1)) | ~ relation(sK5(sK1)) | ~ spl19_1), inference(trivial_inequality_removal, [], [f1229])).
fof(f1229, plain, (~ (sK1 = sK1) | in(sK2(sK5(sK1)), sK1) | ~ function(sK5(sK1)) | ~ relation(sK5(sK1)) | ~ spl19_1), inference(superposition, [], [f103, f1200])).
fof(f103, plain, ! [X1] : (~ (relation_dom(X1) = sK1) | in(sK2(X1), sK1) | ~ function(X1) | ~ relation(X1)), inference(cnf_transformation, [], [f69])).
fof(f633, plain, spl19_1, inference(avatar_contradiction_clause, [], [f632])).
fof(f632, plain, ($false | spl19_1), inference(subsumption_resolution, [], [f631, f227])).
fof(f227, plain, (~ sP0(sK1) | spl19_1), inference(avatar_component_clause, [], [f226])).
fof(f631, plain, (sP0(sK1) | spl19_1), inference(trivial_inequality_removal, [], [f630])).
fof(f630, plain, (~ (sK7(sK1) = sK7(sK1)) | sP0(sK1) | spl19_1), inference(superposition, [], [f118, f628])).
fof(f628, plain, ((sK8(sK1) = sK7(sK1)) | spl19_1), inference(backward_demodulation, [], [f625, f626])).
fof(f626, plain, ((singleton(sK6(sK1)) = sK7(sK1)) | spl19_1), inference(resolution, [], [f227, f115])).
fof(f115, plain, ! [X0] : (sP0(X0) | (sK7(X0) = singleton(sK6(X0)))), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (sP0(X0) | (~ (sK7(X0) = sK8(X0)) & (sK8(X0) = singleton(sK6(X0))) & in(sK6(X0), X0) & (sK7(X0) = singleton(sK6(X0))) & in(sK6(X0), X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7, sK8])], [f64, f77])).
fof(f77, plain, ! [X0] : (? [X1, X2, X3] : (~ (X2 = X3) & (singleton(X1) = X3) & in(X1, X0) & (singleton(X1) = X2) & in(X1, X0)) => (~ (sK7(X0) = sK8(X0)) & (sK8(X0) = singleton(sK6(X0))) & in(sK6(X0), X0) & (sK7(X0) = singleton(sK6(X0))) & in(sK6(X0), X0))), introduced(choice_axiom, [])).
fof(f64, plain, ! [X0] : (sP0(X0) | ? [X1, X2, X3] : (~ (X2 = X3) & (singleton(X1) = X3) & in(X1, X0) & (singleton(X1) = X2) & in(X1, X0))), inference(definition_folding, [], [f47, e63])).
fof(f47, plain, ! [X0] : (? [X4] : (! [X5, X6] : (in(ordered_pair(X5, X6), X4) <=> ((singleton(X5) = X6) & in(X5, X0) & in(X5, X0))) & function(X4) & relation(X4)) | ? [X1, X2, X3] : (~ (X2 = X3) & (singleton(X1) = X3) & in(X1, X0) & (singleton(X1) = X2) & in(X1, X0))), inference(flattening, [], [f46])).
fof(f46, plain, ! [X0] : (? [X4] : (! [X5, X6] : (in(ordered_pair(X5, X6), X4) <=> ((singleton(X5) = X6) & in(X5, X0) & in(X5, X0))) & function(X4) & relation(X4)) | ? [X1, X2, X3] : (~ (X2 = X3) & ((singleton(X1) = X3) & in(X1, X0) & (singleton(X1) = X2) & in(X1, X0)))), inference(ennf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (! [X1, X2, X3] : (((singleton(X1) = X3) & in(X1, X0) & (singleton(X1) = X2) & in(X1, X0)) => (X2 = X3)) => ? [X4] : (! [X5, X6] : (in(ordered_pair(X5, X6), X4) <=> ((singleton(X5) = X6) & in(X5, X0) & in(X5, X0))) & function(X4) & relation(X4))), inference(rectify, [], [f8])).
fof(f8, plain, ! [X0] : (! [X1, X2, X3] : (((singleton(X1) = X3) & in(X1, X0) & (singleton(X1) = X2) & in(X1, X0)) => (X2 = X3)) => ? [X1] : (! [X2, X3] : (in(ordered_pair(X2, X3), X1) <=> ((singleton(X2) = X3) & in(X2, X0) & in(X2, X0))) & function(X1) & relation(X1))), file('/home/ubuntu/library/tptp/Problems/SEU/SEU283+1.p', s1_funct_1__e16_22__wellord2__1)).
fof(f625, plain, ((singleton(sK6(sK1)) = sK8(sK1)) | spl19_1), inference(resolution, [], [f227, f117])).
fof(f117, plain, ! [X0] : (sP0(X0) | (sK8(X0) = singleton(sK6(X0)))), inference(cnf_transformation, [], [f78])).
fof(f118, plain, ! [X0] : (~ (sK7(X0) = sK8(X0)) | sP0(X0)), inference(cnf_transformation, [], [f78])).