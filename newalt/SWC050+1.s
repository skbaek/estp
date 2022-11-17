fof(f5177, plain, $false, inference(avatar_sat_refutation, [], [f610, f616, f5174])).
fof(f5174, plain, ~ spl59_1, inference(avatar_contradiction_clause, [], [f5173])).
fof(f5173, plain, ($false | ~ spl59_1), inference(subsumption_resolution, [], [f5172, f1751])).
fof(f1751, plain, (rearsegP(sK55, sK54(sK55, sK56)) | ~ spl59_1), inference(resolution, [], [f619, f559])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | rearsegP(X0, sK54(X0, X1))), inference(cnf_transformation, [], [f349])).
fof(f349, plain, ! [X0, X1, X2, X3] : (((rearsegP(X0, sK54(X0, X1)) & rearsegP(X1, sK54(X0, X1)) & neq(sK54(X0, X1), nil) & ssList(sK54(X0, X1))) & ! [X5] : (~ rearsegP(X2, X5) | ~ rearsegP(X3, X5) | ~ neq(X5, nil) | ~ ssList(X5)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54])], [f347, f348])).
fof(f348, plain, ! [X1, X0] : (? [X4] : (rearsegP(X0, X4) & rearsegP(X1, X4) & neq(X4, nil) & ssList(X4)) => (rearsegP(X0, sK54(X0, X1)) & rearsegP(X1, sK54(X0, X1)) & neq(sK54(X0, X1), nil) & ssList(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1, X2, X3] : ((? [X4] : (rearsegP(X0, X4) & rearsegP(X1, X4) & neq(X4, nil) & ssList(X4)) & ! [X5] : (~ rearsegP(X2, X5) | ~ rearsegP(X3, X5) | ~ neq(X5, nil) | ~ ssList(X5)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X2, X3, X0, X1] : ((? [X4] : (rearsegP(X2, X4) & rearsegP(X3, X4) & neq(X4, nil) & ssList(X4)) & ! [X5] : (~ rearsegP(X0, X5) | ~ rearsegP(X1, X5) | ~ neq(X5, nil) | ~ ssList(X5)) & neq(X1, nil)) | ~ sP6(X2, X3, X0, X1)), inference(nnf_transformation, [], [f233])).
fof(f233, plain, ! [X2, X3, X0, X1] : ((? [X4] : (rearsegP(X2, X4) & rearsegP(X3, X4) & neq(X4, nil) & ssList(X4)) & ! [X5] : (~ rearsegP(X0, X5) | ~ rearsegP(X1, X5) | ~ neq(X5, nil) | ~ ssList(X5)) & neq(X1, nil)) | ~ sP6(X2, X3, X0, X1)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X2, X3, X0, X1] : (sP6(X2, X3, X0, X1) <=> (? [X4] : (rearsegP(X2, X4) & rearsegP(X3, X4) & neq(X4, nil) & ssList(X4)) & ! [X5] : (~ rearsegP(X0, X5) | ~ rearsegP(X1, X5) | ~ neq(X5, nil) | ~ ssList(X5)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f619, plain, (sP6(sK55, sK56, sK55, sK56) | ~ spl59_1), inference(forward_demodulation, [], [f618, f565])).
fof(f565, plain, (sK55 = sK57), inference(cnf_transformation, [], [f354])).
fof(f354, plain, ((((((~ neq(sK58, nil) & neq(sK56, nil)) | sP6(sK57, sK58, sK55, sK56)) & (sK55 = sK57) & (sK56 = sK58) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)) & ssList(sK55)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK55, sK56, sK57, sK58])], [f234, f353, f352, f351, f350])).
fof(f350, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, X0, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, sK55, X1)) & (sK55 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK55))), introduced(choice_axiom, [])).
fof(f351, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, sK55, X1)) & (sK55 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK56, nil)) | sP6(X2, X3, sK55, sK56)) & (sK55 = X2) & (sK56 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK56, nil)) | sP6(X2, X3, sK55, sK56)) & (sK55 = X2) & (sK56 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK56, nil)) | sP6(sK57, X3, sK55, sK56)) & (sK55 = sK57) & (sK56 = X3) & ssList(X3)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK56, nil)) | sP6(sK57, X3, sK55, sK56)) & (sK55 = sK57) & (sK56 = X3) & ssList(X3)) => (((~ neq(sK58, nil) & neq(sK56, nil)) | sP6(sK57, sK58, sK55, sK56)) & (sK55 = sK57) & (sK56 = sK58) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, X0, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (rearsegP(X2, X4) & rearsegP(X3, X4) & neq(X4, nil) & ssList(X4)) & ! [X5] : (~ rearsegP(X0, X5) | ~ rearsegP(X1, X5) | ~ neq(X5, nil) | ~ ssList(X5)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : ((rearsegP(X2, X4) & rearsegP(X3, X4) & neq(X4, nil)) & ssList(X4)) & ! [X5] : (~ rearsegP(X0, X5) | ~ rearsegP(X1, X5) | ~ neq(X5, nil) | ~ ssList(X5)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X4] : (ssList(X4) => (~ rearsegP(X2, X4) | ~ rearsegP(X3, X4) | ~ neq(X4, nil))) | ? [X5] : (rearsegP(X0, X5) & rearsegP(X1, X5) & neq(X5, nil) & ssList(X5)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X5] : (ssList(X5) => (~ rearsegP(X2, X5) | ~ rearsegP(X3, X5) | ~ neq(X5, nil))) | ? [X4] : (rearsegP(X0, X4) & rearsegP(X1, X4) & neq(X4, nil) & ssList(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X5] : (ssList(X5) => (~ rearsegP(X2, X5) | ~ rearsegP(X3, X5) | ~ neq(X5, nil))) | ? [X4] : (rearsegP(X0, X4) & rearsegP(X1, X4) & neq(X4, nil) & ssList(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC050+1.p', co1)).
fof(f618, plain, (sP6(sK57, sK56, sK55, sK56) | ~ spl59_1), inference(forward_demodulation, [], [f605, f564])).
fof(f564, plain, (sK56 = sK58), inference(cnf_transformation, [], [f354])).
fof(f605, plain, (sP6(sK57, sK58, sK55, sK56) | ~ spl59_1), inference(avatar_component_clause, [], [f603])).
fof(f603, plain, (spl59_1 <=> sP6(sK57, sK58, sK55, sK56)), introduced(avatar_definition, [new_symbols(naming, [spl59_1])])).
fof(f5172, plain, (~ rearsegP(sK55, sK54(sK55, sK56)) | ~ spl59_1), inference(subsumption_resolution, [], [f5171, f986])).
fof(f986, plain, (ssList(sK54(sK55, sK56)) | ~ spl59_1), inference(resolution, [], [f556, f619])).
fof(f556, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK54(X0, X1))), inference(cnf_transformation, [], [f349])).
fof(f5171, plain, (~ ssList(sK54(sK55, sK56)) | ~ rearsegP(sK55, sK54(sK55, sK56)) | ~ spl59_1), inference(subsumption_resolution, [], [f5167, f1753])).
fof(f1753, plain, (neq(sK54(sK55, sK56), nil) | ~ spl59_1), inference(resolution, [], [f619, f557])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(sK54(X0, X1), nil)), inference(cnf_transformation, [], [f349])).
fof(f5167, plain, (~ neq(sK54(sK55, sK56), nil) | ~ ssList(sK54(sK55, sK56)) | ~ rearsegP(sK55, sK54(sK55, sK56)) | ~ spl59_1), inference(resolution, [], [f1976, f1752])).
fof(f1752, plain, (rearsegP(sK56, sK54(sK55, sK56)) | ~ spl59_1), inference(resolution, [], [f619, f558])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | rearsegP(X1, sK54(X0, X1))), inference(cnf_transformation, [], [f349])).
fof(f1976, plain, (! [X0] : (~ rearsegP(sK56, X0) | ~ neq(X0, nil) | ~ ssList(X0) | ~ rearsegP(sK55, X0)) | ~ spl59_1), inference(resolution, [], [f555, f619])).
fof(f555, plain, ! [X2, X0, X5, X3, X1] : (~ sP6(X0, X1, X2, X3) | ~ rearsegP(X3, X5) | ~ neq(X5, nil) | ~ ssList(X5) | ~ rearsegP(X2, X5)), inference(cnf_transformation, [], [f349])).
fof(f616, plain, spl59_2, inference(avatar_contradiction_clause, [], [f615])).
fof(f615, plain, ($false | spl59_2), inference(subsumption_resolution, [], [f614, f611])).
fof(f611, plain, neq(sK56, nil), inference(subsumption_resolution, [], [f566, f554])).
fof(f554, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X3, nil)), inference(cnf_transformation, [], [f349])).
fof(f566, plain, (neq(sK56, nil) | sP6(sK57, sK58, sK55, sK56)), inference(cnf_transformation, [], [f354])).
fof(f614, plain, (~ neq(sK56, nil) | spl59_2), inference(forward_demodulation, [], [f609, f564])).
fof(f609, plain, (~ neq(sK58, nil) | spl59_2), inference(avatar_component_clause, [], [f607])).
fof(f607, plain, (spl59_2 <=> neq(sK58, nil)), introduced(avatar_definition, [new_symbols(naming, [spl59_2])])).
fof(f610, plain, (spl59_1 | ~ spl59_2), inference(avatar_split_clause, [], [f567, f607, f603])).
fof(f567, plain, (~ neq(sK58, nil) | sP6(sK57, sK58, sK55, sK56)), inference(cnf_transformation, [], [f354])).