fof(f69088, plain, $false, inference(avatar_sat_refutation, [], [f610, f616, f69084])).
fof(f69084, plain, ~ spl59_1, inference(avatar_contradiction_clause, [], [f69083])).
fof(f69083, plain, ($false | ~ spl59_1), inference(subsumption_resolution, [], [f69078, f4512])).
fof(f4512, plain, (memberP(sK56, hd(sK55)) | ~ spl59_1), inference(backward_demodulation, [], [f1267, f4510])).
fof(f4510, plain, ((sK54(sK56, sK55) = hd(sK55)) | ~ spl59_1), inference(forward_demodulation, [], [f4431, f1407])).
fof(f1407, plain, ((sK55 = cons(sK54(sK56, sK55), nil)) | ~ spl59_1), inference(resolution, [], [f557, f619])).
fof(f619, plain, (sP6(sK56, sK55, sK56, sK55) | ~ spl59_1), inference(forward_demodulation, [], [f618, f563])).
fof(f563, plain, (sK56 = sK58), inference(cnf_transformation, [], [f354])).
fof(f354, plain, ((((((~ neq(sK58, nil) & neq(sK56, nil)) | sP6(sK58, sK57, sK56, sK55)) & (sK55 = sK57) & (sK56 = sK58) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)) & ssList(sK55)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK55, sK56, sK57, sK58])], [f234, f353, f352, f351, f350])).
fof(f350, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, X0)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, sK55)) & (sK55 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK55))), introduced(choice_axiom, [])).
fof(f351, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, sK55)) & (sK55 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK56, nil)) | sP6(X3, X2, sK56, sK55)) & (sK55 = X2) & (sK56 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK56, nil)) | sP6(X3, X2, sK56, sK55)) & (sK55 = X2) & (sK56 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK56, nil)) | sP6(X3, sK57, sK56, sK55)) & (sK55 = sK57) & (sK56 = X3) & ssList(X3)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK56, nil)) | sP6(X3, sK57, sK56, sK55)) & (sK55 = sK57) & (sK56 = X3) & ssList(X3)) => (((~ neq(sK58, nil) & neq(sK56, nil)) | sP6(sK58, sK57, sK56, sK55)) & (sK55 = sK57) & (sK56 = sK58) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, X0)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X3, X2, X1, X0] : ((? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4)) & ! [X5] : (~ memberP(X1, X5) | ~ (cons(X5, nil) = X0) | ~ ssItem(X5)) & neq(X1, nil)) | ~ sP6(X3, X2, X1, X0)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2, X1, X0] : (sP6(X3, X2, X1, X0) <=> (? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4)) & ! [X5] : (~ memberP(X1, X5) | ~ (cons(X5, nil) = X0) | ~ ssItem(X5)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4)) & ! [X5] : (~ memberP(X1, X5) | ~ (cons(X5, nil) = X0) | ~ ssItem(X5)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : ((memberP(X3, X4) & (cons(X4, nil) = X2)) & ssItem(X4)) & ! [X5] : (~ memberP(X1, X5) | ~ (cons(X5, nil) = X0) | ~ ssItem(X5)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X4] : (ssItem(X4) => (~ memberP(X3, X4) | ~ (cons(X4, nil) = X2))) | ? [X5] : (memberP(X1, X5) & (cons(X5, nil) = X0) & ssItem(X5)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X5] : (ssItem(X5) => (~ memberP(X3, X5) | ~ (cons(X5, nil) = X2))) | ? [X4] : (memberP(X1, X4) & (cons(X4, nil) = X0) & ssItem(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X5] : (ssItem(X5) => (~ memberP(X3, X5) | ~ (cons(X5, nil) = X2))) | ? [X4] : (memberP(X1, X4) & (cons(X4, nil) = X0) & ssItem(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC381+1.p', co1)).
fof(f618, plain, (sP6(sK58, sK55, sK56, sK55) | ~ spl59_1), inference(forward_demodulation, [], [f605, f564])).
fof(f564, plain, (sK55 = sK57), inference(cnf_transformation, [], [f354])).
fof(f605, plain, (sP6(sK58, sK57, sK56, sK55) | ~ spl59_1), inference(avatar_component_clause, [], [f603])).
fof(f603, plain, (spl59_1 <=> sP6(sK58, sK57, sK56, sK55)), introduced(avatar_definition, [new_symbols(naming, [spl59_1])])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f349])).
fof(f349, plain, ! [X0, X1, X2, X3] : (((memberP(X0, sK54(X0, X1)) & (cons(sK54(X0, X1), nil) = X1) & ssItem(sK54(X0, X1))) & ! [X5] : (~ memberP(X2, X5) | ~ (cons(X5, nil) = X3) | ~ ssItem(X5)) & neq(X2, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54])], [f347, f348])).
fof(f348, plain, ! [X1, X0] : (? [X4] : (memberP(X0, X4) & (cons(X4, nil) = X1) & ssItem(X4)) => (memberP(X0, sK54(X0, X1)) & (cons(sK54(X0, X1), nil) = X1) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1, X2, X3] : ((? [X4] : (memberP(X0, X4) & (cons(X4, nil) = X1) & ssItem(X4)) & ! [X5] : (~ memberP(X2, X5) | ~ (cons(X5, nil) = X3) | ~ ssItem(X5)) & neq(X2, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2, X1, X0] : ((? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4)) & ! [X5] : (~ memberP(X1, X5) | ~ (cons(X5, nil) = X0) | ~ ssItem(X5)) & neq(X1, nil)) | ~ sP6(X3, X2, X1, X0)), inference(nnf_transformation, [], [f233])).
fof(f4431, plain, ((sK54(sK56, sK55) = hd(cons(sK54(sK56, sK55), nil))) | ~ spl59_1), inference(resolution, [], [f1005, f986])).
fof(f986, plain, (ssItem(sK54(sK56, sK55)) | ~ spl59_1), inference(resolution, [], [f556, f619])).
fof(f556, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X0, X1))), inference(cnf_transformation, [], [f349])).
fof(f1005, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f456, f447])).
fof(f447, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC381+1.p', ax17)).
fof(f456, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC381+1.p', ax23)).
fof(f1267, plain, (memberP(sK56, sK54(sK56, sK55)) | ~ spl59_1), inference(resolution, [], [f558, f619])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | memberP(X0, sK54(X0, X1))), inference(cnf_transformation, [], [f349])).
fof(f69078, plain, (~ memberP(sK56, hd(sK55)) | ~ spl59_1), inference(resolution, [], [f619, f4515])).
fof(f4515, plain, (! [X2, X0, X1] : (~ sP6(X0, X1, X2, sK55) | ~ memberP(X2, hd(sK55))) | ~ spl59_1), inference(backward_demodulation, [], [f2917, f4510])).
fof(f2917, plain, (! [X2, X0, X1] : (~ sP6(X0, X1, X2, sK55) | ~ memberP(X2, sK54(sK56, sK55))) | ~ spl59_1), inference(subsumption_resolution, [], [f2879, f986])).
fof(f2879, plain, (! [X2, X0, X1] : (~ sP6(X0, X1, X2, sK55) | ~ ssItem(sK54(sK56, sK55)) | ~ memberP(X2, sK54(sK56, sK55))) | ~ spl59_1), inference(superposition, [], [f594, f1407])).
fof(f594, plain, ! [X2, X0, X5, X1] : (~ sP6(X0, X1, X2, cons(X5, nil)) | ~ ssItem(X5) | ~ memberP(X2, X5)), inference(equality_resolution, [], [f555])).
fof(f555, plain, ! [X2, X0, X5, X3, X1] : (~ memberP(X2, X5) | ~ (cons(X5, nil) = X3) | ~ ssItem(X5) | ~ sP6(X0, X1, X2, X3)), inference(cnf_transformation, [], [f349])).
fof(f616, plain, spl59_2, inference(avatar_contradiction_clause, [], [f615])).
fof(f615, plain, ($false | spl59_2), inference(subsumption_resolution, [], [f614, f611])).
fof(f611, plain, neq(sK56, nil), inference(subsumption_resolution, [], [f565, f554])).
fof(f554, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X2, nil)), inference(cnf_transformation, [], [f349])).
fof(f565, plain, (neq(sK56, nil) | sP6(sK58, sK57, sK56, sK55)), inference(cnf_transformation, [], [f354])).
fof(f614, plain, (~ neq(sK56, nil) | spl59_2), inference(forward_demodulation, [], [f609, f563])).
fof(f609, plain, (~ neq(sK58, nil) | spl59_2), inference(avatar_component_clause, [], [f607])).
fof(f607, plain, (spl59_2 <=> neq(sK58, nil)), introduced(avatar_definition, [new_symbols(naming, [spl59_2])])).
fof(f610, plain, (spl59_1 | ~ spl59_2), inference(avatar_split_clause, [], [f566, f607, f603])).
fof(f566, plain, (~ neq(sK58, nil) | sP6(sK58, sK57, sK56, sK55)), inference(cnf_transformation, [], [f354])).