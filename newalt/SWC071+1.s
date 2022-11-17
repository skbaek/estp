fof(f6684, plain, $false, inference(avatar_sat_refutation, [], [f616, f621, f630, f634, f638, f1167, f1333, f5210, f6031, f6079, f6176, f6576])).
fof(f6576, plain, (spl61_2 | ~ spl61_5), inference(avatar_contradiction_clause, [], [f6575])).
fof(f6575, plain, ($false | (spl61_2 | ~ spl61_5)), inference(subsumption_resolution, [], [f6247, f614])).
fof(f614, plain, (~ (nil = sK59) | spl61_2), inference(avatar_component_clause, [], [f613])).
fof(f613, plain, (spl61_2 <=> (nil = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_2])])).
fof(f6247, plain, ((nil = sK59) | ~ spl61_5), inference(backward_demodulation, [], [f569, f628])).
fof(f628, plain, ((nil = sK57) | ~ spl61_5), inference(avatar_component_clause, [], [f627])).
fof(f627, plain, (spl61_5 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_5])])).
fof(f569, plain, (sK57 = sK59), inference(cnf_transformation, [], [f357])).
fof(f357, plain, (((((((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (~ (nil = sK57) | ~ (nil = sK58)) & ! [X4] : (~ segmentP(sK57, X4) | ~ segmentP(sK58, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60])], [f352, f356, f355, f354, f353])).
fof(f353, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (~ (nil = X0) | ~ (nil = X1)) & ! [X4] : (~ segmentP(X0, X4) | ~ segmentP(X1, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (~ (nil = sK57) | ~ (nil = X1)) & ! [X4] : (~ segmentP(sK57, X4) | ~ segmentP(X1, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (~ (nil = sK57) | ~ (nil = X1)) & ! [X4] : (~ segmentP(sK57, X4) | ~ segmentP(X1, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (~ (nil = sK57) | ~ (nil = sK58)) & ! [X4] : (~ segmentP(sK57, X4) | ~ segmentP(sK58, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f355, plain, (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (~ (nil = sK57) | ~ (nil = sK58)) & ! [X4] : (~ segmentP(sK57, X4) | ~ segmentP(sK58, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & (~ (nil = sK57) | ~ (nil = sK58)) & ! [X4] : (~ segmentP(sK57, X4) | ~ segmentP(sK58, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f356, plain, (? [X3] : ((((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & (~ (nil = sK57) | ~ (nil = sK58)) & ! [X4] : (~ segmentP(sK57, X4) | ~ segmentP(sK58, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => ((((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (~ (nil = sK57) | ~ (nil = sK58)) & ! [X4] : (~ segmentP(sK57, X4) | ~ segmentP(sK58, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f352, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (~ (nil = X0) | ~ (nil = X1)) & ! [X4] : (~ segmentP(X0, X4) | ~ segmentP(X1, X4) | ~ neq(X4, nil) | ~ ssList(X4)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(rectify, [], [f234])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (~ (nil = X0) | ~ (nil = X1)) & ! [X9] : (~ segmentP(X0, X9) | ~ segmentP(X1, X9) | ~ neq(X9, nil) | ~ ssList(X9)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2] : (sP6(X3, X2) <=> ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & (~ (nil = X0) | ~ (nil = X1)) & ! [X9] : (~ segmentP(X0, X9) | ~ segmentP(X1, X9) | ~ neq(X9, nil) | ~ ssList(X9)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : ((! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2)) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & (~ (nil = X0) | ~ (nil = X1)) & ! [X9] : (~ segmentP(X0, X9) | ~ segmentP(X1, X9) | ~ neq(X9, nil) | ~ ssList(X9)) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X7] : (lt(X7, X4) & memberP(X6, X7) & ssItem(X7)) | ? [X8] : (lt(X4, X8) & memberP(X5, X8) & ssItem(X8)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | ((nil = X0) & (nil = X1)) | ? [X9] : (segmentP(X0, X9) & segmentP(X1, X9) & neq(X9, nil) & ssList(X9)) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => (? [X9] : (lt(X9, X5) & memberP(X7, X9) & ssItem(X9)) | ? [X8] : (lt(X5, X8) & memberP(X6, X8) & ssItem(X8)) | ~ (app(app(X6, X2), X7) = X3) | ~ (cons(X5, nil) = X2)))))) | ((nil = X0) & (nil = X1)) | ? [X4] : (segmentP(X0, X4) & segmentP(X1, X4) & neq(X4, nil) & ssList(X4)) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => (? [X9] : (lt(X9, X5) & memberP(X7, X9) & ssItem(X9)) | ? [X8] : (lt(X5, X8) & memberP(X6, X8) & ssItem(X8)) | ~ (app(app(X6, X2), X7) = X3) | ~ (cons(X5, nil) = X2)))))) | ((nil = X0) & (nil = X1)) | ? [X4] : (segmentP(X0, X4) & segmentP(X1, X4) & neq(X4, nil) & ssList(X4)) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC071+1.p', co1)).
fof(f6176, plain, (~ spl61_24 | ~ spl61_268), inference(avatar_contradiction_clause, [], [f6175])).
fof(f6175, plain, ($false | (~ spl61_24 | ~ spl61_268)), inference(subsumption_resolution, [], [f6174, f632])).
fof(f632, plain, ssList(sK57), inference(forward_demodulation, [], [f566, f569])).
fof(f566, plain, ssList(sK59), inference(cnf_transformation, [], [f357])).
fof(f6174, plain, (~ ssList(sK57) | (~ spl61_24 | ~ spl61_268)), inference(resolution, [], [f6127, f501])).
fof(f501, plain, ! [X0] : (segmentP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ! [X0] : (segmentP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ! [X0] : (ssList(X0) => segmentP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC071+1.p', ax55)).
fof(f6127, plain, (~ segmentP(sK57, sK57) | (~ spl61_24 | ~ spl61_268)), inference(subsumption_resolution, [], [f6126, f632])).
fof(f6126, plain, (~ segmentP(sK57, sK57) | ~ ssList(sK57) | (~ spl61_24 | ~ spl61_268)), inference(subsumption_resolution, [], [f6122, f1291])).
fof(f1291, plain, (neq(sK57, nil) | ~ spl61_24), inference(avatar_component_clause, [], [f1290])).
fof(f1290, plain, (spl61_24 <=> neq(sK57, nil)), introduced(avatar_definition, [new_symbols(naming, [spl61_24])])).
fof(f6122, plain, (~ segmentP(sK57, sK57) | ~ neq(sK57, nil) | ~ ssList(sK57) | ~ spl61_268), inference(resolution, [], [f5209, f570])).
fof(f570, plain, ! [X4] : (~ segmentP(sK58, X4) | ~ segmentP(sK57, X4) | ~ neq(X4, nil) | ~ ssList(X4)), inference(cnf_transformation, [], [f357])).
fof(f5209, plain, (segmentP(sK58, sK57) | ~ spl61_268), inference(avatar_component_clause, [], [f5207])).
fof(f5207, plain, (spl61_268 <=> segmentP(sK58, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_268])])).
fof(f6079, plain, (~ spl61_1 | spl61_266), inference(avatar_contradiction_clause, [], [f6078])).
fof(f6078, plain, ($false | (~ spl61_1 | spl61_266)), inference(subsumption_resolution, [], [f6077, f1169])).
fof(f1169, plain, (sP6(sK58, sK57) | ~ spl61_1), inference(backward_demodulation, [], [f1168, f569])).
fof(f1168, plain, (sP6(sK58, sK59) | ~ spl61_1), inference(forward_demodulation, [], [f611, f568])).
fof(f568, plain, (sK58 = sK60), inference(cnf_transformation, [], [f357])).
fof(f611, plain, (sP6(sK60, sK59) | ~ spl61_1), inference(avatar_component_clause, [], [f609])).
fof(f609, plain, (spl61_1 <=> sP6(sK60, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_1])])).
fof(f6077, plain, (~ sP6(sK58, sK57) | spl61_266), inference(resolution, [], [f5201, f558])).
fof(f558, plain, ! [X0, X1] : (ssList(sK55(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f351, plain, ! [X0, X1] : ((((! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f347, f350, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) => (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f350, plain, ! [X1, X0] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) => (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(nnf_transformation, [], [f233])).
fof(f5201, plain, (~ ssList(sK55(sK58, sK57)) | spl61_266), inference(avatar_component_clause, [], [f5199])).
fof(f5199, plain, (spl61_266 <=> ssList(sK55(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_266])])).
fof(f6031, plain, (~ spl61_1 | spl61_267), inference(avatar_contradiction_clause, [], [f6030])).
fof(f6030, plain, ($false | (~ spl61_1 | spl61_267)), inference(subsumption_resolution, [], [f6029, f1169])).
fof(f6029, plain, (~ sP6(sK58, sK57) | spl61_267), inference(resolution, [], [f5205, f559])).
fof(f559, plain, ! [X0, X1] : (ssList(sK56(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f5205, plain, (~ ssList(sK56(sK58, sK57)) | spl61_267), inference(avatar_component_clause, [], [f5203])).
fof(f5203, plain, (spl61_267 <=> ssList(sK56(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_267])])).
fof(f5210, plain, (~ spl61_266 | ~ spl61_267 | spl61_268 | ~ spl61_1), inference(avatar_split_clause, [], [f5197, f609, f5207, f5203, f5199])).
fof(f5197, plain, (segmentP(sK58, sK57) | ~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ spl61_1), inference(subsumption_resolution, [], [f5196, f631])).
fof(f631, plain, ssList(sK58), inference(forward_demodulation, [], [f567, f568])).
fof(f567, plain, ssList(sK60), inference(cnf_transformation, [], [f357])).
fof(f5196, plain, (segmentP(sK58, sK57) | ~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK58) | ~ spl61_1), inference(subsumption_resolution, [], [f5175, f632])).
fof(f5175, plain, (segmentP(sK58, sK57) | ~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | ~ spl61_1), inference(superposition, [], [f579, f4288])).
fof(f4288, plain, ((sK58 = app(app(sK55(sK58, sK57), sK57), sK56(sK58, sK57))) | ~ spl61_1), inference(resolution, [], [f1169, f561])).
fof(f561, plain, ! [X0, X1] : (~ sP6(X0, X1) | (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f351])).
fof(f579, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f379])).
fof(f379, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f260])).
fof(f260, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1))) & ssList(sK14(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15])], [f257, f259, f258])).
fof(f258, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f259, plain, ! [X1, X0] : (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1)))), introduced(choice_axiom, [])).
fof(f257, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f256])).
fof(f256, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC071+1.p', ax7)).
fof(f1333, plain, (spl61_5 | spl61_24), inference(avatar_contradiction_clause, [], [f1332])).
fof(f1332, plain, ($false | (spl61_5 | spl61_24)), inference(subsumption_resolution, [], [f1331, f632])).
fof(f1331, plain, (~ ssList(sK57) | (spl61_5 | spl61_24)), inference(subsumption_resolution, [], [f1330, f450])).
fof(f450, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC071+1.p', ax17)).
fof(f1330, plain, (~ ssList(nil) | ~ ssList(sK57) | (spl61_5 | spl61_24)), inference(subsumption_resolution, [], [f1328, f629])).
fof(f629, plain, (~ (nil = sK57) | spl61_5), inference(avatar_component_clause, [], [f627])).
fof(f1328, plain, ((nil = sK57) | ~ ssList(nil) | ~ ssList(sK57) | spl61_24), inference(resolution, [], [f1292, f448])).
fof(f448, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC071+1.p', ax15)).
fof(f1292, plain, (~ neq(sK57, nil) | spl61_24), inference(avatar_component_clause, [], [f1290])).
fof(f1167, plain, (~ spl61_1 | ~ spl61_2), inference(avatar_contradiction_clause, [], [f1166])).
fof(f1166, plain, ($false | (~ spl61_1 | ~ spl61_2)), inference(subsumption_resolution, [], [f1165, f642])).
fof(f642, plain, (sP6(sK58, nil) | (~ spl61_1 | ~ spl61_2)), inference(forward_demodulation, [], [f641, f568])).
fof(f641, plain, (sP6(sK60, nil) | (~ spl61_1 | ~ spl61_2)), inference(forward_demodulation, [], [f611, f615])).
fof(f615, plain, ((nil = sK59) | ~ spl61_2), inference(avatar_component_clause, [], [f613])).
fof(f1165, plain, (~ sP6(sK58, nil) | (~ spl61_1 | ~ spl61_2)), inference(resolution, [], [f1160, f557])).
fof(f557, plain, ! [X0, X1] : (ssItem(sK54(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f1160, plain, (~ ssItem(sK54(sK58, nil)) | (~ spl61_1 | ~ spl61_2)), inference(subsumption_resolution, [], [f1159, f480])).
fof(f480, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (ssItem(X0) => ~ memberP(nil, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC071+1.p', ax38)).
fof(f1159, plain, (memberP(nil, sK54(sK58, nil)) | ~ ssItem(sK54(sK58, nil)) | (~ spl61_1 | ~ spl61_2)), inference(subsumption_resolution, [], [f1153, f450])).
fof(f1153, plain, (memberP(nil, sK54(sK58, nil)) | ~ ssList(nil) | ~ ssItem(sK54(sK58, nil)) | (~ spl61_1 | ~ spl61_2)), inference(superposition, [], [f604, f1145])).
fof(f1145, plain, ((nil = cons(sK54(sK58, nil), nil)) | (~ spl61_1 | ~ spl61_2)), inference(resolution, [], [f560, f642])).
fof(f560, plain, ! [X0, X1] : (~ sP6(X0, X1) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f351])).
fof(f604, plain, ! [X2, X1] : (memberP(cons(X1, X2), X1) | ~ ssList(X2) | ~ ssItem(X1)), inference(duplicate_literal_removal, [], [f589])).
fof(f589, plain, ! [X2, X1] : (memberP(cons(X1, X2), X1) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X1)), inference(equality_resolution, [], [f478])).
fof(f478, plain, ! [X2, X0, X1] : (memberP(cons(X1, X2), X0) | ~ (X0 = X1) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f328])).
fof(f328, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & (memberP(X2, X0) | (X0 = X1) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f327])).
fof(f327, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & ((memberP(X2, X0) | (X0 = X1)) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => (memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC071+1.p', ax37)).
fof(f638, plain, (spl61_5 | ~ spl61_2), inference(avatar_split_clause, [], [f635, f613, f627])).
fof(f635, plain, ((nil = sK57) | ~ spl61_2), inference(backward_demodulation, [], [f569, f615])).
fof(f634, plain, (spl61_4 | ~ spl61_3), inference(avatar_split_clause, [], [f633, f618, f623])).
fof(f623, plain, (spl61_4 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_4])])).
fof(f618, plain, (spl61_3 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl61_3])])).
fof(f633, plain, ((nil = sK58) | ~ spl61_3), inference(backward_demodulation, [], [f568, f620])).
fof(f620, plain, ((nil = sK60) | ~ spl61_3), inference(avatar_component_clause, [], [f618])).
fof(f630, plain, (~ spl61_4 | ~ spl61_5), inference(avatar_split_clause, [], [f571, f627, f623])).
fof(f571, plain, (~ (nil = sK57) | ~ (nil = sK58)), inference(cnf_transformation, [], [f357])).
fof(f621, plain, (spl61_1 | spl61_3), inference(avatar_split_clause, [], [f572, f618, f609])).
fof(f572, plain, ((nil = sK60) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f357])).
fof(f616, plain, (spl61_1 | spl61_2), inference(avatar_split_clause, [], [f573, f613, f609])).
fof(f573, plain, ((nil = sK59) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f357])).