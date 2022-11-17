fof(f4150, plain, $false, inference(avatar_sat_refutation, [], [f610, f616, f3030, f3129, f4115, f4139])).
fof(f4139, plain, (spl60_30 | spl60_108), inference(avatar_contradiction_clause, [], [f4138])).
fof(f4138, plain, ($false | (spl60_30 | spl60_108)), inference(subsumption_resolution, [], [f4137, f613])).
fof(f613, plain, ssList(sK56), inference(forward_demodulation, [], [f562, f565])).
fof(f565, plain, (sK56 = sK58), inference(cnf_transformation, [], [f354])).
fof(f354, plain, ((((((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK56, sK57, sK59, sK58)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK56, sK57, sK58, sK59])], [f233, f353, f352, f351, f350])).
fof(f350, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X0, X1, X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(sK56, X1, X3, X2)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f351, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(sK56, X1, X3, X2)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, sK57, X3, X2)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, sK57, X3, X2)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, sK57, X3, sK58)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, sK57, X3, sK58)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) => (((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK56, sK57, sK59, sK58)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f233, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X0, X1, X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f222, e232])).
fof(f232, plain, ! [X0, X1, X3, X2] : (((~ rearsegP(X1, X0) | ~ neq(X0, nil)) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X1, nil)) | ~ sP6(X0, X1, X3, X2)), inference(usedef, [], [e232])).
fof(e232, plain, ! [X0, X1, X3, X2] : (sP6(X0, X1, X3, X2) <=> ((~ rearsegP(X1, X0) | ~ neq(X0, nil)) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | ((~ rearsegP(X1, X0) | ~ neq(X0, nil)) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f221])).
fof(f221, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | ((~ rearsegP(X1, X0) | ~ neq(X0, nil)) & ? [X4] : (? [X5] : (((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2)) & ssList(X5)) & ssItem(X4)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & ((rearsegP(X1, X0) & neq(X0, nil)) | ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(X5, cons(X4, nil)) = X3) | ~ (cons(X4, nil) = X2)))) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & ((rearsegP(X1, X0) & neq(X0, nil)) | ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(X5, cons(X4, nil)) = X3) | ~ (cons(X4, nil) = X2)))) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC106+1.p', co1)).
fof(f562, plain, ssList(sK58), inference(cnf_transformation, [], [f354])).
fof(f4137, plain, (~ ssList(sK56) | (spl60_30 | spl60_108)), inference(subsumption_resolution, [], [f4136, f447])).
fof(f447, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC106+1.p', ax17)).
fof(f4136, plain, (~ ssList(nil) | ~ ssList(sK56) | (spl60_30 | spl60_108)), inference(subsumption_resolution, [], [f4134, f1162])).
fof(f1162, plain, (~ (nil = sK56) | spl60_30), inference(avatar_component_clause, [], [f1161])).
fof(f1161, plain, (spl60_30 <=> (nil = sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_30])])).
fof(f4134, plain, ((nil = sK56) | ~ ssList(nil) | ~ ssList(sK56) | spl60_108), inference(resolution, [], [f3128, f445])).
fof(f445, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f318])).
fof(f318, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f118])).
fof(f118, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC106+1.p', ax15)).
fof(f3128, plain, (~ neq(sK56, nil) | spl60_108), inference(avatar_component_clause, [], [f3126])).
fof(f3126, plain, (spl60_108 <=> neq(sK56, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_108])])).
fof(f4115, plain, (spl60_107 | ~ spl60_1), inference(avatar_split_clause, [], [f4114, f603, f3122])).
fof(f3122, plain, (spl60_107 <=> rearsegP(sK57, sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_107])])).
fof(f603, plain, (spl60_1 <=> sP6(sK56, sK57, sK59, sK58)), introduced(avatar_definition, [new_symbols(naming, [spl60_1])])).
fof(f4114, plain, (rearsegP(sK57, sK56) | ~ spl60_1), inference(subsumption_resolution, [], [f4113, f612])).
fof(f612, plain, ssList(sK57), inference(forward_demodulation, [], [f563, f564])).
fof(f564, plain, (sK57 = sK59), inference(cnf_transformation, [], [f354])).
fof(f563, plain, ssList(sK59), inference(cnf_transformation, [], [f354])).
fof(f4113, plain, (rearsegP(sK57, sK56) | ~ ssList(sK57) | ~ spl60_1), inference(subsumption_resolution, [], [f4112, f613])).
fof(f4112, plain, (rearsegP(sK57, sK56) | ~ ssList(sK56) | ~ ssList(sK57) | ~ spl60_1), inference(subsumption_resolution, [], [f4084, f987])).
fof(f987, plain, (ssList(sK55(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f556, f619])).
fof(f619, plain, (sP6(sK56, sK57, sK57, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f618, f564])).
fof(f618, plain, (sP6(sK56, sK57, sK59, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f605, f565])).
fof(f605, plain, (sP6(sK56, sK57, sK59, sK58) | ~ spl60_1), inference(avatar_component_clause, [], [f603])).
fof(f556, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK55(X2, X3))), inference(cnf_transformation, [], [f349])).
fof(f349, plain, ! [X0, X1, X2, X3] : (((~ rearsegP(X1, X0) | ~ neq(X0, nil)) & (((app(sK55(X2, X3), cons(sK54(X2, X3), nil)) = X2) & (cons(sK54(X2, X3), nil) = X3) & ssList(sK55(X2, X3))) & ssItem(sK54(X2, X3))) & neq(X1, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55])], [f346, f348, f347])).
fof(f347, plain, ! [X3, X2] : (? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X2) & (cons(X4, nil) = X3) & ssList(X5)) & ssItem(X4)) => (? [X5] : ((app(X5, cons(sK54(X2, X3), nil)) = X2) & (cons(sK54(X2, X3), nil) = X3) & ssList(X5)) & ssItem(sK54(X2, X3)))), introduced(choice_axiom, [])).
fof(f348, plain, ! [X3, X2] : (? [X5] : ((app(X5, cons(sK54(X2, X3), nil)) = X2) & (cons(sK54(X2, X3), nil) = X3) & ssList(X5)) => ((app(sK55(X2, X3), cons(sK54(X2, X3), nil)) = X2) & (cons(sK54(X2, X3), nil) = X3) & ssList(sK55(X2, X3)))), introduced(choice_axiom, [])).
fof(f346, plain, ! [X0, X1, X2, X3] : (((~ rearsegP(X1, X0) | ~ neq(X0, nil)) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X2) & (cons(X4, nil) = X3) & ssList(X5)) & ssItem(X4)) & neq(X1, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f345])).
fof(f345, plain, ! [X0, X1, X3, X2] : (((~ rearsegP(X1, X0) | ~ neq(X0, nil)) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X1, nil)) | ~ sP6(X0, X1, X3, X2)), inference(nnf_transformation, [], [f232])).
fof(f4084, plain, (rearsegP(sK57, sK56) | ~ ssList(sK55(sK57, sK56)) | ~ ssList(sK56) | ~ ssList(sK57) | ~ spl60_1), inference(superposition, [], [f572, f3120])).
fof(f3120, plain, ((sK57 = app(sK55(sK57, sK56), sK56)) | ~ spl60_1), inference(backward_demodulation, [], [f3114, f3115])).
fof(f3115, plain, ((sK56 = cons(sK54(sK57, sK56), nil)) | ~ spl60_1), inference(resolution, [], [f619, f557])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (cons(sK54(X2, X3), nil) = X3)), inference(cnf_transformation, [], [f349])).
fof(f3114, plain, ((sK57 = app(sK55(sK57, sK56), cons(sK54(sK57, sK56), nil))) | ~ spl60_1), inference(resolution, [], [f619, f558])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(sK55(X2, X3), cons(sK54(X2, X3), nil)) = X2)), inference(cnf_transformation, [], [f349])).
fof(f572, plain, ! [X2, X1] : (rearsegP(app(X2, X1), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X2, X1))), inference(equality_resolution, [], [f372])).
fof(f372, plain, ! [X2, X0, X1] : (rearsegP(X0, X1) | ~ (app(X2, X1) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f254])).
fof(f254, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (((app(sK13(X0, X1), X1) = X0) & ssList(sK13(X0, X1))) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f252, f253])).
fof(f253, plain, ! [X1, X0] : (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) => ((app(sK13(X0, X1), X1) = X0) & ssList(sK13(X0, X1)))), introduced(choice_axiom, [])).
fof(f252, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f251])).
fof(f251, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X2, X1) = X0) & ssList(X2)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (! [X1] : ((rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC106+1.p', ax6)).
fof(f3129, plain, (~ spl60_107 | ~ spl60_108 | ~ spl60_1), inference(avatar_split_clause, [], [f3116, f603, f3126, f3122])).
fof(f3116, plain, (~ neq(sK56, nil) | ~ rearsegP(sK57, sK56) | ~ spl60_1), inference(resolution, [], [f619, f559])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ~ neq(X0, nil) | ~ rearsegP(X1, X0)), inference(cnf_transformation, [], [f349])).
fof(f3030, plain, (~ spl60_1 | ~ spl60_30), inference(avatar_contradiction_clause, [], [f3029])).
fof(f3029, plain, ($false | (~ spl60_1 | ~ spl60_30)), inference(subsumption_resolution, [], [f3028, f447])).
fof(f3028, plain, (~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(subsumption_resolution, [], [f3016, f1423])).
fof(f1423, plain, (ssItem(sK54(sK57, nil)) | (~ spl60_1 | ~ spl60_30)), inference(forward_demodulation, [], [f986, f1163])).
fof(f1163, plain, ((nil = sK56) | ~ spl60_30), inference(avatar_component_clause, [], [f1161])).
fof(f986, plain, (ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f555, f619])).
fof(f555, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X2, X3))), inference(cnf_transformation, [], [f349])).
fof(f3016, plain, (~ ssItem(sK54(sK57, nil)) | ~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(trivial_inequality_removal, [], [f2994])).
fof(f2994, plain, (~ (nil = nil) | ~ ssItem(sK54(sK57, nil)) | ~ ssList(nil) | (~ spl60_1 | ~ spl60_30)), inference(superposition, [], [f454, f1603])).
fof(f1603, plain, ((nil = cons(sK54(sK57, nil), nil)) | (~ spl60_1 | ~ spl60_30)), inference(resolution, [], [f557, f1424])).
fof(f1424, plain, (sP6(nil, sK57, sK57, nil) | (~ spl60_1 | ~ spl60_30)), inference(forward_demodulation, [], [f619, f1163])).
fof(f454, plain, ! [X0, X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f125])).
fof(f125, plain, ! [X0] : (! [X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (nil = cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC106+1.p', ax21)).
fof(f616, plain, spl60_2, inference(avatar_contradiction_clause, [], [f615])).
fof(f615, plain, ($false | spl60_2), inference(subsumption_resolution, [], [f614, f611])).
fof(f611, plain, neq(sK57, nil), inference(subsumption_resolution, [], [f566, f554])).
fof(f554, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X1, nil)), inference(cnf_transformation, [], [f349])).
fof(f566, plain, (neq(sK57, nil) | sP6(sK56, sK57, sK59, sK58)), inference(cnf_transformation, [], [f354])).
fof(f614, plain, (~ neq(sK57, nil) | spl60_2), inference(forward_demodulation, [], [f609, f564])).
fof(f609, plain, (~ neq(sK59, nil) | spl60_2), inference(avatar_component_clause, [], [f607])).
fof(f607, plain, (spl60_2 <=> neq(sK59, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_2])])).
fof(f610, plain, (spl60_1 | ~ spl60_2), inference(avatar_split_clause, [], [f567, f607, f603])).
fof(f567, plain, (~ neq(sK59, nil) | sP6(sK56, sK57, sK59, sK58)), inference(cnf_transformation, [], [f354])).