fof(f4106, plain, $false, inference(avatar_sat_refutation, [], [f610, f616, f4052])).
fof(f4052, plain, ~ spl60_1, inference(avatar_contradiction_clause, [], [f4051])).
fof(f4051, plain, ($false | ~ spl60_1), inference(subsumption_resolution, [], [f4050, f987])).
fof(f987, plain, (ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f555, f619])).
fof(f619, plain, (sP6(sK56, sK57, sK56, sK57) | ~ spl60_1), inference(forward_demodulation, [], [f618, f564])).
fof(f564, plain, (sK57 = sK59), inference(cnf_transformation, [], [f354])).
fof(f354, plain, ((((((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK56, sK59, sK58, sK57)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK56, sK57, sK58, sK59])], [f233, f353, f352, f351, f350])).
fof(f350, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X0, X3, X2, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(sK56, X3, X2, X1)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f351, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(sK56, X3, X2, X1)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, X3, X2, sK57)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, X3, X2, sK57)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, X3, sK58, sK57)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, X3, sK58, sK57)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) => (((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK56, sK59, sK58, sK57)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f233, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X0, X3, X2, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f222, e232])).
fof(f232, plain, ! [X0, X3, X2, X1] : ((~ singletonP(X0) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X1, nil)) | ~ sP6(X0, X3, X2, X1)), inference(usedef, [], [e232])).
fof(e232, plain, ! [X0, X3, X2, X1] : (sP6(X0, X3, X2, X1) <=> (~ singletonP(X0) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (~ singletonP(X0) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f221])).
fof(f221, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | (~ singletonP(X0) & ? [X4] : (? [X5] : (((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2)) & ssList(X5)) & ssItem(X4)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (singletonP(X0) | ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(X5, cons(X4, nil)) = X3) | ~ (cons(X4, nil) = X2)))) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (singletonP(X0) | ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(X5, cons(X4, nil)) = X3) | ~ (cons(X4, nil) = X2)))) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC254+1.p', co1)).
fof(f618, plain, (sP6(sK56, sK59, sK56, sK57) | ~ spl60_1), inference(forward_demodulation, [], [f605, f565])).
fof(f565, plain, (sK56 = sK58), inference(cnf_transformation, [], [f354])).
fof(f605, plain, (sP6(sK56, sK59, sK58, sK57) | ~ spl60_1), inference(avatar_component_clause, [], [f603])).
fof(f603, plain, (spl60_1 <=> sP6(sK56, sK59, sK58, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_1])])).
fof(f555, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X1, X2))), inference(cnf_transformation, [], [f349])).
fof(f349, plain, ! [X0, X1, X2, X3] : ((~ singletonP(X0) & (((app(sK55(X1, X2), cons(sK54(X1, X2), nil)) = X1) & (cons(sK54(X1, X2), nil) = X2) & ssList(sK55(X1, X2))) & ssItem(sK54(X1, X2))) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55])], [f346, f348, f347])).
fof(f347, plain, ! [X2, X1] : (? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X1) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) => (? [X5] : ((app(X5, cons(sK54(X1, X2), nil)) = X1) & (cons(sK54(X1, X2), nil) = X2) & ssList(X5)) & ssItem(sK54(X1, X2)))), introduced(choice_axiom, [])).
fof(f348, plain, ! [X2, X1] : (? [X5] : ((app(X5, cons(sK54(X1, X2), nil)) = X1) & (cons(sK54(X1, X2), nil) = X2) & ssList(X5)) => ((app(sK55(X1, X2), cons(sK54(X1, X2), nil)) = X1) & (cons(sK54(X1, X2), nil) = X2) & ssList(sK55(X1, X2)))), introduced(choice_axiom, [])).
fof(f346, plain, ! [X0, X1, X2, X3] : ((~ singletonP(X0) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X1) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f345])).
fof(f345, plain, ! [X0, X3, X2, X1] : ((~ singletonP(X0) & ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & neq(X1, nil)) | ~ sP6(X0, X3, X2, X1)), inference(nnf_transformation, [], [f232])).
fof(f4050, plain, (~ ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(subsumption_resolution, [], [f4005, f870])).
fof(f870, plain, (~ singletonP(sK56) | ~ spl60_1), inference(resolution, [], [f559, f619])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ~ singletonP(X0)), inference(cnf_transformation, [], [f349])).
fof(f4005, plain, (singletonP(sK56) | ~ ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(superposition, [], [f1450, f3220])).
fof(f3220, plain, ((sK56 = cons(sK54(sK57, sK56), nil)) | ~ spl60_1), inference(resolution, [], [f619, f557])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (cons(sK54(X1, X2), nil) = X2)), inference(cnf_transformation, [], [f349])).
fof(f1450, plain, ! [X0] : (singletonP(cons(X0, nil)) | ~ ssItem(X0)), inference(subsumption_resolution, [], [f1449, f447])).
fof(f447, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC254+1.p', ax17)).
fof(f1449, plain, ! [X0] : (~ ssItem(X0) | singletonP(cons(X0, nil)) | ~ ssList(nil)), inference(duplicate_literal_removal, [], [f1448])).
fof(f1448, plain, ! [X0] : (~ ssItem(X0) | singletonP(cons(X0, nil)) | ~ ssItem(X0) | ~ ssList(nil)), inference(resolution, [], [f570, f446])).
fof(f446, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC254+1.p', ax16)).
fof(f570, plain, ! [X1] : (~ ssList(cons(X1, nil)) | ~ ssItem(X1) | singletonP(cons(X1, nil))), inference(equality_resolution, [], [f366])).
fof(f366, plain, ! [X0, X1] : (singletonP(X0) | ~ (cons(X1, nil) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f246])).
fof(f246, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0))) | ~ singletonP(X0))) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f244, f245])).
fof(f245, plain, ! [X0] : (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) => ((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0)))), introduced(choice_axiom, [])).
fof(f244, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(rectify, [], [f243])).
fof(f243, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f100])).
fof(f100, plain, ! [X0] : ((singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : (ssList(X0) => (singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC254+1.p', ax4)).
fof(f616, plain, spl60_2, inference(avatar_contradiction_clause, [], [f615])).
fof(f615, plain, ($false | spl60_2), inference(subsumption_resolution, [], [f614, f611])).
fof(f611, plain, neq(sK57, nil), inference(subsumption_resolution, [], [f566, f554])).
fof(f554, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X3, nil)), inference(cnf_transformation, [], [f349])).
fof(f566, plain, (neq(sK57, nil) | sP6(sK56, sK59, sK58, sK57)), inference(cnf_transformation, [], [f354])).
fof(f614, plain, (~ neq(sK57, nil) | spl60_2), inference(forward_demodulation, [], [f609, f564])).
fof(f609, plain, (~ neq(sK59, nil) | spl60_2), inference(avatar_component_clause, [], [f607])).
fof(f607, plain, (spl60_2 <=> neq(sK59, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_2])])).
fof(f610, plain, (spl60_1 | ~ spl60_2), inference(avatar_split_clause, [], [f567, f607, f603])).
fof(f567, plain, (~ neq(sK59, nil) | sP6(sK56, sK59, sK58, sK57)), inference(cnf_transformation, [], [f354])).