fof(f105081, plain, $false, inference(avatar_sat_refutation, [], [f612, f618, f3961, f105080])).
fof(f105080, plain, (~ spl60_1 | ~ spl60_192), inference(avatar_contradiction_clause, [], [f105079])).
fof(f105079, plain, ($false | (~ spl60_1 | ~ spl60_192)), inference(resolution, [], [f57195, f621])).
fof(f621, plain, (sP6(sK57, sK56, sK57, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f620, f564])).
fof(f564, plain, (sK57 = sK59), inference(cnf_transformation, [], [f354])).
fof(f354, plain, ((((((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK59, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK56, sK57, sK58, sK59])], [f233, f353, f352, f351, f350])).
fof(f350, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, X0)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, sK56)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f351, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, sK56)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, X2, sK57, sK56)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, X2, sK57, sK56)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X3, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) => (((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK59, sK58, sK57, sK56)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f233, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X1, X0)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f222, e232])).
fof(f232, plain, ! [X3, X2, X1, X0] : ((? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ (app(X7, cons(X6, nil)) = X1) | ~ (cons(X6, nil) = X0) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil)) | ~ sP6(X3, X2, X1, X0)), inference(usedef, [], [e232])).
fof(e232, plain, ! [X3, X2, X1, X0] : (sP6(X3, X2, X1, X0) <=> (? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ (app(X7, cons(X6, nil)) = X1) | ~ (cons(X6, nil) = X0) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ (app(X7, cons(X6, nil)) = X1) | ~ (cons(X6, nil) = X0) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X4] : (ssItem(X4) => ! [X5] : (~ (app(X5, cons(X4, nil)) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X5))) | ? [X6] : (? [X7] : ((app(X7, cons(X6, nil)) = X1) & (cons(X6, nil) = X0) & ssList(X7)) & ssItem(X6)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X6] : (ssItem(X6) => ! [X7] : (~ (app(X7, cons(X6, nil)) = X3) | ~ (cons(X6, nil) = X2) | ~ ssList(X7))) | ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X1) & (cons(X4, nil) = X0) & ssList(X5)) & ssItem(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X6] : (ssItem(X6) => ! [X7] : (~ (app(X7, cons(X6, nil)) = X3) | ~ (cons(X6, nil) = X2) | ~ ssList(X7))) | ? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X1) & (cons(X4, nil) = X0) & ssList(X5)) & ssItem(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC096+1.p', co1)).
fof(f620, plain, (sP6(sK59, sK56, sK57, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f607, f565])).
fof(f565, plain, (sK56 = sK58), inference(cnf_transformation, [], [f354])).
fof(f607, plain, (sP6(sK59, sK58, sK57, sK56) | ~ spl60_1), inference(avatar_component_clause, [], [f605])).
fof(f605, plain, (spl60_1 <=> sP6(sK59, sK58, sK57, sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_1])])).
fof(f57195, plain, (! [X0, X1] : ~ sP6(X0, X1, sK57, sK56) | (~ spl60_1 | ~ spl60_192)), inference(subsumption_resolution, [], [f57156, f989])).
fof(f989, plain, (ssList(sK55(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f557, f621])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK55(X0, X1))), inference(cnf_transformation, [], [f349])).
fof(f349, plain, ! [X0, X1, X2, X3] : (((((app(sK55(X0, X1), cons(sK54(X0, X1), nil)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) & ! [X6] : (! [X7] : (~ (app(X7, cons(X6, nil)) = X2) | ~ (cons(X6, nil) = X3) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X2, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55])], [f346, f348, f347])).
fof(f347, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X0) & (cons(X4, nil) = X1) & ssList(X5)) & ssItem(X4)) => (? [X5] : ((app(X5, cons(sK54(X0, X1), nil)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X5)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f348, plain, ! [X1, X0] : (? [X5] : ((app(X5, cons(sK54(X0, X1), nil)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X5)) => ((app(sK55(X0, X1), cons(sK54(X0, X1), nil)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f346, plain, ! [X0, X1, X2, X3] : ((? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X0) & (cons(X4, nil) = X1) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ (app(X7, cons(X6, nil)) = X2) | ~ (cons(X6, nil) = X3) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X2, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f345])).
fof(f345, plain, ! [X3, X2, X1, X0] : ((? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X3) & (cons(X4, nil) = X2) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (~ (app(X7, cons(X6, nil)) = X1) | ~ (cons(X6, nil) = X0) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil)) | ~ sP6(X3, X2, X1, X0)), inference(nnf_transformation, [], [f232])).
fof(f57156, plain, (! [X0, X1] : (~ sP6(X0, X1, sK57, sK56) | ~ ssList(sK55(sK57, sK56))) | (~ spl60_1 | ~ spl60_192)), inference(superposition, [], [f3825, f3035])).
fof(f3035, plain, ((sK57 = app(sK55(sK57, sK56), sK56)) | ~ spl60_1), inference(backward_demodulation, [], [f3030, f3031])).
fof(f3031, plain, ((sK56 = cons(sK54(sK57, sK56), nil)) | ~ spl60_1), inference(resolution, [], [f621, f558])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f349])).
fof(f3030, plain, ((sK57 = app(sK55(sK57, sK56), cons(sK54(sK57, sK56), nil))) | ~ spl60_1), inference(resolution, [], [f621, f559])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(sK55(X0, X1), cons(sK54(X0, X1), nil)) = X0)), inference(cnf_transformation, [], [f349])).
fof(f3825, plain, (! [X2, X0, X1] : (~ sP6(X0, X1, app(X2, sK56), sK56) | ~ ssList(X2)) | ~ spl60_192), inference(avatar_component_clause, [], [f3824])).
fof(f3824, plain, (spl60_192 <=> ! [X1, X0, X2] : (~ sP6(X0, X1, app(X2, sK56), sK56) | ~ ssList(X2))), introduced(avatar_definition, [new_symbols(naming, [spl60_192])])).
fof(f3961, plain, (spl60_192 | ~ spl60_1), inference(avatar_split_clause, [], [f3960, f605, f3824])).
fof(f3960, plain, (! [X2, X0, X1] : (~ sP6(X0, X1, app(X2, sK56), sK56) | ~ ssList(X2)) | ~ spl60_1), inference(subsumption_resolution, [], [f3915, f988])).
fof(f988, plain, (ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f556, f621])).
fof(f556, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X0, X1))), inference(cnf_transformation, [], [f349])).
fof(f3915, plain, (! [X2, X0, X1] : (~ sP6(X0, X1, app(X2, sK56), sK56) | ~ ssItem(sK54(sK57, sK56)) | ~ ssList(X2)) | ~ spl60_1), inference(superposition, [], [f596, f3031])).
fof(f596, plain, ! [X6, X0, X7, X1] : (~ sP6(X0, X1, app(X7, cons(X6, nil)), cons(X6, nil)) | ~ ssItem(X6) | ~ ssList(X7)), inference(equality_resolution, [], [f595])).
fof(f595, plain, ! [X6, X0, X7, X3, X1] : (~ (cons(X6, nil) = X3) | ~ ssList(X7) | ~ ssItem(X6) | ~ sP6(X0, X1, app(X7, cons(X6, nil)), X3)), inference(equality_resolution, [], [f555])).
fof(f555, plain, ! [X6, X2, X0, X7, X3, X1] : (~ (app(X7, cons(X6, nil)) = X2) | ~ (cons(X6, nil) = X3) | ~ ssList(X7) | ~ ssItem(X6) | ~ sP6(X0, X1, X2, X3)), inference(cnf_transformation, [], [f349])).
fof(f618, plain, spl60_2, inference(avatar_contradiction_clause, [], [f617])).
fof(f617, plain, ($false | spl60_2), inference(subsumption_resolution, [], [f616, f613])).
fof(f613, plain, neq(sK57, nil), inference(subsumption_resolution, [], [f566, f554])).
fof(f554, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X2, nil)), inference(cnf_transformation, [], [f349])).
fof(f566, plain, (neq(sK57, nil) | sP6(sK59, sK58, sK57, sK56)), inference(cnf_transformation, [], [f354])).
fof(f616, plain, (~ neq(sK57, nil) | spl60_2), inference(forward_demodulation, [], [f611, f564])).
fof(f611, plain, (~ neq(sK59, nil) | spl60_2), inference(avatar_component_clause, [], [f609])).
fof(f609, plain, (spl60_2 <=> neq(sK59, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_2])])).
fof(f612, plain, (spl60_1 | ~ spl60_2), inference(avatar_split_clause, [], [f567, f609, f605])).
fof(f567, plain, (~ neq(sK59, nil) | sP6(sK59, sK58, sK57, sK56)), inference(cnf_transformation, [], [f354])).