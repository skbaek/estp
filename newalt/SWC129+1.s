fof(f1006, plain, $false, inference(avatar_sat_refutation, [], [f976, f996])).
fof(f996, plain, ~ spl57_17, inference(avatar_contradiction_clause, [], [f995])).
fof(f995, plain, ($false | ~ spl57_17), inference(subsumption_resolution, [], [f984, f640])).
fof(f640, plain, sP0(nil), inference(subsumption_resolution, [], [f630, f497])).
fof(f497, plain, cyclefreeP(nil), inference(cnf_transformation, [], [f60])).
fof(f60, plain, cyclefreeP(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC129+1.p', ax60)).
fof(f630, plain, (~ cyclefreeP(nil) | sP0(nil)), inference(resolution, [], [f370, f594])).
fof(f594, plain, sP1(nil), inference(resolution, [], [f381, f440])).
fof(f440, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC129+1.p', ax17)).
fof(f381, plain, ! [X0] : (~ ssList(X0) | sP1(X0)), inference(cnf_transformation, [], [f225])).
fof(f225, plain, ! [X0] : (sP1(X0) | ~ ssList(X0)), inference(definition_folding, [], [f105, e224, e223])).
fof(f223, plain, ! [X0] : (sP0(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))), inference(usedef, [], [e223])).
fof(e223, plain, ! [X0] : (sP0(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f224, plain, ! [X0] : ((cyclefreeP(X0) <=> sP0(X0)) | ~ sP1(X0)), inference(usedef, [], [e224])).
fof(e224, plain, ! [X0] : (sP1(X0) <=> (cyclefreeP(X0) <=> sP0(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f105, plain, ! [X0] : ((cyclefreeP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(flattening, [], [f104])).
fof(f104, plain, ! [X0] : ((cyclefreeP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (((~ leq(X2, X1) | ~ leq(X1, X2)) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0)) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0] : (ssList(X0) => (cyclefreeP(X0) <=> ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssList(X3) => ! [X4] : (ssList(X4) => ! [X5] : (ssList(X5) => ((app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) => ~ (leq(X2, X1) & leq(X1, X2)))))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC129+1.p', ax8)).
fof(f370, plain, ! [X0] : (~ sP1(X0) | ~ cyclefreeP(X0) | sP0(X0)), inference(cnf_transformation, [], [f258])).
fof(f258, plain, ! [X0] : (((cyclefreeP(X0) | ~ sP0(X0)) & (sP0(X0) | ~ cyclefreeP(X0))) | ~ sP1(X0)), inference(nnf_transformation, [], [f224])).
fof(f984, plain, (~ sP0(nil) | ~ spl57_17), inference(backward_demodulation, [], [f644, f965])).
fof(f965, plain, ((nil = sK54) | ~ spl57_17), inference(avatar_component_clause, [], [f963])).
fof(f963, plain, (spl57_17 <=> (nil = sK54)), introduced(avatar_definition, [new_symbols(naming, [spl57_17])])).
fof(f644, plain, ~ sP0(sK54), inference(subsumption_resolution, [], [f641, f554])).
fof(f554, plain, ~ cyclefreeP(sK54), inference(cnf_transformation, [], [f347])).
fof(f347, plain, ((((~ cyclefreeP(sK54) & ~ neq(sK56, nil) & (sK53 = sK55) & (sK54 = sK56) & ssList(sK56)) & ssList(sK55)) & ssList(sK54)) & ssList(sK53)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK53, sK54, sK55, sK56])], [f222, f346, f345, f344, f343])).
fof(f343, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (~ cyclefreeP(X1) & ~ neq(X3, nil) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (~ cyclefreeP(X1) & ~ neq(X3, nil) & (sK53 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK53))), introduced(choice_axiom, [])).
fof(f344, plain, (? [X1] : (? [X2] : (? [X3] : (~ cyclefreeP(X1) & ~ neq(X3, nil) & (sK53 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (~ cyclefreeP(sK54) & ~ neq(X3, nil) & (sK53 = X2) & (sK54 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK54))), introduced(choice_axiom, [])).
fof(f345, plain, (? [X2] : (? [X3] : (~ cyclefreeP(sK54) & ~ neq(X3, nil) & (sK53 = X2) & (sK54 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (~ cyclefreeP(sK54) & ~ neq(X3, nil) & (sK53 = sK55) & (sK54 = X3) & ssList(X3)) & ssList(sK55))), introduced(choice_axiom, [])).
fof(f346, plain, (? [X3] : (~ cyclefreeP(sK54) & ~ neq(X3, nil) & (sK53 = sK55) & (sK54 = X3) & ssList(X3)) => (~ cyclefreeP(sK54) & ~ neq(sK56, nil) & (sK53 = sK55) & (sK54 = sK56) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (~ cyclefreeP(X1) & ~ neq(X3, nil) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f221])).
fof(f221, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((~ cyclefreeP(X1) & ~ neq(X3, nil) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (cyclefreeP(X1) | neq(X3, nil) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (cyclefreeP(X1) | neq(X3, nil) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC129+1.p', co1)).
fof(f641, plain, (~ sP0(sK54) | cyclefreeP(sK54)), inference(resolution, [], [f371, f592])).
fof(f592, plain, sP1(sK54), inference(resolution, [], [f381, f590])).
fof(f590, plain, ssList(sK54), inference(forward_demodulation, [], [f550, f551])).
fof(f551, plain, (sK54 = sK56), inference(cnf_transformation, [], [f347])).
fof(f550, plain, ssList(sK56), inference(cnf_transformation, [], [f347])).
fof(f371, plain, ! [X0] : (~ sP1(X0) | ~ sP0(X0) | cyclefreeP(X0)), inference(cnf_transformation, [], [f258])).
fof(f976, plain, spl57_17, inference(avatar_split_clause, [], [f975, f963])).
fof(f975, plain, (nil = sK54), inference(subsumption_resolution, [], [f974, f590])).
fof(f974, plain, ((nil = sK54) | ~ ssList(sK54)), inference(subsumption_resolution, [], [f969, f440])).
fof(f969, plain, ((nil = sK54) | ~ ssList(nil) | ~ ssList(sK54)), inference(resolution, [], [f438, f589])).
fof(f589, plain, ~ neq(sK54, nil), inference(backward_demodulation, [], [f553, f551])).
fof(f553, plain, ~ neq(sK56, nil), inference(cnf_transformation, [], [f347])).
fof(f438, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f316])).
fof(f316, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f118])).
fof(f118, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC129+1.p', ax15)).