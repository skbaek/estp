fof(f56971, plain, $false, inference(avatar_sat_refutation, [], [f615, f621, f1889, f2560, f2646, f2773, f13405, f14101, f41078, f48281, f48299, f54426, f54741])).
fof(f54741, plain, (spl61_323 | ~ spl61_1 | ~ spl61_709), inference(avatar_split_clause, [], [f52317, f14327, f608, f6689])).
fof(f6689, plain, (spl61_323 <=> (sK57 = tl(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_323])])).
fof(f608, plain, (spl61_1 <=> sP6(sK59, sK60, sK57, sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_1])])).
fof(f14327, plain, (spl61_709 <=> (nil = cons(sK54(sK57, sK58), nil))), introduced(avatar_definition, [new_symbols(naming, [spl61_709])])).
fof(f52317, plain, ((sK57 = tl(sK57)) | (~ spl61_1 | ~ spl61_709)), inference(backward_demodulation, [], [f50711, f52315])).
fof(f52315, plain, ((sK57 = cons(sK54(sK57, sK57), sK57)) | (~ spl61_1 | ~ spl61_709)), inference(forward_demodulation, [], [f52314, f850])).
fof(f850, plain, (sK57 = app(nil, sK57)), inference(resolution, [], [f463, f618])).
fof(f618, plain, ssList(sK57), inference(forward_demodulation, [], [f565, f568])).
fof(f568, plain, (sK57 = sK59), inference(cnf_transformation, [], [f356])).
fof(f356, plain, ((((((~ neq(sK60, nil) & neq(sK58, nil)) | sP6(sK59, sK60, sK57, sK58)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60])], [f234, f355, f354, f353, f352])).
fof(f352, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, X0, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, sK57, X1)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, sK57, X1)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK58, nil)) | sP6(X2, X3, sK57, sK58)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK58, nil)) | sP6(X2, X3, sK57, sK58)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK58, nil)) | sP6(sK59, X3, sK57, sK58)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f355, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK58, nil)) | sP6(sK59, X3, sK57, sK58)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => (((~ neq(sK60, nil) & neq(sK58, nil)) | sP6(sK59, sK60, sK57, sK58)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, X0, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X2, X3, X0, X1] : ((? [X4] : (? [X5] : (? [X6] : ((app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X7] : (! [X8] : (! [X9] : (~ neq(X8, nil) | ~ (app(X7, X9) = X0) | ~ (app(app(X7, X8), X9) = X1) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssList(X7)) & neq(X1, nil)) | ~ sP6(X2, X3, X0, X1)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X2, X3, X0, X1] : (sP6(X2, X3, X0, X1) <=> (? [X4] : (? [X5] : (? [X6] : ((app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X7] : (! [X8] : (! [X9] : (~ neq(X8, nil) | ~ (app(X7, X9) = X0) | ~ (app(app(X7, X8), X9) = X1) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssList(X7)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : (? [X6] : ((app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X7] : (! [X8] : (! [X9] : (~ neq(X8, nil) | ~ (app(X7, X9) = X0) | ~ (app(app(X7, X8), X9) = X1) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssList(X7)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : (? [X6] : (((app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3)) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X7] : (! [X8] : (! [X9] : (~ neq(X8, nil) | ~ (app(X7, X9) = X0) | ~ (app(app(X7, X8), X9) = X1) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssList(X7)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (~ (app(X5, X6) = X2) | ~ (app(app(X5, cons(X4, nil)), X6) = X3))))) | ? [X7] : (? [X8] : (? [X9] : (neq(X8, nil) & (app(X7, X9) = X0) & (app(app(X7, X8), X9) = X1) & ssList(X9)) & ssList(X8)) & ssList(X7)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => ! [X9] : (ssList(X9) => (~ (app(X8, X9) = X2) | ~ (app(app(X8, cons(X7, nil)), X9) = X3))))) | ? [X4] : (? [X5] : (? [X6] : (neq(X5, nil) & (app(X4, X6) = X0) & (app(app(X4, X5), X6) = X1) & ssList(X6)) & ssList(X5)) & ssList(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => ! [X9] : (ssList(X9) => (~ (app(X8, X9) = X2) | ~ (app(app(X8, cons(X7, nil)), X9) = X3))))) | ? [X4] : (? [X5] : (? [X6] : (neq(X5, nil) & (app(X4, X6) = X0) & (app(app(X4, X5), X6) = X1) & ssList(X6)) & ssList(X5)) & ssList(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', co1)).
fof(f565, plain, ssList(sK59), inference(cnf_transformation, [], [f356])).
fof(f463, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax28)).
fof(f52314, plain, ((app(nil, sK57) = cons(sK54(sK57, sK57), sK57)) | (~ spl61_1 | ~ spl61_709)), inference(forward_demodulation, [], [f50325, f50454])).
fof(f50454, plain, ((sK57 = sK58) | (~ spl61_1 | ~ spl61_709)), inference(forward_demodulation, [], [f50453, f5262])).
fof(f5262, plain, ((sK57 = app(sK55(sK57, sK58), sK56(sK57, sK58))) | ~ spl61_1), inference(resolution, [], [f624, f562])).
fof(f562, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(sK55(X0, X1), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f351])).
fof(f351, plain, ! [X0, X1, X2, X3] : ((((((app(sK55(X0, X1), sK56(X0, X1)) = X0) & (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), sK56(X0, X1)) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) & ! [X7] : (! [X8] : (! [X9] : (~ neq(X8, nil) | ~ (app(X7, X9) = X2) | ~ (app(app(X7, X8), X9) = X3) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssList(X7)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f347, f350, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X4] : (? [X5] : (? [X6] : ((app(X5, X6) = X0) & (app(app(X5, cons(X4, nil)), X6) = X1) & ssList(X6)) & ssList(X5)) & ssItem(X4)) => (? [X5] : (? [X6] : ((app(X5, X6) = X0) & (app(app(X5, cons(sK54(X0, X1), nil)), X6) = X1) & ssList(X6)) & ssList(X5)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X5] : (? [X6] : ((app(X5, X6) = X0) & (app(app(X5, cons(sK54(X0, X1), nil)), X6) = X1) & ssList(X6)) & ssList(X5)) => (? [X6] : ((app(sK55(X0, X1), X6) = X0) & (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), X6) = X1) & ssList(X6)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f350, plain, ! [X1, X0] : (? [X6] : ((app(sK55(X0, X1), X6) = X0) & (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), X6) = X1) & ssList(X6)) => ((app(sK55(X0, X1), sK56(X0, X1)) = X0) & (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), sK56(X0, X1)) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1, X2, X3] : ((? [X4] : (? [X5] : (? [X6] : ((app(X5, X6) = X0) & (app(app(X5, cons(X4, nil)), X6) = X1) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X7] : (! [X8] : (! [X9] : (~ neq(X8, nil) | ~ (app(X7, X9) = X2) | ~ (app(app(X7, X8), X9) = X3) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssList(X7)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X2, X3, X0, X1] : ((? [X4] : (? [X5] : (? [X6] : ((app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X7] : (! [X8] : (! [X9] : (~ neq(X8, nil) | ~ (app(X7, X9) = X0) | ~ (app(app(X7, X8), X9) = X1) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssList(X7)) & neq(X1, nil)) | ~ sP6(X2, X3, X0, X1)), inference(nnf_transformation, [], [f233])).
fof(f624, plain, (sP6(sK57, sK58, sK57, sK58) | ~ spl61_1), inference(forward_demodulation, [], [f623, f568])).
fof(f623, plain, (sP6(sK59, sK58, sK57, sK58) | ~ spl61_1), inference(forward_demodulation, [], [f610, f567])).
fof(f567, plain, (sK58 = sK60), inference(cnf_transformation, [], [f356])).
fof(f610, plain, (sP6(sK59, sK60, sK57, sK58) | ~ spl61_1), inference(avatar_component_clause, [], [f608])).
fof(f50453, plain, ((sK58 = app(sK55(sK57, sK58), sK56(sK57, sK58))) | (~ spl61_1 | ~ spl61_709)), inference(forward_demodulation, [], [f50324, f994])).
fof(f994, plain, ((sK55(sK57, sK58) = app(sK55(sK57, sK58), nil)) | ~ spl61_1), inference(resolution, [], [f992, f542])).
fof(f542, plain, ! [X0] : (~ ssList(X0) | (app(X0, nil) = X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : ((app(X0, nil) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (ssList(X0) => (app(X0, nil) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax84)).
fof(f992, plain, (ssList(sK55(sK57, sK58)) | ~ spl61_1), inference(resolution, [], [f559, f624])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK55(X0, X1))), inference(cnf_transformation, [], [f351])).
fof(f50324, plain, ((sK58 = app(app(sK55(sK57, sK58), nil), sK56(sK57, sK58))) | (~ spl61_1 | ~ spl61_709)), inference(backward_demodulation, [], [f5261, f14329])).
fof(f14329, plain, ((nil = cons(sK54(sK57, sK58), nil)) | ~ spl61_709), inference(avatar_component_clause, [], [f14327])).
fof(f5261, plain, ((sK58 = app(app(sK55(sK57, sK58), cons(sK54(sK57, sK58), nil)), sK56(sK57, sK58))) | ~ spl61_1), inference(resolution, [], [f624, f561])).
fof(f561, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), sK56(X0, X1)) = X1)), inference(cnf_transformation, [], [f351])).
fof(f50325, plain, ((app(nil, sK58) = cons(sK54(sK57, sK58), sK58)) | (~ spl61_1 | ~ spl61_709)), inference(backward_demodulation, [], [f5298, f14329])).
fof(f5298, plain, ((cons(sK54(sK57, sK58), sK58) = app(cons(sK54(sK57, sK58), nil), sK58)) | ~ spl61_1), inference(resolution, [], [f991, f1946])).
fof(f1946, plain, ! [X71] : (~ ssItem(X71) | (cons(X71, sK58) = app(cons(X71, nil), sK58))), inference(resolution, [], [f537, f617])).
fof(f617, plain, ssList(sK58), inference(forward_demodulation, [], [f566, f567])).
fof(f566, plain, ssList(sK60), inference(cnf_transformation, [], [f356])).
fof(f537, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax81)).
fof(f991, plain, (ssItem(sK54(sK57, sK58)) | ~ spl61_1), inference(resolution, [], [f558, f624])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X0, X1))), inference(cnf_transformation, [], [f351])).
fof(f50711, plain, ((sK57 = tl(cons(sK54(sK57, sK57), sK57))) | (~ spl61_1 | ~ spl61_709)), inference(backward_demodulation, [], [f5301, f50454])).
fof(f5301, plain, ((sK58 = tl(cons(sK54(sK57, sK58), sK58))) | ~ spl61_1), inference(resolution, [], [f991, f1139])).
fof(f1139, plain, ! [X56] : (~ ssItem(X56) | (sK58 = tl(cons(X56, sK58)))), inference(resolution, [], [f460, f617])).
fof(f460, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax25)).
fof(f54426, plain, (~ spl61_323 | ~ spl61_1 | spl61_76 | ~ spl61_709), inference(avatar_split_clause, [], [f54425, f14327, f2557, f608, f6689])).
fof(f2557, plain, (spl61_76 <=> (sK58 = tl(sK58))), introduced(avatar_definition, [new_symbols(naming, [spl61_76])])).
fof(f54425, plain, (~ (sK57 = tl(sK57)) | (~ spl61_1 | spl61_76 | ~ spl61_709)), inference(forward_demodulation, [], [f2559, f50454])).
fof(f2559, plain, (~ (sK58 = tl(sK58)) | spl61_76), inference(avatar_component_clause, [], [f2557])).
fof(f48299, plain, (~ spl61_1 | ~ spl61_651), inference(avatar_contradiction_clause, [], [f48298])).
fof(f48298, plain, ($false | (~ spl61_1 | ~ spl61_651)), inference(resolution, [], [f13404, f624])).
fof(f13404, plain, (! [X0, X1] : ~ sP6(X0, X1, sK57, sK58) | ~ spl61_651), inference(avatar_component_clause, [], [f13403])).
fof(f13403, plain, (spl61_651 <=> ! [X1, X0] : ~ sP6(X0, X1, sK57, sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_651])])).
fof(f48281, plain, (spl61_649 | ~ spl61_650 | spl61_709), inference(avatar_contradiction_clause, [], [f48280])).
fof(f48280, plain, ($false | (spl61_649 | ~ spl61_650 | spl61_709)), inference(subsumption_resolution, [], [f48279, f13400])).
fof(f13400, plain, (ssList(cons(sK54(sK57, sK58), nil)) | ~ spl61_650), inference(avatar_component_clause, [], [f13399])).
fof(f13399, plain, (spl61_650 <=> ssList(cons(sK54(sK57, sK58), nil))), introduced(avatar_definition, [new_symbols(naming, [spl61_650])])).
fof(f48279, plain, (~ ssList(cons(sK54(sK57, sK58), nil)) | (spl61_649 | spl61_709)), inference(subsumption_resolution, [], [f48278, f449])).
fof(f449, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax17)).
fof(f48278, plain, (~ ssList(nil) | ~ ssList(cons(sK54(sK57, sK58), nil)) | (spl61_649 | spl61_709)), inference(subsumption_resolution, [], [f48276, f14328])).
fof(f14328, plain, (~ (nil = cons(sK54(sK57, sK58), nil)) | spl61_709), inference(avatar_component_clause, [], [f14327])).
fof(f48276, plain, ((nil = cons(sK54(sK57, sK58), nil)) | ~ ssList(nil) | ~ ssList(cons(sK54(sK57, sK58), nil)) | spl61_649), inference(resolution, [], [f13397, f447])).
fof(f447, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax15)).
fof(f13397, plain, (~ neq(cons(sK54(sK57, sK58), nil), nil) | spl61_649), inference(avatar_component_clause, [], [f13395])).
fof(f13395, plain, (spl61_649 <=> neq(cons(sK54(sK57, sK58), nil), nil)), introduced(avatar_definition, [new_symbols(naming, [spl61_649])])).
fof(f41078, plain, (spl61_54 | spl61_42), inference(avatar_split_clause, [], [f16069, f1237, f1853])).
fof(f1853, plain, (spl61_54 <=> (sK58 = cons(hd(sK58), tl(sK58)))), introduced(avatar_definition, [new_symbols(naming, [spl61_54])])).
fof(f1237, plain, (spl61_42 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_42])])).
fof(f16069, plain, ((sK58 = cons(hd(sK58), tl(sK58))) | spl61_42), inference(subsumption_resolution, [], [f15976, f1238])).
fof(f1238, plain, (~ (nil = sK58) | spl61_42), inference(avatar_component_clause, [], [f1237])).
fof(f15976, plain, ((nil = sK58) | (sK58 = cons(hd(sK58), tl(sK58)))), inference(resolution, [], [f617, f534])).
fof(f534, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax78)).
fof(f14101, plain, (~ spl61_1 | spl61_650), inference(avatar_contradiction_clause, [], [f14100])).
fof(f14100, plain, ($false | (~ spl61_1 | spl61_650)), inference(subsumption_resolution, [], [f14099, f449])).
fof(f14099, plain, (~ ssList(nil) | (~ spl61_1 | spl61_650)), inference(subsumption_resolution, [], [f14098, f991])).
fof(f14098, plain, (~ ssItem(sK54(sK57, sK58)) | ~ ssList(nil) | spl61_650), inference(resolution, [], [f13401, f448])).
fof(f448, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax16)).
fof(f13401, plain, (~ ssList(cons(sK54(sK57, sK58), nil)) | spl61_650), inference(avatar_component_clause, [], [f13399])).
fof(f13405, plain, (~ spl61_649 | ~ spl61_650 | spl61_651 | ~ spl61_1), inference(avatar_split_clause, [], [f13393, f608, f13403, f13399, f13395])).
fof(f13393, plain, (! [X0, X1] : (~ sP6(X0, X1, sK57, sK58) | ~ ssList(cons(sK54(sK57, sK58), nil)) | ~ neq(cons(sK54(sK57, sK58), nil), nil)) | ~ spl61_1), inference(forward_demodulation, [], [f13392, f5262])).
fof(f13392, plain, (! [X0, X1] : (~ sP6(X0, X1, app(sK55(sK57, sK58), sK56(sK57, sK58)), sK58) | ~ ssList(cons(sK54(sK57, sK58), nil)) | ~ neq(cons(sK54(sK57, sK58), nil), nil)) | ~ spl61_1), inference(subsumption_resolution, [], [f13391, f5264])).
fof(f5264, plain, (ssList(sK55(sK57, sK58)) | ~ spl61_1), inference(resolution, [], [f624, f559])).
fof(f13391, plain, (! [X0, X1] : (~ sP6(X0, X1, app(sK55(sK57, sK58), sK56(sK57, sK58)), sK58) | ~ ssList(cons(sK54(sK57, sK58), nil)) | ~ ssList(sK55(sK57, sK58)) | ~ neq(cons(sK54(sK57, sK58), nil), nil)) | ~ spl61_1), inference(subsumption_resolution, [], [f13363, f1008])).
fof(f1008, plain, (ssList(sK56(sK57, sK58)) | ~ spl61_1), inference(resolution, [], [f560, f624])).
fof(f560, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK56(X0, X1))), inference(cnf_transformation, [], [f351])).
fof(f13363, plain, (! [X0, X1] : (~ sP6(X0, X1, app(sK55(sK57, sK58), sK56(sK57, sK58)), sK58) | ~ ssList(sK56(sK57, sK58)) | ~ ssList(cons(sK54(sK57, sK58), nil)) | ~ ssList(sK55(sK57, sK58)) | ~ neq(cons(sK54(sK57, sK58), nil), nil)) | ~ spl61_1), inference(superposition, [], [f599, f5261])).
fof(f599, plain, ! [X0, X8, X7, X1, X9] : (~ sP6(X0, X1, app(X7, X9), app(app(X7, X8), X9)) | ~ ssList(X9) | ~ ssList(X8) | ~ ssList(X7) | ~ neq(X8, nil)), inference(equality_resolution, [], [f598])).
fof(f598, plain, ! [X0, X8, X7, X3, X1, X9] : (~ neq(X8, nil) | ~ (app(app(X7, X8), X9) = X3) | ~ ssList(X9) | ~ ssList(X8) | ~ ssList(X7) | ~ sP6(X0, X1, app(X7, X9), X3)), inference(equality_resolution, [], [f557])).
fof(f557, plain, ! [X2, X0, X8, X7, X3, X1, X9] : (~ neq(X8, nil) | ~ (app(X7, X9) = X2) | ~ (app(app(X7, X8), X9) = X3) | ~ ssList(X9) | ~ ssList(X8) | ~ ssList(X7) | ~ sP6(X0, X1, X2, X3)), inference(cnf_transformation, [], [f351])).
fof(f2773, plain, (spl61_42 | spl61_75), inference(avatar_contradiction_clause, [], [f2772])).
fof(f2772, plain, ($false | (spl61_42 | spl61_75)), inference(subsumption_resolution, [], [f2771, f617])).
fof(f2771, plain, (~ ssList(sK58) | (spl61_42 | spl61_75)), inference(subsumption_resolution, [], [f2770, f1238])).
fof(f2770, plain, ((nil = sK58) | ~ ssList(sK58) | spl61_75), inference(resolution, [], [f2555, f457])).
fof(f457, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax22)).
fof(f2555, plain, (~ ssItem(hd(sK58)) | spl61_75), inference(avatar_component_clause, [], [f2553])).
fof(f2553, plain, (spl61_75 <=> ssItem(hd(sK58))), introduced(avatar_definition, [new_symbols(naming, [spl61_75])])).
fof(f2646, plain, (spl61_42 | spl61_74), inference(avatar_contradiction_clause, [], [f2645])).
fof(f2645, plain, ($false | (spl61_42 | spl61_74)), inference(subsumption_resolution, [], [f2644, f617])).
fof(f2644, plain, (~ ssList(sK58) | (spl61_42 | spl61_74)), inference(subsumption_resolution, [], [f2643, f1238])).
fof(f2643, plain, ((nil = sK58) | ~ ssList(sK58) | spl61_74), inference(resolution, [], [f2551, f459])).
fof(f459, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax24)).
fof(f2551, plain, (~ ssList(tl(sK58)) | spl61_74), inference(avatar_component_clause, [], [f2549])).
fof(f2549, plain, (spl61_74 <=> ssList(tl(sK58))), introduced(avatar_definition, [new_symbols(naming, [spl61_74])])).
fof(f2560, plain, (~ spl61_74 | ~ spl61_75 | ~ spl61_76 | ~ spl61_54), inference(avatar_split_clause, [], [f2529, f1853, f2557, f2553, f2549])).
fof(f2529, plain, (~ (sK58 = tl(sK58)) | ~ ssItem(hd(sK58)) | ~ ssList(tl(sK58)) | ~ spl61_54), inference(superposition, [], [f450, f1855])).
fof(f1855, plain, ((sK58 = cons(hd(sK58), tl(sK58))) | ~ spl61_54), inference(avatar_component_clause, [], [f1853])).
fof(f450, plain, ! [X0, X1] : (~ (cons(X1, X0) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0] : (! [X1] : (~ (cons(X1, X0) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (cons(X1, X0) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC004+1.p', ax18)).
fof(f1889, plain, ~ spl61_42, inference(avatar_contradiction_clause, [], [f1888])).
fof(f1888, plain, ($false | ~ spl61_42), inference(subsumption_resolution, [], [f1887, f449])).
fof(f1887, plain, (~ ssList(nil) | ~ spl61_42), inference(resolution, [], [f1863, f604])).
fof(f604, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f585])).
fof(f585, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1) | ~ ssList(X1)), inference(equality_resolution, [], [f446])).
fof(f446, plain, ! [X0, X1] : (~ (X0 = X1) | ~ neq(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f1863, plain, (neq(nil, nil) | ~ spl61_42), inference(backward_demodulation, [], [f616, f1239])).
fof(f1239, plain, ((nil = sK58) | ~ spl61_42), inference(avatar_component_clause, [], [f1237])).
fof(f616, plain, neq(sK58, nil), inference(subsumption_resolution, [], [f569, f556])).
fof(f556, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X3, nil)), inference(cnf_transformation, [], [f351])).
fof(f569, plain, (neq(sK58, nil) | sP6(sK59, sK60, sK57, sK58)), inference(cnf_transformation, [], [f356])).
fof(f621, plain, spl61_2, inference(avatar_contradiction_clause, [], [f620])).
fof(f620, plain, ($false | spl61_2), inference(subsumption_resolution, [], [f619, f616])).
fof(f619, plain, (~ neq(sK58, nil) | spl61_2), inference(forward_demodulation, [], [f614, f567])).
fof(f614, plain, (~ neq(sK60, nil) | spl61_2), inference(avatar_component_clause, [], [f612])).
fof(f612, plain, (spl61_2 <=> neq(sK60, nil)), introduced(avatar_definition, [new_symbols(naming, [spl61_2])])).
fof(f615, plain, (spl61_1 | ~ spl61_2), inference(avatar_split_clause, [], [f570, f612, f608])).
fof(f570, plain, (~ neq(sK60, nil) | sP6(sK59, sK60, sK57, sK58)), inference(cnf_transformation, [], [f356])).