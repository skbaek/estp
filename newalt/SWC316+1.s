fof(f580307, plain, $false, inference(avatar_sat_refutation, [], [f618, f624, f1404, f2683, f2978, f2984, f8265, f8978, f16636, f16708, f17359, f17741, f27523, f28204, f40446, f40449, f56927, f77446, f81740, f152717, f491374, f493215, f522639, f524495, f524497, f528241, f529778, f559349, f576197])).
fof(f576197, plain, (spl60_290 | ~ spl60_11511 | ~ spl60_15460), inference(avatar_contradiction_clause, [], [f576196])).
fof(f576196, plain, ($false | (spl60_290 | ~ spl60_11511 | ~ spl60_15460)), inference(subsumption_resolution, [], [f574229, f8977])).
fof(f8977, plain, (~ (sK56 = tl(sK56)) | spl60_290), inference(avatar_component_clause, [], [f8975])).
fof(f8975, plain, (spl60_290 <=> (sK56 = tl(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_290])])).
fof(f574229, plain, ((sK56 = tl(sK56)) | (~ spl60_11511 | ~ spl60_15460)), inference(backward_demodulation, [], [f365913, f267702])).
fof(f267702, plain, ((sK56 = sK57) | ~ spl60_11511), inference(avatar_component_clause, [], [f267701])).
fof(f267701, plain, (spl60_11511 <=> (sK56 = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_11511])])).
fof(f365913, plain, ((sK57 = tl(sK56)) | ~ spl60_15460), inference(avatar_component_clause, [], [f365912])).
fof(f365912, plain, (spl60_15460 <=> (sK57 = tl(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_15460])])).
fof(f559349, plain, (~ spl60_1 | ~ spl60_288 | ~ spl60_289 | ~ spl60_650 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_4646 | ~ spl60_17344 | ~ spl60_17624), inference(avatar_contradiction_clause, [], [f559348])).
fof(f559348, plain, ($false | (~ spl60_1 | ~ spl60_288 | ~ spl60_289 | ~ spl60_650 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_4646 | ~ spl60_17344 | ~ spl60_17624)), inference(resolution, [], [f556218, f627])).
fof(f627, plain, (sP6(sK56, sK57, sK57, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f626, f565])).
fof(f565, plain, (sK57 = sK59), inference(cnf_transformation, [], [f355])).
fof(f355, plain, ((((((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK56, sK57, sK59, sK58)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK56, sK57, sK58, sK59])], [f234, f354, f353, f352, f351])).
fof(f351, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X0, X1, X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(sK56, X1, X3, X2)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(sK56, X1, X3, X2)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, sK57, X3, X2)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, sK57, X3, X2)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, sK57, X3, sK58)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK56, sK57, X3, sK58)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) => (((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK56, sK57, sK59, sK58)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X0, X1, X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X0, X1, X3, X2] : (((~ neq(X0, nil) | ! [X4] : (! [X5] : (! [X6] : (~ neq(nil, X0) | ! [X7] : (~ neq(nil, X0) | ~ (hd(X0) = X7) | ~ (cons(X7, nil) = X6) | ~ ssItem(X7)) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6)) | ~ ssList(X5)) | ~ (X1 = X4) | ~ ssList(X4))) & ? [X8] : (? [X9] : ((app(X9, cons(X8, nil)) = X3) & (app(cons(X8, nil), X9) = X2) & ssList(X9)) & ssItem(X8)) & neq(X1, nil)) | ~ sP6(X0, X1, X3, X2)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X0, X1, X3, X2] : (sP6(X0, X1, X3, X2) <=> ((~ neq(X0, nil) | ! [X4] : (! [X5] : (! [X6] : (~ neq(nil, X0) | ! [X7] : (~ neq(nil, X0) | ~ (hd(X0) = X7) | ~ (cons(X7, nil) = X6) | ~ ssItem(X7)) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6)) | ~ ssList(X5)) | ~ (X1 = X4) | ~ ssList(X4))) & ? [X8] : (? [X9] : ((app(X9, cons(X8, nil)) = X3) & (app(cons(X8, nil), X9) = X2) & ssList(X9)) & ssItem(X8)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | ((~ neq(X0, nil) | ! [X4] : (! [X5] : (! [X6] : (~ neq(nil, X0) | ! [X7] : (~ neq(nil, X0) | ~ (hd(X0) = X7) | ~ (cons(X7, nil) = X6) | ~ ssItem(X7)) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6)) | ~ ssList(X5)) | ~ (X1 = X4) | ~ ssList(X4))) & ? [X8] : (? [X9] : ((app(X9, cons(X8, nil)) = X3) & (app(cons(X8, nil), X9) = X2) & ssList(X9)) & ssItem(X8)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | ((~ neq(X0, nil) | ! [X4] : (! [X5] : (! [X6] : (~ neq(nil, X0) | ! [X7] : (~ neq(nil, X0) | ~ (hd(X0) = X7) | ~ (cons(X7, nil) = X6) | ~ ssItem(X7)) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6)) | ~ ssList(X5)) | ~ (X1 = X4) | ~ ssList(X4))) & ? [X8] : (? [X9] : (((app(X9, cons(X8, nil)) = X3) & (app(cons(X8, nil), X9) = X2)) & ssList(X9)) & ssItem(X8)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & ((neq(X0, nil) & ? [X4] : (? [X5] : (? [X6] : (neq(nil, X0) & ? [X7] : (neq(nil, X0) & (hd(X0) = X7) & (cons(X7, nil) = X6) & ssItem(X7)) & (app(X5, X6) = X4) & (tl(X0) = X5) & ssList(X6)) & ssList(X5)) & (X1 = X4) & ssList(X4))) | ! [X8] : (ssItem(X8) => ! [X9] : (ssList(X9) => (~ (app(X9, cons(X8, nil)) = X3) | ~ (app(cons(X8, nil), X9) = X2)))) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & ((neq(X0, nil) & ? [X6] : (? [X7] : (? [X8] : (neq(nil, X0) & ? [X9] : (neq(nil, X0) & (hd(X0) = X9) & (cons(X9, nil) = X8) & ssItem(X9)) & (app(X7, X8) = X6) & (tl(X0) = X7) & ssList(X8)) & ssList(X7)) & (X1 = X6) & ssList(X6))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(X5, cons(X4, nil)) = X3) | ~ (app(cons(X4, nil), X5) = X2)))) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & ((neq(X0, nil) & ? [X6] : (? [X7] : (? [X8] : (neq(nil, X0) & ? [X9] : (neq(nil, X0) & (hd(X0) = X9) & (cons(X9, nil) = X8) & ssItem(X9)) & (app(X7, X8) = X6) & (tl(X0) = X7) & ssList(X8)) & ssList(X7)) & (X1 = X6) & ssList(X6))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(X5, cons(X4, nil)) = X3) | ~ (app(cons(X4, nil), X5) = X2)))) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', co1)).
fof(f626, plain, (sP6(sK56, sK57, sK59, sK56) | ~ spl60_1), inference(forward_demodulation, [], [f613, f566])).
fof(f566, plain, (sK56 = sK58), inference(cnf_transformation, [], [f355])).
fof(f613, plain, (sP6(sK56, sK57, sK59, sK58) | ~ spl60_1), inference(avatar_component_clause, [], [f611])).
fof(f611, plain, (spl60_1 <=> sP6(sK56, sK57, sK59, sK58)), introduced(avatar_definition, [new_symbols(naming, [spl60_1])])).
fof(f556218, plain, (! [X0, X1] : ~ sP6(sK56, sK57, X0, X1) | (~ spl60_1 | ~ spl60_288 | ~ spl60_289 | ~ spl60_650 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_4646 | ~ spl60_17344 | ~ spl60_17624)), inference(subsumption_resolution, [], [f556217, f80726])).
fof(f80726, plain, (neq(sK56, nil) | ~ spl60_4646), inference(avatar_component_clause, [], [f80725])).
fof(f80725, plain, (spl60_4646 <=> neq(sK56, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_4646])])).
fof(f556217, plain, (! [X0, X1] : (~ sP6(sK56, sK57, X0, X1) | ~ neq(sK56, nil)) | (~ spl60_1 | ~ spl60_288 | ~ spl60_289 | ~ spl60_650 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17344 | ~ spl60_17624)), inference(subsumption_resolution, [], [f556216, f8968])).
fof(f8968, plain, (ssList(tl(sK56)) | ~ spl60_288), inference(avatar_component_clause, [], [f8967])).
fof(f8967, plain, (spl60_288 <=> ssList(tl(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_288])])).
fof(f556216, plain, (! [X0, X1] : (~ sP6(sK56, sK57, X0, X1) | ~ ssList(tl(sK56)) | ~ neq(sK56, nil)) | (~ spl60_1 | ~ spl60_289 | ~ spl60_650 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17344 | ~ spl60_17624)), inference(subsumption_resolution, [], [f556215, f481145])).
fof(f481145, plain, (ssList(cons(hd(sK56), nil)) | ~ spl60_17344), inference(avatar_component_clause, [], [f481144])).
fof(f481144, plain, (spl60_17344 <=> ssList(cons(hd(sK56), nil))), introduced(avatar_definition, [new_symbols(naming, [spl60_17344])])).
fof(f556215, plain, (! [X0, X1] : (~ sP6(sK56, sK57, X0, X1) | ~ ssList(cons(hd(sK56), nil)) | ~ ssList(tl(sK56)) | ~ neq(sK56, nil)) | (~ spl60_1 | ~ spl60_289 | ~ spl60_650 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(subsumption_resolution, [], [f556214, f8972])).
fof(f8972, plain, (ssItem(hd(sK56)) | ~ spl60_289), inference(avatar_component_clause, [], [f8971])).
fof(f8971, plain, (spl60_289 <=> ssItem(hd(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_289])])).
fof(f556214, plain, (! [X0, X1] : (~ sP6(sK56, sK57, X0, X1) | ~ ssItem(hd(sK56)) | ~ ssList(cons(hd(sK56), nil)) | ~ ssList(tl(sK56)) | ~ neq(sK56, nil)) | (~ spl60_1 | ~ spl60_289 | ~ spl60_650 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(subsumption_resolution, [], [f556159, f17730])).
fof(f17730, plain, (neq(nil, sK56) | ~ spl60_650), inference(avatar_component_clause, [], [f17729])).
fof(f17729, plain, (spl60_650 <=> neq(nil, sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_650])])).
fof(f556159, plain, (! [X0, X1] : (~ sP6(sK56, sK57, X0, X1) | ~ neq(nil, sK56) | ~ ssItem(hd(sK56)) | ~ ssList(cons(hd(sK56), nil)) | ~ ssList(tl(sK56)) | ~ neq(sK56, nil)) | (~ spl60_1 | ~ spl60_289 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(superposition, [], [f609, f553166])).
fof(f553166, plain, ((sK57 = app(tl(sK56), cons(hd(sK56), nil))) | (~ spl60_1 | ~ spl60_289 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(backward_demodulation, [], [f551811, f552484])).
fof(f552484, plain, ((sK55(sK57, sK56) = tl(sK56)) | (~ spl60_1 | ~ spl60_289 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(backward_demodulation, [], [f493987, f552483])).
fof(f552483, plain, ((sK56 = cons(hd(sK56), sK55(sK57, sK56))) | (~ spl60_1 | ~ spl60_289 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(backward_demodulation, [], [f510933, f551812])).
fof(f551812, plain, ((sK56 = app(cons(hd(sK56), nil), sK55(sK57, sK56))) | (~ spl60_1 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(backward_demodulation, [], [f133595, f551563])).
fof(f551563, plain, ((sK54(sK57, sK56) = hd(sK56)) | (~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(forward_demodulation, [], [f551562, f522638])).
fof(f522638, plain, ((hd(sK56) = hd(cons(sK54(sK57, sK56), nil))) | ~ spl60_17624), inference(avatar_component_clause, [], [f522636])).
fof(f522636, plain, (spl60_17624 <=> (hd(sK56) = hd(cons(sK54(sK57, sK56), nil)))), introduced(avatar_definition, [new_symbols(naming, [spl60_17624])])).
fof(f551562, plain, ((sK54(sK57, sK56) = hd(cons(sK54(sK57, sK56), nil))) | (~ spl60_653 | spl60_1248 | ~ spl60_1249)), inference(subsumption_resolution, [], [f523360, f28196])).
fof(f28196, plain, (~ (nil = cons(sK54(sK57, sK56), nil)) | spl60_1248), inference(avatar_component_clause, [], [f28195])).
fof(f28195, plain, (spl60_1248 <=> (nil = cons(sK54(sK57, sK56), nil))), introduced(avatar_definition, [new_symbols(naming, [spl60_1248])])).
fof(f523360, plain, ((sK54(sK57, sK56) = hd(cons(sK54(sK57, sK56), nil))) | (nil = cons(sK54(sK57, sK56), nil)) | (~ spl60_653 | ~ spl60_1249)), inference(forward_demodulation, [], [f30849, f28203])).
fof(f28203, plain, ((sK54(sK57, sK56) = sK52(cons(sK54(sK57, sK56), nil))) | ~ spl60_1249), inference(avatar_component_clause, [], [f28201])).
fof(f28201, plain, (spl60_1249 <=> (sK54(sK57, sK56) = sK52(cons(sK54(sK57, sK56), nil)))), introduced(avatar_definition, [new_symbols(naming, [spl60_1249])])).
fof(f30849, plain, ((nil = cons(sK54(sK57, sK56), nil)) | (hd(cons(sK54(sK57, sK56), nil)) = sK52(cons(sK54(sK57, sK56), nil))) | ~ spl60_653), inference(resolution, [], [f17885, f529])).
fof(f529, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (hd(X0) = sK52(X0))), inference(cnf_transformation, [], [f339])).
fof(f339, plain, ! [X0] : (((hd(X0) = sK52(X0)) & ssItem(sK52(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK52])], [f188, f338])).
fof(f338, plain, ! [X0] : (? [X1] : ((hd(X0) = X1) & ssItem(X1)) => ((hd(X0) = sK52(X0)) & ssItem(sK52(X0)))), introduced(choice_axiom, [])).
fof(f188, plain, ! [X0] : (? [X1] : ((hd(X0) = X1) & ssItem(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f187])).
fof(f187, plain, ! [X0] : ((? [X1] : ((hd(X0) = X1) & ssItem(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f75])).
fof(f75, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ? [X1] : ((hd(X0) = X1) & ssItem(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax75)).
fof(f17885, plain, (ssList(cons(sK54(sK57, sK56), nil)) | ~ spl60_653), inference(avatar_component_clause, [], [f17884])).
fof(f17884, plain, (spl60_653 <=> ssList(cons(sK54(sK57, sK56), nil))), introduced(avatar_definition, [new_symbols(naming, [spl60_653])])).
fof(f133595, plain, ((sK56 = app(cons(sK54(sK57, sK56), nil), sK55(sK57, sK56))) | ~ spl60_1), inference(resolution, [], [f627, f558])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(cons(sK54(X2, X3), nil), sK55(X2, X3)) = X3)), inference(cnf_transformation, [], [f350])).
fof(f350, plain, ! [X0, X1, X2, X3] : (((~ neq(X0, nil) | ! [X4] : (! [X5] : (! [X6] : (~ neq(nil, X0) | ! [X7] : (~ neq(nil, X0) | ~ (hd(X0) = X7) | ~ (cons(X7, nil) = X6) | ~ ssItem(X7)) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6)) | ~ ssList(X5)) | ~ (X1 = X4) | ~ ssList(X4))) & (((app(sK55(X2, X3), cons(sK54(X2, X3), nil)) = X2) & (app(cons(sK54(X2, X3), nil), sK55(X2, X3)) = X3) & ssList(sK55(X2, X3))) & ssItem(sK54(X2, X3))) & neq(X1, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55])], [f347, f349, f348])).
fof(f348, plain, ! [X3, X2] : (? [X8] : (? [X9] : ((app(X9, cons(X8, nil)) = X2) & (app(cons(X8, nil), X9) = X3) & ssList(X9)) & ssItem(X8)) => (? [X9] : ((app(X9, cons(sK54(X2, X3), nil)) = X2) & (app(cons(sK54(X2, X3), nil), X9) = X3) & ssList(X9)) & ssItem(sK54(X2, X3)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X3, X2] : (? [X9] : ((app(X9, cons(sK54(X2, X3), nil)) = X2) & (app(cons(sK54(X2, X3), nil), X9) = X3) & ssList(X9)) => ((app(sK55(X2, X3), cons(sK54(X2, X3), nil)) = X2) & (app(cons(sK54(X2, X3), nil), sK55(X2, X3)) = X3) & ssList(sK55(X2, X3)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1, X2, X3] : (((~ neq(X0, nil) | ! [X4] : (! [X5] : (! [X6] : (~ neq(nil, X0) | ! [X7] : (~ neq(nil, X0) | ~ (hd(X0) = X7) | ~ (cons(X7, nil) = X6) | ~ ssItem(X7)) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6)) | ~ ssList(X5)) | ~ (X1 = X4) | ~ ssList(X4))) & ? [X8] : (? [X9] : ((app(X9, cons(X8, nil)) = X2) & (app(cons(X8, nil), X9) = X3) & ssList(X9)) & ssItem(X8)) & neq(X1, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X0, X1, X3, X2] : (((~ neq(X0, nil) | ! [X4] : (! [X5] : (! [X6] : (~ neq(nil, X0) | ! [X7] : (~ neq(nil, X0) | ~ (hd(X0) = X7) | ~ (cons(X7, nil) = X6) | ~ ssItem(X7)) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6)) | ~ ssList(X5)) | ~ (X1 = X4) | ~ ssList(X4))) & ? [X8] : (? [X9] : ((app(X9, cons(X8, nil)) = X3) & (app(cons(X8, nil), X9) = X2) & ssList(X9)) & ssItem(X8)) & neq(X1, nil)) | ~ sP6(X0, X1, X3, X2)), inference(nnf_transformation, [], [f233])).
fof(f510933, plain, ((cons(hd(sK56), sK55(sK57, sK56)) = app(cons(hd(sK56), nil), sK55(sK57, sK56))) | (~ spl60_1 | ~ spl60_289)), inference(resolution, [], [f8429, f8972])).
fof(f8429, plain, (! [X4] : (~ ssItem(X4) | (cons(X4, sK55(sK57, sK56)) = app(cons(X4, nil), sK55(sK57, sK56)))) | ~ spl60_1), inference(resolution, [], [f6552, f536])).
fof(f536, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax81)).
fof(f6552, plain, (ssList(sK55(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f627, f557])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK55(X2, X3))), inference(cnf_transformation, [], [f350])).
fof(f493987, plain, ((sK55(sK57, sK56) = tl(cons(hd(sK56), sK55(sK57, sK56)))) | (~ spl60_1 | ~ spl60_289)), inference(resolution, [], [f8972, f1110])).
fof(f1110, plain, (! [X53] : (~ ssItem(X53) | (sK55(sK57, sK56) = tl(cons(X53, sK55(sK57, sK56))))) | ~ spl60_1), inference(resolution, [], [f459, f995])).
fof(f995, plain, (ssList(sK55(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f557, f627])).
fof(f459, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax25)).
fof(f551811, plain, ((sK57 = app(sK55(sK57, sK56), cons(hd(sK56), nil))) | (~ spl60_1 | ~ spl60_653 | spl60_1248 | ~ spl60_1249 | ~ spl60_17624)), inference(backward_demodulation, [], [f133594, f551563])).
fof(f133594, plain, ((sK57 = app(sK55(sK57, sK56), cons(sK54(sK57, sK56), nil))) | ~ spl60_1), inference(resolution, [], [f627, f559])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(sK55(X2, X3), cons(sK54(X2, X3), nil)) = X2)), inference(cnf_transformation, [], [f350])).
fof(f609, plain, ! [X2, X0, X3] : (~ sP6(X0, app(tl(X0), cons(hd(X0), nil)), X2, X3) | ~ neq(nil, X0) | ~ ssItem(hd(X0)) | ~ ssList(cons(hd(X0), nil)) | ~ ssList(tl(X0)) | ~ neq(X0, nil)), inference(subsumption_resolution, [], [f601, f460])).
fof(f460, plain, ! [X0, X1] : (ssList(app(X0, X1)) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : (ssList(app(X0, X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ssList(app(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax26)).
fof(f601, plain, ! [X2, X0, X3] : (~ neq(X0, nil) | ~ neq(nil, X0) | ~ ssItem(hd(X0)) | ~ ssList(cons(hd(X0), nil)) | ~ ssList(tl(X0)) | ~ ssList(app(tl(X0), cons(hd(X0), nil))) | ~ sP6(X0, app(tl(X0), cons(hd(X0), nil)), X2, X3)), inference(duplicate_literal_removal, [], [f600])).
fof(f600, plain, ! [X2, X0, X3] : (~ neq(X0, nil) | ~ neq(nil, X0) | ~ neq(nil, X0) | ~ ssItem(hd(X0)) | ~ ssList(cons(hd(X0), nil)) | ~ ssList(tl(X0)) | ~ ssList(app(tl(X0), cons(hd(X0), nil))) | ~ sP6(X0, app(tl(X0), cons(hd(X0), nil)), X2, X3)), inference(equality_resolution, [], [f599])).
fof(f599, plain, ! [X2, X0, X3, X1] : (~ neq(X0, nil) | ~ neq(nil, X0) | ~ neq(nil, X0) | ~ ssItem(hd(X0)) | ~ ssList(cons(hd(X0), nil)) | ~ ssList(tl(X0)) | ~ (app(tl(X0), cons(hd(X0), nil)) = X1) | ~ ssList(app(tl(X0), cons(hd(X0), nil))) | ~ sP6(X0, X1, X2, X3)), inference(equality_resolution, [], [f598])).
fof(f598, plain, ! [X2, X0, X5, X3, X1] : (~ neq(X0, nil) | ~ neq(nil, X0) | ~ neq(nil, X0) | ~ ssItem(hd(X0)) | ~ (tl(X0) = X5) | ~ ssList(cons(hd(X0), nil)) | ~ ssList(X5) | ~ (app(X5, cons(hd(X0), nil)) = X1) | ~ ssList(app(X5, cons(hd(X0), nil))) | ~ sP6(X0, X1, X2, X3)), inference(equality_resolution, [], [f597])).
fof(f597, plain, ! [X4, X2, X0, X5, X3, X1] : (~ neq(X0, nil) | ~ neq(nil, X0) | ~ neq(nil, X0) | ~ ssItem(hd(X0)) | ~ (app(X5, cons(hd(X0), nil)) = X4) | ~ (tl(X0) = X5) | ~ ssList(cons(hd(X0), nil)) | ~ ssList(X5) | ~ (X1 = X4) | ~ ssList(X4) | ~ sP6(X0, X1, X2, X3)), inference(equality_resolution, [], [f596])).
fof(f596, plain, ! [X6, X4, X2, X0, X5, X3, X1] : (~ neq(X0, nil) | ~ neq(nil, X0) | ~ neq(nil, X0) | ~ (cons(hd(X0), nil) = X6) | ~ ssItem(hd(X0)) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6) | ~ ssList(X5) | ~ (X1 = X4) | ~ ssList(X4) | ~ sP6(X0, X1, X2, X3)), inference(equality_resolution, [], [f560])).
fof(f560, plain, ! [X6, X4, X2, X0, X7, X5, X3, X1] : (~ neq(X0, nil) | ~ neq(nil, X0) | ~ neq(nil, X0) | ~ (hd(X0) = X7) | ~ (cons(X7, nil) = X6) | ~ ssItem(X7) | ~ (app(X5, X6) = X4) | ~ (tl(X0) = X5) | ~ ssList(X6) | ~ ssList(X5) | ~ (X1 = X4) | ~ ssList(X4) | ~ sP6(X0, X1, X2, X3)), inference(cnf_transformation, [], [f350])).
fof(f529778, plain, (~ spl60_1 | ~ spl60_665 | spl60_15460), inference(avatar_contradiction_clause, [], [f529777])).
fof(f529777, plain, ($false | (~ spl60_1 | ~ spl60_665 | spl60_15460)), inference(subsumption_resolution, [], [f529752, f365914])).
fof(f365914, plain, (~ (sK57 = tl(sK56)) | spl60_15460), inference(avatar_component_clause, [], [f365912])).
fof(f529752, plain, ((sK57 = tl(sK56)) | (~ spl60_1 | ~ spl60_665)), inference(backward_demodulation, [], [f529696, f529708])).
fof(f529708, plain, ((sK56 = cons(sK54(sK57, sK56), sK57)) | (~ spl60_1 | ~ spl60_665)), inference(forward_demodulation, [], [f6576, f529698])).
fof(f529698, plain, ((sK56 = app(cons(sK54(sK57, sK56), nil), sK57)) | (~ spl60_1 | ~ spl60_665)), inference(forward_demodulation, [], [f133595, f17953])).
fof(f17953, plain, ((sK57 = sK55(sK57, sK56)) | ~ spl60_665), inference(avatar_component_clause, [], [f17951])).
fof(f17951, plain, (spl60_665 <=> (sK57 = sK55(sK57, sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_665])])).
fof(f6576, plain, ((cons(sK54(sK57, sK56), sK57) = app(cons(sK54(sK57, sK56), nil), sK57)) | ~ spl60_1), inference(resolution, [], [f994, f1694])).
fof(f1694, plain, ! [X71] : (~ ssItem(X71) | (cons(X71, sK57) = app(cons(X71, nil), sK57))), inference(resolution, [], [f536, f620])).
fof(f620, plain, ssList(sK57), inference(forward_demodulation, [], [f564, f565])).
fof(f564, plain, ssList(sK59), inference(cnf_transformation, [], [f355])).
fof(f994, plain, (ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f556, f627])).
fof(f556, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X2, X3))), inference(cnf_transformation, [], [f350])).
fof(f529696, plain, ((sK57 = tl(cons(sK54(sK57, sK56), sK57))) | (~ spl60_1 | ~ spl60_665)), inference(forward_demodulation, [], [f339379, f17953])).
fof(f339379, plain, ((sK55(sK57, sK56) = tl(cons(sK54(sK57, sK56), sK55(sK57, sK56)))) | ~ spl60_1), inference(resolution, [], [f1110, f133597])).
fof(f133597, plain, (ssItem(sK54(sK57, sK56)) | ~ spl60_1), inference(resolution, [], [f627, f556])).
fof(f528241, plain, (~ spl60_1 | ~ spl60_1223 | ~ spl60_1248 | spl60_11511), inference(avatar_contradiction_clause, [], [f528240])).
fof(f528240, plain, ($false | (~ spl60_1 | ~ spl60_1223 | ~ spl60_1248 | spl60_11511)), inference(subsumption_resolution, [], [f528239, f267703])).
fof(f267703, plain, (~ (sK56 = sK57) | spl60_11511), inference(avatar_component_clause, [], [f267701])).
fof(f528239, plain, ((sK56 = sK57) | (~ spl60_1 | ~ spl60_1223 | ~ spl60_1248)), inference(forward_demodulation, [], [f526688, f876])).
fof(f876, plain, (sK56 = app(sK56, nil)), inference(resolution, [], [f541, f621])).
fof(f621, plain, ssList(sK56), inference(forward_demodulation, [], [f563, f566])).
fof(f563, plain, ssList(sK58), inference(cnf_transformation, [], [f355])).
fof(f541, plain, ! [X0] : (~ ssList(X0) | (app(X0, nil) = X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : ((app(X0, nil) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (ssList(X0) => (app(X0, nil) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax84)).
fof(f526688, plain, ((sK57 = app(sK56, nil)) | (~ spl60_1 | ~ spl60_1223 | ~ spl60_1248)), inference(backward_demodulation, [], [f523672, f27578])).
fof(f27578, plain, ((sK56 = sK55(sK57, sK56)) | ~ spl60_1223), inference(avatar_component_clause, [], [f27576])).
fof(f27576, plain, (spl60_1223 <=> (sK56 = sK55(sK57, sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_1223])])).
fof(f523672, plain, ((sK57 = app(sK55(sK57, sK56), nil)) | (~ spl60_1 | ~ spl60_1248)), inference(backward_demodulation, [], [f133594, f28197])).
fof(f28197, plain, ((nil = cons(sK54(sK57, sK56), nil)) | ~ spl60_1248), inference(avatar_component_clause, [], [f28195])).
fof(f524497, plain, (spl60_1223 | ~ spl60_1 | ~ spl60_1248), inference(avatar_split_clause, [], [f524496, f28195, f611, f27576])).
fof(f524496, plain, ((sK56 = sK55(sK57, sK56)) | (~ spl60_1 | ~ spl60_1248)), inference(backward_demodulation, [], [f998, f523673])).
fof(f523673, plain, ((sK56 = app(nil, sK55(sK57, sK56))) | (~ spl60_1 | ~ spl60_1248)), inference(backward_demodulation, [], [f133595, f28197])).
fof(f998, plain, ((sK55(sK57, sK56) = app(nil, sK55(sK57, sK56))) | ~ spl60_1), inference(resolution, [], [f995, f462])).
fof(f462, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax28)).
fof(f524495, plain, (spl60_665 | ~ spl60_1 | ~ spl60_1248), inference(avatar_split_clause, [], [f524494, f28195, f611, f17951])).
fof(f524494, plain, ((sK57 = sK55(sK57, sK56)) | (~ spl60_1 | ~ spl60_1248)), inference(backward_demodulation, [], [f997, f523672])).
fof(f997, plain, ((sK55(sK57, sK56) = app(sK55(sK57, sK56), nil)) | ~ spl60_1), inference(resolution, [], [f995, f541])).
fof(f522639, plain, (spl60_1248 | spl60_17624 | ~ spl60_1 | ~ spl60_653), inference(avatar_split_clause, [], [f522634, f17884, f611, f522636, f28195])).
fof(f522634, plain, ((hd(sK56) = hd(cons(sK54(sK57, sK56), nil))) | (nil = cons(sK54(sK57, sK56), nil)) | (~ spl60_1 | ~ spl60_653)), inference(forward_demodulation, [], [f489008, f133595])).
fof(f489008, plain, ((hd(cons(sK54(sK57, sK56), nil)) = hd(app(cons(sK54(sK57, sK56), nil), sK55(sK57, sK56)))) | (nil = cons(sK54(sK57, sK56), nil)) | (~ spl60_1 | ~ spl60_653)), inference(resolution, [], [f17885, f8432])).
fof(f8432, plain, (! [X7] : (~ ssList(X7) | (hd(X7) = hd(app(X7, sK55(sK57, sK56)))) | (nil = X7)) | ~ spl60_1), inference(resolution, [], [f6552, f542])).
fof(f542, plain, ! [X0, X1] : (~ ssList(X1) | (nil = X0) | (hd(X0) = hd(app(X0, X1))) | ~ ssList(X0)), inference(cnf_transformation, [], [f204])).
fof(f204, plain, ! [X0] : (! [X1] : ((hd(X0) = hd(app(X0, X1))) | (nil = X0) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f203])).
fof(f203, plain, ! [X0] : (! [X1] : (((hd(X0) = hd(app(X0, X1))) | (nil = X0)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (~ (nil = X0) => (hd(X0) = hd(app(X0, X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax85)).
fof(f493215, plain, (spl60_289 | ~ spl60_256 | ~ spl60_327 | ~ spl60_440), inference(avatar_split_clause, [], [f493122, f11583, f9151, f8761, f8971])).
fof(f8761, plain, (spl60_256 <=> ssItem(sK51(sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_256])])).
fof(f9151, plain, (spl60_327 <=> (sK56 = cons(sK51(sK56), sK50(sK56)))), introduced(avatar_definition, [new_symbols(naming, [spl60_327])])).
fof(f11583, plain, (spl60_440 <=> ! [X29] : ((hd(cons(X29, sK50(sK56))) = X29) | ~ ssItem(X29))), introduced(avatar_definition, [new_symbols(naming, [spl60_440])])).
fof(f493122, plain, (ssItem(hd(sK56)) | (~ spl60_256 | ~ spl60_327 | ~ spl60_440)), inference(backward_demodulation, [], [f8762, f157374])).
fof(f157374, plain, ((hd(sK56) = sK51(sK56)) | (~ spl60_256 | ~ spl60_327 | ~ spl60_440)), inference(forward_demodulation, [], [f157370, f9153])).
fof(f9153, plain, ((sK56 = cons(sK51(sK56), sK50(sK56))) | ~ spl60_327), inference(avatar_component_clause, [], [f9151])).
fof(f157370, plain, ((sK51(sK56) = hd(cons(sK51(sK56), sK50(sK56)))) | (~ spl60_256 | ~ spl60_440)), inference(resolution, [], [f8762, f11584])).
fof(f11584, plain, (! [X29] : (~ ssItem(X29) | (hd(cons(X29, sK50(sK56))) = X29)) | ~ spl60_440), inference(avatar_component_clause, [], [f11583])).
fof(f8762, plain, (ssItem(sK51(sK56)) | ~ spl60_256), inference(avatar_component_clause, [], [f8761])).
fof(f491374, plain, (spl60_17344 | ~ spl60_1 | ~ spl60_653 | spl60_1248), inference(avatar_split_clause, [], [f490476, f28195, f17884, f611, f481144])).
fof(f490476, plain, (ssList(cons(hd(sK56), nil)) | (~ spl60_1 | ~ spl60_653 | spl60_1248)), inference(backward_demodulation, [], [f17885, f490434])).
fof(f490434, plain, ((sK54(sK57, sK56) = hd(sK56)) | (~ spl60_1 | ~ spl60_653 | spl60_1248)), inference(backward_demodulation, [], [f6562, f489222])).
fof(f489222, plain, ((hd(sK56) = hd(cons(sK54(sK57, sK56), nil))) | (~ spl60_1 | ~ spl60_653 | spl60_1248)), inference(forward_demodulation, [], [f489221, f133595])).
fof(f489221, plain, ((hd(cons(sK54(sK57, sK56), nil)) = hd(app(cons(sK54(sK57, sK56), nil), sK55(sK57, sK56)))) | (~ spl60_1 | ~ spl60_653 | spl60_1248)), inference(subsumption_resolution, [], [f489008, f28196])).
fof(f6562, plain, ((sK54(sK57, sK56) = hd(cons(sK54(sK57, sK56), nil))) | ~ spl60_1), inference(resolution, [], [f994, f1057])).
fof(f1057, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f457, f448])).
fof(f448, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax17)).
fof(f457, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax23)).
fof(f152717, plain, (~ spl60_1 | ~ spl60_28 | ~ spl60_653 | spl60_667), inference(avatar_contradiction_clause, [], [f152716])).
fof(f152716, plain, ($false | (~ spl60_1 | ~ spl60_28 | ~ spl60_653 | spl60_667)), inference(subsumption_resolution, [], [f152715, f17962])).
fof(f17962, plain, (~ (sK57 = cons(sK54(sK57, sK56), nil)) | spl60_667), inference(avatar_component_clause, [], [f17961])).
fof(f17961, plain, (spl60_667 <=> (sK57 = cons(sK54(sK57, sK56), nil))), introduced(avatar_definition, [new_symbols(naming, [spl60_667])])).
fof(f152715, plain, ((sK57 = cons(sK54(sK57, sK56), nil)) | (~ spl60_1 | ~ spl60_28 | ~ spl60_653)), inference(backward_demodulation, [], [f27916, f152590])).
fof(f152590, plain, ((sK57 = app(nil, cons(sK54(sK57, sK56), nil))) | (~ spl60_1 | ~ spl60_28)), inference(backward_demodulation, [], [f133594, f1162])).
fof(f1162, plain, ((nil = sK55(sK57, sK56)) | ~ spl60_28), inference(avatar_component_clause, [], [f1160])).
fof(f1160, plain, (spl60_28 <=> (nil = sK55(sK57, sK56))), introduced(avatar_definition, [new_symbols(naming, [spl60_28])])).
fof(f27916, plain, ((cons(sK54(sK57, sK56), nil) = app(nil, cons(sK54(sK57, sK56), nil))) | ~ spl60_653), inference(resolution, [], [f17885, f462])).
fof(f81740, plain, (spl60_30 | spl60_4646), inference(avatar_contradiction_clause, [], [f81739])).
fof(f81739, plain, ($false | (spl60_30 | spl60_4646)), inference(subsumption_resolution, [], [f81738, f621])).
fof(f81738, plain, (~ ssList(sK56) | (spl60_30 | spl60_4646)), inference(subsumption_resolution, [], [f81737, f448])).
fof(f81737, plain, (~ ssList(nil) | ~ ssList(sK56) | (spl60_30 | spl60_4646)), inference(subsumption_resolution, [], [f81735, f1170])).
fof(f1170, plain, (~ (nil = sK56) | spl60_30), inference(avatar_component_clause, [], [f1169])).
fof(f1169, plain, (spl60_30 <=> (nil = sK56)), introduced(avatar_definition, [new_symbols(naming, [spl60_30])])).
fof(f81735, plain, ((nil = sK56) | ~ ssList(nil) | ~ ssList(sK56) | spl60_4646), inference(resolution, [], [f80727, f446])).
fof(f446, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax15)).
fof(f80727, plain, (~ neq(sK56, nil) | spl60_4646), inference(avatar_component_clause, [], [f80725])).
fof(f77446, plain, (spl60_30 | spl60_288), inference(avatar_contradiction_clause, [], [f77445])).
fof(f77445, plain, ($false | (spl60_30 | spl60_288)), inference(subsumption_resolution, [], [f77444, f621])).
fof(f77444, plain, (~ ssList(sK56) | (spl60_30 | spl60_288)), inference(subsumption_resolution, [], [f77443, f1170])).
fof(f77443, plain, ((nil = sK56) | ~ ssList(sK56) | spl60_288), inference(resolution, [], [f8969, f458])).
fof(f458, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax24)).
fof(f8969, plain, (~ ssList(tl(sK56)) | spl60_288), inference(avatar_component_clause, [], [f8967])).
fof(f56927, plain, (spl60_78 | ~ spl60_1 | ~ spl60_667), inference(avatar_split_clause, [], [f54527, f17961, f611, f2515])).
fof(f2515, plain, (spl60_78 <=> (nil = tl(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl60_78])])).
fof(f54527, plain, ((nil = tl(sK57)) | (~ spl60_1 | ~ spl60_667)), inference(backward_demodulation, [], [f6573, f17963])).
fof(f17963, plain, ((sK57 = cons(sK54(sK57, sK56), nil)) | ~ spl60_667), inference(avatar_component_clause, [], [f17961])).
fof(f6573, plain, ((nil = tl(cons(sK54(sK57, sK56), nil))) | ~ spl60_1), inference(resolution, [], [f994, f1086])).
fof(f1086, plain, ! [X6] : (~ ssItem(X6) | (nil = tl(cons(X6, nil)))), inference(resolution, [], [f459, f448])).
fof(f40449, plain, (spl60_327 | spl60_30), inference(avatar_split_clause, [], [f16986, f1169, f9151])).
fof(f16986, plain, ((sK56 = cons(sK51(sK56), sK50(sK56))) | spl60_30), inference(subsumption_resolution, [], [f16854, f1170])).
fof(f16854, plain, ((nil = sK56) | (sK56 = cons(sK51(sK56), sK50(sK56)))), inference(resolution, [], [f621, f454])).
fof(f454, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(sK51(X0), sK50(X0)) = X0)), inference(cnf_transformation, [], [f322])).
fof(f322, plain, ! [X0] : ((((cons(sK51(X0), sK50(X0)) = X0) & ssItem(sK51(X0))) & ssList(sK50(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK50, sK51])], [f125, f321, f320])).
fof(f320, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) => (? [X2] : ((cons(X2, sK50(X0)) = X0) & ssItem(X2)) & ssList(sK50(X0)))), introduced(choice_axiom, [])).
fof(f321, plain, ! [X0] : (? [X2] : ((cons(X2, sK50(X0)) = X0) & ssItem(X2)) => ((cons(sK51(X0), sK50(X0)) = X0) & ssItem(sK51(X0)))), introduced(choice_axiom, [])).
fof(f125, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0] : ((? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (ssList(X0) => (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax20)).
fof(f40446, plain, (spl60_440 | spl60_30), inference(avatar_split_clause, [], [f16996, f1169, f11583])).
fof(f16996, plain, (! [X29] : ((hd(cons(X29, sK50(sK56))) = X29) | ~ ssItem(X29)) | spl60_30), inference(subsumption_resolution, [], [f16913, f1170])).
fof(f16913, plain, ! [X29] : ((hd(cons(X29, sK50(sK56))) = X29) | (nil = sK56) | ~ ssItem(X29)), inference(resolution, [], [f621, f1079])).
fof(f1079, plain, ! [X50, X49] : (~ ssList(X50) | (hd(cons(X49, sK50(X50))) = X49) | (nil = X50) | ~ ssItem(X49)), inference(resolution, [], [f457, f452])).
fof(f452, plain, ! [X0] : (ssList(sK50(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f322])).
fof(f28204, plain, (spl60_1248 | spl60_1249 | ~ spl60_1 | ~ spl60_653), inference(avatar_split_clause, [], [f28199, f17884, f611, f28201, f28195])).
fof(f28199, plain, ((sK54(sK57, sK56) = sK52(cons(sK54(sK57, sK56), nil))) | (nil = cons(sK54(sK57, sK56), nil)) | (~ spl60_1 | ~ spl60_653)), inference(forward_demodulation, [], [f27917, f6562])).
fof(f27917, plain, ((nil = cons(sK54(sK57, sK56), nil)) | (hd(cons(sK54(sK57, sK56), nil)) = sK52(cons(sK54(sK57, sK56), nil))) | ~ spl60_653), inference(resolution, [], [f17885, f529])).
fof(f27523, plain, (~ spl60_1 | spl60_653), inference(avatar_contradiction_clause, [], [f27522])).
fof(f27522, plain, ($false | (~ spl60_1 | spl60_653)), inference(subsumption_resolution, [], [f27521, f448])).
fof(f27521, plain, (~ ssList(nil) | (~ spl60_1 | spl60_653)), inference(subsumption_resolution, [], [f27520, f994])).
fof(f27520, plain, (~ ssItem(sK54(sK57, sK56)) | ~ ssList(nil) | spl60_653), inference(resolution, [], [f17886, f447])).
fof(f447, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax16)).
fof(f17886, plain, (~ ssList(cons(sK54(sK57, sK56), nil)) | spl60_653), inference(avatar_component_clause, [], [f17884])).
fof(f17741, plain, (spl60_30 | spl60_650), inference(avatar_contradiction_clause, [], [f17740])).
fof(f17740, plain, ($false | (spl60_30 | spl60_650)), inference(subsumption_resolution, [], [f17739, f448])).
fof(f17739, plain, (~ ssList(nil) | (spl60_30 | spl60_650)), inference(subsumption_resolution, [], [f17738, f621])).
fof(f17738, plain, (~ ssList(sK56) | ~ ssList(nil) | (spl60_30 | spl60_650)), inference(subsumption_resolution, [], [f17736, f1170])).
fof(f17736, plain, ((nil = sK56) | ~ ssList(sK56) | ~ ssList(nil) | spl60_650), inference(resolution, [], [f17731, f446])).
fof(f17731, plain, (~ neq(nil, sK56) | spl60_650), inference(avatar_component_clause, [], [f17729])).
fof(f17359, plain, (spl60_30 | spl60_256), inference(avatar_contradiction_clause, [], [f17358])).
fof(f17358, plain, ($false | (spl60_30 | spl60_256)), inference(subsumption_resolution, [], [f17357, f621])).
fof(f17357, plain, (~ ssList(sK56) | (spl60_30 | spl60_256)), inference(subsumption_resolution, [], [f17356, f1170])).
fof(f17356, plain, ((nil = sK56) | ~ ssList(sK56) | spl60_256), inference(resolution, [], [f8763, f453])).
fof(f453, plain, ! [X0] : (ssItem(sK51(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f322])).
fof(f8763, plain, (~ ssItem(sK51(sK56)) | spl60_256), inference(avatar_component_clause, [], [f8761])).
fof(f16708, plain, (~ spl60_140 | ~ spl60_1 | spl60_28 | ~ spl60_30), inference(avatar_split_clause, [], [f16707, f1169, f1160, f611, f4111])).
fof(f4111, plain, (spl60_140 <=> ssList(cons(sK54(sK57, nil), nil))), introduced(avatar_definition, [new_symbols(naming, [spl60_140])])).
fof(f16707, plain, (~ ssList(cons(sK54(sK57, nil), nil)) | (~ spl60_1 | spl60_28 | ~ spl60_30)), inference(subsumption_resolution, [], [f16706, f11640])).
fof(f11640, plain, (ssList(sK55(sK57, nil)) | (~ spl60_1 | ~ spl60_30)), inference(backward_demodulation, [], [f6552, f1171])).
fof(f1171, plain, ((nil = sK56) | ~ spl60_30), inference(avatar_component_clause, [], [f1169])).
fof(f16706, plain, (~ ssList(sK55(sK57, nil)) | ~ ssList(cons(sK54(sK57, nil), nil)) | (~ spl60_1 | spl60_28 | ~ spl60_30)), inference(subsumption_resolution, [], [f12038, f11622])).
fof(f11622, plain, (~ (nil = sK55(sK57, nil)) | (spl60_28 | ~ spl60_30)), inference(backward_demodulation, [], [f1161, f1171])).
fof(f1161, plain, (~ (nil = sK55(sK57, sK56)) | spl60_28), inference(avatar_component_clause, [], [f1160])).
fof(f12038, plain, ((nil = sK55(sK57, nil)) | ~ ssList(sK55(sK57, nil)) | ~ ssList(cons(sK54(sK57, nil), nil)) | (~ spl60_1 | ~ spl60_30)), inference(trivial_inequality_removal, [], [f12029])).
fof(f12029, plain, (~ (nil = nil) | (nil = sK55(sK57, nil)) | ~ ssList(sK55(sK57, nil)) | ~ ssList(cons(sK54(sK57, nil), nil)) | (~ spl60_1 | ~ spl60_30)), inference(superposition, [], [f538, f11639])).
fof(f11639, plain, ((nil = app(cons(sK54(sK57, nil), nil), sK55(sK57, nil))) | (~ spl60_1 | ~ spl60_30)), inference(backward_demodulation, [], [f6551, f1171])).
fof(f6551, plain, ((sK56 = app(cons(sK54(sK57, sK56), nil), sK55(sK57, sK56))) | ~ spl60_1), inference(resolution, [], [f627, f558])).
fof(f538, plain, ! [X0, X1] : (~ (nil = app(X0, X1)) | (nil = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f343])).
fof(f343, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | ~ (nil = X0) | ~ (nil = X1)) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f342])).
fof(f342, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | (~ (nil = X0) | ~ (nil = X1))) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f201])).
fof(f201, plain, ! [X0] : (! [X1] : (((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f83])).
fof(f83, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax83)).
fof(f16636, plain, (~ spl60_1 | ~ spl60_30 | spl60_140), inference(avatar_contradiction_clause, [], [f16635])).
fof(f16635, plain, ($false | (~ spl60_1 | ~ spl60_30 | spl60_140)), inference(subsumption_resolution, [], [f16634, f448])).
fof(f16634, plain, (~ ssList(nil) | (~ spl60_1 | ~ spl60_30 | spl60_140)), inference(subsumption_resolution, [], [f16633, f11604])).
fof(f11604, plain, (ssItem(sK54(sK57, nil)) | (~ spl60_1 | ~ spl60_30)), inference(backward_demodulation, [], [f994, f1171])).
fof(f16633, plain, (~ ssItem(sK54(sK57, nil)) | ~ ssList(nil) | spl60_140), inference(resolution, [], [f4113, f447])).
fof(f4113, plain, (~ ssList(cons(sK54(sK57, nil), nil)) | spl60_140), inference(avatar_component_clause, [], [f4111])).
fof(f8978, plain, (~ spl60_288 | ~ spl60_289 | ~ spl60_290 | spl60_30), inference(avatar_split_clause, [], [f8930, f1169, f8975, f8971, f8967])).
fof(f8930, plain, (~ (sK56 = tl(sK56)) | ~ ssItem(hd(sK56)) | ~ ssList(tl(sK56)) | spl60_30), inference(superposition, [], [f449, f6467])).
fof(f6467, plain, ((sK56 = cons(hd(sK56), tl(sK56))) | spl60_30), inference(subsumption_resolution, [], [f6407, f1170])).
fof(f6407, plain, ((nil = sK56) | (sK56 = cons(hd(sK56), tl(sK56)))), inference(resolution, [], [f621, f533])).
fof(f533, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax78)).
fof(f449, plain, ! [X0, X1] : (~ (cons(X1, X0) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0] : (! [X1] : (~ (cons(X1, X0) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (cons(X1, X0) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax18)).
fof(f8265, plain, (~ spl60_1 | ~ spl60_28 | ~ spl60_96), inference(avatar_contradiction_clause, [], [f8264])).
fof(f8264, plain, ($false | (~ spl60_1 | ~ spl60_28 | ~ spl60_96)), inference(resolution, [], [f6644, f6601])).
fof(f6601, plain, (sP6(sK56, sK56, sK56, sK56) | (~ spl60_1 | ~ spl60_28)), inference(backward_demodulation, [], [f627, f6597])).
fof(f6597, plain, ((sK56 = sK57) | (~ spl60_1 | ~ spl60_28)), inference(forward_demodulation, [], [f6581, f853])).
fof(f853, plain, (sK56 = app(nil, sK56)), inference(resolution, [], [f462, f621])).
fof(f6581, plain, ((sK57 = app(nil, sK56)) | (~ spl60_1 | ~ spl60_28)), inference(backward_demodulation, [], [f6555, f6577])).
fof(f6577, plain, ((sK56 = cons(sK54(sK57, sK56), nil)) | (~ spl60_1 | ~ spl60_28)), inference(forward_demodulation, [], [f6575, f6556])).
fof(f6556, plain, ((sK56 = app(cons(sK54(sK57, sK56), nil), nil)) | (~ spl60_1 | ~ spl60_28)), inference(forward_demodulation, [], [f6551, f1162])).
fof(f6575, plain, ((cons(sK54(sK57, sK56), nil) = app(cons(sK54(sK57, sK56), nil), nil)) | ~ spl60_1), inference(resolution, [], [f994, f1664])).
fof(f1664, plain, ! [X6] : (~ ssItem(X6) | (cons(X6, nil) = app(cons(X6, nil), nil))), inference(resolution, [], [f536, f448])).
fof(f6555, plain, ((sK57 = app(nil, cons(sK54(sK57, sK56), nil))) | (~ spl60_1 | ~ spl60_28)), inference(forward_demodulation, [], [f6550, f1162])).
fof(f6550, plain, ((sK57 = app(sK55(sK57, sK56), cons(sK54(sK57, sK56), nil))) | ~ spl60_1), inference(resolution, [], [f627, f559])).
fof(f6644, plain, (! [X0, X1] : ~ sP6(sK56, sK56, X0, X1) | (~ spl60_1 | ~ spl60_28 | ~ spl60_96)), inference(backward_demodulation, [], [f2969, f6597])).
fof(f2969, plain, (! [X0, X1] : ~ sP6(sK57, sK57, X0, X1) | ~ spl60_96), inference(avatar_component_clause, [], [f2968])).
fof(f2968, plain, (spl60_96 <=> ! [X1, X0] : ~ sP6(sK57, sK57, X0, X1)), introduced(avatar_definition, [new_symbols(naming, [spl60_96])])).
fof(f2984, plain, (spl60_32 | spl60_95), inference(avatar_contradiction_clause, [], [f2983])).
fof(f2983, plain, ($false | (spl60_32 | spl60_95)), inference(subsumption_resolution, [], [f2982, f448])).
fof(f2982, plain, (~ ssList(nil) | (spl60_32 | spl60_95)), inference(subsumption_resolution, [], [f2981, f620])).
fof(f2981, plain, (~ ssList(sK57) | ~ ssList(nil) | (spl60_32 | spl60_95)), inference(subsumption_resolution, [], [f2979, f1179])).
fof(f1179, plain, (~ (nil = sK57) | spl60_32), inference(avatar_component_clause, [], [f1178])).
fof(f1178, plain, (spl60_32 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_32])])).
fof(f2979, plain, ((nil = sK57) | ~ ssList(sK57) | ~ ssList(nil) | spl60_95), inference(resolution, [], [f2966, f446])).
fof(f2966, plain, (~ neq(nil, sK57) | spl60_95), inference(avatar_component_clause, [], [f2964])).
fof(f2964, plain, (spl60_95 <=> neq(nil, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_95])])).
fof(f2978, plain, (~ spl60_95 | spl60_96 | spl60_32 | ~ spl60_71 | ~ spl60_78), inference(avatar_split_clause, [], [f2977, f2515, f2483, f1178, f2968, f2964])).
fof(f2483, plain, (spl60_71 <=> ssItem(hd(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl60_71])])).
fof(f2977, plain, (! [X0, X1] : (~ sP6(sK57, sK57, X0, X1) | ~ neq(nil, sK57)) | (spl60_32 | ~ spl60_71 | ~ spl60_78)), inference(subsumption_resolution, [], [f2976, f448])).
fof(f2976, plain, (! [X0, X1] : (~ ssList(nil) | ~ sP6(sK57, sK57, X0, X1) | ~ neq(nil, sK57)) | (spl60_32 | ~ spl60_71 | ~ spl60_78)), inference(forward_demodulation, [], [f2975, f2517])).
fof(f2517, plain, ((nil = tl(sK57)) | ~ spl60_78), inference(avatar_component_clause, [], [f2515])).
fof(f2975, plain, (! [X0, X1] : (~ sP6(sK57, sK57, X0, X1) | ~ neq(nil, sK57) | ~ ssList(tl(sK57))) | (spl60_32 | ~ spl60_71 | ~ spl60_78)), inference(forward_demodulation, [], [f2974, f854])).
fof(f854, plain, (sK57 = app(nil, sK57)), inference(resolution, [], [f462, f620])).
fof(f2974, plain, (! [X0, X1] : (~ sP6(sK57, app(nil, sK57), X0, X1) | ~ neq(nil, sK57) | ~ ssList(tl(sK57))) | (spl60_32 | ~ spl60_71 | ~ spl60_78)), inference(forward_demodulation, [], [f2973, f2517])).
fof(f2973, plain, (! [X0, X1] : (~ sP6(sK57, app(tl(sK57), sK57), X0, X1) | ~ neq(nil, sK57) | ~ ssList(tl(sK57))) | (spl60_32 | ~ spl60_71 | ~ spl60_78)), inference(subsumption_resolution, [], [f2972, f619])).
fof(f619, plain, neq(sK57, nil), inference(subsumption_resolution, [], [f567, f555])).
fof(f555, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X1, nil)), inference(cnf_transformation, [], [f350])).
fof(f567, plain, (neq(sK57, nil) | sP6(sK56, sK57, sK59, sK58)), inference(cnf_transformation, [], [f355])).
fof(f2972, plain, (! [X0, X1] : (~ sP6(sK57, app(tl(sK57), sK57), X0, X1) | ~ neq(nil, sK57) | ~ ssList(tl(sK57)) | ~ neq(sK57, nil)) | (spl60_32 | ~ spl60_71 | ~ spl60_78)), inference(subsumption_resolution, [], [f2971, f620])).
fof(f2971, plain, (! [X0, X1] : (~ sP6(sK57, app(tl(sK57), sK57), X0, X1) | ~ neq(nil, sK57) | ~ ssList(sK57) | ~ ssList(tl(sK57)) | ~ neq(sK57, nil)) | (spl60_32 | ~ spl60_71 | ~ spl60_78)), inference(subsumption_resolution, [], [f2955, f2484])).
fof(f2484, plain, (ssItem(hd(sK57)) | ~ spl60_71), inference(avatar_component_clause, [], [f2483])).
fof(f2955, plain, (! [X0, X1] : (~ sP6(sK57, app(tl(sK57), sK57), X0, X1) | ~ neq(nil, sK57) | ~ ssItem(hd(sK57)) | ~ ssList(sK57) | ~ ssList(tl(sK57)) | ~ neq(sK57, nil)) | (spl60_32 | ~ spl60_78)), inference(superposition, [], [f609, f2649])).
fof(f2649, plain, ((sK57 = cons(hd(sK57), nil)) | (spl60_32 | ~ spl60_78)), inference(backward_demodulation, [], [f1586, f2517])).
fof(f1586, plain, ((sK57 = cons(hd(sK57), tl(sK57))) | spl60_32), inference(subsumption_resolution, [], [f1584, f1179])).
fof(f1584, plain, ((nil = sK57) | (sK57 = cons(hd(sK57), tl(sK57)))), inference(resolution, [], [f533, f620])).
fof(f2683, plain, (spl60_32 | spl60_71), inference(avatar_contradiction_clause, [], [f2682])).
fof(f2682, plain, ($false | (spl60_32 | spl60_71)), inference(subsumption_resolution, [], [f2681, f620])).
fof(f2681, plain, (~ ssList(sK57) | (spl60_32 | spl60_71)), inference(subsumption_resolution, [], [f2680, f1179])).
fof(f2680, plain, ((nil = sK57) | ~ ssList(sK57) | spl60_71), inference(resolution, [], [f2485, f456])).
fof(f456, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC316+1.p', ax22)).
fof(f2485, plain, (~ ssItem(hd(sK57)) | spl60_71), inference(avatar_component_clause, [], [f2483])).
fof(f1404, plain, ~ spl60_32, inference(avatar_contradiction_clause, [], [f1403])).
fof(f1403, plain, ($false | ~ spl60_32), inference(subsumption_resolution, [], [f1402, f448])).
fof(f1402, plain, (~ ssList(nil) | ~ spl60_32), inference(resolution, [], [f1229, f606])).
fof(f606, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f583])).
fof(f583, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1) | ~ ssList(X1)), inference(equality_resolution, [], [f445])).
fof(f445, plain, ! [X0, X1] : (~ (X0 = X1) | ~ neq(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f1229, plain, (neq(nil, nil) | ~ spl60_32), inference(backward_demodulation, [], [f619, f1180])).
fof(f1180, plain, ((nil = sK57) | ~ spl60_32), inference(avatar_component_clause, [], [f1178])).
fof(f624, plain, spl60_2, inference(avatar_contradiction_clause, [], [f623])).
fof(f623, plain, ($false | spl60_2), inference(subsumption_resolution, [], [f622, f619])).
fof(f622, plain, (~ neq(sK57, nil) | spl60_2), inference(forward_demodulation, [], [f617, f565])).
fof(f617, plain, (~ neq(sK59, nil) | spl60_2), inference(avatar_component_clause, [], [f615])).
fof(f615, plain, (spl60_2 <=> neq(sK59, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_2])])).
fof(f618, plain, (spl60_1 | ~ spl60_2), inference(avatar_split_clause, [], [f568, f615, f611])).
fof(f568, plain, (~ neq(sK59, nil) | sP6(sK56, sK57, sK59, sK58)), inference(cnf_transformation, [], [f355])).