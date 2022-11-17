fof(f574831, plain, $false, inference(avatar_sat_refutation, [], [f628, f633, f642, f655, f1579, f4861, f4867, f4971, f5102, f5108, f5159, f46546, f57578, f58250, f59388, f61560, f61651, f84098, f85626, f307200, f531532, f532982, f533413, f553765, f574234])).
fof(f574234, plain, (~ spl63_1 | ~ spl63_31 | spl63_180 | ~ spl63_6661 | ~ spl63_11502), inference(avatar_contradiction_clause, [], [f574233])).
fof(f574233, plain, ($false | (~ spl63_1 | ~ spl63_31 | spl63_180 | ~ spl63_6661 | ~ spl63_11502)), inference(subsumption_resolution, [], [f574232, f5210])).
fof(f5210, plain, (~ (nil = cons(hd(nil), nil)) | spl63_180), inference(avatar_component_clause, [], [f5209])).
fof(f5209, plain, (spl63_180 <=> (nil = cons(hd(nil), nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_180])])).
fof(f574232, plain, ((nil = cons(hd(nil), nil)) | (~ spl63_1 | ~ spl63_31 | ~ spl63_6661 | ~ spl63_11502)), inference(forward_demodulation, [], [f574231, f569751])).
fof(f569751, plain, ((hd(nil) = sK54(sK58, nil)) | (~ spl63_1 | ~ spl63_31 | ~ spl63_6661)), inference(backward_demodulation, [], [f533821, f562780])).
fof(f562780, plain, ((nil = cons(sK54(sK58, nil), nil)) | (~ spl63_31 | ~ spl63_6661)), inference(forward_demodulation, [], [f84540, f1196])).
fof(f1196, plain, ((nil = sK62) | ~ spl63_31), inference(avatar_component_clause, [], [f1194])).
fof(f1194, plain, (spl63_31 <=> (nil = sK62)), introduced(avatar_definition, [new_symbols(naming, [spl63_31])])).
fof(f84540, plain, ((nil = cons(sK54(sK58, sK62), nil)) | ~ spl63_6661), inference(avatar_component_clause, [], [f84538])).
fof(f84538, plain, (spl63_6661 <=> (nil = cons(sK54(sK58, sK62), nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_6661])])).
fof(f533821, plain, ((sK54(sK58, nil) = hd(cons(sK54(sK58, nil), nil))) | (~ spl63_1 | ~ spl63_31)), inference(backward_demodulation, [], [f54766, f1196])).
fof(f54766, plain, ((sK54(sK58, sK62) = hd(cons(sK54(sK58, sK62), nil))) | ~ spl63_1), inference(resolution, [], [f50858, f1077])).
fof(f1077, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f461, f452])).
fof(f452, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax17)).
fof(f461, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax23)).
fof(f50858, plain, (ssItem(sK54(sK58, sK62)) | ~ spl63_1), inference(subsumption_resolution, [], [f50857, f644])).
fof(f644, plain, strictorderedP(sK58), inference(backward_demodulation, [], [f574, f571])).
fof(f571, plain, (sK58 = sK60), inference(cnf_transformation, [], [f359])).
fof(f359, plain, ((((((~ (nil = sK59) & (nil = sK58)) | sP6(sK58, sK59)) & (~ (nil = sK60) | (nil = sK61)) & (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = sK60) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = sK62) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(sK60) & (sK61 = app(sK60, sK62)) & ssList(sK62)) & (sK58 = sK60) & (sK59 = sK61) & ssList(sK61)) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK58, sK59, sK60, sK61, sK62])], [f353, f358, f357, f356, f355, f354])).
fof(f354, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ (nil = X1) & (nil = X0)) | sP6(X0, X1)) & (~ (nil = X2) | (nil = X3)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = X2) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(X2) & (app(X2, X4) = X3) & ssList(X4)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ (nil = X1) & (nil = sK58)) | sP6(sK58, X1)) & (~ (nil = X2) | (nil = X3)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = X2) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(X2) & (app(X2, X4) = X3) & ssList(X4)) & (sK58 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f355, plain, (? [X1] : (? [X2] : (? [X3] : (((~ (nil = X1) & (nil = sK58)) | sP6(sK58, X1)) & (~ (nil = X2) | (nil = X3)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = X2) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(X2) & (app(X2, X4) = X3) & ssList(X4)) & (sK58 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ (nil = sK59) & (nil = sK58)) | sP6(sK58, sK59)) & (~ (nil = X2) | (nil = X3)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = X2) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(X2) & (app(X2, X4) = X3) & ssList(X4)) & (sK58 = X2) & (sK59 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f356, plain, (? [X2] : (? [X3] : (((~ (nil = sK59) & (nil = sK58)) | sP6(sK58, sK59)) & (~ (nil = X2) | (nil = X3)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = X2) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(X2) & (app(X2, X4) = X3) & ssList(X4)) & (sK58 = X2) & (sK59 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ (nil = sK59) & (nil = sK58)) | sP6(sK58, sK59)) & (~ (nil = sK60) | (nil = X3)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = sK60) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(sK60) & (app(sK60, X4) = X3) & ssList(X4)) & (sK58 = sK60) & (sK59 = X3) & ssList(X3)) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f357, plain, (? [X3] : (((~ (nil = sK59) & (nil = sK58)) | sP6(sK58, sK59)) & (~ (nil = sK60) | (nil = X3)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = sK60) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(sK60) & (app(sK60, X4) = X3) & ssList(X4)) & (sK58 = sK60) & (sK59 = X3) & ssList(X3)) => (((~ (nil = sK59) & (nil = sK58)) | sP6(sK58, sK59)) & (~ (nil = sK60) | (nil = sK61)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = sK60) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(sK60) & (app(sK60, X4) = sK61) & ssList(X4)) & (sK58 = sK60) & (sK59 = sK61) & ssList(sK61))), introduced(choice_axiom, [])).
fof(f358, plain, (? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = sK60) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(sK60) & (app(sK60, X4) = sK61) & ssList(X4)) => (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = sK60) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = sK62) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(sK60) & (sK61 = app(sK60, sK62)) & ssList(sK62))), introduced(choice_axiom, [])).
fof(f353, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ (nil = X1) & (nil = X0)) | sP6(X0, X1)) & (~ (nil = X2) | (nil = X3)) & ? [X4] : (! [X5] : (! [X6] : (! [X7] : (! [X8] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = X2) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ (app(cons(X5, nil), X6) = X4) | ~ ssList(X6)) | ~ ssItem(X5)) & strictorderedP(X2) & (app(X2, X4) = X3) & ssList(X4)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(rectify, [], [f234])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ (nil = X1) & (nil = X0)) | sP6(X0, X1)) & (~ (nil = X2) | (nil = X3)) & ? [X9] : (! [X10] : (! [X11] : (! [X12] : (! [X13] : (~ lt(X12, X10) | ~ (app(X13, cons(X12, nil)) = X2) | ~ ssList(X13)) | ~ ssItem(X12)) | ~ (app(cons(X10, nil), X11) = X9) | ~ ssList(X11)) | ~ ssItem(X10)) & strictorderedP(X2) & (app(X2, X9) = X3) & ssList(X9)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X0, X1] : (! [X4] : (~ strictorderedP(X0) | ? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X7, X5) & (app(X8, cons(X7, nil)) = X0) & ssList(X8)) & ssItem(X7)) & (app(cons(X5, nil), X6) = X4) & ssList(X6)) & ssItem(X5)) | ~ (app(X0, X4) = X1) | ~ ssList(X4)) | ~ sP6(X0, X1)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X0, X1] : (sP6(X0, X1) <=> ! [X4] : (~ strictorderedP(X0) | ? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X7, X5) & (app(X8, cons(X7, nil)) = X0) & ssList(X8)) & ssItem(X7)) & (app(cons(X5, nil), X6) = X4) & ssList(X6)) & ssItem(X5)) | ~ (app(X0, X4) = X1) | ~ ssList(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ (nil = X1) & (nil = X0)) | ! [X4] : (~ strictorderedP(X0) | ? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X7, X5) & (app(X8, cons(X7, nil)) = X0) & ssList(X8)) & ssItem(X7)) & (app(cons(X5, nil), X6) = X4) & ssList(X6)) & ssItem(X5)) | ~ (app(X0, X4) = X1) | ~ ssList(X4))) & (~ (nil = X2) | (nil = X3)) & ? [X9] : (! [X10] : (! [X11] : (! [X12] : (! [X13] : (~ lt(X12, X10) | ~ (app(X13, cons(X12, nil)) = X2) | ~ ssList(X13)) | ~ ssItem(X12)) | ~ (app(cons(X10, nil), X11) = X9) | ~ ssList(X11)) | ~ ssItem(X10)) & strictorderedP(X2) & (app(X2, X9) = X3) & ssList(X9)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ (nil = X1) & (nil = X0)) | ! [X4] : (~ strictorderedP(X0) | ? [X5] : (? [X6] : ((? [X7] : (? [X8] : ((lt(X7, X5) & (app(X8, cons(X7, nil)) = X0)) & ssList(X8)) & ssItem(X7)) & (app(cons(X5, nil), X6) = X4)) & ssList(X6)) & ssItem(X5)) | ~ (app(X0, X4) = X1) | ~ ssList(X4))) & (~ (nil = X2) | (nil = X3)) & ? [X9] : ((! [X10] : (! [X11] : (! [X12] : (! [X13] : (~ lt(X12, X10) | ~ (app(X13, cons(X12, nil)) = X2) | ~ ssList(X13)) | ~ ssItem(X12)) | ~ (app(cons(X10, nil), X11) = X9) | ~ ssList(X11)) | ~ ssItem(X10)) & strictorderedP(X2) & (app(X2, X9) = X3)) & ssList(X9)) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => ((((nil = X1) | ~ (nil = X0)) & ? [X4] : (strictorderedP(X0) & ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => (! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = X0)))) | ~ (app(cons(X5, nil), X6) = X4)))) & (app(X0, X4) = X1) & ssList(X4))) | ((nil = X2) & ~ (nil = X3)) | ! [X9] : (ssList(X9) => (? [X10] : (? [X11] : (? [X12] : (? [X13] : (lt(X12, X10) & (app(X13, cons(X12, nil)) = X2) & ssList(X13)) & ssItem(X12)) & (app(cons(X10, nil), X11) = X9) & ssList(X11)) & ssItem(X10)) | ~ strictorderedP(X2) | ~ (app(X2, X9) = X3))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => ((((nil = X1) | ~ (nil = X0)) & ? [X9] : (strictorderedP(X0) & ! [X10] : (ssItem(X10) => ! [X11] : (ssList(X11) => (! [X12] : (ssItem(X12) => ! [X13] : (ssList(X13) => (~ lt(X12, X10) | ~ (app(X13, cons(X12, nil)) = X0)))) | ~ (app(cons(X10, nil), X11) = X9)))) & (app(X0, X9) = X1) & ssList(X9))) | ((nil = X2) & ~ (nil = X3)) | ! [X4] : (ssList(X4) => (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X7, X5) & (app(X8, cons(X7, nil)) = X2) & ssList(X8)) & ssItem(X7)) & (app(cons(X5, nil), X6) = X4) & ssList(X6)) & ssItem(X5)) | ~ strictorderedP(X2) | ~ (app(X2, X4) = X3))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => ((((nil = X1) | ~ (nil = X0)) & ? [X9] : (strictorderedP(X0) & ! [X10] : (ssItem(X10) => ! [X11] : (ssList(X11) => (! [X12] : (ssItem(X12) => ! [X13] : (ssList(X13) => (~ lt(X12, X10) | ~ (app(X13, cons(X12, nil)) = X0)))) | ~ (app(cons(X10, nil), X11) = X9)))) & (app(X0, X9) = X1) & ssList(X9))) | ((nil = X2) & ~ (nil = X3)) | ! [X4] : (ssList(X4) => (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X7, X5) & (app(X8, cons(X7, nil)) = X2) & ssList(X8)) & ssItem(X7)) & (app(cons(X5, nil), X6) = X4) & ssList(X6)) & ssItem(X5)) | ~ strictorderedP(X2) | ~ (app(X2, X4) = X3))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', co1)).
fof(f574, plain, strictorderedP(sK60), inference(cnf_transformation, [], [f359])).
fof(f50857, plain, (ssItem(sK54(sK58, sK62)) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50856, f572])).
fof(f572, plain, ssList(sK62), inference(cnf_transformation, [], [f359])).
fof(f50856, plain, (ssItem(sK54(sK58, sK62)) | ~ ssList(sK62) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50792, f623])).
fof(f623, plain, (sP6(sK58, sK59) | ~ spl63_1), inference(avatar_component_clause, [], [f621])).
fof(f621, plain, (spl63_1 <=> sP6(sK58, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl63_1])])).
fof(f50792, plain, (~ sP6(sK58, sK59) | ssItem(sK54(sK58, sK62)) | ~ ssList(sK62) | ~ strictorderedP(sK58)), inference(superposition, [], [f612, f646])).
fof(f646, plain, (sK59 = app(sK58, sK62)), inference(backward_demodulation, [], [f643, f570])).
fof(f570, plain, (sK59 = sK61), inference(cnf_transformation, [], [f359])).
fof(f643, plain, (sK61 = app(sK58, sK62)), inference(backward_demodulation, [], [f573, f571])).
fof(f573, plain, (sK61 = app(sK60, sK62)), inference(cnf_transformation, [], [f359])).
fof(f612, plain, ! [X2, X0] : (~ sP6(X0, app(X0, X2)) | ssItem(sK54(X0, X2)) | ~ ssList(X2) | ~ strictorderedP(X0)), inference(equality_resolution, [], [f559])).
fof(f559, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | ssItem(sK54(X0, X2)) | ~ (app(X0, X2) = X1) | ~ ssList(X2) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f352])).
fof(f352, plain, ! [X0, X1] : (! [X2] : (~ strictorderedP(X0) | ((((lt(sK56(X0, X2), sK54(X0, X2)) & (app(sK57(X0, X2), cons(sK56(X0, X2), nil)) = X0) & ssList(sK57(X0, X2))) & ssItem(sK56(X0, X2))) & (app(cons(sK54(X0, X2), nil), sK55(X0, X2)) = X2) & ssList(sK55(X0, X2))) & ssItem(sK54(X0, X2))) | ~ (app(X0, X2) = X1) | ~ ssList(X2)) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56, sK57])], [f347, f351, f350, f349, f348])).
fof(f348, plain, ! [X2, X0] : (? [X3] : (? [X4] : (? [X5] : (? [X6] : (lt(X5, X3) & (app(X6, cons(X5, nil)) = X0) & ssList(X6)) & ssItem(X5)) & (app(cons(X3, nil), X4) = X2) & ssList(X4)) & ssItem(X3)) => (? [X4] : (? [X5] : (? [X6] : (lt(X5, sK54(X0, X2)) & (app(X6, cons(X5, nil)) = X0) & ssList(X6)) & ssItem(X5)) & (app(cons(sK54(X0, X2), nil), X4) = X2) & ssList(X4)) & ssItem(sK54(X0, X2)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X2, X0] : (? [X4] : (? [X5] : (? [X6] : (lt(X5, sK54(X0, X2)) & (app(X6, cons(X5, nil)) = X0) & ssList(X6)) & ssItem(X5)) & (app(cons(sK54(X0, X2), nil), X4) = X2) & ssList(X4)) => (? [X5] : (? [X6] : (lt(X5, sK54(X0, X2)) & (app(X6, cons(X5, nil)) = X0) & ssList(X6)) & ssItem(X5)) & (app(cons(sK54(X0, X2), nil), sK55(X0, X2)) = X2) & ssList(sK55(X0, X2)))), introduced(choice_axiom, [])).
fof(f350, plain, ! [X2, X0] : (? [X5] : (? [X6] : (lt(X5, sK54(X0, X2)) & (app(X6, cons(X5, nil)) = X0) & ssList(X6)) & ssItem(X5)) => (? [X6] : (lt(sK56(X0, X2), sK54(X0, X2)) & (app(X6, cons(sK56(X0, X2), nil)) = X0) & ssList(X6)) & ssItem(sK56(X0, X2)))), introduced(choice_axiom, [])).
fof(f351, plain, ! [X2, X0] : (? [X6] : (lt(sK56(X0, X2), sK54(X0, X2)) & (app(X6, cons(sK56(X0, X2), nil)) = X0) & ssList(X6)) => (lt(sK56(X0, X2), sK54(X0, X2)) & (app(sK57(X0, X2), cons(sK56(X0, X2), nil)) = X0) & ssList(sK57(X0, X2)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1] : (! [X2] : (~ strictorderedP(X0) | ? [X3] : (? [X4] : (? [X5] : (? [X6] : (lt(X5, X3) & (app(X6, cons(X5, nil)) = X0) & ssList(X6)) & ssItem(X5)) & (app(cons(X3, nil), X4) = X2) & ssList(X4)) & ssItem(X3)) | ~ (app(X0, X2) = X1) | ~ ssList(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X0, X1] : (! [X4] : (~ strictorderedP(X0) | ? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X7, X5) & (app(X8, cons(X7, nil)) = X0) & ssList(X8)) & ssItem(X7)) & (app(cons(X5, nil), X6) = X4) & ssList(X6)) & ssItem(X5)) | ~ (app(X0, X4) = X1) | ~ ssList(X4)) | ~ sP6(X0, X1)), inference(nnf_transformation, [], [f233])).
fof(f574231, plain, ((nil = cons(sK54(sK58, nil), nil)) | (~ spl63_31 | ~ spl63_11502)), inference(forward_demodulation, [], [f134830, f1196])).
fof(f134830, plain, ((nil = cons(sK54(sK58, sK62), sK62)) | ~ spl63_11502), inference(avatar_component_clause, [], [f134828])).
fof(f134828, plain, (spl63_11502 <=> (nil = cons(sK54(sK58, sK62), sK62))), introduced(avatar_definition, [new_symbols(naming, [spl63_11502])])).
fof(f553765, plain, (~ spl63_31 | ~ spl63_169 | ~ spl63_170 | ~ spl63_4714 | ~ spl63_4740 | spl63_11502), inference(avatar_contradiction_clause, [], [f553764])).
fof(f553764, plain, ($false | (~ spl63_31 | ~ spl63_169 | ~ spl63_170 | ~ spl63_4714 | ~ spl63_4740 | spl63_11502)), inference(subsumption_resolution, [], [f553763, f534205])).
fof(f534205, plain, (ssList(cons(sK54(sK58, nil), nil)) | (~ spl63_31 | ~ spl63_4714)), inference(backward_demodulation, [], [f61509, f1196])).
fof(f61509, plain, (ssList(cons(sK54(sK58, sK62), nil)) | ~ spl63_4714), inference(avatar_component_clause, [], [f61508])).
fof(f61508, plain, (spl63_4714 <=> ssList(cons(sK54(sK58, sK62), nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_4714])])).
fof(f553763, plain, (~ ssList(cons(sK54(sK58, nil), nil)) | (~ spl63_31 | ~ spl63_169 | ~ spl63_170 | ~ spl63_4740 | spl63_11502)), inference(forward_demodulation, [], [f553762, f1196])).
fof(f553762, plain, (~ ssList(cons(sK54(sK58, sK62), nil)) | (~ spl63_31 | ~ spl63_169 | ~ spl63_170 | ~ spl63_4740 | spl63_11502)), inference(subsumption_resolution, [], [f553761, f534992])).
fof(f534992, plain, (~ (nil = cons(sK54(sK58, nil), nil)) | (~ spl63_31 | spl63_11502)), inference(backward_demodulation, [], [f134829, f1196])).
fof(f134829, plain, (~ (nil = cons(sK54(sK58, sK62), sK62)) | spl63_11502), inference(avatar_component_clause, [], [f134828])).
fof(f553761, plain, ((nil = cons(sK54(sK58, nil), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | (~ spl63_31 | ~ spl63_169 | ~ spl63_170 | ~ spl63_4740)), inference(forward_demodulation, [], [f553760, f1196])).
fof(f553760, plain, ((nil = cons(sK54(sK58, sK62), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | (~ spl63_31 | ~ spl63_169 | ~ spl63_170 | ~ spl63_4740)), inference(subsumption_resolution, [], [f553759, f5160])).
fof(f5160, plain, (frontsegP(nil, nil) | (~ spl63_169 | ~ spl63_170)), inference(backward_demodulation, [], [f5107, f5101])).
fof(f5101, plain, ((nil = sK13(nil, nil)) | ~ spl63_169), inference(avatar_component_clause, [], [f5099])).
fof(f5099, plain, (spl63_169 <=> (nil = sK13(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_169])])).
fof(f5107, plain, (frontsegP(nil, sK13(nil, nil)) | ~ spl63_170), inference(avatar_component_clause, [], [f5105])).
fof(f5105, plain, (spl63_170 <=> frontsegP(nil, sK13(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_170])])).
fof(f553759, plain, (~ frontsegP(nil, nil) | (nil = cons(sK54(sK58, sK62), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | (~ spl63_31 | ~ spl63_4740)), inference(forward_demodulation, [], [f553758, f1196])).
fof(f553758, plain, (~ frontsegP(nil, sK62) | (nil = cons(sK54(sK58, sK62), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | ~ spl63_4740), inference(subsumption_resolution, [], [f272603, f452])).
fof(f272603, plain, (~ ssList(nil) | ~ frontsegP(nil, sK62) | (nil = cons(sK54(sK58, sK62), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | ~ spl63_4740), inference(resolution, [], [f61650, f492])).
fof(f492, plain, ! [X0] : (~ frontsegP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f331])).
fof(f331, plain, ! [X0] : (((frontsegP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ frontsegP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f159])).
fof(f159, plain, ! [X0] : ((frontsegP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f46])).
fof(f46, plain, ! [X0] : (ssList(X0) => (frontsegP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax46)).
fof(f61650, plain, (! [X16] : (frontsegP(X16, cons(sK54(sK58, sK62), nil)) | ~ ssList(X16) | ~ frontsegP(X16, sK62)) | ~ spl63_4740), inference(avatar_component_clause, [], [f61649])).
fof(f61649, plain, (spl63_4740 <=> ! [X16] : (~ frontsegP(X16, sK62) | ~ ssList(X16) | frontsegP(X16, cons(sK54(sK58, sK62), nil)))), introduced(avatar_definition, [new_symbols(naming, [spl63_4740])])).
fof(f533413, plain, (spl63_20915 | ~ spl63_1), inference(avatar_split_clause, [], [f50855, f621, f532534])).
fof(f532534, plain, (spl63_20915 <=> ssList(sK55(sK58, sK62))), introduced(avatar_definition, [new_symbols(naming, [spl63_20915])])).
fof(f50855, plain, (ssList(sK55(sK58, sK62)) | ~ spl63_1), inference(subsumption_resolution, [], [f50854, f644])).
fof(f50854, plain, (ssList(sK55(sK58, sK62)) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50853, f572])).
fof(f50853, plain, (ssList(sK55(sK58, sK62)) | ~ ssList(sK62) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50791, f623])).
fof(f50791, plain, (~ sP6(sK58, sK59) | ssList(sK55(sK58, sK62)) | ~ ssList(sK62) | ~ strictorderedP(sK58)), inference(superposition, [], [f611, f646])).
fof(f611, plain, ! [X2, X0] : (~ sP6(X0, app(X0, X2)) | ssList(sK55(X0, X2)) | ~ ssList(X2) | ~ strictorderedP(X0)), inference(equality_resolution, [], [f560])).
fof(f560, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | ssList(sK55(X0, X2)) | ~ (app(X0, X2) = X1) | ~ ssList(X2) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f352])).
fof(f532982, plain, (~ spl63_4714 | ~ spl63_20915 | spl63_6661 | ~ spl63_31 | ~ spl63_1), inference(avatar_split_clause, [], [f61469, f621, f1194, f84538, f532534, f61508])).
fof(f61469, plain, (~ (nil = sK62) | (nil = cons(sK54(sK58, sK62), nil)) | ~ ssList(sK55(sK58, sK62)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | ~ spl63_1), inference(superposition, [], [f543, f50852])).
fof(f50852, plain, ((sK62 = app(cons(sK54(sK58, sK62), nil), sK55(sK58, sK62))) | ~ spl63_1), inference(subsumption_resolution, [], [f50851, f644])).
fof(f50851, plain, ((sK62 = app(cons(sK54(sK58, sK62), nil), sK55(sK58, sK62))) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50850, f572])).
fof(f50850, plain, ((sK62 = app(cons(sK54(sK58, sK62), nil), sK55(sK58, sK62))) | ~ ssList(sK62) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50790, f623])).
fof(f50790, plain, (~ sP6(sK58, sK59) | (sK62 = app(cons(sK54(sK58, sK62), nil), sK55(sK58, sK62))) | ~ ssList(sK62) | ~ strictorderedP(sK58)), inference(superposition, [], [f610, f646])).
fof(f610, plain, ! [X2, X0] : (~ sP6(X0, app(X0, X2)) | (app(cons(sK54(X0, X2), nil), sK55(X0, X2)) = X2) | ~ ssList(X2) | ~ strictorderedP(X0)), inference(equality_resolution, [], [f561])).
fof(f561, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | (app(cons(sK54(X0, X2), nil), sK55(X0, X2)) = X2) | ~ (app(X0, X2) = X1) | ~ ssList(X2) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f352])).
fof(f543, plain, ! [X0, X1] : (~ (nil = app(X0, X1)) | (nil = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f343])).
fof(f343, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | ~ (nil = X0) | ~ (nil = X1)) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f342])).
fof(f342, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | (~ (nil = X0) | ~ (nil = X1))) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f201])).
fof(f201, plain, ! [X0] : (! [X1] : (((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f83])).
fof(f83, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax83)).
fof(f531532, plain, (~ spl63_1 | ~ spl63_4236 | ~ spl63_4310 | ~ spl63_4723), inference(avatar_contradiction_clause, [], [f531531])).
fof(f531531, plain, ($false | (~ spl63_1 | ~ spl63_4236 | ~ spl63_4310 | ~ spl63_4723)), inference(subsumption_resolution, [], [f531530, f306629])).
fof(f306629, plain, (lt(sK56(sK58, sK62), hd(sK62)) | (~ spl63_1 | ~ spl63_4310 | ~ spl63_4723)), inference(backward_demodulation, [], [f264115, f306389])).
fof(f306389, plain, ((hd(sK62) = sK54(sK58, sK62)) | (~ spl63_1 | ~ spl63_4310 | ~ spl63_4723)), inference(subsumption_resolution, [], [f306388, f452])).
fof(f306388, plain, (~ ssList(nil) | (hd(sK62) = sK54(sK58, sK62)) | (~ spl63_1 | ~ spl63_4310 | ~ spl63_4723)), inference(subsumption_resolution, [], [f306363, f50858])).
fof(f306363, plain, (~ ssItem(sK54(sK58, sK62)) | ~ ssList(nil) | (hd(sK62) = sK54(sK58, sK62)) | (~ spl63_4310 | ~ spl63_4723)), inference(resolution, [], [f57577, f61559])).
fof(f61559, plain, (frontsegP(sK62, cons(sK54(sK58, sK62), nil)) | ~ spl63_4723), inference(avatar_component_clause, [], [f61557])).
fof(f61557, plain, (spl63_4723 <=> frontsegP(sK62, cons(sK54(sK58, sK62), nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_4723])])).
fof(f57577, plain, (! [X12, X13] : (~ frontsegP(sK62, cons(X12, X13)) | ~ ssItem(X12) | ~ ssList(X13) | (hd(sK62) = X12)) | ~ spl63_4310), inference(avatar_component_clause, [], [f57576])).
fof(f57576, plain, (spl63_4310 <=> ! [X13, X12] : (~ frontsegP(sK62, cons(X12, X13)) | ~ ssItem(X12) | ~ ssList(X13) | (hd(sK62) = X12))), introduced(avatar_definition, [new_symbols(naming, [spl63_4310])])).
fof(f264115, plain, (lt(sK56(sK58, sK62), sK54(sK58, sK62)) | ~ spl63_1), inference(subsumption_resolution, [], [f264114, f50849])).
fof(f50849, plain, (ssItem(sK56(sK58, sK62)) | ~ spl63_1), inference(subsumption_resolution, [], [f50848, f644])).
fof(f50848, plain, (ssItem(sK56(sK58, sK62)) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50847, f572])).
fof(f50847, plain, (ssItem(sK56(sK58, sK62)) | ~ ssList(sK62) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50789, f623])).
fof(f50789, plain, (~ sP6(sK58, sK59) | ssItem(sK56(sK58, sK62)) | ~ ssList(sK62) | ~ strictorderedP(sK58)), inference(superposition, [], [f609, f646])).
fof(f609, plain, ! [X2, X0] : (~ sP6(X0, app(X0, X2)) | ssItem(sK56(X0, X2)) | ~ ssList(X2) | ~ strictorderedP(X0)), inference(equality_resolution, [], [f562])).
fof(f562, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | ssItem(sK56(X0, X2)) | ~ (app(X0, X2) = X1) | ~ ssList(X2) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f352])).
fof(f264114, plain, (lt(sK56(sK58, sK62), sK54(sK58, sK62)) | ~ ssItem(sK56(sK58, sK62)) | ~ spl63_1), inference(duplicate_literal_removal, [], [f264113])).
fof(f264113, plain, (lt(sK56(sK58, sK62), sK54(sK58, sK62)) | ~ ssItem(sK56(sK58, sK62)) | ~ ssItem(sK56(sK58, sK62)) | ~ spl63_1), inference(resolution, [], [f50895, f469])).
fof(f469, plain, ! [X0] : (leq(X0, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0] : (leq(X0, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (ssItem(X0) => leq(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax31)).
fof(f50895, plain, (! [X20] : (~ leq(X20, sK56(sK58, sK62)) | lt(X20, sK54(sK58, sK62)) | ~ ssItem(X20)) | ~ spl63_1), inference(subsumption_resolution, [], [f50894, f644])).
fof(f50894, plain, (! [X20] : (~ strictorderedP(sK58) | lt(X20, sK54(sK58, sK62)) | ~ leq(X20, sK56(sK58, sK62)) | ~ ssItem(X20)) | ~ spl63_1), inference(subsumption_resolution, [], [f50893, f572])).
fof(f50893, plain, (! [X20] : (~ ssList(sK62) | ~ strictorderedP(sK58) | lt(X20, sK54(sK58, sK62)) | ~ leq(X20, sK56(sK58, sK62)) | ~ ssItem(X20)) | ~ spl63_1), inference(subsumption_resolution, [], [f50805, f623])).
fof(f50805, plain, ! [X20] : (~ sP6(sK58, sK59) | ~ ssList(sK62) | ~ strictorderedP(sK58) | lt(X20, sK54(sK58, sK62)) | ~ leq(X20, sK56(sK58, sK62)) | ~ ssItem(X20)), inference(superposition, [], [f2207, f646])).
fof(f2207, plain, ! [X2, X0, X1] : (~ sP6(X0, app(X0, X1)) | ~ ssList(X1) | ~ strictorderedP(X0) | lt(X2, sK54(X0, X1)) | ~ leq(X2, sK56(X0, X1)) | ~ ssItem(X2)), inference(subsumption_resolution, [], [f2206, f612])).
fof(f2206, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | ~ ssList(X1) | ~ sP6(X0, app(X0, X1)) | lt(X2, sK54(X0, X1)) | ~ leq(X2, sK56(X0, X1)) | ~ ssItem(sK54(X0, X1)) | ~ ssItem(X2)), inference(subsumption_resolution, [], [f2201, f609])).
fof(f2201, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | ~ ssList(X1) | ~ sP6(X0, app(X0, X1)) | lt(X2, sK54(X0, X1)) | ~ leq(X2, sK56(X0, X1)) | ~ ssItem(sK54(X0, X1)) | ~ ssItem(sK56(X0, X1)) | ~ ssItem(X2)), inference(resolution, [], [f606, f552])).
fof(f552, plain, ! [X2, X0, X1] : (~ lt(X1, X2) | lt(X0, X2) | ~ leq(X0, X1) | ~ ssItem(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f214])).
fof(f214, plain, ! [X0] : (! [X1] : (! [X2] : (lt(X0, X2) | ~ lt(X1, X2) | ~ leq(X0, X1) | ~ ssItem(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f213])).
fof(f213, plain, ! [X0] : (! [X1] : (! [X2] : ((lt(X0, X2) | (~ lt(X1, X2) | ~ leq(X0, X1))) | ~ ssItem(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f91])).
fof(f91, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ((lt(X1, X2) & leq(X0, X1)) => lt(X0, X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax91)).
fof(f606, plain, ! [X2, X0] : (lt(sK56(X0, X2), sK54(X0, X2)) | ~ strictorderedP(X0) | ~ ssList(X2) | ~ sP6(X0, app(X0, X2))), inference(equality_resolution, [], [f565])).
fof(f565, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | lt(sK56(X0, X2), sK54(X0, X2)) | ~ (app(X0, X2) = X1) | ~ ssList(X2) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f352])).
fof(f531530, plain, (~ lt(sK56(sK58, sK62), hd(sK62)) | (~ spl63_1 | ~ spl63_4236)), inference(subsumption_resolution, [], [f531529, f50846])).
fof(f50846, plain, (ssList(sK57(sK58, sK62)) | ~ spl63_1), inference(subsumption_resolution, [], [f50845, f644])).
fof(f50845, plain, (ssList(sK57(sK58, sK62)) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50844, f572])).
fof(f50844, plain, (ssList(sK57(sK58, sK62)) | ~ ssList(sK62) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50788, f623])).
fof(f50788, plain, (~ sP6(sK58, sK59) | ssList(sK57(sK58, sK62)) | ~ ssList(sK62) | ~ strictorderedP(sK58)), inference(superposition, [], [f608, f646])).
fof(f608, plain, ! [X2, X0] : (~ sP6(X0, app(X0, X2)) | ssList(sK57(X0, X2)) | ~ ssList(X2) | ~ strictorderedP(X0)), inference(equality_resolution, [], [f563])).
fof(f563, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | ssList(sK57(X0, X2)) | ~ (app(X0, X2) = X1) | ~ ssList(X2) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f352])).
fof(f531529, plain, (~ ssList(sK57(sK58, sK62)) | ~ lt(sK56(sK58, sK62), hd(sK62)) | (~ spl63_1 | ~ spl63_4236)), inference(subsumption_resolution, [], [f531513, f50849])).
fof(f531513, plain, (~ ssItem(sK56(sK58, sK62)) | ~ ssList(sK57(sK58, sK62)) | ~ lt(sK56(sK58, sK62), hd(sK62)) | (~ spl63_1 | ~ spl63_4236)), inference(trivial_inequality_removal, [], [f531512])).
fof(f531512, plain, (~ (sK58 = sK58) | ~ ssItem(sK56(sK58, sK62)) | ~ ssList(sK57(sK58, sK62)) | ~ lt(sK56(sK58, sK62), hd(sK62)) | (~ spl63_1 | ~ spl63_4236)), inference(superposition, [], [f56380, f50843])).
fof(f50843, plain, ((sK58 = app(sK57(sK58, sK62), cons(sK56(sK58, sK62), nil))) | ~ spl63_1), inference(subsumption_resolution, [], [f50842, f644])).
fof(f50842, plain, ((sK58 = app(sK57(sK58, sK62), cons(sK56(sK58, sK62), nil))) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50841, f572])).
fof(f50841, plain, ((sK58 = app(sK57(sK58, sK62), cons(sK56(sK58, sK62), nil))) | ~ ssList(sK62) | ~ strictorderedP(sK58) | ~ spl63_1), inference(subsumption_resolution, [], [f50787, f623])).
fof(f50787, plain, (~ sP6(sK58, sK59) | (sK58 = app(sK57(sK58, sK62), cons(sK56(sK58, sK62), nil))) | ~ ssList(sK62) | ~ strictorderedP(sK58)), inference(superposition, [], [f607, f646])).
fof(f607, plain, ! [X2, X0] : (~ sP6(X0, app(X0, X2)) | (app(sK57(X0, X2), cons(sK56(X0, X2), nil)) = X0) | ~ ssList(X2) | ~ strictorderedP(X0)), inference(equality_resolution, [], [f564])).
fof(f564, plain, ! [X2, X0, X1] : (~ strictorderedP(X0) | (app(sK57(X0, X2), cons(sK56(X0, X2), nil)) = X0) | ~ (app(X0, X2) = X1) | ~ ssList(X2) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f352])).
fof(f56380, plain, (! [X2, X1] : (~ (sK58 = app(X2, cons(X1, nil))) | ~ ssItem(X1) | ~ ssList(X2) | ~ lt(X1, hd(sK62))) | ~ spl63_4236), inference(avatar_component_clause, [], [f56379])).
fof(f56379, plain, (spl63_4236 <=> ! [X1, X2] : (~ lt(X1, hd(sK62)) | ~ ssItem(X1) | ~ ssList(X2) | ~ (sK58 = app(X2, cons(X1, nil))))), introduced(avatar_definition, [new_symbols(naming, [spl63_4236])])).
fof(f307200, plain, (spl63_4236 | ~ spl63_1 | ~ spl63_4310 | ~ spl63_4723), inference(avatar_split_clause, [], [f306434, f61557, f57576, f621, f56379])).
fof(f306434, plain, (! [X0, X1] : (~ lt(X0, hd(sK62)) | ~ ssList(X1) | ~ ssItem(X0) | ~ (sK58 = app(X1, cons(X0, nil)))) | (~ spl63_1 | ~ spl63_4310 | ~ spl63_4723)), inference(backward_demodulation, [], [f61505, f306389])).
fof(f61505, plain, (! [X0, X1] : (~ lt(X0, sK54(sK58, sK62)) | ~ ssList(X1) | ~ ssItem(X0) | ~ (sK58 = app(X1, cons(X0, nil)))) | ~ spl63_1), inference(subsumption_resolution, [], [f61504, f50858])).
fof(f61504, plain, (! [X0, X1] : (~ lt(X0, sK54(sK58, sK62)) | ~ ssList(X1) | ~ ssItem(X0) | ~ (sK58 = app(X1, cons(X0, nil))) | ~ ssItem(sK54(sK58, sK62))) | ~ spl63_1), inference(subsumption_resolution, [], [f61503, f50855])).
fof(f61503, plain, (! [X0, X1] : (~ lt(X0, sK54(sK58, sK62)) | ~ ssList(X1) | ~ ssItem(X0) | ~ (sK58 = app(X1, cons(X0, nil))) | ~ ssList(sK55(sK58, sK62)) | ~ ssItem(sK54(sK58, sK62))) | ~ spl63_1), inference(trivial_inequality_removal, [], [f61456])).
fof(f61456, plain, (! [X0, X1] : (~ (sK62 = sK62) | ~ lt(X0, sK54(sK58, sK62)) | ~ ssList(X1) | ~ ssItem(X0) | ~ (sK58 = app(X1, cons(X0, nil))) | ~ ssList(sK55(sK58, sK62)) | ~ ssItem(sK54(sK58, sK62))) | ~ spl63_1), inference(superposition, [], [f645, f50852])).
fof(f645, plain, ! [X6, X8, X7, X5] : (~ (app(cons(X5, nil), X6) = sK62) | ~ lt(X7, X5) | ~ ssList(X8) | ~ ssItem(X7) | ~ (app(X8, cons(X7, nil)) = sK58) | ~ ssList(X6) | ~ ssItem(X5)), inference(backward_demodulation, [], [f575, f571])).
fof(f575, plain, ! [X6, X8, X7, X5] : (~ lt(X7, X5) | ~ (app(X8, cons(X7, nil)) = sK60) | ~ ssList(X8) | ~ ssItem(X7) | ~ (app(cons(X5, nil), X6) = sK62) | ~ ssList(X6) | ~ ssItem(X5)), inference(cnf_transformation, [], [f359])).
fof(f85626, plain, (~ spl63_1 | spl63_43 | ~ spl63_6661), inference(avatar_contradiction_clause, [], [f85625])).
fof(f85625, plain, ($false | (~ spl63_1 | spl63_43 | ~ spl63_6661)), inference(subsumption_resolution, [], [f85594, f1737])).
fof(f1737, plain, (~ ssItem(hd(nil)) | spl63_43), inference(avatar_component_clause, [], [f1735])).
fof(f1735, plain, (spl63_43 <=> ssItem(hd(nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_43])])).
fof(f85594, plain, (ssItem(hd(nil)) | (~ spl63_1 | ~ spl63_6661)), inference(backward_demodulation, [], [f50858, f85479])).
fof(f85479, plain, ((hd(nil) = sK54(sK58, sK62)) | (~ spl63_1 | ~ spl63_6661)), inference(backward_demodulation, [], [f54766, f84540])).
fof(f84098, plain, (~ spl63_1 | spl63_4714), inference(avatar_contradiction_clause, [], [f84097])).
fof(f84097, plain, ($false | (~ spl63_1 | spl63_4714)), inference(subsumption_resolution, [], [f84096, f452])).
fof(f84096, plain, (~ ssList(nil) | (~ spl63_1 | spl63_4714)), inference(subsumption_resolution, [], [f84095, f50858])).
fof(f84095, plain, (~ ssItem(sK54(sK58, sK62)) | ~ ssList(nil) | spl63_4714), inference(resolution, [], [f61510, f451])).
fof(f451, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax16)).
fof(f61510, plain, (~ ssList(cons(sK54(sK58, sK62), nil)) | spl63_4714), inference(avatar_component_clause, [], [f61508])).
fof(f61651, plain, (~ spl63_4714 | spl63_4740 | ~ spl63_1), inference(avatar_split_clause, [], [f61647, f621, f61649, f61508])).
fof(f61647, plain, (! [X16] : (~ frontsegP(X16, sK62) | frontsegP(X16, cons(sK54(sK58, sK62), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | ~ ssList(X16)) | ~ spl63_1), inference(subsumption_resolution, [], [f61487, f50855])).
fof(f61487, plain, (! [X16] : (~ frontsegP(X16, sK62) | frontsegP(X16, cons(sK54(sK58, sK62), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | ~ ssList(X16) | ~ ssList(sK55(sK58, sK62))) | ~ spl63_1), inference(superposition, [], [f2103, f50852])).
fof(f2103, plain, ! [X6, X4, X5] : (~ frontsegP(X4, app(X5, X6)) | frontsegP(X4, X5) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X6)), inference(subsumption_resolution, [], [f2100, f464])).
fof(f464, plain, ! [X0, X1] : (ssList(app(X0, X1)) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : (ssList(app(X0, X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ssList(app(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax26)).
fof(f2100, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6)), inference(duplicate_literal_removal, [], [f2097])).
fof(f2097, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6) | ~ ssList(X5) | ~ ssList(app(X5, X6))), inference(resolution, [], [f484, f582])).
fof(f582, plain, ! [X2, X1] : (frontsegP(app(X1, X2), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X1, X2))), inference(equality_resolution, [], [f374])).
fof(f374, plain, ! [X2, X0, X1] : (frontsegP(X0, X1) | ~ (app(X1, X2) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (((app(X1, sK12(X0, X1)) = X0) & ssList(sK12(X0, X1))) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f249, f250])).
fof(f250, plain, ! [X1, X0] : (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) => ((app(X1, sK12(X0, X1)) = X0) & ssList(sK12(X0, X1)))), introduced(choice_axiom, [])).
fof(f249, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f248])).
fof(f248, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X1, X2) = X0) & ssList(X2)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (! [X1] : ((frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax5)).
fof(f484, plain, ! [X2, X0, X1] : (~ frontsegP(X1, X2) | frontsegP(X0, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(X0, X2) | ~ frontsegP(X1, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f150])).
fof(f150, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(X0, X2) | (~ frontsegP(X1, X2) | ~ frontsegP(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((frontsegP(X1, X2) & frontsegP(X0, X1)) => frontsegP(X0, X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax40)).
fof(f61560, plain, (~ spl63_4714 | spl63_4723 | ~ spl63_1), inference(avatar_split_clause, [], [f61555, f621, f61557, f61508])).
fof(f61555, plain, (frontsegP(sK62, cons(sK54(sK58, sK62), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | ~ spl63_1), inference(subsumption_resolution, [], [f61554, f572])).
fof(f61554, plain, (frontsegP(sK62, cons(sK54(sK58, sK62), nil)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | ~ ssList(sK62) | ~ spl63_1), inference(subsumption_resolution, [], [f61470, f50855])).
fof(f61470, plain, (frontsegP(sK62, cons(sK54(sK58, sK62), nil)) | ~ ssList(sK55(sK58, sK62)) | ~ ssList(cons(sK54(sK58, sK62), nil)) | ~ ssList(sK62) | ~ spl63_1), inference(superposition, [], [f582, f50852])).
fof(f59388, plain, (spl63_31 | spl63_4303), inference(avatar_contradiction_clause, [], [f59387])).
fof(f59387, plain, ($false | (spl63_31 | spl63_4303)), inference(subsumption_resolution, [], [f59386, f572])).
fof(f59386, plain, (~ ssList(sK62) | (spl63_31 | spl63_4303)), inference(subsumption_resolution, [], [f59385, f1195])).
fof(f1195, plain, (~ (nil = sK62) | spl63_31), inference(avatar_component_clause, [], [f1194])).
fof(f59385, plain, ((nil = sK62) | ~ ssList(sK62) | spl63_4303), inference(resolution, [], [f57547, f460])).
fof(f460, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax22)).
fof(f57547, plain, (~ ssItem(hd(sK62)) | spl63_4303), inference(avatar_component_clause, [], [f57545])).
fof(f57545, plain, (spl63_4303 <=> ssItem(hd(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl63_4303])])).
fof(f58250, plain, (spl63_31 | spl63_4302), inference(avatar_contradiction_clause, [], [f58249])).
fof(f58249, plain, ($false | (spl63_31 | spl63_4302)), inference(subsumption_resolution, [], [f58248, f572])).
fof(f58248, plain, (~ ssList(sK62) | (spl63_31 | spl63_4302)), inference(subsumption_resolution, [], [f58247, f1195])).
fof(f58247, plain, ((nil = sK62) | ~ ssList(sK62) | spl63_4302), inference(resolution, [], [f57543, f462])).
fof(f462, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax24)).
fof(f57543, plain, (~ ssList(tl(sK62)) | spl63_4302), inference(avatar_component_clause, [], [f57541])).
fof(f57541, plain, (spl63_4302 <=> ssList(tl(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl63_4302])])).
fof(f57578, plain, (~ spl63_4303 | ~ spl63_4302 | spl63_4310 | spl63_31), inference(avatar_split_clause, [], [f57504, f1194, f57576, f57541, f57545])).
fof(f57504, plain, (! [X12, X13] : (~ frontsegP(sK62, cons(X12, X13)) | (hd(sK62) = X12) | ~ ssList(X13) | ~ ssList(tl(sK62)) | ~ ssItem(X12) | ~ ssItem(hd(sK62))) | spl63_31), inference(superposition, [], [f488, f51806])).
fof(f51806, plain, ((sK62 = cons(hd(sK62), tl(sK62))) | spl63_31), inference(subsumption_resolution, [], [f51537, f1195])).
fof(f51537, plain, ((nil = sK62) | (sK62 = cons(hd(sK62), tl(sK62)))), inference(resolution, [], [f572, f537])).
fof(f537, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax78)).
fof(f488, plain, ! [X2, X0, X3, X1] : (~ frontsegP(cons(X0, X2), cons(X1, X3)) | (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f330])).
fof(f330, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1)) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f329])).
fof(f329, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | (~ frontsegP(X2, X3) | ~ (X0 = X1))) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f157])).
fof(f157, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f44])).
fof(f44, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax44)).
fof(f46546, plain, (~ spl63_43 | ~ spl63_180), inference(avatar_split_clause, [], [f46545, f5209, f1735])).
fof(f46545, plain, (~ ssItem(hd(nil)) | ~ spl63_180), inference(subsumption_resolution, [], [f46544, f483])).
fof(f483, plain, ~ singletonP(nil), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ~ singletonP(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax39)).
fof(f46544, plain, (~ ssItem(hd(nil)) | singletonP(nil) | ~ spl63_180), inference(subsumption_resolution, [], [f9629, f452])).
fof(f9629, plain, (~ ssList(nil) | ~ ssItem(hd(nil)) | singletonP(nil) | ~ spl63_180), inference(superposition, [], [f581, f5211])).
fof(f5211, plain, ((nil = cons(hd(nil), nil)) | ~ spl63_180), inference(avatar_component_clause, [], [f5209])).
fof(f581, plain, ! [X1] : (~ ssList(cons(X1, nil)) | ~ ssItem(X1) | singletonP(cons(X1, nil))), inference(equality_resolution, [], [f371])).
fof(f371, plain, ! [X0, X1] : (singletonP(X0) | ~ (cons(X1, nil) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f247])).
fof(f247, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0))) | ~ singletonP(X0))) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f245, f246])).
fof(f246, plain, ! [X0] : (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) => ((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0)))), introduced(choice_axiom, [])).
fof(f245, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(rectify, [], [f244])).
fof(f244, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f101])).
fof(f101, plain, ! [X0] : ((singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : (ssList(X0) => (singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax4)).
fof(f5159, plain, (~ spl63_156 | ~ spl63_157 | spl63_165), inference(avatar_contradiction_clause, [], [f5158])).
fof(f5158, plain, ($false | (~ spl63_156 | ~ spl63_157 | spl63_165)), inference(subsumption_resolution, [], [f5157, f452])).
fof(f5157, plain, (~ ssList(nil) | (~ spl63_156 | ~ spl63_157 | spl63_165)), inference(subsumption_resolution, [], [f5156, f4973])).
fof(f4973, plain, (rearsegP(nil, nil) | (~ spl63_156 | ~ spl63_157)), inference(backward_demodulation, [], [f4866, f4860])).
fof(f4860, plain, ((nil = sK12(nil, nil)) | ~ spl63_156), inference(avatar_component_clause, [], [f4858])).
fof(f4858, plain, (spl63_156 <=> (nil = sK12(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_156])])).
fof(f4866, plain, (rearsegP(nil, sK12(nil, nil)) | ~ spl63_157), inference(avatar_component_clause, [], [f4864])).
fof(f4864, plain, (spl63_157 <=> rearsegP(nil, sK12(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_157])])).
fof(f5156, plain, (~ rearsegP(nil, nil) | ~ ssList(nil) | spl63_165), inference(duplicate_literal_removal, [], [f5155])).
fof(f5155, plain, (~ rearsegP(nil, nil) | ~ ssList(nil) | ~ ssList(nil) | spl63_165), inference(resolution, [], [f5080, f375])).
fof(f375, plain, ! [X0, X1] : (ssList(sK13(X0, X1)) | ~ rearsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f255])).
fof(f255, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (((app(sK13(X0, X1), X1) = X0) & ssList(sK13(X0, X1))) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f253, f254])).
fof(f254, plain, ! [X1, X0] : (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) => ((app(sK13(X0, X1), X1) = X0) & ssList(sK13(X0, X1)))), introduced(choice_axiom, [])).
fof(f253, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f252])).
fof(f252, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X2, X1) = X0) & ssList(X2)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f103])).
fof(f103, plain, ! [X0] : (! [X1] : ((rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax6)).
fof(f5080, plain, (~ ssList(sK13(nil, nil)) | spl63_165), inference(avatar_component_clause, [], [f5078])).
fof(f5078, plain, (spl63_165 <=> ssList(sK13(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_165])])).
fof(f5108, plain, (~ spl63_165 | spl63_170), inference(avatar_split_clause, [], [f5103, f5105, f5078])).
fof(f5103, plain, (frontsegP(nil, sK13(nil, nil)) | ~ ssList(sK13(nil, nil))), inference(subsumption_resolution, [], [f5073, f452])).
fof(f5073, plain, (frontsegP(nil, sK13(nil, nil)) | ~ ssList(nil) | ~ ssList(sK13(nil, nil))), inference(duplicate_literal_removal, [], [f5060])).
fof(f5060, plain, (frontsegP(nil, sK13(nil, nil)) | ~ ssList(nil) | ~ ssList(sK13(nil, nil)) | ~ ssList(nil)), inference(superposition, [], [f582, f3370])).
fof(f3370, plain, (nil = app(sK13(nil, nil), nil)), inference(resolution, [], [f1995, f452])).
fof(f1995, plain, ! [X0] : (~ ssList(X0) | (app(sK13(X0, X0), X0) = X0)), inference(duplicate_literal_removal, [], [f1990])).
fof(f1990, plain, ! [X0] : ((app(sK13(X0, X0), X0) = X0) | ~ ssList(X0) | ~ ssList(X0) | ~ ssList(X0)), inference(resolution, [], [f376, f496])).
fof(f496, plain, ! [X0] : (rearsegP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : (rearsegP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (ssList(X0) => rearsegP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax49)).
fof(f376, plain, ! [X0, X1] : (~ rearsegP(X0, X1) | (app(sK13(X0, X1), X1) = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f255])).
fof(f5102, plain, (~ spl63_165 | spl63_169), inference(avatar_split_clause, [], [f5097, f5099, f5078])).
fof(f5097, plain, ((nil = sK13(nil, nil)) | ~ ssList(sK13(nil, nil))), inference(subsumption_resolution, [], [f5074, f452])).
fof(f5074, plain, ((nil = sK13(nil, nil)) | ~ ssList(nil) | ~ ssList(sK13(nil, nil))), inference(trivial_inequality_removal, [], [f5059])).
fof(f5059, plain, (~ (nil = nil) | (nil = sK13(nil, nil)) | ~ ssList(nil) | ~ ssList(sK13(nil, nil))), inference(superposition, [], [f543, f3370])).
fof(f4971, plain, spl63_151, inference(avatar_contradiction_clause, [], [f4970])).
fof(f4970, plain, ($false | spl63_151), inference(subsumption_resolution, [], [f4967, f452])).
fof(f4967, plain, (~ ssList(nil) | spl63_151), inference(resolution, [], [f4941, f486])).
fof(f486, plain, ! [X0] : (frontsegP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f154])).
fof(f154, plain, ! [X0] : (frontsegP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f42])).
fof(f42, plain, ! [X0] : (ssList(X0) => frontsegP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC343+1.p', ax42)).
fof(f4941, plain, (~ frontsegP(nil, nil) | spl63_151), inference(subsumption_resolution, [], [f4940, f452])).
fof(f4940, plain, (~ frontsegP(nil, nil) | ~ ssList(nil) | spl63_151), inference(duplicate_literal_removal, [], [f4939])).
fof(f4939, plain, (~ frontsegP(nil, nil) | ~ ssList(nil) | ~ ssList(nil) | spl63_151), inference(resolution, [], [f4834, f372])).
fof(f372, plain, ! [X0, X1] : (ssList(sK12(X0, X1)) | ~ frontsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f251])).
fof(f4834, plain, (~ ssList(sK12(nil, nil)) | spl63_151), inference(avatar_component_clause, [], [f4832])).
fof(f4832, plain, (spl63_151 <=> ssList(sK12(nil, nil))), introduced(avatar_definition, [new_symbols(naming, [spl63_151])])).
fof(f4867, plain, (~ spl63_151 | spl63_157), inference(avatar_split_clause, [], [f4862, f4864, f4832])).
fof(f4862, plain, (rearsegP(nil, sK12(nil, nil)) | ~ ssList(sK12(nil, nil))), inference(subsumption_resolution, [], [f4826, f452])).
fof(f4826, plain, (rearsegP(nil, sK12(nil, nil)) | ~ ssList(nil) | ~ ssList(sK12(nil, nil))), inference(duplicate_literal_removal, [], [f4815])).
fof(f4815, plain, (rearsegP(nil, sK12(nil, nil)) | ~ ssList(nil) | ~ ssList(sK12(nil, nil)) | ~ ssList(nil)), inference(superposition, [], [f583, f3271])).
fof(f3271, plain, (nil = app(nil, sK12(nil, nil))), inference(resolution, [], [f1941, f452])).
fof(f1941, plain, ! [X0] : (~ ssList(X0) | (app(X0, sK12(X0, X0)) = X0)), inference(duplicate_literal_removal, [], [f1936])).
fof(f1936, plain, ! [X0] : ((app(X0, sK12(X0, X0)) = X0) | ~ ssList(X0) | ~ ssList(X0) | ~ ssList(X0)), inference(resolution, [], [f373, f486])).
fof(f373, plain, ! [X0, X1] : (~ frontsegP(X0, X1) | (app(X1, sK12(X0, X1)) = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f251])).
fof(f583, plain, ! [X2, X1] : (rearsegP(app(X2, X1), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X2, X1))), inference(equality_resolution, [], [f377])).
fof(f377, plain, ! [X2, X0, X1] : (rearsegP(X0, X1) | ~ (app(X2, X1) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f255])).
fof(f4861, plain, (~ spl63_151 | spl63_156), inference(avatar_split_clause, [], [f4856, f4858, f4832])).
fof(f4856, plain, ((nil = sK12(nil, nil)) | ~ ssList(sK12(nil, nil))), inference(subsumption_resolution, [], [f4828, f452])).
fof(f4828, plain, ((nil = sK12(nil, nil)) | ~ ssList(sK12(nil, nil)) | ~ ssList(nil)), inference(trivial_inequality_removal, [], [f4812])).
fof(f4812, plain, (~ (nil = nil) | (nil = sK12(nil, nil)) | ~ ssList(sK12(nil, nil)) | ~ ssList(nil)), inference(superposition, [], [f542, f3271])).
fof(f542, plain, ! [X0, X1] : (~ (nil = app(X0, X1)) | (nil = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f343])).
fof(f1579, plain, (spl63_2 | ~ spl63_4), inference(avatar_split_clause, [], [f1537, f635, f625])).
fof(f625, plain, (spl63_2 <=> (nil = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl63_2])])).
fof(f635, plain, (spl63_4 <=> (nil = sK61)), introduced(avatar_definition, [new_symbols(naming, [spl63_4])])).
fof(f1537, plain, ((nil = sK59) | ~ spl63_4), inference(forward_demodulation, [], [f570, f637])).
fof(f637, plain, ((nil = sK61) | ~ spl63_4), inference(avatar_component_clause, [], [f635])).
fof(f655, plain, (~ spl63_3 | spl63_5), inference(avatar_contradiction_clause, [], [f654])).
fof(f654, plain, ($false | (~ spl63_3 | spl63_5)), inference(subsumption_resolution, [], [f653, f641])).
fof(f641, plain, (~ (nil = sK60) | spl63_5), inference(avatar_component_clause, [], [f639])).
fof(f639, plain, (spl63_5 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl63_5])])).
fof(f653, plain, ((nil = sK60) | ~ spl63_3), inference(backward_demodulation, [], [f571, f632])).
fof(f632, plain, ((nil = sK58) | ~ spl63_3), inference(avatar_component_clause, [], [f630])).
fof(f630, plain, (spl63_3 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl63_3])])).
fof(f642, plain, (spl63_4 | ~ spl63_5), inference(avatar_split_clause, [], [f576, f639, f635])).
fof(f576, plain, (~ (nil = sK60) | (nil = sK61)), inference(cnf_transformation, [], [f359])).
fof(f633, plain, (spl63_1 | spl63_3), inference(avatar_split_clause, [], [f577, f630, f621])).
fof(f577, plain, ((nil = sK58) | sP6(sK58, sK59)), inference(cnf_transformation, [], [f359])).
fof(f628, plain, (spl63_1 | ~ spl63_2), inference(avatar_split_clause, [], [f578, f625, f621])).
fof(f578, plain, (~ (nil = sK59) | sP6(sK58, sK59)), inference(cnf_transformation, [], [f359])).