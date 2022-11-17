fof(f7597, plain, $false, inference(avatar_sat_refutation, [], [f625, f630, f640, f645, f1067, f1312, f1367, f1449, f4492, f4567, f7115, f7120, f7201, f7203, f7499])).
fof(f7499, plain, (~ spl61_1 | ~ spl61_29), inference(avatar_contradiction_clause, [], [f7498])).
fof(f7498, plain, ($false | (~ spl61_1 | ~ spl61_29)), inference(subsumption_resolution, [], [f7222, f479])).
fof(f479, plain, ~ singletonP(nil), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ~ singletonP(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC109+1.p', ax39)).
fof(f7222, plain, (singletonP(nil) | (~ spl61_1 | ~ spl61_29)), inference(backward_demodulation, [], [f1309, f610])).
fof(f610, plain, ((nil = sK57) | ~ spl61_1), inference(avatar_component_clause, [], [f609])).
fof(f609, plain, (spl61_1 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_1])])).
fof(f1309, plain, (singletonP(sK57) | ~ spl61_29), inference(avatar_component_clause, [], [f1307])).
fof(f1307, plain, (spl61_29 <=> singletonP(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_29])])).
fof(f7203, plain, (spl61_1 | ~ spl61_7), inference(avatar_split_clause, [], [f7202, f637, f609])).
fof(f637, plain, (spl61_7 <=> (nil = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_7])])).
fof(f7202, plain, ((nil = sK57) | ~ spl61_7), inference(backward_demodulation, [], [f567, f639])).
fof(f639, plain, ((nil = sK59) | ~ spl61_7), inference(avatar_component_clause, [], [f637])).
fof(f567, plain, (sK57 = sK59), inference(cnf_transformation, [], [f355])).
fof(f355, plain, (((((((~ segmentP(sK58, sK57) | ~ neq(sK57, nil)) & neq(sK58, nil)) | (~ (nil = sK57) & (nil = sK58))) & (((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60])], [f233, f354, f353, f352, f351])).
fof(f351, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ segmentP(X1, X0) | ~ neq(X0, nil)) & neq(X1, nil)) | (~ (nil = X0) & (nil = X1))) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((((~ segmentP(X1, sK57) | ~ neq(sK57, nil)) & neq(X1, nil)) | (~ (nil = sK57) & (nil = X1))) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X1] : (? [X2] : (? [X3] : ((((~ segmentP(X1, sK57) | ~ neq(sK57, nil)) & neq(X1, nil)) | (~ (nil = sK57) & (nil = X1))) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((((~ segmentP(sK58, sK57) | ~ neq(sK57, nil)) & neq(sK58, nil)) | (~ (nil = sK57) & (nil = sK58))) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X2] : (? [X3] : ((((~ segmentP(sK58, sK57) | ~ neq(sK57, nil)) & neq(sK58, nil)) | (~ (nil = sK57) & (nil = sK58))) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((((~ segmentP(sK58, sK57) | ~ neq(sK57, nil)) & neq(sK58, nil)) | (~ (nil = sK57) & (nil = sK58))) & (((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X3] : ((((~ segmentP(sK58, sK57) | ~ neq(sK57, nil)) & neq(sK58, nil)) | (~ (nil = sK57) & (nil = sK58))) & (((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => ((((~ segmentP(sK58, sK57) | ~ neq(sK57, nil)) & neq(sK58, nil)) | (~ (nil = sK57) & (nil = sK58))) & (((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f233, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ segmentP(X1, X0) | ~ neq(X0, nil)) & neq(X1, nil)) | (~ (nil = X0) & (nil = X1))) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f222, e232])).
fof(f232, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(usedef, [], [e232])).
fof(e232, plain, ! [X3, X2] : (sP6(X3, X2) <=> ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ segmentP(X1, X0) | ~ neq(X0, nil)) & neq(X1, nil)) | (~ (nil = X0) & (nil = X1))) & (((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : ((((segmentP(X1, X0) & neq(X0, nil)) | ~ neq(X1, nil)) & ((nil = X0) | ~ (nil = X1))) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X7] : (lt(X7, X4) & memberP(X6, X7) & ssItem(X7)) | ? [X8] : (lt(X4, X8) & memberP(X5, X8) & ssItem(X8)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : ((((segmentP(X1, X0) & neq(X0, nil)) | ~ neq(X1, nil)) & ((nil = X0) | ~ (nil = X1))) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : ((((segmentP(X1, X0) & neq(X0, nil)) | ~ neq(X1, nil)) & ((nil = X0) | ~ (nil = X1))) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC109+1.p', co1)).
fof(f7201, plain, (~ spl61_212 | spl61_3 | ~ spl61_6 | ~ spl61_213), inference(avatar_split_clause, [], [f7200, f5069, f633, f617, f5065])).
fof(f5065, plain, (spl61_212 <=> ssList(sK55(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_212])])).
fof(f617, plain, (spl61_3 <=> segmentP(sK58, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_3])])).
fof(f633, plain, (spl61_6 <=> sP6(sK60, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_6])])).
fof(f5069, plain, (spl61_213 <=> ssList(sK56(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_213])])).
fof(f7200, plain, (segmentP(sK58, sK57) | ~ ssList(sK55(sK58, sK57)) | (~ spl61_6 | ~ spl61_213)), inference(subsumption_resolution, [], [f7199, f646])).
fof(f646, plain, ssList(sK58), inference(forward_demodulation, [], [f565, f566])).
fof(f566, plain, (sK58 = sK60), inference(cnf_transformation, [], [f355])).
fof(f565, plain, ssList(sK60), inference(cnf_transformation, [], [f355])).
fof(f7199, plain, (segmentP(sK58, sK57) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK58) | (~ spl61_6 | ~ spl61_213)), inference(subsumption_resolution, [], [f7198, f647])).
fof(f647, plain, ssList(sK57), inference(forward_demodulation, [], [f564, f567])).
fof(f564, plain, ssList(sK59), inference(cnf_transformation, [], [f355])).
fof(f7198, plain, (segmentP(sK58, sK57) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | (~ spl61_6 | ~ spl61_213)), inference(subsumption_resolution, [], [f5040, f5070])).
fof(f5070, plain, (ssList(sK56(sK58, sK57)) | ~ spl61_213), inference(avatar_component_clause, [], [f5069])).
fof(f5040, plain, (segmentP(sK58, sK57) | ~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | ~ spl61_6), inference(superposition, [], [f579, f4050])).
fof(f4050, plain, ((sK58 = app(app(sK55(sK58, sK57), sK57), sK56(sK58, sK57))) | ~ spl61_6), inference(resolution, [], [f3988, f559])).
fof(f559, plain, ! [X0, X1] : (~ sP6(X0, X1) | (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f350])).
fof(f350, plain, ! [X0, X1] : ((((! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f346, f349, f348, f347])).
fof(f347, plain, ! [X1, X0] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f348, plain, ! [X1, X0] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) => (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) => (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f346, plain, ! [X0, X1] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f345])).
fof(f345, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(nnf_transformation, [], [f232])).
fof(f3988, plain, (sP6(sK58, sK57) | ~ spl61_6), inference(backward_demodulation, [], [f1393, f566])).
fof(f1393, plain, (sP6(sK60, sK57) | ~ spl61_6), inference(forward_demodulation, [], [f635, f567])).
fof(f635, plain, (sP6(sK60, sK59) | ~ spl61_6), inference(avatar_component_clause, [], [f633])).
fof(f579, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f377])).
fof(f377, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f259])).
fof(f259, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1))) & ssList(sK14(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15])], [f256, f258, f257])).
fof(f257, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f258, plain, ! [X1, X0] : (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1)))), introduced(choice_axiom, [])).
fof(f256, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f255])).
fof(f255, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC109+1.p', ax7)).
fof(f7120, plain, (~ spl61_6 | spl61_212), inference(avatar_contradiction_clause, [], [f7119])).
fof(f7119, plain, ($false | (~ spl61_6 | spl61_212)), inference(subsumption_resolution, [], [f7118, f3988])).
fof(f7118, plain, (~ sP6(sK58, sK57) | spl61_212), inference(resolution, [], [f5067, f556])).
fof(f556, plain, ! [X0, X1] : (ssList(sK55(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f5067, plain, (~ ssList(sK55(sK58, sK57)) | spl61_212), inference(avatar_component_clause, [], [f5065])).
fof(f7115, plain, (~ spl61_6 | spl61_213), inference(avatar_contradiction_clause, [], [f7114])).
fof(f7114, plain, ($false | (~ spl61_6 | spl61_213)), inference(subsumption_resolution, [], [f7113, f3988])).
fof(f7113, plain, (~ sP6(sK58, sK57) | spl61_213), inference(resolution, [], [f5071, f557])).
fof(f557, plain, ! [X0, X1] : (ssList(sK56(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f5071, plain, (~ ssList(sK56(sK58, sK57)) | spl61_213), inference(avatar_component_clause, [], [f5069])).
fof(f4567, plain, (spl61_1 | ~ spl61_3 | ~ spl61_4), inference(avatar_contradiction_clause, [], [f4566])).
fof(f4566, plain, ($false | (spl61_1 | ~ spl61_3 | ~ spl61_4)), inference(subsumption_resolution, [], [f4565, f647])).
fof(f4565, plain, (~ ssList(sK57) | (spl61_1 | ~ spl61_3 | ~ spl61_4)), inference(subsumption_resolution, [], [f4561, f611])).
fof(f611, plain, (~ (nil = sK57) | spl61_1), inference(avatar_component_clause, [], [f609])).
fof(f4561, plain, ((nil = sK57) | ~ ssList(sK57) | (~ spl61_3 | ~ spl61_4)), inference(resolution, [], [f4560, f502])).
fof(f502, plain, ! [X0] : (~ segmentP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f332])).
fof(f332, plain, ! [X0] : (((segmentP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ segmentP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((segmentP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0] : (ssList(X0) => (segmentP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC109+1.p', ax58)).
fof(f4560, plain, (segmentP(nil, sK57) | (~ spl61_3 | ~ spl61_4)), inference(forward_demodulation, [], [f618, f624])).
fof(f624, plain, ((nil = sK58) | ~ spl61_4), inference(avatar_component_clause, [], [f622])).
fof(f622, plain, (spl61_4 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_4])])).
fof(f618, plain, (segmentP(sK58, sK57) | ~ spl61_3), inference(avatar_component_clause, [], [f617])).
fof(f4492, plain, (spl61_4 | ~ spl61_8), inference(avatar_split_clause, [], [f4491, f642, f622])).
fof(f642, plain, (spl61_8 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl61_8])])).
fof(f4491, plain, ((nil = sK58) | ~ spl61_8), inference(backward_demodulation, [], [f566, f644])).
fof(f644, plain, ((nil = sK60) | ~ spl61_8), inference(avatar_component_clause, [], [f642])).
fof(f1449, plain, (~ spl61_4 | ~ spl61_5), inference(avatar_contradiction_clause, [], [f1448])).
fof(f1448, plain, ($false | (~ spl61_4 | ~ spl61_5)), inference(subsumption_resolution, [], [f1446, f448])).
fof(f448, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC109+1.p', ax17)).
fof(f1446, plain, (~ ssList(nil) | (~ spl61_4 | ~ spl61_5)), inference(resolution, [], [f1396, f605])).
fof(f605, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f588])).
fof(f588, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1) | ~ ssList(X1)), inference(equality_resolution, [], [f445])).
fof(f445, plain, ! [X0, X1] : (~ (X0 = X1) | ~ neq(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f318])).
fof(f318, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC109+1.p', ax15)).
fof(f1396, plain, (neq(nil, nil) | (~ spl61_4 | ~ spl61_5)), inference(backward_demodulation, [], [f629, f624])).
fof(f629, plain, (neq(sK58, nil) | ~ spl61_5), inference(avatar_component_clause, [], [f627])).
fof(f627, plain, (spl61_5 <=> neq(sK58, nil)), introduced(avatar_definition, [new_symbols(naming, [spl61_5])])).
fof(f1367, plain, (~ spl61_6 | spl61_27), inference(avatar_contradiction_clause, [], [f1366])).
fof(f1366, plain, ($false | (~ spl61_6 | spl61_27)), inference(subsumption_resolution, [], [f1365, f649])).
fof(f649, plain, (sP6(sK58, sK57) | ~ spl61_6), inference(forward_demodulation, [], [f648, f566])).
fof(f648, plain, (sP6(sK60, sK57) | ~ spl61_6), inference(forward_demodulation, [], [f635, f567])).
fof(f1365, plain, (~ sP6(sK58, sK57) | spl61_27), inference(resolution, [], [f1300, f555])).
fof(f555, plain, ! [X0, X1] : (ssItem(sK54(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f1300, plain, (~ ssItem(sK54(sK58, sK57)) | spl61_27), inference(avatar_component_clause, [], [f1298])).
fof(f1298, plain, (spl61_27 <=> ssItem(sK54(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_27])])).
fof(f1312, plain, (spl61_29 | ~ spl61_27 | ~ spl61_6), inference(avatar_split_clause, [], [f1311, f633, f1298, f1307])).
fof(f1311, plain, (~ ssItem(sK54(sK58, sK57)) | singletonP(sK57) | ~ spl61_6), inference(subsumption_resolution, [], [f1275, f647])).
fof(f1275, plain, (~ ssList(sK57) | ~ ssItem(sK54(sK58, sK57)) | singletonP(sK57) | ~ spl61_6), inference(superposition, [], [f576, f1252])).
fof(f1252, plain, ((sK57 = cons(sK54(sK58, sK57), nil)) | ~ spl61_6), inference(resolution, [], [f558, f649])).
fof(f558, plain, ! [X0, X1] : (~ sP6(X0, X1) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f350])).
fof(f576, plain, ! [X1] : (~ ssList(cons(X1, nil)) | ~ ssItem(X1) | singletonP(cons(X1, nil))), inference(equality_resolution, [], [f367])).
fof(f367, plain, ! [X0, X1] : (singletonP(X0) | ~ (cons(X1, nil) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f246])).
fof(f246, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0))) | ~ singletonP(X0))) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f244, f245])).
fof(f245, plain, ! [X0] : (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) => ((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0)))), introduced(choice_axiom, [])).
fof(f244, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(rectify, [], [f243])).
fof(f243, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f101])).
fof(f101, plain, ! [X0] : ((singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : (ssList(X0) => (singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC109+1.p', ax4)).
fof(f1067, plain, (spl61_1 | spl61_2), inference(avatar_split_clause, [], [f1066, f613, f609])).
fof(f613, plain, (spl61_2 <=> neq(sK57, nil)), introduced(avatar_definition, [new_symbols(naming, [spl61_2])])).
fof(f1066, plain, ((nil = sK57) | spl61_2), inference(subsumption_resolution, [], [f1065, f647])).
fof(f1065, plain, ((nil = sK57) | ~ ssList(sK57) | spl61_2), inference(subsumption_resolution, [], [f1056, f448])).
fof(f1056, plain, ((nil = sK57) | ~ ssList(nil) | ~ ssList(sK57) | spl61_2), inference(resolution, [], [f446, f615])).
fof(f615, plain, (~ neq(sK57, nil) | spl61_2), inference(avatar_component_clause, [], [f613])).
fof(f446, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f318])).
fof(f645, plain, (spl61_6 | spl61_8), inference(avatar_split_clause, [], [f568, f642, f633])).
fof(f568, plain, ((nil = sK60) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f355])).
fof(f640, plain, (spl61_6 | spl61_7), inference(avatar_split_clause, [], [f569, f637, f633])).
fof(f569, plain, ((nil = sK59) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f355])).
fof(f630, plain, (~ spl61_1 | spl61_5), inference(avatar_split_clause, [], [f571, f627, f609])).
fof(f571, plain, (neq(sK58, nil) | ~ (nil = sK57)), inference(cnf_transformation, [], [f355])).
fof(f625, plain, (spl61_4 | ~ spl61_2 | ~ spl61_3), inference(avatar_split_clause, [], [f572, f617, f613, f622])).
fof(f572, plain, (~ segmentP(sK58, sK57) | ~ neq(sK57, nil) | (nil = sK58)), inference(cnf_transformation, [], [f355])).