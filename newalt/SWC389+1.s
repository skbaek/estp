fof(f6709, plain, $false, inference(avatar_sat_refutation, [], [f614, f619, f628, f633, f1309, f1311, f1617, f1634, f1683, f1717, f5440, f6681, f6708])).
fof(f6708, plain, (~ spl61_4 | spl61_243), inference(avatar_contradiction_clause, [], [f6707])).
fof(f6707, plain, ($false | (~ spl61_4 | spl61_243)), inference(subsumption_resolution, [], [f6706, f4241])).
fof(f4241, plain, (sP6(sK58, sK57) | ~ spl61_4), inference(backward_demodulation, [], [f1720, f566])).
fof(f566, plain, (sK58 = sK60), inference(cnf_transformation, [], [f355])).
fof(f355, plain, ((((((~ singletonP(sK57) & neq(sK58, nil)) | ~ segmentP(sK58, sK57)) & (((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60])], [f233, f354, f353, f352, f351])).
fof(f351, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ singletonP(X0) & neq(X1, nil)) | ~ segmentP(X1, X0)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ singletonP(sK57) & neq(X1, nil)) | ~ segmentP(X1, sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X1] : (? [X2] : (? [X3] : (((~ singletonP(sK57) & neq(X1, nil)) | ~ segmentP(X1, sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ singletonP(sK57) & neq(sK58, nil)) | ~ segmentP(sK58, sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X2] : (? [X3] : (((~ singletonP(sK57) & neq(sK58, nil)) | ~ segmentP(sK58, sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ singletonP(sK57) & neq(sK58, nil)) | ~ segmentP(sK58, sK57)) & (((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X3] : (((~ singletonP(sK57) & neq(sK58, nil)) | ~ segmentP(sK58, sK57)) & (((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => (((~ singletonP(sK57) & neq(sK58, nil)) | ~ segmentP(sK58, sK57)) & (((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f233, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ singletonP(X0) & neq(X1, nil)) | ~ segmentP(X1, X0)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f222, e232])).
fof(f232, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(usedef, [], [e232])).
fof(e232, plain, ! [X3, X2] : (sP6(X3, X2) <=> ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ singletonP(X0) & neq(X1, nil)) | ~ segmentP(X1, X0)) & (((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((singletonP(X0) | ~ neq(X1, nil)) & segmentP(X1, X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X7] : (lt(X7, X4) & memberP(X6, X7) & ssItem(X7)) | ? [X8] : (lt(X4, X8) & memberP(X5, X8) & ssItem(X8)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((singletonP(X0) | ~ neq(X1, nil)) & segmentP(X1, X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((singletonP(X0) | ~ neq(X1, nil)) & segmentP(X1, X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC389+1.p', co1)).
fof(f1720, plain, (sP6(sK60, sK57) | ~ spl61_4), inference(forward_demodulation, [], [f623, f567])).
fof(f567, plain, (sK57 = sK59), inference(cnf_transformation, [], [f355])).
fof(f623, plain, (sP6(sK60, sK59) | ~ spl61_4), inference(avatar_component_clause, [], [f621])).
fof(f621, plain, (spl61_4 <=> sP6(sK60, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_4])])).
fof(f6706, plain, (~ sP6(sK58, sK57) | spl61_243), inference(resolution, [], [f5435, f556])).
fof(f556, plain, ! [X0, X1] : (ssList(sK55(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f350, plain, ! [X0, X1] : ((((! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f346, f349, f348, f347])).
fof(f347, plain, ! [X1, X0] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f348, plain, ! [X1, X0] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) => (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) => (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f346, plain, ! [X0, X1] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f345])).
fof(f345, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(nnf_transformation, [], [f232])).
fof(f5435, plain, (~ ssList(sK55(sK58, sK57)) | spl61_243), inference(avatar_component_clause, [], [f5433])).
fof(f5433, plain, (spl61_243 <=> ssList(sK55(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_243])])).
fof(f6681, plain, (~ spl61_4 | spl61_244), inference(avatar_contradiction_clause, [], [f6680])).
fof(f6680, plain, ($false | (~ spl61_4 | spl61_244)), inference(subsumption_resolution, [], [f6679, f4241])).
fof(f6679, plain, (~ sP6(sK58, sK57) | spl61_244), inference(resolution, [], [f5439, f557])).
fof(f557, plain, ! [X0, X1] : (ssList(sK56(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f5439, plain, (~ ssList(sK56(sK58, sK57)) | spl61_244), inference(avatar_component_clause, [], [f5437])).
fof(f5437, plain, (spl61_244 <=> ssList(sK56(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_244])])).
fof(f5440, plain, (~ spl61_243 | ~ spl61_244 | spl61_1 | ~ spl61_4), inference(avatar_split_clause, [], [f5431, f621, f607, f5437, f5433])).
fof(f607, plain, (spl61_1 <=> segmentP(sK58, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_1])])).
fof(f5431, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | (spl61_1 | ~ spl61_4)), inference(subsumption_resolution, [], [f5430, f634])).
fof(f634, plain, ssList(sK58), inference(forward_demodulation, [], [f565, f566])).
fof(f565, plain, ssList(sK60), inference(cnf_transformation, [], [f355])).
fof(f5430, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK58) | (spl61_1 | ~ spl61_4)), inference(subsumption_resolution, [], [f5429, f635])).
fof(f635, plain, ssList(sK57), inference(forward_demodulation, [], [f564, f567])).
fof(f564, plain, ssList(sK59), inference(cnf_transformation, [], [f355])).
fof(f5429, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | (spl61_1 | ~ spl61_4)), inference(subsumption_resolution, [], [f5408, f609])).
fof(f609, plain, (~ segmentP(sK58, sK57) | spl61_1), inference(avatar_component_clause, [], [f607])).
fof(f5408, plain, (segmentP(sK58, sK57) | ~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | ~ spl61_4), inference(superposition, [], [f577, f4323])).
fof(f4323, plain, ((sK58 = app(app(sK55(sK58, sK57), sK57), sK56(sK58, sK57))) | ~ spl61_4), inference(resolution, [], [f4241, f559])).
fof(f559, plain, ! [X0, X1] : (~ sP6(X0, X1) | (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f350])).
fof(f577, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f377])).
fof(f377, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f259])).
fof(f259, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1))) & ssList(sK14(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15])], [f256, f258, f257])).
fof(f257, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f258, plain, ! [X1, X0] : (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1)))), introduced(choice_axiom, [])).
fof(f256, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f255])).
fof(f255, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC389+1.p', ax7)).
fof(f1717, plain, (~ spl61_3 | ~ spl61_26), inference(avatar_contradiction_clause, [], [f1716])).
fof(f1716, plain, ($false | (~ spl61_3 | ~ spl61_26)), inference(subsumption_resolution, [], [f1715, f448])).
fof(f448, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC389+1.p', ax17)).
fof(f1715, plain, (~ ssList(nil) | (~ spl61_3 | ~ spl61_26)), inference(resolution, [], [f1694, f603])).
fof(f603, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f586])).
fof(f586, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1) | ~ ssList(X1)), inference(equality_resolution, [], [f445])).
fof(f445, plain, ! [X0, X1] : (~ (X0 = X1) | ~ neq(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f318])).
fof(f318, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC389+1.p', ax15)).
fof(f1694, plain, (neq(nil, nil) | (~ spl61_3 | ~ spl61_26)), inference(backward_demodulation, [], [f618, f1166])).
fof(f1166, plain, ((nil = sK58) | ~ spl61_26), inference(avatar_component_clause, [], [f1164])).
fof(f1164, plain, (spl61_26 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_26])])).
fof(f618, plain, (neq(sK58, nil) | ~ spl61_3), inference(avatar_component_clause, [], [f616])).
fof(f616, plain, (spl61_3 <=> neq(sK58, nil)), introduced(avatar_definition, [new_symbols(naming, [spl61_3])])).
fof(f1683, plain, (spl61_26 | ~ spl61_6), inference(avatar_split_clause, [], [f1667, f630, f1164])).
fof(f630, plain, (spl61_6 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl61_6])])).
fof(f1667, plain, ((nil = sK58) | ~ spl61_6), inference(backward_demodulation, [], [f566, f632])).
fof(f632, plain, ((nil = sK60) | ~ spl61_6), inference(avatar_component_clause, [], [f630])).
fof(f1634, plain, (~ spl61_4 | spl61_29), inference(avatar_contradiction_clause, [], [f1633])).
fof(f1633, plain, ($false | (~ spl61_4 | spl61_29)), inference(subsumption_resolution, [], [f1632, f637])).
fof(f637, plain, (sP6(sK58, sK57) | ~ spl61_4), inference(forward_demodulation, [], [f636, f566])).
fof(f636, plain, (sP6(sK60, sK57) | ~ spl61_4), inference(forward_demodulation, [], [f623, f567])).
fof(f1632, plain, (~ sP6(sK58, sK57) | spl61_29), inference(resolution, [], [f1599, f555])).
fof(f555, plain, ! [X0, X1] : (ssItem(sK54(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f1599, plain, (~ ssItem(sK54(sK58, sK57)) | spl61_29), inference(avatar_component_clause, [], [f1597])).
fof(f1597, plain, (spl61_29 <=> ssItem(sK54(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_29])])).
fof(f1617, plain, (~ spl61_29 | spl61_2 | ~ spl61_4), inference(avatar_split_clause, [], [f1589, f621, f611, f1597])).
fof(f611, plain, (spl61_2 <=> singletonP(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_2])])).
fof(f1589, plain, (singletonP(sK57) | ~ ssItem(sK54(sK58, sK57)) | ~ spl61_4), inference(superposition, [], [f1332, f1329])).
fof(f1329, plain, ((sK57 = cons(sK54(sK58, sK57), nil)) | ~ spl61_4), inference(resolution, [], [f558, f637])).
fof(f558, plain, ! [X0, X1] : (~ sP6(X0, X1) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f350])).
fof(f1332, plain, ! [X0] : (singletonP(cons(X0, nil)) | ~ ssItem(X0)), inference(subsumption_resolution, [], [f1331, f448])).
fof(f1331, plain, ! [X0] : (~ ssItem(X0) | singletonP(cons(X0, nil)) | ~ ssList(nil)), inference(duplicate_literal_removal, [], [f1330])).
fof(f1330, plain, ! [X0] : (~ ssItem(X0) | singletonP(cons(X0, nil)) | ~ ssItem(X0) | ~ ssList(nil)), inference(resolution, [], [f574, f447])).
fof(f447, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC389+1.p', ax16)).
fof(f574, plain, ! [X1] : (~ ssList(cons(X1, nil)) | ~ ssItem(X1) | singletonP(cons(X1, nil))), inference(equality_resolution, [], [f367])).
fof(f367, plain, ! [X0, X1] : (singletonP(X0) | ~ (cons(X1, nil) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f246])).
fof(f246, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0))) | ~ singletonP(X0))) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f244, f245])).
fof(f245, plain, ! [X0] : (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) => ((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0)))), introduced(choice_axiom, [])).
fof(f244, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(rectify, [], [f243])).
fof(f243, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f101])).
fof(f101, plain, ! [X0] : ((singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : (ssList(X0) => (singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC389+1.p', ax4)).
fof(f1311, plain, (spl61_24 | ~ spl61_5), inference(avatar_split_clause, [], [f1310, f625, f1155])).
fof(f1155, plain, (spl61_24 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_24])])).
fof(f625, plain, (spl61_5 <=> (nil = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_5])])).
fof(f1310, plain, ((nil = sK57) | ~ spl61_5), inference(forward_demodulation, [], [f567, f627])).
fof(f627, plain, ((nil = sK59) | ~ spl61_5), inference(avatar_component_clause, [], [f625])).
fof(f1309, plain, (spl61_1 | ~ spl61_24), inference(avatar_contradiction_clause, [], [f1308])).
fof(f1308, plain, ($false | (spl61_1 | ~ spl61_24)), inference(subsumption_resolution, [], [f1307, f634])).
fof(f1307, plain, (~ ssList(sK58) | (spl61_1 | ~ spl61_24)), inference(resolution, [], [f1284, f501])).
fof(f501, plain, ! [X0] : (segmentP(X0, nil) | ~ ssList(X0)), inference(cnf_transformation, [], [f176])).
fof(f176, plain, ! [X0] : (segmentP(X0, nil) | ~ ssList(X0)), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0] : (ssList(X0) => segmentP(X0, nil)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC389+1.p', ax57)).
fof(f1284, plain, (~ segmentP(sK58, nil) | (spl61_1 | ~ spl61_24)), inference(forward_demodulation, [], [f609, f1157])).
fof(f1157, plain, ((nil = sK57) | ~ spl61_24), inference(avatar_component_clause, [], [f1155])).
fof(f633, plain, (spl61_4 | spl61_6), inference(avatar_split_clause, [], [f568, f630, f621])).
fof(f568, plain, ((nil = sK60) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f355])).
fof(f628, plain, (spl61_4 | spl61_5), inference(avatar_split_clause, [], [f569, f625, f621])).
fof(f569, plain, ((nil = sK59) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f355])).
fof(f619, plain, (~ spl61_1 | spl61_3), inference(avatar_split_clause, [], [f570, f616, f607])).
fof(f570, plain, (neq(sK58, nil) | ~ segmentP(sK58, sK57)), inference(cnf_transformation, [], [f355])).
fof(f614, plain, (~ spl61_1 | ~ spl61_2), inference(avatar_split_clause, [], [f571, f611, f607])).
fof(f571, plain, (~ singletonP(sK57) | ~ segmentP(sK58, sK57)), inference(cnf_transformation, [], [f355])).