fof(f8992, plain, $false, inference(avatar_sat_refutation, [], [f613, f622, f627, f1621, f1654, f1657, f1660, f6860, f6871, f7645, f8961, f8991])).
fof(f8991, plain, (~ spl61_3 | spl61_258), inference(avatar_contradiction_clause, [], [f8990])).
fof(f8990, plain, ($false | (~ spl61_3 | spl61_258)), inference(subsumption_resolution, [], [f8989, f6951])).
fof(f6951, plain, (sP6(sK58, sK57) | ~ spl61_3), inference(forward_demodulation, [], [f6950, f566])).
fof(f566, plain, (sK58 = sK60), inference(cnf_transformation, [], [f355])).
fof(f355, plain, (((((~ totalorderedP(sK57) | ~ segmentP(sK58, sK57)) & (((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60])], [f233, f354, f353, f352, f351])).
fof(f351, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((~ totalorderedP(X0) | ~ segmentP(X1, X0)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((~ totalorderedP(sK57) | ~ segmentP(X1, sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X1] : (? [X2] : (? [X3] : ((~ totalorderedP(sK57) | ~ segmentP(X1, sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((~ totalorderedP(sK57) | ~ segmentP(sK58, sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X2] : (? [X3] : ((~ totalorderedP(sK57) | ~ segmentP(sK58, sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((~ totalorderedP(sK57) | ~ segmentP(sK58, sK57)) & (((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X3] : ((~ totalorderedP(sK57) | ~ segmentP(sK58, sK57)) & (((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => ((~ totalorderedP(sK57) | ~ segmentP(sK58, sK57)) & (((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f233, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((~ totalorderedP(X0) | ~ segmentP(X1, X0)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f222, e232])).
fof(f232, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(usedef, [], [e232])).
fof(e232, plain, ! [X3, X2] : (sP6(X3, X2) <=> ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((~ totalorderedP(X0) | ~ segmentP(X1, X0)) & (((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : ((totalorderedP(X0) & segmentP(X1, X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X7] : (lt(X7, X4) & memberP(X6, X7) & ssItem(X7)) | ? [X8] : (lt(X4, X8) & memberP(X5, X8) & ssItem(X8)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : ((totalorderedP(X0) & segmentP(X1, X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : ((totalorderedP(X0) & segmentP(X1, X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2) | ~ ssList(X6))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC336+1.p', co1)).
fof(f6950, plain, (sP6(sK60, sK57) | ~ spl61_3), inference(forward_demodulation, [], [f617, f567])).
fof(f567, plain, (sK57 = sK59), inference(cnf_transformation, [], [f355])).
fof(f617, plain, (sP6(sK60, sK59) | ~ spl61_3), inference(avatar_component_clause, [], [f615])).
fof(f615, plain, (spl61_3 <=> sP6(sK60, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_3])])).
fof(f8989, plain, (~ sP6(sK58, sK57) | spl61_258), inference(resolution, [], [f7640, f556])).
fof(f556, plain, ! [X0, X1] : (ssList(sK55(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f350, plain, ! [X0, X1] : ((((! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f346, f349, f348, f347])).
fof(f347, plain, ! [X1, X0] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f348, plain, ! [X1, X0] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) => (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) => (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f346, plain, ! [X0, X1] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f345])).
fof(f345, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(nnf_transformation, [], [f232])).
fof(f7640, plain, (~ ssList(sK55(sK58, sK57)) | spl61_258), inference(avatar_component_clause, [], [f7638])).
fof(f7638, plain, (spl61_258 <=> ssList(sK55(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_258])])).
fof(f8961, plain, (~ spl61_3 | spl61_259), inference(avatar_contradiction_clause, [], [f8960])).
fof(f8960, plain, ($false | (~ spl61_3 | spl61_259)), inference(subsumption_resolution, [], [f8959, f6951])).
fof(f8959, plain, (~ sP6(sK58, sK57) | spl61_259), inference(resolution, [], [f7644, f557])).
fof(f557, plain, ! [X0, X1] : (ssList(sK56(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f7644, plain, (~ ssList(sK56(sK58, sK57)) | spl61_259), inference(avatar_component_clause, [], [f7642])).
fof(f7642, plain, (spl61_259 <=> ssList(sK56(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_259])])).
fof(f7645, plain, (~ spl61_258 | ~ spl61_259 | spl61_1 | ~ spl61_3), inference(avatar_split_clause, [], [f7636, f615, f606, f7642, f7638])).
fof(f606, plain, (spl61_1 <=> segmentP(sK58, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_1])])).
fof(f7636, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | (spl61_1 | ~ spl61_3)), inference(subsumption_resolution, [], [f7635, f628])).
fof(f628, plain, ssList(sK58), inference(forward_demodulation, [], [f565, f566])).
fof(f565, plain, ssList(sK60), inference(cnf_transformation, [], [f355])).
fof(f7635, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK58) | (spl61_1 | ~ spl61_3)), inference(subsumption_resolution, [], [f7634, f629])).
fof(f629, plain, ssList(sK57), inference(forward_demodulation, [], [f564, f567])).
fof(f564, plain, ssList(sK59), inference(cnf_transformation, [], [f355])).
fof(f7634, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | (spl61_1 | ~ spl61_3)), inference(subsumption_resolution, [], [f7613, f608])).
fof(f608, plain, (~ segmentP(sK58, sK57) | spl61_1), inference(avatar_component_clause, [], [f606])).
fof(f7613, plain, (segmentP(sK58, sK57) | ~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | ~ spl61_3), inference(superposition, [], [f576, f7056])).
fof(f7056, plain, ((sK58 = app(app(sK55(sK58, sK57), sK57), sK56(sK58, sK57))) | ~ spl61_3), inference(resolution, [], [f6951, f559])).
fof(f559, plain, ! [X0, X1] : (~ sP6(X0, X1) | (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f350])).
fof(f576, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f377])).
fof(f377, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f259])).
fof(f259, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1))) & ssList(sK14(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15])], [f256, f258, f257])).
fof(f257, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f258, plain, ! [X1, X0] : (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1)))), introduced(choice_axiom, [])).
fof(f256, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f255])).
fof(f255, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC336+1.p', ax7)).
fof(f6871, plain, (spl61_2 | ~ spl61_23), inference(avatar_contradiction_clause, [], [f6870])).
fof(f6870, plain, ($false | (spl61_2 | ~ spl61_23)), inference(subsumption_resolution, [], [f6869, f511])).
fof(f511, plain, totalorderedP(nil), inference(cnf_transformation, [], [f66])).
fof(f66, plain, totalorderedP(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC336+1.p', ax66)).
fof(f6869, plain, (~ totalorderedP(nil) | (spl61_2 | ~ spl61_23)), inference(forward_demodulation, [], [f612, f1151])).
fof(f1151, plain, ((nil = sK57) | ~ spl61_23), inference(avatar_component_clause, [], [f1149])).
fof(f1149, plain, (spl61_23 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_23])])).
fof(f612, plain, (~ totalorderedP(sK57) | spl61_2), inference(avatar_component_clause, [], [f610])).
fof(f610, plain, (spl61_2 <=> totalorderedP(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_2])])).
fof(f6860, plain, (spl61_1 | ~ spl61_23 | ~ spl61_25), inference(avatar_contradiction_clause, [], [f6859])).
fof(f6859, plain, ($false | (spl61_1 | ~ spl61_23 | ~ spl61_25)), inference(subsumption_resolution, [], [f6856, f448])).
fof(f448, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC336+1.p', ax17)).
fof(f6856, plain, (~ ssList(nil) | (spl61_1 | ~ spl61_23 | ~ spl61_25)), inference(resolution, [], [f6480, f499])).
fof(f499, plain, ! [X0] : (segmentP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f173])).
fof(f173, plain, ! [X0] : (segmentP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f55])).
fof(f55, plain, ! [X0] : (ssList(X0) => segmentP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC336+1.p', ax55)).
fof(f6480, plain, (~ segmentP(nil, nil) | (spl61_1 | ~ spl61_23 | ~ spl61_25)), inference(backward_demodulation, [], [f5401, f1151])).
fof(f5401, plain, (~ segmentP(nil, sK57) | (spl61_1 | ~ spl61_25)), inference(backward_demodulation, [], [f608, f1160])).
fof(f1160, plain, ((nil = sK58) | ~ spl61_25), inference(avatar_component_clause, [], [f1158])).
fof(f1158, plain, (spl61_25 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_25])])).
fof(f1660, plain, (~ spl61_4 | spl61_23), inference(avatar_contradiction_clause, [], [f1659])).
fof(f1659, plain, ($false | (~ spl61_4 | spl61_23)), inference(subsumption_resolution, [], [f1658, f1150])).
fof(f1150, plain, (~ (nil = sK57) | spl61_23), inference(avatar_component_clause, [], [f1149])).
fof(f1658, plain, ((nil = sK57) | ~ spl61_4), inference(backward_demodulation, [], [f567, f621])).
fof(f621, plain, ((nil = sK59) | ~ spl61_4), inference(avatar_component_clause, [], [f619])).
fof(f619, plain, (spl61_4 <=> (nil = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_4])])).
fof(f1657, plain, (~ spl61_5 | spl61_25), inference(avatar_contradiction_clause, [], [f1656])).
fof(f1656, plain, ($false | (~ spl61_5 | spl61_25)), inference(subsumption_resolution, [], [f1655, f1159])).
fof(f1159, plain, (~ (nil = sK58) | spl61_25), inference(avatar_component_clause, [], [f1158])).
fof(f1655, plain, ((nil = sK58) | ~ spl61_5), inference(backward_demodulation, [], [f566, f626])).
fof(f626, plain, ((nil = sK60) | ~ spl61_5), inference(avatar_component_clause, [], [f624])).
fof(f624, plain, (spl61_5 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl61_5])])).
fof(f1654, plain, (~ spl61_3 | spl61_28), inference(avatar_contradiction_clause, [], [f1653])).
fof(f1653, plain, ($false | (~ spl61_3 | spl61_28)), inference(subsumption_resolution, [], [f1652, f631])).
fof(f631, plain, (sP6(sK58, sK57) | ~ spl61_3), inference(forward_demodulation, [], [f630, f566])).
fof(f630, plain, (sP6(sK60, sK57) | ~ spl61_3), inference(forward_demodulation, [], [f617, f567])).
fof(f1652, plain, (~ sP6(sK58, sK57) | spl61_28), inference(resolution, [], [f1598, f555])).
fof(f555, plain, ! [X0, X1] : (ssItem(sK54(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f350])).
fof(f1598, plain, (~ ssItem(sK54(sK58, sK57)) | spl61_28), inference(avatar_component_clause, [], [f1596])).
fof(f1596, plain, (spl61_28 <=> ssItem(sK54(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_28])])).
fof(f1621, plain, (~ spl61_28 | spl61_2 | ~ spl61_3), inference(avatar_split_clause, [], [f1571, f615, f610, f1596])).
fof(f1571, plain, (totalorderedP(sK57) | ~ ssItem(sK54(sK58, sK57)) | ~ spl61_3), inference(superposition, [], [f510, f1300])).
fof(f1300, plain, ((sK57 = cons(sK54(sK58, sK57), nil)) | ~ spl61_3), inference(resolution, [], [f558, f631])).
fof(f558, plain, ! [X0, X1] : (~ sP6(X0, X1) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f350])).
fof(f510, plain, ! [X0] : (totalorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0] : (totalorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (ssItem(X0) => totalorderedP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC336+1.p', ax65)).
fof(f627, plain, (spl61_3 | spl61_5), inference(avatar_split_clause, [], [f568, f624, f615])).
fof(f568, plain, ((nil = sK60) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f355])).
fof(f622, plain, (spl61_3 | spl61_4), inference(avatar_split_clause, [], [f569, f619, f615])).
fof(f569, plain, ((nil = sK59) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f355])).
fof(f613, plain, (~ spl61_1 | ~ spl61_2), inference(avatar_split_clause, [], [f570, f610, f606])).
fof(f570, plain, (~ totalorderedP(sK57) | ~ segmentP(sK58, sK57)), inference(cnf_transformation, [], [f355])).