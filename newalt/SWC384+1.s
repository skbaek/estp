fof(f5611, plain, $false, inference(avatar_sat_refutation, [], [f615, f629, f1618, f1676, f1679, f1867, f5473, f5571, f5610])).
fof(f5610, plain, (~ spl61_3 | spl61_252), inference(avatar_contradiction_clause, [], [f5609])).
fof(f5609, plain, ($false | (~ spl61_3 | spl61_252)), inference(subsumption_resolution, [], [f5608, f1879])).
fof(f1879, plain, (sP6(sK58, sK57) | ~ spl61_3), inference(backward_demodulation, [], [f1878, f568])).
fof(f568, plain, (sK57 = sK59), inference(cnf_transformation, [], [f356])).
fof(f356, plain, (((((~ segmentP(sK58, sK57) | ~ singletonP(sK57)) & (((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & neq(sK58, nil) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60])], [f234, f355, f354, f353, f352])).
fof(f352, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((~ segmentP(X1, X0) | ~ singletonP(X0)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & neq(X1, nil) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((~ segmentP(X1, sK57) | ~ singletonP(sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & neq(X1, nil) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X1] : (? [X2] : (? [X3] : ((~ segmentP(X1, sK57) | ~ singletonP(sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & neq(X1, nil) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((~ segmentP(sK58, sK57) | ~ singletonP(sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & neq(sK58, nil) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X2] : (? [X3] : ((~ segmentP(sK58, sK57) | ~ singletonP(sK57)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & neq(sK58, nil) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((~ segmentP(sK58, sK57) | ~ singletonP(sK57)) & (((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & neq(sK58, nil) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f355, plain, (? [X3] : ((~ segmentP(sK58, sK57) | ~ singletonP(sK57)) & (((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & neq(sK58, nil) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => ((~ segmentP(sK58, sK57) | ~ singletonP(sK57)) & (((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & neq(sK58, nil) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((~ segmentP(X1, X0) | ~ singletonP(X0)) & (((nil = X2) & (nil = X3)) | sP6(X3, X2)) & neq(X1, nil) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2] : (sP6(X3, X2) <=> ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((~ segmentP(X1, X0) | ~ singletonP(X0)) & (((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & neq(X1, nil) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ segmentP(X1, X0) | ~ singletonP(X0)) & (((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : ((! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2)) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & neq(X1, nil) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => ((segmentP(X1, X0) & singletonP(X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X7] : (lt(X7, X4) & memberP(X6, X7) & ssItem(X7)) | ? [X8] : (lt(X4, X8) & memberP(X5, X8) & ssItem(X8)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | ~ neq(X1, nil) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => ((segmentP(X1, X0) & singletonP(X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | ~ neq(X1, nil) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => ((segmentP(X1, X0) & singletonP(X0)) | ((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | ~ neq(X1, nil) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC384+1.p', co1)).
fof(f1878, plain, (sP6(sK58, sK59) | ~ spl61_3), inference(forward_demodulation, [], [f619, f567])).
fof(f567, plain, (sK58 = sK60), inference(cnf_transformation, [], [f356])).
fof(f619, plain, (sP6(sK60, sK59) | ~ spl61_3), inference(avatar_component_clause, [], [f617])).
fof(f617, plain, (spl61_3 <=> sP6(sK60, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_3])])).
fof(f5608, plain, (~ sP6(sK58, sK57) | spl61_252), inference(resolution, [], [f5468, f557])).
fof(f557, plain, ! [X0, X1] : (ssList(sK55(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f351, plain, ! [X0, X1] : ((((! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f347, f350, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) => (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f350, plain, ! [X1, X0] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) => (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(nnf_transformation, [], [f233])).
fof(f5468, plain, (~ ssList(sK55(sK58, sK57)) | spl61_252), inference(avatar_component_clause, [], [f5466])).
fof(f5466, plain, (spl61_252 <=> ssList(sK55(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_252])])).
fof(f5571, plain, (~ spl61_3 | spl61_253), inference(avatar_contradiction_clause, [], [f5570])).
fof(f5570, plain, ($false | (~ spl61_3 | spl61_253)), inference(subsumption_resolution, [], [f5569, f1879])).
fof(f5569, plain, (~ sP6(sK58, sK57) | spl61_253), inference(resolution, [], [f5472, f558])).
fof(f558, plain, ! [X0, X1] : (ssList(sK56(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f5472, plain, (~ ssList(sK56(sK58, sK57)) | spl61_253), inference(avatar_component_clause, [], [f5470])).
fof(f5470, plain, (spl61_253 <=> ssList(sK56(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_253])])).
fof(f5473, plain, (~ spl61_252 | ~ spl61_253 | spl61_2 | ~ spl61_3), inference(avatar_split_clause, [], [f5464, f617, f612, f5470, f5466])).
fof(f612, plain, (spl61_2 <=> segmentP(sK58, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_2])])).
fof(f5464, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | (spl61_2 | ~ spl61_3)), inference(subsumption_resolution, [], [f5463, f630])).
fof(f630, plain, ssList(sK58), inference(forward_demodulation, [], [f566, f567])).
fof(f566, plain, ssList(sK60), inference(cnf_transformation, [], [f356])).
fof(f5463, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK58) | (spl61_2 | ~ spl61_3)), inference(subsumption_resolution, [], [f5462, f631])).
fof(f631, plain, ssList(sK57), inference(forward_demodulation, [], [f565, f568])).
fof(f565, plain, ssList(sK59), inference(cnf_transformation, [], [f356])).
fof(f5462, plain, (~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | (spl61_2 | ~ spl61_3)), inference(subsumption_resolution, [], [f5441, f614])).
fof(f614, plain, (~ segmentP(sK58, sK57) | spl61_2), inference(avatar_component_clause, [], [f612])).
fof(f5441, plain, (segmentP(sK58, sK57) | ~ ssList(sK56(sK58, sK57)) | ~ ssList(sK55(sK58, sK57)) | ~ ssList(sK57) | ~ ssList(sK58) | ~ spl61_3), inference(superposition, [], [f578, f1924])).
fof(f1924, plain, ((sK58 = app(app(sK55(sK58, sK57), sK57), sK56(sK58, sK57))) | ~ spl61_3), inference(resolution, [], [f1879, f560])).
fof(f560, plain, ! [X0, X1] : (~ sP6(X0, X1) | (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f351])).
fof(f578, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f378])).
fof(f378, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f260])).
fof(f260, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1))) & ssList(sK14(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15])], [f257, f259, f258])).
fof(f258, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f259, plain, ! [X1, X0] : (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1)))), introduced(choice_axiom, [])).
fof(f257, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f256])).
fof(f256, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC384+1.p', ax7)).
fof(f1867, plain, ~ spl61_25, inference(avatar_contradiction_clause, [], [f1866])).
fof(f1866, plain, ($false | ~ spl61_25), inference(subsumption_resolution, [], [f1865, f449])).
fof(f449, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC384+1.p', ax17)).
fof(f1865, plain, (~ ssList(nil) | ~ spl61_25), inference(resolution, [], [f1805, f604])).
fof(f604, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f587])).
fof(f587, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1) | ~ ssList(X1)), inference(equality_resolution, [], [f446])).
fof(f446, plain, ! [X0, X1] : (~ (X0 = X1) | ~ neq(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC384+1.p', ax15)).
fof(f1805, plain, (neq(nil, nil) | ~ spl61_25), inference(backward_demodulation, [], [f569, f1164])).
fof(f1164, plain, ((nil = sK58) | ~ spl61_25), inference(avatar_component_clause, [], [f1162])).
fof(f1162, plain, (spl61_25 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_25])])).
fof(f569, plain, neq(sK58, nil), inference(cnf_transformation, [], [f356])).
fof(f1679, plain, (~ spl61_5 | spl61_25), inference(avatar_contradiction_clause, [], [f1678])).
fof(f1678, plain, ($false | (~ spl61_5 | spl61_25)), inference(subsumption_resolution, [], [f1677, f1163])).
fof(f1163, plain, (~ (nil = sK58) | spl61_25), inference(avatar_component_clause, [], [f1162])).
fof(f1677, plain, ((nil = sK58) | ~ spl61_5), inference(backward_demodulation, [], [f567, f628])).
fof(f628, plain, ((nil = sK60) | ~ spl61_5), inference(avatar_component_clause, [], [f626])).
fof(f626, plain, (spl61_5 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl61_5])])).
fof(f1676, plain, (~ spl61_3 | spl61_28), inference(avatar_contradiction_clause, [], [f1675])).
fof(f1675, plain, ($false | (~ spl61_3 | spl61_28)), inference(subsumption_resolution, [], [f1674, f633])).
fof(f633, plain, (sP6(sK58, sK57) | ~ spl61_3), inference(forward_demodulation, [], [f632, f567])).
fof(f632, plain, (sP6(sK60, sK57) | ~ spl61_3), inference(forward_demodulation, [], [f619, f568])).
fof(f1674, plain, (~ sP6(sK58, sK57) | spl61_28), inference(resolution, [], [f1608, f556])).
fof(f556, plain, ! [X0, X1] : (ssItem(sK54(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f1608, plain, (~ ssItem(sK54(sK58, sK57)) | spl61_28), inference(avatar_component_clause, [], [f1606])).
fof(f1606, plain, (spl61_28 <=> ssItem(sK54(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_28])])).
fof(f1618, plain, (~ spl61_28 | spl61_1 | ~ spl61_3), inference(avatar_split_clause, [], [f1617, f617, f608, f1606])).
fof(f608, plain, (spl61_1 <=> singletonP(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_1])])).
fof(f1617, plain, (~ ssItem(sK54(sK58, sK57)) | (spl61_1 | ~ spl61_3)), inference(subsumption_resolution, [], [f1616, f610])).
fof(f610, plain, (~ singletonP(sK57) | spl61_1), inference(avatar_component_clause, [], [f608])).
fof(f1616, plain, (~ ssItem(sK54(sK58, sK57)) | singletonP(sK57) | ~ spl61_3), inference(subsumption_resolution, [], [f1585, f631])).
fof(f1585, plain, (~ ssList(sK57) | ~ ssItem(sK54(sK58, sK57)) | singletonP(sK57) | ~ spl61_3), inference(superposition, [], [f575, f1304])).
fof(f1304, plain, ((sK57 = cons(sK54(sK58, sK57), nil)) | ~ spl61_3), inference(resolution, [], [f559, f633])).
fof(f559, plain, ! [X0, X1] : (~ sP6(X0, X1) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f351])).
fof(f575, plain, ! [X1] : (~ ssList(cons(X1, nil)) | ~ ssItem(X1) | singletonP(cons(X1, nil))), inference(equality_resolution, [], [f368])).
fof(f368, plain, ! [X0, X1] : (singletonP(X0) | ~ (cons(X1, nil) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f247])).
fof(f247, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0))) | ~ singletonP(X0))) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f245, f246])).
fof(f246, plain, ! [X0] : (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) => ((cons(sK11(X0), nil) = X0) & ssItem(sK11(X0)))), introduced(choice_axiom, [])).
fof(f245, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X2] : ((cons(X2, nil) = X0) & ssItem(X2)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(rectify, [], [f244])).
fof(f244, plain, ! [X0] : (((singletonP(X0) | ! [X1] : (~ (cons(X1, nil) = X0) | ~ ssItem(X1))) & (? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)) | ~ singletonP(X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f101])).
fof(f101, plain, ! [X0] : ((singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : (ssList(X0) => (singletonP(X0) <=> ? [X1] : ((cons(X1, nil) = X0) & ssItem(X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC384+1.p', ax4)).
fof(f629, plain, (spl61_3 | spl61_5), inference(avatar_split_clause, [], [f570, f626, f617])).
fof(f570, plain, ((nil = sK60) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f356])).
fof(f615, plain, (~ spl61_1 | ~ spl61_2), inference(avatar_split_clause, [], [f572, f612, f608])).
fof(f572, plain, (~ segmentP(sK58, sK57) | ~ singletonP(sK57)), inference(cnf_transformation, [], [f356])).