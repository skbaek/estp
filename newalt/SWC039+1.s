fof(f4036, plain, $false, inference(avatar_sat_refutation, [], [f615, f622, f628, f3869, f3939, f3981, f4027])).
fof(f4027, plain, ~ spl61_106, inference(avatar_contradiction_clause, [], [f4026])).
fof(f4026, plain, ($false | ~ spl61_106), inference(subsumption_resolution, [], [f4025, f623])).
fof(f623, plain, ssList(sK57), inference(forward_demodulation, [], [f565, f569])).
fof(f569, plain, (sK57 = sK59), inference(cnf_transformation, [], [f356])).
fof(f356, plain, (((((((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & ~ (nil = sK57) & (sK57 = sK59) & (sK58 = sK60) & (nil = sK58) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60])], [f234, f355, f354, f353, f352])).
fof(f352, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ~ (nil = X0) & (X0 = X2) & (X1 = X3) & (nil = X1) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ~ (nil = sK57) & (sK57 = X2) & (X1 = X3) & (nil = X1) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ~ (nil = sK57) & (sK57 = X2) & (X1 = X3) & (nil = X1) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ~ (nil = sK57) & (sK57 = X2) & (sK58 = X3) & (nil = sK58) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ~ (nil = sK57) & (sK57 = X2) & (sK58 = X3) & (nil = sK58) & ssList(X3)) & ssList(X2)) => (? [X3] : ((((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & ~ (nil = sK57) & (sK57 = sK59) & (sK58 = X3) & (nil = sK58) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f355, plain, (? [X3] : ((((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & ~ (nil = sK57) & (sK57 = sK59) & (sK58 = X3) & (nil = sK58) & ssList(X3)) => ((((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & ~ (nil = sK57) & (sK57 = sK59) & (sK58 = sK60) & (nil = sK58) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ~ (nil = X0) & (X0 = X2) & (X1 = X3) & (nil = X1) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2] : (sP6(X3, X2) <=> ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & ~ (nil = X0) & (X0 = X2) & (X1 = X3) & (nil = X1) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : ((! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2)) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & ~ (nil = X0) & (X0 = X2) & (X1 = X3) & (nil = X1)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X7] : (lt(X7, X4) & memberP(X6, X7) & ssItem(X7)) | ? [X8] : (lt(X4, X8) & memberP(X5, X8) & ssItem(X8)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | (nil = X0) | ~ (X0 = X2) | ~ (X1 = X3) | ~ (nil = X1)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | (nil = X0) | ~ (X0 = X2) | ~ (X1 = X3) | ~ (nil = X1)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X8] : (lt(X8, X4) & memberP(X6, X8) & ssItem(X8)) | ? [X7] : (lt(X4, X7) & memberP(X5, X7) & ssItem(X7)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | (nil = X0) | ~ (X0 = X2) | ~ (X1 = X3) | ~ (nil = X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC039+1.p', co1)).
fof(f565, plain, ssList(sK59), inference(cnf_transformation, [], [f356])).
fof(f4025, plain, (~ ssList(sK57) | ~ spl61_106), inference(subsumption_resolution, [], [f4021, f570])).
fof(f570, plain, ~ (nil = sK57), inference(cnf_transformation, [], [f356])).
fof(f4021, plain, ((nil = sK57) | ~ ssList(sK57) | ~ spl61_106), inference(resolution, [], [f3868, f503])).
fof(f503, plain, ! [X0] : (~ segmentP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f333])).
fof(f333, plain, ! [X0] : (((segmentP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ segmentP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((segmentP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0] : (ssList(X0) => (segmentP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC039+1.p', ax58)).
fof(f3868, plain, (segmentP(nil, sK57) | ~ spl61_106), inference(avatar_component_clause, [], [f3866])).
fof(f3866, plain, (spl61_106 <=> segmentP(nil, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl61_106])])).
fof(f3981, plain, (~ spl61_1 | ~ spl61_3 | spl61_104), inference(avatar_contradiction_clause, [], [f3980])).
fof(f3980, plain, ($false | (~ spl61_1 | ~ spl61_3 | spl61_104)), inference(subsumption_resolution, [], [f3979, f630])).
fof(f630, plain, (sP6(nil, sK57) | (~ spl61_1 | ~ spl61_3)), inference(forward_demodulation, [], [f629, f619])).
fof(f619, plain, ((nil = sK60) | ~ spl61_3), inference(avatar_component_clause, [], [f617])).
fof(f617, plain, (spl61_3 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl61_3])])).
fof(f629, plain, (sP6(sK60, sK57) | ~ spl61_1), inference(forward_demodulation, [], [f610, f569])).
fof(f610, plain, (sP6(sK60, sK59) | ~ spl61_1), inference(avatar_component_clause, [], [f608])).
fof(f608, plain, (spl61_1 <=> sP6(sK60, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_1])])).
fof(f3979, plain, (~ sP6(nil, sK57) | spl61_104), inference(resolution, [], [f3860, f557])).
fof(f557, plain, ! [X0, X1] : (ssList(sK55(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f351, plain, ! [X0, X1] : ((((! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f347, f350, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) => (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f350, plain, ! [X1, X0] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) => (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(nnf_transformation, [], [f233])).
fof(f3860, plain, (~ ssList(sK55(nil, sK57)) | spl61_104), inference(avatar_component_clause, [], [f3858])).
fof(f3858, plain, (spl61_104 <=> ssList(sK55(nil, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_104])])).
fof(f3939, plain, (~ spl61_1 | ~ spl61_3 | spl61_105), inference(avatar_contradiction_clause, [], [f3938])).
fof(f3938, plain, ($false | (~ spl61_1 | ~ spl61_3 | spl61_105)), inference(subsumption_resolution, [], [f3937, f630])).
fof(f3937, plain, (~ sP6(nil, sK57) | spl61_105), inference(resolution, [], [f3864, f558])).
fof(f558, plain, ! [X0, X1] : (ssList(sK56(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f3864, plain, (~ ssList(sK56(nil, sK57)) | spl61_105), inference(avatar_component_clause, [], [f3862])).
fof(f3862, plain, (spl61_105 <=> ssList(sK56(nil, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl61_105])])).
fof(f3869, plain, (~ spl61_104 | ~ spl61_105 | spl61_106 | ~ spl61_1 | ~ spl61_3), inference(avatar_split_clause, [], [f3856, f617, f608, f3866, f3862, f3858])).
fof(f3856, plain, (segmentP(nil, sK57) | ~ ssList(sK56(nil, sK57)) | ~ ssList(sK55(nil, sK57)) | (~ spl61_1 | ~ spl61_3)), inference(subsumption_resolution, [], [f3855, f449])).
fof(f449, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC039+1.p', ax17)).
fof(f3855, plain, (segmentP(nil, sK57) | ~ ssList(sK56(nil, sK57)) | ~ ssList(sK55(nil, sK57)) | ~ ssList(nil) | (~ spl61_1 | ~ spl61_3)), inference(subsumption_resolution, [], [f3832, f623])).
fof(f3832, plain, (segmentP(nil, sK57) | ~ ssList(sK56(nil, sK57)) | ~ ssList(sK55(nil, sK57)) | ~ ssList(sK57) | ~ ssList(nil) | (~ spl61_1 | ~ spl61_3)), inference(superposition, [], [f578, f1716])).
fof(f1716, plain, ((nil = app(app(sK55(nil, sK57), sK57), sK56(nil, sK57))) | (~ spl61_1 | ~ spl61_3)), inference(resolution, [], [f560, f630])).
fof(f560, plain, ! [X0, X1] : (~ sP6(X0, X1) | (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f351])).
fof(f578, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f378])).
fof(f378, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f260])).
fof(f260, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1))) & ssList(sK14(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15])], [f257, f259, f258])).
fof(f258, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f259, plain, ! [X1, X0] : (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1)))), introduced(choice_axiom, [])).
fof(f257, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f256])).
fof(f256, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC039+1.p', ax7)).
fof(f628, plain, ~ spl61_2, inference(avatar_contradiction_clause, [], [f627])).
fof(f627, plain, ($false | ~ spl61_2), inference(subsumption_resolution, [], [f626, f570])).
fof(f626, plain, ((nil = sK57) | ~ spl61_2), inference(backward_demodulation, [], [f569, f614])).
fof(f614, plain, ((nil = sK59) | ~ spl61_2), inference(avatar_component_clause, [], [f612])).
fof(f612, plain, (spl61_2 <=> (nil = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl61_2])])).
fof(f622, plain, spl61_3, inference(avatar_split_clause, [], [f621, f617])).
fof(f621, plain, (nil = sK60), inference(backward_demodulation, [], [f568, f567])).
fof(f567, plain, (nil = sK58), inference(cnf_transformation, [], [f356])).
fof(f568, plain, (sK58 = sK60), inference(cnf_transformation, [], [f356])).
fof(f615, plain, (spl61_1 | spl61_2), inference(avatar_split_clause, [], [f572, f612, f608])).
fof(f572, plain, ((nil = sK59) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f356])).