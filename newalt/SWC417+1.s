fof(f409446, plain, $false, inference(avatar_sat_refutation, [], [f613, f619, f1399, f2573, f2581, f2685, f11306, f11345, f138753, f409445])).
fof(f409445, plain, (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870), inference(avatar_contradiction_clause, [], [f409444])).
fof(f409444, plain, ($false | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(resolution, [], [f405019, f622])).
fof(f622, plain, (sP6(sK56, sK57, sK56, sK57) | ~ spl60_1), inference(forward_demodulation, [], [f621, f566])).
fof(f566, plain, (sK56 = sK58), inference(cnf_transformation, [], [f355])).
fof(f355, plain, ((((((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK58, sK59, sK56, sK57)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)) & ssList(sK56)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK56, sK57, sK58, sK59])], [f234, f354, f353, f352, f351])).
fof(f351, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, X0, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, sK56, X1)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, sK56, X1)) & (sK56 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X2, X3, sK56, sK57)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(X2, X3, sK56, sK57)) & (sK56 = X2) & (sK57 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK58, X3, sK56, sK57)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK57, nil)) | sP6(sK58, X3, sK56, sK57)) & (sK56 = sK58) & (sK57 = X3) & ssList(X3)) => (((~ neq(sK59, nil) & neq(sK57, nil)) | sP6(sK58, sK59, sK56, sK57)) & (sK56 = sK58) & (sK57 = sK59) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X2, X3, X0, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X2, X3, X0, X1] : ((? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X2) & (app(cons(X4, nil), X5) = X3) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (! [X8] : (~ (app(app(X8, cons(X6, nil)), X7) = X0) | ~ (app(app(X7, cons(X6, nil)), X8) = X1) | ~ ssList(X8)) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil)) | ~ sP6(X2, X3, X0, X1)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X2, X3, X0, X1] : (sP6(X2, X3, X0, X1) <=> (? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X2) & (app(cons(X4, nil), X5) = X3) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (! [X8] : (~ (app(app(X8, cons(X6, nil)), X7) = X0) | ~ (app(app(X7, cons(X6, nil)), X8) = X1) | ~ ssList(X8)) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X2) & (app(cons(X4, nil), X5) = X3) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (! [X8] : (~ (app(app(X8, cons(X6, nil)), X7) = X0) | ~ (app(app(X7, cons(X6, nil)), X8) = X1) | ~ ssList(X8)) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : (((app(X5, cons(X4, nil)) = X2) & (app(cons(X4, nil), X5) = X3)) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (! [X8] : (~ (app(app(X8, cons(X6, nil)), X7) = X0) | ~ (app(app(X7, cons(X6, nil)), X8) = X1) | ~ ssList(X8)) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => (~ (app(X5, cons(X4, nil)) = X2) | ~ (app(cons(X4, nil), X5) = X3)))) | ? [X6] : (? [X7] : (? [X8] : ((app(app(X8, cons(X6, nil)), X7) = X0) & (app(app(X7, cons(X6, nil)), X8) = X1) & ssList(X8)) & ssList(X7)) & ssItem(X6)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => (~ (app(X8, cons(X7, nil)) = X2) | ~ (app(cons(X7, nil), X8) = X3)))) | ? [X4] : (? [X5] : (? [X6] : ((app(app(X6, cons(X4, nil)), X5) = X0) & (app(app(X5, cons(X4, nil)), X6) = X1) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => (~ (app(X8, cons(X7, nil)) = X2) | ~ (app(cons(X7, nil), X8) = X3)))) | ? [X4] : (? [X5] : (? [X6] : ((app(app(X6, cons(X4, nil)), X5) = X0) & (app(app(X5, cons(X4, nil)), X6) = X1) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', co1)).
fof(f621, plain, (sP6(sK58, sK57, sK56, sK57) | ~ spl60_1), inference(forward_demodulation, [], [f608, f565])).
fof(f565, plain, (sK57 = sK59), inference(cnf_transformation, [], [f355])).
fof(f608, plain, (sP6(sK58, sK59, sK56, sK57) | ~ spl60_1), inference(avatar_component_clause, [], [f606])).
fof(f606, plain, (spl60_1 <=> sP6(sK58, sK59, sK56, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_1])])).
fof(f405019, plain, (! [X0, X1] : ~ sP6(X0, X1, sK56, sK57) | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(forward_demodulation, [], [f405018, f278906])).
fof(f278906, plain, ((sK57 = app(cons(hd(sK57), nil), tl(sK57))) | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(backward_demodulation, [], [f155324, f278568])).
fof(f278568, plain, ((sK55(sK56, sK57) = tl(sK57)) | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(backward_demodulation, [], [f152781, f278566])).
fof(f278566, plain, ((sK57 = cons(hd(sK57), sK55(sK56, sK57))) | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(forward_demodulation, [], [f278526, f155324])).
fof(f278526, plain, ((app(cons(hd(sK57), nil), sK55(sK56, sK57)) = cons(hd(sK57), sK55(sK56, sK57))) | (~ spl60_1 | ~ spl60_71)), inference(resolution, [], [f4299, f2486])).
fof(f2486, plain, (ssItem(hd(sK57)) | ~ spl60_71), inference(avatar_component_clause, [], [f2485])).
fof(f2485, plain, (spl60_71 <=> ssItem(hd(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl60_71])])).
fof(f4299, plain, (! [X4] : (~ ssItem(X4) | (cons(X4, sK55(sK56, sK57)) = app(cons(X4, nil), sK55(sK56, sK57)))) | ~ spl60_1), inference(resolution, [], [f3847, f536])).
fof(f536, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax81)).
fof(f3847, plain, (ssList(sK55(sK56, sK57)) | ~ spl60_1), inference(resolution, [], [f622, f558])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK55(X0, X1))), inference(cnf_transformation, [], [f350])).
fof(f350, plain, ! [X0, X1, X2, X3] : (((((app(sK55(X0, X1), cons(sK54(X0, X1), nil)) = X0) & (app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X1) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) & ! [X6] : (! [X7] : (! [X8] : (~ (app(app(X8, cons(X6, nil)), X7) = X2) | ~ (app(app(X7, cons(X6, nil)), X8) = X3) | ~ ssList(X8)) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55])], [f347, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X0) & (app(cons(X4, nil), X5) = X1) & ssList(X5)) & ssItem(X4)) => (? [X5] : ((app(X5, cons(sK54(X0, X1), nil)) = X0) & (app(cons(sK54(X0, X1), nil), X5) = X1) & ssList(X5)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X5] : ((app(X5, cons(sK54(X0, X1), nil)) = X0) & (app(cons(sK54(X0, X1), nil), X5) = X1) & ssList(X5)) => ((app(sK55(X0, X1), cons(sK54(X0, X1), nil)) = X0) & (app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X1) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1, X2, X3] : ((? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X0) & (app(cons(X4, nil), X5) = X1) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (! [X8] : (~ (app(app(X8, cons(X6, nil)), X7) = X2) | ~ (app(app(X7, cons(X6, nil)), X8) = X3) | ~ ssList(X8)) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X2, X3, X0, X1] : ((? [X4] : (? [X5] : ((app(X5, cons(X4, nil)) = X2) & (app(cons(X4, nil), X5) = X3) & ssList(X5)) & ssItem(X4)) & ! [X6] : (! [X7] : (! [X8] : (~ (app(app(X8, cons(X6, nil)), X7) = X0) | ~ (app(app(X7, cons(X6, nil)), X8) = X1) | ~ ssList(X8)) | ~ ssList(X7)) | ~ ssItem(X6)) & neq(X1, nil)) | ~ sP6(X2, X3, X0, X1)), inference(nnf_transformation, [], [f233])).
fof(f152781, plain, ((sK55(sK56, sK57) = tl(cons(hd(sK57), sK55(sK56, sK57)))) | (~ spl60_1 | ~ spl60_71)), inference(resolution, [], [f1105, f2486])).
fof(f1105, plain, (! [X53] : (~ ssItem(X53) | (sK55(sK56, sK57) = tl(cons(X53, sK55(sK56, sK57))))) | ~ spl60_1), inference(resolution, [], [f459, f990])).
fof(f990, plain, (ssList(sK55(sK56, sK57)) | ~ spl60_1), inference(resolution, [], [f558, f622])).
fof(f459, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax25)).
fof(f155324, plain, ((sK57 = app(cons(hd(sK57), nil), sK55(sK56, sK57))) | (~ spl60_1 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(backward_demodulation, [], [f128908, f155077])).
fof(f155077, plain, ((sK54(sK56, sK57) = hd(sK57)) | (~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(subsumption_resolution, [], [f155076, f448])).
fof(f448, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax17)).
fof(f155076, plain, (~ ssList(nil) | (sK54(sK56, sK57) = hd(sK57)) | (~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(subsumption_resolution, [], [f155066, f121719])).
fof(f121719, plain, (ssItem(sK54(sK56, sK57)) | ~ spl60_6870), inference(avatar_component_clause, [], [f121718])).
fof(f121718, plain, (spl60_6870 <=> ssItem(sK54(sK56, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl60_6870])])).
fof(f155066, plain, (~ ssItem(sK54(sK56, sK57)) | ~ ssList(nil) | (sK54(sK56, sK57) = hd(sK57)) | (~ spl60_86 | ~ spl60_468)), inference(resolution, [], [f2580, f11344])).
fof(f11344, plain, (frontsegP(sK57, cons(sK54(sK56, sK57), nil)) | ~ spl60_468), inference(avatar_component_clause, [], [f11342])).
fof(f11342, plain, (spl60_468 <=> frontsegP(sK57, cons(sK54(sK56, sK57), nil))), introduced(avatar_definition, [new_symbols(naming, [spl60_468])])).
fof(f2580, plain, (! [X4, X5] : (~ frontsegP(sK57, cons(X4, X5)) | ~ ssItem(X4) | ~ ssList(X5) | (hd(sK57) = X4)) | ~ spl60_86), inference(avatar_component_clause, [], [f2579])).
fof(f2579, plain, (spl60_86 <=> ! [X5, X4] : (~ frontsegP(sK57, cons(X4, X5)) | ~ ssItem(X4) | ~ ssList(X5) | (hd(sK57) = X4))), introduced(avatar_definition, [new_symbols(naming, [spl60_86])])).
fof(f128908, plain, ((sK57 = app(cons(sK54(sK56, sK57), nil), sK55(sK56, sK57))) | ~ spl60_1), inference(resolution, [], [f622, f559])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(cons(sK54(X0, X1), nil), sK55(X0, X1)) = X1)), inference(cnf_transformation, [], [f350])).
fof(f405018, plain, (! [X0, X1] : ~ sP6(X0, X1, sK56, app(cons(hd(sK57), nil), tl(sK57))) | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(forward_demodulation, [], [f405017, f129945])).
fof(f129945, plain, ((cons(hd(sK57), nil) = app(nil, cons(hd(sK57), nil))) | ~ spl60_71), inference(resolution, [], [f2486, f8562])).
fof(f8562, plain, ! [X9] : (~ ssItem(X9) | (cons(X9, nil) = app(nil, cons(X9, nil)))), inference(resolution, [], [f874, f448])).
fof(f874, plain, ! [X2, X3] : (~ ssList(X3) | ~ ssItem(X2) | (cons(X2, X3) = app(nil, cons(X2, X3)))), inference(resolution, [], [f447, f462])).
fof(f462, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax28)).
fof(f447, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax16)).
fof(f405017, plain, (! [X0, X1] : ~ sP6(X0, X1, sK56, app(app(nil, cons(hd(sK57), nil)), tl(sK57))) | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(subsumption_resolution, [], [f405015, f448])).
fof(f405015, plain, (! [X0, X1] : (~ sP6(X0, X1, sK56, app(app(nil, cons(hd(sK57), nil)), tl(sK57))) | ~ ssList(nil)) | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(superposition, [], [f278898, f871])).
fof(f871, plain, (sK56 = app(sK56, nil)), inference(resolution, [], [f541, f616])).
fof(f616, plain, ssList(sK56), inference(forward_demodulation, [], [f563, f566])).
fof(f563, plain, ssList(sK58), inference(cnf_transformation, [], [f355])).
fof(f541, plain, ! [X0] : (~ ssList(X0) | (app(X0, nil) = X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : ((app(X0, nil) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (ssList(X0) => (app(X0, nil) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax84)).
fof(f278898, plain, (! [X4, X5, X3] : (~ sP6(X3, X4, app(sK56, X5), app(app(X5, cons(hd(sK57), nil)), tl(sK57))) | ~ ssList(X5)) | (~ spl60_1 | ~ spl60_71 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(backward_demodulation, [], [f155087, f278568])).
fof(f155087, plain, (! [X4, X5, X3] : (~ sP6(X3, X4, app(sK56, X5), app(app(X5, cons(hd(sK57), nil)), sK55(sK56, sK57))) | ~ ssList(X5)) | (~ spl60_1 | ~ spl60_86 | ~ spl60_468 | ~ spl60_6870)), inference(backward_demodulation, [], [f5181, f155077])).
fof(f5181, plain, (! [X4, X5, X3] : (~ sP6(X3, X4, app(sK56, X5), app(app(X5, cons(sK54(sK56, sK57), nil)), sK55(sK56, sK57))) | ~ ssList(X5)) | ~ spl60_1), inference(subsumption_resolution, [], [f5180, f3847])).
fof(f5180, plain, (! [X4, X5, X3] : (~ sP6(X3, X4, app(sK56, X5), app(app(X5, cons(sK54(sK56, sK57), nil)), sK55(sK56, sK57))) | ~ ssList(X5) | ~ ssList(sK55(sK56, sK57))) | ~ spl60_1), inference(subsumption_resolution, [], [f5151, f989])).
fof(f989, plain, (ssItem(sK54(sK56, sK57)) | ~ spl60_1), inference(resolution, [], [f557, f622])).
fof(f557, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X0, X1))), inference(cnf_transformation, [], [f350])).
fof(f5151, plain, (! [X4, X5, X3] : (~ sP6(X3, X4, app(sK56, X5), app(app(X5, cons(sK54(sK56, sK57), nil)), sK55(sK56, sK57))) | ~ ssList(X5) | ~ ssItem(sK54(sK56, sK57)) | ~ ssList(sK55(sK56, sK57))) | ~ spl60_1), inference(superposition, [], [f597, f3845])).
fof(f3845, plain, ((sK56 = app(sK55(sK56, sK57), cons(sK54(sK56, sK57), nil))) | ~ spl60_1), inference(resolution, [], [f622, f560])).
fof(f560, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(sK55(X0, X1), cons(sK54(X0, X1), nil)) = X0)), inference(cnf_transformation, [], [f350])).
fof(f597, plain, ! [X6, X0, X8, X7, X1] : (~ sP6(X0, X1, app(app(X8, cons(X6, nil)), X7), app(app(X7, cons(X6, nil)), X8)) | ~ ssList(X7) | ~ ssItem(X6) | ~ ssList(X8)), inference(equality_resolution, [], [f596])).
fof(f596, plain, ! [X6, X0, X8, X7, X3, X1] : (~ (app(app(X7, cons(X6, nil)), X8) = X3) | ~ ssList(X8) | ~ ssList(X7) | ~ ssItem(X6) | ~ sP6(X0, X1, app(app(X8, cons(X6, nil)), X7), X3)), inference(equality_resolution, [], [f556])).
fof(f556, plain, ! [X6, X2, X0, X8, X7, X3, X1] : (~ (app(app(X8, cons(X6, nil)), X7) = X2) | ~ (app(app(X7, cons(X6, nil)), X8) = X3) | ~ ssList(X8) | ~ ssList(X7) | ~ ssItem(X6) | ~ sP6(X0, X1, X2, X3)), inference(cnf_transformation, [], [f350])).
fof(f138753, plain, (spl60_6870 | ~ spl60_1), inference(avatar_split_clause, [], [f128910, f606, f121718])).
fof(f128910, plain, (ssItem(sK54(sK56, sK57)) | ~ spl60_1), inference(resolution, [], [f622, f557])).
fof(f11345, plain, (~ spl60_189 | spl60_468 | ~ spl60_1), inference(avatar_split_clause, [], [f11340, f606, f11342, f5206])).
fof(f5206, plain, (spl60_189 <=> ssList(cons(sK54(sK56, sK57), nil))), introduced(avatar_definition, [new_symbols(naming, [spl60_189])])).
fof(f11340, plain, (frontsegP(sK57, cons(sK54(sK56, sK57), nil)) | ~ ssList(cons(sK54(sK56, sK57), nil)) | ~ spl60_1), inference(subsumption_resolution, [], [f11339, f615])).
fof(f615, plain, ssList(sK57), inference(forward_demodulation, [], [f564, f565])).
fof(f564, plain, ssList(sK59), inference(cnf_transformation, [], [f355])).
fof(f11339, plain, (frontsegP(sK57, cons(sK54(sK56, sK57), nil)) | ~ ssList(cons(sK54(sK56, sK57), nil)) | ~ ssList(sK57) | ~ spl60_1), inference(subsumption_resolution, [], [f5286, f3847])).
fof(f5286, plain, (frontsegP(sK57, cons(sK54(sK56, sK57), nil)) | ~ ssList(sK55(sK56, sK57)) | ~ ssList(cons(sK54(sK56, sK57), nil)) | ~ ssList(sK57) | ~ spl60_1), inference(superposition, [], [f572, f3846])).
fof(f3846, plain, ((sK57 = app(cons(sK54(sK56, sK57), nil), sK55(sK56, sK57))) | ~ spl60_1), inference(resolution, [], [f622, f559])).
fof(f572, plain, ! [X2, X1] : (frontsegP(app(X1, X2), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X1, X2))), inference(equality_resolution, [], [f370])).
fof(f370, plain, ! [X2, X0, X1] : (frontsegP(X0, X1) | ~ (app(X1, X2) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (((app(X1, sK12(X0, X1)) = X0) & ssList(sK12(X0, X1))) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f249, f250])).
fof(f250, plain, ! [X1, X0] : (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) => ((app(X1, sK12(X0, X1)) = X0) & ssList(sK12(X0, X1)))), introduced(choice_axiom, [])).
fof(f249, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f248])).
fof(f248, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X1, X2) = X0) & ssList(X2)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (! [X1] : ((frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax5)).
fof(f11306, plain, (~ spl60_1 | spl60_189), inference(avatar_contradiction_clause, [], [f11305])).
fof(f11305, plain, ($false | (~ spl60_1 | spl60_189)), inference(subsumption_resolution, [], [f11304, f448])).
fof(f11304, plain, (~ ssList(nil) | (~ spl60_1 | spl60_189)), inference(subsumption_resolution, [], [f11303, f989])).
fof(f11303, plain, (~ ssItem(sK54(sK56, sK57)) | ~ ssList(nil) | spl60_189), inference(resolution, [], [f5208, f447])).
fof(f5208, plain, (~ ssList(cons(sK54(sK56, sK57), nil)) | spl60_189), inference(avatar_component_clause, [], [f5206])).
fof(f2685, plain, (spl60_32 | spl60_71), inference(avatar_contradiction_clause, [], [f2684])).
fof(f2684, plain, ($false | (spl60_32 | spl60_71)), inference(subsumption_resolution, [], [f2683, f615])).
fof(f2683, plain, (~ ssList(sK57) | (spl60_32 | spl60_71)), inference(subsumption_resolution, [], [f2682, f1174])).
fof(f1174, plain, (~ (nil = sK57) | spl60_32), inference(avatar_component_clause, [], [f1173])).
fof(f1173, plain, (spl60_32 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl60_32])])).
fof(f2682, plain, ((nil = sK57) | ~ ssList(sK57) | spl60_71), inference(resolution, [], [f2487, f456])).
fof(f456, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax22)).
fof(f2487, plain, (~ ssItem(hd(sK57)) | spl60_71), inference(avatar_component_clause, [], [f2485])).
fof(f2581, plain, (~ spl60_71 | ~ spl60_70 | spl60_86 | spl60_32), inference(avatar_split_clause, [], [f2556, f1173, f2579, f2481, f2485])).
fof(f2481, plain, (spl60_70 <=> ssList(tl(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl60_70])])).
fof(f2556, plain, (! [X4, X5] : (~ frontsegP(sK57, cons(X4, X5)) | (hd(sK57) = X4) | ~ ssList(X5) | ~ ssList(tl(sK57)) | ~ ssItem(X4) | ~ ssItem(hd(sK57))) | spl60_32), inference(superposition, [], [f484, f1588])).
fof(f1588, plain, ((sK57 = cons(hd(sK57), tl(sK57))) | spl60_32), inference(subsumption_resolution, [], [f1586, f1174])).
fof(f1586, plain, ((nil = sK57) | (sK57 = cons(hd(sK57), tl(sK57)))), inference(resolution, [], [f533, f615])).
fof(f533, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax78)).
fof(f484, plain, ! [X2, X0, X3, X1] : (~ frontsegP(cons(X0, X2), cons(X1, X3)) | (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f330])).
fof(f330, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1)) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f329])).
fof(f329, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | (~ frontsegP(X2, X3) | ~ (X0 = X1))) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f157])).
fof(f157, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f44])).
fof(f44, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax44)).
fof(f2573, plain, (spl60_32 | spl60_70), inference(avatar_contradiction_clause, [], [f2572])).
fof(f2572, plain, ($false | (spl60_32 | spl60_70)), inference(subsumption_resolution, [], [f2571, f615])).
fof(f2571, plain, (~ ssList(sK57) | (spl60_32 | spl60_70)), inference(subsumption_resolution, [], [f2570, f1174])).
fof(f2570, plain, ((nil = sK57) | ~ ssList(sK57) | spl60_70), inference(resolution, [], [f2483, f458])).
fof(f458, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax24)).
fof(f2483, plain, (~ ssList(tl(sK57)) | spl60_70), inference(avatar_component_clause, [], [f2481])).
fof(f1399, plain, ~ spl60_32, inference(avatar_contradiction_clause, [], [f1398])).
fof(f1398, plain, ($false | ~ spl60_32), inference(subsumption_resolution, [], [f1397, f448])).
fof(f1397, plain, (~ ssList(nil) | ~ spl60_32), inference(resolution, [], [f1224, f602])).
fof(f602, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f583])).
fof(f583, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1) | ~ ssList(X1)), inference(equality_resolution, [], [f445])).
fof(f445, plain, ! [X0, X1] : (~ (X0 = X1) | ~ neq(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC417+1.p', ax15)).
fof(f1224, plain, (neq(nil, nil) | ~ spl60_32), inference(backward_demodulation, [], [f614, f1175])).
fof(f1175, plain, ((nil = sK57) | ~ spl60_32), inference(avatar_component_clause, [], [f1173])).
fof(f614, plain, neq(sK57, nil), inference(subsumption_resolution, [], [f567, f555])).
fof(f555, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X3, nil)), inference(cnf_transformation, [], [f350])).
fof(f567, plain, (neq(sK57, nil) | sP6(sK58, sK59, sK56, sK57)), inference(cnf_transformation, [], [f355])).
fof(f619, plain, spl60_2, inference(avatar_contradiction_clause, [], [f618])).
fof(f618, plain, ($false | spl60_2), inference(subsumption_resolution, [], [f617, f614])).
fof(f617, plain, (~ neq(sK57, nil) | spl60_2), inference(forward_demodulation, [], [f612, f565])).
fof(f612, plain, (~ neq(sK59, nil) | spl60_2), inference(avatar_component_clause, [], [f610])).
fof(f610, plain, (spl60_2 <=> neq(sK59, nil)), introduced(avatar_definition, [new_symbols(naming, [spl60_2])])).
fof(f613, plain, (spl60_1 | ~ spl60_2), inference(avatar_split_clause, [], [f568, f610, f606])).
fof(f568, plain, (~ neq(sK59, nil) | sP6(sK58, sK59, sK56, sK57)), inference(cnf_transformation, [], [f355])).