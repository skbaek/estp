fof(f64710, plain, $false, inference(avatar_sat_refutation, [], [f616, f622, f1855, f1888, f2559, f2645, f2772, f13644, f50886, f59169, f59565, f64537])).
fof(f64537, plain, (spl61_411 | ~ spl61_1 | ~ spl61_409 | ~ spl61_410), inference(avatar_split_clause, [], [f64536, f8650, f8646, f609, f8654])).
fof(f8654, plain, (spl61_411 <=> ! [X1, X0] : ~ sP6(X0, X1, sK57, sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_411])])).
fof(f609, plain, (spl61_1 <=> sP6(sK60, sK59, sK57, sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_1])])).
fof(f8646, plain, (spl61_409 <=> neq(cons(sK54(sK58, sK57), nil), nil)), introduced(avatar_definition, [new_symbols(naming, [spl61_409])])).
fof(f8650, plain, (spl61_410 <=> ssList(cons(sK54(sK58, sK57), nil))), introduced(avatar_definition, [new_symbols(naming, [spl61_410])])).
fof(f64536, plain, (! [X2, X1] : ~ sP6(X1, X2, sK57, sK58) | (~ spl61_1 | ~ spl61_409 | ~ spl61_410)), inference(forward_demodulation, [], [f64535, f51999])).
fof(f51999, plain, ((sK57 = app(sK55(sK58, sK57), sK56(sK58, sK57))) | ~ spl61_1), inference(resolution, [], [f625, f562])).
fof(f562, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(sK55(X0, X1), sK56(X0, X1)) = X1)), inference(cnf_transformation, [], [f351])).
fof(f351, plain, ! [X0, X1, X2, X3] : (((((! [X7] : (~ geq(X7, sK54(X0, X1)) | ~ memberP(X0, X7) | (sK54(X0, X1) = X7) | ~ ssItem(X7)) & (app(sK55(X0, X1), sK56(X0, X1)) = X1) & (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), sK56(X0, X1)) = X0) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) & ! [X8] : (! [X9] : (! [X10] : (~ neq(X9, nil) | ~ (app(X8, X10) = X2) | ~ (app(app(X8, X9), X10) = X3) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f347, f350, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ geq(X7, X4) | ~ memberP(X0, X7) | (X4 = X7) | ~ ssItem(X7)) & (app(X5, X6) = X1) & (app(app(X5, cons(X4, nil)), X6) = X0) & ssList(X6)) & ssList(X5)) & ssItem(X4)) => (? [X5] : (? [X6] : (! [X7] : (~ geq(X7, sK54(X0, X1)) | ~ memberP(X0, X7) | (sK54(X0, X1) = X7) | ~ ssItem(X7)) & (app(X5, X6) = X1) & (app(app(X5, cons(sK54(X0, X1), nil)), X6) = X0) & ssList(X6)) & ssList(X5)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X5] : (? [X6] : (! [X7] : (~ geq(X7, sK54(X0, X1)) | ~ memberP(X0, X7) | (sK54(X0, X1) = X7) | ~ ssItem(X7)) & (app(X5, X6) = X1) & (app(app(X5, cons(sK54(X0, X1), nil)), X6) = X0) & ssList(X6)) & ssList(X5)) => (? [X6] : (! [X7] : (~ geq(X7, sK54(X0, X1)) | ~ memberP(X0, X7) | (sK54(X0, X1) = X7) | ~ ssItem(X7)) & (app(sK55(X0, X1), X6) = X1) & (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), X6) = X0) & ssList(X6)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f350, plain, ! [X1, X0] : (? [X6] : (! [X7] : (~ geq(X7, sK54(X0, X1)) | ~ memberP(X0, X7) | (sK54(X0, X1) = X7) | ~ ssItem(X7)) & (app(sK55(X0, X1), X6) = X1) & (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), X6) = X0) & ssList(X6)) => (! [X7] : (~ geq(X7, sK54(X0, X1)) | ~ memberP(X0, X7) | (sK54(X0, X1) = X7) | ~ ssItem(X7)) & (app(sK55(X0, X1), sK56(X0, X1)) = X1) & (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), sK56(X0, X1)) = X0) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1, X2, X3] : ((? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ geq(X7, X4) | ~ memberP(X0, X7) | (X4 = X7) | ~ ssItem(X7)) & (app(X5, X6) = X1) & (app(app(X5, cons(X4, nil)), X6) = X0) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X8] : (! [X9] : (! [X10] : (~ neq(X9, nil) | ~ (app(X8, X10) = X2) | ~ (app(app(X8, X9), X10) = X3) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) & neq(X3, nil)) | ~ sP6(X0, X1, X2, X3)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2, X0, X1] : ((? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ geq(X7, X4) | ~ memberP(X3, X7) | (X4 = X7) | ~ ssItem(X7)) & (app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X8] : (! [X9] : (! [X10] : (~ neq(X9, nil) | ~ (app(X8, X10) = X0) | ~ (app(app(X8, X9), X10) = X1) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) & neq(X1, nil)) | ~ sP6(X3, X2, X0, X1)), inference(nnf_transformation, [], [f233])).
fof(f233, plain, ! [X3, X2, X0, X1] : ((? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ geq(X7, X4) | ~ memberP(X3, X7) | (X4 = X7) | ~ ssItem(X7)) & (app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X8] : (! [X9] : (! [X10] : (~ neq(X9, nil) | ~ (app(X8, X10) = X0) | ~ (app(app(X8, X9), X10) = X1) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) & neq(X1, nil)) | ~ sP6(X3, X2, X0, X1)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2, X0, X1] : (sP6(X3, X2, X0, X1) <=> (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ geq(X7, X4) | ~ memberP(X3, X7) | (X4 = X7) | ~ ssItem(X7)) & (app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X8] : (! [X9] : (! [X10] : (~ neq(X9, nil) | ~ (app(X8, X10) = X0) | ~ (app(app(X8, X9), X10) = X1) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) & neq(X1, nil))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f625, plain, (sP6(sK58, sK57, sK57, sK58) | ~ spl61_1), inference(forward_demodulation, [], [f624, f568])).
fof(f568, plain, (sK58 = sK60), inference(cnf_transformation, [], [f356])).
fof(f356, plain, ((((((~ neq(sK60, nil) & neq(sK58, nil)) | sP6(sK60, sK59, sK57, sK58)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60])], [f234, f355, f354, f353, f352])).
fof(f352, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X0, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, sK57, X1)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, sK57, X1)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK58, nil)) | sP6(X3, X2, sK57, sK58)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(sK58, nil)) | sP6(X3, X2, sK57, sK58)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : (((~ neq(X3, nil) & neq(sK58, nil)) | sP6(X3, sK59, sK57, sK58)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f355, plain, (? [X3] : (((~ neq(X3, nil) & neq(sK58, nil)) | sP6(X3, sK59, sK57, sK58)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => (((~ neq(sK60, nil) & neq(sK58, nil)) | sP6(sK60, sK59, sK57, sK58)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | sP6(X3, X2, X0, X1)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ geq(X7, X4) | ~ memberP(X3, X7) | (X4 = X7) | ~ ssItem(X7)) & (app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X8] : (! [X9] : (! [X10] : (~ neq(X9, nil) | ~ (app(X8, X10) = X0) | ~ (app(app(X8, X9), X10) = X1) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((~ neq(X3, nil) & neq(X1, nil)) | (? [X4] : (? [X5] : (? [X6] : ((! [X7] : (~ geq(X7, X4) | ~ memberP(X3, X7) | (X4 = X7) | ~ ssItem(X7)) & (app(X5, X6) = X2) & (app(app(X5, cons(X4, nil)), X6) = X3)) & ssList(X6)) & ssList(X5)) & ssItem(X4)) & ! [X8] : (! [X9] : (! [X10] : (~ neq(X9, nil) | ~ (app(X8, X10) = X0) | ~ (app(app(X8, X9), X10) = X1) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) & neq(X1, nil))) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X7] : (geq(X7, X4) & memberP(X3, X7) & ~ (X4 = X7) & ssItem(X7)) | ~ (app(X5, X6) = X2) | ~ (app(app(X5, cons(X4, nil)), X6) = X3))))) | ? [X8] : (? [X9] : (? [X10] : (neq(X9, nil) & (app(X8, X10) = X0) & (app(app(X8, X9), X10) = X1) & ssList(X10)) & ssList(X9)) & ssList(X8)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => ! [X9] : (ssList(X9) => (? [X10] : (geq(X10, X7) & memberP(X3, X10) & ~ (X7 = X10) & ssItem(X10)) | ~ (app(X8, X9) = X2) | ~ (app(app(X8, cons(X7, nil)), X9) = X3))))) | ? [X4] : (? [X5] : (? [X6] : (neq(X5, nil) & (app(X4, X6) = X0) & (app(app(X4, X5), X6) = X1) & ssList(X6)) & ssList(X5)) & ssList(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((neq(X3, nil) | ~ neq(X1, nil)) & (! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => ! [X9] : (ssList(X9) => (? [X10] : (geq(X10, X7) & memberP(X3, X10) & ~ (X7 = X10) & ssItem(X10)) | ~ (app(X8, X9) = X2) | ~ (app(app(X8, cons(X7, nil)), X9) = X3))))) | ? [X4] : (? [X5] : (? [X6] : (neq(X5, nil) & (app(X4, X6) = X0) & (app(app(X4, X5), X6) = X1) & ssList(X6)) & ssList(X5)) & ssList(X4)) | ~ neq(X1, nil))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', co1)).
fof(f624, plain, (sP6(sK60, sK57, sK57, sK58) | ~ spl61_1), inference(forward_demodulation, [], [f611, f569])).
fof(f569, plain, (sK57 = sK59), inference(cnf_transformation, [], [f356])).
fof(f611, plain, (sP6(sK60, sK59, sK57, sK58) | ~ spl61_1), inference(avatar_component_clause, [], [f609])).
fof(f64535, plain, (! [X2, X1] : ~ sP6(X1, X2, app(sK55(sK58, sK57), sK56(sK58, sK57)), sK58) | (~ spl61_1 | ~ spl61_409 | ~ spl61_410)), inference(subsumption_resolution, [], [f64534, f8647])).
fof(f8647, plain, (neq(cons(sK54(sK58, sK57), nil), nil) | ~ spl61_409), inference(avatar_component_clause, [], [f8646])).
fof(f64534, plain, (! [X2, X1] : (~ sP6(X1, X2, app(sK55(sK58, sK57), sK56(sK58, sK57)), sK58) | ~ neq(cons(sK54(sK58, sK57), nil), nil)) | (~ spl61_1 | ~ spl61_410)), inference(subsumption_resolution, [], [f64533, f51996])).
fof(f51996, plain, (ssList(sK55(sK58, sK57)) | ~ spl61_1), inference(resolution, [], [f625, f559])).
fof(f559, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK55(X0, X1))), inference(cnf_transformation, [], [f351])).
fof(f64533, plain, (! [X2, X1] : (~ sP6(X1, X2, app(sK55(sK58, sK57), sK56(sK58, sK57)), sK58) | ~ ssList(sK55(sK58, sK57)) | ~ neq(cons(sK54(sK58, sK57), nil), nil)) | (~ spl61_1 | ~ spl61_410)), inference(subsumption_resolution, [], [f64532, f8651])).
fof(f8651, plain, (ssList(cons(sK54(sK58, sK57), nil)) | ~ spl61_410), inference(avatar_component_clause, [], [f8650])).
fof(f64532, plain, (! [X2, X1] : (~ sP6(X1, X2, app(sK55(sK58, sK57), sK56(sK58, sK57)), sK58) | ~ ssList(cons(sK54(sK58, sK57), nil)) | ~ ssList(sK55(sK58, sK57)) | ~ neq(cons(sK54(sK58, sK57), nil), nil)) | ~ spl61_1), inference(subsumption_resolution, [], [f64485, f51997])).
fof(f51997, plain, (ssList(sK56(sK58, sK57)) | ~ spl61_1), inference(resolution, [], [f625, f560])).
fof(f560, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssList(sK56(X0, X1))), inference(cnf_transformation, [], [f351])).
fof(f64485, plain, (! [X2, X1] : (~ sP6(X1, X2, app(sK55(sK58, sK57), sK56(sK58, sK57)), sK58) | ~ ssList(sK56(sK58, sK57)) | ~ ssList(cons(sK54(sK58, sK57), nil)) | ~ ssList(sK55(sK58, sK57)) | ~ neq(cons(sK54(sK58, sK57), nil), nil)) | ~ spl61_1), inference(superposition, [], [f600, f51998])).
fof(f51998, plain, ((sK58 = app(app(sK55(sK58, sK57), cons(sK54(sK58, sK57), nil)), sK56(sK58, sK57))) | ~ spl61_1), inference(resolution, [], [f625, f561])).
fof(f561, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | (app(app(sK55(X0, X1), cons(sK54(X0, X1), nil)), sK56(X0, X1)) = X0)), inference(cnf_transformation, [], [f351])).
fof(f600, plain, ! [X0, X10, X8, X1, X9] : (~ sP6(X0, X1, app(X8, X10), app(app(X8, X9), X10)) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ neq(X9, nil)), inference(equality_resolution, [], [f599])).
fof(f599, plain, ! [X0, X10, X8, X3, X1, X9] : (~ neq(X9, nil) | ~ (app(app(X8, X9), X10) = X3) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ sP6(X0, X1, app(X8, X10), X3)), inference(equality_resolution, [], [f557])).
fof(f557, plain, ! [X2, X0, X10, X8, X3, X1, X9] : (~ neq(X9, nil) | ~ (app(X8, X10) = X2) | ~ (app(app(X8, X9), X10) = X3) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ sP6(X0, X1, X2, X3)), inference(cnf_transformation, [], [f351])).
fof(f59565, plain, (~ spl61_1 | spl61_76 | ~ spl61_669), inference(avatar_contradiction_clause, [], [f59564])).
fof(f59564, plain, ($false | (~ spl61_1 | spl61_76 | ~ spl61_669)), inference(subsumption_resolution, [], [f59562, f2558])).
fof(f2558, plain, (~ (sK58 = tl(sK58)) | spl61_76), inference(avatar_component_clause, [], [f2556])).
fof(f2556, plain, (spl61_76 <=> (sK58 = tl(sK58))), introduced(avatar_definition, [new_symbols(naming, [spl61_76])])).
fof(f59562, plain, ((sK58 = tl(sK58)) | (~ spl61_1 | ~ spl61_669)), inference(backward_demodulation, [], [f5761, f59561])).
fof(f59561, plain, ((sK58 = cons(sK54(sK58, sK57), sK58)) | (~ spl61_1 | ~ spl61_669)), inference(forward_demodulation, [], [f59431, f852])).
fof(f852, plain, (sK58 = app(nil, sK58)), inference(resolution, [], [f463, f618])).
fof(f618, plain, ssList(sK58), inference(forward_demodulation, [], [f567, f568])).
fof(f567, plain, ssList(sK60), inference(cnf_transformation, [], [f356])).
fof(f463, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax28)).
fof(f59431, plain, ((app(nil, sK58) = cons(sK54(sK58, sK57), sK58)) | (~ spl61_1 | ~ spl61_669)), inference(backward_demodulation, [], [f5758, f13822])).
fof(f13822, plain, ((nil = cons(sK54(sK58, sK57), nil)) | ~ spl61_669), inference(avatar_component_clause, [], [f13820])).
fof(f13820, plain, (spl61_669 <=> (nil = cons(sK54(sK58, sK57), nil))), introduced(avatar_definition, [new_symbols(naming, [spl61_669])])).
fof(f5758, plain, ((cons(sK54(sK58, sK57), sK58) = app(cons(sK54(sK58, sK57), nil), sK58)) | ~ spl61_1), inference(resolution, [], [f992, f1945])).
fof(f1945, plain, ! [X71] : (~ ssItem(X71) | (cons(X71, sK58) = app(cons(X71, nil), sK58))), inference(resolution, [], [f537, f618])).
fof(f537, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax81)).
fof(f992, plain, (ssItem(sK54(sK58, sK57)) | ~ spl61_1), inference(resolution, [], [f558, f625])).
fof(f558, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | ssItem(sK54(X0, X1))), inference(cnf_transformation, [], [f351])).
fof(f5761, plain, ((sK58 = tl(cons(sK54(sK58, sK57), sK58))) | ~ spl61_1), inference(resolution, [], [f992, f1140])).
fof(f1140, plain, ! [X56] : (~ ssItem(X56) | (sK58 = tl(cons(X56, sK58)))), inference(resolution, [], [f460, f618])).
fof(f460, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax25)).
fof(f59169, plain, (spl61_669 | spl61_409 | ~ spl61_410), inference(avatar_split_clause, [], [f59168, f8650, f8646, f13820])).
fof(f59168, plain, ((nil = cons(sK54(sK58, sK57), nil)) | (spl61_409 | ~ spl61_410)), inference(subsumption_resolution, [], [f59167, f8651])).
fof(f59167, plain, ((nil = cons(sK54(sK58, sK57), nil)) | ~ ssList(cons(sK54(sK58, sK57), nil)) | spl61_409), inference(subsumption_resolution, [], [f58666, f449])).
fof(f449, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax17)).
fof(f58666, plain, ((nil = cons(sK54(sK58, sK57), nil)) | ~ ssList(nil) | ~ ssList(cons(sK54(sK58, sK57), nil)) | spl61_409), inference(resolution, [], [f8648, f447])).
fof(f447, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax15)).
fof(f8648, plain, (~ neq(cons(sK54(sK58, sK57), nil), nil) | spl61_409), inference(avatar_component_clause, [], [f8646])).
fof(f50886, plain, (~ spl61_1 | ~ spl61_411), inference(avatar_contradiction_clause, [], [f50885])).
fof(f50885, plain, ($false | (~ spl61_1 | ~ spl61_411)), inference(subsumption_resolution, [], [f625, f8655])).
fof(f8655, plain, (! [X0, X1] : ~ sP6(X0, X1, sK57, sK58) | ~ spl61_411), inference(avatar_component_clause, [], [f8654])).
fof(f13644, plain, (~ spl61_1 | spl61_410), inference(avatar_contradiction_clause, [], [f13643])).
fof(f13643, plain, ($false | (~ spl61_1 | spl61_410)), inference(subsumption_resolution, [], [f13642, f449])).
fof(f13642, plain, (~ ssList(nil) | (~ spl61_1 | spl61_410)), inference(subsumption_resolution, [], [f13641, f992])).
fof(f13641, plain, (~ ssItem(sK54(sK58, sK57)) | ~ ssList(nil) | spl61_410), inference(resolution, [], [f8652, f448])).
fof(f448, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax16)).
fof(f8652, plain, (~ ssList(cons(sK54(sK58, sK57), nil)) | spl61_410), inference(avatar_component_clause, [], [f8650])).
fof(f2772, plain, (spl61_42 | spl61_75), inference(avatar_contradiction_clause, [], [f2771])).
fof(f2771, plain, ($false | (spl61_42 | spl61_75)), inference(subsumption_resolution, [], [f2770, f618])).
fof(f2770, plain, (~ ssList(sK58) | (spl61_42 | spl61_75)), inference(subsumption_resolution, [], [f2769, f1239])).
fof(f1239, plain, (~ (nil = sK58) | spl61_42), inference(avatar_component_clause, [], [f1238])).
fof(f1238, plain, (spl61_42 <=> (nil = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl61_42])])).
fof(f2769, plain, ((nil = sK58) | ~ ssList(sK58) | spl61_75), inference(resolution, [], [f2554, f457])).
fof(f457, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax22)).
fof(f2554, plain, (~ ssItem(hd(sK58)) | spl61_75), inference(avatar_component_clause, [], [f2552])).
fof(f2552, plain, (spl61_75 <=> ssItem(hd(sK58))), introduced(avatar_definition, [new_symbols(naming, [spl61_75])])).
fof(f2645, plain, (spl61_42 | spl61_74), inference(avatar_contradiction_clause, [], [f2644])).
fof(f2644, plain, ($false | (spl61_42 | spl61_74)), inference(subsumption_resolution, [], [f2643, f618])).
fof(f2643, plain, (~ ssList(sK58) | (spl61_42 | spl61_74)), inference(subsumption_resolution, [], [f2642, f1239])).
fof(f2642, plain, ((nil = sK58) | ~ ssList(sK58) | spl61_74), inference(resolution, [], [f2550, f459])).
fof(f459, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax24)).
fof(f2550, plain, (~ ssList(tl(sK58)) | spl61_74), inference(avatar_component_clause, [], [f2548])).
fof(f2548, plain, (spl61_74 <=> ssList(tl(sK58))), introduced(avatar_definition, [new_symbols(naming, [spl61_74])])).
fof(f2559, plain, (~ spl61_74 | ~ spl61_75 | ~ spl61_76 | ~ spl61_54), inference(avatar_split_clause, [], [f2528, f1852, f2556, f2552, f2548])).
fof(f1852, plain, (spl61_54 <=> (sK58 = cons(hd(sK58), tl(sK58)))), introduced(avatar_definition, [new_symbols(naming, [spl61_54])])).
fof(f2528, plain, (~ (sK58 = tl(sK58)) | ~ ssItem(hd(sK58)) | ~ ssList(tl(sK58)) | ~ spl61_54), inference(superposition, [], [f450, f1854])).
fof(f1854, plain, ((sK58 = cons(hd(sK58), tl(sK58))) | ~ spl61_54), inference(avatar_component_clause, [], [f1852])).
fof(f450, plain, ! [X0, X1] : (~ (cons(X1, X0) = X0) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ! [X0] : (! [X1] : (~ (cons(X1, X0) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (cons(X1, X0) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax18)).
fof(f1888, plain, ~ spl61_42, inference(avatar_contradiction_clause, [], [f1887])).
fof(f1887, plain, ($false | ~ spl61_42), inference(subsumption_resolution, [], [f1886, f449])).
fof(f1886, plain, (~ ssList(nil) | ~ spl61_42), inference(resolution, [], [f1862, f605])).
fof(f605, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f586])).
fof(f586, plain, ! [X1] : (~ neq(X1, X1) | ~ ssList(X1) | ~ ssList(X1)), inference(equality_resolution, [], [f446])).
fof(f446, plain, ! [X0, X1] : (~ (X0 = X1) | ~ neq(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f1862, plain, (neq(nil, nil) | ~ spl61_42), inference(backward_demodulation, [], [f617, f1240])).
fof(f1240, plain, ((nil = sK58) | ~ spl61_42), inference(avatar_component_clause, [], [f1238])).
fof(f617, plain, neq(sK58, nil), inference(subsumption_resolution, [], [f570, f556])).
fof(f556, plain, ! [X2, X0, X3, X1] : (~ sP6(X0, X1, X2, X3) | neq(X3, nil)), inference(cnf_transformation, [], [f351])).
fof(f570, plain, (neq(sK58, nil) | sP6(sK60, sK59, sK57, sK58)), inference(cnf_transformation, [], [f356])).
fof(f1855, plain, (spl61_54 | spl61_42), inference(avatar_split_clause, [], [f1762, f1238, f1852])).
fof(f1762, plain, ((nil = sK58) | (sK58 = cons(hd(sK58), tl(sK58)))), inference(resolution, [], [f534, f618])).
fof(f534, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC003+1.p', ax78)).
fof(f622, plain, spl61_2, inference(avatar_contradiction_clause, [], [f621])).
fof(f621, plain, ($false | spl61_2), inference(subsumption_resolution, [], [f620, f617])).
fof(f620, plain, (~ neq(sK58, nil) | spl61_2), inference(forward_demodulation, [], [f615, f568])).
fof(f615, plain, (~ neq(sK60, nil) | spl61_2), inference(avatar_component_clause, [], [f613])).
fof(f613, plain, (spl61_2 <=> neq(sK60, nil)), introduced(avatar_definition, [new_symbols(naming, [spl61_2])])).
fof(f616, plain, (spl61_1 | ~ spl61_2), inference(avatar_split_clause, [], [f571, f613, f609])).
fof(f571, plain, (~ neq(sK60, nil) | sP6(sK60, sK59, sK57, sK58)), inference(cnf_transformation, [], [f356])).