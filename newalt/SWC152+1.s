fof(f1391290, plain, $false, inference(avatar_sat_refutation, [], [f629, f634, f1918, f2102, f2435, f3557, f3812, f4300, f4470, f4738, f5752, f5863, f6037, f8107, f16195, f16199, f21497, f22260, f22399, f22612, f27481, f28633, f32241, f55858, f58695, f60795, f65071, f65285, f65425, f65428, f72820, f73056, f73666, f176936, f185728, f186358, f188816, f193593, f196353, f198868, f241693, f303941, f307944, f308635, f308965, f311462, f325123, f325250, f327479, f354942, f354945, f379252, f384121, f386169, f402752, f459087, f483140, f486944, f513483, f514165, f515097, f534462, f710877, f711379, f711380, f747281, f756285, f766915, f768773, f773713, f775761, f805393, f825488, f827022, f827334, f844968, f853903, f855434, f855440, f855442, f865187, f865421, f869154, f903546, f903758, f903766, f905498, f905499, f905742, f991451, f1007908, f1016630, f1023426, f1023893, f1023897, f1023900, f1023903, f1024330, f1036570, f1048239, f1061223, f1183114, f1193390, f1203879, f1204586, f1275400, f1302100, f1310304, f1322750, f1347622, f1352720, f1352963, f1357270, f1390835, f1390843, f1391286])).
fof(f1391286, plain, (spl64_22146 | ~ spl64_860 | ~ spl64_6730 | ~ spl64_26092), inference(avatar_split_clause, [], [f1391285, f772213, f138880, f16513, f575865])).
fof(f575865, plain, (spl64_22146 <=> ! [X8] : (memberP(sK53, X8) | ~ ssItem(X8) | ~ memberP(sK62, X8))), introduced(avatar_definition, [new_symbols(naming, [spl64_22146])])).
fof(f16513, plain, (spl64_860 <=> ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_860])])).
fof(f138880, plain, (spl64_6730 <=> (sK57 = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl64_6730])])).
fof(f772213, plain, (spl64_26092 <=> ssList(sK62)), introduced(avatar_definition, [new_symbols(naming, [spl64_26092])])).
fof(f1391285, plain, (! [X8] : (memberP(sK53, X8) | ~ memberP(sK62, X8) | ~ ssItem(X8)) | (~ spl64_860 | ~ spl64_6730 | ~ spl64_26092)), inference(subsumption_resolution, [], [f1391284, f1357376])).
fof(f1357376, plain, (ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK57, nil))) | (~ spl64_860 | ~ spl64_6730)), inference(backward_demodulation, [], [f16514, f138882])).
fof(f138882, plain, ((sK57 = sK59) | ~ spl64_6730), inference(avatar_component_clause, [], [f138880])).
fof(f16514, plain, (ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl64_860), inference(avatar_component_clause, [], [f16513])).
fof(f1391284, plain, (! [X8] : (~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK57, nil))) | memberP(sK53, X8) | ~ memberP(sK62, X8) | ~ ssItem(X8)) | (~ spl64_6730 | ~ spl64_26092)), inference(forward_demodulation, [], [f1391283, f138882])).
fof(f1391283, plain, (! [X8] : (memberP(sK53, X8) | ~ memberP(sK62, X8) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssItem(X8)) | ~ spl64_26092), inference(subsumption_resolution, [], [f1352921, f772214])).
fof(f772214, plain, (ssList(sK62) | ~ spl64_26092), inference(avatar_component_clause, [], [f772213])).
fof(f1352921, plain, ! [X8] : (memberP(sK53, X8) | ~ memberP(sK62, X8) | ~ ssList(sK62) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssItem(X8)), inference(superposition, [], [f473, f565])).
fof(f565, plain, (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)), inference(cnf_transformation, [], [f354])).
fof(f354, plain, (((((((nil = sK55) & (nil = sK56)) | (! [X5] : (~ leq(sK57, X5) | ~ memberP(sK56, X5) | (sK57 = X5) | ~ ssItem(X5)) & memberP(sK56, sK57) & (sK55 = cons(sK57, nil)) & ssItem(sK57))) & ((((((~ leq(sK58, sK59) | ((~ leq(sK63, sK59) | ~ leq(sK58, sK63)) & memberP(sK61, sK63) & ssItem(sK63))) & leq(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)) & ssList(sK62)) & ssList(sK61)) & ssList(sK60)) & ssItem(sK59)) & ssItem(sK58)) & (sK53 = sK55) & (sK54 = sK56) & ssList(sK56)) & ssList(sK55)) & ssList(sK54)) & ssList(sK53)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK53, sK54, sK55, sK56, sK57, sK58, sK59, sK60, sK61, sK62, sK63])], [f222, f353, f352, f351, f350, f349, f348, f347, f346, f345, f344, f343])).
fof(f343, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = X0) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK53))), introduced(choice_axiom, [])).
fof(f344, plain, (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = X2) & (sK54 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK54))), introduced(choice_axiom, [])).
fof(f345, plain, (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = X2) & (sK54 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((((nil = sK55) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = sK55) & (sK54 = X3) & ssList(X3)) & ssList(sK55))), introduced(choice_axiom, [])).
fof(f346, plain, (? [X3] : ((((nil = sK55) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = sK55) & (sK54 = X3) & ssList(X3)) => ((((nil = sK55) & (nil = sK56)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(sK56, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(sK56, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (sK53 = sK55) & (sK54 = sK56) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f347, plain, (? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(sK56, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(sK56, X4) & (cons(X4, nil) = sK55) & ssItem(X4)) => (! [X5] : (~ leq(sK57, X5) | ~ memberP(sK56, X5) | (sK57 = X5) | ~ ssItem(X5)) & memberP(sK56, sK57) & (sK55 = cons(sK57, nil)) & ssItem(sK57))), introduced(choice_axiom, [])).
fof(f348, plain, (? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = sK53) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) => (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(sK58, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(sK58, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, sK58) & (sK53 = app(app(app(app(X8, cons(sK58, nil)), X9), cons(X7, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(sK58))), introduced(choice_axiom, [])).
fof(f349, plain, (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(sK58, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(sK58, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, sK58) & (sK53 = app(app(app(app(X8, cons(sK58, nil)), X9), cons(X7, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) => (? [X8] : (? [X9] : (? [X10] : ((~ leq(sK58, sK59) | ? [X11] : ((~ leq(X11, sK59) | ~ leq(sK58, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(sK59, sK58) & (sK53 = app(app(app(app(X8, cons(sK58, nil)), X9), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(sK59))), introduced(choice_axiom, [])).
fof(f350, plain, (? [X8] : (? [X9] : (? [X10] : ((~ leq(sK58, sK59) | ? [X11] : ((~ leq(X11, sK59) | ~ leq(sK58, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(sK59, sK58) & (sK53 = app(app(app(app(X8, cons(sK58, nil)), X9), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(X8)) => (? [X9] : (? [X10] : ((~ leq(sK58, sK59) | ? [X11] : ((~ leq(X11, sK59) | ~ leq(sK58, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), X9), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(X9)) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f351, plain, (? [X9] : (? [X10] : ((~ leq(sK58, sK59) | ? [X11] : ((~ leq(X11, sK59) | ~ leq(sK58, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), X9), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(X9)) => (? [X10] : ((~ leq(sK58, sK59) | ? [X11] : ((~ leq(X11, sK59) | ~ leq(sK58, X11)) & memberP(sK61, X11) & ssItem(X11))) & leq(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X10)) & ssList(X10)) & ssList(sK61))), introduced(choice_axiom, [])).
fof(f352, plain, (? [X10] : ((~ leq(sK58, sK59) | ? [X11] : ((~ leq(X11, sK59) | ~ leq(sK58, X11)) & memberP(sK61, X11) & ssItem(X11))) & leq(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X10)) & ssList(X10)) => ((~ leq(sK58, sK59) | ? [X11] : ((~ leq(X11, sK59) | ~ leq(sK58, X11)) & memberP(sK61, X11) & ssItem(X11))) & leq(sK59, sK58) & (sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)) & ssList(sK62))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X11] : ((~ leq(X11, sK59) | ~ leq(sK58, X11)) & memberP(sK61, X11) & ssItem(X11)) => ((~ leq(sK63, sK59) | ~ leq(sK58, sK63)) & memberP(sK61, sK63) & ssItem(sK63))), introduced(choice_axiom, [])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (! [X5] : (~ leq(X4, X5) | ~ memberP(X3, X5) | (X4 = X5) | ~ ssItem(X5)) & memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X6] : (? [X7] : (? [X8] : (? [X9] : (? [X10] : ((~ leq(X6, X7) | ? [X11] : ((~ leq(X11, X7) | ~ leq(X6, X11)) & memberP(X9, X11) & ssItem(X11))) & leq(X7, X6) & (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = X0) & ssList(X10)) & ssList(X9)) & ssList(X8)) & ssItem(X7)) & ssItem(X6)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (? [X5] : (leq(X4, X5) & memberP(X3, X5) & ~ (X4 = X5) & ssItem(X5)) | ~ memberP(X3, X4) | ~ (cons(X4, nil) = X2) | ~ ssItem(X4))) | ! [X6] : (ssItem(X6) => ! [X7] : (ssItem(X7) => ! [X8] : (ssList(X8) => ! [X9] : (ssList(X9) => ! [X10] : ((leq(X6, X7) & ! [X11] : ((leq(X11, X7) & leq(X6, X11)) | ~ memberP(X9, X11) | ~ ssItem(X11))) | ~ leq(X7, X6) | ~ (app(app(app(app(X8, cons(X6, nil)), X9), cons(X7, nil)), X10) = X0) | ~ ssList(X10)))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((~ (nil = X2) | ~ (nil = X3)) & ! [X10] : (? [X11] : (leq(X10, X11) & memberP(X3, X11) & ~ (X10 = X11) & ssItem(X11)) | ~ memberP(X3, X10) | ~ (cons(X10, nil) = X2) | ~ ssItem(X10))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => ! [X8] : ((leq(X4, X5) & ! [X9] : ((leq(X9, X5) & leq(X4, X9)) | ~ memberP(X7, X9) | ~ ssItem(X9))) | ~ leq(X5, X4) | ~ (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = X0) | ~ ssList(X8)))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((~ (nil = X2) | ~ (nil = X3)) & ! [X10] : (? [X11] : (leq(X10, X11) & memberP(X3, X11) & ~ (X10 = X11) & ssItem(X11)) | ~ memberP(X3, X10) | ~ (cons(X10, nil) = X2) | ~ ssItem(X10))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => ! [X8] : ((leq(X4, X5) & ! [X9] : ((leq(X9, X5) & leq(X4, X9)) | ~ memberP(X7, X9) | ~ ssItem(X9))) | ~ leq(X5, X4) | ~ (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = X0) | ~ ssList(X8)))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', co1)).
fof(f473, plain, ! [X2, X0, X1] : (memberP(app(X1, X2), X0) | ~ memberP(X2, X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f323])).
fof(f323, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & (memberP(X2, X0) | memberP(X1, X0) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f322])).
fof(f322, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & ((memberP(X2, X0) | memberP(X1, X0)) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax36)).
fof(f1390843, plain, (spl64_16923 | ~ spl64_5 | ~ spl64_150 | ~ spl64_184 | spl64_4111 | ~ spl64_26876 | ~ spl64_27395), inference(avatar_split_clause, [], [f1390842, f904649, f852702, f73037, f3564, f3411, f631, f338589])).
fof(f338589, plain, (spl64_16923 <=> ! [X70] : (~ strictorderedP(app(app(X70, sK53), sK62)) | ~ ssList(app(app(X70, sK53), sK62)) | ~ ssList(X70))), introduced(avatar_definition, [new_symbols(naming, [spl64_16923])])).
fof(f631, plain, (spl64_5 <=> ssItem(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl64_5])])).
fof(f3411, plain, (spl64_150 <=> ssList(sK49(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl64_150])])).
fof(f3564, plain, (spl64_184 <=> (sK62 = cons(sK50(sK62), sK49(sK62)))), introduced(avatar_definition, [new_symbols(naming, [spl64_184])])).
fof(f73037, plain, (spl64_4111 <=> lt(sK57, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl64_4111])])).
fof(f852702, plain, (spl64_26876 <=> (sK57 = sK50(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl64_26876])])).
fof(f904649, plain, (spl64_27395 <=> ! [X44, X43, X45] : (~ strictorderedP(app(app(X43, sK53), cons(X44, X45))) | ~ ssList(app(app(X43, sK53), cons(X44, X45))) | lt(sK57, X44) | ~ ssItem(X44) | ~ ssList(X43) | ~ ssList(X45))), introduced(avatar_definition, [new_symbols(naming, [spl64_27395])])).
fof(f1390842, plain, (! [X65] : (~ strictorderedP(app(app(X65, sK53), sK62)) | ~ ssList(app(app(X65, sK53), sK62)) | ~ ssList(X65)) | (~ spl64_5 | ~ spl64_150 | ~ spl64_184 | spl64_4111 | ~ spl64_26876 | ~ spl64_27395)), inference(subsumption_resolution, [], [f1390841, f3412])).
fof(f3412, plain, (ssList(sK49(sK62)) | ~ spl64_150), inference(avatar_component_clause, [], [f3411])).
fof(f1390841, plain, (! [X65] : (~ strictorderedP(app(app(X65, sK53), sK62)) | ~ ssList(app(app(X65, sK53), sK62)) | ~ ssList(X65) | ~ ssList(sK49(sK62))) | (~ spl64_5 | ~ spl64_184 | spl64_4111 | ~ spl64_26876 | ~ spl64_27395)), inference(subsumption_resolution, [], [f1390840, f633])).
fof(f633, plain, (ssItem(sK57) | ~ spl64_5), inference(avatar_component_clause, [], [f631])).
fof(f1390840, plain, (! [X65] : (~ strictorderedP(app(app(X65, sK53), sK62)) | ~ ssList(app(app(X65, sK53), sK62)) | ~ ssItem(sK57) | ~ ssList(X65) | ~ ssList(sK49(sK62))) | (~ spl64_184 | spl64_4111 | ~ spl64_26876 | ~ spl64_27395)), inference(subsumption_resolution, [], [f1348708, f73038])).
fof(f73038, plain, (~ lt(sK57, sK57) | spl64_4111), inference(avatar_component_clause, [], [f73037])).
fof(f1348708, plain, (! [X65] : (~ strictorderedP(app(app(X65, sK53), sK62)) | ~ ssList(app(app(X65, sK53), sK62)) | lt(sK57, sK57) | ~ ssItem(sK57) | ~ ssList(X65) | ~ ssList(sK49(sK62))) | (~ spl64_184 | ~ spl64_26876 | ~ spl64_27395)), inference(superposition, [], [f904650, f1348045])).
fof(f1348045, plain, ((sK62 = cons(sK57, sK49(sK62))) | (~ spl64_184 | ~ spl64_26876)), inference(backward_demodulation, [], [f3566, f852704])).
fof(f852704, plain, ((sK57 = sK50(sK62)) | ~ spl64_26876), inference(avatar_component_clause, [], [f852702])).
fof(f3566, plain, ((sK62 = cons(sK50(sK62), sK49(sK62))) | ~ spl64_184), inference(avatar_component_clause, [], [f3564])).
fof(f904650, plain, (! [X45, X43, X44] : (~ strictorderedP(app(app(X43, sK53), cons(X44, X45))) | ~ ssList(app(app(X43, sK53), cons(X44, X45))) | lt(sK57, X44) | ~ ssItem(X44) | ~ ssList(X43) | ~ ssList(X45)) | ~ spl64_27395), inference(avatar_component_clause, [], [f904649])).
fof(f1390835, plain, (~ spl64_4 | ~ spl64_114 | spl64_122 | ~ spl64_186 | ~ spl64_277 | ~ spl64_4984 | ~ spl64_6730 | ~ spl64_16908 | ~ spl64_16923 | ~ spl64_26086 | ~ spl64_30426), inference(avatar_contradiction_clause, [], [f1390834])).
fof(f1390834, plain, ($false | (~ spl64_4 | ~ spl64_114 | spl64_122 | ~ spl64_186 | ~ spl64_277 | ~ spl64_4984 | ~ spl64_6730 | ~ spl64_16908 | ~ spl64_16923 | ~ spl64_26086 | ~ spl64_30426)), inference(subsumption_resolution, [], [f1390833, f85862])).
fof(f85862, plain, (ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ spl64_4984), inference(avatar_component_clause, [], [f85861])).
fof(f85861, plain, (spl64_4984 <=> ssList(app(app(sK60, cons(sK58, nil)), sK61))), introduced(avatar_definition, [new_symbols(naming, [spl64_4984])])).
fof(f1390833, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl64_4 | ~ spl64_114 | spl64_122 | ~ spl64_186 | ~ spl64_277 | ~ spl64_6730 | ~ spl64_16908 | ~ spl64_16923 | ~ spl64_26086 | ~ spl64_30426)), inference(subsumption_resolution, [], [f1390832, f667])).
fof(f667, plain, ssList(sK53), inference(forward_demodulation, [], [f556, f559])).
fof(f559, plain, (sK53 = sK55), inference(cnf_transformation, [], [f354])).
fof(f556, plain, ssList(sK55), inference(cnf_transformation, [], [f354])).
fof(f1390832, plain, (~ ssList(sK53) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl64_4 | ~ spl64_114 | spl64_122 | ~ spl64_186 | ~ spl64_277 | ~ spl64_6730 | ~ spl64_16908 | ~ spl64_16923 | ~ spl64_26086 | ~ spl64_30426)), inference(subsumption_resolution, [], [f1390805, f2838])).
fof(f2838, plain, (strictorderedP(sK53) | ~ spl64_114), inference(avatar_component_clause, [], [f2836])).
fof(f2836, plain, (spl64_114 <=> strictorderedP(sK53)), introduced(avatar_definition, [new_symbols(naming, [spl64_114])])).
fof(f1390805, plain, (~ strictorderedP(sK53) | ~ ssList(sK53) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl64_4 | spl64_122 | ~ spl64_186 | ~ spl64_277 | ~ spl64_6730 | ~ spl64_16908 | ~ spl64_16923 | ~ spl64_26086 | ~ spl64_30426)), inference(superposition, [], [f338590, f1358841])).
fof(f1358841, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), sK53), sK62)) | (~ spl64_4 | spl64_122 | ~ spl64_186 | ~ spl64_277 | ~ spl64_6730 | ~ spl64_16908 | ~ spl64_26086 | ~ spl64_30426)), inference(forward_demodulation, [], [f1357289, f1350873])).
fof(f1350873, plain, ((sK53 = cons(sK57, nil)) | (~ spl64_4 | spl64_122 | ~ spl64_186 | ~ spl64_277 | ~ spl64_16908 | ~ spl64_26086 | ~ spl64_30426)), inference(forward_demodulation, [], [f1344395, f1350471])).
fof(f1350471, plain, ((sK57 = hd(sK53)) | (~ spl64_4 | spl64_122 | ~ spl64_186 | ~ spl64_277 | ~ spl64_16908 | ~ spl64_26086)), inference(forward_demodulation, [], [f1303208, f1348111])).
fof(f1348111, plain, ((sK57 = hd(cons(sK57, sK53))) | (~ spl64_277 | ~ spl64_16908)), inference(backward_demodulation, [], [f854141, f338509])).
fof(f338509, plain, ((sK57 = hd(sK62)) | ~ spl64_16908), inference(avatar_component_clause, [], [f338507])).
fof(f338507, plain, (spl64_16908 <=> (sK57 = hd(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl64_16908])])).
fof(f854141, plain, ((hd(sK62) = hd(cons(hd(sK62), sK53))) | ~ spl64_277), inference(resolution, [], [f5174, f2814])).
fof(f2814, plain, ! [X0] : (~ ssItem(X0) | (hd(cons(X0, sK53)) = X0)), inference(resolution, [], [f667, f456])).
fof(f456, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax23)).
fof(f5174, plain, (ssItem(hd(sK62)) | ~ spl64_277), inference(avatar_component_clause, [], [f5173])).
fof(f5173, plain, (spl64_277 <=> ssItem(hd(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl64_277])])).
fof(f1303208, plain, ((hd(sK53) = hd(cons(sK57, sK53))) | (~ spl64_4 | spl64_122 | ~ spl64_186 | ~ spl64_26086)), inference(forward_demodulation, [], [f1053306, f1036770])).
fof(f1036770, plain, ((cons(sK57, sK53) = app(sK53, sK53)) | (~ spl64_4 | ~ spl64_186 | ~ spl64_26086)), inference(forward_demodulation, [], [f1036735, f65286])).
fof(f65286, plain, ((sK53 = cons(sK57, nil)) | ~ spl64_4), inference(forward_demodulation, [], [f628, f559])).
fof(f628, plain, ((sK55 = cons(sK57, nil)) | ~ spl64_4), inference(avatar_component_clause, [], [f626])).
fof(f626, plain, (spl64_4 <=> (sK55 = cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_4])])).
fof(f1036735, plain, ((cons(sK57, sK53) = app(cons(sK57, nil), sK53)) | (~ spl64_186 | ~ spl64_26086)), inference(backward_demodulation, [], [f198906, f768738])).
fof(f768738, plain, ((sK57 = sK50(sK53)) | ~ spl64_26086), inference(avatar_component_clause, [], [f768736])).
fof(f768736, plain, (spl64_26086 <=> (sK57 = sK50(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_26086])])).
fof(f198906, plain, ((cons(sK50(sK53), sK53) = app(cons(sK50(sK53), nil), sK53)) | ~ spl64_186), inference(resolution, [], [f3667, f2821])).
fof(f2821, plain, ! [X4] : (~ ssItem(X4) | (cons(X4, sK53) = app(cons(X4, nil), sK53))), inference(resolution, [], [f667, f535])).
fof(f535, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax81)).
fof(f3667, plain, (ssItem(sK50(sK53)) | ~ spl64_186), inference(avatar_component_clause, [], [f3666])).
fof(f3666, plain, (spl64_186 <=> ssItem(sK50(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_186])])).
fof(f1053306, plain, ((hd(sK53) = hd(app(sK53, sK53))) | spl64_122), inference(subsumption_resolution, [], [f71337, f2873])).
fof(f2873, plain, (~ (nil = sK53) | spl64_122), inference(avatar_component_clause, [], [f2872])).
fof(f2872, plain, (spl64_122 <=> (nil = sK53)), introduced(avatar_definition, [new_symbols(naming, [spl64_122])])).
fof(f71337, plain, ((hd(sK53) = hd(app(sK53, sK53))) | (nil = sK53)), inference(resolution, [], [f2824, f667])).
fof(f2824, plain, ! [X7] : (~ ssList(X7) | (hd(X7) = hd(app(X7, sK53))) | (nil = X7)), inference(resolution, [], [f667, f541])).
fof(f541, plain, ! [X0, X1] : (~ ssList(X1) | (nil = X0) | (hd(X0) = hd(app(X0, X1))) | ~ ssList(X0)), inference(cnf_transformation, [], [f204])).
fof(f204, plain, ! [X0] : (! [X1] : ((hd(X0) = hd(app(X0, X1))) | (nil = X0) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f203])).
fof(f203, plain, ! [X0] : (! [X1] : (((hd(X0) = hd(app(X0, X1))) | (nil = X0)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f85])).
fof(f85, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (~ (nil = X0) => (hd(X0) = hd(app(X0, X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax85)).
fof(f1344395, plain, ((sK53 = cons(hd(sK53), nil)) | (spl64_122 | ~ spl64_30426)), inference(forward_demodulation, [], [f1056566, f1322510])).
fof(f1322510, plain, ((nil = tl(sK53)) | ~ spl64_30426), inference(avatar_component_clause, [], [f1322508])).
fof(f1322508, plain, (spl64_30426 <=> (nil = tl(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_30426])])).
fof(f1056566, plain, ((sK53 = cons(hd(sK53), tl(sK53))) | spl64_122), inference(subsumption_resolution, [], [f1056217, f2873])).
fof(f1056217, plain, ((nil = sK53) | (sK53 = cons(hd(sK53), tl(sK53)))), inference(resolution, [], [f667, f532])).
fof(f532, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax78)).
fof(f1357289, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK57, nil)), sK62)) | ~ spl64_6730), inference(backward_demodulation, [], [f565, f138882])).
fof(f338590, plain, (! [X70] : (~ strictorderedP(app(app(X70, sK53), sK62)) | ~ ssList(app(app(X70, sK53), sK62)) | ~ ssList(X70)) | ~ spl64_16923), inference(avatar_component_clause, [], [f338589])).
fof(f1357270, plain, (spl64_6730 | ~ spl64_78 | ~ spl64_860 | ~ spl64_4981 | ~ spl64_4984 | ~ spl64_28572), inference(avatar_split_clause, [], [f1357262, f1023809, f85861, f85844, f16513, f2098, f138880])).
fof(f2098, plain, (spl64_78 <=> ssItem(sK59)), introduced(avatar_definition, [new_symbols(naming, [spl64_78])])).
fof(f85844, plain, (spl64_4981 <=> ! [X7] : (memberP(sK53, X7) | ~ ssItem(X7) | ~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7))), introduced(avatar_definition, [new_symbols(naming, [spl64_4981])])).
fof(f1023809, plain, (spl64_28572 <=> ! [X8] : (~ memberP(sK53, X8) | ~ ssItem(X8) | (sK57 = X8))), introduced(avatar_definition, [new_symbols(naming, [spl64_28572])])).
fof(f1357262, plain, ((sK57 = sK59) | (~ spl64_78 | ~ spl64_860 | ~ spl64_4981 | ~ spl64_4984 | ~ spl64_28572)), inference(subsumption_resolution, [], [f1357260, f2099])).
fof(f2099, plain, (ssItem(sK59) | ~ spl64_78), inference(avatar_component_clause, [], [f2098])).
fof(f1357260, plain, (~ ssItem(sK59) | (sK57 = sK59) | (~ spl64_78 | ~ spl64_860 | ~ spl64_4981 | ~ spl64_4984 | ~ spl64_28572)), inference(resolution, [], [f1357058, f1023810])).
fof(f1023810, plain, (! [X8] : (~ memberP(sK53, X8) | ~ ssItem(X8) | (sK57 = X8)) | ~ spl64_28572), inference(avatar_component_clause, [], [f1023809])).
fof(f1357058, plain, (memberP(sK53, sK59) | (~ spl64_78 | ~ spl64_860 | ~ spl64_4981 | ~ spl64_4984)), inference(subsumption_resolution, [], [f1357057, f16514])).
fof(f1357057, plain, (memberP(sK53, sK59) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl64_78 | ~ spl64_4981 | ~ spl64_4984)), inference(subsumption_resolution, [], [f1357056, f85862])).
fof(f1357056, plain, (memberP(sK53, sK59) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl64_78 | ~ spl64_4981)), inference(subsumption_resolution, [], [f1357055, f447])).
fof(f447, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax17)).
fof(f1357055, plain, (memberP(sK53, sK59) | ~ ssList(nil) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl64_78 | ~ spl64_4981)), inference(subsumption_resolution, [], [f1357054, f2099])).
fof(f1357054, plain, (~ ssItem(sK59) | memberP(sK53, sK59) | ~ ssList(nil) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl64_4981), inference(duplicate_literal_removal, [], [f1357049])).
fof(f1357049, plain, (~ ssItem(sK59) | memberP(sK53, sK59) | ~ ssList(nil) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssItem(sK59) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl64_4981), inference(resolution, [], [f85845, f579])).
fof(f579, plain, ! [X2, X3, X1] : (memberP(app(X2, cons(X1, X3)), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(app(X2, cons(X1, X3)))), inference(equality_resolution, [], [f363])).
fof(f363, plain, ! [X2, X0, X3, X1] : (memberP(X0, X1) | ~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f240])).
fof(f240, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(sK8(X0, X1), cons(X1, sK9(X0, X1))) = X0) & ssList(sK9(X0, X1))) & ssList(sK8(X0, X1))) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f237, f239, f238])).
fof(f238, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(sK8(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) & ssList(sK8(X0, X1)))), introduced(choice_axiom, [])).
fof(f239, plain, ! [X1, X0] : (? [X5] : ((app(sK8(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) => ((app(sK8(X0, X1), cons(X1, sK9(X0, X1))) = X0) & ssList(sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f237, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(rectify, [], [f236])).
fof(f236, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f100])).
fof(f100, plain, ! [X0] : (! [X1] : ((memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax3)).
fof(f85845, plain, (! [X7] : (~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7) | ~ ssItem(X7) | memberP(sK53, X7)) | ~ spl64_4981), inference(avatar_component_clause, [], [f85844])).
fof(f1352963, plain, (spl64_4981 | ~ spl64_860 | ~ spl64_26092), inference(avatar_split_clause, [], [f1352962, f772213, f16513, f85844])).
fof(f1352962, plain, (! [X7] : (memberP(sK53, X7) | ~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7) | ~ ssItem(X7)) | (~ spl64_860 | ~ spl64_26092)), inference(subsumption_resolution, [], [f1352961, f16514])).
fof(f1352961, plain, (! [X7] : (memberP(sK53, X7) | ~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssItem(X7)) | ~ spl64_26092), inference(subsumption_resolution, [], [f1352920, f772214])).
fof(f1352920, plain, ! [X7] : (memberP(sK53, X7) | ~ memberP(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), X7) | ~ ssList(sK62) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssItem(X7)), inference(superposition, [], [f472, f565])).
fof(f472, plain, ! [X2, X0, X1] : (memberP(app(X1, X2), X0) | ~ memberP(X1, X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f323])).
fof(f1352720, plain, (spl64_4030 | ~ spl64_4035 | ~ spl64_63 | ~ spl64_241 | ~ spl64_242 | ~ spl64_28570), inference(avatar_split_clause, [], [f1203938, f1023798, f4901, f4897, f1920, f72177, f72062])).
fof(f72062, plain, (spl64_4030 <=> (sK57 = hd(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_4030])])).
fof(f72177, plain, (spl64_4035 <=> frontsegP(sK53, sK60)), introduced(avatar_definition, [new_symbols(naming, [spl64_4035])])).
fof(f1920, plain, (spl64_63 <=> (sK60 = cons(hd(sK60), tl(sK60)))), introduced(avatar_definition, [new_symbols(naming, [spl64_63])])).
fof(f4897, plain, (spl64_241 <=> ssList(tl(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_241])])).
fof(f4901, plain, (spl64_242 <=> ssItem(hd(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_242])])).
fof(f1023798, plain, (spl64_28570 <=> ! [X13, X12] : (~ frontsegP(sK53, cons(X12, X13)) | ~ ssItem(X12) | ~ ssList(X13) | (sK57 = X12))), introduced(avatar_definition, [new_symbols(naming, [spl64_28570])])).
fof(f1203938, plain, (~ frontsegP(sK53, sK60) | (sK57 = hd(sK60)) | (~ spl64_63 | ~ spl64_241 | ~ spl64_242 | ~ spl64_28570)), inference(subsumption_resolution, [], [f1203937, f4898])).
fof(f4898, plain, (ssList(tl(sK60)) | ~ spl64_241), inference(avatar_component_clause, [], [f4897])).
fof(f1203937, plain, (~ frontsegP(sK53, sK60) | ~ ssList(tl(sK60)) | (sK57 = hd(sK60)) | (~ spl64_63 | ~ spl64_242 | ~ spl64_28570)), inference(subsumption_resolution, [], [f1061706, f4902])).
fof(f4902, plain, (ssItem(hd(sK60)) | ~ spl64_242), inference(avatar_component_clause, [], [f4901])).
fof(f1061706, plain, (~ frontsegP(sK53, sK60) | ~ ssItem(hd(sK60)) | ~ ssList(tl(sK60)) | (sK57 = hd(sK60)) | (~ spl64_63 | ~ spl64_28570)), inference(superposition, [], [f1023799, f1922])).
fof(f1922, plain, ((sK60 = cons(hd(sK60), tl(sK60))) | ~ spl64_63), inference(avatar_component_clause, [], [f1920])).
fof(f1023799, plain, (! [X12, X13] : (~ frontsegP(sK53, cons(X12, X13)) | ~ ssItem(X12) | ~ ssList(X13) | (sK57 = X12)) | ~ spl64_28570), inference(avatar_component_clause, [], [f1023798])).
fof(f1347622, plain, (spl64_184 | spl64_41 | ~ spl64_26092), inference(avatar_split_clause, [], [f1325794, f772213, f1227, f3564])).
fof(f1227, plain, (spl64_41 <=> (nil = sK62)), introduced(avatar_definition, [new_symbols(naming, [spl64_41])])).
fof(f1325794, plain, ((sK62 = cons(sK50(sK62), sK49(sK62))) | (spl64_41 | ~ spl64_26092)), inference(subsumption_resolution, [], [f1325685, f1228])).
fof(f1228, plain, (~ (nil = sK62) | spl64_41), inference(avatar_component_clause, [], [f1227])).
fof(f1325685, plain, ((nil = sK62) | (sK62 = cons(sK50(sK62), sK49(sK62))) | ~ spl64_26092), inference(resolution, [], [f772214, f453])).
fof(f453, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(sK50(X0), sK49(X0)) = X0)), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ! [X0] : ((((cons(sK50(X0), sK49(X0)) = X0) & ssItem(sK50(X0))) & ssList(sK49(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK49, sK50])], [f125, f318, f317])).
fof(f317, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) => (? [X2] : ((cons(X2, sK49(X0)) = X0) & ssItem(X2)) & ssList(sK49(X0)))), introduced(choice_axiom, [])).
fof(f318, plain, ! [X0] : (? [X2] : ((cons(X2, sK49(X0)) = X0) & ssItem(X2)) => ((cons(sK50(X0), sK49(X0)) = X0) & ssItem(sK50(X0)))), introduced(choice_axiom, [])).
fof(f125, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0] : ((? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (ssList(X0) => (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax20)).
fof(f1322750, plain, (spl64_30426 | ~ spl64_4 | ~ spl64_5), inference(avatar_split_clause, [], [f1056181, f631, f626, f1322508])).
fof(f1056181, plain, ((nil = tl(sK53)) | (~ spl64_4 | ~ spl64_5)), inference(forward_demodulation, [], [f1056143, f65286])).
fof(f1056143, plain, ((nil = tl(cons(sK57, nil))) | ~ spl64_5), inference(resolution, [], [f633, f1123])).
fof(f1123, plain, ! [X6] : (~ ssItem(X6) | (nil = tl(cons(X6, nil)))), inference(resolution, [], [f458, f447])).
fof(f458, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax25)).
fof(f1310304, plain, (~ spl64_4 | ~ spl64_186 | ~ spl64_518 | spl64_1665 | ~ spl64_4031 | ~ spl64_6730 | ~ spl64_26086 | ~ spl64_29836), inference(avatar_contradiction_clause, [], [f1310303])).
fof(f1310303, plain, ($false | (~ spl64_4 | ~ spl64_186 | ~ spl64_518 | spl64_1665 | ~ spl64_4031 | ~ spl64_6730 | ~ spl64_26086 | ~ spl64_29836)), inference(subsumption_resolution, [], [f1310302, f1226434])).
fof(f1226434, plain, (totalorderedP(cons(sK57, sK53)) | (~ spl64_518 | ~ spl64_4031)), inference(forward_demodulation, [], [f10222, f72067])).
fof(f72067, plain, ((sK53 = sK60) | ~ spl64_4031), inference(avatar_component_clause, [], [f72066])).
fof(f72066, plain, (spl64_4031 <=> (sK53 = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl64_4031])])).
fof(f10222, plain, (totalorderedP(cons(sK57, sK60)) | ~ spl64_518), inference(avatar_component_clause, [], [f10221])).
fof(f10221, plain, (spl64_518 <=> totalorderedP(cons(sK57, sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_518])])).
fof(f1310302, plain, (~ totalorderedP(cons(sK57, sK53)) | (~ spl64_4 | ~ spl64_186 | spl64_1665 | ~ spl64_4031 | ~ spl64_6730 | ~ spl64_26086 | ~ spl64_29836)), inference(forward_demodulation, [], [f1310301, f1036770])).
fof(f1310301, plain, (~ totalorderedP(app(sK53, sK53)) | (~ spl64_4 | ~ spl64_186 | spl64_1665 | ~ spl64_4031 | ~ spl64_6730 | ~ spl64_26086 | ~ spl64_29836)), inference(forward_demodulation, [], [f1310300, f1036765])).
fof(f1036765, plain, ((sK53 = app(sK53, nil)) | (~ spl64_4 | ~ spl64_186 | ~ spl64_26086)), inference(forward_demodulation, [], [f1036731, f65286])).
fof(f1036731, plain, ((cons(sK57, nil) = app(cons(sK57, nil), nil)) | (~ spl64_186 | ~ spl64_26086)), inference(backward_demodulation, [], [f198893, f768738])).
fof(f198893, plain, ((cons(sK50(sK53), nil) = app(cons(sK50(sK53), nil), nil)) | ~ spl64_186), inference(resolution, [], [f3667, f1576])).
fof(f1576, plain, ! [X6] : (~ ssItem(X6) | (cons(X6, nil) = app(cons(X6, nil), nil))), inference(resolution, [], [f535, f447])).
fof(f1310300, plain, (~ totalorderedP(app(app(sK53, nil), sK53)) | (~ spl64_4 | spl64_1665 | ~ spl64_4031 | ~ spl64_6730 | ~ spl64_29836)), inference(forward_demodulation, [], [f1212584, f1302276])).
fof(f1302276, plain, ((sK53 = app(sK53, cons(sK58, nil))) | (~ spl64_4031 | ~ spl64_29836)), inference(forward_demodulation, [], [f1192890, f72067])).
fof(f1192890, plain, ((sK53 = app(sK60, cons(sK58, nil))) | ~ spl64_29836), inference(avatar_component_clause, [], [f1192889])).
fof(f1192889, plain, (spl64_29836 <=> (sK53 = app(sK60, cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_29836])])).
fof(f1212584, plain, (~ totalorderedP(app(app(app(sK53, cons(sK58, nil)), nil), sK53)) | (~ spl64_4 | spl64_1665 | ~ spl64_4031 | ~ spl64_6730)), inference(backward_demodulation, [], [f1194003, f72067])).
fof(f1194003, plain, (~ totalorderedP(app(app(app(sK60, cons(sK58, nil)), nil), sK53)) | (~ spl64_4 | spl64_1665 | ~ spl64_6730)), inference(forward_demodulation, [], [f1194002, f65286])).
fof(f1194002, plain, (~ totalorderedP(app(app(app(sK60, cons(sK58, nil)), nil), cons(sK57, nil))) | (spl64_1665 | ~ spl64_6730)), inference(forward_demodulation, [], [f34147, f138882])).
fof(f34147, plain, (~ totalorderedP(app(app(app(sK60, cons(sK58, nil)), nil), cons(sK59, nil))) | spl64_1665), inference(avatar_component_clause, [], [f34146])).
fof(f34146, plain, (spl64_1665 <=> totalorderedP(app(app(app(sK60, cons(sK58, nil)), nil), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_1665])])).
fof(f1302100, plain, (~ spl64_5 | ~ spl64_3584 | spl64_4111 | ~ spl64_18853 | ~ spl64_19755 | ~ spl64_26802 | ~ spl64_28563), inference(avatar_contradiction_clause, [], [f1302099])).
fof(f1302099, plain, ($false | (~ spl64_5 | ~ spl64_3584 | spl64_4111 | ~ spl64_18853 | ~ spl64_19755 | ~ spl64_26802 | ~ spl64_28563)), inference(subsumption_resolution, [], [f1302098, f65066])).
fof(f65066, plain, (sP4(sK53) | ~ spl64_3584), inference(avatar_component_clause, [], [f65065])).
fof(f65065, plain, (spl64_3584 <=> sP4(sK53)), introduced(avatar_definition, [new_symbols(naming, [spl64_3584])])).
fof(f1302098, plain, (~ sP4(sK53) | (~ spl64_5 | spl64_4111 | ~ spl64_18853 | ~ spl64_19755 | ~ spl64_26802 | ~ spl64_28563)), inference(forward_demodulation, [], [f1254572, f402751])).
fof(f402751, plain, ((sK53 = app(cons(sK57, sK61), sK53)) | ~ spl64_18853), inference(avatar_component_clause, [], [f402749])).
fof(f402749, plain, (spl64_18853 <=> (sK53 = app(cons(sK57, sK61), sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_18853])])).
fof(f1254572, plain, (~ sP4(app(cons(sK57, sK61), sK53)) | (~ spl64_5 | spl64_4111 | ~ spl64_19755 | ~ spl64_26802 | ~ spl64_28563)), inference(subsumption_resolution, [], [f1254571, f482896])).
fof(f482896, plain, (ssList(sK61) | ~ spl64_19755), inference(avatar_component_clause, [], [f482895])).
fof(f482895, plain, (spl64_19755 <=> ssList(sK61)), introduced(avatar_definition, [new_symbols(naming, [spl64_19755])])).
fof(f1254571, plain, (~ sP4(app(cons(sK57, sK61), sK53)) | ~ ssList(sK61) | (~ spl64_5 | spl64_4111 | ~ spl64_26802 | ~ spl64_28563)), inference(subsumption_resolution, [], [f1254570, f447])).
fof(f1254570, plain, (~ sP4(app(cons(sK57, sK61), sK53)) | ~ ssList(nil) | ~ ssList(sK61) | (~ spl64_5 | spl64_4111 | ~ spl64_26802 | ~ spl64_28563)), inference(subsumption_resolution, [], [f1254569, f633])).
fof(f1254569, plain, (~ sP4(app(cons(sK57, sK61), sK53)) | ~ ssItem(sK57) | ~ ssList(nil) | ~ ssList(sK61) | (spl64_4111 | ~ spl64_26802 | ~ spl64_28563)), inference(subsumption_resolution, [], [f1254543, f73038])).
fof(f1254543, plain, (~ sP4(app(cons(sK57, sK61), sK53)) | lt(sK57, sK57) | ~ ssItem(sK57) | ~ ssList(nil) | ~ ssList(sK61) | (~ spl64_26802 | ~ spl64_28563)), inference(duplicate_literal_removal, [], [f1254499])).
fof(f1254499, plain, (~ sP4(app(cons(sK57, sK61), sK53)) | lt(sK57, sK57) | ~ ssItem(sK57) | ~ ssList(nil) | ~ ssList(sK61) | lt(sK57, sK57) | (~ spl64_26802 | ~ spl64_28563)), inference(superposition, [], [f1023764, f827216])).
fof(f827216, plain, ((cons(sK57, sK61) = app(nil, cons(sK57, sK61))) | ~ spl64_26802), inference(avatar_component_clause, [], [f827214])).
fof(f827214, plain, (spl64_26802 <=> (cons(sK57, sK61) = app(nil, cons(sK57, sK61)))), introduced(avatar_definition, [new_symbols(naming, [spl64_26802])])).
fof(f1023764, plain, (! [X35, X36, X34] : (~ sP4(app(app(X34, cons(X35, X36)), sK53)) | lt(sK57, X35) | ~ ssItem(X35) | ~ ssList(X34) | ~ ssList(X36) | lt(X35, sK57)) | ~ spl64_28563), inference(avatar_component_clause, [], [f1023763])).
fof(f1023763, plain, (spl64_28563 <=> ! [X34, X36, X35] : (~ sP4(app(app(X34, cons(X35, X36)), sK53)) | lt(sK57, X35) | ~ ssItem(X35) | ~ ssList(X34) | ~ ssList(X36) | lt(X35, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl64_28563])])).
fof(f1275400, plain, (~ spl64_84 | ~ spl64_1493 | ~ spl64_4031 | ~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158 | spl64_29836), inference(avatar_contradiction_clause, [], [f1275399])).
fof(f1275399, plain, ($false | (~ spl64_84 | ~ spl64_1493 | ~ spl64_4031 | ~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158 | spl64_29836)), inference(subsumption_resolution, [], [f1275398, f1267019])).
fof(f1267019, plain, ((sK53 = app(sK53, cons(sK58, nil))) | (~ spl64_84 | ~ spl64_1493 | ~ spl64_4031 | ~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1267018, f28550])).
fof(f28550, plain, (ssList(cons(sK58, nil)) | ~ spl64_1493), inference(avatar_component_clause, [], [f28549])).
fof(f28549, plain, (spl64_1493 <=> ssList(cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_1493])])).
fof(f1267018, plain, ((sK53 = app(sK53, cons(sK58, nil))) | ~ ssList(cons(sK58, nil)) | (~ spl64_84 | ~ spl64_4031 | ~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1267007, f667])).
fof(f1267007, plain, (~ ssList(sK53) | (sK53 = app(sK53, cons(sK58, nil))) | ~ ssList(cons(sK58, nil)) | (~ spl64_84 | ~ spl64_4031 | ~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158)), inference(resolution, [], [f1212369, f1656])).
fof(f1656, plain, ! [X0, X1] : (~ frontsegP(X1, app(X1, X0)) | ~ ssList(X1) | (app(X1, X0) = X1) | ~ ssList(X0)), inference(subsumption_resolution, [], [f1655, f459])).
fof(f459, plain, ! [X0, X1] : (ssList(app(X0, X1)) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : (ssList(app(X0, X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ssList(app(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax26)).
fof(f1655, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X1, X0)) | (app(X1, X0) = X1) | ~ frontsegP(X1, app(X1, X0))), inference(duplicate_literal_removal, [], [f1652])).
fof(f1652, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X1, X0)) | (app(X1, X0) = X1) | ~ frontsegP(X1, app(X1, X0)) | ~ ssList(app(X1, X0)) | ~ ssList(X1)), inference(resolution, [], [f581, f480])).
fof(f480, plain, ! [X0, X1] : (~ frontsegP(X1, X0) | (X0 = X1) | ~ frontsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f153])).
fof(f153, plain, ! [X0] : (! [X1] : ((X0 = X1) | ~ frontsegP(X1, X0) | ~ frontsegP(X0, X1) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f152])).
fof(f152, plain, ! [X0] : (! [X1] : (((X0 = X1) | (~ frontsegP(X1, X0) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((frontsegP(X1, X0) & frontsegP(X0, X1)) => (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax41)).
fof(f581, plain, ! [X2, X1] : (frontsegP(app(X1, X2), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X1, X2))), inference(equality_resolution, [], [f369])).
fof(f369, plain, ! [X2, X0, X1] : (frontsegP(X0, X1) | ~ (app(X1, X2) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f248])).
fof(f248, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (((app(X1, sK11(X0, X1)) = X0) & ssList(sK11(X0, X1))) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f246, f247])).
fof(f247, plain, ! [X1, X0] : (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) => ((app(X1, sK11(X0, X1)) = X0) & ssList(sK11(X0, X1)))), introduced(choice_axiom, [])).
fof(f246, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f245])).
fof(f245, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X1, X2) = X0) & ssList(X2)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (! [X1] : ((frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax5)).
fof(f1212369, plain, (frontsegP(sK53, app(sK53, cons(sK58, nil))) | (~ spl64_84 | ~ spl64_4031 | ~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158)), inference(backward_demodulation, [], [f1182775, f72067])).
fof(f1182775, plain, (frontsegP(sK53, app(sK60, cons(sK58, nil))) | (~ spl64_84 | ~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1182774, f2422])).
fof(f2422, plain, (ssList(cons(sK59, nil)) | ~ spl64_84), inference(avatar_component_clause, [], [f2421])).
fof(f2421, plain, (spl64_84 <=> ssList(cons(sK59, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_84])])).
fof(f1182774, plain, (frontsegP(sK53, app(sK60, cons(sK58, nil))) | ~ ssList(cons(sK59, nil)) | (~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1182773, f482896])).
fof(f1182773, plain, (frontsegP(sK53, app(sK60, cons(sK58, nil))) | ~ ssList(sK61) | ~ ssList(cons(sK59, nil)) | (~ spl64_4975 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1182681, f1002091])).
fof(f1002091, plain, (ssList(app(sK60, cons(sK58, nil))) | ~ spl64_28158), inference(avatar_component_clause, [], [f1002090])).
fof(f1002090, plain, (spl64_28158 <=> ssList(app(sK60, cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_28158])])).
fof(f1182681, plain, (frontsegP(sK53, app(sK60, cons(sK58, nil))) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssList(sK61) | ~ ssList(cons(sK59, nil)) | ~ spl64_4975), inference(superposition, [], [f15960, f85810])).
fof(f85810, plain, ((sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl64_4975), inference(avatar_component_clause, [], [f85808])).
fof(f85808, plain, (spl64_4975 <=> (sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_4975])])).
fof(f15960, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(X3) | ~ ssList(X4)), inference(subsumption_resolution, [], [f15959, f459])).
fof(f15959, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3))), inference(subsumption_resolution, [], [f15957, f459])).
fof(f15957, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(app(app(X2, X3), X4)) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3))), inference(duplicate_literal_removal, [], [f15949])).
fof(f15949, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(app(app(X2, X3), X4)) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3)) | ~ ssList(app(app(X2, X3), X4))), inference(resolution, [], [f1902, f581])).
fof(f1902, plain, ! [X6, X4, X5] : (~ frontsegP(X4, app(X5, X6)) | frontsegP(X4, X5) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X6)), inference(subsumption_resolution, [], [f1899, f459])).
fof(f1899, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6)), inference(duplicate_literal_removal, [], [f1896])).
fof(f1896, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6) | ~ ssList(X5) | ~ ssList(app(X5, X6))), inference(resolution, [], [f479, f581])).
fof(f479, plain, ! [X2, X0, X1] : (~ frontsegP(X1, X2) | frontsegP(X0, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(X0, X2) | ~ frontsegP(X1, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f150])).
fof(f150, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(X0, X2) | (~ frontsegP(X1, X2) | ~ frontsegP(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((frontsegP(X1, X2) & frontsegP(X0, X1)) => frontsegP(X0, X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax40)).
fof(f1275398, plain, (~ (sK53 = app(sK53, cons(sK58, nil))) | (~ spl64_4031 | spl64_29836)), inference(forward_demodulation, [], [f1192891, f72067])).
fof(f1192891, plain, (~ (sK53 = app(sK60, cons(sK58, nil))) | spl64_29836), inference(avatar_component_clause, [], [f1192889])).
fof(f1204586, plain, (spl64_7229 | ~ spl64_54 | ~ spl64_64 | ~ spl64_241 | ~ spl64_242 | ~ spl64_4035 | ~ spl64_8418 | ~ spl64_8419 | ~ spl64_28570), inference(avatar_split_clause, [], [f1204585, f1023798, f186395, f186391, f72177, f4901, f4897, f1925, f1855, f148611])).
fof(f148611, plain, (spl64_7229 <=> (sK57 = sK50(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_7229])])).
fof(f1855, plain, (spl64_54 <=> ssItem(sK50(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_54])])).
fof(f1925, plain, (spl64_64 <=> (sK60 = cons(sK50(sK60), sK49(sK60)))), introduced(avatar_definition, [new_symbols(naming, [spl64_64])])).
fof(f186391, plain, (spl64_8418 <=> ! [X29] : ((hd(cons(X29, sK49(sK60))) = X29) | ~ ssItem(X29))), introduced(avatar_definition, [new_symbols(naming, [spl64_8418])])).
fof(f186395, plain, (spl64_8419 <=> ! [X47] : ((sK49(sK60) = tl(cons(X47, sK49(sK60)))) | ~ ssItem(X47))), introduced(avatar_definition, [new_symbols(naming, [spl64_8419])])).
fof(f1204585, plain, ((sK57 = sK50(sK60)) | (~ spl64_54 | ~ spl64_64 | ~ spl64_241 | ~ spl64_242 | ~ spl64_4035 | ~ spl64_8418 | ~ spl64_8419 | ~ spl64_28570)), inference(subsumption_resolution, [], [f1204584, f4898])).
fof(f1204584, plain, (~ ssList(tl(sK60)) | (sK57 = sK50(sK60)) | (~ spl64_54 | ~ spl64_64 | ~ spl64_242 | ~ spl64_4035 | ~ spl64_8418 | ~ spl64_8419 | ~ spl64_28570)), inference(forward_demodulation, [], [f1204583, f1078282])).
fof(f1078282, plain, ((tl(sK60) = sK49(sK60)) | (~ spl64_54 | ~ spl64_64 | ~ spl64_242 | ~ spl64_8418 | ~ spl64_8419)), inference(forward_demodulation, [], [f1078265, f1074367])).
fof(f1074367, plain, ((sK60 = cons(hd(sK60), sK49(sK60))) | (~ spl64_54 | ~ spl64_64 | ~ spl64_8418)), inference(backward_demodulation, [], [f1927, f1074364])).
fof(f1074364, plain, ((hd(sK60) = sK50(sK60)) | (~ spl64_54 | ~ spl64_64 | ~ spl64_8418)), inference(forward_demodulation, [], [f1074360, f1927])).
fof(f1074360, plain, ((sK50(sK60) = hd(cons(sK50(sK60), sK49(sK60)))) | (~ spl64_54 | ~ spl64_8418)), inference(resolution, [], [f186392, f1856])).
fof(f1856, plain, (ssItem(sK50(sK60)) | ~ spl64_54), inference(avatar_component_clause, [], [f1855])).
fof(f186392, plain, (! [X29] : (~ ssItem(X29) | (hd(cons(X29, sK49(sK60))) = X29)) | ~ spl64_8418), inference(avatar_component_clause, [], [f186391])).
fof(f1927, plain, ((sK60 = cons(sK50(sK60), sK49(sK60))) | ~ spl64_64), inference(avatar_component_clause, [], [f1925])).
fof(f1078265, plain, ((sK49(sK60) = tl(cons(hd(sK60), sK49(sK60)))) | (~ spl64_242 | ~ spl64_8419)), inference(resolution, [], [f186396, f4902])).
fof(f186396, plain, (! [X47] : (~ ssItem(X47) | (sK49(sK60) = tl(cons(X47, sK49(sK60))))) | ~ spl64_8419), inference(avatar_component_clause, [], [f186395])).
fof(f1204583, plain, (~ ssList(sK49(sK60)) | (sK57 = sK50(sK60)) | (~ spl64_54 | ~ spl64_64 | ~ spl64_242 | ~ spl64_4035 | ~ spl64_8418 | ~ spl64_28570)), inference(subsumption_resolution, [], [f1204582, f4902])).
fof(f1204582, plain, (~ ssItem(hd(sK60)) | ~ ssList(sK49(sK60)) | (sK57 = sK50(sK60)) | (~ spl64_54 | ~ spl64_64 | ~ spl64_4035 | ~ spl64_8418 | ~ spl64_28570)), inference(forward_demodulation, [], [f1204581, f1074364])).
fof(f1204581, plain, (~ ssItem(sK50(sK60)) | ~ ssList(sK49(sK60)) | (sK57 = sK50(sK60)) | (~ spl64_64 | ~ spl64_4035 | ~ spl64_28570)), inference(subsumption_resolution, [], [f1061710, f72178])).
fof(f72178, plain, (frontsegP(sK53, sK60) | ~ spl64_4035), inference(avatar_component_clause, [], [f72177])).
fof(f1061710, plain, (~ frontsegP(sK53, sK60) | ~ ssItem(sK50(sK60)) | ~ ssList(sK49(sK60)) | (sK57 = sK50(sK60)) | (~ spl64_64 | ~ spl64_28570)), inference(superposition, [], [f1023799, f1927])).
fof(f1203879, plain, (spl64_4035 | ~ spl64_84 | ~ spl64_1493 | ~ spl64_4975 | ~ spl64_8415 | ~ spl64_19755 | ~ spl64_28158), inference(avatar_split_clause, [], [f1203878, f1002090, f482895, f186182, f85808, f28549, f2421, f72177])).
fof(f186182, plain, (spl64_8415 <=> ssList(sK60)), introduced(avatar_definition, [new_symbols(naming, [spl64_8415])])).
fof(f1203878, plain, (frontsegP(sK53, sK60) | (~ spl64_84 | ~ spl64_1493 | ~ spl64_4975 | ~ spl64_8415 | ~ spl64_19755 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1203877, f28550])).
fof(f1203877, plain, (frontsegP(sK53, sK60) | ~ ssList(cons(sK58, nil)) | (~ spl64_84 | ~ spl64_4975 | ~ spl64_8415 | ~ spl64_19755 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1203876, f667])).
fof(f1203876, plain, (frontsegP(sK53, sK60) | ~ ssList(sK53) | ~ ssList(cons(sK58, nil)) | (~ spl64_84 | ~ spl64_4975 | ~ spl64_8415 | ~ spl64_19755 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1203868, f186183])).
fof(f186183, plain, (ssList(sK60) | ~ spl64_8415), inference(avatar_component_clause, [], [f186182])).
fof(f1203868, plain, (frontsegP(sK53, sK60) | ~ ssList(sK60) | ~ ssList(sK53) | ~ ssList(cons(sK58, nil)) | (~ spl64_84 | ~ spl64_4975 | ~ spl64_19755 | ~ spl64_28158)), inference(resolution, [], [f1182775, f1902])).
fof(f1193390, plain, (~ spl64_4 | ~ spl64_224 | ~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730 | ~ spl64_8415 | ~ spl64_16616 | spl64_16618 | ~ spl64_27396), inference(avatar_contradiction_clause, [], [f1193389])).
fof(f1193389, plain, ($false | (~ spl64_4 | ~ spl64_224 | ~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730 | ~ spl64_8415 | ~ spl64_16616 | spl64_16618 | ~ spl64_27396)), inference(subsumption_resolution, [], [f1193388, f447])).
fof(f1193388, plain, (~ ssList(nil) | (~ spl64_4 | ~ spl64_224 | ~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730 | ~ spl64_8415 | ~ spl64_16616 | spl64_16618 | ~ spl64_27396)), inference(subsumption_resolution, [], [f1193387, f186183])).
fof(f1193387, plain, (~ ssList(sK60) | ~ ssList(nil) | (~ spl64_4 | ~ spl64_224 | ~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730 | ~ spl64_16616 | spl64_16618 | ~ spl64_27396)), inference(subsumption_resolution, [], [f1193386, f323825])).
fof(f323825, plain, (ssItem(sK58) | ~ spl64_16616), inference(avatar_component_clause, [], [f323824])).
fof(f323824, plain, (spl64_16616 <=> ssItem(sK58)), introduced(avatar_definition, [new_symbols(naming, [spl64_16616])])).
fof(f1193386, plain, (~ ssItem(sK58) | ~ ssList(sK60) | ~ ssList(nil) | (~ spl64_4 | ~ spl64_224 | ~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730 | spl64_16618 | ~ spl64_27396)), inference(subsumption_resolution, [], [f1193385, f325241])).
fof(f325241, plain, (~ leq(sK58, sK57) | spl64_16618), inference(avatar_component_clause, [], [f325239])).
fof(f325239, plain, (spl64_16618 <=> leq(sK58, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl64_16618])])).
fof(f1193385, plain, (leq(sK58, sK57) | ~ ssItem(sK58) | ~ ssList(sK60) | ~ ssList(nil) | (~ spl64_4 | ~ spl64_224 | ~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730 | ~ spl64_27396)), inference(subsumption_resolution, [], [f1193384, f1186310])).
fof(f1186310, plain, (ssList(app(app(sK60, cons(sK58, nil)), sK53)) | (~ spl64_4 | ~ spl64_224 | ~ spl64_1493 | ~ spl64_6730)), inference(forward_demodulation, [], [f1183971, f65286])).
fof(f1183971, plain, (ssList(app(app(sK60, cons(sK58, nil)), cons(sK57, nil))) | (~ spl64_224 | ~ spl64_1493 | ~ spl64_6730)), inference(backward_demodulation, [], [f1016294, f138882])).
fof(f1016294, plain, (ssList(app(app(sK60, cons(sK58, nil)), cons(sK59, nil))) | (~ spl64_224 | ~ spl64_1493)), inference(backward_demodulation, [], [f4068, f1016246])).
fof(f1016246, plain, ((app(sK60, cons(sK58, nil)) = app(app(sK60, cons(sK58, nil)), nil)) | ~ spl64_1493), inference(resolution, [], [f28550, f11854])).
fof(f11854, plain, ! [X78] : (~ ssList(X78) | (app(sK60, X78) = app(app(sK60, X78), nil))), inference(resolution, [], [f972, f562])).
fof(f562, plain, ssList(sK60), inference(cnf_transformation, [], [f354])).
fof(f972, plain, ! [X0, X1] : (~ ssList(X1) | ~ ssList(X0) | (app(X1, X0) = app(app(X1, X0), nil))), inference(resolution, [], [f459, f540])).
fof(f540, plain, ! [X0] : (~ ssList(X0) | (app(X0, nil) = X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : ((app(X0, nil) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (ssList(X0) => (app(X0, nil) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax84)).
fof(f4068, plain, (ssList(app(app(app(sK60, cons(sK58, nil)), nil), cons(sK59, nil))) | ~ spl64_224), inference(avatar_component_clause, [], [f4067])).
fof(f4067, plain, (spl64_224 <=> ssList(app(app(app(sK60, cons(sK58, nil)), nil), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_224])])).
fof(f1193384, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK53)) | leq(sK58, sK57) | ~ ssItem(sK58) | ~ ssList(sK60) | ~ ssList(nil) | (~ spl64_4 | ~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730 | ~ spl64_27396)), inference(resolution, [], [f1186345, f904655])).
fof(f904655, plain, (! [X41, X42, X40] : (~ totalorderedP(app(app(X40, cons(X41, X42)), sK53)) | ~ ssList(app(app(X40, cons(X41, X42)), sK53)) | leq(X41, sK57) | ~ ssItem(X41) | ~ ssList(X40) | ~ ssList(X42)) | ~ spl64_27396), inference(avatar_component_clause, [], [f904654])).
fof(f904654, plain, (spl64_27396 <=> ! [X40, X42, X41] : (~ totalorderedP(app(app(X40, cons(X41, X42)), sK53)) | ~ ssList(app(app(X40, cons(X41, X42)), sK53)) | leq(X41, sK57) | ~ ssItem(X41) | ~ ssList(X40) | ~ ssList(X42))), introduced(avatar_definition, [new_symbols(naming, [spl64_27396])])).
fof(f1186345, plain, (totalorderedP(app(app(sK60, cons(sK58, nil)), sK53)) | (~ spl64_4 | ~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730)), inference(forward_demodulation, [], [f1184006, f65286])).
fof(f1184006, plain, (totalorderedP(app(app(sK60, cons(sK58, nil)), cons(sK57, nil))) | (~ spl64_1493 | ~ spl64_1665 | ~ spl64_6730)), inference(backward_demodulation, [], [f1016414, f138882])).
fof(f1016414, plain, (totalorderedP(app(app(sK60, cons(sK58, nil)), cons(sK59, nil))) | (~ spl64_1493 | ~ spl64_1665)), inference(backward_demodulation, [], [f34148, f1016246])).
fof(f34148, plain, (totalorderedP(app(app(app(sK60, cons(sK58, nil)), nil), cons(sK59, nil))) | ~ spl64_1665), inference(avatar_component_clause, [], [f34146])).
fof(f1183114, plain, (~ spl64_6727 | ~ spl64_78 | ~ spl64_84 | ~ spl64_16190 | ~ spl64_16936 | ~ spl64_28567), inference(avatar_split_clause, [], [f1183113, f1023783, f340057, f299545, f2421, f2098, f138864])).
fof(f138864, plain, (spl64_6727 <=> leq(sK57, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl64_6727])])).
fof(f299545, plain, (spl64_16190 <=> sP0(cons(sK59, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_16190])])).
fof(f340057, plain, (spl64_16936 <=> leq(sK59, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl64_16936])])).
fof(f1023783, plain, (spl64_28567 <=> ! [X22, X23, X24] : (~ sP0(app(app(X22, cons(X23, X24)), sK53)) | ~ leq(sK57, X23) | ~ ssItem(X23) | ~ ssList(X22) | ~ ssList(X24) | ~ leq(X23, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl64_28567])])).
fof(f1183113, plain, (~ leq(sK57, sK59) | (~ spl64_78 | ~ spl64_84 | ~ spl64_16190 | ~ spl64_16936 | ~ spl64_28567)), inference(subsumption_resolution, [], [f1183112, f299546])).
fof(f299546, plain, (sP0(cons(sK59, sK53)) | ~ spl64_16190), inference(avatar_component_clause, [], [f299545])).
fof(f1183112, plain, (~ sP0(cons(sK59, sK53)) | ~ leq(sK57, sK59) | (~ spl64_78 | ~ spl64_84 | ~ spl64_16936 | ~ spl64_28567)), inference(forward_demodulation, [], [f1183111, f70960])).
fof(f70960, plain, ((cons(sK59, sK53) = app(cons(sK59, nil), sK53)) | ~ spl64_78), inference(resolution, [], [f2821, f2099])).
fof(f1183111, plain, (~ sP0(app(cons(sK59, nil), sK53)) | ~ leq(sK57, sK59) | (~ spl64_78 | ~ spl64_84 | ~ spl64_16936 | ~ spl64_28567)), inference(subsumption_resolution, [], [f1183110, f340059])).
fof(f340059, plain, (leq(sK59, sK57) | ~ spl64_16936), inference(avatar_component_clause, [], [f340057])).
fof(f1183110, plain, (~ sP0(app(cons(sK59, nil), sK53)) | ~ leq(sK57, sK59) | ~ leq(sK59, sK57) | (~ spl64_78 | ~ spl64_84 | ~ spl64_28567)), inference(subsumption_resolution, [], [f1183109, f447])).
fof(f1183109, plain, (~ sP0(app(cons(sK59, nil), sK53)) | ~ leq(sK57, sK59) | ~ ssList(nil) | ~ leq(sK59, sK57) | (~ spl64_78 | ~ spl64_84 | ~ spl64_28567)), inference(subsumption_resolution, [], [f1182940, f2099])).
fof(f1182940, plain, (~ sP0(app(cons(sK59, nil), sK53)) | ~ leq(sK57, sK59) | ~ ssItem(sK59) | ~ ssList(nil) | ~ leq(sK59, sK57) | (~ spl64_84 | ~ spl64_28567)), inference(duplicate_literal_removal, [], [f1182891])).
fof(f1182891, plain, (~ sP0(app(cons(sK59, nil), sK53)) | ~ leq(sK57, sK59) | ~ ssItem(sK59) | ~ ssList(nil) | ~ ssList(nil) | ~ leq(sK59, sK57) | (~ spl64_84 | ~ spl64_28567)), inference(superposition, [], [f1023784, f2450])).
fof(f2450, plain, ((cons(sK59, nil) = app(nil, cons(sK59, nil))) | ~ spl64_84), inference(resolution, [], [f2422, f461])).
fof(f461, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax28)).
fof(f1023784, plain, (! [X24, X23, X22] : (~ sP0(app(app(X22, cons(X23, X24)), sK53)) | ~ leq(sK57, X23) | ~ ssItem(X23) | ~ ssList(X22) | ~ ssList(X24) | ~ leq(X23, sK57)) | ~ spl64_28567), inference(avatar_component_clause, [], [f1023783])).
fof(f1061223, plain, (spl64_4975 | ~ spl64_41 | ~ spl64_860), inference(avatar_split_clause, [], [f1060438, f16513, f1227, f85808])).
fof(f1060438, plain, ((sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl64_41 | ~ spl64_860)), inference(forward_demodulation, [], [f1060054, f1049581])).
fof(f1049581, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), nil)) | ~ spl64_41), inference(backward_demodulation, [], [f565, f1229])).
fof(f1229, plain, ((nil = sK62) | ~ spl64_41), inference(avatar_component_clause, [], [f1227])).
fof(f1060054, plain, ((app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)) = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), nil)) | ~ spl64_860), inference(resolution, [], [f16514, f540])).
fof(f1048239, plain, (spl64_183 | spl64_41 | ~ spl64_26092), inference(avatar_split_clause, [], [f851599, f772213, f1227, f3559])).
fof(f3559, plain, (spl64_183 <=> (sK62 = cons(hd(sK62), tl(sK62)))), introduced(avatar_definition, [new_symbols(naming, [spl64_183])])).
fof(f851599, plain, ((nil = sK62) | (sK62 = cons(hd(sK62), tl(sK62))) | ~ spl64_26092), inference(resolution, [], [f772214, f532])).
fof(f1036570, plain, (~ spl64_4984 | ~ spl64_84 | spl64_860), inference(avatar_split_clause, [], [f1036569, f16513, f2421, f85861])).
fof(f1036569, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl64_84 | spl64_860)), inference(subsumption_resolution, [], [f998734, f2422])).
fof(f998734, plain, (~ ssList(cons(sK59, nil)) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | spl64_860), inference(resolution, [], [f16515, f459])).
fof(f16515, plain, (~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | spl64_860), inference(avatar_component_clause, [], [f16513])).
fof(f1024330, plain, (spl64_121 | spl64_122), inference(avatar_split_clause, [], [f66008, f2872, f2868])).
fof(f2868, plain, (spl64_121 <=> (sK53 = cons(sK50(sK53), sK49(sK53)))), introduced(avatar_definition, [new_symbols(naming, [spl64_121])])).
fof(f66008, plain, ((nil = sK53) | (sK53 = cons(sK50(sK53), sK49(sK53)))), inference(resolution, [], [f667, f453])).
fof(f1023903, plain, (~ spl64_5 | spl64_28572 | ~ spl64_4), inference(avatar_split_clause, [], [f1023902, f626, f1023809, f631])).
fof(f1023902, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | ~ ssItem(sK57) | ~ ssItem(X8)) | ~ spl64_4), inference(subsumption_resolution, [], [f66603, f477])).
fof(f477, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (ssItem(X0) => ~ memberP(nil, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax38)).
fof(f66603, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssItem(sK57) | ~ ssItem(X8)) | ~ spl64_4), inference(subsumption_resolution, [], [f66558, f447])).
fof(f66558, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssList(nil) | ~ ssItem(sK57) | ~ ssItem(X8)) | ~ spl64_4), inference(superposition, [], [f474, f65286])).
fof(f474, plain, ! [X2, X0, X1] : (~ memberP(cons(X1, X2), X0) | (X0 = X1) | memberP(X2, X0) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f325])).
fof(f325, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & (memberP(X2, X0) | (X0 = X1) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f324])).
fof(f324, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & ((memberP(X2, X0) | (X0 = X1)) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => (memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax37)).
fof(f1023900, plain, (~ spl64_5 | spl64_28570 | ~ spl64_4), inference(avatar_split_clause, [], [f66608, f626, f1023798, f631])).
fof(f66608, plain, (! [X12, X13] : (~ frontsegP(sK53, cons(X12, X13)) | (sK57 = X12) | ~ ssList(X13) | ~ ssItem(X12) | ~ ssItem(sK57)) | ~ spl64_4), inference(subsumption_resolution, [], [f66561, f447])).
fof(f66561, plain, (! [X12, X13] : (~ frontsegP(sK53, cons(X12, X13)) | (sK57 = X12) | ~ ssList(X13) | ~ ssList(nil) | ~ ssItem(X12) | ~ ssItem(sK57)) | ~ spl64_4), inference(superposition, [], [f483, f65286])).
fof(f483, plain, ! [X2, X0, X3, X1] : (~ frontsegP(cons(X0, X2), cons(X1, X3)) | (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f327])).
fof(f327, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1)) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f326])).
fof(f326, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | (~ frontsegP(X2, X3) | ~ (X0 = X1))) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f157])).
fof(f157, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f44])).
fof(f44, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax44)).
fof(f1023897, plain, (~ spl64_5 | spl64_28567 | ~ spl64_4), inference(avatar_split_clause, [], [f66616, f626, f1023783, f631])).
fof(f66616, plain, (! [X24, X23, X22] : (~ sP0(app(app(X22, cons(X23, X24)), sK53)) | ~ leq(X23, sK57) | ~ ssList(X24) | ~ ssList(X22) | ~ ssItem(sK57) | ~ ssItem(X23) | ~ leq(sK57, X23)) | ~ spl64_4), inference(subsumption_resolution, [], [f66570, f447])).
fof(f66570, plain, (! [X24, X23, X22] : (~ sP0(app(app(X22, cons(X23, X24)), sK53)) | ~ leq(X23, sK57) | ~ ssList(nil) | ~ ssList(X24) | ~ ssList(X22) | ~ ssItem(sK57) | ~ ssItem(X23) | ~ leq(sK57, X23)) | ~ spl64_4), inference(superposition, [], [f584, f65286])).
fof(f584, plain, ! [X6, X10, X8, X7, X9] : (~ sP0(app(app(X8, cons(X6, X9)), cons(X7, X10))) | ~ leq(X6, X7) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ leq(X7, X6)), inference(equality_resolution, [], [f379])).
fof(f379, plain, ! [X6, X0, X10, X8, X7, X9] : (~ leq(X7, X6) | ~ leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ sP0(X0)), inference(cnf_transformation, [], [f266])).
fof(f266, plain, ! [X0] : ((sP0(X0) | (((((leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), sK18(X0))), cons(sK16(X0), sK19(X0))) = X0) & ssList(sK19(X0))) & ssList(sK18(X0))) & ssList(sK17(X0))) & ssItem(sK16(X0))) & ssItem(sK15(X0)))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (~ leq(X7, X6) | ~ leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ sP0(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK15, sK16, sK17, sK18, sK19])], [f260, f265, f264, f263, f262, f261])).
fof(f261, plain, ! [X0] : (? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, X1) & leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1)) => (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, sK15(X0)) & leq(sK15(X0), X2) & (app(app(X3, cons(sK15(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(sK15(X0)))), introduced(choice_axiom, [])).
fof(f262, plain, ! [X0] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, sK15(X0)) & leq(sK15(X0), X2) & (app(app(X3, cons(sK15(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(X3, cons(sK15(X0), X4)), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(sK16(X0)))), introduced(choice_axiom, [])).
fof(f263, plain, ! [X0] : (? [X3] : (? [X4] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(X3, cons(sK15(X0), X4)), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) => (? [X4] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), X4)), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(sK17(X0)))), introduced(choice_axiom, [])).
fof(f264, plain, ! [X0] : (? [X4] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), X4)), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), sK18(X0))), cons(sK16(X0), X5)) = X0) & ssList(X5)) & ssList(sK18(X0)))), introduced(choice_axiom, [])).
fof(f265, plain, ! [X0] : (? [X5] : (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), sK18(X0))), cons(sK16(X0), X5)) = X0) & ssList(X5)) => (leq(sK16(X0), sK15(X0)) & leq(sK15(X0), sK16(X0)) & (app(app(sK17(X0), cons(sK15(X0), sK18(X0))), cons(sK16(X0), sK19(X0))) = X0) & ssList(sK19(X0)))), introduced(choice_axiom, [])).
fof(f260, plain, ! [X0] : ((sP0(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, X1) & leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (~ leq(X7, X6) | ~ leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ sP0(X0))), inference(rectify, [], [f259])).
fof(f259, plain, ! [X0] : ((sP0(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (leq(X2, X1) & leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1)) | ~ sP0(X0))), inference(nnf_transformation, [], [f223])).
fof(f223, plain, ! [X0] : (sP0(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))), inference(usedef, [], [e223])).
fof(e223, plain, ! [X0] : (sP0(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f1023893, plain, (~ spl64_5 | spl64_28563 | ~ spl64_4), inference(avatar_split_clause, [], [f66624, f626, f1023763, f631])).
fof(f66624, plain, (! [X35, X36, X34] : (~ sP4(app(app(X34, cons(X35, X36)), sK53)) | lt(X35, sK57) | ~ ssList(X36) | ~ ssList(X34) | ~ ssItem(sK57) | ~ ssItem(X35) | lt(sK57, X35)) | ~ spl64_4), inference(subsumption_resolution, [], [f66574, f447])).
fof(f66574, plain, (! [X35, X36, X34] : (~ sP4(app(app(X34, cons(X35, X36)), sK53)) | lt(X35, sK57) | ~ ssList(nil) | ~ ssList(X36) | ~ ssList(X34) | ~ ssItem(sK57) | ~ ssItem(X35) | lt(sK57, X35)) | ~ spl64_4), inference(superposition, [], [f586, f65286])).
fof(f586, plain, ! [X6, X10, X8, X7, X9] : (~ sP4(app(app(X8, cons(X6, X9)), cons(X7, X10))) | lt(X6, X7) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | lt(X7, X6)), inference(equality_resolution, [], [f403])).
fof(f403, plain, ! [X6, X0, X10, X8, X7, X9] : (lt(X7, X6) | lt(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ sP4(X0)), inference(cnf_transformation, [], [f284])).
fof(f284, plain, ! [X0] : ((sP4(X0) | (((((~ lt(sK26(X0), sK25(X0)) & ~ lt(sK25(X0), sK26(X0)) & (app(app(sK27(X0), cons(sK25(X0), sK28(X0))), cons(sK26(X0), sK29(X0))) = X0) & ssList(sK29(X0))) & ssList(sK28(X0))) & ssList(sK27(X0))) & ssItem(sK26(X0))) & ssItem(sK25(X0)))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (lt(X7, X6) | lt(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ sP4(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK25, sK26, sK27, sK28, sK29])], [f278, f283, f282, f281, f280, f279])).
fof(f279, plain, ! [X0] : (? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(X2, X1) & ~ lt(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1)) => (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(X2, sK25(X0)) & ~ lt(sK25(X0), X2) & (app(app(X3, cons(sK25(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(sK25(X0)))), introduced(choice_axiom, [])).
fof(f280, plain, ! [X0] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(X2, sK25(X0)) & ~ lt(sK25(X0), X2) & (app(app(X3, cons(sK25(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (? [X5] : (~ lt(sK26(X0), sK25(X0)) & ~ lt(sK25(X0), sK26(X0)) & (app(app(X3, cons(sK25(X0), X4)), cons(sK26(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(sK26(X0)))), introduced(choice_axiom, [])).
fof(f281, plain, ! [X0] : (? [X3] : (? [X4] : (? [X5] : (~ lt(sK26(X0), sK25(X0)) & ~ lt(sK25(X0), sK26(X0)) & (app(app(X3, cons(sK25(X0), X4)), cons(sK26(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) => (? [X4] : (? [X5] : (~ lt(sK26(X0), sK25(X0)) & ~ lt(sK25(X0), sK26(X0)) & (app(app(sK27(X0), cons(sK25(X0), X4)), cons(sK26(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(sK27(X0)))), introduced(choice_axiom, [])).
fof(f282, plain, ! [X0] : (? [X4] : (? [X5] : (~ lt(sK26(X0), sK25(X0)) & ~ lt(sK25(X0), sK26(X0)) & (app(app(sK27(X0), cons(sK25(X0), X4)), cons(sK26(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : (~ lt(sK26(X0), sK25(X0)) & ~ lt(sK25(X0), sK26(X0)) & (app(app(sK27(X0), cons(sK25(X0), sK28(X0))), cons(sK26(X0), X5)) = X0) & ssList(X5)) & ssList(sK28(X0)))), introduced(choice_axiom, [])).
fof(f283, plain, ! [X0] : (? [X5] : (~ lt(sK26(X0), sK25(X0)) & ~ lt(sK25(X0), sK26(X0)) & (app(app(sK27(X0), cons(sK25(X0), sK28(X0))), cons(sK26(X0), X5)) = X0) & ssList(X5)) => (~ lt(sK26(X0), sK25(X0)) & ~ lt(sK25(X0), sK26(X0)) & (app(app(sK27(X0), cons(sK25(X0), sK28(X0))), cons(sK26(X0), sK29(X0))) = X0) & ssList(sK29(X0)))), introduced(choice_axiom, [])).
fof(f278, plain, ! [X0] : ((sP4(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(X2, X1) & ~ lt(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (lt(X7, X6) | lt(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ sP4(X0))), inference(rectify, [], [f277])).
fof(f277, plain, ! [X0] : ((sP4(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(X2, X1) & ~ lt(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (lt(X2, X1) | lt(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1)) | ~ sP4(X0))), inference(nnf_transformation, [], [f229])).
fof(f229, plain, ! [X0] : (sP4(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (lt(X2, X1) | lt(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))), inference(usedef, [], [e229])).
fof(e229, plain, ! [X0] : (sP4(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (lt(X2, X1) | lt(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f1023426, plain, (spl64_4984 | ~ spl64_19755 | ~ spl64_28158), inference(avatar_contradiction_clause, [], [f1023425])).
fof(f1023425, plain, ($false | (spl64_4984 | ~ spl64_19755 | ~ spl64_28158)), inference(subsumption_resolution, [], [f1023424, f1002091])).
fof(f1023424, plain, (~ ssList(app(sK60, cons(sK58, nil))) | (spl64_4984 | ~ spl64_19755)), inference(subsumption_resolution, [], [f1023423, f482896])).
fof(f1023423, plain, (~ ssList(sK61) | ~ ssList(app(sK60, cons(sK58, nil))) | spl64_4984), inference(resolution, [], [f85863, f459])).
fof(f85863, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | spl64_4984), inference(avatar_component_clause, [], [f85861])).
fof(f1016630, plain, (spl64_28158 | ~ spl64_222 | ~ spl64_1493), inference(avatar_split_clause, [], [f1016293, f28549, f4053, f1002090])).
fof(f4053, plain, (spl64_222 <=> ssList(app(app(sK60, cons(sK58, nil)), nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_222])])).
fof(f1016293, plain, (ssList(app(sK60, cons(sK58, nil))) | (~ spl64_222 | ~ spl64_1493)), inference(backward_demodulation, [], [f4054, f1016246])).
fof(f4054, plain, (ssList(app(app(sK60, cons(sK58, nil)), nil)) | ~ spl64_222), inference(avatar_component_clause, [], [f4053])).
fof(f1007908, plain, (spl64_41 | spl64_276 | ~ spl64_26092), inference(avatar_contradiction_clause, [], [f1007907])).
fof(f1007907, plain, ($false | (spl64_41 | spl64_276 | ~ spl64_26092)), inference(subsumption_resolution, [], [f1007906, f772214])).
fof(f1007906, plain, (~ ssList(sK62) | (spl64_41 | spl64_276)), inference(subsumption_resolution, [], [f1007905, f1228])).
fof(f1007905, plain, ((nil = sK62) | ~ ssList(sK62) | spl64_276), inference(resolution, [], [f5171, f457])).
fof(f457, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax24)).
fof(f5171, plain, (~ ssList(tl(sK62)) | spl64_276), inference(avatar_component_clause, [], [f5169])).
fof(f5169, plain, (spl64_276 <=> ssList(tl(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl64_276])])).
fof(f991451, plain, (~ spl64_4984 | ~ spl64_84 | ~ spl64_122 | spl64_223 | ~ spl64_26092), inference(avatar_split_clause, [], [f991450, f772213, f4057, f2872, f2421, f85861])).
fof(f4057, plain, (spl64_223 <=> segmentP(nil, cons(sK59, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_223])])).
fof(f991450, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | (~ spl64_84 | ~ spl64_122 | spl64_223 | ~ spl64_26092)), inference(subsumption_resolution, [], [f991449, f447])).
fof(f991449, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(nil) | (~ spl64_84 | ~ spl64_122 | spl64_223 | ~ spl64_26092)), inference(subsumption_resolution, [], [f991335, f2422])).
fof(f991335, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(cons(sK59, nil)) | ~ ssList(nil) | (~ spl64_122 | spl64_223 | ~ spl64_26092)), inference(subsumption_resolution, [], [f991334, f772214])).
fof(f991334, plain, (~ ssList(sK62) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(cons(sK59, nil)) | ~ ssList(nil) | (~ spl64_122 | spl64_223)), inference(subsumption_resolution, [], [f991283, f4058])).
fof(f4058, plain, (~ segmentP(nil, cons(sK59, nil)) | spl64_223), inference(avatar_component_clause, [], [f4057])).
fof(f991283, plain, (segmentP(nil, cons(sK59, nil)) | ~ ssList(sK62) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(cons(sK59, nil)) | ~ ssList(nil) | ~ spl64_122), inference(superposition, [], [f583, f989738])).
fof(f989738, plain, ((nil = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), sK62)) | ~ spl64_122), inference(forward_demodulation, [], [f565, f2874])).
fof(f2874, plain, ((nil = sK53) | ~ spl64_122), inference(avatar_component_clause, [], [f2872])).
fof(f583, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f376])).
fof(f376, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f257])).
fof(f257, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK13(X0, X1), X1), sK14(X0, X1)) = X0) & ssList(sK14(X0, X1))) & ssList(sK13(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14])], [f254, f256, f255])).
fof(f255, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK13(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK13(X0, X1)))), introduced(choice_axiom, [])).
fof(f256, plain, ! [X1, X0] : (? [X5] : ((app(app(sK13(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK13(X0, X1), X1), sK14(X0, X1)) = X0) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f254, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f253])).
fof(f253, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax7)).
fof(f905742, plain, (~ spl64_151 | ~ spl64_150 | spl64_180 | ~ spl64_184), inference(avatar_split_clause, [], [f852505, f3564, f3540, f3411, f3415])).
fof(f3415, plain, (spl64_151 <=> ssItem(sK50(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl64_151])])).
fof(f3540, plain, (spl64_180 <=> memberP(sK62, sK50(sK62))), introduced(avatar_definition, [new_symbols(naming, [spl64_180])])).
fof(f852505, plain, (memberP(sK62, sK50(sK62)) | ~ ssList(sK49(sK62)) | ~ ssItem(sK50(sK62)) | ~ spl64_184), inference(superposition, [], [f608, f3566])).
fof(f608, plain, ! [X2, X1] : (memberP(cons(X1, X2), X1) | ~ ssList(X2) | ~ ssItem(X1)), inference(duplicate_literal_removal, [], [f593])).
fof(f593, plain, ! [X2, X1] : (memberP(cons(X1, X2), X1) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X1)), inference(equality_resolution, [], [f475])).
fof(f475, plain, ! [X2, X0, X1] : (memberP(cons(X1, X2), X0) | ~ (X0 = X1) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f325])).
fof(f905499, plain, (~ spl64_5 | spl64_27396 | ~ spl64_4), inference(avatar_split_clause, [], [f66628, f626, f904654, f631])).
fof(f66628, plain, (! [X41, X42, X40] : (~ totalorderedP(app(app(X40, cons(X41, X42)), sK53)) | ~ ssList(X42) | ~ ssList(X40) | ~ ssItem(sK57) | ~ ssItem(X41) | leq(X41, sK57) | ~ ssList(app(app(X40, cons(X41, X42)), sK53))) | ~ spl64_4), inference(subsumption_resolution, [], [f66576, f447])).
fof(f66576, plain, (! [X41, X42, X40] : (~ totalorderedP(app(app(X40, cons(X41, X42)), sK53)) | ~ ssList(nil) | ~ ssList(X42) | ~ ssList(X40) | ~ ssItem(sK57) | ~ ssItem(X41) | leq(X41, sK57) | ~ ssList(app(app(X40, cons(X41, X42)), sK53))) | ~ spl64_4), inference(superposition, [], [f587, f65286])).
fof(f587, plain, ! [X6, X10, X8, X7, X9] : (~ totalorderedP(app(app(X8, cons(X6, X9)), cons(X7, X10))) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | leq(X6, X7) | ~ ssList(app(app(X8, cons(X6, X9)), cons(X7, X10)))), inference(equality_resolution, [], [f413])).
fof(f413, plain, ! [X6, X0, X10, X8, X7, X9] : (leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ totalorderedP(X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f292])).
fof(f292, plain, ! [X0] : (((totalorderedP(X0) | (((((~ leq(sK30(X0), sK31(X0)) & (app(app(sK32(X0), cons(sK30(X0), sK33(X0))), cons(sK31(X0), sK34(X0))) = X0) & ssList(sK34(X0))) & ssList(sK33(X0))) & ssList(sK32(X0))) & ssItem(sK31(X0))) & ssItem(sK30(X0)))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ totalorderedP(X0))) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK30, sK31, sK32, sK33, sK34])], [f286, f291, f290, f289, f288, f287])).
fof(f287, plain, ! [X0] : (? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1)) => (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ leq(sK30(X0), X2) & (app(app(X3, cons(sK30(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(sK30(X0)))), introduced(choice_axiom, [])).
fof(f288, plain, ! [X0] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ leq(sK30(X0), X2) & (app(app(X3, cons(sK30(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (? [X5] : (~ leq(sK30(X0), sK31(X0)) & (app(app(X3, cons(sK30(X0), X4)), cons(sK31(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(sK31(X0)))), introduced(choice_axiom, [])).
fof(f289, plain, ! [X0] : (? [X3] : (? [X4] : (? [X5] : (~ leq(sK30(X0), sK31(X0)) & (app(app(X3, cons(sK30(X0), X4)), cons(sK31(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) => (? [X4] : (? [X5] : (~ leq(sK30(X0), sK31(X0)) & (app(app(sK32(X0), cons(sK30(X0), X4)), cons(sK31(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(sK32(X0)))), introduced(choice_axiom, [])).
fof(f290, plain, ! [X0] : (? [X4] : (? [X5] : (~ leq(sK30(X0), sK31(X0)) & (app(app(sK32(X0), cons(sK30(X0), X4)), cons(sK31(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : (~ leq(sK30(X0), sK31(X0)) & (app(app(sK32(X0), cons(sK30(X0), sK33(X0))), cons(sK31(X0), X5)) = X0) & ssList(X5)) & ssList(sK33(X0)))), introduced(choice_axiom, [])).
fof(f291, plain, ! [X0] : (? [X5] : (~ leq(sK30(X0), sK31(X0)) & (app(app(sK32(X0), cons(sK30(X0), sK33(X0))), cons(sK31(X0), X5)) = X0) & ssList(X5)) => (~ leq(sK30(X0), sK31(X0)) & (app(app(sK32(X0), cons(sK30(X0), sK33(X0))), cons(sK31(X0), sK34(X0))) = X0) & ssList(sK34(X0)))), introduced(choice_axiom, [])).
fof(f286, plain, ! [X0] : (((totalorderedP(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (leq(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ totalorderedP(X0))) | ~ ssList(X0)), inference(rectify, [], [f285])).
fof(f285, plain, ! [X0] : (((totalorderedP(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ leq(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1)) | ~ totalorderedP(X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f112])).
fof(f112, plain, ! [X0] : ((totalorderedP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(flattening, [], [f111])).
fof(f111, plain, ! [X0] : ((totalorderedP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : ((leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0)) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : (ssList(X0) => (totalorderedP(X0) <=> ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssList(X3) => ! [X4] : (ssList(X4) => ! [X5] : (ssList(X5) => ((app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) => leq(X1, X2))))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax11)).
fof(f905498, plain, (~ spl64_5 | spl64_27395 | ~ spl64_4), inference(avatar_split_clause, [], [f66630, f626, f904649, f631])).
fof(f66630, plain, (! [X45, X43, X44] : (~ strictorderedP(app(app(X43, sK53), cons(X44, X45))) | ~ ssList(X45) | ~ ssList(X43) | ~ ssItem(X44) | ~ ssItem(sK57) | lt(sK57, X44) | ~ ssList(app(app(X43, sK53), cons(X44, X45)))) | ~ spl64_4), inference(subsumption_resolution, [], [f66577, f447])).
fof(f66577, plain, (! [X45, X43, X44] : (~ strictorderedP(app(app(X43, sK53), cons(X44, X45))) | ~ ssList(X45) | ~ ssList(nil) | ~ ssList(X43) | ~ ssItem(X44) | ~ ssItem(sK57) | lt(sK57, X44) | ~ ssList(app(app(X43, sK53), cons(X44, X45)))) | ~ spl64_4), inference(superposition, [], [f588, f65286])).
fof(f588, plain, ! [X6, X10, X8, X7, X9] : (~ strictorderedP(app(app(X8, cons(X6, X9)), cons(X7, X10))) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | lt(X6, X7) | ~ ssList(app(app(X8, cons(X6, X9)), cons(X7, X10)))), inference(equality_resolution, [], [f421])).
fof(f421, plain, ! [X6, X0, X10, X8, X7, X9] : (lt(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ strictorderedP(X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f300])).
fof(f300, plain, ! [X0] : (((strictorderedP(X0) | (((((~ lt(sK35(X0), sK36(X0)) & (app(app(sK37(X0), cons(sK35(X0), sK38(X0))), cons(sK36(X0), sK39(X0))) = X0) & ssList(sK39(X0))) & ssList(sK38(X0))) & ssList(sK37(X0))) & ssItem(sK36(X0))) & ssItem(sK35(X0)))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (lt(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ strictorderedP(X0))) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK35, sK36, sK37, sK38, sK39])], [f294, f299, f298, f297, f296, f295])).
fof(f295, plain, ! [X0] : (? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1)) => (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(sK35(X0), X2) & (app(app(X3, cons(sK35(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(sK35(X0)))), introduced(choice_axiom, [])).
fof(f296, plain, ! [X0] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(sK35(X0), X2) & (app(app(X3, cons(sK35(X0), X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (? [X5] : (~ lt(sK35(X0), sK36(X0)) & (app(app(X3, cons(sK35(X0), X4)), cons(sK36(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(sK36(X0)))), introduced(choice_axiom, [])).
fof(f297, plain, ! [X0] : (? [X3] : (? [X4] : (? [X5] : (~ lt(sK35(X0), sK36(X0)) & (app(app(X3, cons(sK35(X0), X4)), cons(sK36(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) => (? [X4] : (? [X5] : (~ lt(sK35(X0), sK36(X0)) & (app(app(sK37(X0), cons(sK35(X0), X4)), cons(sK36(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(sK37(X0)))), introduced(choice_axiom, [])).
fof(f298, plain, ! [X0] : (? [X4] : (? [X5] : (~ lt(sK35(X0), sK36(X0)) & (app(app(sK37(X0), cons(sK35(X0), X4)), cons(sK36(X0), X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : (~ lt(sK35(X0), sK36(X0)) & (app(app(sK37(X0), cons(sK35(X0), sK38(X0))), cons(sK36(X0), X5)) = X0) & ssList(X5)) & ssList(sK38(X0)))), introduced(choice_axiom, [])).
fof(f299, plain, ! [X0] : (? [X5] : (~ lt(sK35(X0), sK36(X0)) & (app(app(sK37(X0), cons(sK35(X0), sK38(X0))), cons(sK36(X0), X5)) = X0) & ssList(X5)) => (~ lt(sK35(X0), sK36(X0)) & (app(app(sK37(X0), cons(sK35(X0), sK38(X0))), cons(sK36(X0), sK39(X0))) = X0) & ssList(sK39(X0)))), introduced(choice_axiom, [])).
fof(f294, plain, ! [X0] : (((strictorderedP(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X6] : (! [X7] : (! [X8] : (! [X9] : (! [X10] : (lt(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10)) | ~ ssList(X9)) | ~ ssList(X8)) | ~ ssItem(X7)) | ~ ssItem(X6)) | ~ strictorderedP(X0))) | ~ ssList(X0)), inference(rectify, [], [f293])).
fof(f293, plain, ! [X0] : (((strictorderedP(X0) | ? [X1] : (? [X2] : (? [X3] : (? [X4] : (? [X5] : (~ lt(X1, X2) & (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) & ssList(X5)) & ssList(X4)) & ssList(X3)) & ssItem(X2)) & ssItem(X1))) & (! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (lt(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1)) | ~ strictorderedP(X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f114])).
fof(f114, plain, ! [X0] : ((strictorderedP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (lt(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(flattening, [], [f113])).
fof(f113, plain, ! [X0] : ((strictorderedP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : ((lt(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0)) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (ssList(X0) => (strictorderedP(X0) <=> ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssList(X3) => ! [X4] : (ssList(X4) => ! [X5] : (ssList(X5) => ((app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) => lt(X1, X2))))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax12)).
fof(f903766, plain, (spl64_64 | spl64_37 | ~ spl64_8415), inference(avatar_split_clause, [], [f852224, f186182, f1209, f1925])).
fof(f1209, plain, (spl64_37 <=> (nil = sK60)), introduced(avatar_definition, [new_symbols(naming, [spl64_37])])).
fof(f852224, plain, ((sK60 = cons(sK50(sK60), sK49(sK60))) | (spl64_37 | ~ spl64_8415)), inference(subsumption_resolution, [], [f852099, f1210])).
fof(f1210, plain, (~ (nil = sK60) | spl64_37), inference(avatar_component_clause, [], [f1209])).
fof(f852099, plain, ((nil = sK60) | (sK60 = cons(sK50(sK60), sK49(sK60))) | ~ spl64_8415), inference(resolution, [], [f186183, f453])).
fof(f903758, plain, (spl64_4030 | ~ spl64_4 | ~ spl64_5 | spl64_37 | ~ spl64_7230 | ~ spl64_8415 | ~ spl64_19516 | ~ spl64_20674 | spl64_20685 | ~ spl64_26763), inference(avatar_split_clause, [], [f903757, f826864, f534807, f534441, f458434, f186182, f148821, f1209, f631, f626, f72062])).
fof(f148821, plain, (spl64_7230 <=> ssList(app(sK60, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_7230])])).
fof(f458434, plain, (spl64_19516 <=> (hd(app(app(sK60, sK53), nil)) = hd(app(app(app(sK60, sK53), nil), sK61)))), introduced(avatar_definition, [new_symbols(naming, [spl64_19516])])).
fof(f534441, plain, (spl64_20674 <=> ssList(app(app(sK60, sK53), sK61))), introduced(avatar_definition, [new_symbols(naming, [spl64_20674])])).
fof(f534807, plain, (spl64_20685 <=> (nil = app(app(sK60, sK53), sK61))), introduced(avatar_definition, [new_symbols(naming, [spl64_20685])])).
fof(f826864, plain, (spl64_26763 <=> (sK53 = app(app(app(sK60, sK53), sK61), sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_26763])])).
fof(f903757, plain, ((sK57 = hd(sK60)) | (~ spl64_4 | ~ spl64_5 | spl64_37 | ~ spl64_7230 | ~ spl64_8415 | ~ spl64_19516 | ~ spl64_20674 | spl64_20685 | ~ spl64_26763)), inference(forward_demodulation, [], [f806431, f903521])).
fof(f903521, plain, ((sK57 = hd(app(sK60, sK53))) | (~ spl64_4 | ~ spl64_5 | ~ spl64_7230 | ~ spl64_19516 | ~ spl64_20674 | spl64_20685 | ~ spl64_26763)), inference(forward_demodulation, [], [f515654, f903422])).
fof(f903422, plain, ((sK57 = hd(app(app(sK60, sK53), sK61))) | (~ spl64_4 | ~ spl64_5 | ~ spl64_20674 | spl64_20685 | ~ spl64_26763)), inference(forward_demodulation, [], [f903421, f65287])).
fof(f65287, plain, ((sK57 = hd(sK53)) | (~ spl64_4 | ~ spl64_5)), inference(backward_demodulation, [], [f6260, f65286])).
fof(f6260, plain, ((sK57 = hd(cons(sK57, nil))) | ~ spl64_5), inference(resolution, [], [f1092, f633])).
fof(f1092, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f456, f447])).
fof(f903421, plain, ((hd(sK53) = hd(app(app(sK60, sK53), sK61))) | (~ spl64_20674 | spl64_20685 | ~ spl64_26763)), inference(forward_demodulation, [], [f875965, f826866])).
fof(f826866, plain, ((sK53 = app(app(app(sK60, sK53), sK61), sK53)) | ~ spl64_26763), inference(avatar_component_clause, [], [f826864])).
fof(f875965, plain, ((hd(app(app(app(sK60, sK53), sK61), sK53)) = hd(app(app(sK60, sK53), sK61))) | (~ spl64_20674 | spl64_20685)), inference(subsumption_resolution, [], [f875900, f534808])).
fof(f534808, plain, (~ (nil = app(app(sK60, sK53), sK61)) | spl64_20685), inference(avatar_component_clause, [], [f534807])).
fof(f875900, plain, ((hd(app(app(app(sK60, sK53), sK61), sK53)) = hd(app(app(sK60, sK53), sK61))) | (nil = app(app(sK60, sK53), sK61)) | ~ spl64_20674), inference(resolution, [], [f534442, f2824])).
fof(f534442, plain, (ssList(app(app(sK60, sK53), sK61)) | ~ spl64_20674), inference(avatar_component_clause, [], [f534441])).
fof(f515654, plain, ((hd(app(sK60, sK53)) = hd(app(app(sK60, sK53), sK61))) | (~ spl64_7230 | ~ spl64_19516)), inference(forward_demodulation, [], [f458436, f514809])).
fof(f514809, plain, ((app(sK60, sK53) = app(app(sK60, sK53), nil)) | ~ spl64_7230), inference(resolution, [], [f148822, f540])).
fof(f148822, plain, (ssList(app(sK60, sK53)) | ~ spl64_7230), inference(avatar_component_clause, [], [f148821])).
fof(f458436, plain, ((hd(app(app(sK60, sK53), nil)) = hd(app(app(app(sK60, sK53), nil), sK61))) | ~ spl64_19516), inference(avatar_component_clause, [], [f458434])).
fof(f806431, plain, ((hd(sK60) = hd(app(sK60, sK53))) | (spl64_37 | ~ spl64_8415)), inference(subsumption_resolution, [], [f806337, f1210])).
fof(f806337, plain, ((hd(sK60) = hd(app(sK60, sK53))) | (nil = sK60) | ~ spl64_8415), inference(resolution, [], [f186183, f2824])).
fof(f903546, plain, (spl64_63 | spl64_37 | ~ spl64_8415), inference(avatar_split_clause, [], [f852228, f186182, f1209, f1920])).
fof(f852228, plain, ((sK60 = cons(hd(sK60), tl(sK60))) | (spl64_37 | ~ spl64_8415)), inference(subsumption_resolution, [], [f852105, f1210])).
fof(f852105, plain, ((nil = sK60) | (sK60 = cons(hd(sK60), tl(sK60))) | ~ spl64_8415), inference(resolution, [], [f186183, f532])).
fof(f869154, plain, (~ spl64_276 | spl64_16908 | ~ spl64_16909 | ~ spl64_4 | ~ spl64_5 | ~ spl64_183 | ~ spl64_277), inference(avatar_split_clause, [], [f869153, f5173, f3559, f631, f626, f338511, f338507, f5169])).
fof(f338511, plain, (spl64_16909 <=> frontsegP(sK62, sK53)), introduced(avatar_definition, [new_symbols(naming, [spl64_16909])])).
fof(f869153, plain, (~ frontsegP(sK62, sK53) | (sK57 = hd(sK62)) | ~ ssList(tl(sK62)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_183 | ~ spl64_277)), inference(subsumption_resolution, [], [f852317, f5174])).
fof(f852317, plain, (~ frontsegP(sK62, sK53) | (sK57 = hd(sK62)) | ~ ssList(tl(sK62)) | ~ ssItem(hd(sK62)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_183)), inference(superposition, [], [f66607, f3561])).
fof(f3561, plain, ((sK62 = cons(hd(sK62), tl(sK62))) | ~ spl64_183), inference(avatar_component_clause, [], [f3559])).
fof(f66607, plain, (! [X10, X11] : (~ frontsegP(cons(X10, X11), sK53) | (sK57 = X10) | ~ ssList(X11) | ~ ssItem(X10)) | (~ spl64_4 | ~ spl64_5)), inference(subsumption_resolution, [], [f66606, f633])).
fof(f66606, plain, (! [X10, X11] : (~ frontsegP(cons(X10, X11), sK53) | (sK57 = X10) | ~ ssList(X11) | ~ ssItem(sK57) | ~ ssItem(X10)) | ~ spl64_4), inference(subsumption_resolution, [], [f66560, f447])).
fof(f66560, plain, (! [X10, X11] : (~ frontsegP(cons(X10, X11), sK53) | (sK57 = X10) | ~ ssList(nil) | ~ ssList(X11) | ~ ssItem(sK57) | ~ ssItem(X10)) | ~ spl64_4), inference(superposition, [], [f483, f65286])).
fof(f865421, plain, (spl64_16909 | ~ spl64_4 | ~ spl64_5 | ~ spl64_150 | ~ spl64_184 | ~ spl64_26876), inference(avatar_split_clause, [], [f865420, f852702, f3564, f3411, f631, f626, f338511])).
fof(f865420, plain, (frontsegP(sK62, sK53) | (~ spl64_4 | ~ spl64_5 | ~ spl64_150 | ~ spl64_184 | ~ spl64_26876)), inference(subsumption_resolution, [], [f865322, f3412])).
fof(f865322, plain, (frontsegP(sK62, sK53) | ~ ssList(sK49(sK62)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_184 | ~ spl64_26876)), inference(superposition, [], [f66638, f865203])).
fof(f865203, plain, ((sK62 = cons(sK57, sK49(sK62))) | (~ spl64_184 | ~ spl64_26876)), inference(backward_demodulation, [], [f3566, f852704])).
fof(f66638, plain, (! [X51] : (frontsegP(cons(sK57, X51), sK53) | ~ ssList(X51)) | (~ spl64_4 | ~ spl64_5)), inference(subsumption_resolution, [], [f66637, f486])).
fof(f486, plain, ! [X0] : (frontsegP(X0, nil) | ~ ssList(X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : (frontsegP(X0, nil) | ~ ssList(X0)), inference(ennf_transformation, [], [f45])).
fof(f45, plain, ! [X0] : (ssList(X0) => frontsegP(X0, nil)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax45)).
fof(f66637, plain, (! [X51] : (frontsegP(cons(sK57, X51), sK53) | ~ frontsegP(X51, nil) | ~ ssList(X51)) | (~ spl64_4 | ~ spl64_5)), inference(subsumption_resolution, [], [f66636, f633])).
fof(f66636, plain, (! [X51] : (frontsegP(cons(sK57, X51), sK53) | ~ frontsegP(X51, nil) | ~ ssList(X51) | ~ ssItem(sK57)) | ~ spl64_4), inference(subsumption_resolution, [], [f66580, f447])).
fof(f66580, plain, (! [X51] : (frontsegP(cons(sK57, X51), sK53) | ~ frontsegP(X51, nil) | ~ ssList(nil) | ~ ssList(X51) | ~ ssItem(sK57)) | ~ spl64_4), inference(superposition, [], [f607, f65286])).
fof(f607, plain, ! [X2, X3, X1] : (frontsegP(cons(X1, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1)), inference(duplicate_literal_removal, [], [f594])).
fof(f594, plain, ! [X2, X3, X1] : (frontsegP(cons(X1, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X1)), inference(equality_resolution, [], [f485])).
fof(f485, plain, ! [X2, X0, X3, X1] : (frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f327])).
fof(f865187, plain, (spl64_26876 | ~ spl64_4 | ~ spl64_5 | ~ spl64_151 | ~ spl64_180 | ~ spl64_22146), inference(avatar_split_clause, [], [f865186, f575865, f3540, f3415, f631, f626, f852702])).
fof(f865186, plain, ((sK57 = sK50(sK62)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_151 | ~ spl64_180 | ~ spl64_22146)), inference(subsumption_resolution, [], [f865180, f3416])).
fof(f3416, plain, (ssItem(sK50(sK62)) | ~ spl64_151), inference(avatar_component_clause, [], [f3415])).
fof(f865180, plain, ((sK57 = sK50(sK62)) | ~ ssItem(sK50(sK62)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_151 | ~ spl64_180 | ~ spl64_22146)), inference(resolution, [], [f857004, f66605])).
fof(f66605, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | ~ ssItem(X8)) | (~ spl64_4 | ~ spl64_5)), inference(subsumption_resolution, [], [f66604, f477])).
fof(f66604, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssItem(X8)) | (~ spl64_4 | ~ spl64_5)), inference(subsumption_resolution, [], [f66603, f633])).
fof(f857004, plain, (memberP(sK53, sK50(sK62)) | (~ spl64_151 | ~ spl64_180 | ~ spl64_22146)), inference(subsumption_resolution, [], [f856999, f3416])).
fof(f856999, plain, (~ ssItem(sK50(sK62)) | memberP(sK53, sK50(sK62)) | (~ spl64_180 | ~ spl64_22146)), inference(resolution, [], [f3542, f575866])).
fof(f575866, plain, (! [X8] : (~ memberP(sK62, X8) | ~ ssItem(X8) | memberP(sK53, X8)) | ~ spl64_22146), inference(avatar_component_clause, [], [f575865])).
fof(f3542, plain, (memberP(sK62, sK50(sK62)) | ~ spl64_180), inference(avatar_component_clause, [], [f3540])).
fof(f855442, plain, (spl64_4033 | ~ spl64_4 | ~ spl64_5 | ~ spl64_63 | ~ spl64_241 | ~ spl64_4030), inference(avatar_split_clause, [], [f855441, f72062, f4897, f1920, f631, f626, f72150])).
fof(f72150, plain, (spl64_4033 <=> frontsegP(sK60, sK53)), introduced(avatar_definition, [new_symbols(naming, [spl64_4033])])).
fof(f855441, plain, (frontsegP(sK60, sK53) | (~ spl64_4 | ~ spl64_5 | ~ spl64_63 | ~ spl64_241 | ~ spl64_4030)), inference(subsumption_resolution, [], [f850973, f4898])).
fof(f850973, plain, (frontsegP(sK60, sK53) | ~ ssList(tl(sK60)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_63 | ~ spl64_4030)), inference(superposition, [], [f66638, f847626])).
fof(f847626, plain, ((sK60 = cons(sK57, tl(sK60))) | (~ spl64_63 | ~ spl64_4030)), inference(backward_demodulation, [], [f1922, f72064])).
fof(f72064, plain, ((sK57 = hd(sK60)) | ~ spl64_4030), inference(avatar_component_clause, [], [f72062])).
fof(f855440, plain, (spl64_4033 | ~ spl64_4 | ~ spl64_5 | ~ spl64_53 | ~ spl64_64 | ~ spl64_7229), inference(avatar_split_clause, [], [f855439, f148611, f1925, f1851, f631, f626, f72150])).
fof(f1851, plain, (spl64_53 <=> ssList(sK49(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_53])])).
fof(f855439, plain, (frontsegP(sK60, sK53) | (~ spl64_4 | ~ spl64_5 | ~ spl64_53 | ~ spl64_64 | ~ spl64_7229)), inference(subsumption_resolution, [], [f851083, f1852])).
fof(f1852, plain, (ssList(sK49(sK60)) | ~ spl64_53), inference(avatar_component_clause, [], [f1851])).
fof(f851083, plain, (frontsegP(sK60, sK53) | ~ ssList(sK49(sK60)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_64 | ~ spl64_7229)), inference(superposition, [], [f66638, f847831])).
fof(f847831, plain, ((sK60 = cons(sK57, sK49(sK60))) | (~ spl64_64 | ~ spl64_7229)), inference(backward_demodulation, [], [f1927, f148613])).
fof(f148613, plain, ((sK57 = sK50(sK60)) | ~ spl64_7229), inference(avatar_component_clause, [], [f148611])).
fof(f855434, plain, (spl64_4031 | ~ spl64_4033 | ~ spl64_4035 | ~ spl64_8415), inference(avatar_contradiction_clause, [], [f855433])).
fof(f855433, plain, ($false | (spl64_4031 | ~ spl64_4033 | ~ spl64_4035 | ~ spl64_8415)), inference(subsumption_resolution, [], [f855432, f667])).
fof(f855432, plain, (~ ssList(sK53) | (spl64_4031 | ~ spl64_4033 | ~ spl64_4035 | ~ spl64_8415)), inference(subsumption_resolution, [], [f855431, f186183])).
fof(f855431, plain, (~ ssList(sK60) | ~ ssList(sK53) | (spl64_4031 | ~ spl64_4033 | ~ spl64_4035)), inference(subsumption_resolution, [], [f855430, f72178])).
fof(f855430, plain, (~ frontsegP(sK53, sK60) | ~ ssList(sK60) | ~ ssList(sK53) | (spl64_4031 | ~ spl64_4033)), inference(subsumption_resolution, [], [f855423, f72068])).
fof(f72068, plain, (~ (sK53 = sK60) | spl64_4031), inference(avatar_component_clause, [], [f72066])).
fof(f855423, plain, ((sK53 = sK60) | ~ frontsegP(sK53, sK60) | ~ ssList(sK60) | ~ ssList(sK53) | ~ spl64_4033), inference(resolution, [], [f72151, f480])).
fof(f72151, plain, (frontsegP(sK60, sK53) | ~ spl64_4033), inference(avatar_component_clause, [], [f72150])).
fof(f853903, plain, (spl64_41 | spl64_277 | ~ spl64_26092), inference(avatar_contradiction_clause, [], [f853902])).
fof(f853902, plain, ($false | (spl64_41 | spl64_277 | ~ spl64_26092)), inference(subsumption_resolution, [], [f853901, f772214])).
fof(f853901, plain, (~ ssList(sK62) | (spl64_41 | spl64_277)), inference(subsumption_resolution, [], [f853900, f1228])).
fof(f853900, plain, ((nil = sK62) | ~ ssList(sK62) | spl64_277), inference(resolution, [], [f5175, f455])).
fof(f455, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax22)).
fof(f5175, plain, (~ ssItem(hd(sK62)) | spl64_277), inference(avatar_component_clause, [], [f5173])).
fof(f844968, plain, (~ spl64_4 | ~ spl64_5 | spl64_253 | ~ spl64_4031), inference(avatar_contradiction_clause, [], [f844967])).
fof(f844967, plain, ($false | (~ spl64_4 | ~ spl64_5 | spl64_253 | ~ spl64_4031)), inference(subsumption_resolution, [], [f844966, f65288])).
fof(f65288, plain, ((nil = tl(sK53)) | (~ spl64_4 | ~ spl64_5)), inference(backward_demodulation, [], [f6287, f65286])).
fof(f6287, plain, ((nil = tl(cons(sK57, nil))) | ~ spl64_5), inference(resolution, [], [f1123, f633])).
fof(f844966, plain, (~ (nil = tl(sK53)) | (spl64_253 | ~ spl64_4031)), inference(forward_demodulation, [], [f4949, f72067])).
fof(f4949, plain, (~ (nil = tl(sK60)) | spl64_253), inference(avatar_component_clause, [], [f4948])).
fof(f4948, plain, (spl64_253 <=> (nil = tl(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_253])])).
fof(f827334, plain, (spl64_26802 | ~ spl64_2951 | ~ spl64_6522), inference(avatar_split_clause, [], [f402777, f129356, f58631, f827214])).
fof(f58631, plain, (spl64_2951 <=> ssList(cons(sK58, sK61))), introduced(avatar_definition, [new_symbols(naming, [spl64_2951])])).
fof(f129356, plain, (spl64_6522 <=> (sK57 = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl64_6522])])).
fof(f402777, plain, ((cons(sK57, sK61) = app(nil, cons(sK57, sK61))) | (~ spl64_2951 | ~ spl64_6522)), inference(forward_demodulation, [], [f58741, f129358])).
fof(f129358, plain, ((sK57 = sK58) | ~ spl64_6522), inference(avatar_component_clause, [], [f129356])).
fof(f58741, plain, ((cons(sK58, sK61) = app(nil, cons(sK58, sK61))) | ~ spl64_2951), inference(resolution, [], [f58632, f461])).
fof(f58632, plain, (ssList(cons(sK58, sK61)) | ~ spl64_2951), inference(avatar_component_clause, [], [f58631])).
fof(f827022, plain, (spl64_26763 | ~ spl64_4 | ~ spl64_4975 | ~ spl64_6522 | ~ spl64_6730), inference(avatar_split_clause, [], [f775779, f138880, f129356, f85808, f626, f826864])).
fof(f775779, plain, ((sK53 = app(app(app(sK60, sK53), sK61), sK53)) | (~ spl64_4 | ~ spl64_4975 | ~ spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f775778, f65286])).
fof(f775778, plain, ((sK53 = app(app(app(sK60, cons(sK57, nil)), sK61), sK53)) | (~ spl64_4 | ~ spl64_4975 | ~ spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f775777, f129358])).
fof(f775777, plain, ((sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), sK53)) | (~ spl64_4 | ~ spl64_4975 | ~ spl64_6730)), inference(forward_demodulation, [], [f775776, f65286])).
fof(f775776, plain, ((sK53 = app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK57, nil))) | (~ spl64_4975 | ~ spl64_6730)), inference(forward_demodulation, [], [f85810, f138882])).
fof(f825488, plain, (spl64_223 | ~ spl64_4985 | ~ spl64_8415 | ~ spl64_19755 | ~ spl64_20685), inference(avatar_contradiction_clause, [], [f825487])).
fof(f825487, plain, ($false | (spl64_223 | ~ spl64_4985 | ~ spl64_8415 | ~ spl64_19755 | ~ spl64_20685)), inference(subsumption_resolution, [], [f825486, f447])).
fof(f825486, plain, (~ ssList(nil) | (spl64_223 | ~ spl64_4985 | ~ spl64_8415 | ~ spl64_19755 | ~ spl64_20685)), inference(subsumption_resolution, [], [f825485, f667])).
fof(f825485, plain, (~ ssList(sK53) | ~ ssList(nil) | (spl64_223 | ~ spl64_4985 | ~ spl64_8415 | ~ spl64_19755 | ~ spl64_20685)), inference(subsumption_resolution, [], [f825484, f186183])).
fof(f825484, plain, (~ ssList(sK60) | ~ ssList(sK53) | ~ ssList(nil) | (spl64_223 | ~ spl64_4985 | ~ spl64_19755 | ~ spl64_20685)), inference(subsumption_resolution, [], [f825483, f482896])).
fof(f825483, plain, (~ ssList(sK61) | ~ ssList(sK60) | ~ ssList(sK53) | ~ ssList(nil) | (spl64_223 | ~ spl64_4985 | ~ spl64_20685)), inference(subsumption_resolution, [], [f825440, f668724])).
fof(f668724, plain, (~ segmentP(nil, sK53) | (spl64_223 | ~ spl64_4985)), inference(subsumption_resolution, [], [f668720, f447])).
fof(f668720, plain, (~ ssList(nil) | ~ segmentP(nil, sK53) | (spl64_223 | ~ spl64_4985)), inference(resolution, [], [f85870, f4058])).
fof(f85870, plain, (! [X5] : (segmentP(X5, cons(sK59, nil)) | ~ ssList(X5) | ~ segmentP(X5, sK53)) | ~ spl64_4985), inference(avatar_component_clause, [], [f85869])).
fof(f85869, plain, (spl64_4985 <=> ! [X5] : (~ segmentP(X5, sK53) | ~ ssList(X5) | segmentP(X5, cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_4985])])).
fof(f825440, plain, (segmentP(nil, sK53) | ~ ssList(sK61) | ~ ssList(sK60) | ~ ssList(sK53) | ~ ssList(nil) | ~ spl64_20685), inference(superposition, [], [f583, f534809])).
fof(f534809, plain, ((nil = app(app(sK60, sK53), sK61)) | ~ spl64_20685), inference(avatar_component_clause, [], [f534807])).
fof(f805393, plain, (spl64_197 | ~ spl64_4 | ~ spl64_5 | ~ spl64_121 | ~ spl64_185 | ~ spl64_186), inference(avatar_split_clause, [], [f805392, f3666, f3662, f2868, f631, f626, f3714])).
fof(f3714, plain, (spl64_197 <=> (nil = sK49(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_197])])).
fof(f3662, plain, (spl64_185 <=> ssList(sK49(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_185])])).
fof(f805392, plain, ((nil = sK49(sK53)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_121 | ~ spl64_185 | ~ spl64_186)), inference(subsumption_resolution, [], [f805391, f3663])).
fof(f3663, plain, (ssList(sK49(sK53)) | ~ spl64_185), inference(avatar_component_clause, [], [f3662])).
fof(f805391, plain, ((nil = sK49(sK53)) | ~ ssList(sK49(sK53)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_121 | ~ spl64_186)), inference(subsumption_resolution, [], [f805363, f3667])).
fof(f805363, plain, ((nil = sK49(sK53)) | ~ ssItem(sK50(sK53)) | ~ ssList(sK49(sK53)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_121)), inference(trivial_inequality_removal, [], [f805361])).
fof(f805361, plain, (~ (sK53 = sK53) | (nil = sK49(sK53)) | ~ ssItem(sK50(sK53)) | ~ ssList(sK49(sK53)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_121)), inference(superposition, [], [f66602, f2870])).
fof(f2870, plain, ((sK53 = cons(sK50(sK53), sK49(sK53))) | ~ spl64_121), inference(avatar_component_clause, [], [f2868])).
fof(f66602, plain, (! [X4, X5] : (~ (sK53 = cons(X4, X5)) | (nil = X5) | ~ ssItem(X4) | ~ ssList(X5)) | (~ spl64_4 | ~ spl64_5)), inference(subsumption_resolution, [], [f66601, f447])).
fof(f66601, plain, (! [X4, X5] : (~ (sK53 = cons(X4, X5)) | (nil = X5) | ~ ssItem(X4) | ~ ssList(X5) | ~ ssList(nil)) | (~ spl64_4 | ~ spl64_5)), inference(subsumption_resolution, [], [f66555, f633])).
fof(f66555, plain, (! [X4, X5] : (~ (sK53 = cons(X4, X5)) | (nil = X5) | ~ ssItem(X4) | ~ ssItem(sK57) | ~ ssList(X5) | ~ ssList(nil)) | ~ spl64_4), inference(superposition, [], [f450, f65286])).
fof(f450, plain, ! [X2, X0, X3, X1] : (~ (cons(X2, X0) = cons(X3, X1)) | (X0 = X1) | ~ ssItem(X3) | ~ ssItem(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((X0 = X1) & (X2 = X3)) | ~ (cons(X2, X0) = cons(X3, X1)) | ~ ssItem(X3)) | ~ ssItem(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f122])).
fof(f122, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((((X0 = X1) & (X2 = X3)) | ~ (cons(X2, X0) = cons(X3, X1))) | ~ ssItem(X3)) | ~ ssItem(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssItem(X3) => ((cons(X2, X0) = cons(X3, X1)) => ((X0 = X1) & (X2 = X3))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax19)).
fof(f775761, plain, (~ spl64_4 | ~ spl64_6522 | spl64_7003 | ~ spl64_16372), inference(avatar_contradiction_clause, [], [f775760])).
fof(f775760, plain, ($false | (~ spl64_4 | ~ spl64_6522 | spl64_7003 | ~ spl64_16372)), inference(subsumption_resolution, [], [f775759, f145225])).
fof(f145225, plain, (~ (sK53 = cons(sK57, sK53)) | spl64_7003), inference(avatar_component_clause, [], [f145224])).
fof(f145224, plain, (spl64_7003 <=> (sK53 = cons(sK57, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_7003])])).
fof(f775759, plain, ((sK53 = cons(sK57, sK53)) | (~ spl64_4 | ~ spl64_6522 | ~ spl64_16372)), inference(forward_demodulation, [], [f775758, f65286])).
fof(f775758, plain, ((cons(sK57, nil) = cons(sK57, sK53)) | (~ spl64_6522 | ~ spl64_16372)), inference(forward_demodulation, [], [f303936, f129358])).
fof(f303936, plain, ((cons(sK58, nil) = cons(sK58, sK53)) | ~ spl64_16372), inference(avatar_component_clause, [], [f303934])).
fof(f303934, plain, (spl64_16372 <=> (cons(sK58, nil) = cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_16372])])).
fof(f773713, plain, spl64_26092, inference(avatar_split_clause, [], [f564, f772213])).
fof(f564, plain, ssList(sK62), inference(cnf_transformation, [], [f354])).
fof(f768773, plain, (spl64_26086 | ~ spl64_4 | ~ spl64_5 | ~ spl64_186 | ~ spl64_3619), inference(avatar_split_clause, [], [f768772, f65282, f3666, f631, f626, f768736])).
fof(f65282, plain, (spl64_3619 <=> (cons(sK57, nil) = cons(sK50(cons(sK57, nil)), nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_3619])])).
fof(f768772, plain, ((sK57 = sK50(sK53)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_186 | ~ spl64_3619)), inference(forward_demodulation, [], [f768771, f65287])).
fof(f768771, plain, ((sK50(sK53) = hd(sK53)) | (~ spl64_4 | ~ spl64_186 | ~ spl64_3619)), inference(forward_demodulation, [], [f198871, f705040])).
fof(f705040, plain, ((sK53 = cons(sK50(sK53), nil)) | (~ spl64_4 | ~ spl64_3619)), inference(forward_demodulation, [], [f65284, f65286])).
fof(f65284, plain, ((cons(sK57, nil) = cons(sK50(cons(sK57, nil)), nil)) | ~ spl64_3619), inference(avatar_component_clause, [], [f65282])).
fof(f198871, plain, ((sK50(sK53) = hd(cons(sK50(sK53), nil))) | ~ spl64_186), inference(resolution, [], [f3667, f1092])).
fof(f766915, plain, (~ spl64_5058 | ~ spl64_6730 | spl64_16190), inference(avatar_split_clause, [], [f766897, f299545, f138880, f97467])).
fof(f97467, plain, (spl64_5058 <=> sP0(cons(sK57, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_5058])])).
fof(f766897, plain, (~ sP0(cons(sK57, sK53)) | (~ spl64_6730 | spl64_16190)), inference(backward_demodulation, [], [f299547, f138882])).
fof(f299547, plain, (~ sP0(cons(sK59, sK53)) | spl64_16190), inference(avatar_component_clause, [], [f299545])).
fof(f756285, plain, (~ spl64_4 | ~ spl64_5 | ~ spl64_84 | ~ spl64_4031 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_7230 | spl64_16373 | ~ spl64_19755 | ~ spl64_20674), inference(avatar_contradiction_clause, [], [f756284])).
fof(f756284, plain, ($false | (~ spl64_4 | ~ spl64_5 | ~ spl64_84 | ~ spl64_4031 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_7230 | spl64_16373 | ~ spl64_19755 | ~ spl64_20674)), inference(subsumption_resolution, [], [f756283, f404341])).
fof(f404341, plain, (~ frontsegP(sK53, cons(sK57, sK53)) | (~ spl64_4 | ~ spl64_6522 | spl64_16373)), inference(forward_demodulation, [], [f404340, f65286])).
fof(f404340, plain, (~ frontsegP(cons(sK57, nil), cons(sK57, sK53)) | (~ spl64_6522 | spl64_16373)), inference(forward_demodulation, [], [f303940, f129358])).
fof(f303940, plain, (~ frontsegP(cons(sK58, nil), cons(sK58, sK53)) | spl64_16373), inference(avatar_component_clause, [], [f303938])).
fof(f303938, plain, (spl64_16373 <=> frontsegP(cons(sK58, nil), cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_16373])])).
fof(f756283, plain, (frontsegP(sK53, cons(sK57, sK53)) | (~ spl64_4 | ~ spl64_5 | ~ spl64_84 | ~ spl64_4031 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_7230 | ~ spl64_19755 | ~ spl64_20674)), inference(forward_demodulation, [], [f756282, f70974])).
fof(f70974, plain, ((cons(sK57, sK53) = app(sK53, sK53)) | (~ spl64_4 | ~ spl64_5)), inference(forward_demodulation, [], [f70958, f65286])).
fof(f70958, plain, ((cons(sK57, sK53) = app(cons(sK57, nil), sK53)) | ~ spl64_5), inference(resolution, [], [f2821, f633])).
fof(f756282, plain, (frontsegP(sK53, app(sK53, sK53)) | (~ spl64_4 | ~ spl64_84 | ~ spl64_4031 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_7230 | ~ spl64_19755 | ~ spl64_20674)), inference(forward_demodulation, [], [f653241, f72067])).
fof(f653241, plain, (frontsegP(sK53, app(sK60, sK53)) | (~ spl64_4 | ~ spl64_84 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_7230 | ~ spl64_19755 | ~ spl64_20674)), inference(subsumption_resolution, [], [f653240, f482896])).
fof(f653240, plain, (frontsegP(sK53, app(sK60, sK53)) | ~ ssList(sK61) | (~ spl64_4 | ~ spl64_84 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_7230 | ~ spl64_20674)), inference(subsumption_resolution, [], [f653239, f667])).
fof(f653239, plain, (frontsegP(sK53, app(sK60, sK53)) | ~ ssList(sK53) | ~ ssList(sK61) | (~ spl64_4 | ~ spl64_84 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_7230 | ~ spl64_20674)), inference(subsumption_resolution, [], [f653217, f148822])).
fof(f653217, plain, (frontsegP(sK53, app(sK60, sK53)) | ~ ssList(app(sK60, sK53)) | ~ ssList(sK53) | ~ ssList(sK61) | (~ spl64_4 | ~ spl64_84 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_20674)), inference(resolution, [], [f574178, f1902])).
fof(f574178, plain, (frontsegP(sK53, app(app(sK60, sK53), sK61)) | (~ spl64_4 | ~ spl64_84 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_20674)), inference(subsumption_resolution, [], [f574177, f2422])).
fof(f574177, plain, (frontsegP(sK53, app(app(sK60, sK53), sK61)) | ~ ssList(cons(sK59, nil)) | (~ spl64_4 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_20674)), inference(subsumption_resolution, [], [f574176, f667])).
fof(f574176, plain, (frontsegP(sK53, app(app(sK60, sK53), sK61)) | ~ ssList(sK53) | ~ ssList(cons(sK59, nil)) | (~ spl64_4 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_20674)), inference(subsumption_resolution, [], [f574150, f534442])).
fof(f574150, plain, (frontsegP(sK53, app(app(sK60, sK53), sK61)) | ~ ssList(app(app(sK60, sK53), sK61)) | ~ ssList(sK53) | ~ ssList(cons(sK59, nil)) | (~ spl64_4 | ~ spl64_4977 | ~ spl64_6522)), inference(resolution, [], [f410059, f1902])).
fof(f410059, plain, (frontsegP(sK53, app(app(app(sK60, sK53), sK61), cons(sK59, nil))) | (~ spl64_4 | ~ spl64_4977 | ~ spl64_6522)), inference(forward_demodulation, [], [f410058, f65286])).
fof(f410058, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK57, nil)), sK61), cons(sK59, nil))) | (~ spl64_4977 | ~ spl64_6522)), inference(forward_demodulation, [], [f85821, f129358])).
fof(f85821, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl64_4977), inference(avatar_component_clause, [], [f85819])).
fof(f85819, plain, (spl64_4977 <=> frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_4977])])).
fof(f747281, plain, (~ spl64_542 | ~ spl64_4031 | spl64_5058), inference(avatar_contradiction_clause, [], [f747280])).
fof(f747280, plain, ($false | (~ spl64_542 | ~ spl64_4031 | spl64_5058)), inference(subsumption_resolution, [], [f745051, f97469])).
fof(f97469, plain, (~ sP0(cons(sK57, sK53)) | spl64_5058), inference(avatar_component_clause, [], [f97467])).
fof(f745051, plain, (sP0(cons(sK57, sK53)) | (~ spl64_542 | ~ spl64_4031)), inference(backward_demodulation, [], [f10606, f72067])).
fof(f10606, plain, (sP0(cons(sK57, sK60)) | ~ spl64_542), inference(avatar_component_clause, [], [f10605])).
fof(f10605, plain, (spl64_542 <=> sP0(cons(sK57, sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_542])])).
fof(f711380, plain, (spl64_8418 | spl64_37 | ~ spl64_8415), inference(avatar_split_clause, [], [f492656, f186182, f1209, f186391])).
fof(f492656, plain, (! [X29] : ((hd(cons(X29, sK49(sK60))) = X29) | ~ ssItem(X29)) | (spl64_37 | ~ spl64_8415)), inference(subsumption_resolution, [], [f492412, f1210])).
fof(f492412, plain, (! [X29] : ((hd(cons(X29, sK49(sK60))) = X29) | (nil = sK60) | ~ ssItem(X29)) | ~ spl64_8415), inference(resolution, [], [f186183, f1114])).
fof(f1114, plain, ! [X50, X49] : (~ ssList(X50) | (hd(cons(X49, sK49(X50))) = X49) | (nil = X50) | ~ ssItem(X49)), inference(resolution, [], [f456, f451])).
fof(f451, plain, ! [X0] : (ssList(sK49(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f711379, plain, (spl64_8419 | spl64_37 | ~ spl64_8415), inference(avatar_split_clause, [], [f492662, f186182, f1209, f186395])).
fof(f492662, plain, (! [X47] : ((sK49(sK60) = tl(cons(X47, sK49(sK60)))) | ~ ssItem(X47)) | (spl64_37 | ~ spl64_8415)), inference(subsumption_resolution, [], [f492428, f1210])).
fof(f492428, plain, (! [X47] : ((sK49(sK60) = tl(cons(X47, sK49(sK60)))) | (nil = sK60) | ~ ssItem(X47)) | ~ spl64_8415), inference(resolution, [], [f186183, f1145])).
fof(f1145, plain, ! [X50, X49] : (~ ssList(X50) | (sK49(X50) = tl(cons(X49, sK49(X50)))) | (nil = X50) | ~ ssItem(X49)), inference(resolution, [], [f458, f451])).
fof(f710877, plain, (~ spl64_197 | ~ spl64_4 | spl64_1450), inference(avatar_split_clause, [], [f710876, f26181, f626, f3714])).
fof(f26181, plain, (spl64_1450 <=> (nil = sK49(cons(sK57, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_1450])])).
fof(f710876, plain, (~ (nil = sK49(sK53)) | (~ spl64_4 | spl64_1450)), inference(forward_demodulation, [], [f26182, f65286])).
fof(f26182, plain, (~ (nil = sK49(cons(sK57, nil))) | spl64_1450), inference(avatar_component_clause, [], [f26181])).
fof(f534462, plain, (~ spl64_7230 | ~ spl64_19755 | spl64_20674), inference(avatar_contradiction_clause, [], [f534461])).
fof(f534461, plain, ($false | (~ spl64_7230 | ~ spl64_19755 | spl64_20674)), inference(subsumption_resolution, [], [f534460, f148822])).
fof(f534460, plain, (~ ssList(app(sK60, sK53)) | (~ spl64_19755 | spl64_20674)), inference(subsumption_resolution, [], [f534459, f482896])).
fof(f534459, plain, (~ ssList(sK61) | ~ ssList(app(sK60, sK53)) | spl64_20674), inference(resolution, [], [f534443, f459])).
fof(f534443, plain, (~ ssList(app(app(sK60, sK53), sK61)) | spl64_20674), inference(avatar_component_clause, [], [f534441])).
fof(f515097, plain, (~ spl64_7230 | spl64_7231 | ~ spl64_19515), inference(avatar_contradiction_clause, [], [f515096])).
fof(f515096, plain, ($false | (~ spl64_7230 | spl64_7231 | ~ spl64_19515)), inference(subsumption_resolution, [], [f515095, f148826])).
fof(f148826, plain, (~ (nil = app(sK60, sK53)) | spl64_7231), inference(avatar_component_clause, [], [f148825])).
fof(f148825, plain, (spl64_7231 <=> (nil = app(sK60, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_7231])])).
fof(f515095, plain, ((nil = app(sK60, sK53)) | (~ spl64_7230 | ~ spl64_19515)), inference(forward_demodulation, [], [f514809, f458429])).
fof(f458429, plain, ((nil = app(app(sK60, sK53), nil)) | ~ spl64_19515), inference(avatar_component_clause, [], [f458427])).
fof(f458427, plain, (spl64_19515 <=> (nil = app(app(sK60, sK53), nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_19515])])).
fof(f514165, plain, (spl64_19515 | ~ spl64_4 | ~ spl64_1006 | ~ spl64_6522), inference(avatar_split_clause, [], [f514140, f129356, f17961, f626, f458427])).
fof(f17961, plain, (spl64_1006 <=> (nil = app(app(sK60, cons(sK58, nil)), nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_1006])])).
fof(f514140, plain, ((nil = app(app(sK60, sK53), nil)) | (~ spl64_4 | ~ spl64_1006 | ~ spl64_6522)), inference(forward_demodulation, [], [f514139, f65286])).
fof(f514139, plain, ((nil = app(app(sK60, cons(sK57, nil)), nil)) | (~ spl64_1006 | ~ spl64_6522)), inference(forward_demodulation, [], [f17963, f129358])).
fof(f17963, plain, ((nil = app(app(sK60, cons(sK58, nil)), nil)) | ~ spl64_1006), inference(avatar_component_clause, [], [f17961])).
fof(f513483, plain, (spl64_7230 | ~ spl64_8415), inference(avatar_contradiction_clause, [], [f513482])).
fof(f513482, plain, ($false | (spl64_7230 | ~ spl64_8415)), inference(subsumption_resolution, [], [f513481, f186183])).
fof(f513481, plain, (~ ssList(sK60) | spl64_7230), inference(subsumption_resolution, [], [f513480, f667])).
fof(f513480, plain, (~ ssList(sK53) | ~ ssList(sK60) | spl64_7230), inference(resolution, [], [f148823, f459])).
fof(f148823, plain, (~ ssList(app(sK60, sK53)) | spl64_7230), inference(avatar_component_clause, [], [f148821])).
fof(f486944, plain, (~ spl64_19515 | ~ spl64_4 | spl64_1006 | ~ spl64_6522), inference(avatar_split_clause, [], [f486942, f129356, f17961, f626, f458427])).
fof(f486942, plain, (~ (nil = app(app(sK60, sK53), nil)) | (~ spl64_4 | spl64_1006 | ~ spl64_6522)), inference(forward_demodulation, [], [f486941, f65286])).
fof(f486941, plain, (~ (nil = app(app(sK60, cons(sK57, nil)), nil)) | (spl64_1006 | ~ spl64_6522)), inference(forward_demodulation, [], [f17962, f129358])).
fof(f17962, plain, (~ (nil = app(app(sK60, cons(sK58, nil)), nil)) | spl64_1006), inference(avatar_component_clause, [], [f17961])).
fof(f483140, plain, spl64_19755, inference(avatar_split_clause, [], [f563, f482895])).
fof(f563, plain, ssList(sK61), inference(cnf_transformation, [], [f354])).
fof(f459087, plain, (spl64_19516 | spl64_19515 | ~ spl64_4 | ~ spl64_222 | ~ spl64_6522), inference(avatar_split_clause, [], [f407914, f129356, f4053, f626, f458427, f458434])).
fof(f407914, plain, ((nil = app(app(sK60, sK53), nil)) | (hd(app(app(sK60, sK53), nil)) = hd(app(app(app(sK60, sK53), nil), sK61))) | (~ spl64_4 | ~ spl64_222 | ~ spl64_6522)), inference(forward_demodulation, [], [f407913, f65286])).
fof(f407913, plain, ((nil = app(app(sK60, cons(sK57, nil)), nil)) | (hd(app(app(sK60, sK53), nil)) = hd(app(app(app(sK60, sK53), nil), sK61))) | (~ spl64_4 | ~ spl64_222 | ~ spl64_6522)), inference(forward_demodulation, [], [f407912, f129358])).
fof(f407912, plain, ((hd(app(app(sK60, sK53), nil)) = hd(app(app(app(sK60, sK53), nil), sK61))) | (nil = app(app(sK60, cons(sK58, nil)), nil)) | (~ spl64_4 | ~ spl64_222 | ~ spl64_6522)), inference(forward_demodulation, [], [f407911, f65286])).
fof(f407911, plain, ((hd(app(app(sK60, cons(sK57, nil)), nil)) = hd(app(app(app(sK60, cons(sK57, nil)), nil), sK61))) | (nil = app(app(sK60, cons(sK58, nil)), nil)) | (~ spl64_222 | ~ spl64_6522)), inference(forward_demodulation, [], [f33557, f129358])).
fof(f33557, plain, ((hd(app(app(sK60, cons(sK58, nil)), nil)) = hd(app(app(app(sK60, cons(sK58, nil)), nil), sK61))) | (nil = app(app(sK60, cons(sK58, nil)), nil)) | ~ spl64_222), inference(resolution, [], [f4054, f1790])).
fof(f1790, plain, ! [X72] : (~ ssList(X72) | (hd(X72) = hd(app(X72, sK61))) | (nil = X72)), inference(resolution, [], [f541, f563])).
fof(f402752, plain, (~ spl64_2689 | spl64_18853 | ~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_6730 | ~ spl64_16439), inference(avatar_split_clause, [], [f402747, f306079, f138880, f129356, f85819, f28549, f1209, f626, f402749, f55814])).
fof(f55814, plain, (spl64_2689 <=> ssList(cons(sK57, sK61))), introduced(avatar_definition, [new_symbols(naming, [spl64_2689])])).
fof(f306079, plain, (spl64_16439 <=> frontsegP(cons(sK58, sK61), sK53)), introduced(avatar_definition, [new_symbols(naming, [spl64_16439])])).
fof(f402747, plain, ((sK53 = app(cons(sK57, sK61), sK53)) | ~ ssList(cons(sK57, sK61)) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_6730 | ~ spl64_16439)), inference(subsumption_resolution, [], [f402746, f399321])).
fof(f399321, plain, (frontsegP(cons(sK57, sK61), sK53) | (~ spl64_6522 | ~ spl64_16439)), inference(backward_demodulation, [], [f306080, f129358])).
fof(f306080, plain, (frontsegP(cons(sK58, sK61), sK53) | ~ spl64_16439), inference(avatar_component_clause, [], [f306079])).
fof(f402746, plain, (~ frontsegP(cons(sK57, sK61), sK53) | (sK53 = app(cons(sK57, sK61), sK53)) | ~ ssList(cons(sK57, sK61)) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f402745, f129358])).
fof(f402745, plain, ((sK53 = app(cons(sK57, sK61), sK53)) | ~ ssList(cons(sK57, sK61)) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f402744, f129358])).
fof(f402744, plain, ((sK53 = app(cons(sK58, sK61), sK53)) | ~ ssList(cons(sK57, sK61)) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f402743, f65286])).
fof(f402743, plain, ((sK53 = app(cons(sK58, sK61), cons(sK57, nil))) | ~ ssList(cons(sK57, sK61)) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f402742, f138882])).
fof(f402742, plain, (~ ssList(cons(sK57, sK61)) | (sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f396750, f129358])).
fof(f396750, plain, (~ ssList(cons(sK58, sK61)) | (sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6730)), inference(subsumption_resolution, [], [f396749, f667])).
fof(f396749, plain, (~ ssList(sK53) | ~ ssList(cons(sK58, sK61)) | (sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6730)), inference(forward_demodulation, [], [f396748, f65286])).
fof(f396748, plain, (~ ssList(cons(sK57, nil)) | ~ ssList(cons(sK58, sK61)) | (sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_37 | ~ spl64_1493 | ~ spl64_4977 | ~ spl64_6730)), inference(forward_demodulation, [], [f396747, f138882])).
fof(f396747, plain, (~ ssList(cons(sK59, nil)) | ~ ssList(cons(sK58, sK61)) | (sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_37 | ~ spl64_1493 | ~ spl64_4977)), inference(subsumption_resolution, [], [f370583, f667])).
fof(f370583, plain, (~ ssList(cons(sK59, nil)) | ~ ssList(sK53) | ~ ssList(cons(sK58, sK61)) | (sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | ~ frontsegP(cons(sK58, sK61), sK53) | (~ spl64_37 | ~ spl64_1493 | ~ spl64_4977)), inference(resolution, [], [f343859, f1734])).
fof(f1734, plain, ! [X4, X5, X3] : (~ frontsegP(X4, app(X3, X5)) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X3) | (app(X3, X5) = X4) | ~ frontsegP(X3, X4)), inference(subsumption_resolution, [], [f1731, f459])).
fof(f1731, plain, ! [X4, X5, X3] : (~ frontsegP(X3, X4) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X3) | (app(X3, X5) = X4) | ~ frontsegP(X4, app(X3, X5)) | ~ ssList(app(X3, X5))), inference(duplicate_literal_removal, [], [f1724])).
fof(f1724, plain, ! [X4, X5, X3] : (~ frontsegP(X3, X4) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X3) | (app(X3, X5) = X4) | ~ frontsegP(X4, app(X3, X5)) | ~ ssList(app(X3, X5)) | ~ ssList(X4)), inference(resolution, [], [f482, f480])).
fof(f482, plain, ! [X2, X0, X1] : (frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f156])).
fof(f156, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f155])).
fof(f155, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (frontsegP(X0, X1) => frontsegP(app(X0, X2), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax43)).
fof(f343859, plain, (frontsegP(sK53, app(cons(sK58, sK61), cons(sK59, nil))) | (~ spl64_37 | ~ spl64_1493 | ~ spl64_4977)), inference(forward_demodulation, [], [f343858, f18322])).
fof(f18322, plain, (cons(sK58, sK61) = app(cons(sK58, nil), sK61)), inference(resolution, [], [f1607, f560])).
fof(f560, plain, ssItem(sK58), inference(cnf_transformation, [], [f354])).
fof(f1607, plain, ! [X72] : (~ ssItem(X72) | (cons(X72, sK61) = app(cons(X72, nil), sK61))), inference(resolution, [], [f535, f563])).
fof(f343858, plain, (frontsegP(sK53, app(app(cons(sK58, nil), sK61), cons(sK59, nil))) | (~ spl64_37 | ~ spl64_1493 | ~ spl64_4977)), inference(forward_demodulation, [], [f343857, f28701])).
fof(f28701, plain, ((cons(sK58, nil) = app(nil, cons(sK58, nil))) | ~ spl64_1493), inference(resolution, [], [f28550, f461])).
fof(f343857, plain, (frontsegP(sK53, app(app(app(nil, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl64_37 | ~ spl64_4977)), inference(forward_demodulation, [], [f85821, f1211])).
fof(f1211, plain, ((nil = sK60) | ~ spl64_37), inference(avatar_component_clause, [], [f1209])).
fof(f386169, plain, (~ spl64_4 | ~ spl64_114 | spl64_478 | ~ spl64_1494 | ~ spl64_6730), inference(avatar_contradiction_clause, [], [f386168])).
fof(f386168, plain, ($false | (~ spl64_4 | ~ spl64_114 | spl64_478 | ~ spl64_1494 | ~ spl64_6730)), inference(subsumption_resolution, [], [f386167, f2838])).
fof(f386167, plain, (~ strictorderedP(sK53) | (~ spl64_4 | spl64_478 | ~ spl64_1494 | ~ spl64_6730)), inference(forward_demodulation, [], [f386166, f2817])).
fof(f2817, plain, (sK53 = app(nil, sK53)), inference(resolution, [], [f667, f461])).
fof(f386166, plain, (~ strictorderedP(app(nil, sK53)) | (~ spl64_4 | spl64_478 | ~ spl64_1494 | ~ spl64_6730)), inference(forward_demodulation, [], [f386165, f907])).
fof(f907, plain, (nil = app(nil, nil)), inference(resolution, [], [f461, f447])).
fof(f386165, plain, (~ strictorderedP(app(app(nil, nil), sK53)) | (~ spl64_4 | spl64_478 | ~ spl64_1494 | ~ spl64_6730)), inference(forward_demodulation, [], [f386164, f907])).
fof(f386164, plain, (~ strictorderedP(app(app(app(nil, nil), nil), sK53)) | (~ spl64_4 | spl64_478 | ~ spl64_1494 | ~ spl64_6730)), inference(forward_demodulation, [], [f386163, f28555])).
fof(f28555, plain, ((nil = cons(sK58, nil)) | ~ spl64_1494), inference(avatar_component_clause, [], [f28553])).
fof(f28553, plain, (spl64_1494 <=> (nil = cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_1494])])).
fof(f386163, plain, (~ strictorderedP(app(app(app(nil, cons(sK58, nil)), nil), sK53)) | (~ spl64_4 | spl64_478 | ~ spl64_6730)), inference(forward_demodulation, [], [f386162, f65286])).
fof(f386162, plain, (~ strictorderedP(app(app(app(nil, cons(sK58, nil)), nil), cons(sK57, nil))) | (spl64_478 | ~ spl64_6730)), inference(forward_demodulation, [], [f9913, f138882])).
fof(f9913, plain, (~ strictorderedP(app(app(app(nil, cons(sK58, nil)), nil), cons(sK59, nil))) | spl64_478), inference(avatar_component_clause, [], [f9912])).
fof(f9912, plain, (spl64_478 <=> strictorderedP(app(app(app(nil, cons(sK58, nil)), nil), cons(sK59, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl64_478])])).
fof(f384121, plain, (~ spl64_4098 | ~ spl64_6730 | spl64_16936), inference(avatar_contradiction_clause, [], [f384120])).
fof(f384120, plain, ($false | (~ spl64_4098 | ~ spl64_6730 | spl64_16936)), inference(subsumption_resolution, [], [f383471, f72812])).
fof(f72812, plain, (leq(sK57, sK57) | ~ spl64_4098), inference(avatar_component_clause, [], [f72811])).
fof(f72811, plain, (spl64_4098 <=> leq(sK57, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl64_4098])])).
fof(f383471, plain, (~ leq(sK57, sK57) | (~ spl64_6730 | spl64_16936)), inference(backward_demodulation, [], [f340058, f138882])).
fof(f340058, plain, (~ leq(sK59, sK57) | spl64_16936), inference(avatar_component_clause, [], [f340057])).
fof(f379252, plain, (spl64_1494 | ~ spl64_2952 | ~ spl64_1493), inference(avatar_split_clause, [], [f379251, f28549, f58635, f28553])).
fof(f58635, plain, (spl64_2952 <=> (nil = cons(sK58, sK61))), introduced(avatar_definition, [new_symbols(naming, [spl64_2952])])).
fof(f379251, plain, (~ (nil = cons(sK58, sK61)) | (nil = cons(sK58, nil)) | ~ spl64_1493), inference(subsumption_resolution, [], [f379250, f28550])).
fof(f379250, plain, (~ (nil = cons(sK58, sK61)) | (nil = cons(sK58, nil)) | ~ ssList(cons(sK58, nil))), inference(subsumption_resolution, [], [f365601, f563])).
fof(f365601, plain, (~ (nil = cons(sK58, sK61)) | (nil = cons(sK58, nil)) | ~ ssList(sK61) | ~ ssList(cons(sK58, nil))), inference(superposition, [], [f538, f18322])).
fof(f538, plain, ! [X0, X1] : (~ (nil = app(X0, X1)) | (nil = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f340])).
fof(f340, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | ~ (nil = X0) | ~ (nil = X1)) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f339])).
fof(f339, plain, ! [X0] : (! [X1] : ((((nil = app(X0, X1)) | (~ (nil = X0) | ~ (nil = X1))) & (((nil = X0) & (nil = X1)) | ~ (nil = app(X0, X1)))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f201])).
fof(f201, plain, ! [X0] : (! [X1] : (((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f83])).
fof(f83, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((nil = app(X0, X1)) <=> ((nil = X0) & (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax83)).
fof(f354945, plain, (~ spl64_6521 | ~ spl64_5 | ~ spl64_16616 | ~ spl64_16837), inference(avatar_split_clause, [], [f354944, f327472, f323824, f631, f129352])).
fof(f129352, plain, (spl64_6521 <=> lt(sK58, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl64_6521])])).
fof(f327472, plain, (spl64_16837 <=> lt(sK57, sK58)), introduced(avatar_definition, [new_symbols(naming, [spl64_16837])])).
fof(f354944, plain, (~ lt(sK58, sK57) | (~ spl64_5 | ~ spl64_16616 | ~ spl64_16837)), inference(subsumption_resolution, [], [f354943, f323825])).
fof(f354943, plain, (~ lt(sK58, sK57) | ~ ssItem(sK58) | (~ spl64_5 | ~ spl64_16837)), inference(subsumption_resolution, [], [f337377, f633])).
fof(f337377, plain, (~ lt(sK58, sK57) | ~ ssItem(sK57) | ~ ssItem(sK58) | ~ spl64_16837), inference(resolution, [], [f327474, f467])).
fof(f467, plain, ! [X0, X1] : (~ lt(X1, X0) | ~ lt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f143])).
fof(f143, plain, ! [X0] : (! [X1] : (~ lt(X1, X0) | ~ lt(X0, X1) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f142])).
fof(f142, plain, ! [X0] : (! [X1] : ((~ lt(X1, X0) | ~ lt(X0, X1)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f33])).
fof(f33, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (lt(X0, X1) => ~ lt(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax33)).
fof(f327474, plain, (lt(sK57, sK58) | ~ spl64_16837), inference(avatar_component_clause, [], [f327472])).
fof(f354942, plain, (spl64_6521 | ~ spl64_4 | ~ spl64_5 | spl64_122 | ~ spl64_5228 | ~ spl64_16616), inference(avatar_split_clause, [], [f354939, f323824, f104999, f2872, f631, f626, f129352])).
fof(f104999, plain, (spl64_5228 <=> strictorderedP(cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl64_5228])])).
fof(f354939, plain, (lt(sK58, sK57) | (~ spl64_4 | ~ spl64_5 | spl64_122 | ~ spl64_5228 | ~ spl64_16616)), inference(forward_demodulation, [], [f354938, f65287])).
fof(f354938, plain, (lt(sK58, hd(sK53)) | (spl64_122 | ~ spl64_5228 | ~ spl64_16616)), inference(subsumption_resolution, [], [f354937, f323825])).
fof(f354937, plain, (lt(sK58, hd(sK53)) | ~ ssItem(sK58) | (spl64_122 | ~ spl64_5228)), inference(subsumption_resolution, [], [f354936, f667])).
fof(f354936, plain, (lt(sK58, hd(sK53)) | ~ ssList(sK53) | ~ ssItem(sK58) | (spl64_122 | ~ spl64_5228)), inference(subsumption_resolution, [], [f354934, f2873])).
fof(f354934, plain, ((nil = sK53) | lt(sK58, hd(sK53)) | ~ ssList(sK53) | ~ ssItem(sK58) | ~ spl64_5228), inference(resolution, [], [f105000, f520])).
fof(f520, plain, ! [X0, X1] : (~ strictorderedP(cons(X0, X1)) | (nil = X1) | lt(X0, hd(X1)) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f334])).
fof(f334, plain, ! [X0] : (! [X1] : (((strictorderedP(cons(X0, X1)) | ((~ lt(X0, hd(X1)) | ~ strictorderedP(X1) | (nil = X1)) & ~ (nil = X1))) & ((lt(X0, hd(X1)) & strictorderedP(X1) & ~ (nil = X1)) | (nil = X1) | ~ strictorderedP(cons(X0, X1)))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f333])).
fof(f333, plain, ! [X0] : (! [X1] : (((strictorderedP(cons(X0, X1)) | ((~ lt(X0, hd(X1)) | ~ strictorderedP(X1) | (nil = X1)) & ~ (nil = X1))) & (((lt(X0, hd(X1)) & strictorderedP(X1) & ~ (nil = X1)) | (nil = X1)) | ~ strictorderedP(cons(X0, X1)))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f184])).
fof(f184, plain, ! [X0] : (! [X1] : ((strictorderedP(cons(X0, X1)) <=> ((lt(X0, hd(X1)) & strictorderedP(X1) & ~ (nil = X1)) | (nil = X1))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f70])).
fof(f70, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => (strictorderedP(cons(X0, X1)) <=> ((lt(X0, hd(X1)) & strictorderedP(X1) & ~ (nil = X1)) | (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax70)).
fof(f105000, plain, (strictorderedP(cons(sK58, sK53)) | ~ spl64_5228), inference(avatar_component_clause, [], [f104999])).
fof(f327479, plain, (~ spl64_16616 | spl64_16837 | ~ spl64_5 | spl64_6522 | ~ spl64_6730), inference(avatar_split_clause, [], [f327478, f138880, f129356, f631, f327472, f323824])).
fof(f327478, plain, (lt(sK57, sK58) | ~ ssItem(sK58) | (~ spl64_5 | spl64_6522 | ~ spl64_6730)), inference(subsumption_resolution, [], [f327477, f633])).
fof(f327477, plain, (~ ssItem(sK57) | lt(sK57, sK58) | ~ ssItem(sK58) | (spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f327476, f138882])).
fof(f327476, plain, (lt(sK57, sK58) | ~ ssItem(sK58) | ~ ssItem(sK59) | (spl64_6522 | ~ spl64_6730)), inference(subsumption_resolution, [], [f323819, f129357])).
fof(f129357, plain, (~ (sK57 = sK58) | spl64_6522), inference(avatar_component_clause, [], [f129356])).
fof(f323819, plain, (lt(sK57, sK58) | (sK57 = sK58) | ~ ssItem(sK58) | ~ ssItem(sK59) | ~ spl64_6730), inference(forward_demodulation, [], [f311972, f138882])).
fof(f311972, plain, ((sK57 = sK58) | lt(sK59, sK58) | ~ ssItem(sK58) | ~ ssItem(sK59) | ~ spl64_6730), inference(forward_demodulation, [], [f2107, f138882])).
fof(f2107, plain, ((sK58 = sK59) | lt(sK59, sK58) | ~ ssItem(sK58) | ~ ssItem(sK59)), inference(resolution, [], [f566, f548])).
fof(f548, plain, ! [X0, X1] : (~ leq(X0, X1) | (X0 = X1) | lt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f216])).
fof(f216, plain, ! [X0] : (! [X1] : (lt(X0, X1) | (X0 = X1) | ~ leq(X0, X1) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f215])).
fof(f215, plain, ! [X0] : (! [X1] : (((lt(X0, X1) | (X0 = X1)) | ~ leq(X0, X1)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f92])).
fof(f92, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (leq(X0, X1) => (lt(X0, X1) | (X0 = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax92)).
fof(f566, plain, leq(sK59, sK58), inference(cnf_transformation, [], [f354])).
fof(f325250, plain, (~ spl64_16616 | ~ spl64_16618 | ~ spl64_5 | spl64_6522 | ~ spl64_6730), inference(avatar_split_clause, [], [f325249, f138880, f129356, f631, f325239, f323824])).
fof(f325249, plain, (~ leq(sK58, sK57) | ~ ssItem(sK58) | (~ spl64_5 | spl64_6522 | ~ spl64_6730)), inference(subsumption_resolution, [], [f325248, f633])).
fof(f325248, plain, (~ ssItem(sK57) | ~ leq(sK58, sK57) | ~ ssItem(sK58) | (spl64_6522 | ~ spl64_6730)), inference(forward_demodulation, [], [f325247, f138882])).
fof(f325247, plain, (~ leq(sK58, sK57) | ~ ssItem(sK59) | ~ ssItem(sK58) | (spl64_6522 | ~ spl64_6730)), inference(subsumption_resolution, [], [f323843, f129357])).
fof(f323843, plain, (~ leq(sK58, sK57) | (sK57 = sK58) | ~ ssItem(sK59) | ~ ssItem(sK58) | ~ spl64_6730), inference(forward_demodulation, [], [f311950, f138882])).
fof(f311950, plain, ((sK57 = sK58) | ~ leq(sK58, sK59) | ~ ssItem(sK59) | ~ ssItem(sK58) | ~ spl64_6730), inference(forward_demodulation, [], [f2108, f138882])).
fof(f2108, plain, ((sK58 = sK59) | ~ leq(sK58, sK59) | ~ ssItem(sK59) | ~ ssItem(sK58)), inference(resolution, [], [f566, f462])).
fof(f462, plain, ! [X0, X1] : (~ leq(X1, X0) | (X0 = X1) | ~ leq(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f137])).
fof(f137, plain, ! [X0] : (! [X1] : ((X0 = X1) | ~ leq(X1, X0) | ~ leq(X0, X1) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f136])).
fof(f136, plain, ! [X0] : (! [X1] : (((X0 = X1) | (~ leq(X1, X0) | ~ leq(X0, X1))) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ((leq(X1, X0) & leq(X0, X1)) => (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax29)).
fof(f325123, plain, spl64_16616, inference(avatar_split_clause, [], [f560, f323824])).
fof(f311462, plain, (~ spl64_4 | ~ spl64_78 | ~ spl64_84 | ~ spl64_2958 | ~ spl64_6522 | ~ spl64_6730 | spl64_16439), inference(avatar_contradiction_clause, [], [f311461])).
fof(f311461, plain, ($false | (~ spl64_4 | ~ spl64_78 | ~ spl64_84 | ~ spl64_2958 | ~ spl64_6522 | ~ spl64_6730 | spl64_16439)), inference(subsumption_resolution, [], [f310690, f308132])).
fof(f308132, plain, (frontsegP(cons(sK57, sK61), sK53) | (~ spl64_4 | ~ spl64_78 | ~ spl64_84 | ~ spl64_2958 | ~ spl64_6730)), inference(forward_demodulation, [], [f307251, f65286])).
fof(f307251, plain, (frontsegP(cons(sK57, sK61), cons(sK57, nil)) | (~ spl64_78 | ~ spl64_84 | ~ spl64_2958 | ~ spl64_6730)), inference(backward_demodulation, [], [f302424, f138882])).
fof(f302424, plain, (frontsegP(cons(sK59, sK61), cons(sK59, nil)) | (~ spl64_78 | ~ spl64_84 | ~ spl64_2958)), inference(subsumption_resolution, [], [f302423, f58665])).
fof(f58665, plain, (ssList(cons(sK59, sK61)) | ~ spl64_2958), inference(avatar_component_clause, [], [f58664])).
fof(f58664, plain, (spl64_2958 <=> ssList(cons(sK59, sK61))), introduced(avatar_definition, [new_symbols(naming, [spl64_2958])])).
fof(f302423, plain, (frontsegP(cons(sK59, sK61), cons(sK59, nil)) | ~ ssList(cons(sK59, sK61)) | (~ spl64_78 | ~ spl64_84)), inference(subsumption_resolution, [], [f302422, f2422])).
fof(f302422, plain, (frontsegP(cons(sK59, sK61), cons(sK59, nil)) | ~ ssList(cons(sK59, nil)) | ~ ssList(cons(sK59, sK61)) | ~ spl64_78), inference(subsumption_resolution, [], [f302380, f563])).
fof(f302380, plain, (frontsegP(cons(sK59, sK61), cons(sK59, nil)) | ~ ssList(sK61) | ~ ssList(cons(sK59, nil)) | ~ ssList(cons(sK59, sK61)) | ~ spl64_78), inference(superposition, [], [f581, f18323])).
fof(f18323, plain, ((cons(sK59, sK61) = app(cons(sK59, nil), sK61)) | ~ spl64_78), inference(resolution, [], [f1607, f2099])).
fof(f310690, plain, (~ frontsegP(cons(sK57, sK61), sK53) | (~ spl64_6522 | spl64_16439)), inference(backward_demodulation, [], [f306081, f129358])).
fof(f306081, plain, (~ frontsegP(cons(sK58, sK61), sK53) | spl64_16439), inference(avatar_component_clause, [], [f306079])).
fof(f308965, plain, (spl64_6522 | ~ spl64_4 | ~ spl64_5 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_2951 | spl64_2952 | ~ spl64_4975 | ~ spl64_6730), inference(avatar_split_clause, [], [f307997, f138880, f85808, f58635, f58631, f28549, f1209, f631, f626, f129356])).
fof(f307997, plain, ((sK57 = sK58) | (~ spl64_4 | ~ spl64_5 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_2951 | spl64_2952 | ~ spl64_4975 | ~ spl64_6730)), inference(forward_demodulation, [], [f307993, f65287])).
fof(f307993, plain, ((sK58 = hd(sK53)) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_2951 | spl64_2952 | ~ spl64_4975 | ~ spl64_6730)), inference(backward_demodulation, [], [f71360, f307992])).
fof(f307992, plain, ((sK53 = app(cons(sK58, sK61), sK53)) | (~ spl64_4 | ~ spl64_37 | ~ spl64_1493 | ~ spl64_4975 | ~ spl64_6730)), inference(forward_demodulation, [], [f306894, f65286])).
fof(f306894, plain, ((sK53 = app(cons(sK58, sK61), cons(sK57, nil))) | (~ spl64_37 | ~ spl64_1493 | ~ spl64_4975 | ~ spl64_6730)), inference(backward_demodulation, [], [f202070, f138882])).
fof(f202070, plain, ((sK53 = app(cons(sK58, sK61), cons(sK59, nil))) | (~ spl64_37 | ~ spl64_1493 | ~ spl64_4975)), inference(forward_demodulation, [], [f202069, f18322])).
fof(f202069, plain, ((sK53 = app(app(cons(sK58, nil), sK61), cons(sK59, nil))) | (~ spl64_37 | ~ spl64_1493 | ~ spl64_4975)), inference(forward_demodulation, [], [f202068, f28701])).
fof(f202068, plain, ((sK53 = app(app(app(nil, cons(sK58, nil)), sK61), cons(sK59, nil))) | (~ spl64_37 | ~ spl64_4975)), inference(forward_demodulation, [], [f85810, f1211])).
fof(f71360, plain, ((sK58 = hd(app(cons(sK58, sK61), sK53))) | (~ spl64_2951 | spl64_2952)), inference(forward_demodulation, [], [f71359, f16866])).
fof(f16866, plain, (sK58 = hd(cons(sK58, sK61))), inference(resolution, [], [f1117, f560])).
fof(f1117, plain, ! [X54] : (~ ssItem(X54) | (hd(cons(X54, sK61)) = X54)), inference(resolution, [], [f456, f563])).
fof(f71359, plain, ((hd(cons(sK58, sK61)) = hd(app(cons(sK58, sK61), sK53))) | (~ spl64_2951 | spl64_2952)), inference(subsumption_resolution, [], [f71294, f58636])).
fof(f58636, plain, (~ (nil = cons(sK58, sK61)) | spl64_2952), inference(avatar_component_clause, [], [f58635])).
fof(f71294, plain, ((hd(cons(sK58, sK61)) = hd(app(cons(sK58, sK61), sK53))) | (nil = cons(sK58, sK61)) | ~ spl64_2951), inference(resolution, [], [f2824, f58632])).
fof(f308635, plain, (spl64_5228 | ~ spl64_4 | ~ spl64_478 | ~ spl64_1493 | ~ spl64_6730), inference(avatar_split_clause, [], [f307508, f138880, f28549, f9912, f626, f104999])).
fof(f307508, plain, (strictorderedP(cons(sK58, sK53)) | (~ spl64_4 | ~ spl64_478 | ~ spl64_1493 | ~ spl64_6730)), inference(forward_demodulation, [], [f307507, f70959])).
fof(f70959, plain, (cons(sK58, sK53) = app(cons(sK58, nil), sK53)), inference(resolution, [], [f2821, f560])).
fof(f307507, plain, (strictorderedP(app(cons(sK58, nil), sK53)) | (~ spl64_4 | ~ spl64_478 | ~ spl64_1493 | ~ spl64_6730)), inference(forward_demodulation, [], [f306402, f65286])).
fof(f306402, plain, (strictorderedP(app(cons(sK58, nil), cons(sK57, nil))) | (~ spl64_478 | ~ spl64_1493 | ~ spl64_6730)), inference(backward_demodulation, [], [f29062, f138882])).
fof(f29062, plain, (strictorderedP(app(cons(sK58, nil), cons(sK59, nil))) | (~ spl64_478 | ~ spl64_1493)), inference(forward_demodulation, [], [f28925, f9784])).
fof(f9784, plain, (cons(sK58, nil) = app(cons(sK58, nil), nil)), inference(resolution, [], [f1576, f560])).
fof(f28925, plain, (strictorderedP(app(app(cons(sK58, nil), nil), cons(sK59, nil))) | (~ spl64_478 | ~ spl64_1493)), inference(backward_demodulation, [], [f9914, f28701])).
fof(f9914, plain, (strictorderedP(app(app(app(nil, cons(sK58, nil)), nil), cons(sK59, nil))) | ~ spl64_478), inference(avatar_component_clause, [], [f9912])).
fof(f307944, plain, (~ spl64_4098 | spl64_6727 | ~ spl64_6730), inference(avatar_contradiction_clause, [], [f307943])).
fof(f307943, plain, ($false | (~ spl64_4098 | spl64_6727 | ~ spl64_6730)), inference(subsumption_resolution, [], [f306863, f72812])).
fof(f306863, plain, (~ leq(sK57, sK57) | (spl64_6727 | ~ spl64_6730)), inference(backward_demodulation, [], [f138866, f138882])).
fof(f138866, plain, (~ leq(sK57, sK59) | spl64_6727), inference(avatar_component_clause, [], [f138864])).
fof(f303941, plain, (spl64_16372 | ~ spl64_16373 | ~ spl64_1493), inference(avatar_split_clause, [], [f303932, f28549, f303938, f303934])).
fof(f303932, plain, (~ frontsegP(cons(sK58, nil), cons(sK58, sK53)) | (cons(sK58, nil) = cons(sK58, sK53)) | ~ spl64_1493), inference(subsumption_resolution, [], [f303931, f667])).
fof(f303931, plain, (~ frontsegP(cons(sK58, nil), cons(sK58, sK53)) | (cons(sK58, nil) = cons(sK58, sK53)) | ~ ssList(sK53) | ~ spl64_1493), inference(subsumption_resolution, [], [f303874, f28550])).
fof(f303874, plain, (~ frontsegP(cons(sK58, nil), cons(sK58, sK53)) | ~ ssList(cons(sK58, nil)) | (cons(sK58, nil) = cons(sK58, sK53)) | ~ ssList(sK53)), inference(superposition, [], [f1656, f70959])).
fof(f241693, plain, (~ spl64_4 | ~ spl64_5 | spl64_122 | ~ spl64_7003), inference(avatar_contradiction_clause, [], [f241692])).
fof(f241692, plain, ($false | (~ spl64_4 | ~ spl64_5 | spl64_122 | ~ spl64_7003)), inference(subsumption_resolution, [], [f241691, f2873])).
fof(f241691, plain, ((nil = sK53) | (~ spl64_4 | ~ spl64_5 | ~ spl64_7003)), inference(forward_demodulation, [], [f241506, f65288])).
fof(f241506, plain, ((sK53 = tl(sK53)) | (~ spl64_5 | ~ spl64_7003)), inference(backward_demodulation, [], [f69460, f145226])).
fof(f145226, plain, ((sK53 = cons(sK57, sK53)) | ~ spl64_7003), inference(avatar_component_clause, [], [f145224])).
fof(f69460, plain, ((sK53 = tl(cons(sK57, sK53))) | ~ spl64_5), inference(resolution, [], [f2815, f633])).
fof(f2815, plain, ! [X1] : (~ ssItem(X1) | (sK53 = tl(cons(X1, sK53)))), inference(resolution, [], [f667, f458])).
fof(f198868, plain, (spl64_122 | spl64_186), inference(avatar_contradiction_clause, [], [f198867])).
fof(f198867, plain, ($false | (spl64_122 | spl64_186)), inference(subsumption_resolution, [], [f198866, f667])).
fof(f198866, plain, (~ ssList(sK53) | (spl64_122 | spl64_186)), inference(subsumption_resolution, [], [f198865, f2873])).
fof(f198865, plain, ((nil = sK53) | ~ ssList(sK53) | spl64_186), inference(resolution, [], [f3668, f452])).
fof(f452, plain, ! [X0] : (ssItem(sK50(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f319])).
fof(f3668, plain, (~ ssItem(sK50(sK53)) | spl64_186), inference(avatar_component_clause, [], [f3666])).
fof(f196353, plain, (~ spl64_37 | spl64_122 | ~ spl64_7231), inference(avatar_contradiction_clause, [], [f196352])).
fof(f196352, plain, ($false | (~ spl64_37 | spl64_122 | ~ spl64_7231)), inference(subsumption_resolution, [], [f196351, f2873])).
fof(f196351, plain, ((nil = sK53) | (~ spl64_37 | ~ spl64_7231)), inference(backward_demodulation, [], [f2817, f196350])).
fof(f196350, plain, ((nil = app(nil, sK53)) | (~ spl64_37 | ~ spl64_7231)), inference(forward_demodulation, [], [f148827, f1211])).
fof(f148827, plain, ((nil = app(sK60, sK53)) | ~ spl64_7231), inference(avatar_component_clause, [], [f148825])).
fof(f193593, plain, (spl64_114 | ~ spl64_4 | ~ spl64_5), inference(avatar_split_clause, [], [f193592, f631, f626, f2836])).
fof(f193592, plain, (strictorderedP(sK53) | (~ spl64_4 | ~ spl64_5)), inference(subsumption_resolution, [], [f66546, f633])).
fof(f66546, plain, (strictorderedP(sK53) | ~ ssItem(sK57) | ~ spl64_4), inference(superposition, [], [f516, f65286])).
fof(f516, plain, ! [X0] : (strictorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ! [X0] : (strictorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f68])).
fof(f68, plain, ! [X0] : (ssItem(X0) => strictorderedP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax68)).
fof(f188816, plain, (~ spl64_4030 | spl64_4096 | ~ spl64_4098), inference(avatar_contradiction_clause, [], [f188815])).
fof(f188815, plain, ($false | (~ spl64_4030 | spl64_4096 | ~ spl64_4098)), inference(subsumption_resolution, [], [f188757, f72812])).
fof(f188757, plain, (~ leq(sK57, sK57) | (~ spl64_4030 | spl64_4096)), inference(backward_demodulation, [], [f72800, f72064])).
fof(f72800, plain, (~ leq(sK57, hd(sK60)) | spl64_4096), inference(avatar_component_clause, [], [f72798])).
fof(f72798, plain, (spl64_4096 <=> leq(sK57, hd(sK60))), introduced(avatar_definition, [new_symbols(naming, [spl64_4096])])).
fof(f186358, plain, spl64_8415, inference(avatar_split_clause, [], [f562, f186182])).
fof(f185728, plain, (spl64_4985 | ~ spl64_41 | ~ spl64_84 | ~ spl64_4984), inference(avatar_split_clause, [], [f185727, f85861, f2421, f1227, f85869])).
fof(f185727, plain, (! [X5] : (~ segmentP(X5, sK53) | segmentP(X5, cons(sK59, nil)) | ~ ssList(X5)) | (~ spl64_41 | ~ spl64_84 | ~ spl64_4984)), inference(subsumption_resolution, [], [f183785, f85862])).
fof(f183785, plain, (! [X5] : (~ segmentP(X5, sK53) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | segmentP(X5, cons(sK59, nil)) | ~ ssList(X5)) | (~ spl64_41 | ~ spl64_84)), inference(subsumption_resolution, [], [f183784, f447])).
fof(f183784, plain, (! [X5] : (~ segmentP(X5, sK53) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | segmentP(X5, cons(sK59, nil)) | ~ ssList(nil) | ~ ssList(X5)) | (~ spl64_41 | ~ spl64_84)), inference(subsumption_resolution, [], [f183783, f667])).
fof(f183783, plain, (! [X5] : (~ segmentP(X5, sK53) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(sK53) | segmentP(X5, cons(sK59, nil)) | ~ ssList(nil) | ~ ssList(X5)) | (~ spl64_41 | ~ spl64_84)), inference(subsumption_resolution, [], [f165789, f2422])).
fof(f165789, plain, (! [X5] : (~ segmentP(X5, sK53) | ~ ssList(app(app(sK60, cons(sK58, nil)), sK61)) | ~ ssList(cons(sK59, nil)) | ~ ssList(sK53) | segmentP(X5, cons(sK59, nil)) | ~ ssList(nil) | ~ ssList(X5)) | ~ spl64_41), inference(superposition, [], [f2510, f16196])).
fof(f16196, plain, ((sK53 = app(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil)), nil)) | ~ spl64_41), inference(forward_demodulation, [], [f565, f1229])).
fof(f2510, plain, ! [X6, X4, X5, X3] : (~ segmentP(X6, app(app(X4, X5), X3)) | ~ ssList(X4) | ~ ssList(X5) | ~ ssList(app(app(X4, X5), X3)) | segmentP(X6, X5) | ~ ssList(X3) | ~ ssList(X6)), inference(duplicate_literal_removal, [], [f2502])).
fof(f2502, plain, ! [X6, X4, X5, X3] : (~ ssList(X3) | ~ ssList(X4) | ~ ssList(X5) | ~ ssList(app(app(X4, X5), X3)) | segmentP(X6, X5) | ~ segmentP(X6, app(app(X4, X5), X3)) | ~ ssList(X5) | ~ ssList(app(app(X4, X5), X3)) | ~ ssList(X6)), inference(resolution, [], [f583, f496])).
fof(f496, plain, ! [X2, X0, X1] : (~ segmentP(X1, X2) | segmentP(X0, X2) | ~ segmentP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ! [X0] : (! [X1] : (! [X2] : (segmentP(X0, X2) | ~ segmentP(X1, X2) | ~ segmentP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f169])).
fof(f169, plain, ! [X0] : (! [X1] : (! [X2] : ((segmentP(X0, X2) | (~ segmentP(X1, X2) | ~ segmentP(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f53])).
fof(f53, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((segmentP(X1, X2) & segmentP(X0, X1)) => segmentP(X0, X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax53)).
fof(f176936, plain, (~ spl64_860 | spl64_4977 | ~ spl64_41), inference(avatar_split_clause, [], [f176935, f1227, f85819, f16513])).
fof(f176935, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ spl64_41), inference(subsumption_resolution, [], [f176934, f667])).
fof(f176934, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(sK53) | ~ spl64_41), inference(subsumption_resolution, [], [f94362, f447])).
fof(f94362, plain, (frontsegP(sK53, app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(nil) | ~ ssList(app(app(app(sK60, cons(sK58, nil)), sK61), cons(sK59, nil))) | ~ ssList(sK53) | ~ spl64_41), inference(superposition, [], [f581, f16196])).
fof(f73666, plain, (spl64_518 | ~ spl64_5 | spl64_37 | ~ spl64_59 | ~ spl64_4096), inference(avatar_split_clause, [], [f73665, f72798, f1876, f1209, f631, f10221])).
fof(f1876, plain, (spl64_59 <=> totalorderedP(sK60)), introduced(avatar_definition, [new_symbols(naming, [spl64_59])])).
fof(f73665, plain, (totalorderedP(cons(sK57, sK60)) | (~ spl64_5 | spl64_37 | ~ spl64_59 | ~ spl64_4096)), inference(subsumption_resolution, [], [f73664, f633])).
fof(f73664, plain, (totalorderedP(cons(sK57, sK60)) | ~ ssItem(sK57) | (spl64_37 | ~ spl64_59 | ~ spl64_4096)), inference(subsumption_resolution, [], [f73663, f562])).
fof(f73663, plain, (totalorderedP(cons(sK57, sK60)) | ~ ssList(sK60) | ~ ssItem(sK57) | (spl64_37 | ~ spl64_59 | ~ spl64_4096)), inference(subsumption_resolution, [], [f73662, f1210])).
fof(f73662, plain, (totalorderedP(cons(sK57, sK60)) | (nil = sK60) | ~ ssList(sK60) | ~ ssItem(sK57) | (~ spl64_59 | ~ spl64_4096)), inference(subsumption_resolution, [], [f73646, f1877])).
fof(f1877, plain, (totalorderedP(sK60) | ~ spl64_59), inference(avatar_component_clause, [], [f1876])).
fof(f73646, plain, (totalorderedP(cons(sK57, sK60)) | ~ totalorderedP(sK60) | (nil = sK60) | ~ ssList(sK60) | ~ ssItem(sK57) | ~ spl64_4096), inference(resolution, [], [f72799, f515])).
fof(f515, plain, ! [X0, X1] : (~ leq(X0, hd(X1)) | totalorderedP(cons(X0, X1)) | ~ totalorderedP(X1) | (nil = X1) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f332])).
fof(f332, plain, ! [X0] : (! [X1] : (((totalorderedP(cons(X0, X1)) | ((~ leq(X0, hd(X1)) | ~ totalorderedP(X1) | (nil = X1)) & ~ (nil = X1))) & ((leq(X0, hd(X1)) & totalorderedP(X1) & ~ (nil = X1)) | (nil = X1) | ~ totalorderedP(cons(X0, X1)))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f331])).
fof(f331, plain, ! [X0] : (! [X1] : (((totalorderedP(cons(X0, X1)) | ((~ leq(X0, hd(X1)) | ~ totalorderedP(X1) | (nil = X1)) & ~ (nil = X1))) & (((leq(X0, hd(X1)) & totalorderedP(X1) & ~ (nil = X1)) | (nil = X1)) | ~ totalorderedP(cons(X0, X1)))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f182])).
fof(f182, plain, ! [X0] : (! [X1] : ((totalorderedP(cons(X0, X1)) <=> ((leq(X0, hd(X1)) & totalorderedP(X1) & ~ (nil = X1)) | (nil = X1))) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f67])).
fof(f67, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => (totalorderedP(cons(X0, X1)) <=> ((leq(X0, hd(X1)) & totalorderedP(X1) & ~ (nil = X1)) | (nil = X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax67)).
fof(f72799, plain, (leq(sK57, hd(sK60)) | ~ spl64_4096), inference(avatar_component_clause, [], [f72798])).
fof(f73056, plain, (~ spl64_5 | ~ spl64_4111), inference(avatar_contradiction_clause, [], [f73055])).
fof(f73055, plain, ($false | (~ spl64_5 | ~ spl64_4111)), inference(subsumption_resolution, [], [f73044, f633])).
fof(f73044, plain, (~ ssItem(sK57) | ~ spl64_4111), inference(resolution, [], [f73039, f546])).
fof(f546, plain, ! [X0] : (~ lt(X0, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f212])).
fof(f212, plain, ! [X0] : (~ lt(X0, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f90])).
fof(f90, plain, ! [X0] : (ssItem(X0) => ~ lt(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax90)).
fof(f73039, plain, (lt(sK57, sK57) | ~ spl64_4111), inference(avatar_component_clause, [], [f73037])).
fof(f72820, plain, (~ spl64_5 | spl64_4098), inference(avatar_contradiction_clause, [], [f72819])).
fof(f72819, plain, ($false | (~ spl64_5 | spl64_4098)), inference(subsumption_resolution, [], [f72818, f633])).
fof(f72818, plain, (~ ssItem(sK57) | spl64_4098), inference(resolution, [], [f72813, f464])).
fof(f464, plain, ! [X0] : (leq(X0, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0] : (leq(X0, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : (ssItem(X0) => leq(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax31)).
fof(f72813, plain, (~ leq(sK57, sK57) | spl64_4098), inference(avatar_component_clause, [], [f72811])).
fof(f65428, plain, (~ spl64_4 | spl64_122 | ~ spl64_3585), inference(avatar_contradiction_clause, [], [f65427])).
fof(f65427, plain, ($false | (~ spl64_4 | spl64_122 | ~ spl64_3585)), inference(subsumption_resolution, [], [f65426, f2873])).
fof(f65426, plain, ((nil = sK53) | (~ spl64_4 | ~ spl64_3585)), inference(backward_demodulation, [], [f65286, f65077])).
fof(f65077, plain, ((nil = cons(sK57, nil)) | ~ spl64_3585), inference(avatar_component_clause, [], [f65075])).
fof(f65075, plain, (spl64_3585 <=> (nil = cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_3585])])).
fof(f65425, plain, (spl64_3584 | ~ spl64_4 | ~ spl64_1356), inference(avatar_split_clause, [], [f65345, f22395, f626, f65065])).
fof(f22395, plain, (spl64_1356 <=> sP4(cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_1356])])).
fof(f65345, plain, (sP4(sK53) | (~ spl64_4 | ~ spl64_1356)), inference(backward_demodulation, [], [f22396, f65286])).
fof(f22396, plain, (sP4(cons(sK57, nil)) | ~ spl64_1356), inference(avatar_component_clause, [], [f22395])).
fof(f65285, plain, (spl64_3585 | spl64_3619 | ~ spl64_1260 | ~ spl64_1450), inference(avatar_split_clause, [], [f65280, f26181, f21436, f65282, f65075])).
fof(f21436, plain, (spl64_1260 <=> ssList(cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_1260])])).
fof(f65280, plain, ((cons(sK57, nil) = cons(sK50(cons(sK57, nil)), nil)) | (nil = cons(sK57, nil)) | (~ spl64_1260 | ~ spl64_1450)), inference(forward_demodulation, [], [f21539, f26183])).
fof(f26183, plain, ((nil = sK49(cons(sK57, nil))) | ~ spl64_1450), inference(avatar_component_clause, [], [f26181])).
fof(f21539, plain, ((nil = cons(sK57, nil)) | (cons(sK57, nil) = cons(sK50(cons(sK57, nil)), sK49(cons(sK57, nil)))) | ~ spl64_1260), inference(resolution, [], [f21437, f453])).
fof(f21437, plain, (ssList(cons(sK57, nil)) | ~ spl64_1260), inference(avatar_component_clause, [], [f21436])).
fof(f65071, plain, (spl64_122 | ~ spl64_2), inference(avatar_split_clause, [], [f65070, f616, f2872])).
fof(f616, plain, (spl64_2 <=> (nil = sK55)), introduced(avatar_definition, [new_symbols(naming, [spl64_2])])).
fof(f65070, plain, ((nil = sK53) | ~ spl64_2), inference(forward_demodulation, [], [f559, f618])).
fof(f618, plain, ((nil = sK55) | ~ spl64_2), inference(avatar_component_clause, [], [f616])).
fof(f60795, plain, (~ spl64_78 | spl64_2958), inference(avatar_contradiction_clause, [], [f60794])).
fof(f60794, plain, ($false | (~ spl64_78 | spl64_2958)), inference(subsumption_resolution, [], [f60793, f563])).
fof(f60793, plain, (~ ssList(sK61) | (~ spl64_78 | spl64_2958)), inference(subsumption_resolution, [], [f60792, f2099])).
fof(f60792, plain, (~ ssItem(sK59) | ~ ssList(sK61) | spl64_2958), inference(resolution, [], [f58666, f446])).
fof(f446, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax16)).
fof(f58666, plain, (~ ssList(cons(sK59, sK61)) | spl64_2958), inference(avatar_component_clause, [], [f58664])).
fof(f58695, plain, spl64_2951, inference(avatar_contradiction_clause, [], [f58694])).
fof(f58694, plain, ($false | spl64_2951), inference(subsumption_resolution, [], [f58693, f563])).
fof(f58693, plain, (~ ssList(sK61) | spl64_2951), inference(subsumption_resolution, [], [f58692, f560])).
fof(f58692, plain, (~ ssItem(sK58) | ~ ssList(sK61) | spl64_2951), inference(resolution, [], [f58633, f446])).
fof(f58633, plain, (~ ssList(cons(sK58, sK61)) | spl64_2951), inference(avatar_component_clause, [], [f58631])).
fof(f55858, plain, (~ spl64_5 | spl64_2689), inference(avatar_contradiction_clause, [], [f55857])).
fof(f55857, plain, ($false | (~ spl64_5 | spl64_2689)), inference(subsumption_resolution, [], [f55856, f563])).
fof(f55856, plain, (~ ssList(sK61) | (~ spl64_5 | spl64_2689)), inference(subsumption_resolution, [], [f55855, f633])).
fof(f55855, plain, (~ ssItem(sK57) | ~ ssList(sK61) | spl64_2689), inference(resolution, [], [f55816, f446])).
fof(f55816, plain, (~ ssList(cons(sK57, sK61)) | spl64_2689), inference(avatar_component_clause, [], [f55814])).
fof(f32241, plain, (~ spl64_37 | spl64_542 | ~ spl64_1351), inference(avatar_contradiction_clause, [], [f32240])).
fof(f32240, plain, ($false | (~ spl64_37 | spl64_542 | ~ spl64_1351)), inference(subsumption_resolution, [], [f31609, f22257])).
fof(f22257, plain, (sP0(cons(sK57, nil)) | ~ spl64_1351), inference(avatar_component_clause, [], [f22256])).
fof(f22256, plain, (spl64_1351 <=> sP0(cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_1351])])).
fof(f31609, plain, (~ sP0(cons(sK57, nil)) | (~ spl64_37 | spl64_542)), inference(backward_demodulation, [], [f10607, f1211])).
fof(f10607, plain, (~ sP0(cons(sK57, sK60)) | spl64_542), inference(avatar_component_clause, [], [f10605])).
fof(f28633, plain, spl64_1493, inference(avatar_contradiction_clause, [], [f28632])).
fof(f28632, plain, ($false | spl64_1493), inference(subsumption_resolution, [], [f28631, f447])).
fof(f28631, plain, (~ ssList(nil) | spl64_1493), inference(subsumption_resolution, [], [f28630, f560])).
fof(f28630, plain, (~ ssItem(sK58) | ~ ssList(nil) | spl64_1493), inference(resolution, [], [f28551, f446])).
fof(f28551, plain, (~ ssList(cons(sK58, nil)) | spl64_1493), inference(avatar_component_clause, [], [f28549])).
fof(f27481, plain, (~ spl64_5 | spl64_1355), inference(avatar_contradiction_clause, [], [f27480])).
fof(f27480, plain, ($false | (~ spl64_5 | spl64_1355)), inference(subsumption_resolution, [], [f27479, f633])).
fof(f27479, plain, (~ ssItem(sK57) | spl64_1355), inference(resolution, [], [f22392, f507])).
fof(f507, plain, ! [X0] : (strictorderP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f180])).
fof(f180, plain, ! [X0] : (strictorderP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f63])).
fof(f63, plain, ! [X0] : (ssItem(X0) => strictorderP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax63)).
fof(f22392, plain, (~ strictorderP(cons(sK57, nil)) | spl64_1355), inference(avatar_component_clause, [], [f22391])).
fof(f22391, plain, (spl64_1355 <=> strictorderP(cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_1355])])).
fof(f22612, plain, (~ spl64_5 | spl64_1350), inference(avatar_contradiction_clause, [], [f22611])).
fof(f22611, plain, ($false | (~ spl64_5 | spl64_1350)), inference(subsumption_resolution, [], [f22610, f633])).
fof(f22610, plain, (~ ssItem(sK57) | spl64_1350), inference(resolution, [], [f22253, f503])).
fof(f503, plain, ! [X0] : (cyclefreeP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ! [X0] : (cyclefreeP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f59])).
fof(f59, plain, ! [X0] : (ssItem(X0) => cyclefreeP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax59)).
fof(f22253, plain, (~ cyclefreeP(cons(sK57, nil)) | spl64_1350), inference(avatar_component_clause, [], [f22252])).
fof(f22252, plain, (spl64_1350 <=> cyclefreeP(cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_1350])])).
fof(f22399, plain, (spl64_1356 | ~ spl64_1355 | ~ spl64_1260), inference(avatar_split_clause, [], [f22389, f21436, f22391, f22395])).
fof(f22389, plain, (~ strictorderP(cons(sK57, nil)) | sP4(cons(sK57, nil)) | ~ spl64_1260), inference(resolution, [], [f21533, f401])).
fof(f401, plain, ! [X0] : (~ sP5(X0) | ~ strictorderP(X0) | sP4(X0)), inference(cnf_transformation, [], [f276])).
fof(f276, plain, ! [X0] : (((strictorderP(X0) | ~ sP4(X0)) & (sP4(X0) | ~ strictorderP(X0))) | ~ sP5(X0)), inference(nnf_transformation, [], [f230])).
fof(f230, plain, ! [X0] : ((strictorderP(X0) <=> sP4(X0)) | ~ sP5(X0)), inference(usedef, [], [e230])).
fof(e230, plain, ! [X0] : (sP5(X0) <=> (strictorderP(X0) <=> sP4(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f21533, plain, (sP5(cons(sK57, nil)) | ~ spl64_1260), inference(resolution, [], [f21437, f412])).
fof(f412, plain, ! [X0] : (~ ssList(X0) | sP5(X0)), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ! [X0] : (sP5(X0) | ~ ssList(X0)), inference(definition_folding, [], [f110, e230, e229])).
fof(f110, plain, ! [X0] : ((strictorderP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (lt(X2, X1) | lt(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(flattening, [], [f109])).
fof(f109, plain, ! [X0] : ((strictorderP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (((lt(X2, X1) | lt(X1, X2)) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0)) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (ssList(X0) => (strictorderP(X0) <=> ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssList(X3) => ! [X4] : (ssList(X4) => ! [X5] : (ssList(X5) => ((app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) => (lt(X2, X1) | lt(X1, X2)))))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax10)).
fof(f22260, plain, (spl64_1351 | ~ spl64_1350 | ~ spl64_1260), inference(avatar_split_clause, [], [f22250, f21436, f22252, f22256])).
fof(f22250, plain, (~ cyclefreeP(cons(sK57, nil)) | sP0(cons(sK57, nil)) | ~ spl64_1260), inference(resolution, [], [f21531, f377])).
fof(f377, plain, ! [X0] : (~ sP1(X0) | ~ cyclefreeP(X0) | sP0(X0)), inference(cnf_transformation, [], [f258])).
fof(f258, plain, ! [X0] : (((cyclefreeP(X0) | ~ sP0(X0)) & (sP0(X0) | ~ cyclefreeP(X0))) | ~ sP1(X0)), inference(nnf_transformation, [], [f224])).
fof(f224, plain, ! [X0] : ((cyclefreeP(X0) <=> sP0(X0)) | ~ sP1(X0)), inference(usedef, [], [e224])).
fof(e224, plain, ! [X0] : (sP1(X0) <=> (cyclefreeP(X0) <=> sP0(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f21531, plain, (sP1(cons(sK57, nil)) | ~ spl64_1260), inference(resolution, [], [f21437, f388])).
fof(f388, plain, ! [X0] : (~ ssList(X0) | sP1(X0)), inference(cnf_transformation, [], [f225])).
fof(f225, plain, ! [X0] : (sP1(X0) | ~ ssList(X0)), inference(definition_folding, [], [f106, e224, e223])).
fof(f106, plain, ! [X0] : ((cyclefreeP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (~ leq(X2, X1) | ~ leq(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(flattening, [], [f105])).
fof(f105, plain, ! [X0] : ((cyclefreeP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (((~ leq(X2, X1) | ~ leq(X1, X2)) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0)) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0] : (ssList(X0) => (cyclefreeP(X0) <=> ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssList(X3) => ! [X4] : (ssList(X4) => ! [X5] : (ssList(X5) => ((app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) => ~ (leq(X2, X1) & leq(X1, X2)))))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax8)).
fof(f21497, plain, (~ spl64_5 | spl64_1260), inference(avatar_contradiction_clause, [], [f21496])).
fof(f21496, plain, ($false | (~ spl64_5 | spl64_1260)), inference(subsumption_resolution, [], [f21495, f447])).
fof(f21495, plain, (~ ssList(nil) | (~ spl64_5 | spl64_1260)), inference(subsumption_resolution, [], [f21494, f633])).
fof(f21494, plain, (~ ssItem(sK57) | ~ ssList(nil) | spl64_1260), inference(resolution, [], [f21438, f446])).
fof(f21438, plain, (~ ssList(cons(sK57, nil)) | spl64_1260), inference(avatar_component_clause, [], [f21436])).
fof(f16199, plain, (~ spl64_222 | ~ spl64_84 | spl64_224), inference(avatar_split_clause, [], [f16198, f4067, f2421, f4053])).
fof(f16198, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), nil)) | (~ spl64_84 | spl64_224)), inference(subsumption_resolution, [], [f10100, f2422])).
fof(f10100, plain, (~ ssList(cons(sK59, nil)) | ~ ssList(app(app(sK60, cons(sK58, nil)), nil)) | spl64_224), inference(resolution, [], [f4069, f459])).
fof(f4069, plain, (~ ssList(app(app(app(sK60, cons(sK58, nil)), nil), cons(sK59, nil))) | spl64_224), inference(avatar_component_clause, [], [f4067])).
fof(f16195, plain, spl64_222, inference(avatar_contradiction_clause, [], [f16194])).
fof(f16194, plain, ($false | spl64_222), inference(subsumption_resolution, [], [f16193, f447])).
fof(f16193, plain, (~ ssList(nil) | spl64_222), inference(subsumption_resolution, [], [f16192, f560])).
fof(f16192, plain, (~ ssItem(sK58) | ~ ssList(nil) | spl64_222), inference(resolution, [], [f16184, f446])).
fof(f16184, plain, (~ ssList(cons(sK58, nil)) | spl64_222), inference(subsumption_resolution, [], [f16183, f562])).
fof(f16183, plain, (~ ssList(cons(sK58, nil)) | ~ ssList(sK60) | spl64_222), inference(resolution, [], [f6103, f459])).
fof(f6103, plain, (~ ssList(app(sK60, cons(sK58, nil))) | spl64_222), inference(subsumption_resolution, [], [f6102, f447])).
fof(f6102, plain, (~ ssList(nil) | ~ ssList(app(sK60, cons(sK58, nil))) | spl64_222), inference(resolution, [], [f4055, f459])).
fof(f4055, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), nil)) | spl64_222), inference(avatar_component_clause, [], [f4053])).
fof(f8107, plain, (spl64_41 | spl64_151), inference(avatar_contradiction_clause, [], [f8106])).
fof(f8106, plain, ($false | (spl64_41 | spl64_151)), inference(subsumption_resolution, [], [f8105, f564])).
fof(f8105, plain, (~ ssList(sK62) | (spl64_41 | spl64_151)), inference(subsumption_resolution, [], [f8104, f1228])).
fof(f8104, plain, ((nil = sK62) | ~ ssList(sK62) | spl64_151), inference(resolution, [], [f3417, f452])).
fof(f3417, plain, (~ ssItem(sK50(sK62)) | spl64_151), inference(avatar_component_clause, [], [f3415])).
fof(f6037, plain, (spl64_59 | ~ spl64_63 | ~ spl64_242 | ~ spl64_253), inference(avatar_contradiction_clause, [], [f6036])).
fof(f6036, plain, ($false | (spl64_59 | ~ spl64_63 | ~ spl64_242 | ~ spl64_253)), inference(subsumption_resolution, [], [f6035, f4902])).
fof(f6035, plain, (~ ssItem(hd(sK60)) | (spl64_59 | ~ spl64_63 | ~ spl64_253)), inference(subsumption_resolution, [], [f5994, f1878])).
fof(f1878, plain, (~ totalorderedP(sK60) | spl64_59), inference(avatar_component_clause, [], [f1876])).
fof(f5994, plain, (totalorderedP(sK60) | ~ ssItem(hd(sK60)) | (~ spl64_63 | ~ spl64_253)), inference(superposition, [], [f509, f5960])).
fof(f5960, plain, ((sK60 = cons(hd(sK60), nil)) | (~ spl64_63 | ~ spl64_253)), inference(backward_demodulation, [], [f1922, f4950])).
fof(f4950, plain, ((nil = tl(sK60)) | ~ spl64_253), inference(avatar_component_clause, [], [f4948])).
fof(f509, plain, ! [X0] : (totalorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f181])).
fof(f181, plain, ! [X0] : (totalorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : (ssItem(X0) => totalorderedP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax65)).
fof(f5863, plain, (spl64_37 | spl64_241), inference(avatar_contradiction_clause, [], [f5862])).
fof(f5862, plain, ($false | (spl64_37 | spl64_241)), inference(subsumption_resolution, [], [f5861, f562])).
fof(f5861, plain, (~ ssList(sK60) | (spl64_37 | spl64_241)), inference(subsumption_resolution, [], [f5860, f1210])).
fof(f5860, plain, ((nil = sK60) | ~ ssList(sK60) | spl64_241), inference(resolution, [], [f4899, f457])).
fof(f4899, plain, (~ ssList(tl(sK60)) | spl64_241), inference(avatar_component_clause, [], [f4897])).
fof(f5752, plain, (spl64_37 | spl64_242), inference(avatar_contradiction_clause, [], [f5751])).
fof(f5751, plain, ($false | (spl64_37 | spl64_242)), inference(subsumption_resolution, [], [f5750, f562])).
fof(f5750, plain, (~ ssList(sK60) | (spl64_37 | spl64_242)), inference(subsumption_resolution, [], [f5749, f1210])).
fof(f5749, plain, ((nil = sK60) | ~ ssList(sK60) | spl64_242), inference(resolution, [], [f4903, f455])).
fof(f4903, plain, (~ ssItem(hd(sK60)) | spl64_242), inference(avatar_component_clause, [], [f4901])).
fof(f4738, plain, (spl64_37 | spl64_54), inference(avatar_contradiction_clause, [], [f4737])).
fof(f4737, plain, ($false | (spl64_37 | spl64_54)), inference(subsumption_resolution, [], [f4736, f562])).
fof(f4736, plain, (~ ssList(sK60) | (spl64_37 | spl64_54)), inference(subsumption_resolution, [], [f4735, f1210])).
fof(f4735, plain, ((nil = sK60) | ~ ssList(sK60) | spl64_54), inference(resolution, [], [f1857, f452])).
fof(f1857, plain, (~ ssItem(sK50(sK60)) | spl64_54), inference(avatar_component_clause, [], [f1855])).
fof(f4470, plain, (~ spl64_78 | ~ spl64_92), inference(avatar_contradiction_clause, [], [f4469])).
fof(f4469, plain, ($false | (~ spl64_78 | ~ spl64_92)), inference(subsumption_resolution, [], [f4468, f447])).
fof(f4468, plain, (~ ssList(nil) | (~ spl64_78 | ~ spl64_92)), inference(subsumption_resolution, [], [f4456, f2099])).
fof(f4456, plain, (~ ssItem(sK59) | ~ ssList(nil) | ~ spl64_92), inference(trivial_inequality_removal, [], [f4427])).
fof(f4427, plain, (~ (nil = nil) | ~ ssItem(sK59) | ~ ssList(nil) | ~ spl64_92), inference(superposition, [], [f454, f2484])).
fof(f2484, plain, ((nil = cons(sK59, nil)) | ~ spl64_92), inference(avatar_component_clause, [], [f2482])).
fof(f2482, plain, (spl64_92 <=> (nil = cons(sK59, nil))), introduced(avatar_definition, [new_symbols(naming, [spl64_92])])).
fof(f454, plain, ! [X0, X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : (! [X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (nil = cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax21)).
fof(f4300, plain, (spl64_92 | ~ spl64_84 | ~ spl64_223), inference(avatar_split_clause, [], [f4299, f4057, f2421, f2482])).
fof(f4299, plain, ((nil = cons(sK59, nil)) | (~ spl64_84 | ~ spl64_223)), inference(subsumption_resolution, [], [f4295, f2422])).
fof(f4295, plain, ((nil = cons(sK59, nil)) | ~ ssList(cons(sK59, nil)) | ~ spl64_223), inference(resolution, [], [f4059, f501])).
fof(f501, plain, ! [X0] : (~ segmentP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f330])).
fof(f330, plain, ! [X0] : (((segmentP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ segmentP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((segmentP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0] : (ssList(X0) => (segmentP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC152+1.p', ax58)).
fof(f4059, plain, (segmentP(nil, cons(sK59, nil)) | ~ spl64_223), inference(avatar_component_clause, [], [f4057])).
fof(f3812, plain, (spl64_122 | spl64_185), inference(avatar_split_clause, [], [f3811, f3662, f2872])).
fof(f3811, plain, ((nil = sK53) | spl64_185), inference(subsumption_resolution, [], [f3805, f667])).
fof(f3805, plain, ((nil = sK53) | ~ ssList(sK53) | spl64_185), inference(resolution, [], [f3664, f451])).
fof(f3664, plain, (~ ssList(sK49(sK53)) | spl64_185), inference(avatar_component_clause, [], [f3662])).
fof(f3557, plain, (spl64_41 | spl64_150), inference(avatar_split_clause, [], [f3556, f3411, f1227])).
fof(f3556, plain, ((nil = sK62) | spl64_150), inference(subsumption_resolution, [], [f3552, f564])).
fof(f3552, plain, ((nil = sK62) | ~ ssList(sK62) | spl64_150), inference(resolution, [], [f3413, f451])).
fof(f3413, plain, (~ ssList(sK49(sK62)) | spl64_150), inference(avatar_component_clause, [], [f3411])).
fof(f2435, plain, (~ spl64_78 | spl64_84), inference(avatar_contradiction_clause, [], [f2434])).
fof(f2434, plain, ($false | (~ spl64_78 | spl64_84)), inference(subsumption_resolution, [], [f2433, f447])).
fof(f2433, plain, (~ ssList(nil) | (~ spl64_78 | spl64_84)), inference(subsumption_resolution, [], [f2432, f2099])).
fof(f2432, plain, (~ ssItem(sK59) | ~ ssList(nil) | spl64_84), inference(resolution, [], [f2423, f446])).
fof(f2423, plain, (~ ssList(cons(sK59, nil)) | spl64_84), inference(avatar_component_clause, [], [f2421])).
fof(f2102, plain, spl64_78, inference(avatar_split_clause, [], [f561, f2098])).
fof(f561, plain, ssItem(sK59), inference(cnf_transformation, [], [f354])).
fof(f1918, plain, (spl64_37 | spl64_53), inference(avatar_split_clause, [], [f1917, f1851, f1209])).
fof(f1917, plain, ((nil = sK60) | spl64_53), inference(subsumption_resolution, [], [f1904, f562])).
fof(f1904, plain, ((nil = sK60) | ~ ssList(sK60) | spl64_53), inference(resolution, [], [f1853, f451])).
fof(f1853, plain, (~ ssList(sK49(sK60)) | spl64_53), inference(avatar_component_clause, [], [f1851])).
fof(f634, plain, (spl64_5 | spl64_2), inference(avatar_split_clause, [], [f574, f616, f631])).
fof(f574, plain, ((nil = sK55) | ssItem(sK57)), inference(cnf_transformation, [], [f354])).
fof(f629, plain, (spl64_4 | spl64_2), inference(avatar_split_clause, [], [f575, f616, f626])).
fof(f575, plain, ((nil = sK55) | (sK55 = cons(sK57, nil))), inference(cnf_transformation, [], [f354])).