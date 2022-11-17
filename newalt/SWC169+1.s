fof(f542316, plain, $false, inference(avatar_sat_refutation, [], [f617, f622, f2099, f5819, f6770, f6984, f7319, f7324, f7335, f7345, f7485, f8865, f10808, f22094, f31119, f38779, f38920, f40715, f43109, f52620, f52634, f53941, f56284, f68052, f69041, f70239, f71992, f86271, f162571, f241987, f242015, f243347, f460890, f522615])).
fof(f522615, plain, (~ spl62_55 | ~ spl62_68 | spl62_2013), inference(avatar_contradiction_clause, [], [f522614])).
fof(f522614, plain, ($false | (~ spl62_55 | ~ spl62_68 | spl62_2013)), inference(subsumption_resolution, [], [f522613, f2148])).
fof(f2148, plain, ((nil = cons(sK58, nil)) | ~ spl62_55), inference(avatar_component_clause, [], [f2146])).
fof(f2146, plain, (spl62_55 <=> (nil = cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl62_55])])).
fof(f522613, plain, (~ (nil = cons(sK58, nil)) | (~ spl62_68 | spl62_2013)), inference(forward_demodulation, [], [f41513, f2365])).
fof(f2365, plain, ((nil = sK53) | ~ spl62_68), inference(avatar_component_clause, [], [f2363])).
fof(f2363, plain, (spl62_68 <=> (nil = sK53)), introduced(avatar_definition, [new_symbols(naming, [spl62_68])])).
fof(f41513, plain, (~ (nil = cons(sK58, sK53)) | spl62_2013), inference(avatar_component_clause, [], [f41512])).
fof(f41512, plain, (spl62_2013 <=> (nil = cons(sK58, sK53))), introduced(avatar_definition, [new_symbols(naming, [spl62_2013])])).
fof(f460890, plain, (spl62_1908 | ~ spl62_7188 | spl62_12460), inference(avatar_contradiction_clause, [], [f460889])).
fof(f460889, plain, ($false | (spl62_1908 | ~ spl62_7188 | spl62_12460)), inference(subsumption_resolution, [], [f460888, f169262])).
fof(f169262, plain, (ssList(sK12(sK53, sK61)) | ~ spl62_7188), inference(avatar_component_clause, [], [f169261])).
fof(f169261, plain, (spl62_7188 <=> ssList(sK12(sK53, sK61))), introduced(avatar_definition, [new_symbols(naming, [spl62_7188])])).
fof(f460888, plain, (~ ssList(sK12(sK53, sK61)) | (spl62_1908 | spl62_12460)), inference(subsumption_resolution, [], [f460887, f38872])).
fof(f38872, plain, (~ (nil = sK12(sK53, sK61)) | spl62_1908), inference(avatar_component_clause, [], [f38871])).
fof(f38871, plain, (spl62_1908 <=> (nil = sK12(sK53, sK61))), introduced(avatar_definition, [new_symbols(naming, [spl62_1908])])).
fof(f460887, plain, ((nil = sK12(sK53, sK61)) | ~ ssList(sK12(sK53, sK61)) | spl62_12460), inference(resolution, [], [f243278, f455])).
fof(f455, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax24)).
fof(f243278, plain, (~ ssList(tl(sK12(sK53, sK61))) | spl62_12460), inference(avatar_component_clause, [], [f243276])).
fof(f243276, plain, (spl62_12460 <=> ssList(tl(sK12(sK53, sK61)))), introduced(avatar_definition, [new_symbols(naming, [spl62_12460])])).
fof(f243347, plain, (~ spl62_12460 | spl62_25 | ~ spl62_243 | ~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498 | ~ spl62_1904 | ~ spl62_7189), inference(avatar_split_clause, [], [f243346, f169265, f38750, f7434, f7011, f6779, f6278, f4104, f1147, f243276])).
fof(f1147, plain, (spl62_25 <=> (nil = sK61)), introduced(avatar_definition, [new_symbols(naming, [spl62_25])])).
fof(f4104, plain, (spl62_243 <=> (nil = tl(sK53))), introduced(avatar_definition, [new_symbols(naming, [spl62_243])])).
fof(f6278, plain, (spl62_414 <=> (sK57 = sK58)), introduced(avatar_definition, [new_symbols(naming, [spl62_414])])).
fof(f6779, plain, (spl62_432 <=> ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl62_432])])).
fof(f7011, plain, (spl62_457 <=> (sK53 = cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl62_457])])).
fof(f7434, plain, (spl62_498 <=> (nil = app(app(sK60, cons(sK58, nil)), cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl62_498])])).
fof(f38750, plain, (spl62_1904 <=> ssList(sK61)), introduced(avatar_definition, [new_symbols(naming, [spl62_1904])])).
fof(f169265, plain, (spl62_7189 <=> (sK12(sK53, sK61) = app(app(sK60, sK53), sK53))), introduced(avatar_definition, [new_symbols(naming, [spl62_7189])])).
fof(f243346, plain, (~ ssList(tl(sK12(sK53, sK61))) | (spl62_25 | ~ spl62_243 | ~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498 | ~ spl62_1904 | ~ spl62_7189)), inference(subsumption_resolution, [], [f243345, f1148])).
fof(f1148, plain, (~ (nil = sK61) | spl62_25), inference(avatar_component_clause, [], [f1147])).
fof(f243345, plain, ((nil = sK61) | ~ ssList(tl(sK12(sK53, sK61))) | (~ spl62_243 | ~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498 | ~ spl62_1904 | ~ spl62_7189)), inference(subsumption_resolution, [], [f243344, f38751])).
fof(f38751, plain, (ssList(sK61) | ~ spl62_1904), inference(avatar_component_clause, [], [f38750])).
fof(f243344, plain, (~ ssList(sK61) | (nil = sK61) | ~ ssList(tl(sK12(sK53, sK61))) | (~ spl62_243 | ~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498 | ~ spl62_7189)), inference(subsumption_resolution, [], [f243254, f169670])).
fof(f169670, plain, rearsegP(sK61, nil), inference(subsumption_resolution, [], [f169669, f445])).
fof(f445, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax17)).
fof(f169669, plain, (rearsegP(sK61, nil) | ~ ssList(nil)), inference(duplicate_literal_removal, [], [f169666])).
fof(f169666, plain, (rearsegP(sK61, nil) | ~ ssList(nil) | ~ ssList(nil)), inference(resolution, [], [f3042, f489])).
fof(f489, plain, ! [X0] : (rearsegP(X0, X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f164])).
fof(f164, plain, ! [X0] : (rearsegP(X0, X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (ssList(X0) => rearsegP(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax49)).
fof(f3042, plain, ! [X4] : (~ rearsegP(nil, X4) | rearsegP(sK61, X4) | ~ ssList(X4)), inference(subsumption_resolution, [], [f3041, f445])).
fof(f3041, plain, ! [X4] : (rearsegP(sK61, X4) | ~ rearsegP(nil, X4) | ~ ssList(X4) | ~ ssList(nil)), inference(subsumption_resolution, [], [f3025, f561])).
fof(f561, plain, ssList(sK61), inference(cnf_transformation, [], [f352])).
fof(f352, plain, (((((((nil = sK55) & (nil = sK56)) | (memberP(sK56, sK57) & (sK55 = cons(sK57, nil)) & ssItem(sK57))) & ((((~ neq(sK58, sK59) & (sK53 = app(app(app(sK60, cons(sK58, nil)), cons(sK59, nil)), sK61)) & ssList(sK61)) & ssList(sK60)) & ssItem(sK59)) & ssItem(sK58)) & (sK53 = sK55) & (sK54 = sK56) & ssList(sK56)) & ssList(sK55)) & ssList(sK54)) & ssList(sK53)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK53, sK54, sK55, sK56, sK57, sK58, sK59, sK60, sK61])], [f222, f351, f350, f349, f348, f347, f346, f345, f344, f343])).
fof(f343, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = X0) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = sK53) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (sK53 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK53))), introduced(choice_axiom, [])).
fof(f344, plain, (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = sK53) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (sK53 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = sK53) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (sK53 = X2) & (sK54 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK54))), introduced(choice_axiom, [])).
fof(f345, plain, (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = sK53) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (sK53 = X2) & (sK54 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((((nil = sK55) & (nil = X3)) | ? [X4] : (memberP(X3, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = sK53) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (sK53 = sK55) & (sK54 = X3) & ssList(X3)) & ssList(sK55))), introduced(choice_axiom, [])).
fof(f346, plain, (? [X3] : ((((nil = sK55) & (nil = X3)) | ? [X4] : (memberP(X3, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = sK53) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (sK53 = sK55) & (sK54 = X3) & ssList(X3)) => ((((nil = sK55) & (nil = sK56)) | ? [X4] : (memberP(sK56, X4) & (cons(X4, nil) = sK55) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = sK53) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (sK53 = sK55) & (sK54 = sK56) & ssList(sK56))), introduced(choice_axiom, [])).
fof(f347, plain, (? [X4] : (memberP(sK56, X4) & (cons(X4, nil) = sK55) & ssItem(X4)) => (memberP(sK56, sK57) & (sK55 = cons(sK57, nil)) & ssItem(sK57))), introduced(choice_axiom, [])).
fof(f348, plain, (? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = sK53) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) => (? [X6] : (? [X7] : (? [X8] : (~ neq(sK58, X6) & (sK53 = app(app(app(X7, cons(sK58, nil)), cons(X6, nil)), X8)) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(sK58))), introduced(choice_axiom, [])).
fof(f349, plain, (? [X6] : (? [X7] : (? [X8] : (~ neq(sK58, X6) & (sK53 = app(app(app(X7, cons(sK58, nil)), cons(X6, nil)), X8)) & ssList(X8)) & ssList(X7)) & ssItem(X6)) => (? [X7] : (? [X8] : (~ neq(sK58, sK59) & (sK53 = app(app(app(X7, cons(sK58, nil)), cons(sK59, nil)), X8)) & ssList(X8)) & ssList(X7)) & ssItem(sK59))), introduced(choice_axiom, [])).
fof(f350, plain, (? [X7] : (? [X8] : (~ neq(sK58, sK59) & (sK53 = app(app(app(X7, cons(sK58, nil)), cons(sK59, nil)), X8)) & ssList(X8)) & ssList(X7)) => (? [X8] : (~ neq(sK58, sK59) & (sK53 = app(app(app(sK60, cons(sK58, nil)), cons(sK59, nil)), X8)) & ssList(X8)) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f351, plain, (? [X8] : (~ neq(sK58, sK59) & (sK53 = app(app(app(sK60, cons(sK58, nil)), cons(sK59, nil)), X8)) & ssList(X8)) => (~ neq(sK58, sK59) & (sK53 = app(app(app(sK60, cons(sK58, nil)), cons(sK59, nil)), sK61)) & ssList(sK61))), introduced(choice_axiom, [])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (memberP(X3, X4) & (cons(X4, nil) = X2) & ssItem(X4))) & ? [X5] : (? [X6] : (? [X7] : (? [X8] : (~ neq(X5, X6) & (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = X0) & ssList(X8)) & ssList(X7)) & ssItem(X6)) & ssItem(X5)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (~ memberP(X3, X4) | ~ (cons(X4, nil) = X2) | ~ ssItem(X4))) | ! [X5] : (ssItem(X5) => ! [X6] : (ssItem(X6) => ! [X7] : (ssList(X7) => ! [X8] : (neq(X5, X6) | ~ (app(app(app(X7, cons(X5, nil)), cons(X6, nil)), X8) = X0) | ~ ssList(X8))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((~ (nil = X2) | ~ (nil = X3)) & ! [X8] : (~ memberP(X3, X8) | ~ (cons(X8, nil) = X2) | ~ ssItem(X8))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (neq(X4, X5) | ~ (app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = X0) | ~ ssList(X7))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (((~ (nil = X2) | ~ (nil = X3)) & ! [X8] : (~ memberP(X3, X8) | ~ (cons(X8, nil) = X2) | ~ ssItem(X8))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (neq(X4, X5) | ~ (app(app(app(X6, cons(X4, nil)), cons(X5, nil)), X7) = X0) | ~ ssList(X7))))) | ~ (X0 = X2) | ~ (X1 = X3) | ~ ssList(X3))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', co1)).
fof(f3025, plain, ! [X4] : (rearsegP(sK61, X4) | ~ rearsegP(nil, X4) | ~ ssList(sK61) | ~ ssList(X4) | ~ ssList(nil)), inference(superposition, [], [f490, f888])).
fof(f888, plain, (sK61 = app(sK61, nil)), inference(resolution, [], [f538, f561])).
fof(f538, plain, ! [X0] : (~ ssList(X0) | (app(X0, nil) = X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : ((app(X0, nil) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (ssList(X0) => (app(X0, nil) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax84)).
fof(f490, plain, ! [X2, X0, X1] : (rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : (! [X1] : (! [X2] : (rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f165])).
fof(f165, plain, ! [X0] : (! [X1] : (! [X2] : ((rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f50])).
fof(f50, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (rearsegP(X0, X1) => rearsegP(app(X2, X0), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax50)).
fof(f243254, plain, (~ rearsegP(sK61, nil) | ~ ssList(sK61) | (nil = sK61) | ~ ssList(tl(sK12(sK53, sK61))) | (~ spl62_243 | ~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498 | ~ spl62_7189)), inference(superposition, [], [f1579, f242173])).
fof(f242173, plain, ((nil = app(tl(sK12(sK53, sK61)), sK61)) | (~ spl62_243 | ~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498 | ~ spl62_7189)), inference(backward_demodulation, [], [f165723, f169267])).
fof(f169267, plain, ((sK12(sK53, sK61) = app(app(sK60, sK53), sK53)) | ~ spl62_7189), inference(avatar_component_clause, [], [f169265])).
fof(f165723, plain, ((nil = app(tl(app(app(sK60, sK53), sK53)), sK61)) | (~ spl62_243 | ~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498)), inference(forward_demodulation, [], [f165722, f4106])).
fof(f4106, plain, ((nil = tl(sK53)) | ~ spl62_243), inference(avatar_component_clause, [], [f4104])).
fof(f165722, plain, ((tl(sK53) = app(tl(app(app(sK60, sK53), sK53)), sK61)) | (~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498)), inference(forward_demodulation, [], [f165721, f162592])).
fof(f162592, plain, ((sK53 = app(app(app(sK60, sK53), sK53), sK61)) | (~ spl62_414 | ~ spl62_457)), inference(forward_demodulation, [], [f162591, f72128])).
fof(f72128, plain, ((sK53 = cons(sK57, nil)) | (~ spl62_414 | ~ spl62_457)), inference(backward_demodulation, [], [f7013, f6280])).
fof(f6280, plain, ((sK57 = sK58) | ~ spl62_414), inference(avatar_component_clause, [], [f6278])).
fof(f7013, plain, ((sK53 = cons(sK58, nil)) | ~ spl62_457), inference(avatar_component_clause, [], [f7011])).
fof(f162591, plain, ((sK53 = app(app(app(sK60, cons(sK57, nil)), cons(sK57, nil)), sK61)) | ~ spl62_414), inference(forward_demodulation, [], [f2316, f6280])).
fof(f2316, plain, (sK53 = app(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)), sK61)), inference(forward_demodulation, [], [f562, f1010])).
fof(f1010, plain, (sK58 = sK59), inference(subsumption_resolution, [], [f1009, f558])).
fof(f558, plain, ssItem(sK58), inference(cnf_transformation, [], [f352])).
fof(f1009, plain, ((sK58 = sK59) | ~ ssItem(sK58)), inference(subsumption_resolution, [], [f1004, f559])).
fof(f559, plain, ssItem(sK59), inference(cnf_transformation, [], [f352])).
fof(f1004, plain, ((sK58 = sK59) | ~ ssItem(sK59) | ~ ssItem(sK58)), inference(resolution, [], [f354, f563])).
fof(f563, plain, ~ neq(sK58, sK59), inference(cnf_transformation, [], [f352])).
fof(f354, plain, ! [X0, X1] : (neq(X0, X1) | (X0 = X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f232])).
fof(f232, plain, ! [X0] : (! [X1] : (((neq(X0, X1) | (X0 = X1)) & (~ (X0 = X1) | ~ neq(X0, X1))) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f99])).
fof(f99, plain, ! [X0] : (! [X1] : ((neq(X0, X1) <=> ~ (X0 = X1)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (neq(X0, X1) <=> ~ (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax1)).
fof(f562, plain, (sK53 = app(app(app(sK60, cons(sK58, nil)), cons(sK59, nil)), sK61)), inference(cnf_transformation, [], [f352])).
fof(f165721, plain, ((tl(app(app(app(sK60, sK53), sK53), sK61)) = app(tl(app(app(sK60, sK53), sK53)), sK61)) | (~ spl62_414 | ~ spl62_432 | ~ spl62_457 | spl62_498)), inference(subsumption_resolution, [], [f165662, f71198])).
fof(f71198, plain, (~ (nil = app(app(sK60, sK53), sK53)) | (~ spl62_457 | spl62_498)), inference(backward_demodulation, [], [f7435, f7013])).
fof(f7435, plain, (~ (nil = app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | spl62_498), inference(avatar_component_clause, [], [f7434])).
fof(f165662, plain, ((tl(app(app(app(sK60, sK53), sK53), sK61)) = app(tl(app(app(sK60, sK53), sK53)), sK61)) | (nil = app(app(sK60, sK53), sK53)) | (~ spl62_414 | ~ spl62_432 | ~ spl62_457)), inference(resolution, [], [f2254, f162756])).
fof(f162756, plain, (ssList(app(app(sK60, sK53), sK53)) | (~ spl62_414 | ~ spl62_432 | ~ spl62_457)), inference(forward_demodulation, [], [f162755, f72128])).
fof(f162755, plain, (ssList(app(app(sK60, cons(sK57, nil)), cons(sK57, nil))) | (~ spl62_414 | ~ spl62_432)), inference(forward_demodulation, [], [f6780, f6280])).
fof(f6780, plain, (ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ spl62_432), inference(avatar_component_clause, [], [f6779])).
fof(f2254, plain, ! [X8] : (~ ssList(X8) | (tl(app(X8, sK61)) = app(tl(X8), sK61)) | (nil = X8)), inference(resolution, [], [f561, f540])).
fof(f540, plain, ! [X0, X1] : (~ ssList(X1) | (nil = X0) | (tl(app(X0, X1)) = app(tl(X0), X1)) | ~ ssList(X0)), inference(cnf_transformation, [], [f206])).
fof(f206, plain, ! [X0] : (! [X1] : ((tl(app(X0, X1)) = app(tl(X0), X1)) | (nil = X0) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f205])).
fof(f205, plain, ! [X0] : (! [X1] : (((tl(app(X0, X1)) = app(tl(X0), X1)) | (nil = X0)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f86])).
fof(f86, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (~ (nil = X0) => (tl(app(X0, X1)) = app(tl(X0), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax86)).
fof(f1579, plain, ! [X0, X1] : (~ rearsegP(X1, app(X0, X1)) | ~ ssList(X1) | (app(X0, X1) = X1) | ~ ssList(X0)), inference(subsumption_resolution, [], [f1578, f457])).
fof(f457, plain, ! [X0, X1] : (ssList(app(X0, X1)) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : (ssList(app(X0, X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ssList(app(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax26)).
fof(f1578, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X0, X1)) | (app(X0, X1) = X1) | ~ rearsegP(X1, app(X0, X1))), inference(duplicate_literal_removal, [], [f1570])).
fof(f1570, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X0, X1)) | (app(X0, X1) = X1) | ~ rearsegP(X1, app(X0, X1)) | ~ ssList(app(X0, X1)) | ~ ssList(X1)), inference(resolution, [], [f574, f488])).
fof(f488, plain, ! [X0, X1] : (~ rearsegP(X1, X0) | (X0 = X1) | ~ rearsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f163])).
fof(f163, plain, ! [X0] : (! [X1] : ((X0 = X1) | ~ rearsegP(X1, X0) | ~ rearsegP(X0, X1) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f162])).
fof(f162, plain, ! [X0] : (! [X1] : (((X0 = X1) | (~ rearsegP(X1, X0) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f48])).
fof(f48, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((rearsegP(X1, X0) & rearsegP(X0, X1)) => (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax48)).
fof(f574, plain, ! [X2, X1] : (rearsegP(app(X2, X1), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X2, X1))), inference(equality_resolution, [], [f370])).
fof(f370, plain, ! [X2, X0, X1] : (rearsegP(X0, X1) | ~ (app(X2, X1) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f252])).
fof(f252, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (((app(sK12(X0, X1), X1) = X0) & ssList(sK12(X0, X1))) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f250, f251])).
fof(f251, plain, ! [X1, X0] : (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) => ((app(sK12(X0, X1), X1) = X0) & ssList(sK12(X0, X1)))), introduced(choice_axiom, [])).
fof(f250, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f249])).
fof(f249, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X2, X1) = X0) & ssList(X2)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f103])).
fof(f103, plain, ! [X0] : (! [X1] : ((rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax6)).
fof(f242015, plain, (~ spl62_7188 | spl62_7189 | ~ spl62_414 | ~ spl62_439 | ~ spl62_442 | ~ spl62_457), inference(avatar_split_clause, [], [f173028, f7011, f6835, f6813, f6278, f169265, f169261])).
fof(f6813, plain, (spl62_439 <=> ! [X8] : (~ (sK53 = app(X8, sK61)) | ~ ssList(X8) | (app(app(sK60, cons(sK58, nil)), cons(sK58, nil)) = X8))), introduced(avatar_definition, [new_symbols(naming, [spl62_439])])).
fof(f6835, plain, (spl62_442 <=> rearsegP(sK53, sK61)), introduced(avatar_definition, [new_symbols(naming, [spl62_442])])).
fof(f173028, plain, ((sK12(sK53, sK61) = app(app(sK60, sK53), sK53)) | ~ ssList(sK12(sK53, sK61)) | (~ spl62_414 | ~ spl62_439 | ~ spl62_442 | ~ spl62_457)), inference(trivial_inequality_removal, [], [f173027])).
fof(f173027, plain, (~ (sK53 = sK53) | (sK12(sK53, sK61) = app(app(sK60, sK53), sK53)) | ~ ssList(sK12(sK53, sK61)) | (~ spl62_414 | ~ spl62_439 | ~ spl62_442 | ~ spl62_457)), inference(superposition, [], [f162607, f7354])).
fof(f7354, plain, ((sK53 = app(sK12(sK53, sK61), sK61)) | ~ spl62_442), inference(subsumption_resolution, [], [f7353, f631])).
fof(f631, plain, ssList(sK53), inference(forward_demodulation, [], [f554, f557])).
fof(f557, plain, (sK53 = sK55), inference(cnf_transformation, [], [f352])).
fof(f554, plain, ssList(sK55), inference(cnf_transformation, [], [f352])).
fof(f7353, plain, ((sK53 = app(sK12(sK53, sK61), sK61)) | ~ ssList(sK53) | ~ spl62_442), inference(subsumption_resolution, [], [f7349, f561])).
fof(f7349, plain, ((sK53 = app(sK12(sK53, sK61), sK61)) | ~ ssList(sK61) | ~ ssList(sK53) | ~ spl62_442), inference(resolution, [], [f6837, f369])).
fof(f369, plain, ! [X0, X1] : (~ rearsegP(X0, X1) | (app(sK12(X0, X1), X1) = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f252])).
fof(f6837, plain, (rearsegP(sK53, sK61) | ~ spl62_442), inference(avatar_component_clause, [], [f6835])).
fof(f162607, plain, (! [X8] : (~ (sK53 = app(X8, sK61)) | (app(app(sK60, sK53), sK53) = X8) | ~ ssList(X8)) | (~ spl62_414 | ~ spl62_439 | ~ spl62_457)), inference(forward_demodulation, [], [f162606, f72128])).
fof(f162606, plain, (! [X8] : ((app(app(sK60, cons(sK57, nil)), cons(sK57, nil)) = X8) | ~ (sK53 = app(X8, sK61)) | ~ ssList(X8)) | (~ spl62_414 | ~ spl62_439)), inference(forward_demodulation, [], [f6814, f6280])).
fof(f6814, plain, (! [X8] : (~ (sK53 = app(X8, sK61)) | ~ ssList(X8) | (app(app(sK60, cons(sK58, nil)), cons(sK58, nil)) = X8)) | ~ spl62_439), inference(avatar_component_clause, [], [f6813])).
fof(f241987, plain, (~ spl62_442 | ~ spl62_1904 | ~ spl62_2985 | spl62_7188), inference(avatar_contradiction_clause, [], [f241986])).
fof(f241986, plain, ($false | (~ spl62_442 | ~ spl62_1904 | ~ spl62_2985 | spl62_7188)), inference(subsumption_resolution, [], [f241985, f68241])).
fof(f68241, plain, (ssList(sK53) | ~ spl62_2985), inference(avatar_component_clause, [], [f68240])).
fof(f68240, plain, (spl62_2985 <=> ssList(sK53)), introduced(avatar_definition, [new_symbols(naming, [spl62_2985])])).
fof(f241985, plain, (~ ssList(sK53) | (~ spl62_442 | ~ spl62_1904 | spl62_7188)), inference(subsumption_resolution, [], [f241984, f38751])).
fof(f241984, plain, (~ ssList(sK61) | ~ ssList(sK53) | (~ spl62_442 | spl62_7188)), inference(subsumption_resolution, [], [f241983, f6837])).
fof(f241983, plain, (~ rearsegP(sK53, sK61) | ~ ssList(sK61) | ~ ssList(sK53) | spl62_7188), inference(resolution, [], [f169263, f368])).
fof(f368, plain, ! [X0, X1] : (ssList(sK12(X0, X1)) | ~ rearsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f252])).
fof(f169263, plain, (~ ssList(sK12(sK53, sK61)) | spl62_7188), inference(avatar_component_clause, [], [f169261])).
fof(f162571, plain, (~ spl62_457 | ~ spl62_1420 | ~ spl62_1905 | ~ spl62_3113 | ~ spl62_3477), inference(avatar_contradiction_clause, [], [f162570])).
fof(f162570, plain, ($false | (~ spl62_457 | ~ spl62_1420 | ~ spl62_1905 | ~ spl62_3113 | ~ spl62_3477)), inference(subsumption_resolution, [], [f162569, f86251])).
fof(f86251, plain, (ssList(sK60) | ~ spl62_3477), inference(avatar_component_clause, [], [f86250])).
fof(f86250, plain, (spl62_3477 <=> ssList(sK60)), introduced(avatar_definition, [new_symbols(naming, [spl62_3477])])).
fof(f162569, plain, (~ ssList(sK60) | (~ spl62_457 | ~ spl62_1420 | ~ spl62_1905 | ~ spl62_3113)), inference(subsumption_resolution, [], [f162560, f68968])).
fof(f68968, plain, (sP4(sK53) | ~ spl62_3113), inference(avatar_component_clause, [], [f68966])).
fof(f68966, plain, (spl62_3113 <=> sP4(sK53)), introduced(avatar_definition, [new_symbols(naming, [spl62_3113])])).
fof(f162560, plain, (~ sP4(sK53) | ~ ssList(sK60) | (~ spl62_457 | ~ spl62_1420 | ~ spl62_1905)), inference(superposition, [], [f31118, f71362])).
fof(f71362, plain, ((sK53 = app(app(sK60, sK53), sK53)) | (~ spl62_457 | ~ spl62_1905)), inference(backward_demodulation, [], [f38820, f7013])).
fof(f38820, plain, ((sK53 = app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ spl62_1905), inference(avatar_component_clause, [], [f38818])).
fof(f38818, plain, (spl62_1905 <=> (sK53 = app(app(sK60, cons(sK58, nil)), cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl62_1905])])).
fof(f31118, plain, (! [X1] : (~ sP4(app(app(X1, sK53), sK53)) | ~ ssList(X1)) | ~ spl62_1420), inference(avatar_component_clause, [], [f31117])).
fof(f31117, plain, (spl62_1420 <=> ! [X1] : (~ sP4(app(app(X1, sK53), sK53)) | ~ ssList(X1))), introduced(avatar_definition, [new_symbols(naming, [spl62_1420])])).
fof(f86271, plain, spl62_3477, inference(avatar_split_clause, [], [f560, f86250])).
fof(f560, plain, ssList(sK60), inference(cnf_transformation, [], [f352])).
fof(f71992, plain, (spl62_3113 | ~ spl62_457 | ~ spl62_527), inference(avatar_split_clause, [], [f71204, f8861, f7011, f68966])).
fof(f8861, plain, (spl62_527 <=> sP4(cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl62_527])])).
fof(f71204, plain, (sP4(sK53) | (~ spl62_457 | ~ spl62_527)), inference(backward_demodulation, [], [f8862, f7013])).
fof(f8862, plain, (sP4(cons(sK58, nil)) | ~ spl62_527), inference(avatar_component_clause, [], [f8861])).
fof(f70239, plain, (spl62_1905 | ~ spl62_25 | ~ spl62_432), inference(avatar_split_clause, [], [f70238, f6779, f1147, f38818])).
fof(f70238, plain, ((sK53 = app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | (~ spl62_25 | ~ spl62_432)), inference(backward_demodulation, [], [f7381, f70237])).
fof(f70237, plain, ((sK53 = app(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)), nil)) | ~ spl62_25), inference(forward_demodulation, [], [f2316, f1149])).
fof(f1149, plain, ((nil = sK61) | ~ spl62_25), inference(avatar_component_clause, [], [f1147])).
fof(f7381, plain, ((app(app(sK60, cons(sK58, nil)), cons(sK58, nil)) = app(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)), nil)) | ~ spl62_432), inference(resolution, [], [f6780, f538])).
fof(f69041, plain, spl62_2985, inference(avatar_split_clause, [], [f631, f68240])).
fof(f68052, plain, (~ spl62_47 | spl62_55 | ~ spl62_68 | ~ spl62_430), inference(avatar_contradiction_clause, [], [f68051])).
fof(f68051, plain, ($false | (~ spl62_47 | spl62_55 | ~ spl62_68 | ~ spl62_430)), inference(subsumption_resolution, [], [f68050, f2086])).
fof(f2086, plain, (ssList(cons(sK58, nil)) | ~ spl62_47), inference(avatar_component_clause, [], [f2085])).
fof(f2085, plain, (spl62_47 <=> ssList(cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl62_47])])).
fof(f68050, plain, (~ ssList(cons(sK58, nil)) | (spl62_55 | ~ spl62_68 | ~ spl62_430)), inference(subsumption_resolution, [], [f68020, f2147])).
fof(f2147, plain, (~ (nil = cons(sK58, nil)) | spl62_55), inference(avatar_component_clause, [], [f2146])).
fof(f68020, plain, ((nil = cons(sK58, nil)) | ~ ssList(cons(sK58, nil)) | (~ spl62_68 | ~ spl62_430)), inference(resolution, [], [f63195, f499])).
fof(f499, plain, ! [X0] : (~ segmentP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f330])).
fof(f330, plain, ! [X0] : (((segmentP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ segmentP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((segmentP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0] : (ssList(X0) => (segmentP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax58)).
fof(f63195, plain, (segmentP(nil, cons(sK58, nil)) | (~ spl62_68 | ~ spl62_430)), inference(forward_demodulation, [], [f6769, f2365])).
fof(f6769, plain, (segmentP(sK53, cons(sK58, nil)) | ~ spl62_430), inference(avatar_component_clause, [], [f6767])).
fof(f6767, plain, (spl62_430 <=> segmentP(sK53, cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl62_430])])).
fof(f56284, plain, (spl62_68 | ~ spl62_2), inference(avatar_split_clause, [], [f56281, f609, f2363])).
fof(f609, plain, (spl62_2 <=> (nil = sK55)), introduced(avatar_definition, [new_symbols(naming, [spl62_2])])).
fof(f56281, plain, ((nil = sK53) | ~ spl62_2), inference(backward_demodulation, [], [f557, f611])).
fof(f611, plain, ((nil = sK55) | ~ spl62_2), inference(avatar_component_clause, [], [f609])).
fof(f53941, plain, (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_414 | ~ spl62_429), inference(avatar_contradiction_clause, [], [f53940])).
fof(f53940, plain, ($false | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_414 | ~ spl62_429)), inference(subsumption_resolution, [], [f53758, f53753])).
fof(f53753, plain, (gt(sK57, sK57) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_414 | ~ spl62_429)), inference(backward_demodulation, [], [f40185, f6280])).
fof(f40185, plain, (gt(sK57, sK58) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(subsumption_resolution, [], [f40184, f621])).
fof(f621, plain, (ssItem(sK57) | ~ spl62_4), inference(avatar_component_clause, [], [f619])).
fof(f619, plain, (spl62_4 <=> ssItem(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl62_4])])).
fof(f40184, plain, (gt(sK57, sK58) | ~ ssItem(sK57) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(subsumption_resolution, [], [f40178, f558])).
fof(f40178, plain, (gt(sK57, sK58) | ~ ssItem(sK58) | ~ ssItem(sK57) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(resolution, [], [f39333, f468])).
fof(f468, plain, ! [X0, X1] : (~ lt(X1, X0) | gt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f321])).
fof(f321, plain, ! [X0] : (! [X1] : (((gt(X0, X1) | ~ lt(X1, X0)) & (lt(X1, X0) | ~ gt(X0, X1))) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f146])).
fof(f146, plain, ! [X0] : (! [X1] : ((gt(X0, X1) <=> lt(X1, X0)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (gt(X0, X1) <=> lt(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax35)).
fof(f39333, plain, (lt(sK58, sK57) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(subsumption_resolution, [], [f39332, f631])).
fof(f39332, plain, (lt(sK58, sK57) | ~ ssList(sK53) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(subsumption_resolution, [], [f39331, f558])).
fof(f39331, plain, (~ ssItem(sK58) | lt(sK58, sK57) | ~ ssList(sK53) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(subsumption_resolution, [], [f39330, f6764])).
fof(f6764, plain, (ssList(app(sK60, cons(sK58, nil))) | ~ spl62_429), inference(avatar_component_clause, [], [f6763])).
fof(f6763, plain, (spl62_429 <=> ssList(app(sK60, cons(sK58, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl62_429])])).
fof(f39330, plain, (~ ssList(app(sK60, cons(sK58, nil))) | ~ ssItem(sK58) | lt(sK58, sK57) | ~ ssList(sK53) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98)), inference(subsumption_resolution, [], [f39329, f445])).
fof(f39329, plain, (~ ssList(nil) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssItem(sK58) | lt(sK58, sK57) | ~ ssList(sK53) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98)), inference(subsumption_resolution, [], [f39281, f2618])).
fof(f2618, plain, (strictorderedP(sK53) | (~ spl62_3 | ~ spl62_4)), inference(subsumption_resolution, [], [f2585, f621])).
fof(f2585, plain, (strictorderedP(sK53) | ~ ssItem(sK57) | ~ spl62_3), inference(superposition, [], [f514, f2320])).
fof(f2320, plain, ((sK53 = cons(sK57, nil)) | ~ spl62_3), inference(forward_demodulation, [], [f616, f557])).
fof(f616, plain, ((sK55 = cons(sK57, nil)) | ~ spl62_3), inference(avatar_component_clause, [], [f614])).
fof(f614, plain, (spl62_3 <=> (sK55 = cons(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl62_3])])).
fof(f514, plain, ! [X0] : (strictorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f183])).
fof(f183, plain, ! [X0] : (strictorderedP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f68])).
fof(f68, plain, ! [X0] : (ssItem(X0) => strictorderedP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax68)).
fof(f39281, plain, (~ strictorderedP(sK53) | ~ ssList(nil) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssItem(sK58) | lt(sK58, sK57) | ~ ssList(sK53) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98)), inference(superposition, [], [f2887, f39048])).
fof(f39048, plain, ((sK53 = app(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)), sK53)) | ~ spl62_98), inference(backward_demodulation, [], [f2316, f2924])).
fof(f2924, plain, ((sK53 = sK61) | ~ spl62_98), inference(avatar_component_clause, [], [f2922])).
fof(f2922, plain, (spl62_98 <=> (sK53 = sK61)), introduced(avatar_definition, [new_symbols(naming, [spl62_98])])).
fof(f2887, plain, (! [X2, X0, X1] : (~ strictorderedP(app(app(X0, cons(X1, X2)), sK53)) | ~ ssList(X2) | ~ ssList(X0) | ~ ssItem(X1) | lt(X1, sK57) | ~ ssList(app(app(X0, cons(X1, X2)), sK53))) | (~ spl62_3 | ~ spl62_4)), inference(subsumption_resolution, [], [f2886, f621])).
fof(f2886, plain, (! [X2, X0, X1] : (~ strictorderedP(app(app(X0, cons(X1, X2)), sK53)) | ~ ssList(X2) | ~ ssList(X0) | ~ ssItem(sK57) | ~ ssItem(X1) | lt(X1, sK57) | ~ ssList(app(app(X0, cons(X1, X2)), sK53))) | ~ spl62_3), inference(subsumption_resolution, [], [f2883, f445])).
fof(f2883, plain, (! [X2, X0, X1] : (~ strictorderedP(app(app(X0, cons(X1, X2)), sK53)) | ~ ssList(nil) | ~ ssList(X2) | ~ ssList(X0) | ~ ssItem(sK57) | ~ ssItem(X1) | lt(X1, sK57) | ~ ssList(app(app(X0, cons(X1, X2)), sK53))) | ~ spl62_3), inference(superposition, [], [f580, f2320])).
fof(f580, plain, ! [X6, X10, X8, X7, X9] : (~ strictorderedP(app(app(X8, cons(X6, X9)), cons(X7, X10))) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | lt(X6, X7) | ~ ssList(app(app(X8, cons(X6, X9)), cons(X7, X10)))), inference(equality_resolution, [], [f419])).
fof(f419, plain, ! [X6, X0, X10, X8, X7, X9] : (lt(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ strictorderedP(X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f300])).
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
fof(f12, plain, ! [X0] : (ssList(X0) => (strictorderedP(X0) <=> ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssList(X3) => ! [X4] : (ssList(X4) => ! [X5] : (ssList(X5) => ((app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) => lt(X1, X2))))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax12)).
fof(f53758, plain, (~ gt(sK57, sK57) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_414 | ~ spl62_429)), inference(backward_demodulation, [], [f40243, f6280])).
fof(f40243, plain, (~ gt(sK58, sK57) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(subsumption_resolution, [], [f40242, f558])).
fof(f40242, plain, (~ gt(sK58, sK57) | ~ ssItem(sK58) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(subsumption_resolution, [], [f40238, f621])).
fof(f40238, plain, (~ gt(sK58, sK57) | ~ ssItem(sK57) | ~ ssItem(sK58) | (~ spl62_3 | ~ spl62_4 | ~ spl62_98 | ~ spl62_429)), inference(resolution, [], [f40185, f550])).
fof(f550, plain, ! [X0, X1] : (~ gt(X1, X0) | ~ gt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f219])).
fof(f219, plain, ! [X0] : (! [X1] : (~ gt(X1, X0) | ~ gt(X0, X1) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f218])).
fof(f218, plain, ! [X0] : (! [X1] : ((~ gt(X1, X0) | ~ gt(X0, X1)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f94])).
fof(f94, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (gt(X0, X1) => ~ gt(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax94)).
fof(f52634, plain, (~ spl62_456 | spl62_457 | ~ spl62_47 | ~ spl62_430), inference(avatar_split_clause, [], [f52633, f6767, f2085, f7011, f7007])).
fof(f7007, plain, (spl62_456 <=> segmentP(cons(sK58, nil), sK53)), introduced(avatar_definition, [new_symbols(naming, [spl62_456])])).
fof(f52633, plain, ((sK53 = cons(sK58, nil)) | ~ segmentP(cons(sK58, nil), sK53) | (~ spl62_47 | ~ spl62_430)), inference(subsumption_resolution, [], [f52632, f2086])).
fof(f52632, plain, ((sK53 = cons(sK58, nil)) | ~ segmentP(cons(sK58, nil), sK53) | ~ ssList(cons(sK58, nil)) | ~ spl62_430), inference(subsumption_resolution, [], [f22703, f631])).
fof(f22703, plain, ((sK53 = cons(sK58, nil)) | ~ segmentP(cons(sK58, nil), sK53) | ~ ssList(sK53) | ~ ssList(cons(sK58, nil)) | ~ spl62_430), inference(resolution, [], [f6769, f495])).
fof(f495, plain, ! [X0, X1] : (~ segmentP(X1, X0) | (X0 = X1) | ~ segmentP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f172])).
fof(f172, plain, ! [X0] : (! [X1] : ((X0 = X1) | ~ segmentP(X1, X0) | ~ segmentP(X0, X1) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f171])).
fof(f171, plain, ! [X0] : (! [X1] : (((X0 = X1) | (~ segmentP(X1, X0) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f54])).
fof(f54, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((segmentP(X1, X0) & segmentP(X0, X1)) => (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax54)).
fof(f52620, plain, (spl62_414 | ~ spl62_3 | ~ spl62_4 | ~ spl62_429 | ~ spl62_432 | ~ spl62_434), inference(avatar_split_clause, [], [f52619, f6788, f6779, f6763, f619, f614, f6278])).
fof(f6788, plain, (spl62_434 <=> ! [X2] : (memberP(sK53, X2) | ~ ssItem(X2) | ~ memberP(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)), X2))), introduced(avatar_definition, [new_symbols(naming, [spl62_434])])).
fof(f52619, plain, ((sK57 = sK58) | (~ spl62_3 | ~ spl62_4 | ~ spl62_429 | ~ spl62_432 | ~ spl62_434)), inference(subsumption_resolution, [], [f52477, f558])).
fof(f52477, plain, ((sK57 = sK58) | ~ ssItem(sK58) | (~ spl62_3 | ~ spl62_4 | ~ spl62_429 | ~ spl62_432 | ~ spl62_434)), inference(resolution, [], [f52476, f2627])).
fof(f2627, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | ~ ssItem(X8)) | (~ spl62_3 | ~ spl62_4)), inference(subsumption_resolution, [], [f2626, f475])).
fof(f475, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (ssItem(X0) => ~ memberP(nil, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax38)).
fof(f2626, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssItem(X8)) | (~ spl62_3 | ~ spl62_4)), inference(subsumption_resolution, [], [f2625, f621])).
fof(f2625, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssItem(sK57) | ~ ssItem(X8)) | ~ spl62_3), inference(subsumption_resolution, [], [f2597, f445])).
fof(f2597, plain, (! [X8] : (~ memberP(sK53, X8) | (sK57 = X8) | memberP(nil, X8) | ~ ssList(nil) | ~ ssItem(sK57) | ~ ssItem(X8)) | ~ spl62_3), inference(superposition, [], [f472, f2320])).
fof(f472, plain, ! [X2, X0, X1] : (~ memberP(cons(X1, X2), X0) | (X0 = X1) | memberP(X2, X0) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f325])).
fof(f325, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & (memberP(X2, X0) | (X0 = X1) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f324])).
fof(f324, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & ((memberP(X2, X0) | (X0 = X1)) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => (memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax37)).
fof(f52476, plain, (memberP(sK53, sK58) | (~ spl62_429 | ~ spl62_432 | ~ spl62_434)), inference(subsumption_resolution, [], [f52475, f6780])).
fof(f52475, plain, (memberP(sK53, sK58) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | (~ spl62_429 | ~ spl62_434)), inference(subsumption_resolution, [], [f52474, f6764])).
fof(f52474, plain, (memberP(sK53, sK58) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ spl62_434), inference(subsumption_resolution, [], [f52473, f445])).
fof(f52473, plain, (memberP(sK53, sK58) | ~ ssList(nil) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ spl62_434), inference(subsumption_resolution, [], [f52466, f558])).
fof(f52466, plain, (~ ssItem(sK58) | memberP(sK53, sK58) | ~ ssList(nil) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ spl62_434), inference(duplicate_literal_removal, [], [f52465])).
fof(f52465, plain, (~ ssItem(sK58) | memberP(sK53, sK58) | ~ ssList(nil) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssItem(sK58) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ spl62_434), inference(resolution, [], [f6789, f571])).
fof(f571, plain, ! [X2, X3, X1] : (memberP(app(X2, cons(X1, X3)), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(app(X2, cons(X1, X3)))), inference(equality_resolution, [], [f361])).
fof(f361, plain, ! [X2, X0, X3, X1] : (memberP(X0, X1) | ~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f240])).
fof(f240, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(sK8(X0, X1), cons(X1, sK9(X0, X1))) = X0) & ssList(sK9(X0, X1))) & ssList(sK8(X0, X1))) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f237, f239, f238])).
fof(f238, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(sK8(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) & ssList(sK8(X0, X1)))), introduced(choice_axiom, [])).
fof(f239, plain, ! [X1, X0] : (? [X5] : ((app(sK8(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) => ((app(sK8(X0, X1), cons(X1, sK9(X0, X1))) = X0) & ssList(sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f237, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(rectify, [], [f236])).
fof(f236, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f100])).
fof(f100, plain, ! [X0] : (! [X1] : ((memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax3)).
fof(f6789, plain, (! [X2] : (~ memberP(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)), X2) | ~ ssItem(X2) | memberP(sK53, X2)) | ~ spl62_434), inference(avatar_component_clause, [], [f6788])).
fof(f43109, plain, ~ spl62_2013, inference(avatar_contradiction_clause, [], [f43108])).
fof(f43108, plain, ($false | ~ spl62_2013), inference(subsumption_resolution, [], [f43107, f631])).
fof(f43107, plain, (~ ssList(sK53) | ~ spl62_2013), inference(subsumption_resolution, [], [f43104, f558])).
fof(f43104, plain, (~ ssItem(sK58) | ~ ssList(sK53) | ~ spl62_2013), inference(trivial_inequality_removal, [], [f43054])).
fof(f43054, plain, (~ (nil = nil) | ~ ssItem(sK58) | ~ ssList(sK53) | ~ spl62_2013), inference(superposition, [], [f452, f41514])).
fof(f41514, plain, ((nil = cons(sK58, sK53)) | ~ spl62_2013), inference(avatar_component_clause, [], [f41512])).
fof(f452, plain, ! [X0, X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : (! [X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (nil = cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax21)).
fof(f40715, plain, (~ spl62_4 | ~ spl62_1419), inference(avatar_contradiction_clause, [], [f40714])).
fof(f40714, plain, ($false | (~ spl62_4 | ~ spl62_1419)), inference(subsumption_resolution, [], [f40703, f621])).
fof(f40703, plain, (~ ssItem(sK57) | ~ spl62_1419), inference(resolution, [], [f31115, f544])).
fof(f544, plain, ! [X0] : (~ lt(X0, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f212])).
fof(f212, plain, ! [X0] : (~ lt(X0, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f90])).
fof(f90, plain, ! [X0] : (ssItem(X0) => ~ lt(X0, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax90)).
fof(f31115, plain, (lt(sK57, sK57) | ~ spl62_1419), inference(avatar_component_clause, [], [f31113])).
fof(f31113, plain, (spl62_1419 <=> lt(sK57, sK57)), introduced(avatar_definition, [new_symbols(naming, [spl62_1419])])).
fof(f38920, plain, (spl62_98 | ~ spl62_442 | ~ spl62_1908), inference(avatar_split_clause, [], [f38911, f38871, f6835, f2922])).
fof(f38911, plain, ((sK53 = sK61) | (~ spl62_442 | ~ spl62_1908)), inference(backward_demodulation, [], [f865, f38904])).
fof(f38904, plain, ((sK53 = app(nil, sK61)) | (~ spl62_442 | ~ spl62_1908)), inference(backward_demodulation, [], [f7354, f38873])).
fof(f38873, plain, ((nil = sK12(sK53, sK61)) | ~ spl62_1908), inference(avatar_component_clause, [], [f38871])).
fof(f865, plain, (sK61 = app(nil, sK61)), inference(resolution, [], [f459, f561])).
fof(f459, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax28)).
fof(f38779, plain, spl62_1904, inference(avatar_split_clause, [], [f561, f38750])).
fof(f31119, plain, (spl62_1419 | spl62_1420 | ~ spl62_3 | ~ spl62_4), inference(avatar_split_clause, [], [f31111, f619, f614, f31117, f31113])).
fof(f31111, plain, (! [X1] : (~ sP4(app(app(X1, sK53), sK53)) | lt(sK57, sK57) | ~ ssList(X1)) | (~ spl62_3 | ~ spl62_4)), inference(subsumption_resolution, [], [f31110, f621])).
fof(f31110, plain, (! [X1] : (~ sP4(app(app(X1, sK53), sK53)) | lt(sK57, sK57) | ~ ssList(X1) | ~ ssItem(sK57)) | (~ spl62_3 | ~ spl62_4)), inference(subsumption_resolution, [], [f31109, f445])).
fof(f31109, plain, (! [X1] : (~ sP4(app(app(X1, sK53), sK53)) | lt(sK57, sK57) | ~ ssList(nil) | ~ ssList(X1) | ~ ssItem(sK57)) | (~ spl62_3 | ~ spl62_4)), inference(duplicate_literal_removal, [], [f31108])).
fof(f31108, plain, (! [X1] : (~ sP4(app(app(X1, sK53), sK53)) | lt(sK57, sK57) | ~ ssList(nil) | ~ ssList(X1) | ~ ssItem(sK57) | lt(sK57, sK57)) | (~ spl62_3 | ~ spl62_4)), inference(superposition, [], [f2828, f2320])).
fof(f2828, plain, (! [X2, X0, X1] : (~ sP4(app(app(X0, sK53), cons(X1, X2))) | lt(sK57, X1) | ~ ssList(X2) | ~ ssList(X0) | ~ ssItem(X1) | lt(X1, sK57)) | (~ spl62_3 | ~ spl62_4)), inference(subsumption_resolution, [], [f2827, f621])).
fof(f2827, plain, (! [X2, X0, X1] : (~ sP4(app(app(X0, sK53), cons(X1, X2))) | lt(sK57, X1) | ~ ssList(X2) | ~ ssList(X0) | ~ ssItem(X1) | ~ ssItem(sK57) | lt(X1, sK57)) | ~ spl62_3), inference(subsumption_resolution, [], [f2825, f445])).
fof(f2825, plain, (! [X2, X0, X1] : (~ sP4(app(app(X0, sK53), cons(X1, X2))) | lt(sK57, X1) | ~ ssList(X2) | ~ ssList(nil) | ~ ssList(X0) | ~ ssItem(X1) | ~ ssItem(sK57) | lt(X1, sK57)) | ~ spl62_3), inference(superposition, [], [f578, f2320])).
fof(f578, plain, ! [X6, X10, X8, X7, X9] : (~ sP4(app(app(X8, cons(X6, X9)), cons(X7, X10))) | lt(X6, X7) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | lt(X7, X6)), inference(equality_resolution, [], [f401])).
fof(f401, plain, ! [X6, X0, X10, X8, X7, X9] : (lt(X7, X6) | lt(X6, X7) | ~ (app(app(X8, cons(X6, X9)), cons(X7, X10)) = X0) | ~ ssList(X10) | ~ ssList(X9) | ~ ssList(X8) | ~ ssItem(X7) | ~ ssItem(X6) | ~ sP4(X0)), inference(cnf_transformation, [], [f284])).
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
fof(f22094, plain, (~ spl62_3 | ~ spl62_414 | ~ spl62_430 | spl62_456), inference(avatar_contradiction_clause, [], [f22093])).
fof(f22093, plain, ($false | (~ spl62_3 | ~ spl62_414 | ~ spl62_430 | spl62_456)), inference(subsumption_resolution, [], [f22092, f22082])).
fof(f22082, plain, (segmentP(sK53, sK53) | (~ spl62_3 | ~ spl62_414 | ~ spl62_430)), inference(forward_demodulation, [], [f21913, f2320])).
fof(f21913, plain, (segmentP(sK53, cons(sK57, nil)) | (~ spl62_414 | ~ spl62_430)), inference(backward_demodulation, [], [f6769, f6280])).
fof(f22092, plain, (~ segmentP(sK53, sK53) | (~ spl62_3 | ~ spl62_414 | spl62_456)), inference(forward_demodulation, [], [f21923, f2320])).
fof(f21923, plain, (~ segmentP(cons(sK57, nil), sK53) | (~ spl62_414 | spl62_456)), inference(backward_demodulation, [], [f7009, f6280])).
fof(f7009, plain, (~ segmentP(cons(sK58, nil), sK53) | spl62_456), inference(avatar_component_clause, [], [f7007])).
fof(f10808, plain, spl62_526, inference(avatar_contradiction_clause, [], [f10807])).
fof(f10807, plain, ($false | spl62_526), inference(subsumption_resolution, [], [f10806, f558])).
fof(f10806, plain, (~ ssItem(sK58) | spl62_526), inference(resolution, [], [f8858, f505])).
fof(f505, plain, ! [X0] : (strictorderP(cons(X0, nil)) | ~ ssItem(X0)), inference(cnf_transformation, [], [f180])).
fof(f180, plain, ! [X0] : (strictorderP(cons(X0, nil)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f63])).
fof(f63, plain, ! [X0] : (ssItem(X0) => strictorderP(cons(X0, nil))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax63)).
fof(f8858, plain, (~ strictorderP(cons(sK58, nil)) | spl62_526), inference(avatar_component_clause, [], [f8857])).
fof(f8857, plain, (spl62_526 <=> strictorderP(cons(sK58, nil))), introduced(avatar_definition, [new_symbols(naming, [spl62_526])])).
fof(f8865, plain, (spl62_527 | ~ spl62_526 | ~ spl62_47), inference(avatar_split_clause, [], [f8855, f2085, f8857, f8861])).
fof(f8855, plain, (~ strictorderP(cons(sK58, nil)) | sP4(cons(sK58, nil)) | ~ spl62_47), inference(resolution, [], [f2107, f399])).
fof(f399, plain, ! [X0] : (~ sP5(X0) | ~ strictorderP(X0) | sP4(X0)), inference(cnf_transformation, [], [f276])).
fof(f276, plain, ! [X0] : (((strictorderP(X0) | ~ sP4(X0)) & (sP4(X0) | ~ strictorderP(X0))) | ~ sP5(X0)), inference(nnf_transformation, [], [f230])).
fof(f230, plain, ! [X0] : ((strictorderP(X0) <=> sP4(X0)) | ~ sP5(X0)), inference(usedef, [], [e230])).
fof(e230, plain, ! [X0] : (sP5(X0) <=> (strictorderP(X0) <=> sP4(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f2107, plain, (sP5(cons(sK58, nil)) | ~ spl62_47), inference(resolution, [], [f2086, f410])).
fof(f410, plain, ! [X0] : (~ ssList(X0) | sP5(X0)), inference(cnf_transformation, [], [f231])).
fof(f231, plain, ! [X0] : (sP5(X0) | ~ ssList(X0)), inference(definition_folding, [], [f110, e230, e229])).
fof(f110, plain, ! [X0] : ((strictorderP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (lt(X2, X1) | lt(X1, X2) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(flattening, [], [f109])).
fof(f109, plain, ! [X0] : ((strictorderP(X0) <=> ! [X1] : (! [X2] : (! [X3] : (! [X4] : (! [X5] : (((lt(X2, X1) | lt(X1, X2)) | ~ (app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0)) | ~ ssList(X5)) | ~ ssList(X4)) | ~ ssList(X3)) | ~ ssItem(X2)) | ~ ssItem(X1))) | ~ ssList(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (ssList(X0) => (strictorderP(X0) <=> ! [X1] : (ssItem(X1) => ! [X2] : (ssItem(X2) => ! [X3] : (ssList(X3) => ! [X4] : (ssList(X4) => ! [X5] : (ssList(X5) => ((app(app(X3, cons(X1, X4)), cons(X2, X5)) = X0) => (lt(X2, X1) | lt(X1, X2)))))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax10)).
fof(f7485, plain, (spl62_98 | ~ spl62_498), inference(avatar_split_clause, [], [f7477, f7434, f2922])).
fof(f7477, plain, ((sK53 = sK61) | ~ spl62_498), inference(backward_demodulation, [], [f865, f7453])).
fof(f7453, plain, ((sK53 = app(nil, sK61)) | ~ spl62_498), inference(backward_demodulation, [], [f2316, f7436])).
fof(f7436, plain, ((nil = app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ spl62_498), inference(avatar_component_clause, [], [f7434])).
fof(f7345, plain, (~ spl62_432 | spl62_434), inference(avatar_split_clause, [], [f7344, f6788, f6779])).
fof(f7344, plain, ! [X2] : (memberP(sK53, X2) | ~ memberP(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)), X2) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ ssItem(X2)), inference(subsumption_resolution, [], [f7156, f561])).
fof(f7156, plain, ! [X2] : (memberP(sK53, X2) | ~ memberP(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)), X2) | ~ ssList(sK61) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ ssItem(X2)), inference(superposition, [], [f470, f2316])).
fof(f470, plain, ! [X2, X0, X1] : (memberP(app(X1, X2), X0) | ~ memberP(X1, X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f323])).
fof(f323, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & (memberP(X2, X0) | memberP(X1, X0) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f322])).
fof(f322, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & ((memberP(X2, X0) | memberP(X1, X0)) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax36)).
fof(f7335, plain, (~ spl62_432 | spl62_439), inference(avatar_split_clause, [], [f7334, f6813, f6779])).
fof(f7334, plain, ! [X8] : (~ (sK53 = app(X8, sK61)) | (app(app(sK60, cons(sK58, nil)), cons(sK58, nil)) = X8) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ ssList(X8)), inference(subsumption_resolution, [], [f7161, f561])).
fof(f7161, plain, ! [X8] : (~ (sK53 = app(X8, sK61)) | (app(app(sK60, cons(sK58, nil)), cons(sK58, nil)) = X8) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ ssList(sK61) | ~ ssList(X8)), inference(superposition, [], [f531, f2316])).
fof(f531, plain, ! [X2, X0, X1] : (~ (app(X2, X1) = app(X0, X1)) | (X0 = X2) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ! [X0] : (! [X1] : (! [X2] : ((X0 = X2) | ~ (app(X2, X1) = app(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f195])).
fof(f195, plain, ! [X0] : (! [X1] : (! [X2] : (((X0 = X2) | ~ (app(X2, X1) = app(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f79])).
fof(f79, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((app(X2, X1) = app(X0, X1)) => (X0 = X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax79)).
fof(f7324, plain, (~ spl62_432 | spl62_442), inference(avatar_split_clause, [], [f7323, f6835, f6779])).
fof(f7323, plain, (rearsegP(sK53, sK61) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil)))), inference(subsumption_resolution, [], [f7322, f631])).
fof(f7322, plain, (rearsegP(sK53, sK61) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ ssList(sK53)), inference(subsumption_resolution, [], [f7168, f561])).
fof(f7168, plain, (rearsegP(sK53, sK61) | ~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | ~ ssList(sK61) | ~ ssList(sK53)), inference(superposition, [], [f574, f2316])).
fof(f7319, plain, (~ spl62_47 | ~ spl62_429 | spl62_432), inference(avatar_contradiction_clause, [], [f7318])).
fof(f7318, plain, ($false | (~ spl62_47 | ~ spl62_429 | spl62_432)), inference(subsumption_resolution, [], [f7317, f6764])).
fof(f7317, plain, (~ ssList(app(sK60, cons(sK58, nil))) | (~ spl62_47 | spl62_432)), inference(subsumption_resolution, [], [f7316, f2086])).
fof(f7316, plain, (~ ssList(cons(sK58, nil)) | ~ ssList(app(sK60, cons(sK58, nil))) | spl62_432), inference(resolution, [], [f6781, f457])).
fof(f6781, plain, (~ ssList(app(app(sK60, cons(sK58, nil)), cons(sK58, nil))) | spl62_432), inference(avatar_component_clause, [], [f6779])).
fof(f6984, plain, (~ spl62_47 | spl62_429), inference(avatar_contradiction_clause, [], [f6983])).
fof(f6983, plain, ($false | (~ spl62_47 | spl62_429)), inference(subsumption_resolution, [], [f6982, f560])).
fof(f6982, plain, (~ ssList(sK60) | (~ spl62_47 | spl62_429)), inference(subsumption_resolution, [], [f6981, f2086])).
fof(f6981, plain, (~ ssList(cons(sK58, nil)) | ~ ssList(sK60) | spl62_429), inference(resolution, [], [f6765, f457])).
fof(f6765, plain, (~ ssList(app(sK60, cons(sK58, nil))) | spl62_429), inference(avatar_component_clause, [], [f6763])).
fof(f6770, plain, (~ spl62_429 | spl62_430 | ~ spl62_47), inference(avatar_split_clause, [], [f6761, f2085, f6767, f6763])).
fof(f6761, plain, (segmentP(sK53, cons(sK58, nil)) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ spl62_47), inference(subsumption_resolution, [], [f6760, f631])).
fof(f6760, plain, (segmentP(sK53, cons(sK58, nil)) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssList(sK53) | ~ spl62_47), inference(subsumption_resolution, [], [f6759, f2086])).
fof(f6759, plain, (segmentP(sK53, cons(sK58, nil)) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssList(cons(sK58, nil)) | ~ ssList(sK53)), inference(subsumption_resolution, [], [f6738, f561])).
fof(f6738, plain, (segmentP(sK53, cons(sK58, nil)) | ~ ssList(sK61) | ~ ssList(app(sK60, cons(sK58, nil))) | ~ ssList(cons(sK58, nil)) | ~ ssList(sK53)), inference(superposition, [], [f575, f2316])).
fof(f575, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f374])).
fof(f374, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f257])).
fof(f257, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK13(X0, X1), X1), sK14(X0, X1)) = X0) & ssList(sK14(X0, X1))) & ssList(sK13(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14])], [f254, f256, f255])).
fof(f255, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK13(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK13(X0, X1)))), introduced(choice_axiom, [])).
fof(f256, plain, ! [X1, X0] : (? [X5] : ((app(app(sK13(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK13(X0, X1), X1), sK14(X0, X1)) = X0) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f254, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f253])).
fof(f253, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax7)).
fof(f5819, plain, (spl62_243 | ~ spl62_3 | ~ spl62_4), inference(avatar_split_clause, [], [f5818, f619, f614, f4104])).
fof(f5818, plain, ((nil = tl(sK53)) | (~ spl62_3 | ~ spl62_4)), inference(forward_demodulation, [], [f5815, f2320])).
fof(f5815, plain, ((nil = tl(cons(sK57, nil))) | ~ spl62_4), inference(resolution, [], [f1056, f621])).
fof(f1056, plain, ! [X6] : (~ ssItem(X6) | (nil = tl(cons(X6, nil)))), inference(resolution, [], [f456, f445])).
fof(f456, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax25)).
fof(f2099, plain, spl62_47, inference(avatar_contradiction_clause, [], [f2098])).
fof(f2098, plain, ($false | spl62_47), inference(subsumption_resolution, [], [f2097, f445])).
fof(f2097, plain, (~ ssList(nil) | spl62_47), inference(subsumption_resolution, [], [f2096, f558])).
fof(f2096, plain, (~ ssItem(sK58) | ~ ssList(nil) | spl62_47), inference(resolution, [], [f2087, f444])).
fof(f444, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC169+1.p', ax16)).
fof(f2087, plain, (~ ssList(cons(sK58, nil)) | spl62_47), inference(avatar_component_clause, [], [f2085])).
fof(f622, plain, (spl62_4 | spl62_2), inference(avatar_split_clause, [], [f567, f609, f619])).
fof(f567, plain, ((nil = sK55) | ssItem(sK57)), inference(cnf_transformation, [], [f352])).
fof(f617, plain, (spl62_3 | spl62_2), inference(avatar_split_clause, [], [f568, f609, f614])).
fof(f568, plain, ((nil = sK55) | (sK55 = cons(sK57, nil))), inference(cnf_transformation, [], [f352])).