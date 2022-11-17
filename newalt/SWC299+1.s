fof(f1301299, plain, $false, inference(avatar_sat_refutation, [], [f626, f2288, f2435, f3869, f3994, f5336, f5520, f6795, f7027, f7047, f7262, f9060, f20169, f26442, f30554, f58816, f134817, f179127, f203857, f204824, f207479, f209307, f263757, f264049, f280708, f280784, f291376, f374129, f374490, f374492, f375535, f466301, f529869, f534120, f609079, f630245, f694703, f701779, f814914, f816039, f825012, f851749, f851773, f854577, f916397, f955527, f998220, f1223212, f1293691, f1301296])).
fof(f1301296, plain, (~ spl66_1635 | ~ spl66_9488 | ~ spl66_9493 | spl66_32898), inference(avatar_contradiction_clause, [], [f1301295])).
fof(f1301295, plain, ($false | (~ spl66_1635 | ~ spl66_9488 | ~ spl66_9493 | spl66_32898)), inference(subsumption_resolution, [], [f1301294, f29112])).
fof(f29112, plain, (ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ spl66_1635), inference(avatar_component_clause, [], [f29111])).
fof(f29111, plain, (spl66_1635 <=> ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl66_1635])])).
fof(f1301294, plain, (~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | (~ spl66_1635 | ~ spl66_9488 | ~ spl66_9493 | spl66_32898)), inference(subsumption_resolution, [], [f1301293, f204001])).
fof(f204001, plain, (ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ spl66_9488), inference(avatar_component_clause, [], [f204000])).
fof(f204000, plain, (spl66_9488 <=> ssList(app(app(sK63, cons(sK61, nil)), sK64))), introduced(avatar_definition, [new_symbols(naming, [spl66_9488])])).
fof(f1301293, plain, (~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | (~ spl66_1635 | ~ spl66_9493 | spl66_32898)), inference(subsumption_resolution, [], [f1301292, f455])).
fof(f455, plain, ssList(nil), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ssList(nil), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax17)).
fof(f1301292, plain, (~ ssList(nil) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | (~ spl66_1635 | ~ spl66_9493 | spl66_32898)), inference(subsumption_resolution, [], [f1301291, f576])).
fof(f576, plain, ssItem(sK62), inference(cnf_transformation, [], [f362])).
fof(f362, plain, (((((((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & (((((lt(sK62, sK61) & (sK57 = app(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), sK65)) & ssList(sK65)) & ssList(sK64)) & ssList(sK63)) & ssItem(sK62)) & ssItem(sK61)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60)) & ssList(sK59)) & ssList(sK58)) & ssList(sK57)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK57, sK58, sK59, sK60, sK61, sK62, sK63, sK64, sK65])], [f352, f361, f360, f359, f358, f357, f356, f355, f354, f353])).
fof(f353, plain, (? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = X0) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)) => (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = sK57) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(sK57))), introduced(choice_axiom, [])).
fof(f354, plain, (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = sK57) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) => (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = sK57) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) & ssList(sK58))), introduced(choice_axiom, [])).
fof(f355, plain, (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = sK57) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = X2) & (sK58 = X3) & ssList(X3)) & ssList(X2)) => (? [X3] : ((((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = sK57) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) & ssList(sK59))), introduced(choice_axiom, [])).
fof(f356, plain, (? [X3] : ((((nil = sK59) & (nil = X3)) | sP6(X3, sK59)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = sK57) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = sK59) & (sK58 = X3) & ssList(X3)) => ((((nil = sK59) & (nil = sK60)) | sP6(sK60, sK59)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = sK57) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (sK57 = sK59) & (sK58 = sK60) & ssList(sK60))), introduced(choice_axiom, [])).
fof(f357, plain, (? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = sK57) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) => (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, sK61) & (sK57 = app(app(app(app(X6, cons(sK61, nil)), X7), cons(X5, nil)), X8)) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(sK61))), introduced(choice_axiom, [])).
fof(f358, plain, (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, sK61) & (sK57 = app(app(app(app(X6, cons(sK61, nil)), X7), cons(X5, nil)), X8)) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) => (? [X6] : (? [X7] : (? [X8] : (lt(sK62, sK61) & (sK57 = app(app(app(app(X6, cons(sK61, nil)), X7), cons(sK62, nil)), X8)) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(sK62))), introduced(choice_axiom, [])).
fof(f359, plain, (? [X6] : (? [X7] : (? [X8] : (lt(sK62, sK61) & (sK57 = app(app(app(app(X6, cons(sK61, nil)), X7), cons(sK62, nil)), X8)) & ssList(X8)) & ssList(X7)) & ssList(X6)) => (? [X7] : (? [X8] : (lt(sK62, sK61) & (sK57 = app(app(app(app(sK63, cons(sK61, nil)), X7), cons(sK62, nil)), X8)) & ssList(X8)) & ssList(X7)) & ssList(sK63))), introduced(choice_axiom, [])).
fof(f360, plain, (? [X7] : (? [X8] : (lt(sK62, sK61) & (sK57 = app(app(app(app(sK63, cons(sK61, nil)), X7), cons(sK62, nil)), X8)) & ssList(X8)) & ssList(X7)) => (? [X8] : (lt(sK62, sK61) & (sK57 = app(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), X8)) & ssList(X8)) & ssList(sK64))), introduced(choice_axiom, [])).
fof(f361, plain, (? [X8] : (lt(sK62, sK61) & (sK57 = app(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), X8)) & ssList(X8)) => (lt(sK62, sK61) & (sK57 = app(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), sK65)) & ssList(sK65))), introduced(choice_axiom, [])).
fof(f352, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X4] : (? [X5] : (? [X6] : (? [X7] : (? [X8] : (lt(X5, X4) & (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = X0) & ssList(X8)) & ssList(X7)) & ssList(X6)) & ssItem(X5)) & ssItem(X4)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(rectify, [], [f234])).
fof(f234, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | sP6(X3, X2)) & ? [X9] : (? [X10] : (? [X11] : (? [X12] : (? [X13] : (lt(X10, X9) & (app(app(app(app(X11, cons(X9, nil)), X12), cons(X10, nil)), X13) = X0) & ssList(X13)) & ssList(X12)) & ssList(X11)) & ssItem(X10)) & ssItem(X9)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(definition_folding, [], [f223, e233])).
fof(f233, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(usedef, [], [e233])).
fof(e233, plain, ! [X3, X2] : (sP6(X3, X2) <=> ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f223, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : ((((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & ? [X9] : (? [X10] : (? [X11] : (? [X12] : (? [X13] : (lt(X10, X9) & (app(app(app(app(X11, cons(X9, nil)), X12), cons(X10, nil)), X13) = X0) & ssList(X13)) & ssList(X12)) & ssList(X11)) & ssItem(X10)) & ssItem(X9)) & (X0 = X2) & (X1 = X3) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(flattening, [], [f222])).
fof(f222, plain, ? [X0] : (? [X1] : (? [X2] : (? [X3] : (((((nil = X2) & (nil = X3)) | ? [X4] : (? [X5] : (? [X6] : ((! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2)) & ssList(X6)) & ssList(X5)) & ssItem(X4))) & ? [X9] : (? [X10] : (? [X11] : (? [X12] : (? [X13] : ((lt(X10, X9) & (app(app(app(app(X11, cons(X9, nil)), X12), cons(X10, nil)), X13) = X0)) & ssList(X13)) & ssList(X12)) & ssList(X11)) & ssItem(X10)) & ssItem(X9)) & (X0 = X2) & (X1 = X3)) & ssList(X3)) & ssList(X2)) & ssList(X1)) & ssList(X0)), inference(ennf_transformation, [], [f98])).
fof(f98, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X4] : (ssItem(X4) => ! [X5] : (ssList(X5) => ! [X6] : (ssList(X6) => (? [X7] : (lt(X7, X4) & memberP(X6, X7) & ssItem(X7)) | ? [X8] : (lt(X4, X8) & memberP(X5, X8) & ssItem(X8)) | ~ (app(app(X5, X2), X6) = X3) | ~ (cons(X4, nil) = X2)))))) | ! [X9] : (ssItem(X9) => ! [X10] : (ssItem(X10) => ! [X11] : (ssList(X11) => ! [X12] : (ssList(X12) => ! [X13] : (ssList(X13) => (~ lt(X10, X9) | ~ (app(app(app(app(X11, cons(X9, nil)), X12), cons(X10, nil)), X13) = X0))))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(rectify, [], [f97])).
fof(f97, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X9] : (ssItem(X9) => ! [X10] : (ssList(X10) => ! [X11] : (ssList(X11) => (? [X13] : (lt(X13, X9) & memberP(X11, X13) & ssItem(X13)) | ? [X12] : (lt(X9, X12) & memberP(X10, X12) & ssItem(X12)) | ~ (app(app(X10, X2), X11) = X3) | ~ (cons(X9, nil) = X2)))))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => ! [X8] : (ssList(X8) => (~ lt(X5, X4) | ~ (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = X0))))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), inference(negated_conjecture, [], [f96])).
fof(f96, plain, ~ ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (((~ (nil = X2) | ~ (nil = X3)) & ! [X9] : (ssItem(X9) => ! [X10] : (ssList(X10) => ! [X11] : (ssList(X11) => (? [X13] : (lt(X13, X9) & memberP(X11, X13) & ssItem(X13)) | ? [X12] : (lt(X9, X12) & memberP(X10, X12) & ssItem(X12)) | ~ (app(app(X10, X2), X11) = X3) | ~ (cons(X9, nil) = X2)))))) | ! [X4] : (ssItem(X4) => ! [X5] : (ssItem(X5) => ! [X6] : (ssList(X6) => ! [X7] : (ssList(X7) => ! [X8] : (ssList(X8) => (~ lt(X5, X4) | ~ (app(app(app(app(X6, cons(X4, nil)), X7), cons(X5, nil)), X8) = X0))))))) | ~ (X0 = X2) | ~ (X1 = X3)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', co1)).
fof(f1301291, plain, (~ ssItem(sK62) | ~ ssList(nil) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | (~ spl66_1635 | ~ spl66_9493 | spl66_32898)), inference(subsumption_resolution, [], [f1301290, f1185837])).
fof(f1185837, plain, (~ memberP(sK57, sK62) | spl66_32898), inference(avatar_component_clause, [], [f1185836])).
fof(f1185836, plain, (spl66_32898 <=> memberP(sK57, sK62)), introduced(avatar_definition, [new_symbols(naming, [spl66_32898])])).
fof(f1301290, plain, (memberP(sK57, sK62) | ~ ssItem(sK62) | ~ ssList(nil) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | (~ spl66_1635 | ~ spl66_9493)), inference(duplicate_literal_removal, [], [f1301285])).
fof(f1301285, plain, (memberP(sK57, sK62) | ~ ssItem(sK62) | ~ ssList(nil) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssItem(sK62) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | (~ spl66_1635 | ~ spl66_9493)), inference(resolution, [], [f212675, f585])).
fof(f585, plain, ! [X2, X3, X1] : (memberP(app(X2, cons(X1, X3)), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(app(X2, cons(X1, X3)))), inference(equality_resolution, [], [f371])).
fof(f371, plain, ! [X2, X0, X3, X1] : (memberP(X0, X1) | ~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f243])).
fof(f243, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(sK9(X0, X1), cons(X1, sK10(X0, X1))) = X0) & ssList(sK10(X0, X1))) & ssList(sK9(X0, X1))) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9, sK10])], [f240, f242, f241])).
fof(f241, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(sK9(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) & ssList(sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f242, plain, ! [X1, X0] : (? [X5] : ((app(sK9(X0, X1), cons(X1, X5)) = X0) & ssList(X5)) => ((app(sK9(X0, X1), cons(X1, sK10(X0, X1))) = X0) & ssList(sK10(X0, X1)))), introduced(choice_axiom, [])).
fof(f240, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(X4, cons(X1, X5)) = X0) & ssList(X5)) & ssList(X4)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(rectify, [], [f239])).
fof(f239, plain, ! [X0] : (! [X1] : (((memberP(X0, X1) | ! [X2] : (! [X3] : (~ (app(X2, cons(X1, X3)) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2)) | ~ memberP(X0, X1))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f100])).
fof(f100, plain, ! [X0] : (! [X1] : ((memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (memberP(X0, X1) <=> ? [X2] : (? [X3] : ((app(X2, cons(X1, X3)) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax3)).
fof(f212675, plain, (! [X7] : (~ memberP(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), X7) | memberP(sK57, X7) | ~ ssItem(X7)) | (~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f212674, f29112])).
fof(f212674, plain, (! [X7] : (memberP(sK57, X7) | ~ memberP(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), X7) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ ssItem(X7)) | ~ spl66_9493), inference(subsumption_resolution, [], [f212581, f204648])).
fof(f204648, plain, (ssList(sK65) | ~ spl66_9493), inference(avatar_component_clause, [], [f204647])).
fof(f204647, plain, (spl66_9493 <=> ssList(sK65)), introduced(avatar_definition, [new_symbols(naming, [spl66_9493])])).
fof(f212581, plain, ! [X7] : (memberP(sK57, X7) | ~ memberP(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), X7) | ~ ssList(sK65) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ ssItem(X7)), inference(superposition, [], [f480, f580])).
fof(f580, plain, (sK57 = app(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), sK65)), inference(cnf_transformation, [], [f362])).
fof(f480, plain, ! [X2, X0, X1] : (memberP(app(X1, X2), X0) | ~ memberP(X1, X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f326])).
fof(f326, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & (memberP(X2, X0) | memberP(X1, X0) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(flattening, [], [f325])).
fof(f325, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(app(X1, X2), X0) | (~ memberP(X2, X0) & ~ memberP(X1, X0))) & ((memberP(X2, X0) | memberP(X1, X0)) | ~ memberP(app(X1, X2), X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (memberP(app(X1, X2), X0) <=> (memberP(X2, X0) | memberP(X1, X0)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax36)).
fof(f1293691, plain, (spl66_25663 | ~ spl66_62 | ~ spl66_1635 | ~ spl66_9488 | ~ spl66_9493), inference(avatar_split_clause, [], [f1293690, f204647, f204000, f29111, f2274, f814903])).
fof(f814903, plain, (spl66_25663 <=> ! [X0] : (frontsegP(sK57, X0) | ~ frontsegP(app(app(sK63, cons(sK61, nil)), sK64), X0) | ~ ssList(X0))), introduced(avatar_definition, [new_symbols(naming, [spl66_25663])])).
fof(f2274, plain, (spl66_62 <=> ssList(cons(sK62, nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_62])])).
fof(f1293690, plain, (! [X0] : (frontsegP(sK57, X0) | ~ ssList(X0) | ~ frontsegP(app(app(sK63, cons(sK61, nil)), sK64), X0)) | (~ spl66_62 | ~ spl66_1635 | ~ spl66_9488 | ~ spl66_9493)), inference(subsumption_resolution, [], [f1293689, f204001])).
fof(f1293689, plain, (! [X0] : (frontsegP(sK57, X0) | ~ ssList(X0) | ~ frontsegP(app(app(sK63, cons(sK61, nil)), sK64), X0) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64))) | (~ spl66_62 | ~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f1293688, f2275])).
fof(f2275, plain, (ssList(cons(sK62, nil)) | ~ spl66_62), inference(avatar_component_clause, [], [f2274])).
fof(f1293688, plain, (! [X0] : (frontsegP(sK57, X0) | ~ ssList(X0) | ~ frontsegP(app(app(sK63, cons(sK61, nil)), sK64), X0) | ~ ssList(cons(sK62, nil)) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64))) | (~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f1293667, f633])).
fof(f633, plain, ssList(sK57), inference(forward_demodulation, [], [f571, f574])).
fof(f574, plain, (sK57 = sK59), inference(cnf_transformation, [], [f362])).
fof(f571, plain, ssList(sK59), inference(cnf_transformation, [], [f362])).
fof(f1293667, plain, (! [X0] : (frontsegP(sK57, X0) | ~ ssList(X0) | ~ ssList(sK57) | ~ frontsegP(app(app(sK63, cons(sK61, nil)), sK64), X0) | ~ ssList(cons(sK62, nil)) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64))) | (~ spl66_1635 | ~ spl66_9493)), inference(resolution, [], [f212690, f1964])).
fof(f1964, plain, ! [X10, X8, X7, X9] : (~ frontsegP(X7, app(X9, X10)) | frontsegP(X7, X8) | ~ ssList(X8) | ~ ssList(X7) | ~ frontsegP(X9, X8) | ~ ssList(X10) | ~ ssList(X9)), inference(subsumption_resolution, [], [f1959, f467])).
fof(f467, plain, ! [X0, X1] : (ssList(app(X0, X1)) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : (ssList(app(X0, X1)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ssList(app(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax26)).
fof(f1959, plain, ! [X10, X8, X7, X9] : (frontsegP(X7, X8) | ~ frontsegP(X7, app(X9, X10)) | ~ ssList(X8) | ~ ssList(app(X9, X10)) | ~ ssList(X7) | ~ frontsegP(X9, X8) | ~ ssList(X10) | ~ ssList(X9)), inference(duplicate_literal_removal, [], [f1958])).
fof(f1958, plain, ! [X10, X8, X7, X9] : (frontsegP(X7, X8) | ~ frontsegP(X7, app(X9, X10)) | ~ ssList(X8) | ~ ssList(app(X9, X10)) | ~ ssList(X7) | ~ frontsegP(X9, X8) | ~ ssList(X10) | ~ ssList(X8) | ~ ssList(X9)), inference(resolution, [], [f487, f490])).
fof(f490, plain, ! [X2, X0, X1] : (frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f156])).
fof(f156, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f155])).
fof(f155, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(app(X0, X2), X1) | ~ frontsegP(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f43])).
fof(f43, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (frontsegP(X0, X1) => frontsegP(app(X0, X2), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax43)).
fof(f487, plain, ! [X2, X0, X1] : (~ frontsegP(X1, X2) | frontsegP(X0, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ! [X0] : (! [X1] : (! [X2] : (frontsegP(X0, X2) | ~ frontsegP(X1, X2) | ~ frontsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f150])).
fof(f150, plain, ! [X0] : (! [X1] : (! [X2] : ((frontsegP(X0, X2) | (~ frontsegP(X1, X2) | ~ frontsegP(X0, X1))) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f40])).
fof(f40, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => ((frontsegP(X1, X2) & frontsegP(X0, X1)) => frontsegP(X0, X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax40)).
fof(f212690, plain, (frontsegP(sK57, app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | (~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f212689, f633])).
fof(f212689, plain, (frontsegP(sK57, app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ ssList(sK57) | (~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f212688, f29112])).
fof(f212688, plain, (frontsegP(sK57, app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ ssList(sK57) | ~ spl66_9493), inference(subsumption_resolution, [], [f212592, f204648])).
fof(f212592, plain, (frontsegP(sK57, app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ ssList(sK65) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ ssList(sK57)), inference(superposition, [], [f587, f580])).
fof(f587, plain, ! [X2, X1] : (frontsegP(app(X1, X2), X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(X1, X2))), inference(equality_resolution, [], [f377])).
fof(f377, plain, ! [X2, X0, X1] : (frontsegP(X0, X1) | ~ (app(X1, X2) = X0) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (((app(X1, sK12(X0, X1)) = X0) & ssList(sK12(X0, X1))) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f249, f250])).
fof(f250, plain, ! [X1, X0] : (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) => ((app(X1, sK12(X0, X1)) = X0) & ssList(sK12(X0, X1)))), introduced(choice_axiom, [])).
fof(f249, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X1, X3) = X0) & ssList(X3)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f248])).
fof(f248, plain, ! [X0] : (! [X1] : (((frontsegP(X0, X1) | ! [X2] : (~ (app(X1, X2) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X1, X2) = X0) & ssList(X2)) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (! [X1] : ((frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (frontsegP(X0, X1) <=> ? [X2] : ((app(X1, X2) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax5)).
fof(f1223212, plain, (~ spl66_1 | ~ spl66_11238 | ~ spl66_11240 | spl66_20077 | ~ spl66_32898), inference(avatar_contradiction_clause, [], [f1223211])).
fof(f1223211, plain, ($false | (~ spl66_1 | ~ spl66_11238 | ~ spl66_11240 | spl66_20077 | ~ spl66_32898)), inference(subsumption_resolution, [], [f1223210, f576])).
fof(f1223210, plain, (~ ssItem(sK62) | (~ spl66_1 | ~ spl66_11238 | ~ spl66_11240 | spl66_20077 | ~ spl66_32898)), inference(subsumption_resolution, [], [f1223209, f529606])).
fof(f529606, plain, (~ (sK62 = hd(sK57)) | spl66_20077), inference(avatar_component_clause, [], [f529605])).
fof(f529605, plain, (spl66_20077 <=> (sK62 = hd(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_20077])])).
fof(f1223209, plain, ((sK62 = hd(sK57)) | ~ ssItem(sK62) | (~ spl66_1 | ~ spl66_11238 | ~ spl66_11240 | ~ spl66_32898)), inference(resolution, [], [f291422, f1185838])).
fof(f1185838, plain, (memberP(sK57, sK62) | ~ spl66_32898), inference(avatar_component_clause, [], [f1185836])).
fof(f291422, plain, (! [X8] : (~ memberP(sK57, X8) | (hd(sK57) = X8) | ~ ssItem(X8)) | (~ spl66_1 | ~ spl66_11238 | ~ spl66_11240)), inference(backward_demodulation, [], [f280707, f291418])).
fof(f291418, plain, ((hd(sK57) = sK54(sK58, sK57)) | (~ spl66_1 | ~ spl66_11238)), inference(forward_demodulation, [], [f291377, f212237])).
fof(f212237, plain, ((sK57 = cons(sK54(sK58, sK57), nil)) | ~ spl66_1), inference(resolution, [], [f210133, f565])).
fof(f565, plain, ! [X0, X1] : (~ sP6(X0, X1) | (cons(sK54(X0, X1), nil) = X1)), inference(cnf_transformation, [], [f351])).
fof(f351, plain, ! [X0, X1] : ((((! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1))) & ssList(sK55(X0, X1))) & ssItem(sK54(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK54, sK55, sK56])], [f347, f350, f349, f348])).
fof(f348, plain, ! [X1, X0] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) => (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(sK54(X0, X1)))), introduced(choice_axiom, [])).
fof(f349, plain, ! [X1, X0] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(X3)) => (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) & ssList(sK55(X0, X1)))), introduced(choice_axiom, [])).
fof(f350, plain, ! [X1, X0] : (? [X4] : (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), X4) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(X4)) => (! [X5] : (~ lt(X5, sK54(X0, X1)) | ~ memberP(sK56(X0, X1), X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(sK54(X0, X1), X6) | ~ memberP(sK55(X0, X1), X6) | ~ ssItem(X6)) & (app(app(sK55(X0, X1), X1), sK56(X0, X1)) = X0) & (cons(sK54(X0, X1), nil) = X1) & ssList(sK56(X0, X1)))), introduced(choice_axiom, [])).
fof(f347, plain, ! [X0, X1] : (? [X2] : (? [X3] : (? [X4] : (! [X5] : (~ lt(X5, X2) | ~ memberP(X4, X5) | ~ ssItem(X5)) & ! [X6] : (~ lt(X2, X6) | ~ memberP(X3, X6) | ~ ssItem(X6)) & (app(app(X3, X1), X4) = X0) & (cons(X2, nil) = X1) & ssList(X4)) & ssList(X3)) & ssItem(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f346])).
fof(f346, plain, ! [X3, X2] : (? [X4] : (? [X5] : (? [X6] : (! [X7] : (~ lt(X7, X4) | ~ memberP(X6, X7) | ~ ssItem(X7)) & ! [X8] : (~ lt(X4, X8) | ~ memberP(X5, X8) | ~ ssItem(X8)) & (app(app(X5, X2), X6) = X3) & (cons(X4, nil) = X2) & ssList(X6)) & ssList(X5)) & ssItem(X4)) | ~ sP6(X3, X2)), inference(nnf_transformation, [], [f233])).
fof(f210133, plain, (sP6(sK58, sK57) | ~ spl66_1), inference(forward_demodulation, [], [f210132, f573])).
fof(f573, plain, (sK58 = sK60), inference(cnf_transformation, [], [f362])).
fof(f210132, plain, (sP6(sK60, sK57) | ~ spl66_1), inference(forward_demodulation, [], [f621, f574])).
fof(f621, plain, (sP6(sK60, sK59) | ~ spl66_1), inference(avatar_component_clause, [], [f619])).
fof(f619, plain, (spl66_1 <=> sP6(sK60, sK59)), introduced(avatar_definition, [new_symbols(naming, [spl66_1])])).
fof(f291377, plain, ((sK54(sK58, sK57) = hd(cons(sK54(sK58, sK57), nil))) | ~ spl66_11238), inference(resolution, [], [f280695, f1078])).
fof(f1078, plain, ! [X6] : (~ ssItem(X6) | (hd(cons(X6, nil)) = X6)), inference(resolution, [], [f464, f455])).
fof(f464, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (hd(cons(X1, X0)) = X1)), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((hd(cons(X1, X0)) = X1) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (hd(cons(X1, X0)) = X1))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax23)).
fof(f280695, plain, (ssItem(sK54(sK58, sK57)) | ~ spl66_11238), inference(avatar_component_clause, [], [f280694])).
fof(f280694, plain, (spl66_11238 <=> ssItem(sK54(sK58, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_11238])])).
fof(f280707, plain, (! [X8] : (~ memberP(sK57, X8) | ~ ssItem(X8) | (sK54(sK58, sK57) = X8)) | ~ spl66_11240), inference(avatar_component_clause, [], [f280706])).
fof(f280706, plain, (spl66_11240 <=> ! [X8] : (~ memberP(sK57, X8) | ~ ssItem(X8) | (sK54(sK58, sK57) = X8))), introduced(avatar_definition, [new_symbols(naming, [spl66_11240])])).
fof(f998220, plain, (~ spl66_132 | ~ spl66_259 | ~ spl66_1635 | ~ spl66_3745 | ~ spl66_6386 | spl66_8391 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10843 | ~ spl66_25662 | ~ spl66_28606), inference(avatar_contradiction_clause, [], [f998219])).
fof(f998219, plain, ($false | (~ spl66_132 | ~ spl66_259 | ~ spl66_1635 | ~ spl66_3745 | ~ spl66_6386 | spl66_8391 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10843 | ~ spl66_25662 | ~ spl66_28606)), inference(subsumption_resolution, [], [f998218, f747813])).
fof(f747813, plain, (~ frontsegP(sK57, cons(sK62, sK64)) | (~ spl66_259 | spl66_8391)), inference(backward_demodulation, [], [f179126, f4601])).
fof(f4601, plain, ((sK57 = cons(sK62, nil)) | ~ spl66_259), inference(avatar_component_clause, [], [f4599])).
fof(f4599, plain, (spl66_259 <=> (sK57 = cons(sK62, nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_259])])).
fof(f179126, plain, (~ frontsegP(cons(sK62, nil), cons(sK62, sK64)) | spl66_8391), inference(avatar_component_clause, [], [f179124])).
fof(f179124, plain, (spl66_8391 <=> frontsegP(cons(sK62, nil), cons(sK62, sK64))), introduced(avatar_definition, [new_symbols(naming, [spl66_8391])])).
fof(f998218, plain, (frontsegP(sK57, cons(sK62, sK64)) | (~ spl66_132 | ~ spl66_259 | ~ spl66_1635 | ~ spl66_3745 | ~ spl66_6386 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10843 | ~ spl66_25662 | ~ spl66_28606)), inference(forward_demodulation, [], [f997937, f919211])).
fof(f919211, plain, ((cons(sK62, sK64) = app(sK57, sK64)) | (~ spl66_132 | ~ spl66_259 | ~ spl66_3745)), inference(forward_demodulation, [], [f919210, f4601])).
fof(f919210, plain, ((cons(sK62, sK64) = app(cons(sK62, nil), sK64)) | (~ spl66_132 | ~ spl66_3745)), inference(forward_demodulation, [], [f191719, f59823])).
fof(f59823, plain, ((sK62 = hd(sK63)) | ~ spl66_3745), inference(avatar_component_clause, [], [f59821])).
fof(f59821, plain, (spl66_3745 <=> (sK62 = hd(sK63))), introduced(avatar_definition, [new_symbols(naming, [spl66_3745])])).
fof(f191719, plain, ((cons(hd(sK63), sK64) = app(cons(hd(sK63), nil), sK64)) | ~ spl66_132), inference(resolution, [], [f3104, f3321])).
fof(f3321, plain, ! [X4] : (~ ssItem(X4) | (cons(X4, sK64) = app(cons(X4, nil), sK64))), inference(resolution, [], [f578, f543])).
fof(f543, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (cons(X1, X0) = app(cons(X1, nil), X0))), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (! [X1] : ((cons(X1, X0) = app(cons(X1, nil), X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (cons(X1, X0) = app(cons(X1, nil), X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax81)).
fof(f578, plain, ssList(sK64), inference(cnf_transformation, [], [f362])).
fof(f3104, plain, (ssItem(hd(sK63)) | ~ spl66_132), inference(avatar_component_clause, [], [f3103])).
fof(f3103, plain, (spl66_132 <=> ssItem(hd(sK63))), introduced(avatar_definition, [new_symbols(naming, [spl66_132])])).
fof(f997937, plain, (frontsegP(sK57, app(sK57, sK64)) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_6386 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10843 | ~ spl66_25662 | ~ spl66_28606)), inference(backward_demodulation, [], [f960090, f997728])).
fof(f997728, plain, ((sK57 = app(sK57, cons(sK61, nil))) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_6386 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10843 | ~ spl66_28606)), inference(subsumption_resolution, [], [f997727, f633])).
fof(f997727, plain, ((sK57 = app(sK57, cons(sK61, nil))) | ~ ssList(sK57) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_6386 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10843 | ~ spl66_28606)), inference(subsumption_resolution, [], [f997726, f959112])).
fof(f959112, plain, (ssList(app(sK57, cons(sK61, nil))) | (~ spl66_10798 | ~ spl66_28606)), inference(backward_demodulation, [], [f261510, f955520])).
fof(f955520, plain, ((sK57 = sK63) | ~ spl66_28606), inference(avatar_component_clause, [], [f955519])).
fof(f955519, plain, (spl66_28606 <=> (sK57 = sK63)), introduced(avatar_definition, [new_symbols(naming, [spl66_28606])])).
fof(f261510, plain, (ssList(app(sK63, cons(sK61, nil))) | ~ spl66_10798), inference(avatar_component_clause, [], [f261509])).
fof(f261509, plain, (spl66_10798 <=> ssList(app(sK63, cons(sK61, nil)))), introduced(avatar_definition, [new_symbols(naming, [spl66_10798])])).
fof(f997726, plain, ((sK57 = app(sK57, cons(sK61, nil))) | ~ ssList(app(sK57, cons(sK61, nil))) | ~ ssList(sK57) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_6386 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10843 | ~ spl66_28606)), inference(subsumption_resolution, [], [f997706, f960055])).
fof(f960055, plain, (frontsegP(sK57, app(sK57, cons(sK61, nil))) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_6386 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_28606)), inference(backward_demodulation, [], [f809449, f955520])).
fof(f809449, plain, (frontsegP(sK57, app(sK63, cons(sK61, nil))) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_6386 | ~ spl66_9493 | ~ spl66_10798)), inference(subsumption_resolution, [], [f809448, f633])).
fof(f809448, plain, (~ ssList(sK57) | frontsegP(sK57, app(sK63, cons(sK61, nil))) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_6386 | ~ spl66_9493 | ~ spl66_10798)), inference(subsumption_resolution, [], [f809447, f134667])).
fof(f134667, plain, (ssList(sK64) | ~ spl66_6386), inference(avatar_component_clause, [], [f134666])).
fof(f134666, plain, (spl66_6386 <=> ssList(sK64)), introduced(avatar_definition, [new_symbols(naming, [spl66_6386])])).
fof(f809447, plain, (~ ssList(sK64) | ~ ssList(sK57) | frontsegP(sK57, app(sK63, cons(sK61, nil))) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798)), inference(subsumption_resolution, [], [f809446, f261510])).
fof(f809446, plain, (~ ssList(app(sK63, cons(sK61, nil))) | ~ ssList(sK64) | ~ ssList(sK57) | frontsegP(sK57, app(sK63, cons(sK61, nil))) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_9493)), inference(duplicate_literal_removal, [], [f809302])).
fof(f809302, plain, (~ ssList(app(sK63, cons(sK61, nil))) | ~ ssList(sK64) | ~ ssList(sK57) | frontsegP(sK57, app(sK63, cons(sK61, nil))) | ~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_9493)), inference(resolution, [], [f16720, f748069])).
fof(f748069, plain, (! [X9] : (~ frontsegP(app(app(app(sK63, cons(sK61, nil)), sK64), sK57), X9) | frontsegP(sK57, X9) | ~ ssList(X9)) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_9493)), inference(backward_demodulation, [], [f212679, f4601])).
fof(f212679, plain, (! [X9] : (frontsegP(sK57, X9) | ~ frontsegP(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), X9) | ~ ssList(X9)) | (~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f212678, f29112])).
fof(f212678, plain, (! [X9] : (frontsegP(sK57, X9) | ~ frontsegP(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), X9) | ~ ssList(X9) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)))) | ~ spl66_9493), inference(subsumption_resolution, [], [f212583, f204648])).
fof(f212583, plain, ! [X9] : (frontsegP(sK57, X9) | ~ frontsegP(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), X9) | ~ ssList(sK65) | ~ ssList(X9) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)))), inference(superposition, [], [f490, f580])).
fof(f16720, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(X3) | ~ ssList(X4)), inference(subsumption_resolution, [], [f16719, f467])).
fof(f16719, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3))), inference(subsumption_resolution, [], [f16717, f467])).
fof(f16717, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(app(app(X2, X3), X4)) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3))), inference(duplicate_literal_removal, [], [f16710])).
fof(f16710, plain, ! [X4, X2, X3] : (frontsegP(app(app(X2, X3), X4), X2) | ~ ssList(X2) | ~ ssList(app(app(X2, X3), X4)) | ~ ssList(X3) | ~ ssList(X4) | ~ ssList(app(X2, X3)) | ~ ssList(app(app(X2, X3), X4))), inference(resolution, [], [f1963, f587])).
fof(f1963, plain, ! [X6, X4, X5] : (~ frontsegP(X4, app(X5, X6)) | frontsegP(X4, X5) | ~ ssList(X5) | ~ ssList(X4) | ~ ssList(X6)), inference(subsumption_resolution, [], [f1960, f467])).
fof(f1960, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6)), inference(duplicate_literal_removal, [], [f1957])).
fof(f1957, plain, ! [X6, X4, X5] : (frontsegP(X4, X5) | ~ frontsegP(X4, app(X5, X6)) | ~ ssList(X5) | ~ ssList(app(X5, X6)) | ~ ssList(X4) | ~ ssList(X6) | ~ ssList(X5) | ~ ssList(app(X5, X6))), inference(resolution, [], [f487, f587])).
fof(f997706, plain, ((sK57 = app(sK57, cons(sK61, nil))) | ~ frontsegP(sK57, app(sK57, cons(sK61, nil))) | ~ ssList(app(sK57, cons(sK61, nil))) | ~ ssList(sK57) | (~ spl66_10843 | ~ spl66_28606)), inference(resolution, [], [f959116, f488])).
fof(f488, plain, ! [X0, X1] : (~ frontsegP(X1, X0) | (X0 = X1) | ~ frontsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f153])).
fof(f153, plain, ! [X0] : (! [X1] : ((X0 = X1) | ~ frontsegP(X1, X0) | ~ frontsegP(X0, X1) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f152])).
fof(f152, plain, ! [X0] : (! [X1] : (((X0 = X1) | (~ frontsegP(X1, X0) | ~ frontsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ((frontsegP(X1, X0) & frontsegP(X0, X1)) => (X0 = X1)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax41)).
fof(f959116, plain, (frontsegP(app(sK57, cons(sK61, nil)), sK57) | (~ spl66_10843 | ~ spl66_28606)), inference(backward_demodulation, [], [f274485, f955520])).
fof(f274485, plain, (frontsegP(app(sK63, cons(sK61, nil)), sK57) | ~ spl66_10843), inference(avatar_component_clause, [], [f274484])).
fof(f274484, plain, (spl66_10843 <=> frontsegP(app(sK63, cons(sK61, nil)), sK57)), introduced(avatar_definition, [new_symbols(naming, [spl66_10843])])).
fof(f960090, plain, (frontsegP(sK57, app(app(sK57, cons(sK61, nil)), sK64)) | (~ spl66_25662 | ~ spl66_28606)), inference(backward_demodulation, [], [f814899, f955520])).
fof(f814899, plain, (frontsegP(sK57, app(app(sK63, cons(sK61, nil)), sK64)) | ~ spl66_25662), inference(avatar_component_clause, [], [f814897])).
fof(f814897, plain, (spl66_25662 <=> frontsegP(sK57, app(app(sK63, cons(sK61, nil)), sK64))), introduced(avatar_definition, [new_symbols(naming, [spl66_25662])])).
fof(f955527, plain, (spl66_28606 | ~ spl66_131 | ~ spl66_164 | ~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_305 | ~ spl66_327 | ~ spl66_3745 | ~ spl66_20077 | ~ spl66_25007), inference(avatar_split_clause, [], [f955526, f694724, f529605, f59821, f5190, f5057, f5009, f5005, f3866, f3862, f3249, f3099, f955519])).
fof(f3099, plain, (spl66_131 <=> ssList(tl(sK63))), introduced(avatar_definition, [new_symbols(naming, [spl66_131])])).
fof(f3249, plain, (spl66_164 <=> (sK63 = cons(hd(sK63), tl(sK63)))), introduced(avatar_definition, [new_symbols(naming, [spl66_164])])).
fof(f3862, plain, (spl66_225 <=> (sK57 = cons(sK51(sK57), sK50(sK57)))), introduced(avatar_definition, [new_symbols(naming, [spl66_225])])).
fof(f3866, plain, (spl66_226 <=> (nil = sK57)), introduced(avatar_definition, [new_symbols(naming, [spl66_226])])).
fof(f5005, plain, (spl66_293 <=> ssList(sK50(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_293])])).
fof(f5009, plain, (spl66_294 <=> ssItem(sK51(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_294])])).
fof(f5057, plain, (spl66_305 <=> (nil = sK50(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_305])])).
fof(f5190, plain, (spl66_327 <=> ssItem(hd(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_327])])).
fof(f694724, plain, (spl66_25007 <=> frontsegP(sK57, sK63)), introduced(avatar_definition, [new_symbols(naming, [spl66_25007])])).
fof(f955526, plain, ((sK57 = sK63) | (~ spl66_131 | ~ spl66_164 | ~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_305 | ~ spl66_327 | ~ spl66_3745 | ~ spl66_20077 | ~ spl66_25007)), inference(subsumption_resolution, [], [f955439, f3100])).
fof(f3100, plain, (ssList(tl(sK63)) | ~ spl66_131), inference(avatar_component_clause, [], [f3099])).
fof(f955439, plain, ((sK57 = sK63) | ~ ssList(tl(sK63)) | (~ spl66_164 | ~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_305 | ~ spl66_327 | ~ spl66_3745 | ~ spl66_20077 | ~ spl66_25007)), inference(subsumption_resolution, [], [f955371, f694725])).
fof(f694725, plain, (frontsegP(sK57, sK63) | ~ spl66_25007), inference(avatar_component_clause, [], [f694724])).
fof(f955371, plain, (~ frontsegP(sK57, sK63) | (sK57 = sK63) | ~ ssList(tl(sK63)) | (~ spl66_164 | ~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_305 | ~ spl66_327 | ~ spl66_3745 | ~ spl66_20077)), inference(superposition, [], [f747535, f933840])).
fof(f933840, plain, ((sK63 = cons(sK62, tl(sK63))) | (~ spl66_164 | ~ spl66_3745)), inference(forward_demodulation, [], [f3251, f59823])).
fof(f3251, plain, ((sK63 = cons(hd(sK63), tl(sK63))) | ~ spl66_164), inference(avatar_component_clause, [], [f3249])).
fof(f747535, plain, (! [X59] : (~ frontsegP(sK57, cons(sK62, X59)) | (sK57 = cons(sK62, X59)) | ~ ssList(X59)) | (~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_305 | ~ spl66_327 | ~ spl66_20077)), inference(forward_demodulation, [], [f747455, f529607])).
fof(f529607, plain, ((sK62 = hd(sK57)) | ~ spl66_20077), inference(avatar_component_clause, [], [f529605])).
fof(f747455, plain, (! [X59] : (~ frontsegP(sK57, cons(sK62, X59)) | ~ ssList(X59) | (sK57 = cons(hd(sK57), X59))) | (~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_305 | ~ spl66_327 | ~ spl66_20077)), inference(backward_demodulation, [], [f745773, f529607])).
fof(f745773, plain, (! [X59] : (~ frontsegP(sK57, cons(hd(sK57), X59)) | ~ ssList(X59) | (sK57 = cons(hd(sK57), X59))) | (~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_305 | ~ spl66_327)), inference(subsumption_resolution, [], [f745772, f494])).
fof(f494, plain, ! [X0] : (frontsegP(X0, nil) | ~ ssList(X0)), inference(cnf_transformation, [], [f158])).
fof(f158, plain, ! [X0] : (frontsegP(X0, nil) | ~ ssList(X0)), inference(ennf_transformation, [], [f45])).
fof(f45, plain, ! [X0] : (ssList(X0) => frontsegP(X0, nil)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax45)).
fof(f745772, plain, (! [X59] : (~ frontsegP(X59, nil) | ~ frontsegP(sK57, cons(hd(sK57), X59)) | ~ ssList(X59) | (sK57 = cons(hd(sK57), X59))) | (~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_305 | ~ spl66_327)), inference(forward_demodulation, [], [f533628, f5059])).
fof(f5059, plain, ((nil = sK50(sK57)) | ~ spl66_305), inference(avatar_component_clause, [], [f5057])).
fof(f533628, plain, (! [X59] : (~ frontsegP(sK57, cons(hd(sK57), X59)) | ~ ssList(X59) | (sK57 = cons(hd(sK57), X59)) | ~ frontsegP(X59, sK50(sK57))) | (~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294 | ~ spl66_327)), inference(subsumption_resolution, [], [f533627, f5191])).
fof(f5191, plain, (ssItem(hd(sK57)) | ~ spl66_327), inference(avatar_component_clause, [], [f5190])).
fof(f533627, plain, (! [X59] : (~ frontsegP(sK57, cons(hd(sK57), X59)) | ~ ssList(X59) | ~ ssItem(hd(sK57)) | (sK57 = cons(hd(sK57), X59)) | ~ frontsegP(X59, sK50(sK57))) | (~ spl66_225 | spl66_226 | ~ spl66_293 | ~ spl66_294)), inference(subsumption_resolution, [], [f530311, f5006])).
fof(f5006, plain, (ssList(sK50(sK57)) | ~ spl66_293), inference(avatar_component_clause, [], [f5005])).
fof(f530311, plain, (! [X59] : (~ frontsegP(sK57, cons(hd(sK57), X59)) | ~ ssList(sK50(sK57)) | ~ ssList(X59) | ~ ssItem(hd(sK57)) | (sK57 = cons(hd(sK57), X59)) | ~ frontsegP(X59, sK50(sK57))) | (~ spl66_225 | spl66_226 | ~ spl66_294)), inference(superposition, [], [f2077, f509252])).
fof(f509252, plain, ((sK57 = cons(hd(sK57), sK50(sK57))) | (~ spl66_225 | spl66_226 | ~ spl66_294)), inference(backward_demodulation, [], [f3864, f509251])).
fof(f509251, plain, ((sK51(sK57) = hd(sK57)) | (~ spl66_225 | spl66_226 | ~ spl66_294)), inference(forward_demodulation, [], [f509245, f3864])).
fof(f509245, plain, ((sK51(sK57) = hd(cons(sK51(sK57), sK50(sK57)))) | (spl66_226 | ~ spl66_294)), inference(resolution, [], [f212171, f5010])).
fof(f5010, plain, (ssItem(sK51(sK57)) | ~ spl66_294), inference(avatar_component_clause, [], [f5009])).
fof(f212171, plain, (! [X29] : (~ ssItem(X29) | (hd(cons(X29, sK50(sK57))) = X29)) | spl66_226), inference(subsumption_resolution, [], [f211928, f3867])).
fof(f3867, plain, (~ (nil = sK57) | spl66_226), inference(avatar_component_clause, [], [f3866])).
fof(f211928, plain, ! [X29] : ((hd(cons(X29, sK50(sK57))) = X29) | (nil = sK57) | ~ ssItem(X29)), inference(resolution, [], [f633, f1100])).
fof(f1100, plain, ! [X50, X49] : (~ ssList(X50) | (hd(cons(X49, sK50(X50))) = X49) | (nil = X50) | ~ ssItem(X49)), inference(resolution, [], [f464, f459])).
fof(f459, plain, ! [X0] : (ssList(sK50(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f322])).
fof(f322, plain, ! [X0] : ((((cons(sK51(X0), sK50(X0)) = X0) & ssItem(sK51(X0))) & ssList(sK50(X0))) | (nil = X0) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK50, sK51])], [f125, f321, f320])).
fof(f320, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) => (? [X2] : ((cons(X2, sK50(X0)) = X0) & ssItem(X2)) & ssList(sK50(X0)))), introduced(choice_axiom, [])).
fof(f321, plain, ! [X0] : (? [X2] : ((cons(X2, sK50(X0)) = X0) & ssItem(X2)) => ((cons(sK51(X0), sK50(X0)) = X0) & ssItem(sK51(X0)))), introduced(choice_axiom, [])).
fof(f125, plain, ! [X0] : (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f124])).
fof(f124, plain, ! [X0] : ((? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : (ssList(X0) => (? [X1] : (? [X2] : ((cons(X2, X1) = X0) & ssItem(X2)) & ssList(X1)) | (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax20)).
fof(f3864, plain, ((sK57 = cons(sK51(sK57), sK50(sK57))) | ~ spl66_225), inference(avatar_component_clause, [], [f3862])).
fof(f2077, plain, ! [X8, X7, X9] : (~ frontsegP(cons(X9, X8), cons(X9, X7)) | ~ ssList(X8) | ~ ssList(X7) | ~ ssItem(X9) | (cons(X9, X8) = cons(X9, X7)) | ~ frontsegP(X7, X8)), inference(subsumption_resolution, [], [f2076, f454])).
fof(f454, plain, ! [X0, X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0] : (! [X1] : (ssList(cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ssList(cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax16)).
fof(f2076, plain, ! [X8, X7, X9] : (~ frontsegP(X7, X8) | ~ ssList(X8) | ~ ssList(X7) | ~ ssItem(X9) | (cons(X9, X8) = cons(X9, X7)) | ~ frontsegP(cons(X9, X8), cons(X9, X7)) | ~ ssList(cons(X9, X8))), inference(subsumption_resolution, [], [f2071, f454])).
fof(f2071, plain, ! [X8, X7, X9] : (~ frontsegP(X7, X8) | ~ ssList(X8) | ~ ssList(X7) | ~ ssItem(X9) | (cons(X9, X8) = cons(X9, X7)) | ~ frontsegP(cons(X9, X8), cons(X9, X7)) | ~ ssList(cons(X9, X7)) | ~ ssList(cons(X9, X8))), inference(resolution, [], [f613, f488])).
fof(f613, plain, ! [X2, X3, X1] : (frontsegP(cons(X1, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1)), inference(duplicate_literal_removal, [], [f600])).
fof(f600, plain, ! [X2, X3, X1] : (frontsegP(cons(X1, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X1)), inference(equality_resolution, [], [f493])).
fof(f493, plain, ! [X2, X0, X3, X1] : (frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f330])).
fof(f330, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | ~ frontsegP(X2, X3) | ~ (X0 = X1)) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f329])).
fof(f329, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : (((frontsegP(cons(X0, X2), cons(X1, X3)) | (~ frontsegP(X2, X3) | ~ (X0 = X1))) & ((frontsegP(X2, X3) & (X0 = X1)) | ~ frontsegP(cons(X0, X2), cons(X1, X3)))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f157])).
fof(f157, plain, ! [X0] : (! [X1] : (! [X2] : (! [X3] : ((frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))) | ~ ssList(X3)) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f44])).
fof(f44, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => ! [X3] : (ssList(X3) => (frontsegP(cons(X0, X2), cons(X1, X3)) <=> (frontsegP(X2, X3) & (X0 = X1))))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax44)).
fof(f916397, plain, (~ spl66_29 | spl66_38 | ~ spl66_259 | ~ spl66_334 | ~ spl66_1389 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_20077 | ~ spl66_25892), inference(avatar_contradiction_clause, [], [f916396])).
fof(f916396, plain, ($false | (~ spl66_29 | spl66_38 | ~ spl66_259 | ~ spl66_334 | ~ spl66_1389 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_20077 | ~ spl66_25892)), inference(subsumption_resolution, [], [f916395, f824949])).
fof(f824949, plain, (ssList(app(sK64, sK57)) | ~ spl66_25892), inference(avatar_component_clause, [], [f824948])).
fof(f824948, plain, (spl66_25892 <=> ssList(app(sK64, sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_25892])])).
fof(f916395, plain, (~ ssList(app(sK64, sK57)) | (~ spl66_29 | spl66_38 | ~ spl66_259 | ~ spl66_334 | ~ spl66_1389 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_20077)), inference(subsumption_resolution, [], [f916394, f575])).
fof(f575, plain, ssItem(sK61), inference(cnf_transformation, [], [f362])).
fof(f916394, plain, (~ ssItem(sK61) | ~ ssList(app(sK64, sK57)) | (~ spl66_29 | spl66_38 | ~ spl66_259 | ~ spl66_334 | ~ spl66_1389 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_20077)), inference(subsumption_resolution, [], [f916370, f1571])).
fof(f1571, plain, (~ (sK61 = sK62) | spl66_38), inference(avatar_component_clause, [], [f1570])).
fof(f1570, plain, (spl66_38 <=> (sK61 = sK62)), introduced(avatar_definition, [new_symbols(naming, [spl66_38])])).
fof(f916370, plain, ((sK61 = sK62) | ~ ssItem(sK61) | ~ ssList(app(sK64, sK57)) | (~ spl66_29 | ~ spl66_259 | ~ spl66_334 | ~ spl66_1389 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_20077)), inference(resolution, [], [f877072, f747312])).
fof(f747312, plain, (! [X12, X13] : (~ frontsegP(sK57, cons(X12, X13)) | (sK62 = X12) | ~ ssItem(X12) | ~ ssList(X13)) | (~ spl66_334 | ~ spl66_20077)), inference(backward_demodulation, [], [f5222, f529607])).
fof(f5222, plain, (! [X12, X13] : (~ frontsegP(sK57, cons(X12, X13)) | ~ ssItem(X12) | ~ ssList(X13) | (hd(sK57) = X12)) | ~ spl66_334), inference(avatar_component_clause, [], [f5221])).
fof(f5221, plain, (spl66_334 <=> ! [X13, X12] : (~ frontsegP(sK57, cons(X12, X13)) | ~ ssItem(X12) | ~ ssList(X13) | (hd(sK57) = X12))), introduced(avatar_definition, [new_symbols(naming, [spl66_334])])).
fof(f877072, plain, (frontsegP(sK57, cons(sK61, app(sK64, sK57))) | (~ spl66_29 | ~ spl66_259 | ~ spl66_1389 | ~ spl66_1635 | ~ spl66_9493)), inference(backward_demodulation, [], [f874738, f783258])).
fof(f783258, plain, (app(cons(sK61, sK64), sK57) = cons(sK61, app(sK64, sK57))), inference(resolution, [], [f212145, f575])).
fof(f212145, plain, ! [X248] : (~ ssItem(X248) | (cons(X248, app(sK64, sK57)) = app(cons(X248, sK64), sK57))), inference(resolution, [], [f633, f3316])).
fof(f3316, plain, ! [X2, X3] : (~ ssList(X3) | (cons(X2, app(sK64, X3)) = app(cons(X2, sK64), X3)) | ~ ssItem(X2)), inference(resolution, [], [f578, f468])).
fof(f468, plain, ! [X2, X0, X1] : (~ ssList(X1) | ~ ssItem(X2) | (cons(X2, app(X1, X0)) = app(cons(X2, X1), X0)) | ~ ssList(X0)), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ! [X0] : (! [X1] : (! [X2] : ((cons(X2, app(X1, X0)) = app(cons(X2, X1), X0)) | ~ ssItem(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssItem(X2) => (cons(X2, app(X1, X0)) = app(cons(X2, X1), X0))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax27)).
fof(f874738, plain, (frontsegP(sK57, app(cons(sK61, sK64), sK57)) | (~ spl66_29 | ~ spl66_259 | ~ spl66_1389 | ~ spl66_1635 | ~ spl66_9493)), inference(backward_demodulation, [], [f874218, f10411])).
fof(f10411, plain, (cons(sK61, sK64) = app(cons(sK61, nil), sK64)), inference(resolution, [], [f3321, f575])).
fof(f874218, plain, (frontsegP(sK57, app(app(cons(sK61, nil), sK64), sK57)) | (~ spl66_29 | ~ spl66_259 | ~ spl66_1389 | ~ spl66_1635 | ~ spl66_9493)), inference(backward_demodulation, [], [f873645, f26501])).
fof(f26501, plain, ((cons(sK61, nil) = app(nil, cons(sK61, nil))) | ~ spl66_1389), inference(resolution, [], [f26367, f469])).
fof(f469, plain, ! [X0] : (~ ssList(X0) | (app(nil, X0) = X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : ((app(nil, X0) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f28])).
fof(f28, plain, ! [X0] : (ssList(X0) => (app(nil, X0) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax28)).
fof(f26367, plain, (ssList(cons(sK61, nil)) | ~ spl66_1389), inference(avatar_component_clause, [], [f26366])).
fof(f26366, plain, (spl66_1389 <=> ssList(cons(sK61, nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_1389])])).
fof(f873645, plain, (frontsegP(sK57, app(app(app(nil, cons(sK61, nil)), sK64), sK57)) | (~ spl66_29 | ~ spl66_259 | ~ spl66_1635 | ~ spl66_9493)), inference(forward_demodulation, [], [f748072, f1204])).
fof(f1204, plain, ((nil = sK63) | ~ spl66_29), inference(avatar_component_clause, [], [f1202])).
fof(f1202, plain, (spl66_29 <=> (nil = sK63)), introduced(avatar_definition, [new_symbols(naming, [spl66_29])])).
fof(f748072, plain, (frontsegP(sK57, app(app(app(sK63, cons(sK61, nil)), sK64), sK57)) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_9493)), inference(backward_demodulation, [], [f212690, f4601])).
fof(f854577, plain, (spl66_164 | spl66_29), inference(avatar_split_clause, [], [f28883, f1202, f3249])).
fof(f28883, plain, ((nil = sK63) | (sK63 = cons(hd(sK63), tl(sK63)))), inference(resolution, [], [f577, f540])).
fof(f540, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(hd(X0), tl(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : ((cons(hd(X0), tl(X0)) = X0) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f193])).
fof(f193, plain, ! [X0] : (((cons(hd(X0), tl(X0)) = X0) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f78])).
fof(f78, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => (cons(hd(X0), tl(X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax78)).
fof(f577, plain, ssList(sK63), inference(cnf_transformation, [], [f362])).
fof(f851773, plain, (spl66_3745 | ~ spl66_131 | ~ spl66_132 | ~ spl66_164 | ~ spl66_334 | ~ spl66_20077 | ~ spl66_25007), inference(avatar_split_clause, [], [f851772, f694724, f529605, f5221, f3249, f3103, f3099, f59821])).
fof(f851772, plain, ((sK62 = hd(sK63)) | (~ spl66_131 | ~ spl66_132 | ~ spl66_164 | ~ spl66_334 | ~ spl66_20077 | ~ spl66_25007)), inference(subsumption_resolution, [], [f851771, f3100])).
fof(f851771, plain, ((sK62 = hd(sK63)) | ~ ssList(tl(sK63)) | (~ spl66_132 | ~ spl66_164 | ~ spl66_334 | ~ spl66_20077 | ~ spl66_25007)), inference(subsumption_resolution, [], [f851770, f3104])).
fof(f851770, plain, ((sK62 = hd(sK63)) | ~ ssItem(hd(sK63)) | ~ ssList(tl(sK63)) | (~ spl66_164 | ~ spl66_334 | ~ spl66_20077 | ~ spl66_25007)), inference(subsumption_resolution, [], [f773797, f694725])).
fof(f773797, plain, (~ frontsegP(sK57, sK63) | (sK62 = hd(sK63)) | ~ ssItem(hd(sK63)) | ~ ssList(tl(sK63)) | (~ spl66_164 | ~ spl66_334 | ~ spl66_20077)), inference(superposition, [], [f747312, f3251])).
fof(f851749, plain, (spl66_25007 | ~ spl66_1389 | ~ spl66_6386 | ~ spl66_10800 | ~ spl66_25663), inference(avatar_split_clause, [], [f851748, f814903, f263022, f134666, f26366, f694724])).
fof(f263022, plain, (spl66_10800 <=> ssList(sK63)), introduced(avatar_definition, [new_symbols(naming, [spl66_10800])])).
fof(f851748, plain, (frontsegP(sK57, sK63) | (~ spl66_1389 | ~ spl66_6386 | ~ spl66_10800 | ~ spl66_25663)), inference(subsumption_resolution, [], [f851747, f134667])).
fof(f851747, plain, (frontsegP(sK57, sK63) | ~ ssList(sK64) | (~ spl66_1389 | ~ spl66_10800 | ~ spl66_25663)), inference(subsumption_resolution, [], [f851746, f26367])).
fof(f851746, plain, (frontsegP(sK57, sK63) | ~ ssList(cons(sK61, nil)) | ~ ssList(sK64) | (~ spl66_10800 | ~ spl66_25663)), inference(subsumption_resolution, [], [f851738, f263023])).
fof(f263023, plain, (ssList(sK63) | ~ spl66_10800), inference(avatar_component_clause, [], [f263022])).
fof(f851738, plain, (frontsegP(sK57, sK63) | ~ ssList(sK63) | ~ ssList(cons(sK61, nil)) | ~ ssList(sK64) | ~ spl66_25663), inference(duplicate_literal_removal, [], [f851730])).
fof(f851730, plain, (frontsegP(sK57, sK63) | ~ ssList(sK63) | ~ ssList(sK63) | ~ ssList(cons(sK61, nil)) | ~ ssList(sK64) | ~ spl66_25663), inference(resolution, [], [f814904, f16720])).
fof(f814904, plain, (! [X0] : (~ frontsegP(app(app(sK63, cons(sK61, nil)), sK64), X0) | frontsegP(sK57, X0) | ~ ssList(X0)) | ~ spl66_25663), inference(avatar_component_clause, [], [f814903])).
fof(f825012, plain, (~ spl66_6386 | spl66_25892), inference(avatar_contradiction_clause, [], [f825011])).
fof(f825011, plain, ($false | (~ spl66_6386 | spl66_25892)), inference(subsumption_resolution, [], [f825010, f134667])).
fof(f825010, plain, (~ ssList(sK64) | spl66_25892), inference(subsumption_resolution, [], [f825009, f633])).
fof(f825009, plain, (~ ssList(sK57) | ~ ssList(sK64) | spl66_25892), inference(resolution, [], [f824950, f467])).
fof(f824950, plain, (~ ssList(app(sK64, sK57)) | spl66_25892), inference(avatar_component_clause, [], [f824948])).
fof(f816039, plain, (~ spl66_6386 | spl66_9488 | ~ spl66_10798), inference(avatar_contradiction_clause, [], [f816038])).
fof(f816038, plain, ($false | (~ spl66_6386 | spl66_9488 | ~ spl66_10798)), inference(subsumption_resolution, [], [f816037, f261510])).
fof(f816037, plain, (~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_6386 | spl66_9488)), inference(subsumption_resolution, [], [f816036, f134667])).
fof(f816036, plain, (~ ssList(sK64) | ~ ssList(app(sK63, cons(sK61, nil))) | spl66_9488), inference(resolution, [], [f204002, f467])).
fof(f204002, plain, (~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | spl66_9488), inference(avatar_component_clause, [], [f204000])).
fof(f814914, plain, (~ spl66_9488 | spl66_25662 | ~ spl66_259 | ~ spl66_1635 | ~ spl66_9493), inference(avatar_split_clause, [], [f814913, f204647, f29111, f4599, f814897, f204000])).
fof(f814913, plain, (frontsegP(sK57, app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f752411, f633])).
fof(f752411, plain, (frontsegP(sK57, app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(sK57) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_9493)), inference(duplicate_literal_removal, [], [f752392])).
fof(f752392, plain, (frontsegP(sK57, app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(sK57) | ~ ssList(sK57) | (~ spl66_259 | ~ spl66_1635 | ~ spl66_9493)), inference(resolution, [], [f748072, f1963])).
fof(f701779, plain, (spl66_31 | ~ spl66_259 | ~ spl66_338 | ~ spl66_8390), inference(avatar_split_clause, [], [f701778, f179120, f5238, f4599, f1211])).
fof(f1211, plain, (spl66_31 <=> (nil = sK64)), introduced(avatar_definition, [new_symbols(naming, [spl66_31])])).
fof(f5238, plain, (spl66_338 <=> (nil = tl(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_338])])).
fof(f179120, plain, (spl66_8390 <=> (cons(sK62, nil) = cons(sK62, sK64))), introduced(avatar_definition, [new_symbols(naming, [spl66_8390])])).
fof(f701778, plain, ((nil = sK64) | (~ spl66_259 | ~ spl66_338 | ~ spl66_8390)), inference(forward_demodulation, [], [f701777, f5240])).
fof(f5240, plain, ((nil = tl(sK57)) | ~ spl66_338), inference(avatar_component_clause, [], [f5238])).
fof(f701777, plain, ((sK64 = tl(sK57)) | (~ spl66_259 | ~ spl66_8390)), inference(forward_demodulation, [], [f6299, f700222])).
fof(f700222, plain, ((sK57 = cons(sK62, sK64)) | (~ spl66_259 | ~ spl66_8390)), inference(forward_demodulation, [], [f179122, f4601])).
fof(f179122, plain, ((cons(sK62, nil) = cons(sK62, sK64)) | ~ spl66_8390), inference(avatar_component_clause, [], [f179120])).
fof(f6299, plain, (sK64 = tl(cons(sK62, sK64))), inference(resolution, [], [f1138, f576])).
fof(f1138, plain, ! [X60] : (~ ssItem(X60) | (sK64 = tl(cons(X60, sK64)))), inference(resolution, [], [f466, f578])).
fof(f466, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssItem(X1) | (tl(cons(X1, X0)) = X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (! [X1] : ((tl(cons(X1, X0)) = X0) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => (tl(cons(X1, X0)) = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax25)).
fof(f694703, plain, (~ spl66_1 | ~ spl66_31 | spl66_38 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10800 | ~ spl66_11238 | ~ spl66_11240 | ~ spl66_20077), inference(avatar_contradiction_clause, [], [f694702])).
fof(f694702, plain, ($false | (~ spl66_1 | ~ spl66_31 | spl66_38 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10800 | ~ spl66_11238 | ~ spl66_11240 | ~ spl66_20077)), inference(subsumption_resolution, [], [f694701, f575])).
fof(f694701, plain, (~ ssItem(sK61) | (~ spl66_1 | ~ spl66_31 | spl66_38 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10800 | ~ spl66_11238 | ~ spl66_11240 | ~ spl66_20077)), inference(subsumption_resolution, [], [f694693, f1571])).
fof(f694693, plain, ((sK61 = sK62) | ~ ssItem(sK61) | (~ spl66_1 | ~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10800 | ~ spl66_11238 | ~ spl66_11240 | ~ spl66_20077)), inference(resolution, [], [f694688, f534466])).
fof(f534466, plain, (! [X8] : (~ memberP(sK57, X8) | (sK62 = X8) | ~ ssItem(X8)) | (~ spl66_1 | ~ spl66_11238 | ~ spl66_11240 | ~ spl66_20077)), inference(backward_demodulation, [], [f291422, f529607])).
fof(f694688, plain, (memberP(sK57, sK61) | (~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10800)), inference(subsumption_resolution, [], [f694687, f261510])).
fof(f694687, plain, (memberP(sK57, sK61) | ~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798 | ~ spl66_10800)), inference(subsumption_resolution, [], [f694686, f263023])).
fof(f694686, plain, (memberP(sK57, sK61) | ~ ssList(sK63) | ~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798)), inference(subsumption_resolution, [], [f694685, f455])).
fof(f694685, plain, (memberP(sK57, sK61) | ~ ssList(nil) | ~ ssList(sK63) | ~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798)), inference(subsumption_resolution, [], [f694684, f575])).
fof(f694684, plain, (~ ssItem(sK61) | memberP(sK57, sK61) | ~ ssList(nil) | ~ ssList(sK63) | ~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798)), inference(duplicate_literal_removal, [], [f694679])).
fof(f694679, plain, (~ ssItem(sK61) | memberP(sK57, sK61) | ~ ssList(nil) | ~ ssList(sK63) | ~ ssItem(sK61) | ~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798)), inference(resolution, [], [f276684, f585])).
fof(f276684, plain, (! [X1] : (~ memberP(app(sK63, cons(sK61, nil)), X1) | ~ ssItem(X1) | memberP(sK57, X1)) | (~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493 | ~ spl66_10798)), inference(subsumption_resolution, [], [f276683, f261510])).
fof(f276683, plain, (! [X1] : (memberP(sK57, X1) | ~ ssItem(X1) | ~ memberP(app(sK63, cons(sK61, nil)), X1) | ~ ssList(app(sK63, cons(sK61, nil)))) | (~ spl66_31 | ~ spl66_62 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f276679, f2275])).
fof(f276679, plain, (! [X1] : (memberP(sK57, X1) | ~ ssItem(X1) | ~ memberP(app(sK63, cons(sK61, nil)), X1) | ~ ssList(cons(sK62, nil)) | ~ ssList(app(sK63, cons(sK61, nil)))) | (~ spl66_31 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493)), inference(duplicate_literal_removal, [], [f276676])).
fof(f276676, plain, (! [X1] : (memberP(sK57, X1) | ~ ssItem(X1) | ~ memberP(app(sK63, cons(sK61, nil)), X1) | ~ ssList(cons(sK62, nil)) | ~ ssList(app(sK63, cons(sK61, nil))) | ~ ssItem(X1)) | (~ spl66_31 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493)), inference(resolution, [], [f260913, f480])).
fof(f260913, plain, (! [X7] : (~ memberP(app(app(sK63, cons(sK61, nil)), cons(sK62, nil)), X7) | memberP(sK57, X7) | ~ ssItem(X7)) | (~ spl66_31 | ~ spl66_879 | ~ spl66_1635 | ~ spl66_9493)), inference(forward_demodulation, [], [f260912, f247167])).
fof(f247167, plain, ((app(sK63, cons(sK61, nil)) = app(app(sK63, cons(sK61, nil)), nil)) | (~ spl66_31 | ~ spl66_879)), inference(backward_demodulation, [], [f203553, f1213])).
fof(f1213, plain, ((nil = sK64) | ~ spl66_31), inference(avatar_component_clause, [], [f1211])).
fof(f203553, plain, ((app(sK63, cons(sK61, sK64)) = app(app(sK63, cons(sK61, sK64)), nil)) | ~ spl66_879), inference(resolution, [], [f28917, f20083])).
fof(f20083, plain, (ssList(cons(sK61, sK64)) | ~ spl66_879), inference(avatar_component_clause, [], [f20082])).
fof(f20082, plain, (spl66_879 <=> ssList(cons(sK61, sK64))), introduced(avatar_definition, [new_symbols(naming, [spl66_879])])).
fof(f28917, plain, ! [X11] : (~ ssList(X11) | (app(sK63, X11) = app(app(sK63, X11), nil))), inference(resolution, [], [f577, f956])).
fof(f956, plain, ! [X0, X1] : (~ ssList(X1) | ~ ssList(X0) | (app(X1, X0) = app(app(X1, X0), nil))), inference(resolution, [], [f467, f548])).
fof(f548, plain, ! [X0] : (~ ssList(X0) | (app(X0, nil) = X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : ((app(X0, nil) = X0) | ~ ssList(X0)), inference(ennf_transformation, [], [f84])).
fof(f84, plain, ! [X0] : (ssList(X0) => (app(X0, nil) = X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax84)).
fof(f260912, plain, (! [X7] : (~ memberP(app(app(app(sK63, cons(sK61, nil)), nil), cons(sK62, nil)), X7) | memberP(sK57, X7) | ~ ssItem(X7)) | (~ spl66_31 | ~ spl66_1635 | ~ spl66_9493)), inference(forward_demodulation, [], [f212675, f1213])).
fof(f630245, plain, (~ spl66_1 | ~ spl66_131 | ~ spl66_164 | ~ spl66_1389 | ~ spl66_3745 | ~ spl66_10800 | spl66_10843 | ~ spl66_11238 | ~ spl66_11255 | ~ spl66_20077), inference(avatar_contradiction_clause, [], [f630244])).
fof(f630244, plain, ($false | (~ spl66_1 | ~ spl66_131 | ~ spl66_164 | ~ spl66_1389 | ~ spl66_3745 | ~ spl66_10800 | spl66_10843 | ~ spl66_11238 | ~ spl66_11255 | ~ spl66_20077)), inference(subsumption_resolution, [], [f630243, f3100])).
fof(f630243, plain, (~ ssList(tl(sK63)) | (~ spl66_1 | ~ spl66_164 | ~ spl66_1389 | ~ spl66_3745 | ~ spl66_10800 | spl66_10843 | ~ spl66_11238 | ~ spl66_11255 | ~ spl66_20077)), inference(subsumption_resolution, [], [f630196, f325088])).
fof(f325088, plain, (~ frontsegP(sK63, sK57) | (~ spl66_1389 | ~ spl66_10800 | spl66_10843)), inference(subsumption_resolution, [], [f325087, f263023])).
fof(f325087, plain, (~ frontsegP(sK63, sK57) | ~ ssList(sK63) | (~ spl66_1389 | spl66_10843)), inference(subsumption_resolution, [], [f325086, f633])).
fof(f325086, plain, (~ frontsegP(sK63, sK57) | ~ ssList(sK57) | ~ ssList(sK63) | (~ spl66_1389 | spl66_10843)), inference(subsumption_resolution, [], [f325085, f26367])).
fof(f325085, plain, (~ frontsegP(sK63, sK57) | ~ ssList(cons(sK61, nil)) | ~ ssList(sK57) | ~ ssList(sK63) | spl66_10843), inference(resolution, [], [f274486, f490])).
fof(f274486, plain, (~ frontsegP(app(sK63, cons(sK61, nil)), sK57) | spl66_10843), inference(avatar_component_clause, [], [f274484])).
fof(f630196, plain, (frontsegP(sK63, sK57) | ~ ssList(tl(sK63)) | (~ spl66_1 | ~ spl66_164 | ~ spl66_3745 | ~ spl66_11238 | ~ spl66_11255 | ~ spl66_20077)), inference(superposition, [], [f534467, f629244])).
fof(f629244, plain, ((sK63 = cons(sK62, tl(sK63))) | (~ spl66_164 | ~ spl66_3745)), inference(backward_demodulation, [], [f3251, f59823])).
fof(f534467, plain, (! [X51] : (frontsegP(cons(sK62, X51), sK57) | ~ ssList(X51)) | (~ spl66_1 | ~ spl66_11238 | ~ spl66_11255 | ~ spl66_20077)), inference(backward_demodulation, [], [f291437, f529607])).
fof(f291437, plain, (! [X51] : (frontsegP(cons(hd(sK57), X51), sK57) | ~ ssList(X51)) | (~ spl66_1 | ~ spl66_11238 | ~ spl66_11255)), inference(backward_demodulation, [], [f280783, f291418])).
fof(f280783, plain, (! [X51] : (frontsegP(cons(sK54(sK58, sK57), X51), sK57) | ~ ssList(X51)) | ~ spl66_11255), inference(avatar_component_clause, [], [f280782])).
fof(f280782, plain, (spl66_11255 <=> ! [X51] : (frontsegP(cons(sK54(sK58, sK57), X51), sK57) | ~ ssList(X51))), introduced(avatar_definition, [new_symbols(naming, [spl66_11255])])).
fof(f609079, plain, (spl66_305 | ~ spl66_225 | spl66_226 | ~ spl66_294 | ~ spl66_338 | ~ spl66_20077), inference(avatar_split_clause, [], [f609076, f529605, f5238, f5009, f3866, f3862, f5057])).
fof(f609076, plain, ((nil = sK50(sK57)) | (~ spl66_225 | spl66_226 | ~ spl66_294 | ~ spl66_338 | ~ spl66_20077)), inference(forward_demodulation, [], [f609075, f5240])).
fof(f609075, plain, ((sK50(sK57) = tl(sK57)) | (~ spl66_225 | spl66_226 | ~ spl66_294 | ~ spl66_20077)), inference(forward_demodulation, [], [f609074, f534582])).
fof(f534582, plain, ((sK57 = cons(sK62, sK50(sK57))) | (~ spl66_225 | spl66_226 | ~ spl66_294 | ~ spl66_20077)), inference(backward_demodulation, [], [f509252, f529607])).
fof(f609074, plain, ((sK50(sK57) = tl(cons(sK62, sK50(sK57)))) | spl66_226), inference(resolution, [], [f212175, f576])).
fof(f212175, plain, (! [X47] : (~ ssItem(X47) | (sK50(sK57) = tl(cons(X47, sK50(sK57))))) | spl66_226), inference(subsumption_resolution, [], [f211944, f3867])).
fof(f211944, plain, ! [X47] : ((sK50(sK57) = tl(cons(X47, sK50(sK57)))) | (nil = sK57) | ~ ssItem(X47)), inference(resolution, [], [f633, f1133])).
fof(f1133, plain, ! [X50, X49] : (~ ssList(X50) | (sK50(X50) = tl(cons(X49, sK50(X50)))) | (nil = X50) | ~ ssItem(X49)), inference(resolution, [], [f466, f459])).
fof(f534120, plain, (spl66_338 | ~ spl66_1 | ~ spl66_327 | ~ spl66_11238), inference(avatar_split_clause, [], [f534119, f280694, f5190, f619, f5238])).
fof(f534119, plain, ((nil = tl(sK57)) | (~ spl66_1 | ~ spl66_327 | ~ spl66_11238)), inference(forward_demodulation, [], [f447499, f291419])).
fof(f291419, plain, ((sK57 = cons(hd(sK57), nil)) | (~ spl66_1 | ~ spl66_11238)), inference(backward_demodulation, [], [f212237, f291418])).
fof(f447499, plain, ((nil = tl(cons(hd(sK57), nil))) | ~ spl66_327), inference(resolution, [], [f5191, f1111])).
fof(f1111, plain, ! [X6] : (~ ssItem(X6) | (nil = tl(cons(X6, nil)))), inference(resolution, [], [f466, f455])).
fof(f529869, plain, (spl66_259 | ~ spl66_338 | ~ spl66_13086 | ~ spl66_15126 | ~ spl66_20077), inference(avatar_contradiction_clause, [], [f529868])).
fof(f529868, plain, ($false | (spl66_259 | ~ spl66_338 | ~ spl66_13086 | ~ spl66_15126 | ~ spl66_20077)), inference(subsumption_resolution, [], [f529690, f4600])).
fof(f4600, plain, (~ (sK57 = cons(sK62, nil)) | spl66_259), inference(avatar_component_clause, [], [f4599])).
fof(f529690, plain, ((sK57 = cons(sK62, nil)) | (~ spl66_338 | ~ spl66_13086 | ~ spl66_15126 | ~ spl66_20077)), inference(backward_demodulation, [], [f375778, f529607])).
fof(f375778, plain, ((sK57 = cons(hd(sK57), nil)) | (~ spl66_338 | ~ spl66_13086 | ~ spl66_15126)), inference(forward_demodulation, [], [f375777, f5240])).
fof(f375777, plain, ((sK57 = cons(hd(sK57), tl(sK57))) | (~ spl66_13086 | ~ spl66_15126)), inference(forward_demodulation, [], [f374489, f320760])).
fof(f320760, plain, ((sK57 = sK13(sK57, nil)) | ~ spl66_13086), inference(avatar_component_clause, [], [f320758])).
fof(f320758, plain, (spl66_13086 <=> (sK57 = sK13(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_13086])])).
fof(f374489, plain, ((sK13(sK57, nil) = cons(hd(sK13(sK57, nil)), tl(sK13(sK57, nil)))) | ~ spl66_15126), inference(avatar_component_clause, [], [f374487])).
fof(f374487, plain, (spl66_15126 <=> (sK13(sK57, nil) = cons(hd(sK13(sK57, nil)), tl(sK13(sK57, nil))))), introduced(avatar_definition, [new_symbols(naming, [spl66_15126])])).
fof(f466301, plain, ~ spl66_38, inference(avatar_contradiction_clause, [], [f466300])).
fof(f466300, plain, ($false | ~ spl66_38), inference(subsumption_resolution, [], [f464554, f464552])).
fof(f464552, plain, (gt(sK61, sK61) | ~ spl66_38), inference(backward_demodulation, [], [f1163, f1572])).
fof(f1572, plain, ((sK61 = sK62) | ~ spl66_38), inference(avatar_component_clause, [], [f1570])).
fof(f1163, plain, gt(sK61, sK62), inference(subsumption_resolution, [], [f1162, f575])).
fof(f1162, plain, (gt(sK61, sK62) | ~ ssItem(sK61)), inference(subsumption_resolution, [], [f1161, f576])).
fof(f1161, plain, (gt(sK61, sK62) | ~ ssItem(sK62) | ~ ssItem(sK61)), inference(resolution, [], [f478, f581])).
fof(f581, plain, lt(sK62, sK61), inference(cnf_transformation, [], [f362])).
fof(f478, plain, ! [X0, X1] : (~ lt(X1, X0) | gt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f324])).
fof(f324, plain, ! [X0] : (! [X1] : (((gt(X0, X1) | ~ lt(X1, X0)) & (lt(X1, X0) | ~ gt(X0, X1))) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f146])).
fof(f146, plain, ! [X0] : (! [X1] : ((gt(X0, X1) <=> lt(X1, X0)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (gt(X0, X1) <=> lt(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax35)).
fof(f464554, plain, (~ gt(sK61, sK61) | ~ spl66_38), inference(backward_demodulation, [], [f1408, f1572])).
fof(f1408, plain, ~ gt(sK62, sK61), inference(subsumption_resolution, [], [f1407, f576])).
fof(f1407, plain, (~ gt(sK62, sK61) | ~ ssItem(sK62)), inference(subsumption_resolution, [], [f1406, f575])).
fof(f1406, plain, (~ gt(sK62, sK61) | ~ ssItem(sK61) | ~ ssItem(sK62)), inference(resolution, [], [f560, f1163])).
fof(f560, plain, ! [X0, X1] : (~ gt(X1, X0) | ~ gt(X0, X1) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f219])).
fof(f219, plain, ! [X0] : (! [X1] : (~ gt(X1, X0) | ~ gt(X0, X1) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f218])).
fof(f218, plain, ! [X0] : (! [X1] : ((~ gt(X1, X0) | ~ gt(X0, X1)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f94])).
fof(f94, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => (gt(X0, X1) => ~ gt(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax94)).
fof(f375535, plain, (spl66_226 | ~ spl66_15123), inference(avatar_contradiction_clause, [], [f375534])).
fof(f375534, plain, ($false | (spl66_226 | ~ spl66_15123)), inference(subsumption_resolution, [], [f375533, f3867])).
fof(f375533, plain, ((nil = sK57) | ~ spl66_15123), inference(forward_demodulation, [], [f375432, f875])).
fof(f875, plain, (nil = app(nil, nil)), inference(resolution, [], [f469, f455])).
fof(f375432, plain, ((sK57 = app(nil, nil)) | ~ spl66_15123), inference(backward_demodulation, [], [f7796, f374474])).
fof(f374474, plain, ((nil = sK13(sK57, nil)) | ~ spl66_15123), inference(avatar_component_clause, [], [f374472])).
fof(f374472, plain, (spl66_15123 <=> (nil = sK13(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_15123])])).
fof(f7796, plain, (sK57 = app(sK13(sK57, nil), nil)), inference(resolution, [], [f1733, f633])).
fof(f1733, plain, ! [X1] : (~ ssList(X1) | (app(sK13(X1, nil), nil) = X1)), inference(subsumption_resolution, [], [f1731, f455])).
fof(f1731, plain, ! [X1] : ((app(sK13(X1, nil), nil) = X1) | ~ ssList(nil) | ~ ssList(X1)), inference(duplicate_literal_removal, [], [f1728])).
fof(f1728, plain, ! [X1] : ((app(sK13(X1, nil), nil) = X1) | ~ ssList(nil) | ~ ssList(X1) | ~ ssList(X1)), inference(resolution, [], [f379, f501])).
fof(f501, plain, ! [X0] : (rearsegP(X0, nil) | ~ ssList(X0)), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ! [X0] : (rearsegP(X0, nil) | ~ ssList(X0)), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (ssList(X0) => rearsegP(X0, nil)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax51)).
fof(f379, plain, ! [X0, X1] : (~ rearsegP(X0, X1) | (app(sK13(X0, X1), X1) = X0) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f255])).
fof(f255, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (((app(sK13(X0, X1), X1) = X0) & ssList(sK13(X0, X1))) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f253, f254])).
fof(f254, plain, ! [X1, X0] : (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) => ((app(sK13(X0, X1), X1) = X0) & ssList(sK13(X0, X1)))), introduced(choice_axiom, [])).
fof(f253, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X3] : ((app(X3, X1) = X0) & ssList(X3)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f252])).
fof(f252, plain, ! [X0] : (! [X1] : (((rearsegP(X0, X1) | ! [X2] : (~ (app(X2, X1) = X0) | ~ ssList(X2))) & (? [X2] : ((app(X2, X1) = X0) & ssList(X2)) | ~ rearsegP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f103])).
fof(f103, plain, ! [X0] : (! [X1] : ((rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (rearsegP(X0, X1) <=> ? [X2] : ((app(X2, X1) = X0) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax6)).
fof(f374492, plain, (spl66_13086 | ~ spl66_13079), inference(avatar_split_clause, [], [f374491, f320718, f320758])).
fof(f320718, plain, (spl66_13079 <=> ssList(sK13(sK57, nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_13079])])).
fof(f374491, plain, ((sK57 = sK13(sK57, nil)) | ~ spl66_13079), inference(forward_demodulation, [], [f374148, f7796])).
fof(f374148, plain, ((sK13(sK57, nil) = app(sK13(sK57, nil), nil)) | ~ spl66_13079), inference(resolution, [], [f320719, f548])).
fof(f320719, plain, (ssList(sK13(sK57, nil)) | ~ spl66_13079), inference(avatar_component_clause, [], [f320718])).
fof(f374490, plain, (spl66_15126 | spl66_15123 | ~ spl66_13079), inference(avatar_split_clause, [], [f374145, f320718, f374472, f374487])).
fof(f374145, plain, ((nil = sK13(sK57, nil)) | (sK13(sK57, nil) = cons(hd(sK13(sK57, nil)), tl(sK13(sK57, nil)))) | ~ spl66_13079), inference(resolution, [], [f320719, f540])).
fof(f374129, plain, (~ spl66_1635 | ~ spl66_9493 | spl66_13079), inference(avatar_contradiction_clause, [], [f374128])).
fof(f374128, plain, ($false | (~ spl66_1635 | ~ spl66_9493 | spl66_13079)), inference(subsumption_resolution, [], [f374127, f633])).
fof(f374127, plain, (~ ssList(sK57) | (~ spl66_1635 | ~ spl66_9493 | spl66_13079)), inference(subsumption_resolution, [], [f374126, f455])).
fof(f374126, plain, (~ ssList(nil) | ~ ssList(sK57) | (~ spl66_1635 | ~ spl66_9493 | spl66_13079)), inference(subsumption_resolution, [], [f374125, f216758])).
fof(f216758, plain, (rearsegP(sK57, nil) | (~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f216757, f204648])).
fof(f216757, plain, (rearsegP(sK57, nil) | ~ ssList(sK65) | (~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f216755, f455])).
fof(f216755, plain, (rearsegP(sK57, nil) | ~ ssList(nil) | ~ ssList(sK65) | (~ spl66_1635 | ~ spl66_9493)), inference(resolution, [], [f212681, f501])).
fof(f212681, plain, (! [X10] : (~ rearsegP(sK65, X10) | rearsegP(sK57, X10) | ~ ssList(X10)) | (~ spl66_1635 | ~ spl66_9493)), inference(subsumption_resolution, [], [f212680, f204648])).
fof(f212680, plain, (! [X10] : (rearsegP(sK57, X10) | ~ rearsegP(sK65, X10) | ~ ssList(X10) | ~ ssList(sK65)) | ~ spl66_1635), inference(subsumption_resolution, [], [f212584, f29112])).
fof(f212584, plain, ! [X10] : (rearsegP(sK57, X10) | ~ rearsegP(sK65, X10) | ~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | ~ ssList(X10) | ~ ssList(sK65)), inference(superposition, [], [f500, f580])).
fof(f500, plain, ! [X2, X0, X1] : (rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f166])).
fof(f166, plain, ! [X0] : (! [X1] : (! [X2] : (rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(flattening, [], [f165])).
fof(f165, plain, ! [X0] : (! [X1] : (! [X2] : ((rearsegP(app(X2, X0), X1) | ~ rearsegP(X0, X1)) | ~ ssList(X2)) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f50])).
fof(f50, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => ! [X2] : (ssList(X2) => (rearsegP(X0, X1) => rearsegP(app(X2, X0), X1))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax50)).
fof(f374125, plain, (~ rearsegP(sK57, nil) | ~ ssList(nil) | ~ ssList(sK57) | spl66_13079), inference(resolution, [], [f320720, f378])).
fof(f378, plain, ! [X0, X1] : (ssList(sK13(X0, X1)) | ~ rearsegP(X0, X1) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f255])).
fof(f320720, plain, (~ ssList(sK13(sK57, nil)) | spl66_13079), inference(avatar_component_clause, [], [f320718])).
fof(f291376, plain, (~ spl66_1 | spl66_11238), inference(avatar_contradiction_clause, [], [f291375])).
fof(f291375, plain, ($false | (~ spl66_1 | spl66_11238)), inference(subsumption_resolution, [], [f291374, f210133])).
fof(f291374, plain, (~ sP6(sK58, sK57) | spl66_11238), inference(resolution, [], [f280696, f562])).
fof(f562, plain, ! [X0, X1] : (ssItem(sK54(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f351])).
fof(f280696, plain, (~ ssItem(sK54(sK58, sK57)) | spl66_11238), inference(avatar_component_clause, [], [f280694])).
fof(f280784, plain, (~ spl66_11238 | spl66_11255 | ~ spl66_1), inference(avatar_split_clause, [], [f280780, f619, f280782, f280694])).
fof(f280780, plain, (! [X51] : (frontsegP(cons(sK54(sK58, sK57), X51), sK57) | ~ ssList(X51) | ~ ssItem(sK54(sK58, sK57))) | ~ spl66_1), inference(subsumption_resolution, [], [f280779, f494])).
fof(f280779, plain, (! [X51] : (frontsegP(cons(sK54(sK58, sK57), X51), sK57) | ~ frontsegP(X51, nil) | ~ ssList(X51) | ~ ssItem(sK54(sK58, sK57))) | ~ spl66_1), inference(subsumption_resolution, [], [f280675, f455])).
fof(f280675, plain, (! [X51] : (frontsegP(cons(sK54(sK58, sK57), X51), sK57) | ~ frontsegP(X51, nil) | ~ ssList(nil) | ~ ssList(X51) | ~ ssItem(sK54(sK58, sK57))) | ~ spl66_1), inference(superposition, [], [f613, f212237])).
fof(f280708, plain, (~ spl66_11238 | spl66_11240 | ~ spl66_1), inference(avatar_split_clause, [], [f280704, f619, f280706, f280694])).
fof(f280704, plain, (! [X8] : (~ memberP(sK57, X8) | (sK54(sK58, sK57) = X8) | ~ ssItem(sK54(sK58, sK57)) | ~ ssItem(X8)) | ~ spl66_1), inference(subsumption_resolution, [], [f280703, f485])).
fof(f485, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : (~ memberP(nil, X0) | ~ ssItem(X0)), inference(ennf_transformation, [], [f38])).
fof(f38, plain, ! [X0] : (ssItem(X0) => ~ memberP(nil, X0)), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax38)).
fof(f280703, plain, (! [X8] : (~ memberP(sK57, X8) | (sK54(sK58, sK57) = X8) | memberP(nil, X8) | ~ ssItem(sK54(sK58, sK57)) | ~ ssItem(X8)) | ~ spl66_1), inference(subsumption_resolution, [], [f280653, f455])).
fof(f280653, plain, (! [X8] : (~ memberP(sK57, X8) | (sK54(sK58, sK57) = X8) | memberP(nil, X8) | ~ ssList(nil) | ~ ssItem(sK54(sK58, sK57)) | ~ ssItem(X8)) | ~ spl66_1), inference(superposition, [], [f482, f212237])).
fof(f482, plain, ! [X2, X0, X1] : (~ memberP(cons(X1, X2), X0) | (X0 = X1) | memberP(X2, X0) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f328])).
fof(f328, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & (memberP(X2, X0) | (X0 = X1) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(flattening, [], [f327])).
fof(f327, plain, ! [X0] : (! [X1] : (! [X2] : (((memberP(cons(X1, X2), X0) | (~ memberP(X2, X0) & ~ (X0 = X1))) & ((memberP(X2, X0) | (X0 = X1)) | ~ memberP(cons(X1, X2), X0))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(nnf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : (! [X1] : (! [X2] : ((memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1))) | ~ ssList(X2)) | ~ ssItem(X1)) | ~ ssItem(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (ssItem(X0) => ! [X1] : (ssItem(X1) => ! [X2] : (ssList(X2) => (memberP(cons(X1, X2), X0) <=> (memberP(X2, X0) | (X0 = X1)))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax37)).
fof(f264049, plain, (spl66_10798 | ~ spl66_90 | ~ spl66_1389), inference(avatar_split_clause, [], [f203604, f26366, f2774, f261509])).
fof(f2774, plain, (spl66_90 <=> ssList(app(app(sK63, cons(sK61, nil)), nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_90])])).
fof(f203604, plain, (ssList(app(sK63, cons(sK61, nil))) | (~ spl66_90 | ~ spl66_1389)), inference(backward_demodulation, [], [f2775, f203551])).
fof(f203551, plain, ((app(sK63, cons(sK61, nil)) = app(app(sK63, cons(sK61, nil)), nil)) | ~ spl66_1389), inference(resolution, [], [f28917, f26367])).
fof(f2775, plain, (ssList(app(app(sK63, cons(sK61, nil)), nil)) | ~ spl66_90), inference(avatar_component_clause, [], [f2774])).
fof(f263757, plain, spl66_10800, inference(avatar_split_clause, [], [f577, f263022])).
fof(f209307, plain, (spl66_229 | spl66_226), inference(avatar_split_clause, [], [f6594, f3866, f3881])).
fof(f3881, plain, (spl66_229 <=> (sK57 = cons(hd(sK57), tl(sK57)))), introduced(avatar_definition, [new_symbols(naming, [spl66_229])])).
fof(f6594, plain, ((nil = sK57) | (sK57 = cons(hd(sK57), tl(sK57)))), inference(resolution, [], [f633, f540])).
fof(f207479, plain, spl66_9493, inference(avatar_split_clause, [], [f579, f204647])).
fof(f579, plain, ssList(sK65), inference(cnf_transformation, [], [f362])).
fof(f204824, plain, (~ spl66_9488 | ~ spl66_9493 | ~ spl66_62 | spl66_91 | ~ spl66_226), inference(avatar_split_clause, [], [f204312, f3866, f2778, f2274, f204647, f204000])).
fof(f2778, plain, (spl66_91 <=> segmentP(nil, cons(sK62, nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_91])])).
fof(f204312, plain, (~ ssList(sK65) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | (~ spl66_62 | spl66_91 | ~ spl66_226)), inference(subsumption_resolution, [], [f204311, f455])).
fof(f204311, plain, (~ ssList(sK65) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(nil) | (~ spl66_62 | spl66_91 | ~ spl66_226)), inference(subsumption_resolution, [], [f204310, f2275])).
fof(f204310, plain, (~ ssList(sK65) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(cons(sK62, nil)) | ~ ssList(nil) | (spl66_91 | ~ spl66_226)), inference(subsumption_resolution, [], [f108066, f2779])).
fof(f2779, plain, (~ segmentP(nil, cons(sK62, nil)) | spl66_91), inference(avatar_component_clause, [], [f2778])).
fof(f108066, plain, (segmentP(nil, cons(sK62, nil)) | ~ ssList(sK65) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | ~ ssList(cons(sK62, nil)) | ~ ssList(nil) | ~ spl66_226), inference(superposition, [], [f589, f59851])).
fof(f59851, plain, ((nil = app(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil)), sK65)) | ~ spl66_226), inference(forward_demodulation, [], [f580, f3868])).
fof(f3868, plain, ((nil = sK57) | ~ spl66_226), inference(avatar_component_clause, [], [f3866])).
fof(f589, plain, ! [X2, X3, X1] : (segmentP(app(app(X2, X1), X3), X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(app(app(X2, X1), X3))), inference(equality_resolution, [], [f384])).
fof(f384, plain, ! [X2, X0, X3, X1] : (segmentP(X0, X1) | ~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3) | ~ ssList(X2) | ~ ssList(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f260])).
fof(f260, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & ((((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1))) & ssList(sK14(X0, X1))) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14, sK15])], [f257, f259, f258])).
fof(f258, plain, ! [X1, X0] : (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) => (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) & ssList(sK14(X0, X1)))), introduced(choice_axiom, [])).
fof(f259, plain, ! [X1, X0] : (? [X5] : ((app(app(sK14(X0, X1), X1), X5) = X0) & ssList(X5)) => ((app(app(sK14(X0, X1), X1), sK15(X0, X1)) = X0) & ssList(sK15(X0, X1)))), introduced(choice_axiom, [])).
fof(f257, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X4] : (? [X5] : ((app(app(X4, X1), X5) = X0) & ssList(X5)) & ssList(X4)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(rectify, [], [f256])).
fof(f256, plain, ! [X0] : (! [X1] : (((segmentP(X0, X1) | ! [X2] : (! [X3] : (~ (app(app(X2, X1), X3) = X0) | ~ ssList(X3)) | ~ ssList(X2))) & (? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2)) | ~ segmentP(X0, X1))) | ~ ssList(X1)) | ~ ssList(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : (! [X1] : ((segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))) | ~ ssList(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssList(X1) => (segmentP(X0, X1) <=> ? [X2] : (? [X3] : ((app(app(X2, X1), X3) = X0) & ssList(X3)) & ssList(X2))))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax7)).
fof(f203857, plain, (~ spl66_62 | ~ spl66_90 | ~ spl66_1389 | spl66_1635), inference(avatar_contradiction_clause, [], [f203856])).
fof(f203856, plain, ($false | (~ spl66_62 | ~ spl66_90 | ~ spl66_1389 | spl66_1635)), inference(subsumption_resolution, [], [f203604, f124211])).
fof(f124211, plain, (~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_62 | spl66_1635)), inference(subsumption_resolution, [], [f124210, f578])).
fof(f124210, plain, (~ ssList(sK64) | ~ ssList(app(sK63, cons(sK61, nil))) | (~ spl66_62 | spl66_1635)), inference(resolution, [], [f59846, f467])).
fof(f59846, plain, (~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | (~ spl66_62 | spl66_1635)), inference(subsumption_resolution, [], [f33757, f2275])).
fof(f33757, plain, (~ ssList(cons(sK62, nil)) | ~ ssList(app(app(sK63, cons(sK61, nil)), sK64)) | spl66_1635), inference(resolution, [], [f29113, f467])).
fof(f29113, plain, (~ ssList(app(app(app(sK63, cons(sK61, nil)), sK64), cons(sK62, nil))) | spl66_1635), inference(avatar_component_clause, [], [f29111])).
fof(f179127, plain, (spl66_8390 | ~ spl66_8391 | ~ spl66_62 | ~ spl66_6386), inference(avatar_split_clause, [], [f179118, f134666, f2274, f179124, f179120])).
fof(f179118, plain, (~ frontsegP(cons(sK62, nil), cons(sK62, sK64)) | (cons(sK62, nil) = cons(sK62, sK64)) | (~ spl66_62 | ~ spl66_6386)), inference(subsumption_resolution, [], [f179117, f134667])).
fof(f179117, plain, (~ frontsegP(cons(sK62, nil), cons(sK62, sK64)) | (cons(sK62, nil) = cons(sK62, sK64)) | ~ ssList(sK64) | ~ spl66_62), inference(subsumption_resolution, [], [f179075, f2275])).
fof(f179075, plain, (~ frontsegP(cons(sK62, nil), cons(sK62, sK64)) | ~ ssList(cons(sK62, nil)) | (cons(sK62, nil) = cons(sK62, sK64)) | ~ ssList(sK64)), inference(superposition, [], [f1658, f10412])).
fof(f10412, plain, (cons(sK62, sK64) = app(cons(sK62, nil), sK64)), inference(resolution, [], [f3321, f576])).
fof(f1658, plain, ! [X0, X1] : (~ frontsegP(X1, app(X1, X0)) | ~ ssList(X1) | (app(X1, X0) = X1) | ~ ssList(X0)), inference(subsumption_resolution, [], [f1657, f467])).
fof(f1657, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X1, X0)) | (app(X1, X0) = X1) | ~ frontsegP(X1, app(X1, X0))), inference(duplicate_literal_removal, [], [f1650])).
fof(f1650, plain, ! [X0, X1] : (~ ssList(X0) | ~ ssList(X1) | ~ ssList(app(X1, X0)) | (app(X1, X0) = X1) | ~ frontsegP(X1, app(X1, X0)) | ~ ssList(app(X1, X0)) | ~ ssList(X1)), inference(resolution, [], [f587, f488])).
fof(f134817, plain, spl66_6386, inference(avatar_split_clause, [], [f578, f134666])).
fof(f58816, plain, (spl66_90 | ~ spl66_1389), inference(avatar_contradiction_clause, [], [f58815])).
fof(f58815, plain, ($false | (spl66_90 | ~ spl66_1389)), inference(subsumption_resolution, [], [f58814, f577])).
fof(f58814, plain, (~ ssList(sK63) | (spl66_90 | ~ spl66_1389)), inference(subsumption_resolution, [], [f58813, f26367])).
fof(f58813, plain, (~ ssList(cons(sK61, nil)) | ~ ssList(sK63) | spl66_90), inference(resolution, [], [f31698, f467])).
fof(f31698, plain, (~ ssList(app(sK63, cons(sK61, nil))) | spl66_90), inference(subsumption_resolution, [], [f31697, f455])).
fof(f31697, plain, (~ ssList(nil) | ~ ssList(app(sK63, cons(sK61, nil))) | spl66_90), inference(resolution, [], [f2776, f467])).
fof(f2776, plain, (~ ssList(app(app(sK63, cons(sK61, nil)), nil)) | spl66_90), inference(avatar_component_clause, [], [f2774])).
fof(f30554, plain, (spl66_29 | spl66_132), inference(avatar_contradiction_clause, [], [f30553])).
fof(f30553, plain, ($false | (spl66_29 | spl66_132)), inference(subsumption_resolution, [], [f30552, f577])).
fof(f30552, plain, (~ ssList(sK63) | (spl66_29 | spl66_132)), inference(subsumption_resolution, [], [f30551, f1203])).
fof(f1203, plain, (~ (nil = sK63) | spl66_29), inference(avatar_component_clause, [], [f1202])).
fof(f30551, plain, ((nil = sK63) | ~ ssList(sK63) | spl66_132), inference(resolution, [], [f3105, f463])).
fof(f463, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (ssItem(hd(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : ((ssItem(hd(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssItem(hd(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax22)).
fof(f3105, plain, (~ ssItem(hd(sK63)) | spl66_132), inference(avatar_component_clause, [], [f3103])).
fof(f26442, plain, spl66_1389, inference(avatar_contradiction_clause, [], [f26441])).
fof(f26441, plain, ($false | spl66_1389), inference(subsumption_resolution, [], [f26440, f455])).
fof(f26440, plain, (~ ssList(nil) | spl66_1389), inference(subsumption_resolution, [], [f26439, f575])).
fof(f26439, plain, (~ ssItem(sK61) | ~ ssList(nil) | spl66_1389), inference(resolution, [], [f26368, f454])).
fof(f26368, plain, (~ ssList(cons(sK61, nil)) | spl66_1389), inference(avatar_component_clause, [], [f26366])).
fof(f20169, plain, spl66_879, inference(avatar_contradiction_clause, [], [f20168])).
fof(f20168, plain, ($false | spl66_879), inference(subsumption_resolution, [], [f20167, f578])).
fof(f20167, plain, (~ ssList(sK64) | spl66_879), inference(subsumption_resolution, [], [f20166, f575])).
fof(f20166, plain, (~ ssItem(sK61) | ~ ssList(sK64) | spl66_879), inference(resolution, [], [f20084, f454])).
fof(f20084, plain, (~ ssList(cons(sK61, sK64)) | spl66_879), inference(avatar_component_clause, [], [f20082])).
fof(f9060, plain, (spl66_226 | ~ spl66_2), inference(avatar_split_clause, [], [f9055, f623, f3866])).
fof(f623, plain, (spl66_2 <=> (nil = sK59)), introduced(avatar_definition, [new_symbols(naming, [spl66_2])])).
fof(f9055, plain, ((nil = sK57) | ~ spl66_2), inference(backward_demodulation, [], [f574, f625])).
fof(f625, plain, ((nil = sK59) | ~ spl66_2), inference(avatar_component_clause, [], [f623])).
fof(f7262, plain, (spl66_226 | spl66_327), inference(avatar_contradiction_clause, [], [f7261])).
fof(f7261, plain, ($false | (spl66_226 | spl66_327)), inference(subsumption_resolution, [], [f7260, f633])).
fof(f7260, plain, (~ ssList(sK57) | (spl66_226 | spl66_327)), inference(subsumption_resolution, [], [f7259, f3867])).
fof(f7259, plain, ((nil = sK57) | ~ ssList(sK57) | spl66_327), inference(resolution, [], [f5192, f463])).
fof(f5192, plain, (~ ssItem(hd(sK57)) | spl66_327), inference(avatar_component_clause, [], [f5190])).
fof(f7047, plain, (~ spl66_327 | ~ spl66_326 | spl66_334 | ~ spl66_229), inference(avatar_split_clause, [], [f6874, f3881, f5221, f5186, f5190])).
fof(f5186, plain, (spl66_326 <=> ssList(tl(sK57))), introduced(avatar_definition, [new_symbols(naming, [spl66_326])])).
fof(f6874, plain, (! [X12, X13] : (~ frontsegP(sK57, cons(X12, X13)) | (hd(sK57) = X12) | ~ ssList(X13) | ~ ssList(tl(sK57)) | ~ ssItem(X12) | ~ ssItem(hd(sK57))) | ~ spl66_229), inference(superposition, [], [f491, f3883])).
fof(f3883, plain, ((sK57 = cons(hd(sK57), tl(sK57))) | ~ spl66_229), inference(avatar_component_clause, [], [f3881])).
fof(f491, plain, ! [X2, X0, X3, X1] : (~ frontsegP(cons(X0, X2), cons(X1, X3)) | (X0 = X1) | ~ ssList(X3) | ~ ssList(X2) | ~ ssItem(X1) | ~ ssItem(X0)), inference(cnf_transformation, [], [f330])).
fof(f7027, plain, (spl66_226 | spl66_326), inference(avatar_contradiction_clause, [], [f7026])).
fof(f7026, plain, ($false | (spl66_226 | spl66_326)), inference(subsumption_resolution, [], [f7025, f633])).
fof(f7025, plain, (~ ssList(sK57) | (spl66_226 | spl66_326)), inference(subsumption_resolution, [], [f7024, f3867])).
fof(f7024, plain, ((nil = sK57) | ~ ssList(sK57) | spl66_326), inference(resolution, [], [f5188, f465])).
fof(f465, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (ssList(tl(X0)) | (nil = X0) | ~ ssList(X0)), inference(flattening, [], [f130])).
fof(f130, plain, ! [X0] : ((ssList(tl(X0)) | (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f24])).
fof(f24, plain, ! [X0] : (ssList(X0) => (~ (nil = X0) => ssList(tl(X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax24)).
fof(f5188, plain, (~ ssList(tl(sK57)) | spl66_326), inference(avatar_component_clause, [], [f5186])).
fof(f6795, plain, (spl66_226 | spl66_294), inference(avatar_contradiction_clause, [], [f6794])).
fof(f6794, plain, ($false | (spl66_226 | spl66_294)), inference(subsumption_resolution, [], [f6793, f633])).
fof(f6793, plain, (~ ssList(sK57) | (spl66_226 | spl66_294)), inference(subsumption_resolution, [], [f6792, f3867])).
fof(f6792, plain, ((nil = sK57) | ~ ssList(sK57) | spl66_294), inference(resolution, [], [f5011, f460])).
fof(f460, plain, ! [X0] : (ssItem(sK51(X0)) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f322])).
fof(f5011, plain, (~ ssItem(sK51(sK57)) | spl66_294), inference(avatar_component_clause, [], [f5009])).
fof(f5520, plain, (spl66_29 | spl66_131), inference(avatar_contradiction_clause, [], [f5519])).
fof(f5519, plain, ($false | (spl66_29 | spl66_131)), inference(subsumption_resolution, [], [f5518, f577])).
fof(f5518, plain, (~ ssList(sK63) | (spl66_29 | spl66_131)), inference(subsumption_resolution, [], [f5517, f1203])).
fof(f5517, plain, ((nil = sK63) | ~ ssList(sK63) | spl66_131), inference(resolution, [], [f3101, f465])).
fof(f3101, plain, (~ ssList(tl(sK63)) | spl66_131), inference(avatar_component_clause, [], [f3099])).
fof(f5336, plain, (spl66_226 | spl66_293), inference(avatar_split_clause, [], [f5335, f5005, f3866])).
fof(f5335, plain, ((nil = sK57) | spl66_293), inference(subsumption_resolution, [], [f5329, f633])).
fof(f5329, plain, ((nil = sK57) | ~ ssList(sK57) | spl66_293), inference(resolution, [], [f5007, f459])).
fof(f5007, plain, (~ ssList(sK50(sK57)) | spl66_293), inference(avatar_component_clause, [], [f5005])).
fof(f3994, plain, (~ spl66_62 | spl66_70 | ~ spl66_91), inference(avatar_contradiction_clause, [], [f3993])).
fof(f3993, plain, ($false | (~ spl66_62 | spl66_70 | ~ spl66_91)), inference(subsumption_resolution, [], [f3992, f2275])).
fof(f3992, plain, (~ ssList(cons(sK62, nil)) | (spl66_70 | ~ spl66_91)), inference(subsumption_resolution, [], [f3988, f2336])).
fof(f2336, plain, (~ (nil = cons(sK62, nil)) | spl66_70), inference(avatar_component_clause, [], [f2335])).
fof(f2335, plain, (spl66_70 <=> (nil = cons(sK62, nil))), introduced(avatar_definition, [new_symbols(naming, [spl66_70])])).
fof(f3988, plain, ((nil = cons(sK62, nil)) | ~ ssList(cons(sK62, nil)) | ~ spl66_91), inference(resolution, [], [f2780, f509])).
fof(f509, plain, ! [X0] : (~ segmentP(nil, X0) | (nil = X0) | ~ ssList(X0)), inference(cnf_transformation, [], [f333])).
fof(f333, plain, ! [X0] : (((segmentP(nil, X0) | ~ (nil = X0)) & ((nil = X0) | ~ segmentP(nil, X0))) | ~ ssList(X0)), inference(nnf_transformation, [], [f177])).
fof(f177, plain, ! [X0] : ((segmentP(nil, X0) <=> (nil = X0)) | ~ ssList(X0)), inference(ennf_transformation, [], [f58])).
fof(f58, plain, ! [X0] : (ssList(X0) => (segmentP(nil, X0) <=> (nil = X0))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax58)).
fof(f2780, plain, (segmentP(nil, cons(sK62, nil)) | ~ spl66_91), inference(avatar_component_clause, [], [f2778])).
fof(f3869, plain, (spl66_225 | spl66_226), inference(avatar_split_clause, [], [f3807, f3866, f3862])).
fof(f3807, plain, ((nil = sK57) | (sK57 = cons(sK51(sK57), sK50(sK57)))), inference(resolution, [], [f633, f461])).
fof(f461, plain, ! [X0] : (~ ssList(X0) | (nil = X0) | (cons(sK51(X0), sK50(X0)) = X0)), inference(cnf_transformation, [], [f322])).
fof(f2435, plain, ~ spl66_70, inference(avatar_contradiction_clause, [], [f2434])).
fof(f2434, plain, ($false | ~ spl66_70), inference(subsumption_resolution, [], [f2433, f455])).
fof(f2433, plain, (~ ssList(nil) | ~ spl66_70), inference(subsumption_resolution, [], [f2421, f576])).
fof(f2421, plain, (~ ssItem(sK62) | ~ ssList(nil) | ~ spl66_70), inference(trivial_inequality_removal, [], [f2406])).
fof(f2406, plain, (~ (nil = nil) | ~ ssItem(sK62) | ~ ssList(nil) | ~ spl66_70), inference(superposition, [], [f462, f2337])).
fof(f2337, plain, ((nil = cons(sK62, nil)) | ~ spl66_70), inference(avatar_component_clause, [], [f2335])).
fof(f462, plain, ! [X0, X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1) | ~ ssList(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : (! [X1] : (~ (nil = cons(X1, X0)) | ~ ssItem(X1)) | ~ ssList(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (ssList(X0) => ! [X1] : (ssItem(X1) => ~ (nil = cons(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SWC/SWC299+1.p', ax21)).
fof(f2288, plain, spl66_62, inference(avatar_contradiction_clause, [], [f2287])).
fof(f2287, plain, ($false | spl66_62), inference(subsumption_resolution, [], [f2286, f455])).
fof(f2286, plain, (~ ssList(nil) | spl66_62), inference(subsumption_resolution, [], [f2285, f576])).
fof(f2285, plain, (~ ssItem(sK62) | ~ ssList(nil) | spl66_62), inference(resolution, [], [f2276, f454])).
fof(f2276, plain, (~ ssList(cons(sK62, nil)) | spl66_62), inference(avatar_component_clause, [], [f2274])).
fof(f626, plain, (spl66_1 | spl66_2), inference(avatar_split_clause, [], [f583, f623, f619])).
fof(f583, plain, ((nil = sK59) | sP6(sK60, sK59)), inference(cnf_transformation, [], [f362])).