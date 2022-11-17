fof(f18364, plain, $false, inference(avatar_sat_refutation, [], [f232, f242, f247, f827, f3045, f3106, f18361])).
fof(f18361, plain, (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36), inference(avatar_contradiction_clause, [], [f18360])).
fof(f18360, plain, ($false | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(subsumption_resolution, [], [f18359, f180])).
fof(f180, plain, aNaturalNumber0(xl), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (aNaturalNumber0(xn) & aNaturalNumber0(xm) & aNaturalNumber0(xl)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', m__1324)).
fof(f18359, plain, (~ aNaturalNumber0(xl) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(subsumption_resolution, [], [f18358, f7645])).
fof(f7645, plain, (aNaturalNumber0(sdtmndt0(sK4, sK5)) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(subsumption_resolution, [], [f7644, f5677])).
fof(f5677, plain, (sP0(sK5, sK4) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(subsumption_resolution, [], [f5674, f4850])).
fof(f4850, plain, (sP1(sK5) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5)), inference(backward_demodulation, [], [f231, f4849])).
fof(f4849, plain, ((sK5 = sK9) | (spl10_1 | ~ spl10_4 | ~ spl10_5)), inference(subsumption_resolution, [], [f4790, f183])).
fof(f183, plain, aNaturalNumber0(sK5), inference(cnf_transformation, [], [f114])).
fof(f114, plain, (doDivides0(xl, sdtpldt0(xm, xn)) & ((sdtpldt0(xm, xn) = sdtasdt0(xl, sK4)) & aNaturalNumber0(sK4)) & doDivides0(xl, xm) & ((xm = sdtasdt0(xl, sK5)) & aNaturalNumber0(sK5))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5])], [f40, f113, f112])).
fof(f112, plain, (? [X0] : ((sdtasdt0(xl, X0) = sdtpldt0(xm, xn)) & aNaturalNumber0(X0)) => ((sdtpldt0(xm, xn) = sdtasdt0(xl, sK4)) & aNaturalNumber0(sK4))), introduced(choice_axiom, [])).
fof(f113, plain, (? [X1] : ((xm = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) => ((xm = sdtasdt0(xl, sK5)) & aNaturalNumber0(sK5))), introduced(choice_axiom, [])).
fof(f40, plain, (doDivides0(xl, sdtpldt0(xm, xn)) & ? [X0] : ((sdtasdt0(xl, X0) = sdtpldt0(xm, xn)) & aNaturalNumber0(X0)) & doDivides0(xl, xm) & ? [X1] : ((xm = sdtasdt0(xl, X1)) & aNaturalNumber0(X1))), inference(rectify, [], [f35])).
fof(f35, plain, (doDivides0(xl, sdtpldt0(xm, xn)) & ? [X0] : ((sdtasdt0(xl, X0) = sdtpldt0(xm, xn)) & aNaturalNumber0(X0)) & doDivides0(xl, xm) & ? [X0] : ((xm = sdtasdt0(xl, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', m__1324_04)).
fof(f4790, plain, ((sK5 = sK9) | ~ aNaturalNumber0(sK5) | (spl10_1 | ~ spl10_4 | ~ spl10_5)), inference(trivial_inequality_removal, [], [f4787])).
fof(f4787, plain, (~ (xm = xm) | (sK5 = sK9) | ~ aNaturalNumber0(sK5) | (spl10_1 | ~ spl10_4 | ~ spl10_5)), inference(superposition, [], [f1347, f184])).
fof(f184, plain, (xm = sdtasdt0(xl, sK5)), inference(cnf_transformation, [], [f114])).
fof(f1347, plain, (! [X0] : (~ (xm = sdtasdt0(xl, X0)) | (sK9 = X0) | ~ aNaturalNumber0(X0)) | (spl10_1 | ~ spl10_4 | ~ spl10_5)), inference(subsumption_resolution, [], [f1346, f180])).
fof(f1346, plain, (! [X0] : (~ (xm = sdtasdt0(xl, X0)) | (sK9 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(xl)) | (spl10_1 | ~ spl10_4 | ~ spl10_5)), inference(subsumption_resolution, [], [f1345, f226])).
fof(f226, plain, (~ (sz00 = xl) | spl10_1), inference(avatar_component_clause, [], [f225])).
fof(f225, plain, (spl10_1 <=> (sz00 = xl)), introduced(avatar_definition, [new_symbols(naming, [spl10_1])])).
fof(f1345, plain, (! [X0] : (~ (xm = sdtasdt0(xl, X0)) | (sK9 = X0) | ~ aNaturalNumber0(X0) | (sz00 = xl) | ~ aNaturalNumber0(xl)) | (~ spl10_4 | ~ spl10_5)), inference(subsumption_resolution, [], [f1335, f246])).
fof(f246, plain, (aNaturalNumber0(sK9) | ~ spl10_5), inference(avatar_component_clause, [], [f244])).
fof(f244, plain, (spl10_5 <=> aNaturalNumber0(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl10_5])])).
fof(f1335, plain, (! [X0] : (~ (xm = sdtasdt0(xl, X0)) | (sK9 = X0) | ~ aNaturalNumber0(X0) | ~ aNaturalNumber0(sK9) | (sz00 = xl) | ~ aNaturalNumber0(xl)) | ~ spl10_4), inference(superposition, [], [f145, f241])).
fof(f241, plain, ((xm = sdtasdt0(xl, sK9)) | ~ spl10_4), inference(avatar_component_clause, [], [f239])).
fof(f239, plain, (spl10_4 <=> (xm = sdtasdt0(xl, sK9))), introduced(avatar_definition, [new_symbols(naming, [spl10_4])])).
fof(f145, plain, ! [X2, X0, X1] : (~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2)) | (X1 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ! [X0] : (! [X1, X2] : ((X1 = X2) | (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1)) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f62])).
fof(f62, plain, ! [X0] : ((! [X1, X2] : (((X1 = X2) | (~ (sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) & ~ (sdtasdt0(X0, X1) = sdtasdt0(X0, X2)))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1))) | (sz00 = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (aNaturalNumber0(X0) => (~ (sz00 = X0) => ! [X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1)) => (((sdtasdt0(X1, X0) = sdtasdt0(X2, X0)) | (sdtasdt0(X0, X1) = sdtasdt0(X0, X2))) => (X1 = X2))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', mMulCanc)).
fof(f231, plain, (sP1(sK9) | ~ spl10_2), inference(avatar_component_clause, [], [f229])).
fof(f229, plain, (spl10_2 <=> sP1(sK9)), introduced(avatar_definition, [new_symbols(naming, [spl10_2])])).
fof(f5674, plain, (sP0(sK5, sK4) | ~ sP1(sK5) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(superposition, [], [f195, f5585])).
fof(f5585, plain, ((sK4 = sK6(sK5)) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(subsumption_resolution, [], [f5581, f4923])).
fof(f4923, plain, (aNaturalNumber0(sK6(sK5)) | (spl10_1 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(backward_demodulation, [], [f3044, f4849])).
fof(f3044, plain, (aNaturalNumber0(sK6(sK9)) | ~ spl10_36), inference(avatar_component_clause, [], [f3042])).
fof(f3042, plain, (spl10_36 <=> aNaturalNumber0(sK6(sK9))), introduced(avatar_definition, [new_symbols(naming, [spl10_36])])).
fof(f5581, plain, ((sK4 = sK6(sK5)) | ~ aNaturalNumber0(sK6(sK5)) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5)), inference(trivial_inequality_removal, [], [f5580])).
fof(f5580, plain, (~ (sdtpldt0(xm, xn) = sdtpldt0(xm, xn)) | (sK4 = sK6(sK5)) | ~ aNaturalNumber0(sK6(sK5)) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5)), inference(superposition, [], [f1353, f4855])).
fof(f4855, plain, ((sdtpldt0(xm, xn) = sdtasdt0(xl, sK6(sK5))) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5)), inference(backward_demodulation, [], [f376, f4849])).
fof(f376, plain, ((sdtpldt0(xm, xn) = sdtasdt0(xl, sK6(sK9))) | ~ spl10_2), inference(resolution, [], [f190, f231])).
fof(f190, plain, ! [X0] : (~ sP1(X0) | (sdtpldt0(xm, xn) = sdtasdt0(xl, sK6(X0)))), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ! [X0] : ((sP0(X0, sK6(X0)) & sdtlseqdt0(X0, sK6(X0)) & ((sK6(X0) = sdtpldt0(X0, sK7(X0))) & aNaturalNumber0(sK7(X0))) & (sdtsldt0(sdtpldt0(xm, xn), xl) = sK6(X0)) & (sdtpldt0(xm, xn) = sdtasdt0(xl, sK6(X0))) & aNaturalNumber0(sK6(X0))) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7])], [f116, f118, f117])).
fof(f117, plain, ! [X0] : (? [X1] : (sP0(X0, X1) & sdtlseqdt0(X0, X1) & ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) => (sP0(X0, sK6(X0)) & sdtlseqdt0(X0, sK6(X0)) & ? [X2] : ((sdtpldt0(X0, X2) = sK6(X0)) & aNaturalNumber0(X2)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = sK6(X0)) & (sdtpldt0(xm, xn) = sdtasdt0(xl, sK6(X0))) & aNaturalNumber0(sK6(X0)))), introduced(choice_axiom, [])).
fof(f118, plain, ! [X0] : (? [X2] : ((sdtpldt0(X0, X2) = sK6(X0)) & aNaturalNumber0(X2)) => ((sK6(X0) = sdtpldt0(X0, sK7(X0))) & aNaturalNumber0(sK7(X0)))), introduced(choice_axiom, [])).
fof(f116, plain, ! [X0] : (? [X1] : (sP0(X0, X1) & sdtlseqdt0(X0, X1) & ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) | ~ sP1(X0)), inference(rectify, [], [f115])).
fof(f115, plain, ! [X0] : (? [X1] : (sP0(X0, X1) & sdtlseqdt0(X0, X1) & ? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) | ~ sP1(X0)), inference(nnf_transformation, [], [f98])).
fof(f98, plain, ! [X0] : (? [X1] : (sP0(X0, X1) & sdtlseqdt0(X0, X1) & ? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) | ~ sP1(X0)), inference(usedef, [], [e98])).
fof(e98, plain, ! [X0] : (sP1(X0) <=> ? [X1] : (sP0(X0, X1) & sdtlseqdt0(X0, X1) & ? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f1353, plain, (! [X2] : (~ (sdtpldt0(xm, xn) = sdtasdt0(xl, X2)) | (sK4 = X2) | ~ aNaturalNumber0(X2)) | spl10_1), inference(subsumption_resolution, [], [f1352, f180])).
fof(f1352, plain, (! [X2] : (~ (sdtpldt0(xm, xn) = sdtasdt0(xl, X2)) | (sK4 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(xl)) | spl10_1), inference(subsumption_resolution, [], [f1351, f226])).
fof(f1351, plain, ! [X2] : (~ (sdtpldt0(xm, xn) = sdtasdt0(xl, X2)) | (sK4 = X2) | ~ aNaturalNumber0(X2) | (sz00 = xl) | ~ aNaturalNumber0(xl)), inference(subsumption_resolution, [], [f1337, f186])).
fof(f186, plain, aNaturalNumber0(sK4), inference(cnf_transformation, [], [f114])).
fof(f1337, plain, ! [X2] : (~ (sdtpldt0(xm, xn) = sdtasdt0(xl, X2)) | (sK4 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sK4) | (sz00 = xl) | ~ aNaturalNumber0(xl)), inference(superposition, [], [f145, f187])).
fof(f187, plain, (sdtpldt0(xm, xn) = sdtasdt0(xl, sK4)), inference(cnf_transformation, [], [f114])).
fof(f195, plain, ! [X0] : (sP0(X0, sK6(X0)) | ~ sP1(X0)), inference(cnf_transformation, [], [f119])).
fof(f7644, plain, (aNaturalNumber0(sdtmndt0(sK4, sK5)) | ~ sP0(sK5, sK4) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(superposition, [], [f196, f5647])).
fof(f5647, plain, ((sdtmndt0(sK4, sK5) = sK8(sK5, sK4)) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(backward_demodulation, [], [f5559, f5585])).
fof(f5559, plain, ((sdtmndt0(sK6(sK5), sK5) = sK8(sK5, sK6(sK5))) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5)), inference(resolution, [], [f385, f4850])).
fof(f385, plain, ! [X0] : (~ sP1(X0) | (sK8(X0, sK6(X0)) = sdtmndt0(sK6(X0), X0))), inference(resolution, [], [f198, f195])).
fof(f198, plain, ! [X0, X1] : (~ sP0(X0, X1) | (sdtmndt0(X1, X0) = sK8(X0, X1))), inference(cnf_transformation, [], [f122])).
fof(f122, plain, ! [X0, X1] : (((xn = sdtasdt0(xl, sK8(X0, X1))) & (sdtpldt0(sdtasdt0(xl, X0), xn) = sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, sK8(X0, X1)))) & (sdtmndt0(X1, X0) = sK8(X0, X1)) & (sdtpldt0(X0, sK8(X0, X1)) = X1) & aNaturalNumber0(sK8(X0, X1))) | ~ sP0(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f120, f121])).
fof(f121, plain, ! [X1, X0] : (? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) => ((xn = sdtasdt0(xl, sK8(X0, X1))) & (sdtpldt0(sdtasdt0(xl, X0), xn) = sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, sK8(X0, X1)))) & (sdtmndt0(X1, X0) = sK8(X0, X1)) & (sdtpldt0(X0, sK8(X0, X1)) = X1) & aNaturalNumber0(sK8(X0, X1)))), introduced(choice_axiom, [])).
fof(f120, plain, ! [X0, X1] : (? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ sP0(X0, X1)), inference(nnf_transformation, [], [f97])).
fof(f97, plain, ! [X0, X1] : (? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ sP0(X0, X1)), inference(usedef, [], [e97])).
fof(e97, plain, ! [X0, X1] : (sP0(X0, X1) <=> ? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f196, plain, ! [X0, X1] : (aNaturalNumber0(sK8(X0, X1)) | ~ sP0(X0, X1)), inference(cnf_transformation, [], [f122])).
fof(f18358, plain, (~ aNaturalNumber0(sdtmndt0(sK4, sK5)) | ~ aNaturalNumber0(xl) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(subsumption_resolution, [], [f18287, f206])).
fof(f206, plain, ~ doDivides0(xl, xn), inference(cnf_transformation, [], [f125])).
fof(f125, plain, (~ doDivides0(xl, xn) & ! [X0] : (~ (xn = sdtasdt0(xl, X0)) | ~ aNaturalNumber0(X0)) & ((sP1(sK9) & (sdtsldt0(xm, xl) = sK9) & (xm = sdtasdt0(xl, sK9)) & aNaturalNumber0(sK9)) | (sz00 = xl))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f123, f124])).
fof(f124, plain, (? [X1] : (sP1(X1) & (sdtsldt0(xm, xl) = X1) & (xm = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) => (sP1(sK9) & (sdtsldt0(xm, xl) = sK9) & (xm = sdtasdt0(xl, sK9)) & aNaturalNumber0(sK9))), introduced(choice_axiom, [])).
fof(f123, plain, (~ doDivides0(xl, xn) & ! [X0] : (~ (xn = sdtasdt0(xl, X0)) | ~ aNaturalNumber0(X0)) & (? [X1] : (sP1(X1) & (sdtsldt0(xm, xl) = X1) & (xm = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) | (sz00 = xl))), inference(rectify, [], [f99])).
fof(f99, plain, (~ doDivides0(xl, xn) & ! [X4] : (~ (xn = sdtasdt0(xl, X4)) | ~ aNaturalNumber0(X4)) & (? [X0] : (sP1(X0) & (sdtsldt0(xm, xl) = X0) & (xm = sdtasdt0(xl, X0)) & aNaturalNumber0(X0)) | (sz00 = xl))), inference(definition_folding, [], [f96, e98, e97])).
fof(f96, plain, (~ doDivides0(xl, xn) & ! [X4] : (~ (xn = sdtasdt0(xl, X4)) | ~ aNaturalNumber0(X4)) & (? [X0] : (? [X1] : (? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & sdtlseqdt0(X0, X1) & ? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) & (sdtsldt0(xm, xl) = X0) & (xm = sdtasdt0(xl, X0)) & aNaturalNumber0(X0)) | (sz00 = xl))), inference(flattening, [], [f95])).
fof(f95, plain, ((~ doDivides0(xl, xn) & ! [X4] : (~ (xn = sdtasdt0(xl, X4)) | ~ aNaturalNumber0(X4))) & (? [X0] : (? [X1] : (? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & sdtlseqdt0(X0, X1) & ? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) & (sdtsldt0(xm, xl) = X0) & (xm = sdtasdt0(xl, X0)) & aNaturalNumber0(X0)) | (sz00 = xl))), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ~ ((~ (sz00 = xl) => ? [X0] : (? [X1] : (? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & sdtlseqdt0(X0, X1) & ? [X3] : ((sdtpldt0(X0, X3) = X1) & aNaturalNumber0(X3)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) & (sdtsldt0(xm, xl) = X0) & (xm = sdtasdt0(xl, X0)) & aNaturalNumber0(X0))) => (doDivides0(xl, xn) | ? [X4] : ((xn = sdtasdt0(xl, X4)) & aNaturalNumber0(X4)))), inference(rectify, [], [f37])).
fof(f37, plain, ~ ((~ (sz00 = xl) => ? [X0] : (? [X1] : (? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & sdtlseqdt0(X0, X1) & ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) & (sdtsldt0(xm, xl) = X0) & (xm = sdtasdt0(xl, X0)) & aNaturalNumber0(X0))) => (doDivides0(xl, xn) | ? [X0] : ((xn = sdtasdt0(xl, X0)) & aNaturalNumber0(X0)))), inference(negated_conjecture, [], [f36])).
fof(f36, plain, ~ ((~ (sz00 = xl) => ? [X0] : (? [X1] : (? [X2] : ((xn = sdtasdt0(xl, X2)) & (sdtpldt0(sdtasdt0(xl, X0), sdtasdt0(xl, X2)) = sdtpldt0(sdtasdt0(xl, X0), xn)) & (sdtmndt0(X1, X0) = X2) & (sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & sdtlseqdt0(X0, X1) & ? [X2] : ((sdtpldt0(X0, X2) = X1) & aNaturalNumber0(X2)) & (sdtsldt0(sdtpldt0(xm, xn), xl) = X1) & (sdtpldt0(xm, xn) = sdtasdt0(xl, X1)) & aNaturalNumber0(X1)) & (sdtsldt0(xm, xl) = X0) & (xm = sdtasdt0(xl, X0)) & aNaturalNumber0(X0))) => (doDivides0(xl, xn) | ? [X0] : ((xn = sdtasdt0(xl, X0)) & aNaturalNumber0(X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', m__)).
fof(f18287, plain, (doDivides0(xl, xn) | ~ aNaturalNumber0(sdtmndt0(sK4, sK5)) | ~ aNaturalNumber0(xl) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(superposition, [], [f221, f5648])).
fof(f5648, plain, ((xn = sdtasdt0(xl, sdtmndt0(sK4, sK5))) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5 | ~ spl10_36)), inference(backward_demodulation, [], [f5561, f5585])).
fof(f5561, plain, ((xn = sdtasdt0(xl, sdtmndt0(sK6(sK5), sK5))) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5)), inference(backward_demodulation, [], [f5358, f5559])).
fof(f5358, plain, ((xn = sdtasdt0(xl, sK8(sK5, sK6(sK5)))) | (spl10_1 | ~ spl10_2 | ~ spl10_4 | ~ spl10_5)), inference(resolution, [], [f390, f4850])).
fof(f390, plain, ! [X0] : (~ sP1(X0) | (xn = sdtasdt0(xl, sK8(X0, sK6(X0))))), inference(resolution, [], [f200, f195])).
fof(f200, plain, ! [X0, X1] : (~ sP0(X0, X1) | (xn = sdtasdt0(xl, sK8(X0, X1)))), inference(cnf_transformation, [], [f122])).
fof(f221, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f213, f130])).
fof(f130, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f45])).
fof(f45, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', mSortsB_02)).
fof(f213, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f174])).
fof(f174, plain, ! [X2, X0, X1] : (doDivides0(X0, X1) | ~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtasdt0(X0, sK3(X0, X1)) = X1) & aNaturalNumber0(sK3(X0, X1))) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3])], [f107, f108])).
fof(f108, plain, ! [X1, X0] : (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) => ((sdtasdt0(X0, sK3(X0, X1)) = X1) & aNaturalNumber0(sK3(X0, X1)))), introduced(choice_axiom, [])).
fof(f107, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f106])).
fof(f106, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f88])).
fof(f88, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f87])).
fof(f87, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', mDefDiv)).
fof(f3106, plain, (~ spl10_2 | spl10_35), inference(avatar_contradiction_clause, [], [f3105])).
fof(f3105, plain, ($false | (~ spl10_2 | spl10_35)), inference(subsumption_resolution, [], [f3104, f231])).
fof(f3104, plain, (~ sP1(sK9) | spl10_35), inference(resolution, [], [f3040, f192])).
fof(f192, plain, ! [X0] : (aNaturalNumber0(sK7(X0)) | ~ sP1(X0)), inference(cnf_transformation, [], [f119])).
fof(f3040, plain, (~ aNaturalNumber0(sK7(sK9)) | spl10_35), inference(avatar_component_clause, [], [f3038])).
fof(f3038, plain, (spl10_35 <=> aNaturalNumber0(sK7(sK9))), introduced(avatar_definition, [new_symbols(naming, [spl10_35])])).
fof(f3045, plain, (~ spl10_35 | spl10_36 | ~ spl10_2 | ~ spl10_5), inference(avatar_split_clause, [], [f3036, f244, f229, f3042, f3038])).
fof(f3036, plain, (aNaturalNumber0(sK6(sK9)) | ~ aNaturalNumber0(sK7(sK9)) | (~ spl10_2 | ~ spl10_5)), inference(subsumption_resolution, [], [f3023, f246])).
fof(f3023, plain, (aNaturalNumber0(sK6(sK9)) | ~ aNaturalNumber0(sK7(sK9)) | ~ aNaturalNumber0(sK9) | ~ spl10_2), inference(superposition, [], [f129, f350])).
fof(f350, plain, ((sK6(sK9) = sdtpldt0(sK9, sK7(sK9))) | ~ spl10_2), inference(resolution, [], [f193, f231])).
fof(f193, plain, ! [X0] : (~ sP1(X0) | (sK6(X0) = sdtpldt0(X0, sK7(X0)))), inference(cnf_transformation, [], [f119])).
fof(f129, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f43])).
fof(f43, plain, ! [X0, X1] : (aNaturalNumber0(sdtpldt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtpldt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', mSortsB)).
fof(f827, plain, ~ spl10_1, inference(avatar_contradiction_clause, [], [f826])).
fof(f826, plain, ($false | ~ spl10_1), inference(subsumption_resolution, [], [f825, f768])).
fof(f768, plain, (~ (sz00 = xn) | ~ spl10_1), inference(backward_demodulation, [], [f724, f753])).
fof(f753, plain, ((sz00 = xm) | ~ spl10_1), inference(forward_demodulation, [], [f728, f309])).
fof(f309, plain, (sz00 = sdtasdt0(sz00, sK5)), inference(resolution, [], [f140, f183])).
fof(f140, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sz00 = sdtasdt0(sz00, X0))), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ! [X0] : (((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00))) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (aNaturalNumber0(X0) => ((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', m_MulZero)).
fof(f728, plain, ((xm = sdtasdt0(sz00, sK5)) | ~ spl10_1), inference(backward_demodulation, [], [f184, f227])).
fof(f227, plain, ((sz00 = xl) | ~ spl10_1), inference(avatar_component_clause, [], [f225])).
fof(f724, plain, ~ (xm = xn), inference(subsumption_resolution, [], [f249, f183])).
fof(f249, plain, (~ (xm = xn) | ~ aNaturalNumber0(sK5)), inference(superposition, [], [f205, f184])).
fof(f205, plain, ! [X0] : (~ (xn = sdtasdt0(xl, X0)) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f125])).
fof(f825, plain, ((sz00 = xn) | ~ spl10_1), inference(backward_demodulation, [], [f824, f308])).
fof(f308, plain, (sz00 = sdtasdt0(sz00, sK4)), inference(resolution, [], [f140, f186])).
fof(f824, plain, ((xn = sdtasdt0(sz00, sK4)) | ~ spl10_1), inference(forward_demodulation, [], [f823, f267])).
fof(f267, plain, (xn = sdtpldt0(sz00, xn)), inference(resolution, [], [f134, f182])).
fof(f182, plain, aNaturalNumber0(xn), inference(cnf_transformation, [], [f34])).
fof(f134, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sdtpldt0(sz00, X0) = X0)), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0] : (aNaturalNumber0(X0) => ((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM476+2.p', m_AddZero)).
fof(f823, plain, ((sdtpldt0(sz00, xn) = sdtasdt0(sz00, sK4)) | ~ spl10_1), inference(forward_demodulation, [], [f822, f753])).
fof(f822, plain, ((sdtpldt0(xm, xn) = sdtasdt0(sz00, sK4)) | ~ spl10_1), inference(forward_demodulation, [], [f187, f227])).
fof(f247, plain, (spl10_1 | spl10_5), inference(avatar_split_clause, [], [f201, f244, f225])).
fof(f201, plain, (aNaturalNumber0(sK9) | (sz00 = xl)), inference(cnf_transformation, [], [f125])).
fof(f242, plain, (spl10_1 | spl10_4), inference(avatar_split_clause, [], [f202, f239, f225])).
fof(f202, plain, ((xm = sdtasdt0(xl, sK9)) | (sz00 = xl)), inference(cnf_transformation, [], [f125])).
fof(f232, plain, (spl10_1 | spl10_2), inference(avatar_split_clause, [], [f204, f229, f225])).
fof(f204, plain, (sP1(sK9) | (sz00 = xl)), inference(cnf_transformation, [], [f125])).