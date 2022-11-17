fof(f2915, plain, $false, inference(avatar_sat_refutation, [], [f511, f529, f551, f2914])).
fof(f2914, plain, (spl11_3 | ~ spl11_7), inference(avatar_contradiction_clause, [], [f2913])).
fof(f2913, plain, ($false | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f2902, f1099])).
fof(f1099, plain, (isPrime0(sK6(sK7)) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f1098, f229])).
fof(f229, plain, aNaturalNumber0(sK7), inference(cnf_transformation, [], [f145])).
fof(f145, plain, (~ isPrime0(xk) & (~ (xk = sK7) & ~ (sz10 = sK7) & doDivides0(sK7, xk) & ((xk = sdtasdt0(sK7, sK8)) & aNaturalNumber0(sK8)) & aNaturalNumber0(sK7))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f113, f144, f143])).
fof(f143, plain, (? [X0] : (~ (xk = X0) & ~ (sz10 = X0) & doDivides0(X0, xk) & ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1)) & aNaturalNumber0(X0)) => (~ (xk = sK7) & ~ (sz10 = sK7) & doDivides0(sK7, xk) & ? [X1] : ((xk = sdtasdt0(sK7, X1)) & aNaturalNumber0(X1)) & aNaturalNumber0(sK7))), introduced(choice_axiom, [])).
fof(f144, plain, (? [X1] : ((xk = sdtasdt0(sK7, X1)) & aNaturalNumber0(X1)) => ((xk = sdtasdt0(sK7, sK8)) & aNaturalNumber0(sK8))), introduced(choice_axiom, [])).
fof(f113, plain, (~ isPrime0(xk) & ? [X0] : (~ (xk = X0) & ~ (sz10 = X0) & doDivides0(X0, xk) & ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1)) & aNaturalNumber0(X0))), inference(flattening, [], [f112])).
fof(f112, plain, (~ isPrime0(xk) & ? [X0] : ((~ (xk = X0) & ~ (sz10 = X0)) & (doDivides0(X0, xk) & ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1)) & aNaturalNumber0(X0)))), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ~ (isPrime0(xk) | ! [X0] : ((doDivides0(X0, xk) & ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1)) & aNaturalNumber0(X0)) => ((xk = X0) | (sz10 = X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', m__1725)).
fof(f1098, plain, (isPrime0(sK6(sK7)) | ~ aNaturalNumber0(sK7) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f1097, f487])).
fof(f487, plain, (~ (sz00 = sK7) | spl11_3), inference(avatar_component_clause, [], [f486])).
fof(f486, plain, (spl11_3 <=> (sz00 = sK7)), introduced(avatar_definition, [new_symbols(naming, [spl11_3])])).
fof(f1097, plain, (isPrime0(sK6(sK7)) | (sz00 = sK7) | ~ aNaturalNumber0(sK7) | ~ spl11_7), inference(subsumption_resolution, [], [f1095, f233])).
fof(f233, plain, ~ (sz10 = sK7), inference(cnf_transformation, [], [f145])).
fof(f1095, plain, (isPrime0(sK6(sK7)) | (sz10 = sK7) | (sz00 = sK7) | ~ aNaturalNumber0(sK7) | ~ spl11_7), inference(resolution, [], [f651, f226])).
fof(f226, plain, ! [X0] : (~ iLess0(X0, xk) | isPrime0(sK6(X0)) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f142])).
fof(f142, plain, ! [X0] : ((isPrime0(sK6(X0)) & ! [X2] : ((sK6(X0) = X2) | (sz10 = X2) | (~ doDivides0(X2, sK6(X0)) & ! [X3] : (~ (sdtasdt0(X2, X3) = sK6(X0)) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X2)) & ~ (sz10 = sK6(X0)) & ~ (sz00 = sK6(X0)) & doDivides0(sK6(X0), X0) & sP0(X0, sK6(X0)) & aNaturalNumber0(sK6(X0))) | ~ iLess0(X0, xk) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f117, f141])).
fof(f141, plain, ! [X0] : (? [X1] : (isPrime0(X1) & ! [X2] : ((X1 = X2) | (sz10 = X2) | (~ doDivides0(X2, X1) & ! [X3] : (~ (sdtasdt0(X2, X3) = X1) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X2)) & ~ (sz10 = X1) & ~ (sz00 = X1) & doDivides0(X1, X0) & sP0(X0, X1) & aNaturalNumber0(X1)) => (isPrime0(sK6(X0)) & ! [X2] : ((sK6(X0) = X2) | (sz10 = X2) | (~ doDivides0(X2, sK6(X0)) & ! [X3] : (~ (sdtasdt0(X2, X3) = sK6(X0)) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X2)) & ~ (sz10 = sK6(X0)) & ~ (sz00 = sK6(X0)) & doDivides0(sK6(X0), X0) & sP0(X0, sK6(X0)) & aNaturalNumber0(sK6(X0)))), introduced(choice_axiom, [])).
fof(f117, plain, ! [X0] : (? [X1] : (isPrime0(X1) & ! [X2] : ((X1 = X2) | (sz10 = X2) | (~ doDivides0(X2, X1) & ! [X3] : (~ (sdtasdt0(X2, X3) = X1) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X2)) & ~ (sz10 = X1) & ~ (sz00 = X1) & doDivides0(X1, X0) & sP0(X0, X1) & aNaturalNumber0(X1)) | ~ iLess0(X0, xk) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(definition_folding, [], [f111, e116])).
fof(f116, plain, ! [X0, X1] : (? [X4] : ((sdtasdt0(X1, X4) = X0) & aNaturalNumber0(X4)) | ~ sP0(X0, X1)), inference(usedef, [], [e116])).
fof(e116, plain, ! [X0, X1] : (sP0(X0, X1) <=> ? [X4] : ((sdtasdt0(X1, X4) = X0) & aNaturalNumber0(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f111, plain, ! [X0] : (? [X1] : (isPrime0(X1) & ! [X2] : ((X1 = X2) | (sz10 = X2) | (~ doDivides0(X2, X1) & ! [X3] : (~ (sdtasdt0(X2, X3) = X1) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X2)) & ~ (sz10 = X1) & ~ (sz00 = X1) & doDivides0(X1, X0) & ? [X4] : ((sdtasdt0(X1, X4) = X0) & aNaturalNumber0(X4)) & aNaturalNumber0(X1)) | ~ iLess0(X0, xk) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f110])).
fof(f110, plain, ! [X0] : ((? [X1] : (isPrime0(X1) & ! [X2] : (((X1 = X2) | (sz10 = X2)) | ((~ doDivides0(X2, X1) & ! [X3] : (~ (sdtasdt0(X2, X3) = X1) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X2))) & ~ (sz10 = X1) & ~ (sz00 = X1) & doDivides0(X1, X0) & ? [X4] : ((sdtasdt0(X1, X4) = X0) & aNaturalNumber0(X4)) & aNaturalNumber0(X1)) | ~ iLess0(X0, xk)) | ((sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f46])).
fof(f46, plain, ! [X0] : ((~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)) => (iLess0(X0, xk) => ? [X1] : (isPrime0(X1) & ! [X2] : (((doDivides0(X2, X1) | ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3))) & aNaturalNumber0(X2)) => ((X1 = X2) | (sz10 = X2))) & ~ (sz10 = X1) & ~ (sz00 = X1) & doDivides0(X1, X0) & ? [X4] : ((sdtasdt0(X1, X4) = X0) & aNaturalNumber0(X4)) & aNaturalNumber0(X1)))), inference(rectify, [], [f39])).
fof(f39, plain, ! [X0] : ((~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)) => (iLess0(X0, xk) => ? [X1] : (isPrime0(X1) & ! [X2] : (((doDivides0(X2, X1) | ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3))) & aNaturalNumber0(X2)) => ((X1 = X2) | (sz10 = X2))) & ~ (sz10 = X1) & ~ (sz00 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', m__1700)).
fof(f651, plain, (iLess0(sK7, xk) | ~ spl11_7), inference(subsumption_resolution, [], [f650, f229])).
fof(f650, plain, (iLess0(sK7, xk) | ~ aNaturalNumber0(sK7) | ~ spl11_7), inference(subsumption_resolution, [], [f649, f216])).
fof(f216, plain, aNaturalNumber0(xk), inference(cnf_transformation, [], [f38])).
fof(f38, plain, aNaturalNumber0(xk), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', m__1716)).
fof(f649, plain, (iLess0(sK7, xk) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(sK7) | ~ spl11_7), inference(subsumption_resolution, [], [f636, f234])).
fof(f234, plain, ~ (xk = sK7), inference(cnf_transformation, [], [f145])).
fof(f636, plain, (iLess0(sK7, xk) | (xk = sK7) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(sK7) | ~ spl11_7), inference(resolution, [], [f197, f510])).
fof(f510, plain, (sdtlseqdt0(sK7, xk) | ~ spl11_7), inference(avatar_component_clause, [], [f508])).
fof(f508, plain, (spl11_7 <=> sdtlseqdt0(sK7, xk)), introduced(avatar_definition, [new_symbols(naming, [spl11_7])])).
fof(f197, plain, ! [X0, X1] : (~ sdtlseqdt0(X0, X1) | iLess0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ! [X0, X1] : (iLess0(X0, X1) | ~ sdtlseqdt0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f92])).
fof(f92, plain, ! [X0, X1] : ((iLess0(X0, X1) | (~ sdtlseqdt0(X0, X1) | (X0 = X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X0, X1) & ~ (X0 = X1)) => iLess0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', mIH_03)).
fof(f2902, plain, (~ isPrime0(sK6(sK7)) | (spl11_3 | ~ spl11_7)), inference(resolution, [], [f2808, f242])).
fof(f242, plain, ! [X0] : (~ sP1(X0) | ~ isPrime0(X0)), inference(cnf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : ((~ isPrime0(X0) & ((~ (sK9(X0) = X0) & ~ (sz10 = sK9(X0)) & doDivides0(sK9(X0), X0) & ((sdtasdt0(sK9(X0), sK10(X0)) = X0) & aNaturalNumber0(sK10(X0))) & aNaturalNumber0(sK9(X0))) | (sz10 = X0) | (sz00 = X0))) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9, sK10])], [f146, f148, f147])).
fof(f147, plain, ! [X0] : (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => (~ (sK9(X0) = X0) & ~ (sz10 = sK9(X0)) & doDivides0(sK9(X0), X0) & ? [X2] : ((sdtasdt0(sK9(X0), X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(sK9(X0)))), introduced(choice_axiom, [])).
fof(f148, plain, ! [X0] : (? [X2] : ((sdtasdt0(sK9(X0), X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(sK9(X0), sK10(X0)) = X0) & aNaturalNumber0(sK10(X0)))), introduced(choice_axiom, [])).
fof(f146, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) | ~ sP1(X0)), inference(nnf_transformation, [], [f118])).
fof(f118, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) | ~ sP1(X0)), inference(usedef, [], [e118])).
fof(e118, plain, ! [X0] : (sP1(X0) <=> (~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f2808, plain, (sP1(sK6(sK7)) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f2801, f1102])).
fof(f1102, plain, (aNaturalNumber0(sK6(sK7)) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f1101, f229])).
fof(f1101, plain, (aNaturalNumber0(sK6(sK7)) | ~ aNaturalNumber0(sK7) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f1100, f487])).
fof(f1100, plain, (aNaturalNumber0(sK6(sK7)) | (sz00 = sK7) | ~ aNaturalNumber0(sK7) | ~ spl11_7), inference(subsumption_resolution, [], [f1096, f233])).
fof(f1096, plain, (aNaturalNumber0(sK6(sK7)) | (sz10 = sK7) | (sz00 = sK7) | ~ aNaturalNumber0(sK7) | ~ spl11_7), inference(resolution, [], [f651, f219])).
fof(f219, plain, ! [X0] : (~ iLess0(X0, xk) | aNaturalNumber0(sK6(X0)) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f142])).
fof(f2801, plain, (sP1(sK6(sK7)) | ~ aNaturalNumber0(sK6(sK7)) | (spl11_3 | ~ spl11_7)), inference(resolution, [], [f2338, f244])).
fof(f244, plain, ! [X0] : (~ doDivides0(X0, xk) | sP1(X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f150])).
fof(f150, plain, ! [X0] : (sP1(X0) | (~ doDivides0(X0, xk) & ! [X1] : (~ (sdtasdt0(X0, X1) = xk) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f119])).
fof(f119, plain, ! [X0] : (sP1(X0) | (~ doDivides0(X0, xk) & ! [X3] : (~ (xk = sdtasdt0(X0, X3)) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X0)), inference(definition_folding, [], [f115, e118])).
fof(f115, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) | (~ doDivides0(X0, xk) & ! [X3] : (~ (xk = sdtasdt0(X0, X3)) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f114])).
fof(f114, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : ((~ (X0 = X1) & ~ (sz10 = X1)) & (doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1))) | (sz10 = X0) | (sz00 = X0))) | (~ doDivides0(X0, xk) & ! [X3] : (~ (xk = sdtasdt0(X0, X3)) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f47])).
fof(f47, plain, ~ ? [X0] : ((isPrime0(X0) | (! [X1] : ((doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => ((X0 = X1) | (sz10 = X1))) & ~ (sz10 = X0) & ~ (sz00 = X0))) & (doDivides0(X0, xk) | ? [X3] : ((xk = sdtasdt0(X0, X3)) & aNaturalNumber0(X3))) & aNaturalNumber0(X0)), inference(rectify, [], [f43])).
fof(f43, plain, ~ ? [X0] : ((isPrime0(X0) | (! [X1] : ((doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => ((X0 = X1) | (sz10 = X1))) & ~ (sz10 = X0) & ~ (sz00 = X0))) & (doDivides0(X0, xk) | ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)), inference(negated_conjecture, [], [f42])).
fof(f42, plain, ~ ? [X0] : ((isPrime0(X0) | (! [X1] : ((doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => ((X0 = X1) | (sz10 = X1))) & ~ (sz10 = X0) & ~ (sz00 = X0))) & (doDivides0(X0, xk) | ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', m__)).
fof(f2338, plain, (doDivides0(sK6(sK7), xk) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f2337, f229])).
fof(f2337, plain, (doDivides0(sK6(sK7), xk) | ~ aNaturalNumber0(sK7) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f2336, f487])).
fof(f2336, plain, (doDivides0(sK6(sK7), xk) | (sz00 = sK7) | ~ aNaturalNumber0(sK7) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f2335, f233])).
fof(f2335, plain, (doDivides0(sK6(sK7), xk) | (sz10 = sK7) | (sz00 = sK7) | ~ aNaturalNumber0(sK7) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f2334, f651])).
fof(f2334, plain, (doDivides0(sK6(sK7), xk) | ~ iLess0(sK7, xk) | (sz10 = sK7) | (sz00 = sK7) | ~ aNaturalNumber0(sK7) | (spl11_3 | ~ spl11_7)), inference(subsumption_resolution, [], [f2318, f1102])).
fof(f2318, plain, (doDivides0(sK6(sK7), xk) | ~ aNaturalNumber0(sK6(sK7)) | ~ iLess0(sK7, xk) | (sz10 = sK7) | (sz00 = sK7) | ~ aNaturalNumber0(sK7)), inference(resolution, [], [f1029, f221])).
fof(f221, plain, ! [X0] : (doDivides0(sK6(X0), X0) | ~ iLess0(X0, xk) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f142])).
fof(f1029, plain, ! [X11] : (~ doDivides0(X11, sK7) | doDivides0(X11, xk) | ~ aNaturalNumber0(X11)), inference(subsumption_resolution, [], [f1028, f229])).
fof(f1028, plain, ! [X11] : (doDivides0(X11, xk) | ~ doDivides0(X11, sK7) | ~ aNaturalNumber0(sK7) | ~ aNaturalNumber0(X11)), inference(subsumption_resolution, [], [f1009, f216])).
fof(f1009, plain, ! [X11] : (doDivides0(X11, xk) | ~ doDivides0(X11, sK7) | ~ aNaturalNumber0(xk) | ~ aNaturalNumber0(sK7) | ~ aNaturalNumber0(X11)), inference(resolution, [], [f204, f232])).
fof(f232, plain, doDivides0(sK7, xk), inference(cnf_transformation, [], [f145])).
fof(f204, plain, ! [X2, X0, X1] : (~ doDivides0(X1, X2) | doDivides0(X0, X2) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ! [X0, X1, X2] : (doDivides0(X0, X2) | ~ doDivides0(X1, X2) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f98])).
fof(f98, plain, ! [X0, X1, X2] : ((doDivides0(X0, X2) | (~ doDivides0(X1, X2) | ~ doDivides0(X0, X1))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f32])).
fof(f32, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((doDivides0(X1, X2) & doDivides0(X0, X1)) => doDivides0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', mDivTrans)).
fof(f551, plain, ~ spl11_3, inference(avatar_contradiction_clause, [], [f550])).
fof(f550, plain, ($false | ~ spl11_3), inference(subsumption_resolution, [], [f549, f227])).
fof(f227, plain, ~ (sz00 = xk), inference(cnf_transformation, [], [f40])).
fof(f40, plain, (~ (sz10 = xk) & ~ (sz00 = xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', m__1716_04)).
fof(f549, plain, ((sz00 = xk) | ~ spl11_3), inference(forward_demodulation, [], [f548, f298])).
fof(f298, plain, (sz00 = sdtasdt0(sz00, sK8)), inference(resolution, [], [f165, f230])).
fof(f230, plain, aNaturalNumber0(sK8), inference(cnf_transformation, [], [f145])).
fof(f165, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sz00 = sdtasdt0(sz00, X0))), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ! [X0] : (((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00))) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (aNaturalNumber0(X0) => ((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', m_MulZero)).
fof(f548, plain, ((xk = sdtasdt0(sz00, sK8)) | ~ spl11_3), inference(forward_demodulation, [], [f231, f488])).
fof(f488, plain, ((sz00 = sK7) | ~ spl11_3), inference(avatar_component_clause, [], [f486])).
fof(f231, plain, (xk = sdtasdt0(sK7, sK8)), inference(cnf_transformation, [], [f145])).
fof(f529, plain, ~ spl11_5, inference(avatar_contradiction_clause, [], [f528])).
fof(f528, plain, ($false | ~ spl11_5), inference(subsumption_resolution, [], [f527, f227])).
fof(f527, plain, ((sz00 = xk) | ~ spl11_5), inference(forward_demodulation, [], [f513, f292])).
fof(f292, plain, (sz00 = sdtasdt0(sK7, sz00)), inference(resolution, [], [f164, f229])).
fof(f164, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sz00 = sdtasdt0(X0, sz00))), inference(cnf_transformation, [], [f62])).
fof(f513, plain, ((xk = sdtasdt0(sK7, sz00)) | ~ spl11_5), inference(backward_demodulation, [], [f231, f499])).
fof(f499, plain, ((sz00 = sK8) | ~ spl11_5), inference(avatar_component_clause, [], [f497])).
fof(f497, plain, (spl11_5 <=> (sz00 = sK8)), introduced(avatar_definition, [new_symbols(naming, [spl11_5])])).
fof(f511, plain, (spl11_5 | spl11_7), inference(avatar_split_clause, [], [f506, f508, f497])).
fof(f506, plain, (sdtlseqdt0(sK7, xk) | (sz00 = sK8)), inference(subsumption_resolution, [], [f505, f230])).
fof(f505, plain, (sdtlseqdt0(sK7, xk) | (sz00 = sK8) | ~ aNaturalNumber0(sK8)), inference(subsumption_resolution, [], [f477, f229])).
fof(f477, plain, (sdtlseqdt0(sK7, xk) | (sz00 = sK8) | ~ aNaturalNumber0(sK7) | ~ aNaturalNumber0(sK8)), inference(superposition, [], [f196, f231])).
fof(f196, plain, ! [X0, X1] : (sdtlseqdt0(X1, sdtasdt0(X1, X0)) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ! [X0, X1] : (sdtlseqdt0(X1, sdtasdt0(X1, X0)) | (sz00 = X0) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f90])).
fof(f90, plain, ! [X0, X1] : ((sdtlseqdt0(X1, sdtasdt0(X1, X0)) | (sz00 = X0)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (~ (sz00 = X0) => sdtlseqdt0(X1, sdtasdt0(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM483+3.p', mMonMul2)).