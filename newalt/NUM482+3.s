fof(f326, plain, $false, inference(subsumption_resolution, [], [f325, f233])).
fof(f233, plain, isPrime0(xk), inference(cnf_transformation, [], [f145])).
fof(f145, plain, (! [X0] : (sP1(X0) | (~ doDivides0(X0, xk) & ! [X1] : (~ (sdtasdt0(X0, X1) = xk) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0)) & isPrime0(xk) & ! [X2] : ((xk = X2) | (sz10 = X2) | (~ doDivides0(X2, xk) & ! [X3] : (~ (xk = sdtasdt0(X2, X3)) | ~ aNaturalNumber0(X3))) | ~ aNaturalNumber0(X2))), inference(rectify, [], [f116])).
fof(f116, plain, (! [X2] : (sP1(X2) | (~ doDivides0(X2, xk) & ! [X5] : (~ (xk = sdtasdt0(X2, X5)) | ~ aNaturalNumber0(X5))) | ~ aNaturalNumber0(X2)) & isPrime0(xk) & ! [X0] : ((xk = X0) | (sz10 = X0) | (~ doDivides0(X0, xk) & ! [X1] : (~ (sdtasdt0(X0, X1) = xk) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0))), inference(definition_folding, [], [f112, e115])).
fof(f115, plain, ! [X2] : ((~ isPrime0(X2) & (? [X3] : (~ (X2 = X3) & ~ (sz10 = X3) & doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3)) | (sz10 = X2) | (sz00 = X2))) | ~ sP1(X2)), inference(usedef, [], [e115])).
fof(e115, plain, ! [X2] : (sP1(X2) <=> (~ isPrime0(X2) & (? [X3] : (~ (X2 = X3) & ~ (sz10 = X3) & doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3)) | (sz10 = X2) | (sz00 = X2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f112, plain, (! [X2] : ((~ isPrime0(X2) & (? [X3] : (~ (X2 = X3) & ~ (sz10 = X3) & doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3)) | (sz10 = X2) | (sz00 = X2))) | (~ doDivides0(X2, xk) & ! [X5] : (~ (xk = sdtasdt0(X2, X5)) | ~ aNaturalNumber0(X5))) | ~ aNaturalNumber0(X2)) & isPrime0(xk) & ! [X0] : ((xk = X0) | (sz10 = X0) | (~ doDivides0(X0, xk) & ! [X1] : (~ (sdtasdt0(X0, X1) = xk) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0))), inference(flattening, [], [f111])).
fof(f111, plain, (! [X2] : ((~ isPrime0(X2) & (? [X3] : ((~ (X2 = X3) & ~ (sz10 = X3)) & (doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3))) | (sz10 = X2) | (sz00 = X2))) | (~ doDivides0(X2, xk) & ! [X5] : (~ (xk = sdtasdt0(X2, X5)) | ~ aNaturalNumber0(X5))) | ~ aNaturalNumber0(X2)) & (isPrime0(xk) & ! [X0] : (((xk = X0) | (sz10 = X0)) | ((~ doDivides0(X0, xk) & ! [X1] : (~ (sdtasdt0(X0, X1) = xk) | ~ aNaturalNumber0(X1))) | ~ aNaturalNumber0(X0))))), inference(ennf_transformation, [], [f46])).
fof(f46, plain, ~ ((isPrime0(xk) & ! [X0] : (((doDivides0(X0, xk) | ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xk = X0) | (sz10 = X0)))) => ? [X2] : ((isPrime0(X2) | (! [X3] : ((doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3)) => ((X2 = X3) | (sz10 = X3))) & ~ (sz10 = X2) & ~ (sz00 = X2))) & (doDivides0(X2, xk) | ? [X5] : ((xk = sdtasdt0(X2, X5)) & aNaturalNumber0(X5))) & aNaturalNumber0(X2))), inference(rectify, [], [f42])).
fof(f42, plain, ~ ((isPrime0(xk) & ! [X0] : (((doDivides0(X0, xk) | ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xk = X0) | (sz10 = X0)))) => ? [X0] : ((isPrime0(X0) | (! [X1] : ((doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => ((X0 = X1) | (sz10 = X1))) & ~ (sz10 = X0) & ~ (sz00 = X0))) & (doDivides0(X0, xk) | ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1))) & aNaturalNumber0(X0))), inference(negated_conjecture, [], [f41])).
fof(f41, plain, ~ ((isPrime0(xk) & ! [X0] : (((doDivides0(X0, xk) | ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1))) & aNaturalNumber0(X0)) => ((xk = X0) | (sz10 = X0)))) => ? [X0] : ((isPrime0(X0) | (! [X1] : ((doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => ((X0 = X1) | (sz10 = X1))) & ~ (sz10 = X0) & ~ (sz00 = X0))) & (doDivides0(X0, xk) | ? [X1] : ((sdtasdt0(X0, X1) = xk) & aNaturalNumber0(X1))) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM482+3.p', m__)).
fof(f325, plain, ~ isPrime0(xk), inference(resolution, [], [f322, f230])).
fof(f230, plain, ! [X0] : (~ sP1(X0) | ~ isPrime0(X0)), inference(cnf_transformation, [], [f144])).
fof(f144, plain, ! [X0] : ((~ isPrime0(X0) & ((~ (sK7(X0) = X0) & ~ (sz10 = sK7(X0)) & doDivides0(sK7(X0), X0) & ((sdtasdt0(sK7(X0), sK8(X0)) = X0) & aNaturalNumber0(sK8(X0))) & aNaturalNumber0(sK7(X0))) | (sz10 = X0) | (sz00 = X0))) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8])], [f141, f143, f142])).
fof(f142, plain, ! [X0] : (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => (~ (sK7(X0) = X0) & ~ (sz10 = sK7(X0)) & doDivides0(sK7(X0), X0) & ? [X2] : ((sdtasdt0(sK7(X0), X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(sK7(X0)))), introduced(choice_axiom, [])).
fof(f143, plain, ! [X0] : (? [X2] : ((sdtasdt0(sK7(X0), X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(sK7(X0), sK8(X0)) = X0) & aNaturalNumber0(sK8(X0)))), introduced(choice_axiom, [])).
fof(f141, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) | ~ sP1(X0)), inference(rectify, [], [f140])).
fof(f140, plain, ! [X2] : ((~ isPrime0(X2) & (? [X3] : (~ (X2 = X3) & ~ (sz10 = X3) & doDivides0(X3, X2) & ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4)) & aNaturalNumber0(X3)) | (sz10 = X2) | (sz00 = X2))) | ~ sP1(X2)), inference(nnf_transformation, [], [f115])).
fof(f322, plain, sP1(xk), inference(subsumption_resolution, [], [f321, f211])).
fof(f211, plain, aNaturalNumber0(xk), inference(cnf_transformation, [], [f38])).
fof(f38, plain, aNaturalNumber0(xk), file('/home/ubuntu/library/tptp/Problems/NUM/NUM482+3.p', m__1716)).
fof(f321, plain, (sP1(xk) | ~ aNaturalNumber0(xk)), inference(resolution, [], [f314, f235])).
fof(f235, plain, ! [X0] : (~ doDivides0(X0, xk) | sP1(X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f145])).
fof(f314, plain, doDivides0(xk, xk), inference(subsumption_resolution, [], [f313, f211])).
fof(f313, plain, (doDivides0(xk, xk) | ~ aNaturalNumber0(xk)), inference(subsumption_resolution, [], [f309, f147])).
fof(f147, plain, aNaturalNumber0(sz10), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (sz00 = sz10) & aNaturalNumber0(sz10)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM482+3.p', mSortsC_01)).
fof(f309, plain, (doDivides0(xk, xk) | ~ aNaturalNumber0(sz10) | ~ aNaturalNumber0(xk)), inference(superposition, [], [f252, f263])).
fof(f263, plain, (xk = sdtasdt0(xk, sz10)), inference(resolution, [], [f157, f211])).
fof(f157, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sdtasdt0(X0, sz10) = X0)), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ! [X0] : (((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : (aNaturalNumber0(X0) => ((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM482+3.p', m_MulUnit)).
fof(f252, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f242, f150])).
fof(f150, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f49])).
fof(f49, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM482+3.p', mSortsB_02)).
fof(f242, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f195])).
fof(f195, plain, ! [X2, X0, X1] : (doDivides0(X0, X1) | ~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtasdt0(X0, sK3(X0, X1)) = X1) & aNaturalNumber0(sK3(X0, X1))) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3])], [f124, f125])).
fof(f125, plain, ! [X1, X0] : (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) => ((sdtasdt0(X0, sK3(X0, X1)) = X1) & aNaturalNumber0(sK3(X0, X1)))), introduced(choice_axiom, [])).
fof(f124, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f123])).
fof(f123, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f94])).
fof(f94, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f93])).
fof(f93, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM482+3.p', mDefDiv)).