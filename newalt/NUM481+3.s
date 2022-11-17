fof(f46408, plain, $false, inference(avatar_sat_refutation, [], [f646, f2783, f3064, f19053, f19935, f21397, f26263, f45729, f46329])).
fof(f46329, plain, (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297), inference(avatar_contradiction_clause, [], [f46328])).
fof(f46328, plain, ($false | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297)), inference(subsumption_resolution, [], [f46289, f26279])).
fof(f26279, plain, (isPrime0(sK9(sK4(sK8))) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295)), inference(subsumption_resolution, [], [f26278, f632])).
fof(f632, plain, (aNaturalNumber0(sK4(sK8)) | ~ spl10_3), inference(avatar_component_clause, [], [f631])).
fof(f631, plain, (spl10_3 <=> aNaturalNumber0(sK4(sK8))), introduced(avatar_definition, [new_symbols(naming, [spl10_3])])).
fof(f26278, plain, (isPrime0(sK9(sK4(sK8))) | ~ aNaturalNumber0(sK4(sK8)) | (spl10_16 | spl10_17 | ~ spl10_295)), inference(subsumption_resolution, [], [f26277, f921])).
fof(f921, plain, (~ (sz00 = sK4(sK8)) | spl10_16), inference(avatar_component_clause, [], [f920])).
fof(f920, plain, (spl10_16 <=> (sz00 = sK4(sK8))), introduced(avatar_definition, [new_symbols(naming, [spl10_16])])).
fof(f26277, plain, (isPrime0(sK9(sK4(sK8))) | (sz00 = sK4(sK8)) | ~ aNaturalNumber0(sK4(sK8)) | (spl10_17 | ~ spl10_295)), inference(subsumption_resolution, [], [f26267, f925])).
fof(f925, plain, (~ (sz10 = sK4(sK8)) | spl10_17), inference(avatar_component_clause, [], [f924])).
fof(f924, plain, (spl10_17 <=> (sz10 = sK4(sK8))), introduced(avatar_definition, [new_symbols(naming, [spl10_17])])).
fof(f26267, plain, (isPrime0(sK9(sK4(sK8))) | (sz10 = sK4(sK8)) | (sz00 = sK4(sK8)) | ~ aNaturalNumber0(sK4(sK8)) | ~ spl10_295), inference(resolution, [], [f18872, f224])).
fof(f224, plain, ! [X3] : (~ iLess0(X3, sK8) | isPrime0(sK9(X3)) | (sz10 = X3) | (sz00 = X3) | ~ aNaturalNumber0(X3)), inference(cnf_transformation, [], [f139])).
fof(f139, plain, (! [X1] : (sP1(X1) | (~ doDivides0(X1, sK8) & ! [X2] : (~ (sdtasdt0(X1, X2) = sK8) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ! [X3] : ((isPrime0(sK9(X3)) & ! [X5] : ((sK9(X3) = X5) | (sz10 = X5) | (~ doDivides0(X5, sK9(X3)) & ! [X6] : (~ (sdtasdt0(X5, X6) = sK9(X3)) | ~ aNaturalNumber0(X6))) | ~ aNaturalNumber0(X5)) & ~ (sz10 = sK9(X3)) & ~ (sz00 = sK9(X3)) & doDivides0(sK9(X3), X3) & sP0(X3, sK9(X3)) & aNaturalNumber0(sK9(X3))) | ~ iLess0(X3, sK8) | (sz10 = X3) | (sz00 = X3) | ~ aNaturalNumber0(X3)) & ~ (sz10 = sK8) & ~ (sz00 = sK8) & aNaturalNumber0(sK8)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f136, f138, f137])).
fof(f137, plain, (? [X0] : (! [X1] : (sP1(X1) | (~ doDivides0(X1, X0) & ! [X2] : (~ (sdtasdt0(X1, X2) = X0) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ! [X3] : (? [X4] : (isPrime0(X4) & ! [X5] : ((X4 = X5) | (sz10 = X5) | (~ doDivides0(X5, X4) & ! [X6] : (~ (sdtasdt0(X5, X6) = X4) | ~ aNaturalNumber0(X6))) | ~ aNaturalNumber0(X5)) & ~ (sz10 = X4) & ~ (sz00 = X4) & doDivides0(X4, X3) & sP0(X3, X4) & aNaturalNumber0(X4)) | ~ iLess0(X3, X0) | (sz10 = X3) | (sz00 = X3) | ~ aNaturalNumber0(X3)) & ~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)) => (! [X1] : (sP1(X1) | (~ doDivides0(X1, sK8) & ! [X2] : (~ (sdtasdt0(X1, X2) = sK8) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ! [X3] : (? [X4] : (isPrime0(X4) & ! [X5] : ((X4 = X5) | (sz10 = X5) | (~ doDivides0(X5, X4) & ! [X6] : (~ (sdtasdt0(X5, X6) = X4) | ~ aNaturalNumber0(X6))) | ~ aNaturalNumber0(X5)) & ~ (sz10 = X4) & ~ (sz00 = X4) & doDivides0(X4, X3) & sP0(X3, X4) & aNaturalNumber0(X4)) | ~ iLess0(X3, sK8) | (sz10 = X3) | (sz00 = X3) | ~ aNaturalNumber0(X3)) & ~ (sz10 = sK8) & ~ (sz00 = sK8) & aNaturalNumber0(sK8))), introduced(choice_axiom, [])).
fof(f138, plain, ! [X3] : (? [X4] : (isPrime0(X4) & ! [X5] : ((X4 = X5) | (sz10 = X5) | (~ doDivides0(X5, X4) & ! [X6] : (~ (sdtasdt0(X5, X6) = X4) | ~ aNaturalNumber0(X6))) | ~ aNaturalNumber0(X5)) & ~ (sz10 = X4) & ~ (sz00 = X4) & doDivides0(X4, X3) & sP0(X3, X4) & aNaturalNumber0(X4)) => (isPrime0(sK9(X3)) & ! [X5] : ((sK9(X3) = X5) | (sz10 = X5) | (~ doDivides0(X5, sK9(X3)) & ! [X6] : (~ (sdtasdt0(X5, X6) = sK9(X3)) | ~ aNaturalNumber0(X6))) | ~ aNaturalNumber0(X5)) & ~ (sz10 = sK9(X3)) & ~ (sz00 = sK9(X3)) & doDivides0(sK9(X3), X3) & sP0(X3, sK9(X3)) & aNaturalNumber0(sK9(X3)))), introduced(choice_axiom, [])).
fof(f136, plain, ? [X0] : (! [X1] : (sP1(X1) | (~ doDivides0(X1, X0) & ! [X2] : (~ (sdtasdt0(X1, X2) = X0) | ~ aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1)) & ! [X3] : (? [X4] : (isPrime0(X4) & ! [X5] : ((X4 = X5) | (sz10 = X5) | (~ doDivides0(X5, X4) & ! [X6] : (~ (sdtasdt0(X5, X6) = X4) | ~ aNaturalNumber0(X6))) | ~ aNaturalNumber0(X5)) & ~ (sz10 = X4) & ~ (sz00 = X4) & doDivides0(X4, X3) & sP0(X3, X4) & aNaturalNumber0(X4)) | ~ iLess0(X3, X0) | (sz10 = X3) | (sz00 = X3) | ~ aNaturalNumber0(X3)) & ~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)), inference(rectify, [], [f109])).
fof(f109, plain, ? [X0] : (! [X6] : (sP1(X6) | (~ doDivides0(X6, X0) & ! [X9] : (~ (sdtasdt0(X6, X9) = X0) | ~ aNaturalNumber0(X9))) | ~ aNaturalNumber0(X6)) & ! [X1] : (? [X2] : (isPrime0(X2) & ! [X3] : ((X2 = X3) | (sz10 = X3) | (~ doDivides0(X3, X2) & ! [X4] : (~ (sdtasdt0(X3, X4) = X2) | ~ aNaturalNumber0(X4))) | ~ aNaturalNumber0(X3)) & ~ (sz10 = X2) & ~ (sz00 = X2) & doDivides0(X2, X1) & sP0(X1, X2) & aNaturalNumber0(X2)) | ~ iLess0(X1, X0) | (sz10 = X1) | (sz00 = X1) | ~ aNaturalNumber0(X1)) & ~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)), inference(definition_folding, [], [f106, e108, e107])).
fof(f107, plain, ! [X1, X2] : (? [X5] : ((sdtasdt0(X2, X5) = X1) & aNaturalNumber0(X5)) | ~ sP0(X1, X2)), inference(usedef, [], [e107])).
fof(e107, plain, ! [X1, X2] : (sP0(X1, X2) <=> ? [X5] : ((sdtasdt0(X2, X5) = X1) & aNaturalNumber0(X5))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f108, plain, ! [X6] : ((~ isPrime0(X6) & (? [X7] : (~ (X6 = X7) & ~ (sz10 = X7) & doDivides0(X7, X6) & ? [X8] : ((sdtasdt0(X7, X8) = X6) & aNaturalNumber0(X8)) & aNaturalNumber0(X7)) | (sz10 = X6) | (sz00 = X6))) | ~ sP1(X6)), inference(usedef, [], [e108])).
fof(e108, plain, ! [X6] : (sP1(X6) <=> (~ isPrime0(X6) & (? [X7] : (~ (X6 = X7) & ~ (sz10 = X7) & doDivides0(X7, X6) & ? [X8] : ((sdtasdt0(X7, X8) = X6) & aNaturalNumber0(X8)) & aNaturalNumber0(X7)) | (sz10 = X6) | (sz00 = X6)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f106, plain, ? [X0] : (! [X6] : ((~ isPrime0(X6) & (? [X7] : (~ (X6 = X7) & ~ (sz10 = X7) & doDivides0(X7, X6) & ? [X8] : ((sdtasdt0(X7, X8) = X6) & aNaturalNumber0(X8)) & aNaturalNumber0(X7)) | (sz10 = X6) | (sz00 = X6))) | (~ doDivides0(X6, X0) & ! [X9] : (~ (sdtasdt0(X6, X9) = X0) | ~ aNaturalNumber0(X9))) | ~ aNaturalNumber0(X6)) & ! [X1] : (? [X2] : (isPrime0(X2) & ! [X3] : ((X2 = X3) | (sz10 = X3) | (~ doDivides0(X3, X2) & ! [X4] : (~ (sdtasdt0(X3, X4) = X2) | ~ aNaturalNumber0(X4))) | ~ aNaturalNumber0(X3)) & ~ (sz10 = X2) & ~ (sz00 = X2) & doDivides0(X2, X1) & ? [X5] : ((sdtasdt0(X2, X5) = X1) & aNaturalNumber0(X5)) & aNaturalNumber0(X2)) | ~ iLess0(X1, X0) | (sz10 = X1) | (sz00 = X1) | ~ aNaturalNumber0(X1)) & ~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)), inference(flattening, [], [f105])).
fof(f105, plain, ? [X0] : ((! [X6] : ((~ isPrime0(X6) & (? [X7] : ((~ (X6 = X7) & ~ (sz10 = X7)) & (doDivides0(X7, X6) & ? [X8] : ((sdtasdt0(X7, X8) = X6) & aNaturalNumber0(X8)) & aNaturalNumber0(X7))) | (sz10 = X6) | (sz00 = X6))) | (~ doDivides0(X6, X0) & ! [X9] : (~ (sdtasdt0(X6, X9) = X0) | ~ aNaturalNumber0(X9))) | ~ aNaturalNumber0(X6)) & ! [X1] : ((? [X2] : (isPrime0(X2) & ! [X3] : (((X2 = X3) | (sz10 = X3)) | ((~ doDivides0(X3, X2) & ! [X4] : (~ (sdtasdt0(X3, X4) = X2) | ~ aNaturalNumber0(X4))) | ~ aNaturalNumber0(X3))) & ~ (sz10 = X2) & ~ (sz00 = X2) & doDivides0(X2, X1) & ? [X5] : ((sdtasdt0(X2, X5) = X1) & aNaturalNumber0(X5)) & aNaturalNumber0(X2)) | ~ iLess0(X1, X0)) | ((sz10 = X1) | (sz00 = X1) | ~ aNaturalNumber0(X1)))) & (~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0))), inference(ennf_transformation, [], [f42])).
fof(f42, plain, ~ ! [X0] : ((~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)) => (! [X1] : ((~ (sz10 = X1) & ~ (sz00 = X1) & aNaturalNumber0(X1)) => (iLess0(X1, X0) => ? [X2] : (isPrime0(X2) & ! [X3] : (((doDivides0(X3, X2) | ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4))) & aNaturalNumber0(X3)) => ((X2 = X3) | (sz10 = X3))) & ~ (sz10 = X2) & ~ (sz00 = X2) & doDivides0(X2, X1) & ? [X5] : ((sdtasdt0(X2, X5) = X1) & aNaturalNumber0(X5)) & aNaturalNumber0(X2)))) => ? [X6] : ((isPrime0(X6) | (! [X7] : ((doDivides0(X7, X6) & ? [X8] : ((sdtasdt0(X7, X8) = X6) & aNaturalNumber0(X8)) & aNaturalNumber0(X7)) => ((X6 = X7) | (sz10 = X7))) & ~ (sz10 = X6) & ~ (sz00 = X6))) & (doDivides0(X6, X0) | ? [X9] : ((sdtasdt0(X6, X9) = X0) & aNaturalNumber0(X9))) & aNaturalNumber0(X6)))), inference(rectify, [], [f39])).
fof(f39, plain, ~ ! [X0] : ((~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)) => (! [X1] : ((~ (sz10 = X1) & ~ (sz00 = X1) & aNaturalNumber0(X1)) => (iLess0(X1, X0) => ? [X2] : (isPrime0(X2) & ! [X3] : (((doDivides0(X3, X2) | ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4))) & aNaturalNumber0(X3)) => ((X2 = X3) | (sz10 = X3))) & ~ (sz10 = X2) & ~ (sz00 = X2) & doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3)) & aNaturalNumber0(X2)))) => ? [X1] : ((isPrime0(X1) | (! [X2] : ((doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3)) & aNaturalNumber0(X2)) => ((X1 = X2) | (sz10 = X2))) & ~ (sz10 = X1) & ~ (sz00 = X1))) & (doDivides0(X1, X0) | ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2))) & aNaturalNumber0(X1)))), inference(negated_conjecture, [], [f38])).
fof(f38, plain, ~ ! [X0] : ((~ (sz10 = X0) & ~ (sz00 = X0) & aNaturalNumber0(X0)) => (! [X1] : ((~ (sz10 = X1) & ~ (sz00 = X1) & aNaturalNumber0(X1)) => (iLess0(X1, X0) => ? [X2] : (isPrime0(X2) & ! [X3] : (((doDivides0(X3, X2) | ? [X4] : ((sdtasdt0(X3, X4) = X2) & aNaturalNumber0(X4))) & aNaturalNumber0(X3)) => ((X2 = X3) | (sz10 = X3))) & ~ (sz10 = X2) & ~ (sz00 = X2) & doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3)) & aNaturalNumber0(X2)))) => ? [X1] : ((isPrime0(X1) | (! [X2] : ((doDivides0(X2, X1) & ? [X3] : ((sdtasdt0(X2, X3) = X1) & aNaturalNumber0(X3)) & aNaturalNumber0(X2)) => ((X1 = X2) | (sz10 = X2))) & ~ (sz10 = X1) & ~ (sz00 = X1))) & (doDivides0(X1, X0) | ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2))) & aNaturalNumber0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', m__)).
fof(f18872, plain, (iLess0(sK4(sK8), sK8) | ~ spl10_295), inference(avatar_component_clause, [], [f18871])).
fof(f18871, plain, (spl10_295 <=> iLess0(sK4(sK8), sK8)), introduced(avatar_definition, [new_symbols(naming, [spl10_295])])).
fof(f46289, plain, (~ isPrime0(sK9(sK4(sK8))) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297)), inference(resolution, [], [f46157, f211])).
fof(f211, plain, ! [X0] : (~ sP1(X0) | ~ isPrime0(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : ((~ isPrime0(X0) & ((~ (sK5(X0) = X0) & ~ (sz10 = sK5(X0)) & doDivides0(sK5(X0), X0) & ((sdtasdt0(sK5(X0), sK6(X0)) = X0) & aNaturalNumber0(sK6(X0))) & aNaturalNumber0(sK5(X0))) | (sz10 = X0) | (sz00 = X0))) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5, sK6])], [f128, f130, f129])).
fof(f129, plain, ! [X0] : (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) => (~ (sK5(X0) = X0) & ~ (sz10 = sK5(X0)) & doDivides0(sK5(X0), X0) & ? [X2] : ((sdtasdt0(sK5(X0), X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(sK5(X0)))), introduced(choice_axiom, [])).
fof(f130, plain, ! [X0] : (? [X2] : ((sdtasdt0(sK5(X0), X2) = X0) & aNaturalNumber0(X2)) => ((sdtasdt0(sK5(X0), sK6(X0)) = X0) & aNaturalNumber0(sK6(X0)))), introduced(choice_axiom, [])).
fof(f128, plain, ! [X0] : ((~ isPrime0(X0) & (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & ? [X2] : ((sdtasdt0(X1, X2) = X0) & aNaturalNumber0(X2)) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) | ~ sP1(X0)), inference(rectify, [], [f127])).
fof(f127, plain, ! [X6] : ((~ isPrime0(X6) & (? [X7] : (~ (X6 = X7) & ~ (sz10 = X7) & doDivides0(X7, X6) & ? [X8] : ((sdtasdt0(X7, X8) = X6) & aNaturalNumber0(X8)) & aNaturalNumber0(X7)) | (sz10 = X6) | (sz00 = X6))) | ~ sP1(X6)), inference(nnf_transformation, [], [f108])).
fof(f46157, plain, (sP1(sK9(sK4(sK8))) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297)), inference(subsumption_resolution, [], [f46118, f26275])).
fof(f26275, plain, (aNaturalNumber0(sK9(sK4(sK8))) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295)), inference(subsumption_resolution, [], [f26274, f632])).
fof(f26274, plain, (aNaturalNumber0(sK9(sK4(sK8))) | ~ aNaturalNumber0(sK4(sK8)) | (spl10_16 | spl10_17 | ~ spl10_295)), inference(subsumption_resolution, [], [f26273, f921])).
fof(f26273, plain, (aNaturalNumber0(sK9(sK4(sK8))) | (sz00 = sK4(sK8)) | ~ aNaturalNumber0(sK4(sK8)) | (spl10_17 | ~ spl10_295)), inference(subsumption_resolution, [], [f26266, f925])).
fof(f26266, plain, (aNaturalNumber0(sK9(sK4(sK8))) | (sz10 = sK4(sK8)) | (sz00 = sK4(sK8)) | ~ aNaturalNumber0(sK4(sK8)) | ~ spl10_295), inference(resolution, [], [f18872, f217])).
fof(f217, plain, ! [X3] : (~ iLess0(X3, sK8) | aNaturalNumber0(sK9(X3)) | (sz10 = X3) | (sz00 = X3) | ~ aNaturalNumber0(X3)), inference(cnf_transformation, [], [f139])).
fof(f46118, plain, (sP1(sK9(sK4(sK8))) | ~ aNaturalNumber0(sK9(sK4(sK8))) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297)), inference(resolution, [], [f46006, f226])).
fof(f226, plain, ! [X1] : (~ doDivides0(X1, sK8) | sP1(X1) | ~ aNaturalNumber0(X1)), inference(cnf_transformation, [], [f139])).
fof(f46006, plain, (doDivides0(sK9(sK4(sK8)), sK8) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297)), inference(subsumption_resolution, [], [f46005, f215])).
fof(f215, plain, ~ (sz00 = sK8), inference(cnf_transformation, [], [f139])).
fof(f46005, plain, (doDivides0(sK9(sK4(sK8)), sK8) | (sz00 = sK8) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297)), inference(subsumption_resolution, [], [f46004, f216])).
fof(f216, plain, ~ (sz10 = sK8), inference(cnf_transformation, [], [f139])).
fof(f46004, plain, (doDivides0(sK9(sK4(sK8)), sK8) | (sz10 = sK8) | (sz00 = sK8) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297)), inference(subsumption_resolution, [], [f46003, f320])).
fof(f320, plain, ~ isPrime0(sK8), inference(resolution, [], [f317, f211])).
fof(f317, plain, sP1(sK8), inference(subsumption_resolution, [], [f316, f214])).
fof(f214, plain, aNaturalNumber0(sK8), inference(cnf_transformation, [], [f139])).
fof(f316, plain, (sP1(sK8) | ~ aNaturalNumber0(sK8)), inference(resolution, [], [f309, f226])).
fof(f309, plain, doDivides0(sK8, sK8), inference(subsumption_resolution, [], [f308, f214])).
fof(f308, plain, (doDivides0(sK8, sK8) | ~ aNaturalNumber0(sK8)), inference(subsumption_resolution, [], [f304, f141])).
fof(f141, plain, aNaturalNumber0(sz10), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (~ (sz00 = sz10) & aNaturalNumber0(sz10)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mSortsC_01)).
fof(f304, plain, (doDivides0(sK8, sK8) | ~ aNaturalNumber0(sz10) | ~ aNaturalNumber0(sK8)), inference(superposition, [], [f243, f254])).
fof(f254, plain, (sK8 = sdtasdt0(sK8, sz10)), inference(resolution, [], [f151, f214])).
fof(f151, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sdtasdt0(X0, sz10) = X0)), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ! [X0] : (((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0)) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : (aNaturalNumber0(X0) => ((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', m_MulUnit)).
fof(f243, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X0)), inference(subsumption_resolution, [], [f233, f144])).
fof(f144, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f45])).
fof(f45, plain, ! [X0, X1] : (aNaturalNumber0(sdtasdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => aNaturalNumber0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mSortsB_02)).
fof(f233, plain, ! [X2, X0] : (doDivides0(X0, sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(sdtasdt0(X0, X2)) | ~ aNaturalNumber0(X0)), inference(equality_resolution, [], [f189])).
fof(f189, plain, ! [X2, X0, X1] : (doDivides0(X0, X1) | ~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (((sdtasdt0(X0, sK3(X0, X1)) = X1) & aNaturalNumber0(sK3(X0, X1))) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK3])], [f117, f118])).
fof(f118, plain, ! [X1, X0] : (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) => ((sdtasdt0(X0, sK3(X0, X1)) = X1) & aNaturalNumber0(sK3(X0, X1)))), introduced(choice_axiom, [])).
fof(f117, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X3] : ((sdtasdt0(X0, X3) = X1) & aNaturalNumber0(X3)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f116])).
fof(f116, plain, ! [X0, X1] : (((doDivides0(X0, X1) | ! [X2] : (~ (sdtasdt0(X0, X2) = X1) | ~ aNaturalNumber0(X2))) & (? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)) | ~ doDivides0(X0, X1))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f90])).
fof(f90, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f89])).
fof(f89, plain, ! [X0, X1] : ((doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => (doDivides0(X0, X1) <=> ? [X2] : ((sdtasdt0(X0, X2) = X1) & aNaturalNumber0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mDefDiv)).
fof(f46003, plain, (doDivides0(sK9(sK4(sK8)), sK8) | isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | ~ spl10_297)), inference(subsumption_resolution, [], [f46002, f26275])).
fof(f46002, plain, (doDivides0(sK9(sK4(sK8)), sK8) | ~ aNaturalNumber0(sK9(sK4(sK8))) | isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ spl10_297), inference(subsumption_resolution, [], [f45963, f214])).
fof(f45963, plain, (doDivides0(sK9(sK4(sK8)), sK8) | ~ aNaturalNumber0(sK8) | ~ aNaturalNumber0(sK9(sK4(sK8))) | isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ spl10_297), inference(resolution, [], [f18880, f861])).
fof(f861, plain, ! [X8, X7] : (~ doDivides0(X7, sK4(X8)) | doDivides0(X7, X8) | ~ aNaturalNumber0(X8) | ~ aNaturalNumber0(X7) | isPrime0(X8) | (sz10 = X8) | (sz00 = X8)), inference(subsumption_resolution, [], [f852, f201])).
fof(f201, plain, ! [X0] : (aNaturalNumber0(sK4(X0)) | isPrime0(X0) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f126])).
fof(f126, plain, ! [X0] : (((isPrime0(X0) | (~ (sK4(X0) = X0) & ~ (sz10 = sK4(X0)) & doDivides0(sK4(X0), X0) & aNaturalNumber0(sK4(X0))) | (sz10 = X0) | (sz00 = X0)) & ((! [X2] : ((X0 = X2) | (sz10 = X2) | ~ doDivides0(X2, X0) | ~ aNaturalNumber0(X2)) & ~ (sz10 = X0) & ~ (sz00 = X0)) | ~ isPrime0(X0))) | ~ aNaturalNumber0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f124, f125])).
fof(f125, plain, ! [X0] : (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & aNaturalNumber0(X1)) => (~ (sK4(X0) = X0) & ~ (sz10 = sK4(X0)) & doDivides0(sK4(X0), X0) & aNaturalNumber0(sK4(X0)))), introduced(choice_axiom, [])).
fof(f124, plain, ! [X0] : (((isPrime0(X0) | ? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0)) & ((! [X2] : ((X0 = X2) | (sz10 = X2) | ~ doDivides0(X2, X0) | ~ aNaturalNumber0(X2)) & ~ (sz10 = X0) & ~ (sz00 = X0)) | ~ isPrime0(X0))) | ~ aNaturalNumber0(X0)), inference(rectify, [], [f123])).
fof(f123, plain, ! [X0] : (((isPrime0(X0) | ? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0)) & ((! [X1] : ((X0 = X1) | (sz10 = X1) | ~ doDivides0(X1, X0) | ~ aNaturalNumber0(X1)) & ~ (sz10 = X0) & ~ (sz00 = X0)) | ~ isPrime0(X0))) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f122])).
fof(f122, plain, ! [X0] : (((isPrime0(X0) | (? [X1] : (~ (X0 = X1) & ~ (sz10 = X1) & doDivides0(X1, X0) & aNaturalNumber0(X1)) | (sz10 = X0) | (sz00 = X0))) & ((! [X1] : ((X0 = X1) | (sz10 = X1) | ~ doDivides0(X1, X0) | ~ aNaturalNumber0(X1)) & ~ (sz10 = X0) & ~ (sz00 = X0)) | ~ isPrime0(X0))) | ~ aNaturalNumber0(X0)), inference(nnf_transformation, [], [f104])).
fof(f104, plain, ! [X0] : ((isPrime0(X0) <=> (! [X1] : ((X0 = X1) | (sz10 = X1) | ~ doDivides0(X1, X0) | ~ aNaturalNumber0(X1)) & ~ (sz10 = X0) & ~ (sz00 = X0))) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f103])).
fof(f103, plain, ! [X0] : ((isPrime0(X0) <=> (! [X1] : (((X0 = X1) | (sz10 = X1)) | (~ doDivides0(X1, X0) | ~ aNaturalNumber0(X1))) & ~ (sz10 = X0) & ~ (sz00 = X0))) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f37])).
fof(f37, plain, ! [X0] : (aNaturalNumber0(X0) => (isPrime0(X0) <=> (! [X1] : ((doDivides0(X1, X0) & aNaturalNumber0(X1)) => ((X0 = X1) | (sz10 = X1))) & ~ (sz10 = X0) & ~ (sz00 = X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mDefPrime)).
fof(f852, plain, ! [X8, X7] : (doDivides0(X7, X8) | ~ doDivides0(X7, sK4(X8)) | ~ aNaturalNumber0(X8) | ~ aNaturalNumber0(sK4(X8)) | ~ aNaturalNumber0(X7) | isPrime0(X8) | (sz10 = X8) | (sz00 = X8)), inference(duplicate_literal_removal, [], [f843])).
fof(f843, plain, ! [X8, X7] : (doDivides0(X7, X8) | ~ doDivides0(X7, sK4(X8)) | ~ aNaturalNumber0(X8) | ~ aNaturalNumber0(sK4(X8)) | ~ aNaturalNumber0(X7) | isPrime0(X8) | (sz10 = X8) | (sz00 = X8) | ~ aNaturalNumber0(X8)), inference(resolution, [], [f193, f202])).
fof(f202, plain, ! [X0] : (doDivides0(sK4(X0), X0) | isPrime0(X0) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f126])).
fof(f193, plain, ! [X2, X0, X1] : (~ doDivides0(X1, X2) | doDivides0(X0, X2) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ! [X0, X1, X2] : (doDivides0(X0, X2) | ~ doDivides0(X1, X2) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f93])).
fof(f93, plain, ! [X0, X1, X2] : ((doDivides0(X0, X2) | (~ doDivides0(X1, X2) | ~ doDivides0(X0, X1))) | (~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f32])).
fof(f32, plain, ! [X0, X1, X2] : ((aNaturalNumber0(X2) & aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((doDivides0(X1, X2) & doDivides0(X0, X1)) => doDivides0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mDivTrans)).
fof(f18880, plain, (doDivides0(sK9(sK4(sK8)), sK4(sK8)) | ~ spl10_297), inference(avatar_component_clause, [], [f18879])).
fof(f18879, plain, (spl10_297 <=> doDivides0(sK9(sK4(sK8)), sK4(sK8))), introduced(avatar_definition, [new_symbols(naming, [spl10_297])])).
fof(f45729, plain, (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | spl10_297), inference(avatar_contradiction_clause, [], [f45728])).
fof(f45728, plain, ($false | (~ spl10_3 | spl10_16 | spl10_17 | ~ spl10_295 | spl10_297)), inference(subsumption_resolution, [], [f45727, f632])).
fof(f45727, plain, (~ aNaturalNumber0(sK4(sK8)) | (spl10_16 | spl10_17 | ~ spl10_295 | spl10_297)), inference(subsumption_resolution, [], [f45726, f921])).
fof(f45726, plain, ((sz00 = sK4(sK8)) | ~ aNaturalNumber0(sK4(sK8)) | (spl10_17 | ~ spl10_295 | spl10_297)), inference(subsumption_resolution, [], [f45725, f925])).
fof(f45725, plain, ((sz10 = sK4(sK8)) | (sz00 = sK4(sK8)) | ~ aNaturalNumber0(sK4(sK8)) | (~ spl10_295 | spl10_297)), inference(subsumption_resolution, [], [f45724, f18872])).
fof(f45724, plain, (~ iLess0(sK4(sK8), sK8) | (sz10 = sK4(sK8)) | (sz00 = sK4(sK8)) | ~ aNaturalNumber0(sK4(sK8)) | spl10_297), inference(resolution, [], [f18881, f219])).
fof(f219, plain, ! [X3] : (doDivides0(sK9(X3), X3) | ~ iLess0(X3, sK8) | (sz10 = X3) | (sz00 = X3) | ~ aNaturalNumber0(X3)), inference(cnf_transformation, [], [f139])).
fof(f18881, plain, (~ doDivides0(sK9(sK4(sK8)), sK4(sK8)) | spl10_297), inference(avatar_component_clause, [], [f18879])).
fof(f26263, plain, spl10_347, inference(avatar_contradiction_clause, [], [f26262])).
fof(f26262, plain, ($false | spl10_347), inference(subsumption_resolution, [], [f26261, f320])).
fof(f26261, plain, (isPrime0(sK8) | spl10_347), inference(subsumption_resolution, [], [f26260, f214])).
fof(f26260, plain, (~ aNaturalNumber0(sK8) | isPrime0(sK8) | spl10_347), inference(subsumption_resolution, [], [f26259, f215])).
fof(f26259, plain, ((sz00 = sK8) | ~ aNaturalNumber0(sK8) | isPrime0(sK8) | spl10_347), inference(subsumption_resolution, [], [f26254, f216])).
fof(f26254, plain, ((sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | isPrime0(sK8) | spl10_347), inference(resolution, [], [f19934, f640])).
fof(f640, plain, ! [X1] : (sdtlseqdt0(sK4(X1), X1) | (sz10 = X1) | (sz00 = X1) | ~ aNaturalNumber0(X1) | isPrime0(X1)), inference(subsumption_resolution, [], [f624, f201])).
fof(f624, plain, ! [X1] : (isPrime0(X1) | (sz10 = X1) | (sz00 = X1) | ~ aNaturalNumber0(X1) | sdtlseqdt0(sK4(X1), X1) | ~ aNaturalNumber0(sK4(X1))), inference(duplicate_literal_removal, [], [f623])).
fof(f623, plain, ! [X1] : (isPrime0(X1) | (sz10 = X1) | (sz00 = X1) | ~ aNaturalNumber0(X1) | (sz00 = X1) | sdtlseqdt0(sK4(X1), X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(sK4(X1))), inference(resolution, [], [f202, f196])).
fof(f196, plain, ! [X0, X1] : (~ doDivides0(X0, X1) | (sz00 = X1) | sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f100])).
fof(f100, plain, ! [X0, X1] : (sdtlseqdt0(X0, X1) | (sz00 = X1) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f99])).
fof(f99, plain, ! [X0, X1] : ((sdtlseqdt0(X0, X1) | ((sz00 = X1) | ~ doDivides0(X0, X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f35])).
fof(f35, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((~ (sz00 = X1) & doDivides0(X0, X1)) => sdtlseqdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mDivLE)).
fof(f19934, plain, (~ sdtlseqdt0(sK4(sK8), sK8) | spl10_347), inference(avatar_component_clause, [], [f19932])).
fof(f19932, plain, (spl10_347 <=> sdtlseqdt0(sK4(sK8), sK8)), introduced(avatar_definition, [new_symbols(naming, [spl10_347])])).
fof(f21397, plain, ~ spl10_307, inference(avatar_contradiction_clause, [], [f21396])).
fof(f21396, plain, ($false | ~ spl10_307), inference(subsumption_resolution, [], [f21395, f214])).
fof(f21395, plain, (~ aNaturalNumber0(sK8) | ~ spl10_307), inference(subsumption_resolution, [], [f21394, f215])).
fof(f21394, plain, ((sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_307), inference(subsumption_resolution, [], [f21393, f216])).
fof(f21393, plain, ((sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_307), inference(subsumption_resolution, [], [f21392, f320])).
fof(f21392, plain, (isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_307), inference(trivial_inequality_removal, [], [f21388])).
fof(f21388, plain, (~ (sK8 = sK8) | isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_307), inference(superposition, [], [f204, f19052])).
fof(f19052, plain, ((sK8 = sK4(sK8)) | ~ spl10_307), inference(avatar_component_clause, [], [f19050])).
fof(f19050, plain, (spl10_307 <=> (sK8 = sK4(sK8))), introduced(avatar_definition, [new_symbols(naming, [spl10_307])])).
fof(f204, plain, ! [X0] : (~ (sK4(X0) = X0) | isPrime0(X0) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f126])).
fof(f19935, plain, (~ spl10_347 | spl10_307 | ~ spl10_3 | ~ spl10_306), inference(avatar_split_clause, [], [f19930, f19046, f631, f19050, f19932])).
fof(f19046, plain, (spl10_306 <=> sdtlseqdt0(sK8, sK4(sK8))), introduced(avatar_definition, [new_symbols(naming, [spl10_306])])).
fof(f19930, plain, ((sK8 = sK4(sK8)) | ~ sdtlseqdt0(sK4(sK8), sK8) | (~ spl10_3 | ~ spl10_306)), inference(subsumption_resolution, [], [f19929, f632])).
fof(f19929, plain, ((sK8 = sK4(sK8)) | ~ sdtlseqdt0(sK4(sK8), sK8) | ~ aNaturalNumber0(sK4(sK8)) | ~ spl10_306), inference(subsumption_resolution, [], [f19886, f214])).
fof(f19886, plain, ((sK8 = sK4(sK8)) | ~ sdtlseqdt0(sK4(sK8), sK8) | ~ aNaturalNumber0(sK8) | ~ aNaturalNumber0(sK4(sK8)) | ~ spl10_306), inference(resolution, [], [f19048, f171])).
fof(f171, plain, ! [X0, X1] : (~ sdtlseqdt0(X1, X0) | (X0 = X1) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ! [X0, X1] : ((X0 = X1) | ~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f73])).
fof(f73, plain, ! [X0, X1] : (((X0 = X1) | (~ sdtlseqdt0(X1, X0) | ~ sdtlseqdt0(X0, X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X1, X0) & sdtlseqdt0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mLEAsym)).
fof(f19048, plain, (sdtlseqdt0(sK8, sK4(sK8)) | ~ spl10_306), inference(avatar_component_clause, [], [f19046])).
fof(f19053, plain, (spl10_306 | spl10_307 | ~ spl10_3 | spl10_295), inference(avatar_split_clause, [], [f19044, f18871, f631, f19050, f19046])).
fof(f19044, plain, ((sK8 = sK4(sK8)) | sdtlseqdt0(sK8, sK4(sK8)) | (~ spl10_3 | spl10_295)), inference(subsumption_resolution, [], [f19043, f632])).
fof(f19043, plain, ((sK8 = sK4(sK8)) | ~ aNaturalNumber0(sK4(sK8)) | sdtlseqdt0(sK8, sK4(sK8)) | spl10_295), inference(subsumption_resolution, [], [f19042, f214])).
fof(f19042, plain, ((sK8 = sK4(sK8)) | ~ aNaturalNumber0(sK8) | ~ aNaturalNumber0(sK4(sK8)) | sdtlseqdt0(sK8, sK4(sK8)) | spl10_295), inference(resolution, [], [f18873, f518])).
fof(f518, plain, ! [X2, X1] : (iLess0(X1, X2) | (X1 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | sdtlseqdt0(X2, X1)), inference(duplicate_literal_removal, [], [f506])).
fof(f506, plain, ! [X2, X1] : (iLess0(X1, X2) | (X1 = X2) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1) | sdtlseqdt0(X2, X1) | ~ aNaturalNumber0(X2) | ~ aNaturalNumber0(X1)), inference(resolution, [], [f186, f174])).
fof(f174, plain, ! [X0, X1] : (sdtlseqdt0(X1, X0) | sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ! [X0, X1] : ((sdtlseqdt0(X1, X0) & ~ (X0 = X1)) | sdtlseqdt0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f77])).
fof(f77, plain, ! [X0, X1] : (((sdtlseqdt0(X1, X0) & ~ (X0 = X1)) | sdtlseqdt0(X0, X1)) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X1, X0) & ~ (X0 = X1)) | sdtlseqdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mLETotal)).
fof(f186, plain, ! [X0, X1] : (~ sdtlseqdt0(X0, X1) | iLess0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f88])).
fof(f88, plain, ! [X0, X1] : (iLess0(X0, X1) | ~ sdtlseqdt0(X0, X1) | (X0 = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(flattening, [], [f87])).
fof(f87, plain, ! [X0, X1] : ((iLess0(X0, X1) | (~ sdtlseqdt0(X0, X1) | (X0 = X1))) | (~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f29])).
fof(f29, plain, ! [X0, X1] : ((aNaturalNumber0(X1) & aNaturalNumber0(X0)) => ((sdtlseqdt0(X0, X1) & ~ (X0 = X1)) => iLess0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mIH_03)).
fof(f18873, plain, (~ iLess0(sK4(sK8), sK8) | spl10_295), inference(avatar_component_clause, [], [f18871])).
fof(f3064, plain, ~ spl10_17, inference(avatar_contradiction_clause, [], [f3063])).
fof(f3063, plain, ($false | ~ spl10_17), inference(subsumption_resolution, [], [f3062, f214])).
fof(f3062, plain, (~ aNaturalNumber0(sK8) | ~ spl10_17), inference(subsumption_resolution, [], [f3061, f215])).
fof(f3061, plain, ((sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_17), inference(subsumption_resolution, [], [f3060, f216])).
fof(f3060, plain, ((sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_17), inference(subsumption_resolution, [], [f3059, f320])).
fof(f3059, plain, (isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_17), inference(trivial_inequality_removal, [], [f3056])).
fof(f3056, plain, (~ (sz10 = sz10) | isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_17), inference(superposition, [], [f203, f926])).
fof(f926, plain, ((sz10 = sK4(sK8)) | ~ spl10_17), inference(avatar_component_clause, [], [f924])).
fof(f203, plain, ! [X0] : (~ (sz10 = sK4(X0)) | isPrime0(X0) | (sz10 = X0) | (sz00 = X0) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f126])).
fof(f2783, plain, ~ spl10_16, inference(avatar_contradiction_clause, [], [f2782])).
fof(f2782, plain, ($false | ~ spl10_16), inference(subsumption_resolution, [], [f2781, f215])).
fof(f2781, plain, ((sz00 = sK8) | ~ spl10_16), inference(backward_demodulation, [], [f1275, f2780])).
fof(f2780, plain, ((sz00 = sdtasdt0(sz00, sK3(sz00, sK8))) | ~ spl10_16), inference(subsumption_resolution, [], [f2779, f140])).
fof(f140, plain, aNaturalNumber0(sz00), inference(cnf_transformation, [], [f2])).
fof(f2, plain, aNaturalNumber0(sz00), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', mSortsC)).
fof(f2779, plain, (~ aNaturalNumber0(sz00) | (sz00 = sdtasdt0(sz00, sK3(sz00, sK8))) | ~ spl10_16), inference(subsumption_resolution, [], [f2759, f214])).
fof(f2759, plain, (~ aNaturalNumber0(sK8) | ~ aNaturalNumber0(sz00) | (sz00 = sdtasdt0(sz00, sK3(sz00, sK8))) | ~ spl10_16), inference(resolution, [], [f358, f1252])).
fof(f1252, plain, (doDivides0(sz00, sK8) | ~ spl10_16), inference(subsumption_resolution, [], [f1251, f214])).
fof(f1251, plain, (doDivides0(sz00, sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_16), inference(subsumption_resolution, [], [f1250, f215])).
fof(f1250, plain, (doDivides0(sz00, sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_16), inference(subsumption_resolution, [], [f1249, f216])).
fof(f1249, plain, (doDivides0(sz00, sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_16), inference(subsumption_resolution, [], [f1247, f320])).
fof(f1247, plain, (doDivides0(sz00, sK8) | isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | ~ spl10_16), inference(superposition, [], [f202, f922])).
fof(f922, plain, ((sz00 = sK4(sK8)) | ~ spl10_16), inference(avatar_component_clause, [], [f920])).
fof(f358, plain, ! [X17, X16] : (~ doDivides0(X16, X17) | ~ aNaturalNumber0(X17) | ~ aNaturalNumber0(X16) | (sz00 = sdtasdt0(sz00, sK3(X16, X17)))), inference(resolution, [], [f187, f154])).
fof(f154, plain, ! [X0] : (~ aNaturalNumber0(X0) | (sz00 = sdtasdt0(sz00, X0))), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ! [X0] : (((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00))) | ~ aNaturalNumber0(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (aNaturalNumber0(X0) => ((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM481+3.p', m_MulZero)).
fof(f187, plain, ! [X0, X1] : (aNaturalNumber0(sK3(X0, X1)) | ~ doDivides0(X0, X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f119])).
fof(f1275, plain, ((sK8 = sdtasdt0(sz00, sK3(sz00, sK8))) | ~ spl10_16), inference(subsumption_resolution, [], [f1274, f140])).
fof(f1274, plain, ((sK8 = sdtasdt0(sz00, sK3(sz00, sK8))) | ~ aNaturalNumber0(sz00) | ~ spl10_16), inference(subsumption_resolution, [], [f1270, f214])).
fof(f1270, plain, ((sK8 = sdtasdt0(sz00, sK3(sz00, sK8))) | ~ aNaturalNumber0(sK8) | ~ aNaturalNumber0(sz00) | ~ spl10_16), inference(resolution, [], [f1252, f188])).
fof(f188, plain, ! [X0, X1] : (~ doDivides0(X0, X1) | (sdtasdt0(X0, sK3(X0, X1)) = X1) | ~ aNaturalNumber0(X1) | ~ aNaturalNumber0(X0)), inference(cnf_transformation, [], [f119])).
fof(f646, plain, spl10_3, inference(avatar_contradiction_clause, [], [f645])).
fof(f645, plain, ($false | spl10_3), inference(subsumption_resolution, [], [f644, f214])).
fof(f644, plain, (~ aNaturalNumber0(sK8) | spl10_3), inference(subsumption_resolution, [], [f643, f215])).
fof(f643, plain, ((sz00 = sK8) | ~ aNaturalNumber0(sK8) | spl10_3), inference(subsumption_resolution, [], [f642, f216])).
fof(f642, plain, ((sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | spl10_3), inference(subsumption_resolution, [], [f641, f320])).
fof(f641, plain, (isPrime0(sK8) | (sz10 = sK8) | (sz00 = sK8) | ~ aNaturalNumber0(sK8) | spl10_3), inference(resolution, [], [f633, f201])).
fof(f633, plain, (~ aNaturalNumber0(sK4(sK8)) | spl10_3), inference(avatar_component_clause, [], [f631])).