fof(f67869, plain, $false, inference(avatar_sat_refutation, [], [f374, f382, f399, f404, f409, f414, f419, f424, f429, f433, f443, f448, f451, f458, f462, f1267, f1393, f1612, f1837, f2071, f19033, f19127, f19247, f19248, f48393, f49478, f50836, f67851])).
fof(f67851, plain, (~ spl29_1 | ~ spl29_15 | ~ spl29_17 | ~ spl29_18 | ~ spl29_57), inference(avatar_contradiction_clause, [], [f67850])).
fof(f67850, plain, ($false | (~ spl29_1 | ~ spl29_15 | ~ spl29_17 | ~ spl29_18 | ~ spl29_57)), inference(subsumption_resolution, [], [f67831, f52184])).
fof(f52184, plain, (isPrime0(sK25(sK28)) | ~ spl29_18), inference(resolution, [], [f447, f328])).
fof(f328, plain, ! [X0] : (~ aElementOf0(X0, xS) | isPrime0(sK25(X0))), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ((xS = cS2043) & ! [X0] : ((aElementOf0(X0, xS) | ! [X1] : ((~ (szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0) & sP7(X1) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X1))) | ~ isPrime0(X1) | (sz00 = X1) | ~ aInteger0(X1))) & (((szAzrzSzezqlpdtcmdtrp0(sz00, sK25(X0)) = X0) & sP6(sK25(X0)) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, sK25(X0))) & isPrime0(sK25(X0)) & ~ (sz00 = sK25(X0)) & aInteger0(sK25(X0))) | ~ aElementOf0(X0, xS))) & aSet0(xS)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK25])], [f192, f193])).
fof(f193, plain, ! [X0] : (? [X2] : ((szAzrzSzezqlpdtcmdtrp0(sz00, X2) = X0) & sP6(X2) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X2)) & isPrime0(X2) & ~ (sz00 = X2) & aInteger0(X2)) => ((szAzrzSzezqlpdtcmdtrp0(sz00, sK25(X0)) = X0) & sP6(sK25(X0)) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, sK25(X0))) & isPrime0(sK25(X0)) & ~ (sz00 = sK25(X0)) & aInteger0(sK25(X0)))), introduced(choice_axiom, [])).
fof(f192, plain, ((xS = cS2043) & ! [X0] : ((aElementOf0(X0, xS) | ! [X1] : ((~ (szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0) & sP7(X1) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X1))) | ~ isPrime0(X1) | (sz00 = X1) | ~ aInteger0(X1))) & (? [X2] : ((szAzrzSzezqlpdtcmdtrp0(sz00, X2) = X0) & sP6(X2) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X2)) & isPrime0(X2) & ~ (sz00 = X2) & aInteger0(X2)) | ~ aElementOf0(X0, xS))) & aSet0(xS)), inference(rectify, [], [f124])).
fof(f124, plain, ((xS = cS2043) & ! [X0] : ((aElementOf0(X0, xS) | ! [X1] : ((~ (szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0) & sP7(X1) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X1))) | ~ isPrime0(X1) | (sz00 = X1) | ~ aInteger0(X1))) & (? [X5] : ((szAzrzSzezqlpdtcmdtrp0(sz00, X5) = X0) & sP6(X5) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X5)) & isPrime0(X5) & ~ (sz00 = X5) & aInteger0(X5)) | ~ aElementOf0(X0, xS))) & aSet0(xS)), inference(definition_folding, [], [f110, e123, e122])).
fof(f122, plain, ! [X5] : (! [X6] : ((aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)) | (~ sdteqdtlpzmzozddtrp0(X6, sz00, X5) & ~ aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ! [X7] : (~ (sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X7)) | ~ aInteger0(X7))) | ~ aInteger0(X6)) & ((sdteqdtlpzmzozddtrp0(X6, sz00, X5) & aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ? [X8] : ((sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X8)) & aInteger0(X8)) & aInteger0(X6)) | ~ aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)))) | ~ sP6(X5)), inference(usedef, [], [e122])).
fof(e122, plain, ! [X5] : (sP6(X5) <=> ! [X6] : ((aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)) | (~ sdteqdtlpzmzozddtrp0(X6, sz00, X5) & ~ aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ! [X7] : (~ (sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X7)) | ~ aInteger0(X7))) | ~ aInteger0(X6)) & ((sdteqdtlpzmzozddtrp0(X6, sz00, X5) & aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ? [X8] : ((sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X8)) & aInteger0(X8)) & aInteger0(X6)) | ~ aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f123, plain, ! [X1] : (! [X2] : ((aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)) | (~ sdteqdtlpzmzozddtrp0(X2, sz00, X1) & ~ aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ! [X3] : (~ (sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) | ~ aInteger0(X3))) | ~ aInteger0(X2)) & ((sdteqdtlpzmzozddtrp0(X2, sz00, X1) & aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ? [X4] : ((sdtpldt0(X2, smndt0(sz00)) = sdtasdt0(X1, X4)) & aInteger0(X4)) & aInteger0(X2)) | ~ aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)))) | ~ sP7(X1)), inference(usedef, [], [e123])).
fof(e123, plain, ! [X1] : (sP7(X1) <=> ! [X2] : ((aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)) | (~ sdteqdtlpzmzozddtrp0(X2, sz00, X1) & ~ aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ! [X3] : (~ (sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) | ~ aInteger0(X3))) | ~ aInteger0(X2)) & ((sdteqdtlpzmzozddtrp0(X2, sz00, X1) & aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ? [X4] : ((sdtpldt0(X2, smndt0(sz00)) = sdtasdt0(X1, X4)) & aInteger0(X4)) & aInteger0(X2)) | ~ aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f110, plain, ((xS = cS2043) & ! [X0] : ((aElementOf0(X0, xS) | ! [X1] : ((~ (szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0) & ! [X2] : ((aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)) | (~ sdteqdtlpzmzozddtrp0(X2, sz00, X1) & ~ aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ! [X3] : (~ (sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) | ~ aInteger0(X3))) | ~ aInteger0(X2)) & ((sdteqdtlpzmzozddtrp0(X2, sz00, X1) & aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ? [X4] : ((sdtpldt0(X2, smndt0(sz00)) = sdtasdt0(X1, X4)) & aInteger0(X4)) & aInteger0(X2)) | ~ aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)))) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X1))) | ~ isPrime0(X1) | (sz00 = X1) | ~ aInteger0(X1))) & (? [X5] : ((szAzrzSzezqlpdtcmdtrp0(sz00, X5) = X0) & ! [X6] : ((aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)) | (~ sdteqdtlpzmzozddtrp0(X6, sz00, X5) & ~ aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ! [X7] : (~ (sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X7)) | ~ aInteger0(X7))) | ~ aInteger0(X6)) & ((sdteqdtlpzmzozddtrp0(X6, sz00, X5) & aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ? [X8] : ((sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X8)) & aInteger0(X8)) & aInteger0(X6)) | ~ aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)))) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X5)) & isPrime0(X5) & ~ (sz00 = X5) & aInteger0(X5)) | ~ aElementOf0(X0, xS))) & aSet0(xS)), inference(flattening, [], [f109])).
fof(f109, plain, ((xS = cS2043) & ! [X0] : ((aElementOf0(X0, xS) | ! [X1] : ((~ (szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0) & (! [X2] : ((aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)) | ((~ sdteqdtlpzmzozddtrp0(X2, sz00, X1) & ~ aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ! [X3] : (~ (sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) | ~ aInteger0(X3))) | ~ aInteger0(X2))) & ((sdteqdtlpzmzozddtrp0(X2, sz00, X1) & aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ? [X4] : ((sdtpldt0(X2, smndt0(sz00)) = sdtasdt0(X1, X4)) & aInteger0(X4)) & aInteger0(X2)) | ~ aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)))) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X1)))) | ~ isPrime0(X1) | (sz00 = X1) | ~ aInteger0(X1))) & (? [X5] : ((szAzrzSzezqlpdtcmdtrp0(sz00, X5) = X0) & ! [X6] : ((aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)) | ((~ sdteqdtlpzmzozddtrp0(X6, sz00, X5) & ~ aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ! [X7] : (~ (sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X7)) | ~ aInteger0(X7))) | ~ aInteger0(X6))) & ((sdteqdtlpzmzozddtrp0(X6, sz00, X5) & aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ? [X8] : ((sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X8)) & aInteger0(X8)) & aInteger0(X6)) | ~ aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)))) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X5)) & isPrime0(X5) & ~ (sz00 = X5) & aInteger0(X5)) | ~ aElementOf0(X0, xS))) & aSet0(xS)), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ((xS = cS2043) & ! [X0] : ((? [X1] : (((! [X2] : ((((sdteqdtlpzmzozddtrp0(X2, sz00, X1) | aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) | ? [X3] : ((sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) & aInteger0(X3))) & aInteger0(X2)) => aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1))) & (aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)) => (sdteqdtlpzmzozddtrp0(X2, sz00, X1) & aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ? [X4] : ((sdtpldt0(X2, smndt0(sz00)) = sdtasdt0(X1, X4)) & aInteger0(X4)) & aInteger0(X2)))) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X1))) => (szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0)) & isPrime0(X1) & ~ (sz00 = X1) & aInteger0(X1)) => aElementOf0(X0, xS)) & (aElementOf0(X0, xS) => ? [X5] : ((szAzrzSzezqlpdtcmdtrp0(sz00, X5) = X0) & ! [X6] : ((((sdteqdtlpzmzozddtrp0(X6, sz00, X5) | aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) | ? [X7] : ((sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X7)) & aInteger0(X7))) & aInteger0(X6)) => aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5))) & (aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)) => (sdteqdtlpzmzozddtrp0(X6, sz00, X5) & aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ? [X8] : ((sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X8)) & aInteger0(X8)) & aInteger0(X6)))) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X5)) & isPrime0(X5) & ~ (sz00 = X5) & aInteger0(X5)))) & aSet0(xS)), inference(rectify, [], [f42])).
fof(f42, plain, ((xS = cS2043) & ! [X0] : ((? [X1] : (((! [X2] : ((((sdteqdtlpzmzozddtrp0(X2, sz00, X1) | aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) | ? [X3] : ((sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) & aInteger0(X3))) & aInteger0(X2)) => aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1))) & (aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)) => (sdteqdtlpzmzozddtrp0(X2, sz00, X1) & aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ? [X3] : ((sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) & aInteger0(X3)) & aInteger0(X2)))) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X1))) => (szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0)) & isPrime0(X1) & ~ (sz00 = X1) & aInteger0(X1)) => aElementOf0(X0, xS)) & (aElementOf0(X0, xS) => ? [X1] : ((szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0) & ! [X2] : ((((sdteqdtlpzmzozddtrp0(X2, sz00, X1) | aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) | ? [X3] : ((sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) & aInteger0(X3))) & aInteger0(X2)) => aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1))) & (aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)) => (sdteqdtlpzmzozddtrp0(X2, sz00, X1) & aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ? [X3] : ((sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) & aInteger0(X3)) & aInteger0(X2)))) & aSet0(szAzrzSzezqlpdtcmdtrp0(sz00, X1)) & isPrime0(X1) & ~ (sz00 = X1) & aInteger0(X1)))) & aSet0(xS)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', m__2046)).
fof(f447, plain, (aElementOf0(sK28, xS) | ~ spl29_18), inference(avatar_component_clause, [], [f445])).
fof(f445, plain, (spl29_18 <=> aElementOf0(sK28, xS)), introduced(avatar_definition, [new_symbols(naming, [spl29_18])])).
fof(f67831, plain, (~ isPrime0(sK25(sK28)) | (~ spl29_1 | ~ spl29_15 | ~ spl29_17 | ~ spl29_18 | ~ spl29_57)), inference(resolution, [], [f65138, f432])).
fof(f432, plain, (! [X0] : (~ aDivisorOf0(X0, xn) | ~ isPrime0(X0)) | ~ spl29_15), inference(avatar_component_clause, [], [f431])).
fof(f431, plain, (spl29_15 <=> ! [X0] : (~ isPrime0(X0) | ~ aDivisorOf0(X0, xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_15])])).
fof(f65138, plain, (aDivisorOf0(sK25(sK28), xn) | (~ spl29_1 | ~ spl29_17 | ~ spl29_18 | ~ spl29_57)), inference(subsumption_resolution, [], [f65137, f442])).
fof(f442, plain, (aElementOf0(xn, sK28) | ~ spl29_17), inference(avatar_component_clause, [], [f440])).
fof(f440, plain, (spl29_17 <=> aElementOf0(xn, sK28)), introduced(avatar_definition, [new_symbols(naming, [spl29_17])])).
fof(f65137, plain, (aDivisorOf0(sK25(sK28), xn) | ~ aElementOf0(xn, sK28) | (~ spl29_1 | ~ spl29_18 | ~ spl29_57)), inference(subsumption_resolution, [], [f64405, f52183])).
fof(f52183, plain, (sP6(sK25(sK28)) | ~ spl29_18), inference(resolution, [], [f447, f330])).
fof(f330, plain, ! [X0] : (~ aElementOf0(X0, xS) | sP6(sK25(X0))), inference(cnf_transformation, [], [f194])).
fof(f64405, plain, (~ sP6(sK25(sK28)) | aDivisorOf0(sK25(sK28), xn) | ~ aElementOf0(xn, sK28) | (~ spl29_1 | ~ spl29_57)), inference(resolution, [], [f5751, f1261])).
fof(f1261, plain, (! [X2] : (sdteqdtlpzmzozddtrp0(X2, sz00, sK25(sK28)) | ~ aElementOf0(X2, sK28)) | ~ spl29_57), inference(avatar_component_clause, [], [f1260])).
fof(f1260, plain, (spl29_57 <=> ! [X2] : (~ aElementOf0(X2, sK28) | sdteqdtlpzmzozddtrp0(X2, sz00, sK25(sK28)))), introduced(avatar_definition, [new_symbols(naming, [spl29_57])])).
fof(f5751, plain, (! [X3] : (~ sdteqdtlpzmzozddtrp0(xn, sz00, X3) | ~ sP6(X3) | aDivisorOf0(X3, xn)) | ~ spl29_1), inference(subsumption_resolution, [], [f5743, f336])).
fof(f336, plain, aInteger0(xn), inference(cnf_transformation, [], [f43])).
fof(f43, plain, aInteger0(xn), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', m__2106)).
fof(f5743, plain, (! [X3] : (aDivisorOf0(X3, xn) | ~ aInteger0(xn) | ~ sP6(X3) | ~ sdteqdtlpzmzozddtrp0(xn, sz00, X3)) | ~ spl29_1), inference(superposition, [], [f1896, f467])).
fof(f467, plain, (xn = sdtpldt0(xn, sz00)), inference(resolution, [], [f210, f336])).
fof(f210, plain, ! [X0] : (~ aInteger0(X0) | (sdtpldt0(X0, sz00) = X0)), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ! [X0] : (((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0)) | ~ aInteger0(X0)), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0] : (aInteger0(X0) => ((sdtpldt0(sz00, X0) = X0) & (sdtpldt0(X0, sz00) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mAddZero)).
fof(f1896, plain, (! [X14, X15] : (aDivisorOf0(X15, sdtpldt0(X14, sz00)) | ~ aInteger0(X14) | ~ sP6(X15) | ~ sdteqdtlpzmzozddtrp0(X14, sz00, X15)) | ~ spl29_1), inference(duplicate_literal_removal, [], [f1894])).
fof(f1894, plain, (! [X14, X15] : (~ sdteqdtlpzmzozddtrp0(X14, sz00, X15) | ~ aInteger0(X14) | ~ sP6(X15) | aDivisorOf0(X15, sdtpldt0(X14, sz00)) | ~ sP6(X15)) | ~ spl29_1), inference(resolution, [], [f324, f589])).
fof(f589, plain, (! [X0, X1] : (~ aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | aDivisorOf0(X0, sdtpldt0(X1, sz00)) | ~ sP6(X0)) | ~ spl29_1), inference(backward_demodulation, [], [f320, f583])).
fof(f583, plain, ((sz00 = smndt0(sz00)) | ~ spl29_1), inference(forward_demodulation, [], [f573, f498])).
fof(f498, plain, ((sz00 = sdtasdt0(smndt0(sz10), sz00)) | ~ spl29_1), inference(resolution, [], [f220, f369])).
fof(f369, plain, (aInteger0(smndt0(sz10)) | ~ spl29_1), inference(avatar_component_clause, [], [f368])).
fof(f368, plain, (spl29_1 <=> aInteger0(smndt0(sz10))), introduced(avatar_definition, [new_symbols(naming, [spl29_1])])).
fof(f220, plain, ! [X0] : (~ aInteger0(X0) | (sz00 = sdtasdt0(X0, sz00))), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ! [X0] : (((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00))) | ~ aInteger0(X0)), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (aInteger0(X0) => ((sz00 = sdtasdt0(sz00, X0)) & (sz00 = sdtasdt0(X0, sz00)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mMulZero)).
fof(f573, plain, (smndt0(sz00) = sdtasdt0(smndt0(sz10), sz00)), inference(resolution, [], [f222, f203])).
fof(f203, plain, aInteger0(sz00), inference(cnf_transformation, [], [f2])).
fof(f2, plain, aInteger0(sz00), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mIntZero)).
fof(f222, plain, ! [X0] : (~ aInteger0(X0) | (smndt0(X0) = sdtasdt0(smndt0(sz10), X0))), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ! [X0] : (((smndt0(X0) = sdtasdt0(X0, smndt0(sz10))) & (smndt0(X0) = sdtasdt0(smndt0(sz10), X0))) | ~ aInteger0(X0)), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (aInteger0(X0) => ((smndt0(X0) = sdtasdt0(X0, smndt0(sz10))) & (smndt0(X0) = sdtasdt0(smndt0(sz10), X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mMulMinOne)).
fof(f320, plain, ! [X0, X1] : (aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) | ~ aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | ~ sP6(X0)), inference(cnf_transformation, [], [f191])).
fof(f191, plain, ! [X0] : (! [X1] : ((aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | (~ sdteqdtlpzmzozddtrp0(X1, sz00, X0) & ~ aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) & ! [X2] : (~ (sdtasdt0(X0, X2) = sdtpldt0(X1, smndt0(sz00))) | ~ aInteger0(X2))) | ~ aInteger0(X1)) & ((sdteqdtlpzmzozddtrp0(X1, sz00, X0) & aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) & ((sdtpldt0(X1, smndt0(sz00)) = sdtasdt0(X0, sK24(X0, X1))) & aInteger0(sK24(X0, X1))) & aInteger0(X1)) | ~ aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)))) | ~ sP6(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK24])], [f189, f190])).
fof(f190, plain, ! [X1, X0] : (? [X3] : ((sdtpldt0(X1, smndt0(sz00)) = sdtasdt0(X0, X3)) & aInteger0(X3)) => ((sdtpldt0(X1, smndt0(sz00)) = sdtasdt0(X0, sK24(X0, X1))) & aInteger0(sK24(X0, X1)))), introduced(choice_axiom, [])).
fof(f189, plain, ! [X0] : (! [X1] : ((aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | (~ sdteqdtlpzmzozddtrp0(X1, sz00, X0) & ~ aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) & ! [X2] : (~ (sdtasdt0(X0, X2) = sdtpldt0(X1, smndt0(sz00))) | ~ aInteger0(X2))) | ~ aInteger0(X1)) & ((sdteqdtlpzmzozddtrp0(X1, sz00, X0) & aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) & ? [X3] : ((sdtpldt0(X1, smndt0(sz00)) = sdtasdt0(X0, X3)) & aInteger0(X3)) & aInteger0(X1)) | ~ aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)))) | ~ sP6(X0)), inference(rectify, [], [f188])).
fof(f188, plain, ! [X5] : (! [X6] : ((aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)) | (~ sdteqdtlpzmzozddtrp0(X6, sz00, X5) & ~ aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ! [X7] : (~ (sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X7)) | ~ aInteger0(X7))) | ~ aInteger0(X6)) & ((sdteqdtlpzmzozddtrp0(X6, sz00, X5) & aDivisorOf0(X5, sdtpldt0(X6, smndt0(sz00))) & ? [X8] : ((sdtpldt0(X6, smndt0(sz00)) = sdtasdt0(X5, X8)) & aInteger0(X8)) & aInteger0(X6)) | ~ aElementOf0(X6, szAzrzSzezqlpdtcmdtrp0(sz00, X5)))) | ~ sP6(X5)), inference(nnf_transformation, [], [f122])).
fof(f324, plain, ! [X0, X1] : (aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | ~ sdteqdtlpzmzozddtrp0(X1, sz00, X0) | ~ aInteger0(X1) | ~ sP6(X0)), inference(cnf_transformation, [], [f191])).
fof(f50836, plain, (spl29_15 | ~ spl29_2 | ~ spl29_81), inference(avatar_split_clause, [], [f49540, f1827, f372, f431])).
fof(f372, plain, (spl29_2 <=> ! [X2] : (~ isPrime0(X2) | ~ aDivisorOf0(X2, smndt0(sz10)))), introduced(avatar_definition, [new_symbols(naming, [spl29_2])])).
fof(f1827, plain, (spl29_81 <=> (smndt0(sz10) = xn)), introduced(avatar_definition, [new_symbols(naming, [spl29_81])])).
fof(f49540, plain, (! [X2] : (~ aDivisorOf0(X2, xn) | ~ isPrime0(X2)) | (~ spl29_2 | ~ spl29_81)), inference(backward_demodulation, [], [f373, f1829])).
fof(f1829, plain, ((smndt0(sz10) = xn) | ~ spl29_81), inference(avatar_component_clause, [], [f1827])).
fof(f373, plain, (! [X2] : (~ aDivisorOf0(X2, smndt0(sz10)) | ~ isPrime0(X2)) | ~ spl29_2), inference(avatar_component_clause, [], [f372])).
fof(f49478, plain, (spl29_80 | spl29_81 | ~ spl29_112 | ~ spl29_338), inference(avatar_contradiction_clause, [], [f49477])).
fof(f49477, plain, ($false | (spl29_80 | spl29_81 | ~ spl29_112 | ~ spl29_338)), inference(subsumption_resolution, [], [f49458, f19044])).
fof(f19044, plain, (aInteger0(xn) | ~ spl29_338), inference(avatar_component_clause, [], [f19043])).
fof(f19043, plain, (spl29_338 <=> aInteger0(xn)), introduced(avatar_definition, [new_symbols(naming, [spl29_338])])).
fof(f49458, plain, (~ aInteger0(xn) | (spl29_80 | spl29_81 | ~ spl29_112 | ~ spl29_338)), inference(resolution, [], [f49371, f351])).
fof(f351, plain, ! [X0] : (~ aDivisorOf0(sz00, X0) | ~ aInteger0(X0)), inference(equality_resolution, [], [f226])).
fof(f226, plain, ! [X0, X1] : (~ (sz00 = X1) | ~ aDivisorOf0(X1, X0) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (! [X1] : ((aDivisorOf0(X1, X0) | ! [X2] : (~ (sdtasdt0(X1, X2) = X0) | ~ aInteger0(X2)) | (sz00 = X1) | ~ aInteger0(X1)) & ((((sdtasdt0(X1, sK9(X0, X1)) = X0) & aInteger0(sK9(X0, X1))) & ~ (sz00 = X1) & aInteger0(X1)) | ~ aDivisorOf0(X1, X0))) | ~ aInteger0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f129, f130])).
fof(f130, plain, ! [X1, X0] : (? [X3] : ((sdtasdt0(X1, X3) = X0) & aInteger0(X3)) => ((sdtasdt0(X1, sK9(X0, X1)) = X0) & aInteger0(sK9(X0, X1)))), introduced(choice_axiom, [])).
fof(f129, plain, ! [X0] : (! [X1] : ((aDivisorOf0(X1, X0) | ! [X2] : (~ (sdtasdt0(X1, X2) = X0) | ~ aInteger0(X2)) | (sz00 = X1) | ~ aInteger0(X1)) & ((? [X3] : ((sdtasdt0(X1, X3) = X0) & aInteger0(X3)) & ~ (sz00 = X1) & aInteger0(X1)) | ~ aDivisorOf0(X1, X0))) | ~ aInteger0(X0)), inference(rectify, [], [f128])).
fof(f128, plain, ! [X0] : (! [X1] : ((aDivisorOf0(X1, X0) | ! [X2] : (~ (sdtasdt0(X1, X2) = X0) | ~ aInteger0(X2)) | (sz00 = X1) | ~ aInteger0(X1)) & ((? [X2] : ((sdtasdt0(X1, X2) = X0) & aInteger0(X2)) & ~ (sz00 = X1) & aInteger0(X1)) | ~ aDivisorOf0(X1, X0))) | ~ aInteger0(X0)), inference(flattening, [], [f127])).
fof(f127, plain, ! [X0] : (! [X1] : ((aDivisorOf0(X1, X0) | (! [X2] : (~ (sdtasdt0(X1, X2) = X0) | ~ aInteger0(X2)) | (sz00 = X1) | ~ aInteger0(X1))) & ((? [X2] : ((sdtasdt0(X1, X2) = X0) & aInteger0(X2)) & ~ (sz00 = X1) & aInteger0(X1)) | ~ aDivisorOf0(X1, X0))) | ~ aInteger0(X0)), inference(nnf_transformation, [], [f77])).
fof(f77, plain, ! [X0] : (! [X1] : (aDivisorOf0(X1, X0) <=> (? [X2] : ((sdtasdt0(X1, X2) = X0) & aInteger0(X2)) & ~ (sz00 = X1) & aInteger0(X1))) | ~ aInteger0(X0)), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0] : (aInteger0(X0) => ! [X1] : (aDivisorOf0(X1, X0) <=> (? [X2] : ((sdtasdt0(X1, X2) = X0) & aInteger0(X2)) & ~ (sz00 = X1) & aInteger0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mDivisor)).
fof(f49371, plain, (aDivisorOf0(sz00, xn) | (spl29_80 | spl29_81 | ~ spl29_112 | ~ spl29_338)), inference(subsumption_resolution, [], [f49370, f19044])).
fof(f49370, plain, (aDivisorOf0(sz00, xn) | ~ aInteger0(xn) | (spl29_80 | spl29_81 | ~ spl29_112)), inference(subsumption_resolution, [], [f49369, f1824])).
fof(f1824, plain, (~ (sz10 = xn) | spl29_80), inference(avatar_component_clause, [], [f1823])).
fof(f1823, plain, (spl29_80 <=> (sz10 = xn)), introduced(avatar_definition, [new_symbols(naming, [spl29_80])])).
fof(f49369, plain, (aDivisorOf0(sz00, xn) | (sz10 = xn) | ~ aInteger0(xn) | (spl29_81 | ~ spl29_112)), inference(subsumption_resolution, [], [f49367, f1828])).
fof(f1828, plain, (~ (smndt0(sz10) = xn) | spl29_81), inference(avatar_component_clause, [], [f1827])).
fof(f49367, plain, (aDivisorOf0(sz00, xn) | (smndt0(sz10) = xn) | (sz10 = xn) | ~ aInteger0(xn) | ~ spl29_112), inference(superposition, [], [f239, f2746])).
fof(f2746, plain, ((sz00 = sK10(xn)) | ~ spl29_112), inference(avatar_component_clause, [], [f2744])).
fof(f2744, plain, (spl29_112 <=> (sz00 = sK10(xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_112])])).
fof(f239, plain, ! [X0] : (aDivisorOf0(sK10(X0), X0) | (smndt0(sz10) = X0) | (sz10 = X0) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f137])).
fof(f137, plain, ! [X0] : ((((isPrime0(sK10(X0)) & aDivisorOf0(sK10(X0), X0)) | (smndt0(sz10) = X0) | (sz10 = X0)) & ((~ (smndt0(sz10) = X0) & ~ (sz10 = X0)) | ! [X2] : (~ isPrime0(X2) | ~ aDivisorOf0(X2, X0)))) | ~ aInteger0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f135, f136])).
fof(f136, plain, ! [X0] : (? [X1] : (isPrime0(X1) & aDivisorOf0(X1, X0)) => (isPrime0(sK10(X0)) & aDivisorOf0(sK10(X0), X0))), introduced(choice_axiom, [])).
fof(f135, plain, ! [X0] : (((? [X1] : (isPrime0(X1) & aDivisorOf0(X1, X0)) | (smndt0(sz10) = X0) | (sz10 = X0)) & ((~ (smndt0(sz10) = X0) & ~ (sz10 = X0)) | ! [X2] : (~ isPrime0(X2) | ~ aDivisorOf0(X2, X0)))) | ~ aInteger0(X0)), inference(rectify, [], [f134])).
fof(f134, plain, ! [X0] : (((? [X1] : (isPrime0(X1) & aDivisorOf0(X1, X0)) | (smndt0(sz10) = X0) | (sz10 = X0)) & ((~ (smndt0(sz10) = X0) & ~ (sz10 = X0)) | ! [X1] : (~ isPrime0(X1) | ~ aDivisorOf0(X1, X0)))) | ~ aInteger0(X0)), inference(flattening, [], [f133])).
fof(f133, plain, ! [X0] : (((? [X1] : (isPrime0(X1) & aDivisorOf0(X1, X0)) | ((smndt0(sz10) = X0) | (sz10 = X0))) & ((~ (smndt0(sz10) = X0) & ~ (sz10 = X0)) | ! [X1] : (~ isPrime0(X1) | ~ aDivisorOf0(X1, X0)))) | ~ aInteger0(X0)), inference(nnf_transformation, [], [f88])).
fof(f88, plain, ! [X0] : ((? [X1] : (isPrime0(X1) & aDivisorOf0(X1, X0)) <=> (~ (smndt0(sz10) = X0) & ~ (sz10 = X0))) | ~ aInteger0(X0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (aInteger0(X0) => (? [X1] : (isPrime0(X1) & aDivisorOf0(X1, X0)) <=> (~ (smndt0(sz10) = X0) & ~ (sz10 = X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mPrimeDivisor)).
fof(f48393, plain, (spl29_112 | ~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_80 | spl29_81 | ~ spl29_82 | ~ spl29_111 | ~ spl29_113 | ~ spl29_338), inference(avatar_split_clause, [], [f48392, f19043, f2748, f2740, f1831, f1827, f1823, f1390, f397, f368, f2744])).
fof(f397, plain, (spl29_8 <=> ! [X0] : (~ aElementOf0(xn, X0) | ~ aElementOf0(X0, xS))), introduced(avatar_definition, [new_symbols(naming, [spl29_8])])).
fof(f1390, plain, (spl29_64 <=> aDivisorOf0(sz10, xn)), introduced(avatar_definition, [new_symbols(naming, [spl29_64])])).
fof(f1831, plain, (spl29_82 <=> isPrime0(sK10(xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_82])])).
fof(f2740, plain, (spl29_111 <=> sP7(sK10(xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_111])])).
fof(f2748, plain, (spl29_113 <=> aInteger0(sK10(xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_113])])).
fof(f48392, plain, ((sz00 = sK10(xn)) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_80 | spl29_81 | ~ spl29_82 | ~ spl29_111 | ~ spl29_113 | ~ spl29_338)), inference(subsumption_resolution, [], [f48391, f2749])).
fof(f2749, plain, (aInteger0(sK10(xn)) | ~ spl29_113), inference(avatar_component_clause, [], [f2748])).
fof(f48391, plain, ((sz00 = sK10(xn)) | ~ aInteger0(sK10(xn)) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_80 | spl29_81 | ~ spl29_82 | ~ spl29_111 | ~ spl29_338)), inference(subsumption_resolution, [], [f48390, f1832])).
fof(f1832, plain, (isPrime0(sK10(xn)) | ~ spl29_82), inference(avatar_component_clause, [], [f1831])).
fof(f48390, plain, (~ isPrime0(sK10(xn)) | (sz00 = sK10(xn)) | ~ aInteger0(sK10(xn)) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_80 | spl29_81 | ~ spl29_111 | ~ spl29_338)), inference(resolution, [], [f40067, f365])).
fof(f365, plain, ! [X1] : (aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, X1), xS) | ~ isPrime0(X1) | (sz00 = X1) | ~ aInteger0(X1)), inference(equality_resolution, [], [f334])).
fof(f334, plain, ! [X0, X1] : (aElementOf0(X0, xS) | ~ (szAzrzSzezqlpdtcmdtrp0(sz00, X1) = X0) | ~ isPrime0(X1) | (sz00 = X1) | ~ aInteger0(X1)), inference(cnf_transformation, [], [f194])).
fof(f40067, plain, (~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(xn)), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_80 | spl29_81 | ~ spl29_111 | ~ spl29_338)), inference(forward_demodulation, [], [f40066, f19417])).
fof(f19417, plain, ((xn = sdtpldt0(xn, sz00)) | (~ spl29_64 | ~ spl29_338)), inference(forward_demodulation, [], [f19416, f6348])).
fof(f6348, plain, ((xn = sK9(xn, sz10)) | ~ spl29_64), inference(forward_demodulation, [], [f6347, f1818])).
fof(f1818, plain, ((xn = sdtasdt0(sz10, sK9(xn, sz10))) | ~ spl29_64), inference(subsumption_resolution, [], [f1816, f336])).
fof(f1816, plain, ((xn = sdtasdt0(sz10, sK9(xn, sz10))) | ~ aInteger0(xn) | ~ spl29_64), inference(resolution, [], [f1392, f228])).
fof(f228, plain, ! [X0, X1] : (~ aDivisorOf0(X1, X0) | (sdtasdt0(X1, sK9(X0, X1)) = X0) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f131])).
fof(f1392, plain, (aDivisorOf0(sz10, xn) | ~ spl29_64), inference(avatar_component_clause, [], [f1390])).
fof(f6347, plain, ((sK9(xn, sz10) = sdtasdt0(sz10, sK9(xn, sz10))) | ~ spl29_64), inference(subsumption_resolution, [], [f6300, f336])).
fof(f6300, plain, (~ aInteger0(xn) | (sK9(xn, sz10) = sdtasdt0(sz10, sK9(xn, sz10))) | ~ spl29_64), inference(resolution, [], [f612, f1392])).
fof(f612, plain, ! [X10, X11] : (~ aDivisorOf0(X10, X11) | ~ aInteger0(X11) | (sK9(X11, X10) = sdtasdt0(sz10, sK9(X11, X10)))), inference(resolution, [], [f227, f217])).
fof(f217, plain, ! [X0] : (~ aInteger0(X0) | (sdtasdt0(sz10, X0) = X0)), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ! [X0] : (((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0)) | ~ aInteger0(X0)), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0] : (aInteger0(X0) => ((sdtasdt0(sz10, X0) = X0) & (sdtasdt0(X0, sz10) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mMulOne)).
fof(f227, plain, ! [X0, X1] : (aInteger0(sK9(X0, X1)) | ~ aDivisorOf0(X1, X0) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f131])).
fof(f19416, plain, ((sK9(xn, sz10) = sdtpldt0(sK9(xn, sz10), sz00)) | (~ spl29_64 | ~ spl29_338)), inference(subsumption_resolution, [], [f19404, f19044])).
fof(f19404, plain, (~ aInteger0(xn) | (sK9(xn, sz10) = sdtpldt0(sK9(xn, sz10), sz00)) | ~ spl29_64), inference(resolution, [], [f1392, f607])).
fof(f607, plain, ! [X0, X1] : (~ aDivisorOf0(X0, X1) | ~ aInteger0(X1) | (sK9(X1, X0) = sdtpldt0(sK9(X1, X0), sz00))), inference(resolution, [], [f227, f210])).
fof(f40066, plain, (~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_80 | spl29_81 | ~ spl29_111 | ~ spl29_338)), inference(subsumption_resolution, [], [f40065, f19044])).
fof(f40065, plain, (~ aInteger0(xn) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_80 | spl29_81 | ~ spl29_111 | ~ spl29_338)), inference(forward_demodulation, [], [f40064, f19417])).
fof(f40064, plain, (~ aInteger0(sdtpldt0(xn, sz00)) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_80 | spl29_81 | ~ spl29_111 | ~ spl29_338)), inference(subsumption_resolution, [], [f40063, f1824])).
fof(f40063, plain, ((sz10 = xn) | ~ aInteger0(sdtpldt0(xn, sz00)) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_81 | ~ spl29_111 | ~ spl29_338)), inference(forward_demodulation, [], [f40062, f19417])).
fof(f40062, plain, ((sz10 = sdtpldt0(xn, sz00)) | ~ aInteger0(sdtpldt0(xn, sz00)) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | spl29_81 | ~ spl29_111 | ~ spl29_338)), inference(subsumption_resolution, [], [f40061, f1828])).
fof(f40061, plain, ((smndt0(sz10) = xn) | (sz10 = sdtpldt0(xn, sz00)) | ~ aInteger0(sdtpldt0(xn, sz00)) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | ~ spl29_111 | ~ spl29_338)), inference(forward_demodulation, [], [f40060, f19417])).
fof(f40060, plain, ((smndt0(sz10) = sdtpldt0(xn, sz00)) | (sz10 = sdtpldt0(xn, sz00)) | ~ aInteger0(sdtpldt0(xn, sz00)) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | ~ spl29_111 | ~ spl29_338)), inference(subsumption_resolution, [], [f40059, f2742])).
fof(f2742, plain, (sP7(sK10(xn)) | ~ spl29_111), inference(avatar_component_clause, [], [f2740])).
fof(f40059, plain, (~ sP7(sK10(xn)) | (smndt0(sz10) = sdtpldt0(xn, sz00)) | (sz10 = sdtpldt0(xn, sz00)) | ~ aInteger0(sdtpldt0(xn, sz00)) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_64 | ~ spl29_338)), inference(forward_demodulation, [], [f40058, f19417])).
fof(f40058, plain, (~ sP7(sK10(sdtpldt0(xn, sz00))) | (smndt0(sz10) = sdtpldt0(xn, sz00)) | (sz10 = sdtpldt0(xn, sz00)) | ~ aInteger0(sdtpldt0(xn, sz00)) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8 | ~ spl29_338)), inference(subsumption_resolution, [], [f40034, f19044])).
fof(f40034, plain, (~ aInteger0(xn) | ~ sP7(sK10(sdtpldt0(xn, sz00))) | (smndt0(sz10) = sdtpldt0(xn, sz00)) | (sz10 = sdtpldt0(xn, sz00)) | ~ aInteger0(sdtpldt0(xn, sz00)) | ~ aElementOf0(szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(xn, sz00))), xS) | (~ spl29_1 | ~ spl29_8)), inference(resolution, [], [f2005, f398])).
fof(f398, plain, (! [X0] : (~ aElementOf0(xn, X0) | ~ aElementOf0(X0, xS)) | ~ spl29_8), inference(avatar_component_clause, [], [f397])).
fof(f2005, plain, (! [X0] : (aElementOf0(X0, szAzrzSzezqlpdtcmdtrp0(sz00, sK10(sdtpldt0(X0, sz00)))) | ~ aInteger0(X0) | ~ sP7(sK10(sdtpldt0(X0, sz00))) | (sdtpldt0(X0, sz00) = smndt0(sz10)) | (sz10 = sdtpldt0(X0, sz00)) | ~ aInteger0(sdtpldt0(X0, sz00))) | ~ spl29_1), inference(resolution, [], [f587, f239])).
fof(f587, plain, (! [X0, X1] : (~ aDivisorOf0(X0, sdtpldt0(X1, sz00)) | aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | ~ aInteger0(X1) | ~ sP7(X0)) | ~ spl29_1), inference(backward_demodulation, [], [f315, f583])).
fof(f315, plain, ! [X0, X1] : (aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | ~ aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) | ~ aInteger0(X1) | ~ sP7(X0)), inference(cnf_transformation, [], [f187])).
fof(f187, plain, ! [X0] : (! [X1] : ((aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | (~ sdteqdtlpzmzozddtrp0(X1, sz00, X0) & ~ aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) & ! [X2] : (~ (sdtasdt0(X0, X2) = sdtpldt0(X1, smndt0(sz00))) | ~ aInteger0(X2))) | ~ aInteger0(X1)) & ((sdteqdtlpzmzozddtrp0(X1, sz00, X0) & aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) & ((sdtpldt0(X1, smndt0(sz00)) = sdtasdt0(X0, sK23(X0, X1))) & aInteger0(sK23(X0, X1))) & aInteger0(X1)) | ~ aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)))) | ~ sP7(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK23])], [f185, f186])).
fof(f186, plain, ! [X1, X0] : (? [X3] : ((sdtpldt0(X1, smndt0(sz00)) = sdtasdt0(X0, X3)) & aInteger0(X3)) => ((sdtpldt0(X1, smndt0(sz00)) = sdtasdt0(X0, sK23(X0, X1))) & aInteger0(sK23(X0, X1)))), introduced(choice_axiom, [])).
fof(f185, plain, ! [X0] : (! [X1] : ((aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | (~ sdteqdtlpzmzozddtrp0(X1, sz00, X0) & ~ aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) & ! [X2] : (~ (sdtasdt0(X0, X2) = sdtpldt0(X1, smndt0(sz00))) | ~ aInteger0(X2))) | ~ aInteger0(X1)) & ((sdteqdtlpzmzozddtrp0(X1, sz00, X0) & aDivisorOf0(X0, sdtpldt0(X1, smndt0(sz00))) & ? [X3] : ((sdtpldt0(X1, smndt0(sz00)) = sdtasdt0(X0, X3)) & aInteger0(X3)) & aInteger0(X1)) | ~ aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)))) | ~ sP7(X0)), inference(rectify, [], [f184])).
fof(f184, plain, ! [X1] : (! [X2] : ((aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)) | (~ sdteqdtlpzmzozddtrp0(X2, sz00, X1) & ~ aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ! [X3] : (~ (sdtasdt0(X1, X3) = sdtpldt0(X2, smndt0(sz00))) | ~ aInteger0(X3))) | ~ aInteger0(X2)) & ((sdteqdtlpzmzozddtrp0(X2, sz00, X1) & aDivisorOf0(X1, sdtpldt0(X2, smndt0(sz00))) & ? [X4] : ((sdtpldt0(X2, smndt0(sz00)) = sdtasdt0(X1, X4)) & aInteger0(X4)) & aInteger0(X2)) | ~ aElementOf0(X2, szAzrzSzezqlpdtcmdtrp0(sz00, X1)))) | ~ sP7(X1)), inference(nnf_transformation, [], [f123])).
fof(f19248, plain, (spl29_111 | spl29_112 | ~ spl29_113 | ~ spl29_3 | ~ spl29_82), inference(avatar_split_clause, [], [f19242, f1831, f377, f2748, f2744, f2740])).
fof(f377, plain, (spl29_3 <=> ! [X1] : (sP7(X1) | ~ aInteger0(X1) | (sz00 = X1) | ~ isPrime0(X1))), introduced(avatar_definition, [new_symbols(naming, [spl29_3])])).
fof(f19242, plain, (~ aInteger0(sK10(xn)) | (sz00 = sK10(xn)) | sP7(sK10(xn)) | (~ spl29_3 | ~ spl29_82)), inference(resolution, [], [f1832, f378])).
fof(f378, plain, (! [X1] : (~ isPrime0(X1) | ~ aInteger0(X1) | (sz00 = X1) | sP7(X1)) | ~ spl29_3), inference(avatar_component_clause, [], [f377])).
fof(f19247, plain, (spl29_80 | spl29_81 | spl29_113 | ~ spl29_338), inference(avatar_contradiction_clause, [], [f19246])).
fof(f19246, plain, ($false | (spl29_80 | spl29_81 | spl29_113 | ~ spl29_338)), inference(subsumption_resolution, [], [f19245, f1828])).
fof(f19245, plain, ((smndt0(sz10) = xn) | (spl29_80 | spl29_113 | ~ spl29_338)), inference(subsumption_resolution, [], [f19244, f19044])).
fof(f19244, plain, (~ aInteger0(xn) | (smndt0(sz10) = xn) | (spl29_80 | spl29_113)), inference(subsumption_resolution, [], [f19243, f1824])).
fof(f19243, plain, ((sz10 = xn) | ~ aInteger0(xn) | (smndt0(sz10) = xn) | spl29_113), inference(resolution, [], [f2750, f1794])).
fof(f1794, plain, ! [X1] : (aInteger0(sK10(X1)) | (sz10 = X1) | ~ aInteger0(X1) | (smndt0(sz10) = X1)), inference(duplicate_literal_removal, [], [f1790])).
fof(f1790, plain, ! [X1] : ((smndt0(sz10) = X1) | (sz10 = X1) | ~ aInteger0(X1) | aInteger0(sK10(X1)) | ~ aInteger0(X1)), inference(resolution, [], [f239, f225])).
fof(f225, plain, ! [X0, X1] : (~ aDivisorOf0(X1, X0) | aInteger0(X1) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f131])).
fof(f2750, plain, (~ aInteger0(sK10(xn)) | spl29_113), inference(avatar_component_clause, [], [f2748])).
fof(f19127, plain, spl29_338, inference(avatar_split_clause, [], [f336, f19043])).
fof(f19033, plain, (~ spl29_9 | ~ spl29_93), inference(avatar_contradiction_clause, [], [f19032])).
fof(f19032, plain, ($false | (~ spl29_9 | ~ spl29_93)), inference(subsumption_resolution, [], [f19019, f403])).
fof(f403, plain, (isPrime0(sK26) | ~ spl29_9), inference(avatar_component_clause, [], [f401])).
fof(f401, plain, (spl29_9 <=> isPrime0(sK26)), introduced(avatar_definition, [new_symbols(naming, [spl29_9])])).
fof(f19019, plain, (~ isPrime0(sK26) | ~ spl29_93), inference(resolution, [], [f2068, f375])).
fof(f375, plain, ! [X2] : (~ aDivisorOf0(X2, sz10) | ~ isPrime0(X2)), inference(subsumption_resolution, [], [f353, f204])).
fof(f204, plain, aInteger0(sz10), inference(cnf_transformation, [], [f3])).
fof(f3, plain, aInteger0(sz10), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mIntOne)).
fof(f353, plain, ! [X2] : (~ isPrime0(X2) | ~ aDivisorOf0(X2, sz10) | ~ aInteger0(sz10)), inference(equality_resolution, [], [f237])).
fof(f237, plain, ! [X2, X0] : (~ (sz10 = X0) | ~ isPrime0(X2) | ~ aDivisorOf0(X2, X0) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f137])).
fof(f2068, plain, (aDivisorOf0(sK26, sz10) | ~ spl29_93), inference(avatar_component_clause, [], [f2066])).
fof(f2066, plain, (spl29_93 <=> aDivisorOf0(sK26, sz10)), introduced(avatar_definition, [new_symbols(naming, [spl29_93])])).
fof(f2071, plain, (~ spl29_14 | ~ spl29_12 | spl29_93 | ~ spl29_11 | spl29_13 | ~ spl29_80), inference(avatar_split_clause, [], [f2070, f1823, f421, f411, f2066, f416, f426])).
fof(f426, plain, (spl29_14 <=> aInteger0(sK26)), introduced(avatar_definition, [new_symbols(naming, [spl29_14])])).
fof(f416, plain, (spl29_12 <=> aInteger0(sK27)), introduced(avatar_definition, [new_symbols(naming, [spl29_12])])).
fof(f411, plain, (spl29_11 <=> (xn = sdtasdt0(sK26, sK27))), introduced(avatar_definition, [new_symbols(naming, [spl29_11])])).
fof(f421, plain, (spl29_13 <=> (sz00 = sK26)), introduced(avatar_definition, [new_symbols(naming, [spl29_13])])).
fof(f2070, plain, (aDivisorOf0(sK26, sz10) | ~ aInteger0(sK27) | ~ aInteger0(sK26) | (~ spl29_11 | spl29_13 | ~ spl29_80)), inference(forward_demodulation, [], [f1496, f1825])).
fof(f1825, plain, ((sz10 = xn) | ~ spl29_80), inference(avatar_component_clause, [], [f1823])).
fof(f1496, plain, (aDivisorOf0(sK26, xn) | ~ aInteger0(sK27) | ~ aInteger0(sK26) | (~ spl29_11 | spl29_13)), inference(subsumption_resolution, [], [f1358, f423])).
fof(f423, plain, (~ (sz00 = sK26) | spl29_13), inference(avatar_component_clause, [], [f421])).
fof(f1358, plain, (aDivisorOf0(sK26, xn) | ~ aInteger0(sK27) | (sz00 = sK26) | ~ aInteger0(sK26) | ~ spl29_11), inference(superposition, [], [f366, f413])).
fof(f413, plain, ((xn = sdtasdt0(sK26, sK27)) | ~ spl29_11), inference(avatar_component_clause, [], [f411])).
fof(f366, plain, ! [X2, X1] : (aDivisorOf0(X1, sdtasdt0(X1, X2)) | ~ aInteger0(X2) | (sz00 = X1) | ~ aInteger0(X1)), inference(subsumption_resolution, [], [f350, f207])).
fof(f207, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(flattening, [], [f58])).
fof(f58, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | (~ aInteger0(X1) | ~ aInteger0(X0))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0, X1] : ((aInteger0(X1) & aInteger0(X0)) => aInteger0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mIntMult)).
fof(f350, plain, ! [X2, X1] : (aDivisorOf0(X1, sdtasdt0(X1, X2)) | ~ aInteger0(X2) | (sz00 = X1) | ~ aInteger0(X1) | ~ aInteger0(sdtasdt0(X1, X2))), inference(equality_resolution, [], [f229])).
fof(f229, plain, ! [X2, X0, X1] : (aDivisorOf0(X1, X0) | ~ (sdtasdt0(X1, X2) = X0) | ~ aInteger0(X2) | (sz00 = X1) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f131])).
fof(f1837, plain, (spl29_80 | spl29_81 | spl29_82), inference(avatar_split_clause, [], [f1836, f1831, f1827, f1823])).
fof(f1836, plain, ((smndt0(sz10) = xn) | (sz10 = xn) | spl29_82), inference(subsumption_resolution, [], [f1835, f336])).
fof(f1835, plain, ((smndt0(sz10) = xn) | (sz10 = xn) | ~ aInteger0(xn) | spl29_82), inference(resolution, [], [f1833, f240])).
fof(f240, plain, ! [X0] : (isPrime0(sK10(X0)) | (smndt0(sz10) = X0) | (sz10 = X0) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f137])).
fof(f1833, plain, (~ isPrime0(sK10(xn)) | spl29_82), inference(avatar_component_clause, [], [f1831])).
fof(f1612, plain, (spl29_13 | ~ spl29_14 | ~ spl29_60), inference(avatar_contradiction_clause, [], [f1611])).
fof(f1611, plain, ($false | (spl29_13 | ~ spl29_14 | ~ spl29_60)), inference(subsumption_resolution, [], [f1610, f423])).
fof(f1610, plain, ((sz00 = sK26) | (~ spl29_14 | ~ spl29_60)), inference(forward_demodulation, [], [f1585, f509])).
fof(f509, plain, ((sz00 = sdtasdt0(sz00, sK26)) | ~ spl29_14), inference(resolution, [], [f221, f428])).
fof(f428, plain, (aInteger0(sK26) | ~ spl29_14), inference(avatar_component_clause, [], [f426])).
fof(f221, plain, ! [X0] : (~ aInteger0(X0) | (sz00 = sdtasdt0(sz00, X0))), inference(cnf_transformation, [], [f73])).
fof(f1585, plain, ((sK26 = sdtasdt0(sz00, sK26)) | (~ spl29_14 | ~ spl29_60)), inference(backward_demodulation, [], [f493, f1368])).
fof(f1368, plain, ((sz00 = sz10) | ~ spl29_60), inference(avatar_component_clause, [], [f1366])).
fof(f1366, plain, (spl29_60 <=> (sz00 = sz10)), introduced(avatar_definition, [new_symbols(naming, [spl29_60])])).
fof(f493, plain, ((sK26 = sdtasdt0(sz10, sK26)) | ~ spl29_14), inference(resolution, [], [f217, f428])).
fof(f1393, plain, (spl29_60 | spl29_64), inference(avatar_split_clause, [], [f1388, f1390, f1366])).
fof(f1388, plain, (aDivisorOf0(sz10, xn) | (sz00 = sz10)), inference(subsumption_resolution, [], [f1387, f204])).
fof(f1387, plain, (aDivisorOf0(sz10, xn) | (sz00 = sz10) | ~ aInteger0(sz10)), inference(subsumption_resolution, [], [f1344, f336])).
fof(f1344, plain, (aDivisorOf0(sz10, xn) | ~ aInteger0(xn) | (sz00 = sz10) | ~ aInteger0(sz10)), inference(superposition, [], [f366, f491])).
fof(f491, plain, (xn = sdtasdt0(sz10, xn)), inference(resolution, [], [f217, f336])).
fof(f1267, plain, (spl29_57 | ~ spl29_18), inference(avatar_split_clause, [], [f1266, f445, f1260])).
fof(f1266, plain, (! [X5] : (~ aElementOf0(X5, sK28) | sdteqdtlpzmzozddtrp0(X5, sz00, sK25(sK28))) | ~ spl29_18), inference(subsumption_resolution, [], [f1242, f1099])).
fof(f1099, plain, (sP6(sK25(sK28)) | ~ spl29_18), inference(resolution, [], [f447, f330])).
fof(f1242, plain, (! [X5] : (~ aElementOf0(X5, sK28) | sdteqdtlpzmzozddtrp0(X5, sz00, sK25(sK28)) | ~ sP6(sK25(sK28))) | ~ spl29_18), inference(superposition, [], [f321, f1234])).
fof(f1234, plain, ((sK28 = szAzrzSzezqlpdtcmdtrp0(sz00, sK25(sK28))) | ~ spl29_18), inference(resolution, [], [f331, f447])).
fof(f331, plain, ! [X0] : (~ aElementOf0(X0, xS) | (szAzrzSzezqlpdtcmdtrp0(sz00, sK25(X0)) = X0)), inference(cnf_transformation, [], [f194])).
fof(f321, plain, ! [X0, X1] : (~ aElementOf0(X1, szAzrzSzezqlpdtcmdtrp0(sz00, X0)) | sdteqdtlpzmzozddtrp0(X1, sz00, X0) | ~ sP6(X0)), inference(cnf_transformation, [], [f191])).
fof(f462, plain, (~ spl29_9 | ~ spl29_10 | ~ spl29_15), inference(avatar_split_clause, [], [f459, f431, f406, f401])).
fof(f406, plain, (spl29_10 <=> aDivisorOf0(sK26, xn)), introduced(avatar_definition, [new_symbols(naming, [spl29_10])])).
fof(f459, plain, (~ isPrime0(sK26) | (~ spl29_10 | ~ spl29_15)), inference(resolution, [], [f432, f408])).
fof(f408, plain, (aDivisorOf0(sK26, xn) | ~ spl29_10), inference(avatar_component_clause, [], [f406])).
fof(f458, plain, (~ spl29_4 | ~ spl29_8), inference(avatar_contradiction_clause, [], [f457])).
fof(f457, plain, ($false | (~ spl29_4 | ~ spl29_8)), inference(subsumption_resolution, [], [f456, f381])).
fof(f381, plain, (! [X0] : aElementOf0(X0, xS) | ~ spl29_4), inference(avatar_component_clause, [], [f380])).
fof(f380, plain, (spl29_4 <=> ! [X0] : aElementOf0(X0, xS)), introduced(avatar_definition, [new_symbols(naming, [spl29_4])])).
fof(f456, plain, (~ aElementOf0(xS, xS) | (~ spl29_4 | ~ spl29_8)), inference(resolution, [], [f398, f381])).
fof(f451, plain, spl29_1, inference(avatar_contradiction_clause, [], [f450])).
fof(f450, plain, ($false | spl29_1), inference(subsumption_resolution, [], [f449, f204])).
fof(f449, plain, (~ aInteger0(sz10) | spl29_1), inference(resolution, [], [f205, f370])).
fof(f370, plain, (~ aInteger0(smndt0(sz10)) | spl29_1), inference(avatar_component_clause, [], [f368])).
fof(f205, plain, ! [X0] : (aInteger0(smndt0(X0)) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ! [X0] : (aInteger0(smndt0(X0)) | ~ aInteger0(X0)), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ! [X0] : (aInteger0(X0) => aInteger0(smndt0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', mIntNeg)).
fof(f448, plain, (spl29_18 | spl29_6), inference(avatar_split_clause, [], [f345, f388, f445])).
fof(f388, plain, (spl29_6 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl29_6])])).
fof(f345, plain, (sP8 | aElementOf0(sK28, xS)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, (sP8 | (! [X0] : (~ isPrime0(X0) | (~ aDivisorOf0(X0, xn) & (! [X1] : (~ (sdtasdt0(X0, X1) = xn) | ~ aInteger0(X1)) | (sz00 = X0) | ~ aInteger0(X0)))) & aElementOf0(xn, sbsmnsldt0(xS)) & (aElementOf0(xn, sK28) & aElementOf0(sK28, xS)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK28])], [f200, f201])).
fof(f201, plain, (? [X2] : (aElementOf0(xn, X2) & aElementOf0(X2, xS)) => (aElementOf0(xn, sK28) & aElementOf0(sK28, xS))), introduced(choice_axiom, [])).
fof(f200, plain, (sP8 | (! [X0] : (~ isPrime0(X0) | (~ aDivisorOf0(X0, xn) & (! [X1] : (~ (sdtasdt0(X0, X1) = xn) | ~ aInteger0(X1)) | (sz00 = X0) | ~ aInteger0(X0)))) & aElementOf0(xn, sbsmnsldt0(xS)) & ? [X2] : (aElementOf0(xn, X2) & aElementOf0(X2, xS)))), inference(rectify, [], [f126])).
fof(f126, plain, (sP8 | (! [X4] : (~ isPrime0(X4) | (~ aDivisorOf0(X4, xn) & (! [X5] : (~ (xn = sdtasdt0(X4, X5)) | ~ aInteger0(X5)) | (sz00 = X4) | ~ aInteger0(X4)))) & aElementOf0(xn, sbsmnsldt0(xS)) & ? [X3] : (aElementOf0(xn, X3) & aElementOf0(X3, xS)))), inference(definition_folding, [], [f112, e125])).
fof(f125, plain, ((~ aElementOf0(xn, sbsmnsldt0(xS)) & ! [X2] : (~ aElementOf0(xn, X2) | ~ aElementOf0(X2, xS)) & ? [X0] : (isPrime0(X0) & aDivisorOf0(X0, xn) & ? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0))) | ~ sP8), inference(usedef, [], [e125])).
fof(e125, plain, (sP8 <=> (~ aElementOf0(xn, sbsmnsldt0(xS)) & ! [X2] : (~ aElementOf0(xn, X2) | ~ aElementOf0(X2, xS)) & ? [X0] : (isPrime0(X0) & aDivisorOf0(X0, xn) & ? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f112, plain, ((~ aElementOf0(xn, sbsmnsldt0(xS)) & ! [X2] : (~ aElementOf0(xn, X2) | ~ aElementOf0(X2, xS)) & ? [X0] : (isPrime0(X0) & aDivisorOf0(X0, xn) & ? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0))) | (! [X4] : (~ isPrime0(X4) | (~ aDivisorOf0(X4, xn) & (! [X5] : (~ (xn = sdtasdt0(X4, X5)) | ~ aInteger0(X5)) | (sz00 = X4) | ~ aInteger0(X4)))) & aElementOf0(xn, sbsmnsldt0(xS)) & ? [X3] : (aElementOf0(xn, X3) & aElementOf0(X3, xS)))), inference(flattening, [], [f111])).
fof(f111, plain, (((~ aElementOf0(xn, sbsmnsldt0(xS)) & ! [X2] : (~ aElementOf0(xn, X2) | ~ aElementOf0(X2, xS))) & ? [X0] : (isPrime0(X0) & aDivisorOf0(X0, xn) & ? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0))) | (! [X4] : (~ isPrime0(X4) | (~ aDivisorOf0(X4, xn) & (! [X5] : (~ (xn = sdtasdt0(X4, X5)) | ~ aInteger0(X5)) | (sz00 = X4) | ~ aInteger0(X4)))) & (aElementOf0(xn, sbsmnsldt0(xS)) & ? [X3] : (aElementOf0(xn, X3) & aElementOf0(X3, xS))))), inference(ennf_transformation, [], [f53])).
fof(f53, plain, ~ ((? [X0] : (isPrime0(X0) & aDivisorOf0(X0, xn) & ? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0)) => (aElementOf0(xn, sbsmnsldt0(xS)) | ? [X2] : (aElementOf0(xn, X2) & aElementOf0(X2, xS)))) & ((aElementOf0(xn, sbsmnsldt0(xS)) & ? [X3] : (aElementOf0(xn, X3) & aElementOf0(X3, xS))) => ? [X4] : (isPrime0(X4) & (aDivisorOf0(X4, xn) | (? [X5] : ((xn = sdtasdt0(X4, X5)) & aInteger0(X5)) & ~ (sz00 = X4) & aInteger0(X4)))))), inference(rectify, [], [f45])).
fof(f45, plain, ~ ((? [X0] : (isPrime0(X0) & aDivisorOf0(X0, xn) & ? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0)) => (aElementOf0(xn, sbsmnsldt0(xS)) | ? [X0] : (aElementOf0(xn, X0) & aElementOf0(X0, xS)))) & ((aElementOf0(xn, sbsmnsldt0(xS)) & ? [X0] : (aElementOf0(xn, X0) & aElementOf0(X0, xS))) => ? [X0] : (isPrime0(X0) & (aDivisorOf0(X0, xn) | (? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0)))))), inference(negated_conjecture, [], [f44])).
fof(f44, plain, ~ ((? [X0] : (isPrime0(X0) & aDivisorOf0(X0, xn) & ? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0)) => (aElementOf0(xn, sbsmnsldt0(xS)) | ? [X0] : (aElementOf0(xn, X0) & aElementOf0(X0, xS)))) & ((aElementOf0(xn, sbsmnsldt0(xS)) & ? [X0] : (aElementOf0(xn, X0) & aElementOf0(X0, xS))) => ? [X0] : (isPrime0(X0) & (aDivisorOf0(X0, xn) | (? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0)))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM447+5.p', m__)).
fof(f443, plain, (spl29_17 | spl29_6), inference(avatar_split_clause, [], [f346, f388, f440])).
fof(f346, plain, (sP8 | aElementOf0(xn, sK28)), inference(cnf_transformation, [], [f202])).
fof(f433, plain, (spl29_15 | spl29_6), inference(avatar_split_clause, [], [f349, f388, f431])).
fof(f349, plain, ! [X0] : (sP8 | ~ isPrime0(X0) | ~ aDivisorOf0(X0, xn)), inference(cnf_transformation, [], [f202])).
fof(f429, plain, (~ spl29_6 | spl29_14), inference(avatar_split_clause, [], [f337, f426, f388])).
fof(f337, plain, (aInteger0(sK26) | ~ sP8), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ((~ aElementOf0(xn, sbsmnsldt0(xS)) & ! [X0] : (~ aElementOf0(xn, X0) | ~ aElementOf0(X0, xS)) & (isPrime0(sK26) & aDivisorOf0(sK26, xn) & ((xn = sdtasdt0(sK26, sK27)) & aInteger0(sK27)) & ~ (sz00 = sK26) & aInteger0(sK26))) | ~ sP8), inference(skolemisation, [status(esa), new_symbols(skolem, [sK26, sK27])], [f196, f198, f197])).
fof(f197, plain, (? [X1] : (isPrime0(X1) & aDivisorOf0(X1, xn) & ? [X2] : ((sdtasdt0(X1, X2) = xn) & aInteger0(X2)) & ~ (sz00 = X1) & aInteger0(X1)) => (isPrime0(sK26) & aDivisorOf0(sK26, xn) & ? [X2] : ((xn = sdtasdt0(sK26, X2)) & aInteger0(X2)) & ~ (sz00 = sK26) & aInteger0(sK26))), introduced(choice_axiom, [])).
fof(f198, plain, (? [X2] : ((xn = sdtasdt0(sK26, X2)) & aInteger0(X2)) => ((xn = sdtasdt0(sK26, sK27)) & aInteger0(sK27))), introduced(choice_axiom, [])).
fof(f196, plain, ((~ aElementOf0(xn, sbsmnsldt0(xS)) & ! [X0] : (~ aElementOf0(xn, X0) | ~ aElementOf0(X0, xS)) & ? [X1] : (isPrime0(X1) & aDivisorOf0(X1, xn) & ? [X2] : ((sdtasdt0(X1, X2) = xn) & aInteger0(X2)) & ~ (sz00 = X1) & aInteger0(X1))) | ~ sP8), inference(rectify, [], [f195])).
fof(f195, plain, ((~ aElementOf0(xn, sbsmnsldt0(xS)) & ! [X2] : (~ aElementOf0(xn, X2) | ~ aElementOf0(X2, xS)) & ? [X0] : (isPrime0(X0) & aDivisorOf0(X0, xn) & ? [X1] : ((sdtasdt0(X0, X1) = xn) & aInteger0(X1)) & ~ (sz00 = X0) & aInteger0(X0))) | ~ sP8), inference(nnf_transformation, [], [f125])).
fof(f424, plain, (~ spl29_6 | ~ spl29_13), inference(avatar_split_clause, [], [f338, f421, f388])).
fof(f338, plain, (~ (sz00 = sK26) | ~ sP8), inference(cnf_transformation, [], [f199])).
fof(f419, plain, (~ spl29_6 | spl29_12), inference(avatar_split_clause, [], [f339, f416, f388])).
fof(f339, plain, (aInteger0(sK27) | ~ sP8), inference(cnf_transformation, [], [f199])).
fof(f414, plain, (~ spl29_6 | spl29_11), inference(avatar_split_clause, [], [f340, f411, f388])).
fof(f340, plain, ((xn = sdtasdt0(sK26, sK27)) | ~ sP8), inference(cnf_transformation, [], [f199])).
fof(f409, plain, (~ spl29_6 | spl29_10), inference(avatar_split_clause, [], [f341, f406, f388])).
fof(f341, plain, (aDivisorOf0(sK26, xn) | ~ sP8), inference(cnf_transformation, [], [f199])).
fof(f404, plain, (~ spl29_6 | spl29_9), inference(avatar_split_clause, [], [f342, f401, f388])).
fof(f342, plain, (isPrime0(sK26) | ~ sP8), inference(cnf_transformation, [], [f199])).
fof(f399, plain, (~ spl29_6 | spl29_8), inference(avatar_split_clause, [], [f343, f397, f388])).
fof(f343, plain, ! [X0] : (~ aElementOf0(xn, X0) | ~ aElementOf0(X0, xS) | ~ sP8), inference(cnf_transformation, [], [f199])).
fof(f382, plain, (spl29_3 | spl29_4), inference(avatar_split_clause, [], [f333, f380, f377])).
fof(f333, plain, ! [X0, X1] : (aElementOf0(X0, xS) | sP7(X1) | ~ isPrime0(X1) | (sz00 = X1) | ~ aInteger0(X1)), inference(cnf_transformation, [], [f194])).
fof(f374, plain, (~ spl29_1 | spl29_2), inference(avatar_split_clause, [], [f352, f372, f368])).
fof(f352, plain, ! [X2] : (~ isPrime0(X2) | ~ aDivisorOf0(X2, smndt0(sz10)) | ~ aInteger0(smndt0(sz10))), inference(equality_resolution, [], [f238])).
fof(f238, plain, ! [X2, X0] : (~ (smndt0(sz10) = X0) | ~ isPrime0(X2) | ~ aDivisorOf0(X2, X0) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f137])).