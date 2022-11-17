fof(f387426, plain, $false, inference(avatar_sat_refutation, [], [f143, f157, f2283, f2399, f6942, f386051, f387425])).
fof(f387425, plain, (~ spl3_7 | ~ spl3_40 | ~ spl3_77), inference(avatar_contradiction_clause, [], [f387424])).
fof(f387424, plain, ($false | (~ spl3_7 | ~ spl3_40 | ~ spl3_77)), inference(subsumption_resolution, [], [f387423, f2246])).
fof(f2246, plain, (aInteger0(sdtasdt0(xp, sK2)) | ~ spl3_40), inference(avatar_component_clause, [], [f2245])).
fof(f2245, plain, (spl3_40 <=> aInteger0(sdtasdt0(xp, sK2))), introduced(avatar_definition, [new_symbols(naming, [spl3_40])])).
fof(f387423, plain, (~ aInteger0(sdtasdt0(xp, sK2)) | (~ spl3_7 | ~ spl3_77)), inference(trivial_inequality_removal, [], [f387420])).
fof(f387420, plain, (~ (sdtpldt0(xa, smndt0(xb)) = sdtpldt0(xa, smndt0(xb))) | ~ aInteger0(sdtasdt0(xp, sK2)) | (~ spl3_7 | ~ spl3_77)), inference(superposition, [], [f156, f134112])).
fof(f134112, plain, ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, sdtasdt0(xp, sK2))) | ~ spl3_77), inference(backward_demodulation, [], [f133297, f134094])).
fof(f134094, plain, (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, sdtasdt0(xq, sK2))), inference(backward_demodulation, [], [f126826, f117])).
fof(f117, plain, (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(sdtasdt0(xp, xq), sK2)), inference(cnf_transformation, [], [f73])).
fof(f73, plain, (((~ sdteqdtlpzmzozddtrp0(xa, xb, xq) & ~ aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) & ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0))) | sP0) & sdteqdtlpzmzozddtrp0(xa, xb, sdtasdt0(xp, xq)) & aDivisorOf0(sdtasdt0(xp, xq), sdtpldt0(xa, smndt0(xb))) & ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(sdtasdt0(xp, xq), sK2)) & aInteger0(sK2)) & ~ (sz00 = sdtasdt0(xp, xq))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK2])], [f71, f72])).
fof(f72, plain, (? [X1] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(sdtasdt0(xp, xq), X1)) & aInteger0(X1)) => ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(sdtasdt0(xp, xq), sK2)) & aInteger0(sK2))), introduced(choice_axiom, [])).
fof(f71, plain, (((~ sdteqdtlpzmzozddtrp0(xa, xb, xq) & ~ aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) & ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0))) | sP0) & sdteqdtlpzmzozddtrp0(xa, xb, sdtasdt0(xp, xq)) & aDivisorOf0(sdtasdt0(xp, xq), sdtpldt0(xa, smndt0(xb))) & ? [X1] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(sdtasdt0(xp, xq), X1)) & aInteger0(X1)) & ~ (sz00 = sdtasdt0(xp, xq))), inference(rectify, [], [f62])).
fof(f62, plain, (((~ sdteqdtlpzmzozddtrp0(xa, xb, xq) & ~ aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) & ! [X1] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X1)) | ~ aInteger0(X1))) | sP0) & sdteqdtlpzmzozddtrp0(xa, xb, sdtasdt0(xp, xq)) & aDivisorOf0(sdtasdt0(xp, xq), sdtpldt0(xa, smndt0(xb))) & ? [X0] : ((sdtasdt0(sdtasdt0(xp, xq), X0) = sdtpldt0(xa, smndt0(xb))) & aInteger0(X0)) & ~ (sz00 = sdtasdt0(xp, xq))), inference(definition_folding, [], [f60, e61])).
fof(f61, plain, ((~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X2] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X2)) | ~ aInteger0(X2))) | ~ sP0), inference(usedef, [], [e61])).
fof(e61, plain, (sP0 <=> (~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X2] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X2)) | ~ aInteger0(X2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f60, plain, (((~ sdteqdtlpzmzozddtrp0(xa, xb, xq) & ~ aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) & ! [X1] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X1)) | ~ aInteger0(X1))) | (~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X2] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X2)) | ~ aInteger0(X2)))) & sdteqdtlpzmzozddtrp0(xa, xb, sdtasdt0(xp, xq)) & aDivisorOf0(sdtasdt0(xp, xq), sdtpldt0(xa, smndt0(xb))) & ? [X0] : ((sdtasdt0(sdtasdt0(xp, xq), X0) = sdtpldt0(xa, smndt0(xb))) & aInteger0(X0)) & ~ (sz00 = sdtasdt0(xp, xq))), inference(flattening, [], [f59])).
fof(f59, plain, (((~ sdteqdtlpzmzozddtrp0(xa, xb, xq) & ~ aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) & ! [X1] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X1)) | ~ aInteger0(X1))) | (~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X2] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X2)) | ~ aInteger0(X2)))) & (sdteqdtlpzmzozddtrp0(xa, xb, sdtasdt0(xp, xq)) & aDivisorOf0(sdtasdt0(xp, xq), sdtpldt0(xa, smndt0(xb))) & ? [X0] : ((sdtasdt0(sdtasdt0(xp, xq), X0) = sdtpldt0(xa, smndt0(xb))) & aInteger0(X0)) & ~ (sz00 = sdtasdt0(xp, xq)))), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ~ ((sdteqdtlpzmzozddtrp0(xa, xb, sdtasdt0(xp, xq)) & aDivisorOf0(sdtasdt0(xp, xq), sdtpldt0(xa, smndt0(xb))) & ? [X0] : ((sdtasdt0(sdtasdt0(xp, xq), X0) = sdtpldt0(xa, smndt0(xb))) & aInteger0(X0)) & ~ (sz00 = sdtasdt0(xp, xq))) => ((sdteqdtlpzmzozddtrp0(xa, xb, xq) | aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) | ? [X1] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X1)) & aInteger0(X1))) & (sdteqdtlpzmzozddtrp0(xa, xb, xp) | aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) | ? [X2] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X2)) & aInteger0(X2))))), inference(rectify, [], [f25])).
fof(f25, plain, ~ ((sdteqdtlpzmzozddtrp0(xa, xb, sdtasdt0(xp, xq)) & aDivisorOf0(sdtasdt0(xp, xq), sdtpldt0(xa, smndt0(xb))) & ? [X0] : ((sdtasdt0(sdtasdt0(xp, xq), X0) = sdtpldt0(xa, smndt0(xb))) & aInteger0(X0)) & ~ (sz00 = sdtasdt0(xp, xq))) => ((sdteqdtlpzmzozddtrp0(xa, xb, xq) | aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) & aInteger0(X0))) & (sdteqdtlpzmzozddtrp0(xa, xb, xp) | aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) & aInteger0(X0))))), inference(negated_conjecture, [], [f24])).
fof(f24, plain, ~ ((sdteqdtlpzmzozddtrp0(xa, xb, sdtasdt0(xp, xq)) & aDivisorOf0(sdtasdt0(xp, xq), sdtpldt0(xa, smndt0(xb))) & ? [X0] : ((sdtasdt0(sdtasdt0(xp, xq), X0) = sdtpldt0(xa, smndt0(xb))) & aInteger0(X0)) & ~ (sz00 = sdtasdt0(xp, xq))) => ((sdteqdtlpzmzozddtrp0(xa, xb, xq) | aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) & aInteger0(X0))) & (sdteqdtlpzmzozddtrp0(xa, xb, xp) | aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) & aInteger0(X0))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM433+3.p', m__)).
fof(f126826, plain, (sdtasdt0(sdtasdt0(xp, xq), sK2) = sdtasdt0(xp, sdtasdt0(xq, sK2))), inference(resolution, [], [f3570, f108])).
fof(f108, plain, aInteger0(xp), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (~ (sz00 = xq) & aInteger0(xq) & ~ (sz00 = xp) & aInteger0(xp) & aInteger0(xb) & aInteger0(xa)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM433+3.p', m__979)).
fof(f3570, plain, ! [X22] : (~ aInteger0(X22) | (sdtasdt0(X22, sdtasdt0(xq, sK2)) = sdtasdt0(sdtasdt0(X22, xq), sK2))), inference(resolution, [], [f782, f110])).
fof(f110, plain, aInteger0(xq), inference(cnf_transformation, [], [f23])).
fof(f782, plain, ! [X4, X3] : (~ aInteger0(X4) | (sdtasdt0(X3, sdtasdt0(X4, sK2)) = sdtasdt0(sdtasdt0(X3, X4), sK2)) | ~ aInteger0(X3)), inference(resolution, [], [f116, f85])).
fof(f85, plain, ! [X2, X0, X1] : (~ aInteger0(X2) | (sdtasdt0(X0, sdtasdt0(X1, X2)) = sdtasdt0(sdtasdt0(X0, X1), X2)) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ! [X0, X1, X2] : ((sdtasdt0(X0, sdtasdt0(X1, X2)) = sdtasdt0(sdtasdt0(X0, X1), X2)) | ~ aInteger0(X2) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(flattening, [], [f39])).
fof(f39, plain, ! [X0, X1, X2] : ((sdtasdt0(X0, sdtasdt0(X1, X2)) = sdtasdt0(sdtasdt0(X0, X1), X2)) | (~ aInteger0(X2) | ~ aInteger0(X1) | ~ aInteger0(X0))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0, X1, X2] : ((aInteger0(X2) & aInteger0(X1) & aInteger0(X0)) => (sdtasdt0(X0, sdtasdt0(X1, X2)) = sdtasdt0(sdtasdt0(X0, X1), X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM433+3.p', mMulAsso)).
fof(f116, plain, aInteger0(sK2), inference(cnf_transformation, [], [f73])).
fof(f133297, plain, ((sdtasdt0(xq, sdtasdt0(xp, sK2)) = sdtasdt0(xp, sdtasdt0(xq, sK2))) | ~ spl3_77), inference(forward_demodulation, [], [f127276, f133283])).
fof(f133283, plain, ((sdtasdt0(sdtasdt0(xq, sK2), xp) = sdtasdt0(xp, sdtasdt0(xq, sK2))) | ~ spl3_77), inference(backward_demodulation, [], [f130054, f133281])).
fof(f133281, plain, ((sdtasdt0(sK2, sdtasdt0(xp, xq)) = sdtasdt0(xp, sdtasdt0(xq, sK2))) | ~ spl3_77), inference(backward_demodulation, [], [f131216, f127416])).
fof(f127416, plain, (sdtasdt0(sdtasdt0(xp, sK2), xq) = sdtasdt0(xp, sdtasdt0(xq, sK2))), inference(resolution, [], [f3807, f108])).
fof(f3807, plain, ! [X26] : (~ aInteger0(X26) | (sdtasdt0(sdtasdt0(X26, sK2), xq) = sdtasdt0(X26, sdtasdt0(xq, sK2)))), inference(forward_demodulation, [], [f3794, f1939])).
fof(f1939, plain, (sdtasdt0(xq, sK2) = sdtasdt0(sK2, xq)), inference(resolution, [], [f342, f110])).
fof(f342, plain, ! [X19] : (~ aInteger0(X19) | (sdtasdt0(X19, sK2) = sdtasdt0(sK2, X19))), inference(resolution, [], [f86, f116])).
fof(f86, plain, ! [X0, X1] : (~ aInteger0(X1) | (sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(flattening, [], [f41])).
fof(f41, plain, ! [X0, X1] : ((sdtasdt0(X0, X1) = sdtasdt0(X1, X0)) | (~ aInteger0(X1) | ~ aInteger0(X0))), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0, X1] : ((aInteger0(X1) & aInteger0(X0)) => (sdtasdt0(X0, X1) = sdtasdt0(X1, X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM433+3.p', mMulComm)).
fof(f3794, plain, ! [X26] : ((sdtasdt0(X26, sdtasdt0(sK2, xq)) = sdtasdt0(sdtasdt0(X26, sK2), xq)) | ~ aInteger0(X26)), inference(resolution, [], [f648, f116])).
fof(f648, plain, ! [X21, X22] : (~ aInteger0(X22) | (sdtasdt0(X21, sdtasdt0(X22, xq)) = sdtasdt0(sdtasdt0(X21, X22), xq)) | ~ aInteger0(X21)), inference(resolution, [], [f85, f110])).
fof(f131216, plain, ((sdtasdt0(sK2, sdtasdt0(xp, xq)) = sdtasdt0(sdtasdt0(xp, sK2), xq)) | ~ spl3_77), inference(forward_demodulation, [], [f130592, f1938])).
fof(f1938, plain, (sdtasdt0(xp, sK2) = sdtasdt0(sK2, xp)), inference(resolution, [], [f342, f108])).
fof(f130592, plain, ((sdtasdt0(sK2, sdtasdt0(xp, xq)) = sdtasdt0(sdtasdt0(sK2, xp), xq)) | ~ spl3_77), inference(resolution, [], [f3791, f6834])).
fof(f6834, plain, (aInteger0(sK2) | ~ spl3_77), inference(avatar_component_clause, [], [f6833])).
fof(f6833, plain, (spl3_77 <=> aInteger0(sK2)), introduced(avatar_definition, [new_symbols(naming, [spl3_77])])).
fof(f3791, plain, ! [X21] : (~ aInteger0(X21) | (sdtasdt0(X21, sdtasdt0(xp, xq)) = sdtasdt0(sdtasdt0(X21, xp), xq))), inference(resolution, [], [f648, f108])).
fof(f130054, plain, ((sdtasdt0(sK2, sdtasdt0(xp, xq)) = sdtasdt0(sdtasdt0(xq, sK2), xp)) | ~ spl3_77), inference(forward_demodulation, [], [f129913, f1939])).
fof(f129913, plain, ((sdtasdt0(sK2, sdtasdt0(xp, xq)) = sdtasdt0(sdtasdt0(sK2, xq), xp)) | ~ spl3_77), inference(resolution, [], [f3758, f6834])).
fof(f3758, plain, ! [X22] : (~ aInteger0(X22) | (sdtasdt0(sdtasdt0(X22, xq), xp) = sdtasdt0(X22, sdtasdt0(xp, xq)))), inference(forward_demodulation, [], [f3743, f2053])).
fof(f2053, plain, (sdtasdt0(xp, xq) = sdtasdt0(xq, xp)), inference(resolution, [], [f339, f110])).
fof(f339, plain, ! [X14] : (~ aInteger0(X14) | (sdtasdt0(X14, xp) = sdtasdt0(xp, X14))), inference(resolution, [], [f86, f108])).
fof(f3743, plain, ! [X22] : ((sdtasdt0(X22, sdtasdt0(xq, xp)) = sdtasdt0(sdtasdt0(X22, xq), xp)) | ~ aInteger0(X22)), inference(resolution, [], [f647, f110])).
fof(f647, plain, ! [X19, X20] : (~ aInteger0(X20) | (sdtasdt0(X19, sdtasdt0(X20, xp)) = sdtasdt0(sdtasdt0(X19, X20), xp)) | ~ aInteger0(X19)), inference(resolution, [], [f85, f108])).
fof(f127276, plain, (sdtasdt0(xq, sdtasdt0(xp, sK2)) = sdtasdt0(sdtasdt0(xq, sK2), xp)), inference(resolution, [], [f3759, f110])).
fof(f3759, plain, ! [X26] : (~ aInteger0(X26) | (sdtasdt0(sdtasdt0(X26, sK2), xp) = sdtasdt0(X26, sdtasdt0(xp, sK2)))), inference(forward_demodulation, [], [f3745, f1938])).
fof(f3745, plain, ! [X26] : ((sdtasdt0(X26, sdtasdt0(sK2, xp)) = sdtasdt0(sdtasdt0(X26, sK2), xp)) | ~ aInteger0(X26)), inference(resolution, [], [f647, f116])).
fof(f156, plain, (! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0)) | ~ spl3_7), inference(avatar_component_clause, [], [f155])).
fof(f155, plain, (spl3_7 <=> ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0))), introduced(avatar_definition, [new_symbols(naming, [spl3_7])])).
fof(f386051, plain, (~ spl3_4 | ~ spl3_42), inference(avatar_contradiction_clause, [], [f386050])).
fof(f386050, plain, ($false | (~ spl3_4 | ~ spl3_42)), inference(subsumption_resolution, [], [f386049, f2372])).
fof(f2372, plain, (aInteger0(sdtasdt0(xq, sK2)) | ~ spl3_42), inference(avatar_component_clause, [], [f2371])).
fof(f2371, plain, (spl3_42 <=> aInteger0(sdtasdt0(xq, sK2))), introduced(avatar_definition, [new_symbols(naming, [spl3_42])])).
fof(f386049, plain, (~ aInteger0(sdtasdt0(xq, sK2)) | ~ spl3_4), inference(trivial_inequality_removal, [], [f386045])).
fof(f386045, plain, (~ (sdtpldt0(xa, smndt0(xb)) = sdtpldt0(xa, smndt0(xb))) | ~ aInteger0(sdtasdt0(xq, sK2)) | ~ spl3_4), inference(superposition, [], [f142, f134094])).
fof(f142, plain, (! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) | ~ aInteger0(X0)) | ~ spl3_4), inference(avatar_component_clause, [], [f141])).
fof(f141, plain, (spl3_4 <=> ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) | ~ aInteger0(X0))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f6942, plain, spl3_77, inference(avatar_split_clause, [], [f116, f6833])).
fof(f2399, plain, spl3_42, inference(avatar_contradiction_clause, [], [f2398])).
fof(f2398, plain, ($false | spl3_42), inference(subsumption_resolution, [], [f2397, f110])).
fof(f2397, plain, (~ aInteger0(xq) | spl3_42), inference(subsumption_resolution, [], [f2396, f116])).
fof(f2396, plain, (~ aInteger0(sK2) | ~ aInteger0(xq) | spl3_42), inference(resolution, [], [f2373, f78])).
fof(f78, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f32])).
fof(f32, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(flattening, [], [f31])).
fof(f31, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | (~ aInteger0(X1) | ~ aInteger0(X0))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0, X1] : ((aInteger0(X1) & aInteger0(X0)) => aInteger0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM433+3.p', mIntMult)).
fof(f2373, plain, (~ aInteger0(sdtasdt0(xq, sK2)) | spl3_42), inference(avatar_component_clause, [], [f2371])).
fof(f2283, plain, spl3_40, inference(avatar_contradiction_clause, [], [f2282])).
fof(f2282, plain, ($false | spl3_40), inference(subsumption_resolution, [], [f2281, f108])).
fof(f2281, plain, (~ aInteger0(xp) | spl3_40), inference(subsumption_resolution, [], [f2280, f116])).
fof(f2280, plain, (~ aInteger0(sK2) | ~ aInteger0(xp) | spl3_40), inference(resolution, [], [f2247, f78])).
fof(f2247, plain, (~ aInteger0(sdtasdt0(xp, sK2)) | spl3_40), inference(avatar_component_clause, [], [f2245])).
fof(f157, plain, (spl3_1 | spl3_7), inference(avatar_split_clause, [], [f120, f155, f127])).
fof(f127, plain, (spl3_1 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f120, plain, ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0) | sP0), inference(cnf_transformation, [], [f73])).
fof(f143, plain, (~ spl3_1 | spl3_4), inference(avatar_split_clause, [], [f112, f141, f127])).
fof(f112, plain, ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) | ~ aInteger0(X0) | ~ sP0), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) | ~ aInteger0(X0))) | ~ sP0), inference(rectify, [], [f69])).
fof(f69, plain, ((~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X2] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X2)) | ~ aInteger0(X2))) | ~ sP0), inference(nnf_transformation, [], [f61])).