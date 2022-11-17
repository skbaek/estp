fof(f346, plain, $false, inference(avatar_sat_refutation, [], [f148, f162, f338, f345])).
fof(f345, plain, ~ spl3_4, inference(avatar_contradiction_clause, [], [f344])).
fof(f344, plain, ($false | ~ spl3_4), inference(subsumption_resolution, [], [f343, f111])).
fof(f111, plain, aInteger0(xq), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (~ (sz00 = xq) & aInteger0(xq) & ~ (sz00 = xp) & aInteger0(xp) & aInteger0(xb) & aInteger0(xa)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM436+3.p', m__979)).
fof(f343, plain, (~ aInteger0(xq) | ~ spl3_4), inference(subsumption_resolution, [], [f342, f118])).
fof(f118, plain, aInteger0(xm), inference(cnf_transformation, [], [f25])).
fof(f25, plain, ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(sdtasdt0(xp, xq), xm)) & aInteger0(xm)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM436+3.p', m__1032)).
fof(f342, plain, (~ aInteger0(xm) | ~ aInteger0(xq) | ~ spl3_4), inference(resolution, [], [f341, f79])).
fof(f79, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | ~ aInteger0(X1) | ~ aInteger0(X0)), inference(flattening, [], [f34])).
fof(f34, plain, ! [X0, X1] : (aInteger0(sdtasdt0(X0, X1)) | (~ aInteger0(X1) | ~ aInteger0(X0))), inference(ennf_transformation, [], [f6])).
fof(f6, plain, ! [X0, X1] : ((aInteger0(X1) & aInteger0(X0)) => aInteger0(sdtasdt0(X0, X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM436+3.p', mIntMult)).
fof(f341, plain, (~ aInteger0(sdtasdt0(xq, xm)) | ~ spl3_4), inference(trivial_inequality_removal, [], [f340])).
fof(f340, plain, (~ (sdtpldt0(xa, smndt0(xb)) = sdtpldt0(xa, smndt0(xb))) | ~ aInteger0(sdtasdt0(xq, xm)) | ~ spl3_4), inference(superposition, [], [f147, f120])).
fof(f120, plain, (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, sdtasdt0(xq, xm))), inference(cnf_transformation, [], [f26])).
fof(f26, plain, ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, sdtasdt0(xp, xm))) & (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, sdtasdt0(xq, xm)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM436+3.p', m__1071)).
fof(f147, plain, (! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) | ~ aInteger0(X0)) | ~ spl3_4), inference(avatar_component_clause, [], [f146])).
fof(f146, plain, (spl3_4 <=> ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) | ~ aInteger0(X0))), introduced(avatar_definition, [new_symbols(naming, [spl3_4])])).
fof(f338, plain, ~ spl3_7, inference(avatar_contradiction_clause, [], [f337])).
fof(f337, plain, ($false | ~ spl3_7), inference(subsumption_resolution, [], [f336, f109])).
fof(f109, plain, aInteger0(xp), inference(cnf_transformation, [], [f23])).
fof(f336, plain, (~ aInteger0(xp) | ~ spl3_7), inference(subsumption_resolution, [], [f335, f118])).
fof(f335, plain, (~ aInteger0(xm) | ~ aInteger0(xp) | ~ spl3_7), inference(resolution, [], [f334, f79])).
fof(f334, plain, (~ aInteger0(sdtasdt0(xp, xm)) | ~ spl3_7), inference(trivial_inequality_removal, [], [f332])).
fof(f332, plain, (~ (sdtpldt0(xa, smndt0(xb)) = sdtpldt0(xa, smndt0(xb))) | ~ aInteger0(sdtasdt0(xp, xm)) | ~ spl3_7), inference(superposition, [], [f161, f121])).
fof(f121, plain, (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, sdtasdt0(xp, xm))), inference(cnf_transformation, [], [f26])).
fof(f161, plain, (! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0)) | ~ spl3_7), inference(avatar_component_clause, [], [f160])).
fof(f160, plain, (spl3_7 <=> ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0))), introduced(avatar_definition, [new_symbols(naming, [spl3_7])])).
fof(f162, plain, (spl3_1 | spl3_7), inference(avatar_split_clause, [], [f125, f160, f132])).
fof(f132, plain, (spl3_1 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl3_1])])).
fof(f125, plain, ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0) | sP0), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ((~ sdteqdtlpzmzozddtrp0(xa, xb, xq) & ~ aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) & ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0))) | sP0), inference(definition_folding, [], [f62, e63])).
fof(f63, plain, ((~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X1] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X1)) | ~ aInteger0(X1))) | ~ sP0), inference(usedef, [], [e63])).
fof(e63, plain, (sP0 <=> (~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X1] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X1)) | ~ aInteger0(X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f62, plain, ((~ sdteqdtlpzmzozddtrp0(xa, xb, xq) & ~ aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) & ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) | ~ aInteger0(X0))) | (~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X1] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X1)) | ~ aInteger0(X1)))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ~ ((sdteqdtlpzmzozddtrp0(xa, xb, xq) | aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) & aInteger0(X0))) & (sdteqdtlpzmzozddtrp0(xa, xb, xp) | aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) | ? [X1] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X1)) & aInteger0(X1)))), inference(rectify, [], [f28])).
fof(f28, plain, ~ ((sdteqdtlpzmzozddtrp0(xa, xb, xq) | aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) & aInteger0(X0))) & (sdteqdtlpzmzozddtrp0(xa, xb, xp) | aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) & aInteger0(X0)))), inference(negated_conjecture, [], [f27])).
fof(f27, plain, ~ ((sdteqdtlpzmzozddtrp0(xa, xb, xq) | aDivisorOf0(xq, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xq, X0)) & aInteger0(X0))) & (sdteqdtlpzmzozddtrp0(xa, xb, xp) | aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) | ? [X0] : ((sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) & aInteger0(X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM436+3.p', m__)).
fof(f148, plain, (~ spl3_1 | spl3_4), inference(avatar_split_clause, [], [f122, f146, f132])).
fof(f122, plain, ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) | ~ aInteger0(X0) | ~ sP0), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X0] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X0)) | ~ aInteger0(X0))) | ~ sP0), inference(rectify, [], [f73])).
fof(f73, plain, ((~ sdteqdtlpzmzozddtrp0(xa, xb, xp) & ~ aDivisorOf0(xp, sdtpldt0(xa, smndt0(xb))) & ! [X1] : (~ (sdtpldt0(xa, smndt0(xb)) = sdtasdt0(xp, X1)) | ~ aInteger0(X1))) | ~ sP0), inference(nnf_transformation, [], [f63])).