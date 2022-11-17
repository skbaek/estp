fof(f454, plain, $false, inference(avatar_sat_refutation, [], [f433, f447, f452, f453])).
fof(f453, plain, spl47_8, inference(avatar_split_clause, [], [f390, f444])).
fof(f444, plain, (spl47_8 <=> doDivides0(xu, xb)), introduced(avatar_definition, [new_symbols(naming, [spl47_8])])).
fof(f390, plain, doDivides0(xu, xb), inference(cnf_transformation, [], [f216])).
fof(f216, plain, (doDivides0(xu, xb) & ((xb = sdtasdt0(xu, sK46)) & aElement0(sK46))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK46])], [f66, f215])).
fof(f215, plain, (? [X0] : ((xb = sdtasdt0(xu, X0)) & aElement0(X0)) => ((xb = sdtasdt0(xu, sK46)) & aElement0(sK46))), introduced(choice_axiom, [])).
fof(f66, plain, (doDivides0(xu, xb) & ? [X0] : ((xb = sdtasdt0(xu, X0)) & aElement0(X0))), inference(flattening, [], [f49])).
fof(f49, plain, ~ ~ (doDivides0(xu, xb) & ? [X0] : ((xb = sdtasdt0(xu, X0)) & aElement0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG125+4.p', m__2612)).
fof(f452, plain, spl47_5, inference(avatar_split_clause, [], [f387, f430])).
fof(f430, plain, (spl47_5 <=> doDivides0(xu, xa)), introduced(avatar_definition, [new_symbols(naming, [spl47_5])])).
fof(f387, plain, doDivides0(xu, xa), inference(cnf_transformation, [], [f214])).
fof(f214, plain, (doDivides0(xu, xa) & ((xa = sdtasdt0(xu, sK45)) & aElement0(sK45))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK45])], [f65, f213])).
fof(f213, plain, (? [X0] : ((xa = sdtasdt0(xu, X0)) & aElement0(X0)) => ((xa = sdtasdt0(xu, sK45)) & aElement0(sK45))), introduced(choice_axiom, [])).
fof(f65, plain, (doDivides0(xu, xa) & ? [X0] : ((xa = sdtasdt0(xu, X0)) & aElement0(X0))), inference(flattening, [], [f48])).
fof(f48, plain, ~ ~ (doDivides0(xu, xa) & ? [X0] : ((xa = sdtasdt0(xu, X0)) & aElement0(X0))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG125+4.p', m__2479)).
fof(f447, plain, (spl47_3 | ~ spl47_8), inference(avatar_split_clause, [], [f380, f444, f421])).
fof(f421, plain, (spl47_3 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl47_3])])).
fof(f380, plain, (~ doDivides0(xu, xb) | sP4), inference(cnf_transformation, [], [f131])).
fof(f131, plain, ((~ aDivisorOf0(xu, xb) & ~ doDivides0(xu, xb) & ! [X0] : (~ (xb = sdtasdt0(xu, X0)) | ~ aElement0(X0))) | sP4), inference(definition_folding, [], [f123, e130])).
fof(f130, plain, ((~ aDivisorOf0(xu, xa) & ~ doDivides0(xu, xa) & ! [X1] : (~ (xa = sdtasdt0(xu, X1)) | ~ aElement0(X1))) | ~ sP4), inference(usedef, [], [e130])).
fof(e130, plain, (sP4 <=> (~ aDivisorOf0(xu, xa) & ~ doDivides0(xu, xa) & ! [X1] : (~ (xa = sdtasdt0(xu, X1)) | ~ aElement0(X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f123, plain, ((~ aDivisorOf0(xu, xb) & ~ doDivides0(xu, xb) & ! [X0] : (~ (xb = sdtasdt0(xu, X0)) | ~ aElement0(X0))) | (~ aDivisorOf0(xu, xa) & ~ doDivides0(xu, xa) & ! [X1] : (~ (xa = sdtasdt0(xu, X1)) | ~ aElement0(X1)))), inference(ennf_transformation, [], [f64])).
fof(f64, plain, ~ ((aDivisorOf0(xu, xb) | doDivides0(xu, xb) | ? [X0] : ((xb = sdtasdt0(xu, X0)) & aElement0(X0))) & (aDivisorOf0(xu, xa) | doDivides0(xu, xa) | ? [X1] : ((xa = sdtasdt0(xu, X1)) & aElement0(X1)))), inference(rectify, [], [f46])).
fof(f46, plain, ~ ((aDivisorOf0(xu, xb) | doDivides0(xu, xb) | ? [X0] : ((xb = sdtasdt0(xu, X0)) & aElement0(X0))) & (aDivisorOf0(xu, xa) | doDivides0(xu, xa) | ? [X0] : ((xa = sdtasdt0(xu, X0)) & aElement0(X0)))), file('/home/ubuntu/library/tptp/Problems/RNG/RNG125+4.p', m__2383)).
fof(f433, plain, (~ spl47_3 | ~ spl47_5), inference(avatar_split_clause, [], [f377, f430, f421])).
fof(f377, plain, (~ doDivides0(xu, xa) | ~ sP4), inference(cnf_transformation, [], [f210])).
fof(f210, plain, ((~ aDivisorOf0(xu, xa) & ~ doDivides0(xu, xa) & ! [X0] : (~ (xa = sdtasdt0(xu, X0)) | ~ aElement0(X0))) | ~ sP4), inference(rectify, [], [f209])).
fof(f209, plain, ((~ aDivisorOf0(xu, xa) & ~ doDivides0(xu, xa) & ! [X1] : (~ (xa = sdtasdt0(xu, X1)) | ~ aElement0(X1))) | ~ sP4), inference(nnf_transformation, [], [f130])).