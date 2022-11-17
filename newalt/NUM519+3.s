fof(f398, plain, $false, inference(avatar_sat_refutation, [], [f370, f385, f396, f397])).
fof(f397, plain, ~ spl20_5, inference(avatar_split_clause, [], [f336, f382])).
fof(f382, plain, (spl20_5 <=> doDivides0(xr, xm)), introduced(avatar_definition, [new_symbols(naming, [spl20_5])])).
fof(f336, plain, ~ doDivides0(xr, xm), inference(cnf_transformation, [], [f139])).
fof(f139, plain, (~ doDivides0(xr, xm) & ! [X0] : (~ (xm = sdtasdt0(xr, X0)) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f53])).
fof(f53, plain, ~ (doDivides0(xr, xm) | ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM519+3.p', m__2698)).
fof(f396, plain, ~ spl20_2, inference(avatar_split_clause, [], [f334, f367])).
fof(f367, plain, (spl20_2 <=> doDivides0(xr, xn)), introduced(avatar_definition, [new_symbols(naming, [spl20_2])])).
fof(f334, plain, ~ doDivides0(xr, xn), inference(cnf_transformation, [], [f138])).
fof(f138, plain, (~ doDivides0(xr, xn) & ! [X0] : (~ (xn = sdtasdt0(xr, X0)) | ~ aNaturalNumber0(X0))), inference(ennf_transformation, [], [f52])).
fof(f52, plain, ~ (doDivides0(xr, xn) | ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM519+3.p', m__2487)).
fof(f385, plain, (spl20_1 | spl20_5), inference(avatar_split_clause, [], [f332, f382, f363])).
fof(f363, plain, (spl20_1 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl20_1])])).
fof(f332, plain, (doDivides0(xr, xm) | sP2), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ((doDivides0(xr, xm) & ((xm = sdtasdt0(xr, sK19)) & aNaturalNumber0(sK19))) | sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19])], [f145, f193])).
fof(f193, plain, (? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((xm = sdtasdt0(xr, sK19)) & aNaturalNumber0(sK19))), introduced(choice_axiom, [])).
fof(f145, plain, ((doDivides0(xr, xm) & ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))) | sP2), inference(definition_folding, [], [f63, e144])).
fof(f144, plain, ((doDivides0(xr, xn) & ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1))) | ~ sP2), inference(usedef, [], [e144])).
fof(e144, plain, (sP2 <=> (doDivides0(xr, xn) & ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f63, plain, ((doDivides0(xr, xm) & ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))) | (doDivides0(xr, xn) & ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1)))), inference(rectify, [], [f51])).
fof(f51, plain, ((doDivides0(xr, xm) & ? [X0] : ((xm = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))) | (doDivides0(xr, xn) & ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM519+3.p', m__2449)).
fof(f370, plain, (~ spl20_1 | spl20_2), inference(avatar_split_clause, [], [f329, f367, f363])).
fof(f329, plain, (doDivides0(xr, xn) | ~ sP2), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ((doDivides0(xr, xn) & ((xn = sdtasdt0(xr, sK18)) & aNaturalNumber0(sK18))) | ~ sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18])], [f190, f191])).
fof(f191, plain, (? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0)) => ((xn = sdtasdt0(xr, sK18)) & aNaturalNumber0(sK18))), introduced(choice_axiom, [])).
fof(f190, plain, ((doDivides0(xr, xn) & ? [X0] : ((xn = sdtasdt0(xr, X0)) & aNaturalNumber0(X0))) | ~ sP2), inference(rectify, [], [f189])).
fof(f189, plain, ((doDivides0(xr, xn) & ? [X1] : ((xn = sdtasdt0(xr, X1)) & aNaturalNumber0(X1))) | ~ sP2), inference(nnf_transformation, [], [f144])).