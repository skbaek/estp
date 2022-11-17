fof(f1145, plain, $false, inference(avatar_sat_refutation, [], [f220, f232, f244, f267, f568, f746, f1142])).
fof(f1142, plain, (~ spl18_1 | ~ spl18_2 | ~ spl18_4 | spl18_14 | ~ spl18_15), inference(avatar_contradiction_clause, [], [f1141])).
fof(f1141, plain, ($false | (~ spl18_1 | ~ spl18_2 | ~ spl18_4 | spl18_14 | ~ spl18_15)), inference(subsumption_resolution, [], [f1140, f224])).
fof(f224, plain, (aElement0(sK7(xR)) | ~ spl18_4), inference(avatar_component_clause, [], [f223])).
fof(f223, plain, (spl18_4 <=> aElement0(sK7(xR))), introduced(avatar_definition, [new_symbols(naming, [spl18_4])])).
fof(f1140, plain, (~ aElement0(sK7(xR)) | (~ spl18_1 | ~ spl18_2 | spl18_14 | ~ spl18_15)), inference(subsumption_resolution, [], [f1138, f566])).
fof(f566, plain, (sdtmndtasgtdt0(sK5(xR), xR, sK7(xR)) | ~ spl18_15), inference(avatar_component_clause, [], [f565])).
fof(f565, plain, (spl18_15 <=> sdtmndtasgtdt0(sK5(xR), xR, sK7(xR))), introduced(avatar_definition, [new_symbols(naming, [spl18_15])])).
fof(f1138, plain, (~ sdtmndtasgtdt0(sK5(xR), xR, sK7(xR)) | ~ aElement0(sK7(xR)) | (~ spl18_1 | ~ spl18_2 | spl18_14)), inference(resolution, [], [f563, f264])).
fof(f264, plain, (! [X2] : (sdtmndtasgtdt0(sK6(xR), xR, sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(X2)) | (~ spl18_1 | ~ spl18_2)), inference(subsumption_resolution, [], [f263, f145])).
fof(f145, plain, ~ sP0(xR), inference(subsumption_resolution, [], [f144, f136])).
fof(f136, plain, ~ isConfluent0(xR), inference(cnf_transformation, [], [f24])).
fof(f24, plain, ~ isConfluent0(xR), inference(flattening, [], [f19])).
fof(f19, plain, ~ isConfluent0(xR), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ isConfluent0(xR), file('/home/ubuntu/library/tptp/Problems/COM/COM023+1.p', m__)).
fof(f144, plain, (~ sP0(xR) | isConfluent0(xR)), inference(resolution, [], [f97, f141])).
fof(f141, plain, sP1(xR), inference(resolution, [], [f107, f130])).
fof(f130, plain, aRewritingSystem0(xR), inference(cnf_transformation, [], [f15])).
fof(f15, plain, aRewritingSystem0(xR), file('/home/ubuntu/library/tptp/Problems/COM/COM023+1.p', m__656)).
fof(f107, plain, ! [X0] : (~ aRewritingSystem0(X0) | sP1(X0)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0] : (sP1(X0) | ~ aRewritingSystem0(X0)), inference(definition_folding, [], [f36, e48, e47])).
fof(f47, plain, ! [X0] : (sP0(X0) <=> ! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | ~ sdtmndtasgtdt0(X1, X0, X3) | ~ sdtmndtasgtdt0(X1, X0, X2) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1))), inference(usedef, [], [e47])).
fof(e47, plain, ! [X0] : (sP0(X0) <=> ! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | ~ sdtmndtasgtdt0(X1, X0, X3) | ~ sdtmndtasgtdt0(X1, X0, X2) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f48, plain, ! [X0] : ((isConfluent0(X0) <=> sP0(X0)) | ~ sP1(X0)), inference(usedef, [], [e48])).
fof(e48, plain, ! [X0] : (sP1(X0) <=> (isConfluent0(X0) <=> sP0(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f36, plain, ! [X0] : ((isConfluent0(X0) <=> ! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | ~ sdtmndtasgtdt0(X1, X0, X3) | ~ sdtmndtasgtdt0(X1, X0, X2) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1))) | ~ aRewritingSystem0(X0)), inference(flattening, [], [f35])).
fof(f35, plain, ! [X0] : ((isConfluent0(X0) <=> ! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | (~ sdtmndtasgtdt0(X1, X0, X3) | ~ sdtmndtasgtdt0(X1, X0, X2) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1)))) | ~ aRewritingSystem0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aRewritingSystem0(X0) => (isConfluent0(X0) <=> ! [X1, X2, X3] : ((sdtmndtasgtdt0(X1, X0, X3) & sdtmndtasgtdt0(X1, X0, X2) & aElement0(X3) & aElement0(X2) & aElement0(X1)) => ? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4))))), file('/home/ubuntu/library/tptp/Problems/COM/COM023+1.p', mCRDef)).
fof(f97, plain, ! [X0] : (~ sP1(X0) | ~ sP0(X0) | isConfluent0(X0)), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ! [X0] : (((isConfluent0(X0) | ~ sP0(X0)) & (sP0(X0) | ~ isConfluent0(X0))) | ~ sP1(X0)), inference(nnf_transformation, [], [f48])).
fof(f263, plain, (! [X2] : (sdtmndtasgtdt0(sK6(xR), xR, sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(X2) | sP0(xR)) | (~ spl18_1 | ~ spl18_2)), inference(subsumption_resolution, [], [f262, f211])).
fof(f211, plain, (aElement0(sK5(xR)) | ~ spl18_1), inference(avatar_component_clause, [], [f210])).
fof(f210, plain, (spl18_1 <=> aElement0(sK5(xR))), introduced(avatar_definition, [new_symbols(naming, [spl18_1])])).
fof(f262, plain, (! [X2] : (sdtmndtasgtdt0(sK6(xR), xR, sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(X2) | ~ aElement0(sK5(xR)) | sP0(xR)) | ~ spl18_2), inference(subsumption_resolution, [], [f256, f215])).
fof(f215, plain, (aElement0(sK6(xR)) | ~ spl18_2), inference(avatar_component_clause, [], [f214])).
fof(f214, plain, (spl18_2 <=> aElement0(sK6(xR))), introduced(avatar_definition, [new_symbols(naming, [spl18_2])])).
fof(f256, plain, ! [X2] : (sdtmndtasgtdt0(sK6(xR), xR, sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(sK6(xR)) | ~ aElement0(X2) | ~ aElement0(sK5(xR)) | sP0(xR)), inference(resolution, [], [f135, f104])).
fof(f104, plain, ! [X0] : (sdtmndtasgtdt0(sK5(X0), X0, sK6(X0)) | sP0(X0)), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ! [X0] : ((sP0(X0) | (! [X4] : (~ sdtmndtasgtdt0(sK7(X0), X0, X4) | ~ sdtmndtasgtdt0(sK6(X0), X0, X4) | ~ aElement0(X4)) & sdtmndtasgtdt0(sK5(X0), X0, sK7(X0)) & sdtmndtasgtdt0(sK5(X0), X0, sK6(X0)) & aElement0(sK7(X0)) & aElement0(sK6(X0)) & aElement0(sK5(X0)))) & (! [X5, X6, X7] : ((sdtmndtasgtdt0(X7, X0, sK8(X0, X6, X7)) & sdtmndtasgtdt0(X6, X0, sK8(X0, X6, X7)) & aElement0(sK8(X0, X6, X7))) | ~ sdtmndtasgtdt0(X5, X0, X7) | ~ sdtmndtasgtdt0(X5, X0, X6) | ~ aElement0(X7) | ~ aElement0(X6) | ~ aElement0(X5)) | ~ sP0(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5, sK6, sK7, sK8])], [f62, f64, f63])).
fof(f63, plain, ! [X0] : (? [X1, X2, X3] : (! [X4] : (~ sdtmndtasgtdt0(X3, X0, X4) | ~ sdtmndtasgtdt0(X2, X0, X4) | ~ aElement0(X4)) & sdtmndtasgtdt0(X1, X0, X3) & sdtmndtasgtdt0(X1, X0, X2) & aElement0(X3) & aElement0(X2) & aElement0(X1)) => (! [X4] : (~ sdtmndtasgtdt0(sK7(X0), X0, X4) | ~ sdtmndtasgtdt0(sK6(X0), X0, X4) | ~ aElement0(X4)) & sdtmndtasgtdt0(sK5(X0), X0, sK7(X0)) & sdtmndtasgtdt0(sK5(X0), X0, sK6(X0)) & aElement0(sK7(X0)) & aElement0(sK6(X0)) & aElement0(sK5(X0)))), introduced(choice_axiom, [])).
fof(f64, plain, ! [X7, X6, X0] : (? [X8] : (sdtmndtasgtdt0(X7, X0, X8) & sdtmndtasgtdt0(X6, X0, X8) & aElement0(X8)) => (sdtmndtasgtdt0(X7, X0, sK8(X0, X6, X7)) & sdtmndtasgtdt0(X6, X0, sK8(X0, X6, X7)) & aElement0(sK8(X0, X6, X7)))), introduced(choice_axiom, [])).
fof(f62, plain, ! [X0] : ((sP0(X0) | ? [X1, X2, X3] : (! [X4] : (~ sdtmndtasgtdt0(X3, X0, X4) | ~ sdtmndtasgtdt0(X2, X0, X4) | ~ aElement0(X4)) & sdtmndtasgtdt0(X1, X0, X3) & sdtmndtasgtdt0(X1, X0, X2) & aElement0(X3) & aElement0(X2) & aElement0(X1))) & (! [X5, X6, X7] : (? [X8] : (sdtmndtasgtdt0(X7, X0, X8) & sdtmndtasgtdt0(X6, X0, X8) & aElement0(X8)) | ~ sdtmndtasgtdt0(X5, X0, X7) | ~ sdtmndtasgtdt0(X5, X0, X6) | ~ aElement0(X7) | ~ aElement0(X6) | ~ aElement0(X5)) | ~ sP0(X0))), inference(rectify, [], [f61])).
fof(f61, plain, ! [X0] : ((sP0(X0) | ? [X1, X2, X3] : (! [X4] : (~ sdtmndtasgtdt0(X3, X0, X4) | ~ sdtmndtasgtdt0(X2, X0, X4) | ~ aElement0(X4)) & sdtmndtasgtdt0(X1, X0, X3) & sdtmndtasgtdt0(X1, X0, X2) & aElement0(X3) & aElement0(X2) & aElement0(X1))) & (! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | ~ sdtmndtasgtdt0(X1, X0, X3) | ~ sdtmndtasgtdt0(X1, X0, X2) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1)) | ~ sP0(X0))), inference(nnf_transformation, [], [f47])).
fof(f135, plain, ! [X2, X0, X1] : (~ sdtmndtasgtdt0(X0, xR, X2) | sdtmndtasgtdt0(X2, xR, sK17(X1, X2)) | ~ sdtmndtasgtdt0(X0, xR, X1) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ! [X0, X1, X2] : ((sdtmndtasgtdt0(X2, xR, sK17(X1, X2)) & sdtmndtasgtdt0(X1, xR, sK17(X1, X2)) & aElement0(sK17(X1, X2))) | ~ sdtmndtasgtdt0(X0, xR, X2) | ~ sdtmndtasgtdt0(X0, xR, X1) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK17])], [f46, f83])).
fof(f83, plain, ! [X2, X1] : (? [X3] : (sdtmndtasgtdt0(X2, xR, X3) & sdtmndtasgtdt0(X1, xR, X3) & aElement0(X3)) => (sdtmndtasgtdt0(X2, xR, sK17(X1, X2)) & sdtmndtasgtdt0(X1, xR, sK17(X1, X2)) & aElement0(sK17(X1, X2)))), introduced(choice_axiom, [])).
fof(f46, plain, ! [X0, X1, X2] : (? [X3] : (sdtmndtasgtdt0(X2, xR, X3) & sdtmndtasgtdt0(X1, xR, X3) & aElement0(X3)) | ~ sdtmndtasgtdt0(X0, xR, X2) | ~ sdtmndtasgtdt0(X0, xR, X1) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0)), inference(flattening, [], [f45])).
fof(f45, plain, ! [X0, X1, X2] : (? [X3] : (sdtmndtasgtdt0(X2, xR, X3) & sdtmndtasgtdt0(X1, xR, X3) & aElement0(X3)) | (~ sdtmndtasgtdt0(X0, xR, X2) | ~ sdtmndtasgtdt0(X0, xR, X1) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0))), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ! [X0, X1, X2] : ((sdtmndtasgtdt0(X0, xR, X2) & sdtmndtasgtdt0(X0, xR, X1) & aElement0(X2) & aElement0(X1) & aElement0(X0)) => ? [X3] : (sdtmndtasgtdt0(X2, xR, X3) & sdtmndtasgtdt0(X1, xR, X3) & aElement0(X3))), file('/home/ubuntu/library/tptp/Problems/COM/COM023+1.p', m__715)).
fof(f563, plain, (~ sdtmndtasgtdt0(sK6(xR), xR, sK17(sK7(xR), sK6(xR))) | spl18_14), inference(avatar_component_clause, [], [f561])).
fof(f561, plain, (spl18_14 <=> sdtmndtasgtdt0(sK6(xR), xR, sK17(sK7(xR), sK6(xR)))), introduced(avatar_definition, [new_symbols(naming, [spl18_14])])).
fof(f746, plain, spl18_15, inference(avatar_contradiction_clause, [], [f745])).
fof(f745, plain, ($false | spl18_15), inference(subsumption_resolution, [], [f744, f145])).
fof(f744, plain, (sP0(xR) | spl18_15), inference(resolution, [], [f567, f105])).
fof(f105, plain, ! [X0] : (sdtmndtasgtdt0(sK5(X0), X0, sK7(X0)) | sP0(X0)), inference(cnf_transformation, [], [f65])).
fof(f567, plain, (~ sdtmndtasgtdt0(sK5(xR), xR, sK7(xR)) | spl18_15), inference(avatar_component_clause, [], [f565])).
fof(f568, plain, (~ spl18_14 | ~ spl18_15 | ~ spl18_1 | ~ spl18_2 | ~ spl18_3 | ~ spl18_4), inference(avatar_split_clause, [], [f559, f223, f218, f214, f210, f565, f561])).
fof(f218, plain, (spl18_3 <=> ! [X2] : (aElement0(sK17(X2, sK6(xR))) | ~ aElement0(X2) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2))), introduced(avatar_definition, [new_symbols(naming, [spl18_3])])).
fof(f559, plain, (~ sdtmndtasgtdt0(sK5(xR), xR, sK7(xR)) | ~ sdtmndtasgtdt0(sK6(xR), xR, sK17(sK7(xR), sK6(xR))) | (~ spl18_1 | ~ spl18_2 | ~ spl18_3 | ~ spl18_4)), inference(subsumption_resolution, [], [f558, f359])).
fof(f359, plain, (aElement0(sK17(sK7(xR), sK6(xR))) | (~ spl18_3 | ~ spl18_4)), inference(subsumption_resolution, [], [f358, f145])).
fof(f358, plain, (aElement0(sK17(sK7(xR), sK6(xR))) | sP0(xR) | (~ spl18_3 | ~ spl18_4)), inference(subsumption_resolution, [], [f353, f224])).
fof(f353, plain, (~ aElement0(sK7(xR)) | aElement0(sK17(sK7(xR), sK6(xR))) | sP0(xR) | ~ spl18_3), inference(resolution, [], [f219, f105])).
fof(f219, plain, (! [X2] : (~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(X2) | aElement0(sK17(X2, sK6(xR)))) | ~ spl18_3), inference(avatar_component_clause, [], [f218])).
fof(f558, plain, (~ sdtmndtasgtdt0(sK5(xR), xR, sK7(xR)) | ~ sdtmndtasgtdt0(sK6(xR), xR, sK17(sK7(xR), sK6(xR))) | ~ aElement0(sK17(sK7(xR), sK6(xR))) | (~ spl18_1 | ~ spl18_2 | ~ spl18_4)), inference(subsumption_resolution, [], [f557, f145])).
fof(f557, plain, (~ sdtmndtasgtdt0(sK5(xR), xR, sK7(xR)) | sP0(xR) | ~ sdtmndtasgtdt0(sK6(xR), xR, sK17(sK7(xR), sK6(xR))) | ~ aElement0(sK17(sK7(xR), sK6(xR))) | (~ spl18_1 | ~ spl18_2 | ~ spl18_4)), inference(subsumption_resolution, [], [f517, f224])).
fof(f517, plain, (~ sdtmndtasgtdt0(sK5(xR), xR, sK7(xR)) | ~ aElement0(sK7(xR)) | sP0(xR) | ~ sdtmndtasgtdt0(sK6(xR), xR, sK17(sK7(xR), sK6(xR))) | ~ aElement0(sK17(sK7(xR), sK6(xR))) | (~ spl18_1 | ~ spl18_2)), inference(resolution, [], [f254, f106])).
fof(f106, plain, ! [X4, X0] : (~ sdtmndtasgtdt0(sK7(X0), X0, X4) | sP0(X0) | ~ sdtmndtasgtdt0(sK6(X0), X0, X4) | ~ aElement0(X4)), inference(cnf_transformation, [], [f65])).
fof(f254, plain, (! [X2] : (sdtmndtasgtdt0(X2, xR, sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(X2)) | (~ spl18_1 | ~ spl18_2)), inference(subsumption_resolution, [], [f253, f145])).
fof(f253, plain, (! [X2] : (sdtmndtasgtdt0(X2, xR, sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(X2) | sP0(xR)) | (~ spl18_1 | ~ spl18_2)), inference(subsumption_resolution, [], [f252, f211])).
fof(f252, plain, (! [X2] : (sdtmndtasgtdt0(X2, xR, sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(X2) | ~ aElement0(sK5(xR)) | sP0(xR)) | ~ spl18_2), inference(subsumption_resolution, [], [f246, f215])).
fof(f246, plain, ! [X2] : (sdtmndtasgtdt0(X2, xR, sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(sK6(xR)) | ~ aElement0(X2) | ~ aElement0(sK5(xR)) | sP0(xR)), inference(resolution, [], [f134, f104])).
fof(f134, plain, ! [X2, X0, X1] : (~ sdtmndtasgtdt0(X0, xR, X2) | sdtmndtasgtdt0(X1, xR, sK17(X1, X2)) | ~ sdtmndtasgtdt0(X0, xR, X1) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f84])).
fof(f267, plain, spl18_4, inference(avatar_contradiction_clause, [], [f266])).
fof(f266, plain, ($false | spl18_4), inference(subsumption_resolution, [], [f265, f145])).
fof(f265, plain, (sP0(xR) | spl18_4), inference(resolution, [], [f225, f103])).
fof(f103, plain, ! [X0] : (aElement0(sK7(X0)) | sP0(X0)), inference(cnf_transformation, [], [f65])).
fof(f225, plain, (~ aElement0(sK7(xR)) | spl18_4), inference(avatar_component_clause, [], [f223])).
fof(f244, plain, spl18_2, inference(avatar_contradiction_clause, [], [f243])).
fof(f243, plain, ($false | spl18_2), inference(subsumption_resolution, [], [f242, f145])).
fof(f242, plain, (sP0(xR) | spl18_2), inference(resolution, [], [f216, f102])).
fof(f102, plain, ! [X0] : (aElement0(sK6(X0)) | sP0(X0)), inference(cnf_transformation, [], [f65])).
fof(f216, plain, (~ aElement0(sK6(xR)) | spl18_2), inference(avatar_component_clause, [], [f214])).
fof(f232, plain, spl18_1, inference(avatar_contradiction_clause, [], [f231])).
fof(f231, plain, ($false | spl18_1), inference(subsumption_resolution, [], [f230, f145])).
fof(f230, plain, (sP0(xR) | spl18_1), inference(resolution, [], [f212, f101])).
fof(f101, plain, ! [X0] : (aElement0(sK5(X0)) | sP0(X0)), inference(cnf_transformation, [], [f65])).
fof(f212, plain, (~ aElement0(sK5(xR)) | spl18_1), inference(avatar_component_clause, [], [f210])).
fof(f220, plain, (~ spl18_1 | ~ spl18_2 | spl18_3), inference(avatar_split_clause, [], [f208, f218, f214, f210])).
fof(f208, plain, ! [X2] : (aElement0(sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(sK6(xR)) | ~ aElement0(X2) | ~ aElement0(sK5(xR))), inference(subsumption_resolution, [], [f202, f145])).
fof(f202, plain, ! [X2] : (aElement0(sK17(X2, sK6(xR))) | ~ sdtmndtasgtdt0(sK5(xR), xR, X2) | ~ aElement0(sK6(xR)) | ~ aElement0(X2) | ~ aElement0(sK5(xR)) | sP0(xR)), inference(resolution, [], [f133, f104])).
fof(f133, plain, ! [X2, X0, X1] : (~ sdtmndtasgtdt0(X0, xR, X2) | aElement0(sK17(X1, X2)) | ~ sdtmndtasgtdt0(X0, xR, X1) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f84])).