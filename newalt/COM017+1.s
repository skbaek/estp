fof(f3761, plain, $false, inference(avatar_sat_refutation, [], [f2755, f3209, f3760])).
fof(f3760, plain, spl18_85, inference(avatar_contradiction_clause, [], [f3759])).
fof(f3759, plain, ($false | spl18_85), inference(subsumption_resolution, [], [f3758, f148])).
fof(f148, plain, aElement0(xv), inference(cnf_transformation, [], [f21])).
fof(f21, plain, (sdtmndtasgtdt0(xv, xR, xc) & aReductOfIn0(xv, xa, xR) & aElement0(xv)), file('/home/ubuntu/library/tptp/Problems/COM/COM017+1.p', m__779)).
fof(f3758, plain, (~ aElement0(xv) | spl18_85), inference(subsumption_resolution, [], [f3757, f149])).
fof(f149, plain, aReductOfIn0(xv, xa, xR), inference(cnf_transformation, [], [f21])).
fof(f3757, plain, (~ aReductOfIn0(xv, xa, xR) | ~ aElement0(xv) | spl18_85), inference(resolution, [], [f2754, f704])).
fof(f704, plain, ! [X0] : (sdtmndtasgtdt0(xu, xR, sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0)), inference(subsumption_resolution, [], [f703, f172])).
fof(f172, plain, sP2(xR), inference(subsumption_resolution, [], [f171, f135])).
fof(f135, plain, isLocallyConfluent0(xR), inference(cnf_transformation, [], [f16])).
fof(f16, plain, (isTerminating0(xR) & isLocallyConfluent0(xR)), file('/home/ubuntu/library/tptp/Problems/COM/COM017+1.p', m__656_01)).
fof(f171, plain, (~ isLocallyConfluent0(xR) | sP2(xR)), inference(resolution, [], [f112, f157])).
fof(f157, plain, sP3(xR), inference(resolution, [], [f123, f134])).
fof(f134, plain, aRewritingSystem0(xR), inference(cnf_transformation, [], [f15])).
fof(f15, plain, aRewritingSystem0(xR), file('/home/ubuntu/library/tptp/Problems/COM/COM017+1.p', m__656)).
fof(f123, plain, ! [X0] : (~ aRewritingSystem0(X0) | sP3(X0)), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ! [X0] : (sP3(X0) | ~ aRewritingSystem0(X0)), inference(definition_folding, [], [f41, e55, e54])).
fof(f54, plain, ! [X0] : (sP2(X0) <=> ! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | ~ aReductOfIn0(X3, X1, X0) | ~ aReductOfIn0(X2, X1, X0) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1))), inference(usedef, [], [e54])).
fof(e54, plain, ! [X0] : (sP2(X0) <=> ! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | ~ aReductOfIn0(X3, X1, X0) | ~ aReductOfIn0(X2, X1, X0) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f55, plain, ! [X0] : ((isLocallyConfluent0(X0) <=> sP2(X0)) | ~ sP3(X0)), inference(usedef, [], [e55])).
fof(e55, plain, ! [X0] : (sP3(X0) <=> (isLocallyConfluent0(X0) <=> sP2(X0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f41, plain, ! [X0] : ((isLocallyConfluent0(X0) <=> ! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | ~ aReductOfIn0(X3, X1, X0) | ~ aReductOfIn0(X2, X1, X0) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1))) | ~ aRewritingSystem0(X0)), inference(flattening, [], [f40])).
fof(f40, plain, ! [X0] : ((isLocallyConfluent0(X0) <=> ! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | (~ aReductOfIn0(X3, X1, X0) | ~ aReductOfIn0(X2, X1, X0) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1)))) | ~ aRewritingSystem0(X0)), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : (aRewritingSystem0(X0) => (isLocallyConfluent0(X0) <=> ! [X1, X2, X3] : ((aReductOfIn0(X3, X1, X0) & aReductOfIn0(X2, X1, X0) & aElement0(X3) & aElement0(X2) & aElement0(X1)) => ? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4))))), file('/home/ubuntu/library/tptp/Problems/COM/COM017+1.p', mWCRDef)).
fof(f112, plain, ! [X0] : (~ sP3(X0) | ~ isLocallyConfluent0(X0) | sP2(X0)), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ! [X0] : (((isLocallyConfluent0(X0) | ~ sP2(X0)) & (sP2(X0) | ~ isLocallyConfluent0(X0))) | ~ sP3(X0)), inference(nnf_transformation, [], [f55])).
fof(f703, plain, ! [X0] : (sdtmndtasgtdt0(xu, xR, sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0) | ~ sP2(xR)), inference(subsumption_resolution, [], [f702, f137])).
fof(f137, plain, aElement0(xa), inference(cnf_transformation, [], [f17])).
fof(f17, plain, (aElement0(xc) & aElement0(xb) & aElement0(xa)), file('/home/ubuntu/library/tptp/Problems/COM/COM017+1.p', m__731)).
fof(f702, plain, ! [X0] : (sdtmndtasgtdt0(xu, xR, sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0) | ~ aElement0(xa) | ~ sP2(xR)), inference(subsumption_resolution, [], [f696, f145])).
fof(f145, plain, aElement0(xu), inference(cnf_transformation, [], [f20])).
fof(f20, plain, (sdtmndtasgtdt0(xu, xR, xb) & aReductOfIn0(xu, xa, xR) & aElement0(xu)), file('/home/ubuntu/library/tptp/Problems/COM/COM017+1.p', m__755)).
fof(f696, plain, ! [X0] : (sdtmndtasgtdt0(xu, xR, sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(xu) | ~ aElement0(X0) | ~ aElement0(xa) | ~ sP2(xR)), inference(resolution, [], [f116, f146])).
fof(f146, plain, aReductOfIn0(xu, xa, xR), inference(cnf_transformation, [], [f20])).
fof(f116, plain, ! [X6, X0, X7, X5] : (~ aReductOfIn0(X7, X5, X0) | sdtmndtasgtdt0(X7, X0, sK12(X0, X6, X7)) | ~ aReductOfIn0(X6, X5, X0) | ~ aElement0(X7) | ~ aElement0(X6) | ~ aElement0(X5) | ~ sP2(X0)), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ! [X0] : ((sP2(X0) | (! [X4] : (~ sdtmndtasgtdt0(sK11(X0), X0, X4) | ~ sdtmndtasgtdt0(sK10(X0), X0, X4) | ~ aElement0(X4)) & aReductOfIn0(sK11(X0), sK9(X0), X0) & aReductOfIn0(sK10(X0), sK9(X0), X0) & aElement0(sK11(X0)) & aElement0(sK10(X0)) & aElement0(sK9(X0)))) & (! [X5, X6, X7] : ((sdtmndtasgtdt0(X7, X0, sK12(X0, X6, X7)) & sdtmndtasgtdt0(X6, X0, sK12(X0, X6, X7)) & aElement0(sK12(X0, X6, X7))) | ~ aReductOfIn0(X7, X5, X0) | ~ aReductOfIn0(X6, X5, X0) | ~ aElement0(X7) | ~ aElement0(X6) | ~ aElement0(X5)) | ~ sP2(X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9, sK10, sK11, sK12])], [f72, f74, f73])).
fof(f73, plain, ! [X0] : (? [X1, X2, X3] : (! [X4] : (~ sdtmndtasgtdt0(X3, X0, X4) | ~ sdtmndtasgtdt0(X2, X0, X4) | ~ aElement0(X4)) & aReductOfIn0(X3, X1, X0) & aReductOfIn0(X2, X1, X0) & aElement0(X3) & aElement0(X2) & aElement0(X1)) => (! [X4] : (~ sdtmndtasgtdt0(sK11(X0), X0, X4) | ~ sdtmndtasgtdt0(sK10(X0), X0, X4) | ~ aElement0(X4)) & aReductOfIn0(sK11(X0), sK9(X0), X0) & aReductOfIn0(sK10(X0), sK9(X0), X0) & aElement0(sK11(X0)) & aElement0(sK10(X0)) & aElement0(sK9(X0)))), introduced(choice_axiom, [])).
fof(f74, plain, ! [X7, X6, X0] : (? [X8] : (sdtmndtasgtdt0(X7, X0, X8) & sdtmndtasgtdt0(X6, X0, X8) & aElement0(X8)) => (sdtmndtasgtdt0(X7, X0, sK12(X0, X6, X7)) & sdtmndtasgtdt0(X6, X0, sK12(X0, X6, X7)) & aElement0(sK12(X0, X6, X7)))), introduced(choice_axiom, [])).
fof(f72, plain, ! [X0] : ((sP2(X0) | ? [X1, X2, X3] : (! [X4] : (~ sdtmndtasgtdt0(X3, X0, X4) | ~ sdtmndtasgtdt0(X2, X0, X4) | ~ aElement0(X4)) & aReductOfIn0(X3, X1, X0) & aReductOfIn0(X2, X1, X0) & aElement0(X3) & aElement0(X2) & aElement0(X1))) & (! [X5, X6, X7] : (? [X8] : (sdtmndtasgtdt0(X7, X0, X8) & sdtmndtasgtdt0(X6, X0, X8) & aElement0(X8)) | ~ aReductOfIn0(X7, X5, X0) | ~ aReductOfIn0(X6, X5, X0) | ~ aElement0(X7) | ~ aElement0(X6) | ~ aElement0(X5)) | ~ sP2(X0))), inference(rectify, [], [f71])).
fof(f71, plain, ! [X0] : ((sP2(X0) | ? [X1, X2, X3] : (! [X4] : (~ sdtmndtasgtdt0(X3, X0, X4) | ~ sdtmndtasgtdt0(X2, X0, X4) | ~ aElement0(X4)) & aReductOfIn0(X3, X1, X0) & aReductOfIn0(X2, X1, X0) & aElement0(X3) & aElement0(X2) & aElement0(X1))) & (! [X1, X2, X3] : (? [X4] : (sdtmndtasgtdt0(X3, X0, X4) & sdtmndtasgtdt0(X2, X0, X4) & aElement0(X4)) | ~ aReductOfIn0(X3, X1, X0) | ~ aReductOfIn0(X2, X1, X0) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aElement0(X1)) | ~ sP2(X0))), inference(nnf_transformation, [], [f54])).
fof(f2754, plain, (~ sdtmndtasgtdt0(xu, xR, sK12(xR, xv, xu)) | spl18_85), inference(avatar_component_clause, [], [f2752])).
fof(f2752, plain, (spl18_85 <=> sdtmndtasgtdt0(xu, xR, sK12(xR, xv, xu))), introduced(avatar_definition, [new_symbols(naming, [spl18_85])])).
fof(f3209, plain, spl18_84, inference(avatar_contradiction_clause, [], [f3208])).
fof(f3208, plain, ($false | spl18_84), inference(subsumption_resolution, [], [f3207, f148])).
fof(f3207, plain, (~ aElement0(xv) | spl18_84), inference(subsumption_resolution, [], [f3206, f149])).
fof(f3206, plain, (~ aReductOfIn0(xv, xa, xR) | ~ aElement0(xv) | spl18_84), inference(resolution, [], [f2750, f526])).
fof(f526, plain, ! [X0] : (aElement0(sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0)), inference(subsumption_resolution, [], [f525, f172])).
fof(f525, plain, ! [X0] : (aElement0(sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0) | ~ sP2(xR)), inference(subsumption_resolution, [], [f524, f137])).
fof(f524, plain, ! [X0] : (aElement0(sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0) | ~ aElement0(xa) | ~ sP2(xR)), inference(subsumption_resolution, [], [f518, f145])).
fof(f518, plain, ! [X0] : (aElement0(sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(xu) | ~ aElement0(X0) | ~ aElement0(xa) | ~ sP2(xR)), inference(resolution, [], [f114, f146])).
fof(f114, plain, ! [X6, X0, X7, X5] : (~ aReductOfIn0(X7, X5, X0) | aElement0(sK12(X0, X6, X7)) | ~ aReductOfIn0(X6, X5, X0) | ~ aElement0(X7) | ~ aElement0(X6) | ~ aElement0(X5) | ~ sP2(X0)), inference(cnf_transformation, [], [f75])).
fof(f2750, plain, (~ aElement0(sK12(xR, xv, xu)) | spl18_84), inference(avatar_component_clause, [], [f2748])).
fof(f2748, plain, (spl18_84 <=> aElement0(sK12(xR, xv, xu))), introduced(avatar_definition, [new_symbols(naming, [spl18_84])])).
fof(f2755, plain, (~ spl18_84 | ~ spl18_85), inference(avatar_split_clause, [], [f2746, f2752, f2748])).
fof(f2746, plain, (~ sdtmndtasgtdt0(xu, xR, sK12(xR, xv, xu)) | ~ aElement0(sK12(xR, xv, xu))), inference(subsumption_resolution, [], [f2745, f148])).
fof(f2745, plain, (~ sdtmndtasgtdt0(xu, xR, sK12(xR, xv, xu)) | ~ aElement0(sK12(xR, xv, xu)) | ~ aElement0(xv)), inference(subsumption_resolution, [], [f2735, f149])).
fof(f2735, plain, (~ sdtmndtasgtdt0(xu, xR, sK12(xR, xv, xu)) | ~ aElement0(sK12(xR, xv, xu)) | ~ aReductOfIn0(xv, xa, xR) | ~ aElement0(xv)), inference(resolution, [], [f151, f686])).
fof(f686, plain, ! [X0] : (sdtmndtasgtdt0(X0, xR, sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0)), inference(subsumption_resolution, [], [f685, f172])).
fof(f685, plain, ! [X0] : (sdtmndtasgtdt0(X0, xR, sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0) | ~ sP2(xR)), inference(subsumption_resolution, [], [f684, f137])).
fof(f684, plain, ! [X0] : (sdtmndtasgtdt0(X0, xR, sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(X0) | ~ aElement0(xa) | ~ sP2(xR)), inference(subsumption_resolution, [], [f678, f145])).
fof(f678, plain, ! [X0] : (sdtmndtasgtdt0(X0, xR, sK12(xR, X0, xu)) | ~ aReductOfIn0(X0, xa, xR) | ~ aElement0(xu) | ~ aElement0(X0) | ~ aElement0(xa) | ~ sP2(xR)), inference(resolution, [], [f115, f146])).
fof(f115, plain, ! [X6, X0, X7, X5] : (~ aReductOfIn0(X7, X5, X0) | sdtmndtasgtdt0(X6, X0, sK12(X0, X6, X7)) | ~ aReductOfIn0(X6, X5, X0) | ~ aElement0(X7) | ~ aElement0(X6) | ~ aElement0(X5) | ~ sP2(X0)), inference(cnf_transformation, [], [f75])).
fof(f151, plain, ! [X0] : (~ sdtmndtasgtdt0(xv, xR, X0) | ~ sdtmndtasgtdt0(xu, xR, X0) | ~ aElement0(X0)), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ! [X0] : (~ sdtmndtasgtdt0(xv, xR, X0) | ~ sdtmndtasgtdt0(xu, xR, X0) | ~ aElement0(X0)), inference(ennf_transformation, [], [f23])).
fof(f23, plain, ~ ? [X0] : (sdtmndtasgtdt0(xv, xR, X0) & sdtmndtasgtdt0(xu, xR, X0) & aElement0(X0)), inference(negated_conjecture, [], [f22])).
fof(f22, plain, ~ ? [X0] : (sdtmndtasgtdt0(xv, xR, X0) & sdtmndtasgtdt0(xu, xR, X0) & aElement0(X0)), file('/home/ubuntu/library/tptp/Problems/COM/COM017+1.p', m__)).