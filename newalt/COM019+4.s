fof(f58786, plain, $false, inference(avatar_sat_refutation, [], [f3807, f56472, f56596, f58039])).
fof(f58039, plain, (~ spl34_9 | ~ spl34_133), inference(avatar_contradiction_clause, [], [f58038])).
fof(f58038, plain, ($false | (~ spl34_9 | ~ spl34_133)), inference(subsumption_resolution, [], [f56945, f266])).
fof(f266, plain, ~ sdtmndtasgtdt0(xb, xR, xd), inference(cnf_transformation, [], [f60])).
fof(f60, plain, (~ sdtmndtasgtdt0(xb, xR, xd) & ~ sdtmndtplgtdt0(xb, xR, xd) & ! [X0] : (~ sdtmndtplgtdt0(X0, xR, xd) | ~ aReductOfIn0(X0, xb, xR) | ~ aElement0(X0)) & ~ aReductOfIn0(xd, xb, xR) & ~ (xb = xd)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ~ (sdtmndtasgtdt0(xb, xR, xd) | sdtmndtplgtdt0(xb, xR, xd) | ? [X0] : (sdtmndtplgtdt0(X0, xR, xd) & aReductOfIn0(X0, xb, xR) & aElement0(X0)) | aReductOfIn0(xd, xb, xR) | (xb = xd)), inference(negated_conjecture, [], [f24])).
fof(f24, plain, ~ (sdtmndtasgtdt0(xb, xR, xd) | sdtmndtplgtdt0(xb, xR, xd) | ? [X0] : (sdtmndtplgtdt0(X0, xR, xd) & aReductOfIn0(X0, xb, xR) & aElement0(X0)) | aReductOfIn0(xd, xb, xR) | (xb = xd)), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', m__)).
fof(f56945, plain, (sdtmndtasgtdt0(xb, xR, xd) | (~ spl34_9 | ~ spl34_133)), inference(backward_demodulation, [], [f17695, f315])).
fof(f315, plain, ((xb = xu) | ~ spl34_9), inference(avatar_component_clause, [], [f313])).
fof(f313, plain, (spl34_9 <=> (xb = xu)), introduced(avatar_definition, [new_symbols(naming, [spl34_9])])).
fof(f17695, plain, (sdtmndtasgtdt0(xu, xR, xd) | ~ spl34_133), inference(subsumption_resolution, [], [f17681, f229])).
fof(f229, plain, aElement0(xu), inference(cnf_transformation, [], [f125])).
fof(f125, plain, (sdtmndtasgtdt0(xu, xR, xb) & ((sdtmndtplgtdt0(xu, xR, xb) & ((sdtmndtplgtdt0(sK29, xR, xb) & aReductOfIn0(sK29, xu, xR) & aElement0(sK29)) | aReductOfIn0(xb, xu, xR))) | (xb = xu)) & aReductOfIn0(xu, xa, xR) & aElement0(xu)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK29])], [f20, f124])).
fof(f124, plain, (? [X0] : (sdtmndtplgtdt0(X0, xR, xb) & aReductOfIn0(X0, xu, xR) & aElement0(X0)) => (sdtmndtplgtdt0(sK29, xR, xb) & aReductOfIn0(sK29, xu, xR) & aElement0(sK29))), introduced(choice_axiom, [])).
fof(f20, plain, (sdtmndtasgtdt0(xu, xR, xb) & ((sdtmndtplgtdt0(xu, xR, xb) & (? [X0] : (sdtmndtplgtdt0(X0, xR, xb) & aReductOfIn0(X0, xu, xR) & aElement0(X0)) | aReductOfIn0(xb, xu, xR))) | (xb = xu)) & aReductOfIn0(xu, xa, xR) & aElement0(xu)), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', m__755)).
fof(f17681, plain, (sdtmndtasgtdt0(xu, xR, xd) | ~ aElement0(xu) | ~ spl34_133), inference(resolution, [], [f17412, f248])).
fof(f248, plain, sdtmndtasgtdt0(xu, xR, xw), inference(cnf_transformation, [], [f130])).
fof(f130, plain, (sdtmndtasgtdt0(xv, xR, xw) & ((sdtmndtplgtdt0(xv, xR, xw) & ((sdtmndtplgtdt0(sK31, xR, xw) & aReductOfIn0(sK31, xv, xR) & aElement0(sK31)) | aReductOfIn0(xw, xv, xR))) | (xv = xw)) & sdtmndtasgtdt0(xu, xR, xw) & ((sdtmndtplgtdt0(xu, xR, xw) & ((sdtmndtplgtdt0(sK32, xR, xw) & aReductOfIn0(sK32, xu, xR) & aElement0(sK32)) | aReductOfIn0(xw, xu, xR))) | (xu = xw)) & aElement0(xw)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK31, sK32])], [f33, f129, f128])).
fof(f128, plain, (? [X0] : (sdtmndtplgtdt0(X0, xR, xw) & aReductOfIn0(X0, xv, xR) & aElement0(X0)) => (sdtmndtplgtdt0(sK31, xR, xw) & aReductOfIn0(sK31, xv, xR) & aElement0(sK31))), introduced(choice_axiom, [])).
fof(f129, plain, (? [X1] : (sdtmndtplgtdt0(X1, xR, xw) & aReductOfIn0(X1, xu, xR) & aElement0(X1)) => (sdtmndtplgtdt0(sK32, xR, xw) & aReductOfIn0(sK32, xu, xR) & aElement0(sK32))), introduced(choice_axiom, [])).
fof(f33, plain, (sdtmndtasgtdt0(xv, xR, xw) & ((sdtmndtplgtdt0(xv, xR, xw) & (? [X0] : (sdtmndtplgtdt0(X0, xR, xw) & aReductOfIn0(X0, xv, xR) & aElement0(X0)) | aReductOfIn0(xw, xv, xR))) | (xv = xw)) & sdtmndtasgtdt0(xu, xR, xw) & ((sdtmndtplgtdt0(xu, xR, xw) & (? [X1] : (sdtmndtplgtdt0(X1, xR, xw) & aReductOfIn0(X1, xu, xR) & aElement0(X1)) | aReductOfIn0(xw, xu, xR))) | (xu = xw)) & aElement0(xw)), inference(rectify, [], [f22])).
fof(f22, plain, (sdtmndtasgtdt0(xv, xR, xw) & ((sdtmndtplgtdt0(xv, xR, xw) & (? [X0] : (sdtmndtplgtdt0(X0, xR, xw) & aReductOfIn0(X0, xv, xR) & aElement0(X0)) | aReductOfIn0(xw, xv, xR))) | (xv = xw)) & sdtmndtasgtdt0(xu, xR, xw) & ((sdtmndtplgtdt0(xu, xR, xw) & (? [X0] : (sdtmndtplgtdt0(X0, xR, xw) & aReductOfIn0(X0, xu, xR) & aElement0(X0)) | aReductOfIn0(xw, xu, xR))) | (xu = xw)) & aElement0(xw)), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', m__799)).
fof(f17412, plain, (! [X4] : (~ sdtmndtasgtdt0(X4, xR, xw) | sdtmndtasgtdt0(X4, xR, xd) | ~ aElement0(X4)) | ~ spl34_133), inference(subsumption_resolution, [], [f17411, f178])).
fof(f178, plain, aRewritingSystem0(xR), inference(cnf_transformation, [], [f15])).
fof(f15, plain, aRewritingSystem0(xR), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', m__656)).
fof(f17411, plain, (! [X4] : (sdtmndtasgtdt0(X4, xR, xd) | ~ sdtmndtasgtdt0(X4, xR, xw) | ~ aRewritingSystem0(xR) | ~ aElement0(X4)) | ~ spl34_133), inference(subsumption_resolution, [], [f17410, f3753])).
fof(f3753, plain, (aElement0(xw) | ~ spl34_133), inference(avatar_component_clause, [], [f3752])).
fof(f3752, plain, (spl34_133 <=> aElement0(xw)), introduced(avatar_definition, [new_symbols(naming, [spl34_133])])).
fof(f17410, plain, ! [X4] : (sdtmndtasgtdt0(X4, xR, xd) | ~ sdtmndtasgtdt0(X4, xR, xw) | ~ aElement0(xw) | ~ aRewritingSystem0(xR) | ~ aElement0(X4)), inference(subsumption_resolution, [], [f17396, f254])).
fof(f254, plain, aElement0(xd), inference(cnf_transformation, [], [f132])).
fof(f132, plain, (aNormalFormOfIn0(xd, xw, xR) & ! [X0] : ~ aReductOfIn0(X0, xd, xR) & sdtmndtasgtdt0(xw, xR, xd) & ((sdtmndtplgtdt0(xw, xR, xd) & ((sdtmndtplgtdt0(sK33, xR, xd) & aReductOfIn0(sK33, xw, xR) & aElement0(sK33)) | aReductOfIn0(xd, xw, xR))) | (xw = xd)) & aElement0(xd)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK33])], [f59, f131])).
fof(f131, plain, (? [X1] : (sdtmndtplgtdt0(X1, xR, xd) & aReductOfIn0(X1, xw, xR) & aElement0(X1)) => (sdtmndtplgtdt0(sK33, xR, xd) & aReductOfIn0(sK33, xw, xR) & aElement0(sK33))), introduced(choice_axiom, [])).
fof(f59, plain, (aNormalFormOfIn0(xd, xw, xR) & ! [X0] : ~ aReductOfIn0(X0, xd, xR) & sdtmndtasgtdt0(xw, xR, xd) & ((sdtmndtplgtdt0(xw, xR, xd) & (? [X1] : (sdtmndtplgtdt0(X1, xR, xd) & aReductOfIn0(X1, xw, xR) & aElement0(X1)) | aReductOfIn0(xd, xw, xR))) | (xw = xd)) & aElement0(xd)), inference(ennf_transformation, [], [f34])).
fof(f34, plain, (aNormalFormOfIn0(xd, xw, xR) & ~ ? [X0] : aReductOfIn0(X0, xd, xR) & sdtmndtasgtdt0(xw, xR, xd) & ((sdtmndtplgtdt0(xw, xR, xd) & (? [X1] : (sdtmndtplgtdt0(X1, xR, xd) & aReductOfIn0(X1, xw, xR) & aElement0(X1)) | aReductOfIn0(xd, xw, xR))) | (xw = xd)) & aElement0(xd)), inference(rectify, [], [f23])).
fof(f23, plain, (aNormalFormOfIn0(xd, xw, xR) & ~ ? [X0] : aReductOfIn0(X0, xd, xR) & sdtmndtasgtdt0(xw, xR, xd) & ((sdtmndtplgtdt0(xw, xR, xd) & (? [X0] : (sdtmndtplgtdt0(X0, xR, xd) & aReductOfIn0(X0, xw, xR) & aElement0(X0)) | aReductOfIn0(xd, xw, xR))) | (xw = xd)) & aElement0(xd)), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', m__818)).
fof(f17396, plain, ! [X4] : (sdtmndtasgtdt0(X4, xR, xd) | ~ sdtmndtasgtdt0(X4, xR, xw) | ~ aElement0(xd) | ~ aElement0(xw) | ~ aRewritingSystem0(xR) | ~ aElement0(X4)), inference(resolution, [], [f259, f143])).
fof(f143, plain, ! [X2, X0, X3, X1] : (~ sdtmndtasgtdt0(X2, X1, X3) | sdtmndtasgtdt0(X0, X1, X3) | ~ sdtmndtasgtdt0(X0, X1, X2) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aRewritingSystem0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ! [X0, X1, X2, X3] : (sdtmndtasgtdt0(X0, X1, X3) | ~ sdtmndtasgtdt0(X2, X1, X3) | ~ sdtmndtasgtdt0(X0, X1, X2) | ~ aElement0(X3) | ~ aElement0(X2) | ~ aRewritingSystem0(X1) | ~ aElement0(X0)), inference(flattening, [], [f43])).
fof(f43, plain, ! [X0, X1, X2, X3] : ((sdtmndtasgtdt0(X0, X1, X3) | (~ sdtmndtasgtdt0(X2, X1, X3) | ~ sdtmndtasgtdt0(X0, X1, X2))) | (~ aElement0(X3) | ~ aElement0(X2) | ~ aRewritingSystem0(X1) | ~ aElement0(X0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1, X2, X3] : ((aElement0(X3) & aElement0(X2) & aRewritingSystem0(X1) & aElement0(X0)) => ((sdtmndtasgtdt0(X2, X1, X3) & sdtmndtasgtdt0(X0, X1, X2)) => sdtmndtasgtdt0(X0, X1, X3))), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', mTCRTrans)).
fof(f259, plain, sdtmndtasgtdt0(xw, xR, xd), inference(cnf_transformation, [], [f132])).
fof(f56596, plain, (spl34_10 | spl34_9), inference(avatar_split_clause, [], [f56595, f313, f317])).
fof(f317, plain, (spl34_10 <=> sdtmndtplgtdt0(xu, xR, xb)), introduced(avatar_definition, [new_symbols(naming, [spl34_10])])).
fof(f56595, plain, ((xb = xu) | sdtmndtplgtdt0(xu, xR, xb)), inference(subsumption_resolution, [], [f56594, f229])).
fof(f56594, plain, ((xb = xu) | sdtmndtplgtdt0(xu, xR, xb) | ~ aElement0(xu)), inference(subsumption_resolution, [], [f56593, f178])).
fof(f56593, plain, ((xb = xu) | sdtmndtplgtdt0(xu, xR, xb) | ~ aRewritingSystem0(xR) | ~ aElement0(xu)), inference(subsumption_resolution, [], [f982, f197])).
fof(f197, plain, aElement0(xb), inference(cnf_transformation, [], [f17])).
fof(f17, plain, (aElement0(xc) & aElement0(xb) & aElement0(xa)), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', m__731)).
fof(f982, plain, ((xb = xu) | sdtmndtplgtdt0(xu, xR, xb) | ~ aElement0(xb) | ~ aRewritingSystem0(xR) | ~ aElement0(xu)), inference(resolution, [], [f140, f235])).
fof(f235, plain, sdtmndtasgtdt0(xu, xR, xb), inference(cnf_transformation, [], [f125])).
fof(f140, plain, ! [X2, X0, X1] : (~ sdtmndtasgtdt0(X0, X1, X2) | (X0 = X2) | sdtmndtplgtdt0(X0, X1, X2) | ~ aElement0(X2) | ~ aRewritingSystem0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ! [X0, X1, X2] : (((sdtmndtasgtdt0(X0, X1, X2) | (~ sdtmndtplgtdt0(X0, X1, X2) & ~ (X0 = X2))) & (sdtmndtplgtdt0(X0, X1, X2) | (X0 = X2) | ~ sdtmndtasgtdt0(X0, X1, X2))) | ~ aElement0(X2) | ~ aRewritingSystem0(X1) | ~ aElement0(X0)), inference(flattening, [], [f78])).
fof(f78, plain, ! [X0, X1, X2] : (((sdtmndtasgtdt0(X0, X1, X2) | (~ sdtmndtplgtdt0(X0, X1, X2) & ~ (X0 = X2))) & ((sdtmndtplgtdt0(X0, X1, X2) | (X0 = X2)) | ~ sdtmndtasgtdt0(X0, X1, X2))) | ~ aElement0(X2) | ~ aRewritingSystem0(X1) | ~ aElement0(X0)), inference(nnf_transformation, [], [f42])).
fof(f42, plain, ! [X0, X1, X2] : ((sdtmndtasgtdt0(X0, X1, X2) <=> (sdtmndtplgtdt0(X0, X1, X2) | (X0 = X2))) | ~ aElement0(X2) | ~ aRewritingSystem0(X1) | ~ aElement0(X0)), inference(flattening, [], [f41])).
fof(f41, plain, ! [X0, X1, X2] : ((sdtmndtasgtdt0(X0, X1, X2) <=> (sdtmndtplgtdt0(X0, X1, X2) | (X0 = X2))) | (~ aElement0(X2) | ~ aRewritingSystem0(X1) | ~ aElement0(X0))), inference(ennf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1, X2] : ((aElement0(X2) & aRewritingSystem0(X1) & aElement0(X0)) => (sdtmndtasgtdt0(X0, X1, X2) <=> (sdtmndtplgtdt0(X0, X1, X2) | (X0 = X2)))), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', mTCRDef)).
fof(f56472, plain, (~ spl34_10 | ~ spl34_133), inference(avatar_contradiction_clause, [], [f56471])).
fof(f56471, plain, ($false | (~ spl34_10 | ~ spl34_133)), inference(subsumption_resolution, [], [f56470, f17879])).
fof(f17879, plain, (sP6(xb, xd) | (~ spl34_10 | ~ spl34_133)), inference(subsumption_resolution, [], [f17875, f254])).
fof(f17875, plain, (sP6(xb, xd) | ~ aElement0(xd) | (~ spl34_10 | ~ spl34_133)), inference(resolution, [], [f17825, f13609])).
fof(f13609, plain, (! [X2] : (sP7(X2, xu) | sP6(xb, X2) | ~ aElement0(X2)) | ~ spl34_10), inference(subsumption_resolution, [], [f13608, f229])).
fof(f13608, plain, (! [X2] : (sP6(xb, X2) | sP7(X2, xu) | ~ aElement0(X2) | ~ aElement0(xu)) | ~ spl34_10), inference(subsumption_resolution, [], [f13607, f197])).
fof(f13607, plain, (! [X2] : (sP6(xb, X2) | sP7(X2, xu) | ~ aElement0(xb) | ~ aElement0(X2) | ~ aElement0(xu)) | ~ spl34_10), inference(subsumption_resolution, [], [f4872, f664])).
fof(f664, plain, iLess0(xu, xa), inference(subsumption_resolution, [], [f663, f196])).
fof(f196, plain, aElement0(xa), inference(cnf_transformation, [], [f17])).
fof(f663, plain, (iLess0(xu, xa) | ~ aElement0(xa)), inference(subsumption_resolution, [], [f652, f229])).
fof(f652, plain, (iLess0(xu, xa) | ~ aElement0(xu) | ~ aElement0(xa)), inference(resolution, [], [f192, f230])).
fof(f230, plain, aReductOfIn0(xu, xa, xR), inference(cnf_transformation, [], [f125])).
fof(f192, plain, ! [X0, X1] : (~ aReductOfIn0(X1, X0, xR) | iLess0(X1, X0) | ~ aElement0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, (isTerminating0(xR) & ! [X0, X1] : (iLess0(X1, X0) | (~ sdtmndtplgtdt0(X0, xR, X1) & ! [X2] : (~ sdtmndtplgtdt0(X2, xR, X1) | ~ aReductOfIn0(X2, X0, xR) | ~ aElement0(X2)) & ~ aReductOfIn0(X1, X0, xR)) | ~ aElement0(X1) | ~ aElement0(X0)) & isLocallyConfluent0(xR) & ! [X3, X4, X5] : ((sdtmndtasgtdt0(X5, xR, sK22(X4, X5)) & ((sdtmndtplgtdt0(X5, xR, sK22(X4, X5)) & ((sdtmndtplgtdt0(sK23(X4, X5), xR, sK22(X4, X5)) & aReductOfIn0(sK23(X4, X5), X5, xR) & aElement0(sK23(X4, X5))) | aReductOfIn0(sK22(X4, X5), X5, xR))) | (sK22(X4, X5) = X5)) & sdtmndtasgtdt0(X4, xR, sK22(X4, X5)) & sP4(sK22(X4, X5), X4) & aElement0(sK22(X4, X5))) | ~ aReductOfIn0(X5, X3, xR) | ~ aReductOfIn0(X4, X3, xR) | ~ aElement0(X5) | ~ aElement0(X4) | ~ aElement0(X3))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK22, sK23])], [f68, f108, f107])).
fof(f107, plain, ! [X5, X4] : (? [X6] : (sdtmndtasgtdt0(X5, xR, X6) & ((sdtmndtplgtdt0(X5, xR, X6) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X6) & aReductOfIn0(X7, X5, xR) & aElement0(X7)) | aReductOfIn0(X6, X5, xR))) | (X5 = X6)) & sdtmndtasgtdt0(X4, xR, X6) & sP4(X6, X4) & aElement0(X6)) => (sdtmndtasgtdt0(X5, xR, sK22(X4, X5)) & ((sdtmndtplgtdt0(X5, xR, sK22(X4, X5)) & (? [X7] : (sdtmndtplgtdt0(X7, xR, sK22(X4, X5)) & aReductOfIn0(X7, X5, xR) & aElement0(X7)) | aReductOfIn0(sK22(X4, X5), X5, xR))) | (sK22(X4, X5) = X5)) & sdtmndtasgtdt0(X4, xR, sK22(X4, X5)) & sP4(sK22(X4, X5), X4) & aElement0(sK22(X4, X5)))), introduced(choice_axiom, [])).
fof(f108, plain, ! [X5, X4] : (? [X7] : (sdtmndtplgtdt0(X7, xR, sK22(X4, X5)) & aReductOfIn0(X7, X5, xR) & aElement0(X7)) => (sdtmndtplgtdt0(sK23(X4, X5), xR, sK22(X4, X5)) & aReductOfIn0(sK23(X4, X5), X5, xR) & aElement0(sK23(X4, X5)))), introduced(choice_axiom, [])).
fof(f68, plain, (isTerminating0(xR) & ! [X0, X1] : (iLess0(X1, X0) | (~ sdtmndtplgtdt0(X0, xR, X1) & ! [X2] : (~ sdtmndtplgtdt0(X2, xR, X1) | ~ aReductOfIn0(X2, X0, xR) | ~ aElement0(X2)) & ~ aReductOfIn0(X1, X0, xR)) | ~ aElement0(X1) | ~ aElement0(X0)) & isLocallyConfluent0(xR) & ! [X3, X4, X5] : (? [X6] : (sdtmndtasgtdt0(X5, xR, X6) & ((sdtmndtplgtdt0(X5, xR, X6) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X6) & aReductOfIn0(X7, X5, xR) & aElement0(X7)) | aReductOfIn0(X6, X5, xR))) | (X5 = X6)) & sdtmndtasgtdt0(X4, xR, X6) & sP4(X6, X4) & aElement0(X6)) | ~ aReductOfIn0(X5, X3, xR) | ~ aReductOfIn0(X4, X3, xR) | ~ aElement0(X5) | ~ aElement0(X4) | ~ aElement0(X3))), inference(definition_folding, [], [f56, e67])).
fof(f67, plain, ! [X6, X4] : ((sdtmndtplgtdt0(X4, xR, X6) & (? [X8] : (sdtmndtplgtdt0(X8, xR, X6) & aReductOfIn0(X8, X4, xR) & aElement0(X8)) | aReductOfIn0(X6, X4, xR))) | (X4 = X6) | ~ sP4(X6, X4)), inference(usedef, [], [e67])).
fof(e67, plain, ! [X6, X4] : (sP4(X6, X4) <=> ((sdtmndtplgtdt0(X4, xR, X6) & (? [X8] : (sdtmndtplgtdt0(X8, xR, X6) & aReductOfIn0(X8, X4, xR) & aElement0(X8)) | aReductOfIn0(X6, X4, xR))) | (X4 = X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f56, plain, (isTerminating0(xR) & ! [X0, X1] : (iLess0(X1, X0) | (~ sdtmndtplgtdt0(X0, xR, X1) & ! [X2] : (~ sdtmndtplgtdt0(X2, xR, X1) | ~ aReductOfIn0(X2, X0, xR) | ~ aElement0(X2)) & ~ aReductOfIn0(X1, X0, xR)) | ~ aElement0(X1) | ~ aElement0(X0)) & isLocallyConfluent0(xR) & ! [X3, X4, X5] : (? [X6] : (sdtmndtasgtdt0(X5, xR, X6) & ((sdtmndtplgtdt0(X5, xR, X6) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X6) & aReductOfIn0(X7, X5, xR) & aElement0(X7)) | aReductOfIn0(X6, X5, xR))) | (X5 = X6)) & sdtmndtasgtdt0(X4, xR, X6) & ((sdtmndtplgtdt0(X4, xR, X6) & (? [X8] : (sdtmndtplgtdt0(X8, xR, X6) & aReductOfIn0(X8, X4, xR) & aElement0(X8)) | aReductOfIn0(X6, X4, xR))) | (X4 = X6)) & aElement0(X6)) | ~ aReductOfIn0(X5, X3, xR) | ~ aReductOfIn0(X4, X3, xR) | ~ aElement0(X5) | ~ aElement0(X4) | ~ aElement0(X3))), inference(flattening, [], [f55])).
fof(f55, plain, (isTerminating0(xR) & ! [X0, X1] : ((iLess0(X1, X0) | (~ sdtmndtplgtdt0(X0, xR, X1) & ! [X2] : (~ sdtmndtplgtdt0(X2, xR, X1) | ~ aReductOfIn0(X2, X0, xR) | ~ aElement0(X2)) & ~ aReductOfIn0(X1, X0, xR))) | (~ aElement0(X1) | ~ aElement0(X0))) & isLocallyConfluent0(xR) & ! [X3, X4, X5] : (? [X6] : (sdtmndtasgtdt0(X5, xR, X6) & ((sdtmndtplgtdt0(X5, xR, X6) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X6) & aReductOfIn0(X7, X5, xR) & aElement0(X7)) | aReductOfIn0(X6, X5, xR))) | (X5 = X6)) & sdtmndtasgtdt0(X4, xR, X6) & ((sdtmndtplgtdt0(X4, xR, X6) & (? [X8] : (sdtmndtplgtdt0(X8, xR, X6) & aReductOfIn0(X8, X4, xR) & aElement0(X8)) | aReductOfIn0(X6, X4, xR))) | (X4 = X6)) & aElement0(X6)) | (~ aReductOfIn0(X5, X3, xR) | ~ aReductOfIn0(X4, X3, xR) | ~ aElement0(X5) | ~ aElement0(X4) | ~ aElement0(X3)))), inference(ennf_transformation, [], [f30])).
fof(f30, plain, (isTerminating0(xR) & ! [X0, X1] : ((aElement0(X1) & aElement0(X0)) => ((sdtmndtplgtdt0(X0, xR, X1) | ? [X2] : (sdtmndtplgtdt0(X2, xR, X1) & aReductOfIn0(X2, X0, xR) & aElement0(X2)) | aReductOfIn0(X1, X0, xR)) => iLess0(X1, X0))) & isLocallyConfluent0(xR) & ! [X3, X4, X5] : ((aReductOfIn0(X5, X3, xR) & aReductOfIn0(X4, X3, xR) & aElement0(X5) & aElement0(X4) & aElement0(X3)) => ? [X6] : (sdtmndtasgtdt0(X5, xR, X6) & ((sdtmndtplgtdt0(X5, xR, X6) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X6) & aReductOfIn0(X7, X5, xR) & aElement0(X7)) | aReductOfIn0(X6, X5, xR))) | (X5 = X6)) & sdtmndtasgtdt0(X4, xR, X6) & ((sdtmndtplgtdt0(X4, xR, X6) & (? [X8] : (sdtmndtplgtdt0(X8, xR, X6) & aReductOfIn0(X8, X4, xR) & aElement0(X8)) | aReductOfIn0(X6, X4, xR))) | (X4 = X6)) & aElement0(X6)))), inference(rectify, [], [f16])).
fof(f16, plain, (isTerminating0(xR) & ! [X0, X1] : ((aElement0(X1) & aElement0(X0)) => ((sdtmndtplgtdt0(X0, xR, X1) | ? [X2] : (sdtmndtplgtdt0(X2, xR, X1) & aReductOfIn0(X2, X0, xR) & aElement0(X2)) | aReductOfIn0(X1, X0, xR)) => iLess0(X1, X0))) & isLocallyConfluent0(xR) & ! [X0, X1, X2] : ((aReductOfIn0(X2, X0, xR) & aReductOfIn0(X1, X0, xR) & aElement0(X2) & aElement0(X1) & aElement0(X0)) => ? [X3] : (sdtmndtasgtdt0(X2, xR, X3) & ((sdtmndtplgtdt0(X2, xR, X3) & (? [X4] : (sdtmndtplgtdt0(X4, xR, X3) & aReductOfIn0(X4, X2, xR) & aElement0(X4)) | aReductOfIn0(X3, X2, xR))) | (X2 = X3)) & sdtmndtasgtdt0(X1, xR, X3) & ((sdtmndtplgtdt0(X1, xR, X3) & (? [X4] : (sdtmndtplgtdt0(X4, xR, X3) & aReductOfIn0(X4, X1, xR) & aElement0(X4)) | aReductOfIn0(X3, X1, xR))) | (X1 = X3)) & aElement0(X3)))), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', m__656_01)).
fof(f4872, plain, (! [X2] : (~ iLess0(xu, xa) | sP6(xb, X2) | sP7(X2, xu) | ~ aElement0(xb) | ~ aElement0(X2) | ~ aElement0(xu)) | ~ spl34_10), inference(resolution, [], [f319, f219])).
fof(f219, plain, ! [X2, X0, X1] : (~ sdtmndtplgtdt0(X0, xR, X2) | ~ iLess0(X0, xa) | sP6(X2, X1) | sP7(X1, X0) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ! [X0, X1, X2] : (sP6(X2, X1) | ~ iLess0(X0, xa) | (~ sdtmndtasgtdt0(X0, xR, X2) & ~ sdtmndtplgtdt0(X0, xR, X2) & ! [X3] : (~ sdtmndtplgtdt0(X3, xR, X2) | ~ aReductOfIn0(X3, X0, xR) | ~ aElement0(X3)) & ~ aReductOfIn0(X2, X0, xR) & ~ (X0 = X2)) | sP7(X1, X0) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0)), inference(definition_folding, [], [f58, e71, e70, e69])).
fof(f69, plain, ! [X5, X1] : ((sdtmndtplgtdt0(X1, xR, X5) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X5) & aReductOfIn0(X7, X1, xR) & aElement0(X7)) | aReductOfIn0(X5, X1, xR))) | (X1 = X5) | ~ sP5(X5, X1)), inference(usedef, [], [e69])).
fof(e69, plain, ! [X5, X1] : (sP5(X5, X1) <=> ((sdtmndtplgtdt0(X1, xR, X5) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X5) & aReductOfIn0(X7, X1, xR) & aElement0(X7)) | aReductOfIn0(X5, X1, xR))) | (X1 = X5))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f70, plain, ! [X2, X1] : (? [X5] : (sdtmndtasgtdt0(X2, xR, X5) & ((sdtmndtplgtdt0(X2, xR, X5) & (? [X6] : (sdtmndtplgtdt0(X6, xR, X5) & aReductOfIn0(X6, X2, xR) & aElement0(X6)) | aReductOfIn0(X5, X2, xR))) | (X2 = X5)) & sdtmndtasgtdt0(X1, xR, X5) & sP5(X5, X1) & aElement0(X5)) | ~ sP6(X2, X1)), inference(usedef, [], [e70])).
fof(e70, plain, ! [X2, X1] : (sP6(X2, X1) <=> ? [X5] : (sdtmndtasgtdt0(X2, xR, X5) & ((sdtmndtplgtdt0(X2, xR, X5) & (? [X6] : (sdtmndtplgtdt0(X6, xR, X5) & aReductOfIn0(X6, X2, xR) & aElement0(X6)) | aReductOfIn0(X5, X2, xR))) | (X2 = X5)) & sdtmndtasgtdt0(X1, xR, X5) & sP5(X5, X1) & aElement0(X5))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f71, plain, ! [X1, X0] : ((~ sdtmndtasgtdt0(X0, xR, X1) & ~ sdtmndtplgtdt0(X0, xR, X1) & ! [X4] : (~ sdtmndtplgtdt0(X4, xR, X1) | ~ aReductOfIn0(X4, X0, xR) | ~ aElement0(X4)) & ~ aReductOfIn0(X1, X0, xR) & ~ (X0 = X1)) | ~ sP7(X1, X0)), inference(usedef, [], [e71])).
fof(e71, plain, ! [X1, X0] : (sP7(X1, X0) <=> (~ sdtmndtasgtdt0(X0, xR, X1) & ~ sdtmndtplgtdt0(X0, xR, X1) & ! [X4] : (~ sdtmndtplgtdt0(X4, xR, X1) | ~ aReductOfIn0(X4, X0, xR) | ~ aElement0(X4)) & ~ aReductOfIn0(X1, X0, xR) & ~ (X0 = X1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f58, plain, ! [X0, X1, X2] : (? [X5] : (sdtmndtasgtdt0(X2, xR, X5) & ((sdtmndtplgtdt0(X2, xR, X5) & (? [X6] : (sdtmndtplgtdt0(X6, xR, X5) & aReductOfIn0(X6, X2, xR) & aElement0(X6)) | aReductOfIn0(X5, X2, xR))) | (X2 = X5)) & sdtmndtasgtdt0(X1, xR, X5) & ((sdtmndtplgtdt0(X1, xR, X5) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X5) & aReductOfIn0(X7, X1, xR) & aElement0(X7)) | aReductOfIn0(X5, X1, xR))) | (X1 = X5)) & aElement0(X5)) | ~ iLess0(X0, xa) | (~ sdtmndtasgtdt0(X0, xR, X2) & ~ sdtmndtplgtdt0(X0, xR, X2) & ! [X3] : (~ sdtmndtplgtdt0(X3, xR, X2) | ~ aReductOfIn0(X3, X0, xR) | ~ aElement0(X3)) & ~ aReductOfIn0(X2, X0, xR) & ~ (X0 = X2)) | (~ sdtmndtasgtdt0(X0, xR, X1) & ~ sdtmndtplgtdt0(X0, xR, X1) & ! [X4] : (~ sdtmndtplgtdt0(X4, xR, X1) | ~ aReductOfIn0(X4, X0, xR) | ~ aElement0(X4)) & ~ aReductOfIn0(X1, X0, xR) & ~ (X0 = X1)) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0)), inference(flattening, [], [f57])).
fof(f57, plain, ! [X0, X1, X2] : ((? [X5] : (sdtmndtasgtdt0(X2, xR, X5) & ((sdtmndtplgtdt0(X2, xR, X5) & (? [X6] : (sdtmndtplgtdt0(X6, xR, X5) & aReductOfIn0(X6, X2, xR) & aElement0(X6)) | aReductOfIn0(X5, X2, xR))) | (X2 = X5)) & sdtmndtasgtdt0(X1, xR, X5) & ((sdtmndtplgtdt0(X1, xR, X5) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X5) & aReductOfIn0(X7, X1, xR) & aElement0(X7)) | aReductOfIn0(X5, X1, xR))) | (X1 = X5)) & aElement0(X5)) | ~ iLess0(X0, xa)) | ((~ sdtmndtasgtdt0(X0, xR, X2) & ~ sdtmndtplgtdt0(X0, xR, X2) & ! [X3] : (~ sdtmndtplgtdt0(X3, xR, X2) | ~ aReductOfIn0(X3, X0, xR) | ~ aElement0(X3)) & ~ aReductOfIn0(X2, X0, xR) & ~ (X0 = X2)) | (~ sdtmndtasgtdt0(X0, xR, X1) & ~ sdtmndtplgtdt0(X0, xR, X1) & ! [X4] : (~ sdtmndtplgtdt0(X4, xR, X1) | ~ aReductOfIn0(X4, X0, xR) | ~ aElement0(X4)) & ~ aReductOfIn0(X1, X0, xR) & ~ (X0 = X1)) | ~ aElement0(X2) | ~ aElement0(X1) | ~ aElement0(X0))), inference(ennf_transformation, [], [f31])).
fof(f31, plain, ! [X0, X1, X2] : (((sdtmndtasgtdt0(X0, xR, X2) | sdtmndtplgtdt0(X0, xR, X2) | ? [X3] : (sdtmndtplgtdt0(X3, xR, X2) & aReductOfIn0(X3, X0, xR) & aElement0(X3)) | aReductOfIn0(X2, X0, xR) | (X0 = X2)) & (sdtmndtasgtdt0(X0, xR, X1) | sdtmndtplgtdt0(X0, xR, X1) | ? [X4] : (sdtmndtplgtdt0(X4, xR, X1) & aReductOfIn0(X4, X0, xR) & aElement0(X4)) | aReductOfIn0(X1, X0, xR) | (X0 = X1)) & aElement0(X2) & aElement0(X1) & aElement0(X0)) => (iLess0(X0, xa) => ? [X5] : (sdtmndtasgtdt0(X2, xR, X5) & ((sdtmndtplgtdt0(X2, xR, X5) & (? [X6] : (sdtmndtplgtdt0(X6, xR, X5) & aReductOfIn0(X6, X2, xR) & aElement0(X6)) | aReductOfIn0(X5, X2, xR))) | (X2 = X5)) & sdtmndtasgtdt0(X1, xR, X5) & ((sdtmndtplgtdt0(X1, xR, X5) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X5) & aReductOfIn0(X7, X1, xR) & aElement0(X7)) | aReductOfIn0(X5, X1, xR))) | (X1 = X5)) & aElement0(X5)))), inference(rectify, [], [f18])).
fof(f18, plain, ! [X0, X1, X2] : (((sdtmndtasgtdt0(X0, xR, X2) | sdtmndtplgtdt0(X0, xR, X2) | ? [X3] : (sdtmndtplgtdt0(X3, xR, X2) & aReductOfIn0(X3, X0, xR) & aElement0(X3)) | aReductOfIn0(X2, X0, xR) | (X0 = X2)) & (sdtmndtasgtdt0(X0, xR, X1) | sdtmndtplgtdt0(X0, xR, X1) | ? [X3] : (sdtmndtplgtdt0(X3, xR, X1) & aReductOfIn0(X3, X0, xR) & aElement0(X3)) | aReductOfIn0(X1, X0, xR) | (X0 = X1)) & aElement0(X2) & aElement0(X1) & aElement0(X0)) => (iLess0(X0, xa) => ? [X3] : (sdtmndtasgtdt0(X2, xR, X3) & ((sdtmndtplgtdt0(X2, xR, X3) & (? [X4] : (sdtmndtplgtdt0(X4, xR, X3) & aReductOfIn0(X4, X2, xR) & aElement0(X4)) | aReductOfIn0(X3, X2, xR))) | (X2 = X3)) & sdtmndtasgtdt0(X1, xR, X3) & ((sdtmndtplgtdt0(X1, xR, X3) & (? [X4] : (sdtmndtplgtdt0(X4, xR, X3) & aReductOfIn0(X4, X1, xR) & aElement0(X4)) | aReductOfIn0(X3, X1, xR))) | (X1 = X3)) & aElement0(X3)))), file('/home/ubuntu/library/tptp/Problems/COM/COM019+4.p', m__715)).
fof(f319, plain, (sdtmndtplgtdt0(xu, xR, xb) | ~ spl34_10), inference(avatar_component_clause, [], [f317])).
fof(f17825, plain, (~ sP7(xd, xu) | ~ spl34_133), inference(resolution, [], [f17695, f203])).
fof(f203, plain, ! [X0, X1] : (~ sdtmndtasgtdt0(X1, xR, X0) | ~ sP7(X0, X1)), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ! [X0, X1] : ((~ sdtmndtasgtdt0(X1, xR, X0) & ~ sdtmndtplgtdt0(X1, xR, X0) & ! [X2] : (~ sdtmndtplgtdt0(X2, xR, X0) | ~ aReductOfIn0(X2, X1, xR) | ~ aElement0(X2)) & ~ aReductOfIn0(X0, X1, xR) & ~ (X0 = X1)) | ~ sP7(X0, X1)), inference(rectify, [], [f110])).
fof(f110, plain, ! [X1, X0] : ((~ sdtmndtasgtdt0(X0, xR, X1) & ~ sdtmndtplgtdt0(X0, xR, X1) & ! [X4] : (~ sdtmndtplgtdt0(X4, xR, X1) | ~ aReductOfIn0(X4, X0, xR) | ~ aElement0(X4)) & ~ aReductOfIn0(X1, X0, xR) & ~ (X0 = X1)) | ~ sP7(X1, X0)), inference(nnf_transformation, [], [f71])).
fof(f56470, plain, (~ sP6(xb, xd) | (~ spl34_10 | ~ spl34_133)), inference(subsumption_resolution, [], [f56418, f266])).
fof(f56418, plain, (sdtmndtasgtdt0(xb, xR, xd) | ~ sP6(xb, xd) | (~ spl34_10 | ~ spl34_133)), inference(superposition, [], [f211, f38058])).
fof(f38058, plain, ((xd = sK24(xb, xd)) | (~ spl34_10 | ~ spl34_133)), inference(resolution, [], [f17616, f17879])).
fof(f17616, plain, ! [X0] : (~ sP6(X0, xd) | (xd = sK24(X0, xd))), inference(resolution, [], [f17426, f205])).
fof(f205, plain, ! [X0, X1] : (sP5(sK24(X0, X1), X1) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f116])).
fof(f116, plain, ! [X0, X1] : ((sdtmndtasgtdt0(X0, xR, sK24(X0, X1)) & ((sdtmndtplgtdt0(X0, xR, sK24(X0, X1)) & ((sdtmndtplgtdt0(sK25(X0, X1), xR, sK24(X0, X1)) & aReductOfIn0(sK25(X0, X1), X0, xR) & aElement0(sK25(X0, X1))) | aReductOfIn0(sK24(X0, X1), X0, xR))) | (sK24(X0, X1) = X0)) & sdtmndtasgtdt0(X1, xR, sK24(X0, X1)) & sP5(sK24(X0, X1), X1) & aElement0(sK24(X0, X1))) | ~ sP6(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK24, sK25])], [f113, f115, f114])).
fof(f114, plain, ! [X1, X0] : (? [X2] : (sdtmndtasgtdt0(X0, xR, X2) & ((sdtmndtplgtdt0(X0, xR, X2) & (? [X3] : (sdtmndtplgtdt0(X3, xR, X2) & aReductOfIn0(X3, X0, xR) & aElement0(X3)) | aReductOfIn0(X2, X0, xR))) | (X0 = X2)) & sdtmndtasgtdt0(X1, xR, X2) & sP5(X2, X1) & aElement0(X2)) => (sdtmndtasgtdt0(X0, xR, sK24(X0, X1)) & ((sdtmndtplgtdt0(X0, xR, sK24(X0, X1)) & (? [X3] : (sdtmndtplgtdt0(X3, xR, sK24(X0, X1)) & aReductOfIn0(X3, X0, xR) & aElement0(X3)) | aReductOfIn0(sK24(X0, X1), X0, xR))) | (sK24(X0, X1) = X0)) & sdtmndtasgtdt0(X1, xR, sK24(X0, X1)) & sP5(sK24(X0, X1), X1) & aElement0(sK24(X0, X1)))), introduced(choice_axiom, [])).
fof(f115, plain, ! [X1, X0] : (? [X3] : (sdtmndtplgtdt0(X3, xR, sK24(X0, X1)) & aReductOfIn0(X3, X0, xR) & aElement0(X3)) => (sdtmndtplgtdt0(sK25(X0, X1), xR, sK24(X0, X1)) & aReductOfIn0(sK25(X0, X1), X0, xR) & aElement0(sK25(X0, X1)))), introduced(choice_axiom, [])).
fof(f113, plain, ! [X0, X1] : (? [X2] : (sdtmndtasgtdt0(X0, xR, X2) & ((sdtmndtplgtdt0(X0, xR, X2) & (? [X3] : (sdtmndtplgtdt0(X3, xR, X2) & aReductOfIn0(X3, X0, xR) & aElement0(X3)) | aReductOfIn0(X2, X0, xR))) | (X0 = X2)) & sdtmndtasgtdt0(X1, xR, X2) & sP5(X2, X1) & aElement0(X2)) | ~ sP6(X0, X1)), inference(rectify, [], [f112])).
fof(f112, plain, ! [X2, X1] : (? [X5] : (sdtmndtasgtdt0(X2, xR, X5) & ((sdtmndtplgtdt0(X2, xR, X5) & (? [X6] : (sdtmndtplgtdt0(X6, xR, X5) & aReductOfIn0(X6, X2, xR) & aElement0(X6)) | aReductOfIn0(X5, X2, xR))) | (X2 = X5)) & sdtmndtasgtdt0(X1, xR, X5) & sP5(X5, X1) & aElement0(X5)) | ~ sP6(X2, X1)), inference(nnf_transformation, [], [f70])).
fof(f17426, plain, ! [X7] : (~ sP5(X7, xd) | (xd = X7)), inference(subsumption_resolution, [], [f17420, f260])).
fof(f260, plain, ! [X0] : ~ aReductOfIn0(X0, xd, xR), inference(cnf_transformation, [], [f132])).
fof(f17420, plain, ! [X7] : (aReductOfIn0(X7, xd, xR) | (xd = X7) | ~ sP5(X7, xd)), inference(resolution, [], [f260, f213])).
fof(f213, plain, ! [X0, X1] : (aReductOfIn0(sK26(X0, X1), X1, xR) | aReductOfIn0(X0, X1, xR) | (X0 = X1) | ~ sP5(X0, X1)), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ! [X0, X1] : ((sdtmndtplgtdt0(X1, xR, X0) & ((sdtmndtplgtdt0(sK26(X0, X1), xR, X0) & aReductOfIn0(sK26(X0, X1), X1, xR) & aElement0(sK26(X0, X1))) | aReductOfIn0(X0, X1, xR))) | (X0 = X1) | ~ sP5(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK26])], [f118, f119])).
fof(f119, plain, ! [X1, X0] : (? [X2] : (sdtmndtplgtdt0(X2, xR, X0) & aReductOfIn0(X2, X1, xR) & aElement0(X2)) => (sdtmndtplgtdt0(sK26(X0, X1), xR, X0) & aReductOfIn0(sK26(X0, X1), X1, xR) & aElement0(sK26(X0, X1)))), introduced(choice_axiom, [])).
fof(f118, plain, ! [X0, X1] : ((sdtmndtplgtdt0(X1, xR, X0) & (? [X2] : (sdtmndtplgtdt0(X2, xR, X0) & aReductOfIn0(X2, X1, xR) & aElement0(X2)) | aReductOfIn0(X0, X1, xR))) | (X0 = X1) | ~ sP5(X0, X1)), inference(rectify, [], [f117])).
fof(f117, plain, ! [X5, X1] : ((sdtmndtplgtdt0(X1, xR, X5) & (? [X7] : (sdtmndtplgtdt0(X7, xR, X5) & aReductOfIn0(X7, X1, xR) & aElement0(X7)) | aReductOfIn0(X5, X1, xR))) | (X1 = X5) | ~ sP5(X5, X1)), inference(nnf_transformation, [], [f69])).
fof(f211, plain, ! [X0, X1] : (sdtmndtasgtdt0(X0, xR, sK24(X0, X1)) | ~ sP6(X0, X1)), inference(cnf_transformation, [], [f116])).
fof(f3807, plain, spl34_133, inference(avatar_split_clause, [], [f243, f3752])).
fof(f243, plain, aElement0(xw), inference(cnf_transformation, [], [f130])).