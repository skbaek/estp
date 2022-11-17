fof(f228105, plain, $false, inference(avatar_sat_refutation, [], [f704, f741, f758, f228056])).
fof(f228056, plain, (~ spl15_15 | ~ spl15_16), inference(avatar_contradiction_clause, [], [f228055])).
fof(f228055, plain, ($false | (~ spl15_15 | ~ spl15_16)), inference(subsumption_resolution, [], [f227951, f349])).
fof(f349, plain, ~ aElementOf0(xx, xT), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ~ aElementOf0(xx, xT), inference(flattening, [], [f73])).
fof(f73, plain, ~ aElementOf0(xx, xT), inference(negated_conjecture, [], [f72])).
fof(f72, plain, ~ aElementOf0(xx, xT), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__)).
fof(f227951, plain, (aElementOf0(xx, xT) | (~ spl15_15 | ~ spl15_16)), inference(resolution, [], [f3530, f1767])).
fof(f1767, plain, (aElementOf0(xx, xP) | ~ spl15_15), inference(subsumption_resolution, [], [f1764, f405])).
fof(f405, plain, aElement0(xx), inference(subsumption_resolution, [], [f401, f333])).
fof(f333, plain, aSet0(xS), inference(cnf_transformation, [], [f62])).
fof(f62, plain, (~ (sz00 = xk) & aSet0(xT) & aSet0(xS)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__2202_02)).
fof(f401, plain, (aElement0(xx) | ~ aSet0(xS)), inference(resolution, [], [f223, f338])).
fof(f338, plain, aElementOf0(xx, xS), inference(cnf_transformation, [], [f64])).
fof(f64, plain, aElementOf0(xx, xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__2256)).
fof(f223, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', mEOfElem)).
fof(f1764, plain, (~ aElement0(xx) | aElementOf0(xx, xP) | ~ spl15_15), inference(resolution, [], [f703, f354])).
fof(f354, plain, ! [X4, X2, X1] : (~ sP0(X4, X1, X2) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(equality_resolution, [], [f244])).
fof(f244, plain, ! [X4, X2, X0, X1] : (aElementOf0(X4, X2) | ~ (X0 = X4) | ~ aElement0(X4) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f184])).
fof(f184, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f182, f183])).
fof(f183, plain, ! [X2, X1, X0] : (? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) => (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f182, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f181])).
fof(f181, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : (((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f180])).
fof(f180, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3))) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f163])).
fof(f163, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e163])).
fof(e163, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f703, plain, (sP0(xx, sdtmndt0(xQ, xy), xP) | ~ spl15_15), inference(avatar_component_clause, [], [f701])).
fof(f701, plain, (spl15_15 <=> sP0(xx, sdtmndt0(xQ, xy), xP)), introduced(avatar_definition, [new_symbols(naming, [spl15_15])])).
fof(f3530, plain, (! [X1] : (~ aElementOf0(X1, xP) | aElementOf0(X1, xT)) | ~ spl15_16), inference(subsumption_resolution, [], [f3516, f334])).
fof(f334, plain, aSet0(xT), inference(cnf_transformation, [], [f62])).
fof(f3516, plain, (! [X1] : (~ aElementOf0(X1, xP) | aElementOf0(X1, xT) | ~ aSet0(xT)) | ~ spl15_16), inference(resolution, [], [f3470, f231])).
fof(f231, plain, ! [X0, X3, X1] : (~ aSubsetOf0(X1, X0) | ~ aElementOf0(X3, X1) | aElementOf0(X3, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f176, f177])).
fof(f177, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f176, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f174])).
fof(f174, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f88])).
fof(f88, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', mDefSub)).
fof(f3470, plain, (aSubsetOf0(xP, xT) | ~ spl15_16), inference(subsumption_resolution, [], [f3469, f334])).
fof(f3469, plain, (aSubsetOf0(xP, xT) | ~ aSet0(xT) | ~ spl15_16), inference(subsumption_resolution, [], [f3465, f332])).
fof(f332, plain, aElementOf0(xk, szNzAzT0), inference(cnf_transformation, [], [f61])).
fof(f61, plain, aElementOf0(xk, szNzAzT0), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__2202)).
fof(f3465, plain, (aSubsetOf0(xP, xT) | ~ aElementOf0(xk, szNzAzT0) | ~ aSet0(xT) | ~ spl15_16), inference(resolution, [], [f2799, f370])).
fof(f370, plain, ! [X4, X0, X1] : (~ aElementOf0(X4, slbdtsldtrb0(X0, X1)) | aSubsetOf0(X4, X0) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f323])).
fof(f323, plain, ! [X4, X2, X0, X1] : (aSubsetOf0(X4, X0) | ~ aElementOf0(X4, X2) | ~ (slbdtsldtrb0(X0, X1) = X2) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f222])).
fof(f222, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ((~ (sbrdtbr0(sK14(X0, X1, X2)) = X1) | ~ aSubsetOf0(sK14(X0, X1, X2), X0) | ~ aElementOf0(sK14(X0, X1, X2), X2)) & (((sbrdtbr0(sK14(X0, X1, X2)) = X1) & aSubsetOf0(sK14(X0, X1, X2), X0)) | aElementOf0(sK14(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0)) & (((sbrdtbr0(X4) = X1) & aSubsetOf0(X4, X0)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f220, f221])).
fof(f221, plain, ! [X2, X1, X0] : (? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) => ((~ (sbrdtbr0(sK14(X0, X1, X2)) = X1) | ~ aSubsetOf0(sK14(X0, X1, X2), X0) | ~ aElementOf0(sK14(X0, X1, X2), X2)) & (((sbrdtbr0(sK14(X0, X1, X2)) = X1) & aSubsetOf0(sK14(X0, X1, X2), X0)) | aElementOf0(sK14(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f220, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0)) & (((sbrdtbr0(X4) = X1) & aSubsetOf0(X4, X0)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(rectify, [], [f219])).
fof(f219, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(flattening, [], [f218])).
fof(f218, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | (? [X3] : (((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0))) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(nnf_transformation, [], [f156])).
fof(f156, plain, ! [X0, X1] : (! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(flattening, [], [f155])).
fof(f155, plain, ! [X0, X1] : (! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2))) | (~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0))), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aSet0(X0)) => ! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', mDefSel)).
fof(f2799, plain, (aElementOf0(xP, slbdtsldtrb0(xT, xk)) | ~ spl15_16), inference(resolution, [], [f856, f348])).
fof(f348, plain, aElementOf0(xP, slbdtsldtrb0(xS, xk)), inference(cnf_transformation, [], [f71])).
fof(f71, plain, aElementOf0(xP, slbdtsldtrb0(xS, xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__2378)).
fof(f856, plain, (! [X4] : (~ aElementOf0(X4, slbdtsldtrb0(xS, xk)) | aElementOf0(X4, slbdtsldtrb0(xT, xk))) | ~ spl15_16), inference(subsumption_resolution, [], [f853, f722])).
fof(f722, plain, (aSet0(slbdtsldtrb0(xT, xk)) | ~ spl15_16), inference(avatar_component_clause, [], [f721])).
fof(f721, plain, (spl15_16 <=> aSet0(slbdtsldtrb0(xT, xk))), introduced(avatar_definition, [new_symbols(naming, [spl15_16])])).
fof(f853, plain, ! [X4] : (~ aElementOf0(X4, slbdtsldtrb0(xS, xk)) | aElementOf0(X4, slbdtsldtrb0(xT, xk)) | ~ aSet0(slbdtsldtrb0(xT, xk))), inference(resolution, [], [f231, f336])).
fof(f336, plain, aSubsetOf0(slbdtsldtrb0(xS, xk), slbdtsldtrb0(xT, xk)), inference(cnf_transformation, [], [f63])).
fof(f63, plain, (~ (slcrc0 = slbdtsldtrb0(xS, xk)) & aSubsetOf0(slbdtsldtrb0(xS, xk), slbdtsldtrb0(xT, xk))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__2227)).
fof(f758, plain, spl15_14, inference(avatar_contradiction_clause, [], [f757])).
fof(f757, plain, ($false | spl15_14), inference(subsumption_resolution, [], [f756, f340])).
fof(f340, plain, aSet0(xQ), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((xk = sbrdtbr0(xQ)) & isFinite0(xQ) & aSet0(xQ)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__2291)).
fof(f756, plain, (~ aSet0(xQ) | spl15_14), inference(subsumption_resolution, [], [f755, f343])).
fof(f343, plain, aElement0(xy), inference(cnf_transformation, [], [f67])).
fof(f67, plain, (aElementOf0(xy, xQ) & aElement0(xy)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__2304)).
fof(f755, plain, (~ aElement0(xy) | ~ aSet0(xQ) | spl15_14), inference(resolution, [], [f751, f261])).
fof(f261, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f99, e167, e166])).
fof(f166, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e166])).
fof(e166, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f167, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e167])).
fof(e167, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f99, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f98])).
fof(f98, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', mDefDiff)).
fof(f751, plain, (~ sP3(xQ, xy) | spl15_14), inference(resolution, [], [f710, f706])).
fof(f706, plain, (~ aSet0(sdtmndt0(xQ, xy)) | spl15_14), inference(subsumption_resolution, [], [f705, f405])).
fof(f705, plain, (~ aElement0(xx) | ~ aSet0(sdtmndt0(xQ, xy)) | spl15_14), inference(resolution, [], [f699, f249])).
fof(f249, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f165])).
fof(f165, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f97, e164, e163])).
fof(f164, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e164])).
fof(e164, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f97, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f96])).
fof(f96, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', mDefCons)).
fof(f699, plain, (~ sP1(sdtmndt0(xQ, xy), xx) | spl15_14), inference(avatar_component_clause, [], [f697])).
fof(f697, plain, (spl15_14 <=> sP1(sdtmndt0(xQ, xy), xx)), introduced(avatar_definition, [new_symbols(naming, [spl15_14])])).
fof(f710, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f355, f252])).
fof(f252, plain, ! [X2, X0, X1] : (~ sP2(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f188, f189])).
fof(f189, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f188, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f187])).
fof(f187, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f186])).
fof(f186, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f166])).
fof(f355, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f250])).
fof(f250, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f185])).
fof(f185, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f167])).
fof(f741, plain, spl15_16, inference(avatar_contradiction_clause, [], [f740])).
fof(f740, plain, ($false | spl15_16), inference(subsumption_resolution, [], [f739, f334])).
fof(f739, plain, (~ aSet0(xT) | spl15_16), inference(subsumption_resolution, [], [f738, f332])).
fof(f738, plain, (~ aElementOf0(xk, szNzAzT0) | ~ aSet0(xT) | spl15_16), inference(resolution, [], [f723, f371])).
fof(f371, plain, ! [X0, X1] : (aSet0(slbdtsldtrb0(X0, X1)) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f322])).
fof(f322, plain, ! [X2, X0, X1] : (aSet0(X2) | ~ (slbdtsldtrb0(X0, X1) = X2) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f222])).
fof(f723, plain, (~ aSet0(slbdtsldtrb0(xT, xk)) | spl15_16), inference(avatar_component_clause, [], [f721])).
fof(f704, plain, (~ spl15_14 | spl15_15), inference(avatar_split_clause, [], [f695, f701, f697])).
fof(f695, plain, (sP0(xx, sdtmndt0(xQ, xy), xP) | ~ sP1(sdtmndt0(xQ, xy), xx)), inference(superposition, [], [f353, f347])).
fof(f347, plain, (xP = sdtpldt0(sdtmndt0(xQ, xy), xx)), inference(cnf_transformation, [], [f70])).
fof(f70, plain, (xP = sdtpldt0(sdtmndt0(xQ, xy), xx)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM558+1.p', m__2357)).
fof(f353, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f238])).
fof(f238, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f179])).
fof(f179, plain, ! [X0, X1] : (! [X2] : (((sdtpldt0(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f164])).