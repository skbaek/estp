fof(f787238, plain, $false, inference(avatar_sat_refutation, [], [f392, f740, f774, f776, f782, f962, f988, f1433, f2562, f3127, f5664, f34991, f47246, f50346, f611509, f611631, f630570, f780763, f780975, f787237])).
fof(f787237, plain, (~ spl15_7 | ~ spl15_20 | ~ spl15_84 | ~ spl15_91 | ~ spl15_1340 | spl15_1348 | spl15_13153), inference(avatar_contradiction_clause, [], [f787236])).
fof(f787236, plain, ($false | (~ spl15_7 | ~ spl15_20 | ~ spl15_84 | ~ spl15_91 | ~ spl15_1340 | spl15_1348 | spl15_13153)), inference(subsumption_resolution, [], [f787232, f35031])).
fof(f35031, plain, (~ aElementOf0(sK5(xS, xP), xS) | spl15_1348), inference(avatar_component_clause, [], [f35029])).
fof(f35029, plain, (spl15_1348 <=> aElementOf0(sK5(xS, xP), xS)), introduced(avatar_definition, [new_symbols(naming, [spl15_1348])])).
fof(f787232, plain, (aElementOf0(sK5(xS, xP), xS) | (~ spl15_7 | ~ spl15_20 | ~ spl15_84 | ~ spl15_91 | ~ spl15_1340 | spl15_13153)), inference(resolution, [], [f787114, f1154])).
fof(f1154, plain, ! [X0] : (~ aElementOf0(X0, xQ) | aElementOf0(X0, xS)), inference(subsumption_resolution, [], [f1141, f333])).
fof(f333, plain, aSet0(xS), inference(cnf_transformation, [], [f62])).
fof(f62, plain, (~ (sz00 = xk) & aSet0(xT) & aSet0(xS)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__2202_02)).
fof(f1141, plain, ! [X0] : (~ aElementOf0(X0, xQ) | aElementOf0(X0, xS) | ~ aSet0(xS)), inference(resolution, [], [f1137, f231])).
fof(f231, plain, ! [X0, X3, X1] : (~ aSubsetOf0(X1, X0) | ~ aElementOf0(X3, X1) | aElementOf0(X3, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f178])).
fof(f178, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f176, f177])).
fof(f177, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f176, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f175])).
fof(f175, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f174])).
fof(f174, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f87])).
fof(f87, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mDefSub)).
fof(f1137, plain, aSubsetOf0(xQ, xS), inference(subsumption_resolution, [], [f1136, f333])).
fof(f1136, plain, (aSubsetOf0(xQ, xS) | ~ aSet0(xS)), inference(subsumption_resolution, [], [f1133, f332])).
fof(f332, plain, aElementOf0(xk, szNzAzT0), inference(cnf_transformation, [], [f61])).
fof(f61, plain, aElementOf0(xk, szNzAzT0), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__2202)).
fof(f1133, plain, (aSubsetOf0(xQ, xS) | ~ aElementOf0(xk, szNzAzT0) | ~ aSet0(xS)), inference(resolution, [], [f371, f339])).
fof(f339, plain, aElementOf0(xQ, slbdtsldtrb0(xS, xk)), inference(cnf_transformation, [], [f65])).
fof(f65, plain, aElementOf0(xQ, slbdtsldtrb0(xS, xk)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__2270)).
fof(f371, plain, ! [X4, X0, X1] : (~ aElementOf0(X4, slbdtsldtrb0(X0, X1)) | aSubsetOf0(X4, X0) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f323])).
fof(f323, plain, ! [X4, X2, X0, X1] : (aSubsetOf0(X4, X0) | ~ aElementOf0(X4, X2) | ~ (slbdtsldtrb0(X0, X1) = X2) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f222])).
fof(f222, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ((~ (sbrdtbr0(sK14(X0, X1, X2)) = X1) | ~ aSubsetOf0(sK14(X0, X1, X2), X0) | ~ aElementOf0(sK14(X0, X1, X2), X2)) & (((sbrdtbr0(sK14(X0, X1, X2)) = X1) & aSubsetOf0(sK14(X0, X1, X2), X0)) | aElementOf0(sK14(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0)) & (((sbrdtbr0(X4) = X1) & aSubsetOf0(X4, X0)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f220, f221])).
fof(f221, plain, ! [X2, X1, X0] : (? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) => ((~ (sbrdtbr0(sK14(X0, X1, X2)) = X1) | ~ aSubsetOf0(sK14(X0, X1, X2), X0) | ~ aElementOf0(sK14(X0, X1, X2), X2)) & (((sbrdtbr0(sK14(X0, X1, X2)) = X1) & aSubsetOf0(sK14(X0, X1, X2), X0)) | aElementOf0(sK14(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f220, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0)) & (((sbrdtbr0(X4) = X1) & aSubsetOf0(X4, X0)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(rectify, [], [f219])).
fof(f219, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(flattening, [], [f218])).
fof(f218, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | (? [X3] : (((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0))) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(nnf_transformation, [], [f155])).
fof(f155, plain, ! [X0, X1] : (! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(flattening, [], [f154])).
fof(f154, plain, ! [X0, X1] : (! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2))) | (~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0))), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aSet0(X0)) => ! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mDefSel)).
fof(f787114, plain, (aElementOf0(sK5(xS, xP), xQ) | (~ spl15_7 | ~ spl15_20 | ~ spl15_84 | ~ spl15_91 | ~ spl15_1340 | spl15_13153)), inference(subsumption_resolution, [], [f787097, f613373])).
fof(f613373, plain, (~ (xx = sK5(xS, xP)) | spl15_13153), inference(avatar_component_clause, [], [f613372])).
fof(f613372, plain, (spl15_13153 <=> (xx = sK5(xS, xP))), introduced(avatar_definition, [new_symbols(naming, [spl15_13153])])).
fof(f787097, plain, ((xx = sK5(xS, xP)) | aElementOf0(sK5(xS, xP), xQ) | (~ spl15_7 | ~ spl15_20 | ~ spl15_84 | ~ spl15_91 | ~ spl15_1340)), inference(resolution, [], [f691987, f34974])).
fof(f34974, plain, (aElementOf0(sK5(xS, xP), xP) | ~ spl15_1340), inference(avatar_component_clause, [], [f34972])).
fof(f34972, plain, (spl15_1340 <=> aElementOf0(sK5(xS, xP), xP)), introduced(avatar_definition, [new_symbols(naming, [spl15_1340])])).
fof(f691987, plain, (! [X0] : (~ aElementOf0(X0, xP) | (xx = X0) | aElementOf0(X0, xQ)) | (~ spl15_7 | ~ spl15_20 | ~ spl15_84 | ~ spl15_91)), inference(resolution, [], [f1323, f4755])).
fof(f4755, plain, (! [X5] : (~ aElementOf0(X5, sdtmndt0(xQ, xy)) | aElementOf0(X5, xQ)) | (~ spl15_7 | ~ spl15_84 | ~ spl15_91)), inference(subsumption_resolution, [], [f4754, f3044])).
fof(f3044, plain, (! [X1] : (~ aElementOf0(X1, sdtmndt0(xQ, xy)) | aElement0(X1)) | (~ spl15_7 | ~ spl15_91)), inference(subsumption_resolution, [], [f3043, f2520])).
fof(f2520, plain, (sP3(xP, xx) | ~ spl15_91), inference(avatar_component_clause, [], [f2519])).
fof(f2519, plain, (spl15_91 <=> sP3(xP, xx)), introduced(avatar_definition, [new_symbols(naming, [spl15_91])])).
fof(f3043, plain, (! [X1] : (~ aElementOf0(X1, sdtmndt0(xQ, xy)) | ~ sP3(xP, xx) | aElement0(X1)) | ~ spl15_7), inference(superposition, [], [f744, f1202])).
fof(f1202, plain, ((sdtmndt0(xQ, xy) = sdtmndt0(xP, xx)) | ~ spl15_7), inference(forward_demodulation, [], [f1201, f347])).
fof(f347, plain, (xP = sdtpldt0(sdtmndt0(xQ, xy), xx)), inference(cnf_transformation, [], [f70])).
fof(f70, plain, (xP = sdtpldt0(sdtmndt0(xQ, xy), xx)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__2357)).
fof(f1201, plain, ((sdtmndt0(xQ, xy) = sdtmndt0(sdtpldt0(sdtmndt0(xQ, xy), xx), xx)) | ~ spl15_7), inference(subsumption_resolution, [], [f1200, f414])).
fof(f414, plain, aElement0(xx), inference(subsumption_resolution, [], [f411, f333])).
fof(f411, plain, (aElement0(xx) | ~ aSet0(xS)), inference(resolution, [], [f223, f338])).
fof(f338, plain, aElementOf0(xx, xS), inference(cnf_transformation, [], [f64])).
fof(f64, plain, aElementOf0(xx, xS), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__2256)).
fof(f223, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mEOfElem)).
fof(f1200, plain, ((sdtmndt0(xQ, xy) = sdtmndt0(sdtpldt0(sdtmndt0(xQ, xy), xx), xx)) | ~ aElement0(xx) | ~ spl15_7), inference(subsumption_resolution, [], [f1180, f467])).
fof(f467, plain, (aSet0(sdtmndt0(xQ, xy)) | ~ spl15_7), inference(avatar_component_clause, [], [f466])).
fof(f466, plain, (spl15_7 <=> aSet0(sdtmndt0(xQ, xy))), introduced(avatar_definition, [new_symbols(naming, [spl15_7])])).
fof(f1180, plain, ((sdtmndt0(xQ, xy) = sdtmndt0(sdtpldt0(sdtmndt0(xQ, xy), xx), xx)) | ~ aSet0(sdtmndt0(xQ, xy)) | ~ aElement0(xx)), inference(resolution, [], [f263, f348])).
fof(f348, plain, ~ aElementOf0(xx, sdtmndt0(xQ, xy)), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((xk = szszuzczcdt0(sbrdtbr0(sdtmndt0(xQ, xy)))) & ~ aElementOf0(xx, sdtmndt0(xQ, xy))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__2411)).
fof(f263, plain, ! [X0, X1] : (aElementOf0(X0, X1) | (sdtmndt0(sdtpldt0(X1, X0), X0) = X1) | ~ aSet0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ! [X0, X1] : ((sdtmndt0(sdtpldt0(X1, X0), X0) = X1) | aElementOf0(X0, X1) | ~ aSet0(X1) | ~ aElement0(X0)), inference(flattening, [], [f100])).
fof(f100, plain, ! [X0, X1] : (((sdtmndt0(sdtpldt0(X1, X0), X0) = X1) | aElementOf0(X0, X1)) | (~ aSet0(X1) | ~ aElement0(X0))), inference(ennf_transformation, [], [f18])).
fof(f18, plain, ! [X0, X1] : ((aSet0(X1) & aElement0(X0)) => (~ aElementOf0(X0, X1) => (sdtmndt0(sdtpldt0(X1, X0), X0) = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mDiffCons)).
fof(f744, plain, ! [X2, X0, X1] : (~ aElementOf0(X2, sdtmndt0(X0, X1)) | ~ sP3(X0, X1) | aElement0(X2)), inference(resolution, [], [f356, f253])).
fof(f253, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | ~ aElementOf0(X4, X2) | aElement0(X4)), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f188, f189])).
fof(f189, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f188, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f187])).
fof(f187, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f186])).
fof(f186, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f166])).
fof(f166, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e166])).
fof(e166, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f356, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f250])).
fof(f250, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f185])).
fof(f185, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f167])).
fof(f167, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e167])).
fof(e167, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f4754, plain, (! [X5] : (aElementOf0(X5, xQ) | ~ aElement0(X5) | ~ aElementOf0(X5, sdtmndt0(xQ, xy))) | ~ spl15_84), inference(subsumption_resolution, [], [f4742, f2463])).
fof(f2463, plain, (sP1(sdtmndt0(xQ, xy), xy) | ~ spl15_84), inference(avatar_component_clause, [], [f2462])).
fof(f2462, plain, (spl15_84 <=> sP1(sdtmndt0(xQ, xy), xy)), introduced(avatar_definition, [new_symbols(naming, [spl15_84])])).
fof(f4742, plain, ! [X5] : (aElementOf0(X5, xQ) | ~ aElement0(X5) | ~ aElementOf0(X5, sdtmndt0(xQ, xy)) | ~ sP1(sdtmndt0(xQ, xy), xy)), inference(superposition, [], [f993, f1019])).
fof(f1019, plain, (xQ = sdtpldt0(sdtmndt0(xQ, xy), xy)), inference(subsumption_resolution, [], [f1004, f340])).
fof(f340, plain, aSet0(xQ), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((xk = sbrdtbr0(xQ)) & isFinite0(xQ) & aSet0(xQ)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__2291)).
fof(f1004, plain, ((xQ = sdtpldt0(sdtmndt0(xQ, xy), xy)) | ~ aSet0(xQ)), inference(resolution, [], [f262, f344])).
fof(f344, plain, aElementOf0(xy, xQ), inference(cnf_transformation, [], [f67])).
fof(f67, plain, (aElementOf0(xy, xQ) & aElement0(xy)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__2304)).
fof(f262, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | (sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ! [X0] : (! [X1] : ((sdtpldt0(sdtmndt0(X0, X1), X1) = X0) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f17])).
fof(f17, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => (sdtpldt0(sdtmndt0(X0, X1), X1) = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mConsDiff)).
fof(f993, plain, ! [X2, X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X2)) | ~ aElement0(X0) | ~ aElementOf0(X0, X1) | ~ sP1(X1, X2)), inference(resolution, [], [f243, f354])).
fof(f354, plain, ! [X0, X1] : (sP0(X1, X0, sdtpldt0(X0, X1)) | ~ sP1(X0, X1)), inference(equality_resolution, [], [f238])).
fof(f238, plain, ! [X2, X0, X1] : (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f179])).
fof(f179, plain, ! [X0, X1] : (! [X2] : (((sdtpldt0(X0, X1) = X2) | ~ sP0(X1, X0, X2)) & (sP0(X1, X0, X2) | ~ (sdtpldt0(X0, X1) = X2))) | ~ sP1(X0, X1)), inference(nnf_transformation, [], [f164])).
fof(f164, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2)) | ~ sP1(X0, X1)), inference(usedef, [], [e164])).
fof(e164, plain, ! [X0, X1] : (sP1(X0, X1) <=> ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> sP0(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f243, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | ~ aElementOf0(X4, X1) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(cnf_transformation, [], [f184])).
fof(f184, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6])], [f182, f183])).
fof(f183, plain, ! [X2, X1, X0] : (? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) => (((~ (sK6(X0, X1, X2) = X0) & ~ aElementOf0(sK6(X0, X1, X2), X1)) | ~ aElement0(sK6(X0, X1, X2)) | ~ aElementOf0(sK6(X0, X1, X2), X2)) & ((((sK6(X0, X1, X2) = X0) | aElementOf0(sK6(X0, X1, X2), X1)) & aElement0(sK6(X0, X1, X2))) | aElementOf0(sK6(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f182, plain, ! [X0, X1, X2] : ((sP0(X0, X1, X2) | ? [X3] : (((~ (X0 = X3) & ~ aElementOf0(X3, X1)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X0 = X3) | aElementOf0(X3, X1)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (~ (X0 = X4) & ~ aElementOf0(X4, X1)) | ~ aElement0(X4)) & ((((X0 = X4) | aElementOf0(X4, X1)) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP0(X0, X1, X2))), inference(rectify, [], [f181])).
fof(f181, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | ? [X3] : (((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(flattening, [], [f180])).
fof(f180, plain, ! [X1, X0, X2] : ((sP0(X1, X0, X2) | (? [X3] : ((((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((~ (X1 = X3) & ~ aElementOf0(X3, X0)) | ~ aElement0(X3))) & ((((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP0(X1, X0, X2))), inference(nnf_transformation, [], [f163])).
fof(f163, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e163])).
fof(e163, plain, ! [X1, X0, X2] : (sP0(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f1323, plain, (! [X0] : (aElementOf0(X0, sdtmndt0(xQ, xy)) | ~ aElementOf0(X0, xP) | (xx = X0)) | ~ spl15_20), inference(resolution, [], [f739, f242])).
fof(f242, plain, ! [X4, X2, X0, X1] : (~ sP0(X0, X1, X2) | aElementOf0(X4, X1) | ~ aElementOf0(X4, X2) | (X0 = X4)), inference(cnf_transformation, [], [f184])).
fof(f739, plain, (sP0(xx, sdtmndt0(xQ, xy), xP) | ~ spl15_20), inference(avatar_component_clause, [], [f737])).
fof(f737, plain, (spl15_20 <=> sP0(xx, sdtmndt0(xQ, xy), xP)), introduced(avatar_definition, [new_symbols(naming, [spl15_20])])).
fof(f780975, plain, (~ spl15_1346 | ~ spl15_1348 | spl15_1 | ~ spl15_24 | ~ spl15_207), inference(avatar_split_clause, [], [f40861, f5634, f779, f385, f35029, f35020])).
fof(f35020, plain, (spl15_1346 <=> sP3(sdtpldt0(xS, sK5(xS, xP)), sK5(xS, xP))), introduced(avatar_definition, [new_symbols(naming, [spl15_1346])])).
fof(f385, plain, (spl15_1 <=> aSubsetOf0(xP, xS)), introduced(avatar_definition, [new_symbols(naming, [spl15_1])])).
fof(f779, plain, (spl15_24 <=> aSet0(xP)), introduced(avatar_definition, [new_symbols(naming, [spl15_24])])).
fof(f5634, plain, (spl15_207 <=> aSet0(xS)), introduced(avatar_definition, [new_symbols(naming, [spl15_207])])).
fof(f40861, plain, (~ aElementOf0(sK5(xS, xP), xS) | ~ sP3(sdtpldt0(xS, sK5(xS, xP)), sK5(xS, xP)) | (spl15_1 | ~ spl15_24 | ~ spl15_207)), inference(superposition, [], [f745, f8606])).
fof(f8606, plain, ((xS = sdtmndt0(sdtpldt0(xS, sK5(xS, xP)), sK5(xS, xP))) | (spl15_1 | ~ spl15_24 | ~ spl15_207)), inference(subsumption_resolution, [], [f8605, f781])).
fof(f781, plain, (aSet0(xP) | ~ spl15_24), inference(avatar_component_clause, [], [f779])).
fof(f8605, plain, ((xS = sdtmndt0(sdtpldt0(xS, sK5(xS, xP)), sK5(xS, xP))) | ~ aSet0(xP) | (spl15_1 | ~ spl15_207)), inference(subsumption_resolution, [], [f8546, f5635])).
fof(f5635, plain, (aSet0(xS) | ~ spl15_207), inference(avatar_component_clause, [], [f5634])).
fof(f8546, plain, (~ aSet0(xS) | (xS = sdtmndt0(sdtpldt0(xS, sK5(xS, xP)), sK5(xS, xP))) | ~ aSet0(xP) | spl15_1), inference(resolution, [], [f1203, f387])).
fof(f387, plain, (~ aSubsetOf0(xP, xS) | spl15_1), inference(avatar_component_clause, [], [f385])).
fof(f1203, plain, ! [X21, X22] : (aSubsetOf0(X22, X21) | ~ aSet0(X21) | (sdtmndt0(sdtpldt0(X21, sK5(X21, X22)), sK5(X21, X22)) = X21) | ~ aSet0(X22)), inference(subsumption_resolution, [], [f1182, f947])).
fof(f947, plain, ! [X0, X1] : (aElement0(sK5(X1, X0)) | ~ aSet0(X0) | ~ aSet0(X1) | aSubsetOf0(X0, X1)), inference(duplicate_literal_removal, [], [f935])).
fof(f935, plain, ! [X0, X1] : (aSubsetOf0(X0, X1) | ~ aSet0(X0) | ~ aSet0(X1) | aElement0(sK5(X1, X0)) | ~ aSet0(X0)), inference(resolution, [], [f232, f223])).
fof(f232, plain, ! [X0, X1] : (aElementOf0(sK5(X0, X1), X1) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f178])).
fof(f1182, plain, ! [X21, X22] : ((sdtmndt0(sdtpldt0(X21, sK5(X21, X22)), sK5(X21, X22)) = X21) | ~ aSet0(X21) | ~ aElement0(sK5(X21, X22)) | aSubsetOf0(X22, X21) | ~ aSet0(X22)), inference(duplicate_literal_removal, [], [f1181])).
fof(f1181, plain, ! [X21, X22] : ((sdtmndt0(sdtpldt0(X21, sK5(X21, X22)), sK5(X21, X22)) = X21) | ~ aSet0(X21) | ~ aElement0(sK5(X21, X22)) | aSubsetOf0(X22, X21) | ~ aSet0(X22) | ~ aSet0(X21)), inference(resolution, [], [f263, f233])).
fof(f233, plain, ! [X0, X1] : (~ aElementOf0(sK5(X0, X1), X0) | aSubsetOf0(X1, X0) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f178])).
fof(f745, plain, ! [X4, X3] : (~ aElementOf0(X4, sdtmndt0(X3, X4)) | ~ sP3(X3, X4)), inference(resolution, [], [f356, f357])).
fof(f357, plain, ! [X4, X2, X1] : (~ sP2(X4, X1, X2) | ~ aElementOf0(X4, X2)), inference(equality_resolution, [], [f255])).
fof(f255, plain, ! [X4, X2, X0, X1] : (~ (X0 = X4) | ~ aElementOf0(X4, X2) | ~ sP2(X0, X1, X2)), inference(cnf_transformation, [], [f190])).
fof(f780763, plain, (~ spl15_1335 | ~ spl15_1343 | spl15_1346), inference(avatar_contradiction_clause, [], [f780762])).
fof(f780762, plain, ($false | (~ spl15_1335 | ~ spl15_1343 | spl15_1346)), inference(subsumption_resolution, [], [f780761, f35005])).
fof(f35005, plain, (aSet0(sdtpldt0(xS, sK5(xS, xP))) | ~ spl15_1343), inference(avatar_component_clause, [], [f35004])).
fof(f35004, plain, (spl15_1343 <=> aSet0(sdtpldt0(xS, sK5(xS, xP)))), introduced(avatar_definition, [new_symbols(naming, [spl15_1343])])).
fof(f780761, plain, (~ aSet0(sdtpldt0(xS, sK5(xS, xP))) | (~ spl15_1335 | spl15_1346)), inference(subsumption_resolution, [], [f780760, f34951])).
fof(f34951, plain, (aElement0(sK5(xS, xP)) | ~ spl15_1335), inference(avatar_component_clause, [], [f34950])).
fof(f34950, plain, (spl15_1335 <=> aElement0(sK5(xS, xP))), introduced(avatar_definition, [new_symbols(naming, [spl15_1335])])).
fof(f780760, plain, (~ aElement0(sK5(xS, xP)) | ~ aSet0(sdtpldt0(xS, sK5(xS, xP))) | spl15_1346), inference(resolution, [], [f35022, f261])).
fof(f261, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f168])).
fof(f168, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f98, e167, e166])).
fof(f98, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f97])).
fof(f97, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mDefDiff)).
fof(f35022, plain, (~ sP3(sdtpldt0(xS, sK5(xS, xP)), sK5(xS, xP)) | spl15_1346), inference(avatar_component_clause, [], [f35020])).
fof(f630570, plain, (spl15_1 | ~ spl15_24 | ~ spl15_207 | ~ spl15_13153), inference(avatar_contradiction_clause, [], [f630569])).
fof(f630569, plain, ($false | (spl15_1 | ~ spl15_24 | ~ spl15_207 | ~ spl15_13153)), inference(subsumption_resolution, [], [f630568, f5635])).
fof(f630568, plain, (~ aSet0(xS) | (spl15_1 | ~ spl15_24 | ~ spl15_13153)), inference(subsumption_resolution, [], [f630567, f781])).
fof(f630567, plain, (~ aSet0(xP) | ~ aSet0(xS) | (spl15_1 | ~ spl15_13153)), inference(subsumption_resolution, [], [f630566, f387])).
fof(f630566, plain, (aSubsetOf0(xP, xS) | ~ aSet0(xP) | ~ aSet0(xS) | ~ spl15_13153), inference(subsumption_resolution, [], [f630564, f338])).
fof(f630564, plain, (~ aElementOf0(xx, xS) | aSubsetOf0(xP, xS) | ~ aSet0(xP) | ~ aSet0(xS) | ~ spl15_13153), inference(superposition, [], [f233, f613374])).
fof(f613374, plain, ((xx = sK5(xS, xP)) | ~ spl15_13153), inference(avatar_component_clause, [], [f613372])).
fof(f611631, plain, (~ spl15_1338 | spl15_1340 | spl15_1 | ~ spl15_24 | ~ spl15_207 | ~ spl15_1335), inference(avatar_split_clause, [], [f611630, f34950, f5634, f779, f385, f34972, f34963])).
fof(f34963, plain, (spl15_1338 <=> sP1(sdtmndt0(xP, sK5(xS, xP)), sK5(xS, xP))), introduced(avatar_definition, [new_symbols(naming, [spl15_1338])])).
fof(f611630, plain, (aElementOf0(sK5(xS, xP), xP) | ~ sP1(sdtmndt0(xP, sK5(xS, xP)), sK5(xS, xP)) | (spl15_1 | ~ spl15_24 | ~ spl15_207 | ~ spl15_1335)), inference(subsumption_resolution, [], [f40846, f34951])).
fof(f40846, plain, (aElementOf0(sK5(xS, xP), xP) | ~ aElement0(sK5(xS, xP)) | ~ sP1(sdtmndt0(xP, sK5(xS, xP)), sK5(xS, xP)) | (spl15_1 | ~ spl15_24 | ~ spl15_207)), inference(superposition, [], [f742, f8236])).
fof(f8236, plain, ((xP = sdtpldt0(sdtmndt0(xP, sK5(xS, xP)), sK5(xS, xP))) | (spl15_1 | ~ spl15_24 | ~ spl15_207)), inference(subsumption_resolution, [], [f8235, f5635])).
fof(f8235, plain, ((xP = sdtpldt0(sdtmndt0(xP, sK5(xS, xP)), sK5(xS, xP))) | ~ aSet0(xS) | (spl15_1 | ~ spl15_24)), inference(subsumption_resolution, [], [f8176, f781])).
fof(f8176, plain, (~ aSet0(xP) | (xP = sdtpldt0(sdtmndt0(xP, sK5(xS, xP)), sK5(xS, xP))) | ~ aSet0(xS) | spl15_1), inference(resolution, [], [f1010, f387])).
fof(f1010, plain, ! [X6, X5] : (aSubsetOf0(X5, X6) | ~ aSet0(X5) | (sdtpldt0(sdtmndt0(X5, sK5(X6, X5)), sK5(X6, X5)) = X5) | ~ aSet0(X6)), inference(duplicate_literal_removal, [], [f1007])).
fof(f1007, plain, ! [X6, X5] : ((sdtpldt0(sdtmndt0(X5, sK5(X6, X5)), sK5(X6, X5)) = X5) | ~ aSet0(X5) | aSubsetOf0(X5, X6) | ~ aSet0(X5) | ~ aSet0(X6)), inference(resolution, [], [f262, f232])).
fof(f742, plain, ! [X0, X1] : (aElementOf0(X0, sdtpldt0(X1, X0)) | ~ aElement0(X0) | ~ sP1(X1, X0)), inference(resolution, [], [f355, f354])).
fof(f355, plain, ! [X4, X2, X1] : (~ sP0(X4, X1, X2) | ~ aElement0(X4) | aElementOf0(X4, X2)), inference(equality_resolution, [], [f244])).
fof(f244, plain, ! [X4, X2, X0, X1] : (aElementOf0(X4, X2) | ~ (X0 = X4) | ~ aElement0(X4) | ~ sP0(X0, X1, X2)), inference(cnf_transformation, [], [f184])).
fof(f611509, plain, (~ spl15_1335 | ~ spl15_1336 | spl15_1338), inference(avatar_contradiction_clause, [], [f611508])).
fof(f611508, plain, ($false | (~ spl15_1335 | ~ spl15_1336 | spl15_1338)), inference(subsumption_resolution, [], [f611507, f34955])).
fof(f34955, plain, (aSet0(sdtmndt0(xP, sK5(xS, xP))) | ~ spl15_1336), inference(avatar_component_clause, [], [f34954])).
fof(f34954, plain, (spl15_1336 <=> aSet0(sdtmndt0(xP, sK5(xS, xP)))), introduced(avatar_definition, [new_symbols(naming, [spl15_1336])])).
fof(f611507, plain, (~ aSet0(sdtmndt0(xP, sK5(xS, xP))) | (~ spl15_1335 | spl15_1338)), inference(subsumption_resolution, [], [f611506, f34951])).
fof(f611506, plain, (~ aElement0(sK5(xS, xP)) | ~ aSet0(sdtmndt0(xP, sK5(xS, xP))) | spl15_1338), inference(resolution, [], [f34965, f249])).
fof(f249, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f165])).
fof(f165, plain, ! [X0, X1] : (sP1(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f96, e164, e163])).
fof(f96, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f95])).
fof(f95, plain, ! [X0, X1] : (! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f15])).
fof(f15, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtpldt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (((X1 = X3) | aElementOf0(X3, X0)) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mDefCons)).
fof(f34965, plain, (~ sP1(sdtmndt0(xP, sK5(xS, xP)), sK5(xS, xP)) | spl15_1338), inference(avatar_component_clause, [], [f34963])).
fof(f50346, plain, (~ spl15_207 | ~ spl15_1335 | spl15_1343), inference(avatar_contradiction_clause, [], [f50345])).
fof(f50345, plain, ($false | (~ spl15_207 | ~ spl15_1335 | spl15_1343)), inference(subsumption_resolution, [], [f50344, f5635])).
fof(f50344, plain, (~ aSet0(xS) | (~ spl15_1335 | spl15_1343)), inference(subsumption_resolution, [], [f50343, f34951])).
fof(f50343, plain, (~ aElement0(sK5(xS, xP)) | ~ aSet0(xS) | spl15_1343), inference(resolution, [], [f50332, f249])).
fof(f50332, plain, (~ sP1(xS, sK5(xS, xP)) | spl15_1343), inference(resolution, [], [f35006, f730])).
fof(f730, plain, ! [X4, X3] : (aSet0(sdtpldt0(X3, X4)) | ~ sP1(X3, X4)), inference(resolution, [], [f354, f240])).
fof(f240, plain, ! [X2, X0, X1] : (~ sP0(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f184])).
fof(f35006, plain, (~ aSet0(sdtpldt0(xS, sK5(xS, xP))) | spl15_1343), inference(avatar_component_clause, [], [f35004])).
fof(f47246, plain, (~ spl15_24 | ~ spl15_1335 | spl15_1336), inference(avatar_contradiction_clause, [], [f47245])).
fof(f47245, plain, ($false | (~ spl15_24 | ~ spl15_1335 | spl15_1336)), inference(subsumption_resolution, [], [f47244, f781])).
fof(f47244, plain, (~ aSet0(xP) | (~ spl15_1335 | spl15_1336)), inference(subsumption_resolution, [], [f47243, f34951])).
fof(f47243, plain, (~ aElement0(sK5(xS, xP)) | ~ aSet0(xP) | spl15_1336), inference(resolution, [], [f47216, f261])).
fof(f47216, plain, (~ sP3(xP, sK5(xS, xP)) | spl15_1336), inference(resolution, [], [f34956, f746])).
fof(f746, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f356, f252])).
fof(f252, plain, ! [X2, X0, X1] : (~ sP2(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f190])).
fof(f34956, plain, (~ aSet0(sdtmndt0(xP, sK5(xS, xP))) | spl15_1336), inference(avatar_component_clause, [], [f34954])).
fof(f34991, plain, (spl15_1 | ~ spl15_24 | ~ spl15_207 | spl15_1335), inference(avatar_contradiction_clause, [], [f34990])).
fof(f34990, plain, ($false | (spl15_1 | ~ spl15_24 | ~ spl15_207 | spl15_1335)), inference(subsumption_resolution, [], [f34989, f387])).
fof(f34989, plain, (aSubsetOf0(xP, xS) | (~ spl15_24 | ~ spl15_207 | spl15_1335)), inference(subsumption_resolution, [], [f34988, f5635])).
fof(f34988, plain, (~ aSet0(xS) | aSubsetOf0(xP, xS) | (~ spl15_24 | spl15_1335)), inference(subsumption_resolution, [], [f34987, f781])).
fof(f34987, plain, (~ aSet0(xP) | ~ aSet0(xS) | aSubsetOf0(xP, xS) | spl15_1335), inference(resolution, [], [f34952, f947])).
fof(f34952, plain, (~ aElement0(sK5(xS, xP)) | spl15_1335), inference(avatar_component_clause, [], [f34950])).
fof(f5664, plain, spl15_207, inference(avatar_split_clause, [], [f333, f5634])).
fof(f3127, plain, (~ spl15_7 | spl15_84), inference(avatar_contradiction_clause, [], [f3126])).
fof(f3126, plain, ($false | (~ spl15_7 | spl15_84)), inference(subsumption_resolution, [], [f3125, f467])).
fof(f3125, plain, (~ aSet0(sdtmndt0(xQ, xy)) | spl15_84), inference(subsumption_resolution, [], [f3124, f343])).
fof(f343, plain, aElement0(xy), inference(cnf_transformation, [], [f67])).
fof(f3124, plain, (~ aElement0(xy) | ~ aSet0(sdtmndt0(xQ, xy)) | spl15_84), inference(resolution, [], [f2464, f249])).
fof(f2464, plain, (~ sP1(sdtmndt0(xQ, xy), xy) | spl15_84), inference(avatar_component_clause, [], [f2462])).
fof(f2562, plain, (~ spl15_24 | spl15_91), inference(avatar_contradiction_clause, [], [f2561])).
fof(f2561, plain, ($false | (~ spl15_24 | spl15_91)), inference(subsumption_resolution, [], [f2560, f781])).
fof(f2560, plain, (~ aSet0(xP) | spl15_91), inference(subsumption_resolution, [], [f2559, f414])).
fof(f2559, plain, (~ aElement0(xx) | ~ aSet0(xP) | spl15_91), inference(resolution, [], [f2521, f261])).
fof(f2521, plain, (~ sP3(xP, xx) | spl15_91), inference(avatar_component_clause, [], [f2519])).
fof(f1433, plain, (spl15_2 | ~ spl15_7 | ~ spl15_20 | ~ spl15_24 | ~ spl15_42), inference(avatar_split_clause, [], [f1432, f985, f779, f737, f466, f389])).
fof(f389, plain, (spl15_2 <=> (xk = sbrdtbr0(xP))), introduced(avatar_definition, [new_symbols(naming, [spl15_2])])).
fof(f985, plain, (spl15_42 <=> isFinite0(xP)), introduced(avatar_definition, [new_symbols(naming, [spl15_42])])).
fof(f1432, plain, ((xk = sbrdtbr0(xP)) | (~ spl15_7 | ~ spl15_20 | ~ spl15_24 | ~ spl15_42)), inference(forward_demodulation, [], [f1431, f349])).
fof(f349, plain, (xk = szszuzczcdt0(sbrdtbr0(sdtmndt0(xQ, xy)))), inference(cnf_transformation, [], [f71])).
fof(f1431, plain, ((szszuzczcdt0(sbrdtbr0(sdtmndt0(xQ, xy))) = sbrdtbr0(xP)) | (~ spl15_7 | ~ spl15_20 | ~ spl15_24 | ~ spl15_42)), inference(forward_demodulation, [], [f1430, f1202])).
fof(f1430, plain, ((sbrdtbr0(xP) = szszuzczcdt0(sbrdtbr0(sdtmndt0(xP, xx)))) | (~ spl15_20 | ~ spl15_24 | ~ spl15_42)), inference(subsumption_resolution, [], [f1429, f781])).
fof(f1429, plain, ((sbrdtbr0(xP) = szszuzczcdt0(sbrdtbr0(sdtmndt0(xP, xx)))) | ~ aSet0(xP) | (~ spl15_20 | ~ spl15_42)), inference(subsumption_resolution, [], [f1399, f987])).
fof(f987, plain, (isFinite0(xP) | ~ spl15_42), inference(avatar_component_clause, [], [f985])).
fof(f1399, plain, ((sbrdtbr0(xP) = szszuzczcdt0(sbrdtbr0(sdtmndt0(xP, xx)))) | ~ isFinite0(xP) | ~ aSet0(xP) | ~ spl15_20), inference(resolution, [], [f292, f1329])).
fof(f1329, plain, (aElementOf0(xx, xP) | ~ spl15_20), inference(subsumption_resolution, [], [f1326, f414])).
fof(f1326, plain, (~ aElement0(xx) | aElementOf0(xx, xP) | ~ spl15_20), inference(resolution, [], [f739, f355])).
fof(f292, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | (sbrdtbr0(X0) = szszuzczcdt0(sbrdtbr0(sdtmndt0(X0, X1)))) | ~ isFinite0(X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f134])).
fof(f134, plain, ! [X0] : (! [X1] : ((sbrdtbr0(X0) = szszuzczcdt0(sbrdtbr0(sdtmndt0(X0, X1)))) | ~ aElementOf0(X1, X0) | ~ isFinite0(X0)) | ~ aSet0(X0)), inference(flattening, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : ((sbrdtbr0(X0) = szszuzczcdt0(sbrdtbr0(sdtmndt0(X0, X1)))) | (~ aElementOf0(X1, X0) | ~ isFinite0(X0))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f44])).
fof(f44, plain, ! [X0] : (aSet0(X0) => ! [X1] : ((aElementOf0(X1, X0) & isFinite0(X0)) => (sbrdtbr0(X0) = szszuzczcdt0(sbrdtbr0(sdtmndt0(X0, X1)))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mCardDiff)).
fof(f988, plain, (~ spl15_8 | spl15_42 | ~ spl15_7), inference(avatar_split_clause, [], [f983, f466, f985, f470])).
fof(f470, plain, (spl15_8 <=> isFinite0(sdtmndt0(xQ, xy))), introduced(avatar_definition, [new_symbols(naming, [spl15_8])])).
fof(f983, plain, (isFinite0(xP) | ~ isFinite0(sdtmndt0(xQ, xy)) | ~ spl15_7), inference(subsumption_resolution, [], [f982, f414])).
fof(f982, plain, (isFinite0(xP) | ~ isFinite0(sdtmndt0(xQ, xy)) | ~ aElement0(xx) | ~ spl15_7), inference(subsumption_resolution, [], [f797, f467])).
fof(f797, plain, (isFinite0(xP) | ~ isFinite0(sdtmndt0(xQ, xy)) | ~ aSet0(sdtmndt0(xQ, xy)) | ~ aElement0(xx)), inference(superposition, [], [f266, f347])).
fof(f266, plain, ! [X0, X1] : (isFinite0(sdtpldt0(X1, X0)) | ~ isFinite0(X1) | ~ aSet0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f107])).
fof(f107, plain, ! [X0] : (! [X1] : (isFinite0(sdtpldt0(X1, X0)) | ~ isFinite0(X1) | ~ aSet0(X1)) | ~ aElement0(X0)), inference(flattening, [], [f106])).
fof(f106, plain, ! [X0] : (! [X1] : (isFinite0(sdtpldt0(X1, X0)) | (~ isFinite0(X1) | ~ aSet0(X1))) | ~ aElement0(X0)), inference(ennf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : (aElement0(X0) => ! [X1] : ((isFinite0(X1) & aSet0(X1)) => isFinite0(sdtpldt0(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mFConsSet)).
fof(f962, plain, spl15_8, inference(avatar_contradiction_clause, [], [f961])).
fof(f961, plain, ($false | spl15_8), inference(subsumption_resolution, [], [f960, f343])).
fof(f960, plain, (~ aElement0(xy) | spl15_8), inference(subsumption_resolution, [], [f959, f340])).
fof(f959, plain, (~ aSet0(xQ) | ~ aElement0(xy) | spl15_8), inference(subsumption_resolution, [], [f958, f341])).
fof(f341, plain, isFinite0(xQ), inference(cnf_transformation, [], [f66])).
fof(f958, plain, (~ isFinite0(xQ) | ~ aSet0(xQ) | ~ aElement0(xy) | spl15_8), inference(resolution, [], [f472, f267])).
fof(f267, plain, ! [X0, X1] : (isFinite0(sdtmndt0(X1, X0)) | ~ isFinite0(X1) | ~ aSet0(X1) | ~ aElement0(X0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0] : (! [X1] : (isFinite0(sdtmndt0(X1, X0)) | ~ isFinite0(X1) | ~ aSet0(X1)) | ~ aElement0(X0)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0] : (! [X1] : (isFinite0(sdtmndt0(X1, X0)) | (~ isFinite0(X1) | ~ aSet0(X1))) | ~ aElement0(X0)), inference(ennf_transformation, [], [f22])).
fof(f22, plain, ! [X0] : (aElement0(X0) => ! [X1] : ((isFinite0(X1) & aSet0(X1)) => isFinite0(sdtmndt0(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', mFDiffSet)).
fof(f472, plain, (~ isFinite0(sdtmndt0(xQ, xy)) | spl15_8), inference(avatar_component_clause, [], [f470])).
fof(f782, plain, (~ spl15_19 | spl15_24), inference(avatar_split_clause, [], [f743, f779, f733])).
fof(f733, plain, (spl15_19 <=> sP1(sdtmndt0(xQ, xy), xx)), introduced(avatar_definition, [new_symbols(naming, [spl15_19])])).
fof(f743, plain, (aSet0(xP) | ~ sP1(sdtmndt0(xQ, xy), xx)), inference(superposition, [], [f730, f347])).
fof(f776, plain, (~ spl15_7 | spl15_19), inference(avatar_split_clause, [], [f775, f733, f466])).
fof(f775, plain, (~ aSet0(sdtmndt0(xQ, xy)) | spl15_19), inference(subsumption_resolution, [], [f741, f414])).
fof(f741, plain, (~ aElement0(xx) | ~ aSet0(sdtmndt0(xQ, xy)) | spl15_19), inference(resolution, [], [f735, f249])).
fof(f735, plain, (~ sP1(sdtmndt0(xQ, xy), xx) | spl15_19), inference(avatar_component_clause, [], [f733])).
fof(f774, plain, spl15_7, inference(avatar_contradiction_clause, [], [f773])).
fof(f773, plain, ($false | spl15_7), inference(subsumption_resolution, [], [f772, f340])).
fof(f772, plain, (~ aSet0(xQ) | spl15_7), inference(subsumption_resolution, [], [f771, f343])).
fof(f771, plain, (~ aElement0(xy) | ~ aSet0(xQ) | spl15_7), inference(resolution, [], [f747, f261])).
fof(f747, plain, (~ sP3(xQ, xy) | spl15_7), inference(resolution, [], [f746, f468])).
fof(f468, plain, (~ aSet0(sdtmndt0(xQ, xy)) | spl15_7), inference(avatar_component_clause, [], [f466])).
fof(f740, plain, (~ spl15_19 | spl15_20), inference(avatar_split_clause, [], [f731, f737, f733])).
fof(f731, plain, (sP0(xx, sdtmndt0(xQ, xy), xP) | ~ sP1(sdtmndt0(xQ, xy), xx)), inference(superposition, [], [f354, f347])).
fof(f392, plain, (~ spl15_1 | ~ spl15_2), inference(avatar_split_clause, [], [f350, f389, f385])).
fof(f350, plain, (~ (xk = sbrdtbr0(xP)) | ~ aSubsetOf0(xP, xS)), inference(cnf_transformation, [], [f162])).
fof(f162, plain, (~ (xk = sbrdtbr0(xP)) | ~ aSubsetOf0(xP, xS)), inference(ennf_transformation, [], [f73])).
fof(f73, plain, ~ ((xk = sbrdtbr0(xP)) & aSubsetOf0(xP, xS)), inference(negated_conjecture, [], [f72])).
fof(f72, plain, ~ ((xk = sbrdtbr0(xP)) & aSubsetOf0(xP, xS)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM556+1.p', m__)).