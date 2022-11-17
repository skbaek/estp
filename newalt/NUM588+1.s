fof(f324114, plain, $false, inference(avatar_sat_refutation, [], [f599, f1209, f1245, f3511, f3580, f3778, f3831, f4689, f5193, f8176, f8182, f8303, f72184, f112731, f112756, f113567, f113580, f324096, f324110])).
fof(f324110, plain, (~ spl28_1 | spl28_8617), inference(avatar_contradiction_clause, [], [f324109])).
fof(f324109, plain, ($false | (~ spl28_1 | spl28_8617)), inference(subsumption_resolution, [], [f324097, f540])).
fof(f540, plain, (aSet0(sK26) | ~ spl28_1), inference(avatar_component_clause, [], [f539])).
fof(f539, plain, (spl28_1 <=> aSet0(sK26)), introduced(avatar_definition, [new_symbols(naming, [spl28_1])])).
fof(f324097, plain, (~ aSet0(sK26) | spl28_8617), inference(resolution, [], [f324025, f312])).
fof(f312, plain, ! [X0] : (aSubsetOf0(X0, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f106])).
fof(f106, plain, ! [X0] : (aSubsetOf0(X0, X0) | ~ aSet0(X0)), inference(ennf_transformation, [], [f12])).
fof(f12, plain, ! [X0] : (aSet0(X0) => aSubsetOf0(X0, X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mSubRefl)).
fof(f324025, plain, (~ aSubsetOf0(sK26, sK26) | spl28_8617), inference(avatar_component_clause, [], [f324023])).
fof(f324023, plain, (spl28_8617 <=> aSubsetOf0(sK26, sK26)), introduced(avatar_definition, [new_symbols(naming, [spl28_8617])])).
fof(f324096, plain, (~ spl28_8617 | ~ spl28_1 | ~ spl28_19 | ~ spl28_118 | ~ spl28_2882 | ~ spl28_2970), inference(avatar_split_clause, [], [f324095, f113564, f112754, f3789, f963, f539, f324023])).
fof(f963, plain, (spl28_19 <=> aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))))), introduced(avatar_definition, [new_symbols(naming, [spl28_19])])).
fof(f3789, plain, (spl28_118 <=> aSubsetOf0(sK27, sK26)), introduced(avatar_definition, [new_symbols(naming, [spl28_118])])).
fof(f112754, plain, (spl28_2882 <=> ! [X1, X0] : (~ aSet0(X0) | aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, sK26))), introduced(avatar_definition, [new_symbols(naming, [spl28_2882])])).
fof(f113564, plain, (spl28_2970 <=> aElementOf0(xk, szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl28_2970])])).
fof(f324095, plain, (~ aSubsetOf0(sK26, sK26) | (~ spl28_1 | ~ spl28_19 | ~ spl28_118 | ~ spl28_2882 | ~ spl28_2970)), inference(subsumption_resolution, [], [f324094, f540])).
fof(f324094, plain, (~ aSet0(sK26) | ~ aSubsetOf0(sK26, sK26) | (~ spl28_1 | ~ spl28_19 | ~ spl28_118 | ~ spl28_2882 | ~ spl28_2970)), inference(subsumption_resolution, [], [f323922, f161771])).
fof(f161771, plain, (~ aSubsetOf0(sK27, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | (~ spl28_1 | ~ spl28_19 | ~ spl28_2970)), inference(subsumption_resolution, [], [f161770, f964])).
fof(f964, plain, (aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ spl28_19), inference(avatar_component_clause, [], [f963])).
fof(f161770, plain, (~ aSubsetOf0(sK27, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | (~ spl28_1 | ~ spl28_2970)), inference(subsumption_resolution, [], [f161745, f114526])).
fof(f114526, plain, ~ aElementOf0(sK27, szDzozmdt0(sdtlpdtrp0(xC, sK25))), inference(backward_demodulation, [], [f480, f114370])).
fof(f114370, plain, (slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))), xk) = szDzozmdt0(sdtlpdtrp0(xC, sK25))), inference(resolution, [], [f472, f475])).
fof(f475, plain, aElementOf0(sK25, szNzAzT0), inference(cnf_transformation, [], [f299])).
fof(f299, plain, (((~ aElementOf0(sK27, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))), xk)) & aElementOf0(sK27, slbdtsldtrb0(sK26, xk)) & aSet0(sK27)) & isCountable0(sK26) & aSubsetOf0(sK26, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))))) & aElementOf0(sK25, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK25, sK26, sK27])], [f211, f298, f297, f296])).
fof(f296, plain, (? [X0] : (? [X1] : (? [X2] : (~ aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk)) & aElementOf0(X2, slbdtsldtrb0(X1, xk)) & aSet0(X2)) & isCountable0(X1) & aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) & aElementOf0(X0, szNzAzT0)) => (? [X1] : (? [X2] : (~ aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))), xk)) & aElementOf0(X2, slbdtsldtrb0(X1, xk)) & aSet0(X2)) & isCountable0(X1) & aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))))) & aElementOf0(sK25, szNzAzT0))), introduced(choice_axiom, [])).
fof(f297, plain, (? [X1] : (? [X2] : (~ aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))), xk)) & aElementOf0(X2, slbdtsldtrb0(X1, xk)) & aSet0(X2)) & isCountable0(X1) & aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))))) => (? [X2] : (~ aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))), xk)) & aElementOf0(X2, slbdtsldtrb0(sK26, xk)) & aSet0(X2)) & isCountable0(sK26) & aSubsetOf0(sK26, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))))), introduced(choice_axiom, [])).
fof(f298, plain, (? [X2] : (~ aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))), xk)) & aElementOf0(X2, slbdtsldtrb0(sK26, xk)) & aSet0(X2)) => (~ aElementOf0(sK27, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))), xk)) & aElementOf0(sK27, slbdtsldtrb0(sK26, xk)) & aSet0(sK27))), introduced(choice_axiom, [])).
fof(f211, plain, ? [X0] : (? [X1] : (? [X2] : (~ aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk)) & aElementOf0(X2, slbdtsldtrb0(X1, xk)) & aSet0(X2)) & isCountable0(X1) & aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) & aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f210])).
fof(f210, plain, ? [X0] : (? [X1] : (? [X2] : (~ aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk)) & (aElementOf0(X2, slbdtsldtrb0(X1, xk)) & aSet0(X2))) & (isCountable0(X1) & aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0)))))) & aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f89])).
fof(f89, plain, ~ ! [X0] : (aElementOf0(X0, szNzAzT0) => ! [X1] : ((isCountable0(X1) & aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) => ! [X2] : ((aElementOf0(X2, slbdtsldtrb0(X1, xk)) & aSet0(X2)) => aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk))))), inference(negated_conjecture, [], [f88])).
fof(f88, plain, ~ ! [X0] : (aElementOf0(X0, szNzAzT0) => ! [X1] : ((isCountable0(X1) & aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) => ! [X2] : ((aElementOf0(X2, slbdtsldtrb0(X1, xk)) & aSet0(X2)) => aElementOf0(X2, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', m__)).
fof(f472, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | (slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk) = szDzozmdt0(sdtlpdtrp0(xC, X0)))), inference(cnf_transformation, [], [f208])).
fof(f208, plain, (! [X0] : ((! [X1] : ((sdtlpdtrp0(sdtlpdtrp0(xC, X0), X1) = sdtlpdtrp0(xc, sdtpldt0(X1, szmzizndt0(sdtlpdtrp0(xN, X0))))) | ~ aElementOf0(X1, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk)) | ~ aSet0(X1)) & (slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk) = szDzozmdt0(sdtlpdtrp0(xC, X0))) & aFunction0(sdtlpdtrp0(xC, X0))) | ~ aElementOf0(X0, szNzAzT0)) & (szNzAzT0 = szDzozmdt0(xC)) & aFunction0(xC)), inference(flattening, [], [f207])).
fof(f207, plain, (! [X0] : ((! [X1] : ((sdtlpdtrp0(sdtlpdtrp0(xC, X0), X1) = sdtlpdtrp0(xc, sdtpldt0(X1, szmzizndt0(sdtlpdtrp0(xN, X0))))) | (~ aElementOf0(X1, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk)) | ~ aSet0(X1))) & (slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk) = szDzozmdt0(sdtlpdtrp0(xC, X0))) & aFunction0(sdtlpdtrp0(xC, X0))) | ~ aElementOf0(X0, szNzAzT0)) & (szNzAzT0 = szDzozmdt0(xC)) & aFunction0(xC)), inference(ennf_transformation, [], [f86])).
fof(f86, plain, (! [X0] : (aElementOf0(X0, szNzAzT0) => (! [X1] : ((aElementOf0(X1, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk)) & aSet0(X1)) => (sdtlpdtrp0(sdtlpdtrp0(xC, X0), X1) = sdtlpdtrp0(xc, sdtpldt0(X1, szmzizndt0(sdtlpdtrp0(xN, X0)))))) & (slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))), xk) = szDzozmdt0(sdtlpdtrp0(xC, X0))) & aFunction0(sdtlpdtrp0(xC, X0)))) & (szNzAzT0 = szDzozmdt0(xC)) & aFunction0(xC)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', m__4151)).
fof(f480, plain, ~ aElementOf0(sK27, slbdtsldtrb0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))), xk)), inference(cnf_transformation, [], [f299])).
fof(f161745, plain, (aElementOf0(sK27, szDzozmdt0(sdtlpdtrp0(xC, sK25))) | ~ aSubsetOf0(sK27, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | (~ spl28_1 | ~ spl28_2970)), inference(superposition, [], [f115161, f114370])).
fof(f115161, plain, (! [X3] : (aElementOf0(sK27, slbdtsldtrb0(X3, xk)) | ~ aSubsetOf0(sK27, X3) | ~ aSet0(X3)) | (~ spl28_1 | ~ spl28_2970)), inference(subsumption_resolution, [], [f115135, f113565])).
fof(f113565, plain, (aElementOf0(xk, szNzAzT0) | ~ spl28_2970), inference(avatar_component_clause, [], [f113564])).
fof(f115135, plain, (! [X3] : (aElementOf0(sK27, slbdtsldtrb0(X3, xk)) | ~ aSubsetOf0(sK27, X3) | ~ aElementOf0(xk, szNzAzT0) | ~ aSet0(X3)) | (~ spl28_1 | ~ spl28_2970)), inference(superposition, [], [f499, f114234])).
fof(f114234, plain, ((xk = sbrdtbr0(sK27)) | (~ spl28_1 | ~ spl28_2970)), inference(subsumption_resolution, [], [f114233, f540])).
fof(f114233, plain, ((xk = sbrdtbr0(sK27)) | ~ aSet0(sK26) | ~ spl28_2970), inference(subsumption_resolution, [], [f114228, f113565])).
fof(f114228, plain, ((xk = sbrdtbr0(sK27)) | ~ aElementOf0(xk, szNzAzT0) | ~ aSet0(sK26)), inference(resolution, [], [f479, f500])).
fof(f500, plain, ! [X4, X0, X1] : (~ aElementOf0(X4, slbdtsldtrb0(X0, X1)) | (sbrdtbr0(X4) = X1) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f402])).
fof(f402, plain, ! [X4, X2, X0, X1] : ((sbrdtbr0(X4) = X1) | ~ aElementOf0(X4, X2) | ~ (slbdtsldtrb0(X0, X1) = X2) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f271])).
fof(f271, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ((~ (sbrdtbr0(sK14(X0, X1, X2)) = X1) | ~ aSubsetOf0(sK14(X0, X1, X2), X0) | ~ aElementOf0(sK14(X0, X1, X2), X2)) & (((sbrdtbr0(sK14(X0, X1, X2)) = X1) & aSubsetOf0(sK14(X0, X1, X2), X0)) | aElementOf0(sK14(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0)) & (((sbrdtbr0(X4) = X1) & aSubsetOf0(X4, X0)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK14])], [f269, f270])).
fof(f270, plain, ! [X2, X1, X0] : (? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) => ((~ (sbrdtbr0(sK14(X0, X1, X2)) = X1) | ~ aSubsetOf0(sK14(X0, X1, X2), X0) | ~ aElementOf0(sK14(X0, X1, X2), X2)) & (((sbrdtbr0(sK14(X0, X1, X2)) = X1) & aSubsetOf0(sK14(X0, X1, X2), X0)) | aElementOf0(sK14(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f269, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0)) & (((sbrdtbr0(X4) = X1) & aSubsetOf0(X4, X0)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(rectify, [], [f268])).
fof(f268, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | ? [X3] : ((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | ~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(flattening, [], [f267])).
fof(f267, plain, ! [X0, X1] : (! [X2] : (((slbdtsldtrb0(X0, X1) = X2) | (? [X3] : (((~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2)) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | (~ (sbrdtbr0(X3) = X1) | ~ aSubsetOf0(X3, X0))) & (((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ (slbdtsldtrb0(X0, X1) = X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(nnf_transformation, [], [f172])).
fof(f172, plain, ! [X0, X1] : (! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2))) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(flattening, [], [f171])).
fof(f171, plain, ! [X0, X1] : (! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2))) | (~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0))), inference(ennf_transformation, [], [f57])).
fof(f57, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aSet0(X0)) => ! [X2] : ((slbdtsldtrb0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> ((sbrdtbr0(X3) = X1) & aSubsetOf0(X3, X0))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mDefSel)).
fof(f479, plain, aElementOf0(sK27, slbdtsldtrb0(sK26, xk)), inference(cnf_transformation, [], [f299])).
fof(f499, plain, ! [X4, X0] : (aElementOf0(X4, slbdtsldtrb0(X0, sbrdtbr0(X4))) | ~ aSubsetOf0(X4, X0) | ~ aElementOf0(sbrdtbr0(X4), szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f498])).
fof(f498, plain, ! [X4, X2, X0] : (aElementOf0(X4, X2) | ~ aSubsetOf0(X4, X0) | ~ (slbdtsldtrb0(X0, sbrdtbr0(X4)) = X2) | ~ aElementOf0(sbrdtbr0(X4), szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f403])).
fof(f403, plain, ! [X4, X2, X0, X1] : (aElementOf0(X4, X2) | ~ (sbrdtbr0(X4) = X1) | ~ aSubsetOf0(X4, X0) | ~ (slbdtsldtrb0(X0, X1) = X2) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f271])).
fof(f323922, plain, (aSubsetOf0(sK27, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSet0(sK26) | ~ aSubsetOf0(sK26, sK26) | (~ spl28_118 | ~ spl28_2882)), inference(resolution, [], [f112755, f3791])).
fof(f3791, plain, (aSubsetOf0(sK27, sK26) | ~ spl28_118), inference(avatar_component_clause, [], [f3789])).
fof(f112755, plain, (! [X0, X1] : (~ aSubsetOf0(X1, X0) | aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSet0(X0) | ~ aSubsetOf0(X0, sK26)) | ~ spl28_2882), inference(avatar_component_clause, [], [f112754])).
fof(f113580, plain, spl28_2970, inference(avatar_split_clause, [], [f457, f113564])).
fof(f457, plain, aElementOf0(xk, szNzAzT0), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((xK = szszuzczcdt0(xk)) & aElementOf0(xk, szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', m__3533)).
fof(f113567, plain, (~ spl28_2970 | spl28_118 | ~ spl28_1), inference(avatar_split_clause, [], [f113562, f539, f3789, f113564])).
fof(f113562, plain, (aSubsetOf0(sK27, sK26) | ~ aElementOf0(xk, szNzAzT0) | ~ spl28_1), inference(subsumption_resolution, [], [f1541, f540])).
fof(f1541, plain, (aSubsetOf0(sK27, sK26) | ~ aElementOf0(xk, szNzAzT0) | ~ aSet0(sK26)), inference(resolution, [], [f501, f479])).
fof(f501, plain, ! [X4, X0, X1] : (~ aElementOf0(X4, slbdtsldtrb0(X0, X1)) | aSubsetOf0(X4, X0) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(equality_resolution, [], [f401])).
fof(f401, plain, ! [X4, X2, X0, X1] : (aSubsetOf0(X4, X0) | ~ aElementOf0(X4, X2) | ~ (slbdtsldtrb0(X0, X1) = X2) | ~ aElementOf0(X1, szNzAzT0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f271])).
fof(f112756, plain, (~ spl28_19 | spl28_2882 | ~ spl28_309), inference(avatar_split_clause, [], [f112752, f8180, f112754, f963])).
fof(f8180, plain, (spl28_309 <=> ! [X0] : (aSubsetOf0(X0, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSet0(X0) | ~ aSubsetOf0(X0, sK26))), introduced(avatar_definition, [new_symbols(naming, [spl28_309])])).
fof(f112752, plain, (! [X0, X1] : (~ aSet0(X0) | ~ aSubsetOf0(X0, sK26) | aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSubsetOf0(X1, X0) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))))) | ~ spl28_309), inference(subsumption_resolution, [], [f70652, f307])).
fof(f307, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f227])).
fof(f227, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f225, f226])).
fof(f226, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f225, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f224])).
fof(f224, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f223])).
fof(f223, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f103])).
fof(f103, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mDefSub)).
fof(f70652, plain, (! [X0, X1] : (~ aSet0(X0) | ~ aSubsetOf0(X0, sK26) | aSubsetOf0(X1, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSubsetOf0(X1, X0) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSet0(X1)) | ~ spl28_309), inference(resolution, [], [f8181, f521])).
fof(f521, plain, ! [X2, X0, X1] : (~ aSubsetOf0(X1, X2) | aSubsetOf0(X0, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f314, f307])).
fof(f314, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, X2) | ~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f110])).
fof(f110, plain, ! [X0, X1, X2] : (aSubsetOf0(X0, X2) | ~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f109])).
fof(f109, plain, ! [X0, X1, X2] : ((aSubsetOf0(X0, X2) | (~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1))) | (~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ! [X0, X1, X2] : ((aSet0(X2) & aSet0(X1) & aSet0(X0)) => ((aSubsetOf0(X1, X2) & aSubsetOf0(X0, X1)) => aSubsetOf0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mSubTrans)).
fof(f8181, plain, (! [X0] : (aSubsetOf0(X0, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSet0(X0) | ~ aSubsetOf0(X0, sK26)) | ~ spl28_309), inference(avatar_component_clause, [], [f8180])).
fof(f112731, plain, (spl28_178 | spl28_284), inference(avatar_contradiction_clause, [], [f112730])).
fof(f112730, plain, ($false | (spl28_178 | spl28_284)), inference(subsumption_resolution, [], [f112729, f475])).
fof(f112729, plain, (~ aElementOf0(sK25, szNzAzT0) | (spl28_178 | spl28_284)), inference(subsumption_resolution, [], [f112714, f8056])).
fof(f8056, plain, (~ (slcrc0 = sdtlpdtrp0(xN, sK25)) | spl28_284), inference(avatar_component_clause, [], [f8055])).
fof(f8055, plain, (spl28_284 <=> (slcrc0 = sdtlpdtrp0(xN, sK25))), introduced(avatar_definition, [new_symbols(naming, [spl28_284])])).
fof(f112714, plain, ((slcrc0 = sdtlpdtrp0(xN, sK25)) | ~ aElementOf0(sK25, szNzAzT0) | spl28_178), inference(resolution, [], [f5970, f5197])).
fof(f5197, plain, (~ aElement0(szmzizndt0(sdtlpdtrp0(xN, sK25))) | spl28_178), inference(avatar_component_clause, [], [f5195])).
fof(f5195, plain, (spl28_178 <=> aElement0(szmzizndt0(sdtlpdtrp0(xN, sK25)))), introduced(avatar_definition, [new_symbols(naming, [spl28_178])])).
fof(f5970, plain, ! [X0] : (aElement0(szmzizndt0(sdtlpdtrp0(xN, X0))) | (slcrc0 = sdtlpdtrp0(xN, X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f5956, f619])).
fof(f619, plain, ! [X0] : (aSet0(sdtlpdtrp0(xN, X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f617, f345])).
fof(f345, plain, aSet0(szNzAzT0), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (isCountable0(szNzAzT0) & aSet0(szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mNATSet)).
fof(f617, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | aSet0(sdtlpdtrp0(xN, X0)) | ~ aSet0(szNzAzT0)), inference(resolution, [], [f464, f307])).
fof(f464, plain, ! [X0] : (aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ! [X0] : ((isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f82])).
fof(f82, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', m__3671)).
fof(f5956, plain, ! [X0] : ((slcrc0 = sdtlpdtrp0(xN, X0)) | aElement0(szmzizndt0(sdtlpdtrp0(xN, X0))) | ~ aSet0(sdtlpdtrp0(xN, X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(resolution, [], [f1031, f464])).
fof(f1031, plain, ! [X0] : (~ aSubsetOf0(X0, szNzAzT0) | (slcrc0 = X0) | aElement0(szmzizndt0(X0)) | ~ aSet0(X0)), inference(resolution, [], [f490, f300])).
fof(f300, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mEOfElem)).
fof(f490, plain, ! [X0] : (aElementOf0(szmzizndt0(X0), X0) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(equality_resolution, [], [f374])).
fof(f374, plain, ! [X0, X1] : (aElementOf0(X1, X0) | ~ (szmzizndt0(X0) = X1) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f251])).
fof(f251, plain, ! [X0] : (! [X1] : (((szmzizndt0(X0) = X1) | (~ sdtlseqdt0(X1, sK10(X0, X1)) & aElementOf0(sK10(X0, X1), X0)) | ~ aElementOf0(X1, X0)) & ((! [X3] : (sdtlseqdt0(X1, X3) | ~ aElementOf0(X3, X0)) & aElementOf0(X1, X0)) | ~ (szmzizndt0(X0) = X1))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f249, f250])).
fof(f250, plain, ! [X1, X0] : (? [X2] : (~ sdtlseqdt0(X1, X2) & aElementOf0(X2, X0)) => (~ sdtlseqdt0(X1, sK10(X0, X1)) & aElementOf0(sK10(X0, X1), X0))), introduced(choice_axiom, [])).
fof(f249, plain, ! [X0] : (! [X1] : (((szmzizndt0(X0) = X1) | ? [X2] : (~ sdtlseqdt0(X1, X2) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0)) & ((! [X3] : (sdtlseqdt0(X1, X3) | ~ aElementOf0(X3, X0)) & aElementOf0(X1, X0)) | ~ (szmzizndt0(X0) = X1))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(rectify, [], [f248])).
fof(f248, plain, ! [X0] : (! [X1] : (((szmzizndt0(X0) = X1) | ? [X2] : (~ sdtlseqdt0(X1, X2) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0)) & ((! [X2] : (sdtlseqdt0(X1, X2) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0)) | ~ (szmzizndt0(X0) = X1))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(flattening, [], [f247])).
fof(f247, plain, ! [X0] : (! [X1] : (((szmzizndt0(X0) = X1) | (? [X2] : (~ sdtlseqdt0(X1, X2) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0))) & ((! [X2] : (sdtlseqdt0(X1, X2) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0)) | ~ (szmzizndt0(X0) = X1))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(nnf_transformation, [], [f157])).
fof(f157, plain, ! [X0] : (! [X1] : ((szmzizndt0(X0) = X1) <=> (! [X2] : (sdtlseqdt0(X1, X2) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(flattening, [], [f156])).
fof(f156, plain, ! [X0] : (! [X1] : ((szmzizndt0(X0) = X1) <=> (! [X2] : (sdtlseqdt0(X1, X2) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0))) | ((slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f47])).
fof(f47, plain, ! [X0] : ((~ (slcrc0 = X0) & aSubsetOf0(X0, szNzAzT0)) => ! [X1] : ((szmzizndt0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X0) => sdtlseqdt0(X1, X2)) & aElementOf0(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mDefMin)).
fof(f72184, plain, (~ spl28_178 | spl28_19 | ~ spl28_138), inference(avatar_split_clause, [], [f72183, f4314, f963, f5195])).
fof(f4314, plain, (spl28_138 <=> aSet0(sdtlpdtrp0(xN, sK25))), introduced(avatar_definition, [new_symbols(naming, [spl28_138])])).
fof(f72183, plain, (~ aElement0(szmzizndt0(sdtlpdtrp0(xN, sK25))) | (spl28_19 | ~ spl28_138)), inference(subsumption_resolution, [], [f72182, f4315])).
fof(f4315, plain, (aSet0(sdtlpdtrp0(xN, sK25)) | ~ spl28_138), inference(avatar_component_clause, [], [f4314])).
fof(f72182, plain, (~ aElement0(szmzizndt0(sdtlpdtrp0(xN, sK25))) | ~ aSet0(sdtlpdtrp0(xN, sK25)) | spl28_19), inference(resolution, [], [f70389, f338])).
fof(f338, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f217])).
fof(f217, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f114, e216, e215])).
fof(f215, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e215])).
fof(e215, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f216, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e216])).
fof(e216, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f114, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f113])).
fof(f113, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mDefDiff)).
fof(f70389, plain, (~ sP3(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))) | spl28_19), inference(resolution, [], [f965, f954])).
fof(f954, plain, ! [X6, X5] : (aSet0(sdtmndt0(X5, X6)) | ~ sP3(X5, X6)), inference(resolution, [], [f486, f329])).
fof(f329, plain, ! [X2, X0, X1] : (~ sP2(X0, X1, X2) | aSet0(X2)), inference(cnf_transformation, [], [f239])).
fof(f239, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f237, f238])).
fof(f238, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f237, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f236])).
fof(f236, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f235])).
fof(f235, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f215])).
fof(f486, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f327])).
fof(f327, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f234])).
fof(f234, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f216])).
fof(f965, plain, (~ aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | spl28_19), inference(avatar_component_clause, [], [f963])).
fof(f8303, plain, (~ spl28_39 | ~ spl28_284), inference(avatar_contradiction_clause, [], [f8302])).
fof(f8302, plain, ($false | (~ spl28_39 | ~ spl28_284)), inference(subsumption_resolution, [], [f8296, f519])).
fof(f519, plain, ~ isCountable0(slcrc0), inference(subsumption_resolution, [], [f483, f482])).
fof(f482, plain, aSet0(slcrc0), inference(equality_resolution, [], [f301])).
fof(f301, plain, ! [X0] : (aSet0(X0) | ~ (slcrc0 = X0)), inference(cnf_transformation, [], [f222])).
fof(f222, plain, ! [X0] : (((slcrc0 = X0) | aElementOf0(sK4(X0), X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f220, f221])).
fof(f221, plain, ! [X0] : (? [X1] : aElementOf0(X1, X0) => aElementOf0(sK4(X0), X0)), introduced(choice_axiom, [])).
fof(f220, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(rectify, [], [f219])).
fof(f219, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(flattening, [], [f218])).
fof(f218, plain, ! [X0] : (((slcrc0 = X0) | (? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0))) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(nnf_transformation, [], [f98])).
fof(f98, plain, ! [X0] : ((slcrc0 = X0) <=> (! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : ((slcrc0 = X0) <=> (~ ? [X1] : aElementOf0(X1, X0) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mDefEmp)).
fof(f483, plain, (~ isCountable0(slcrc0) | ~ aSet0(slcrc0)), inference(equality_resolution, [], [f306])).
fof(f306, plain, ! [X0] : (~ (slcrc0 = X0) | ~ isCountable0(X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ! [X0] : (~ (slcrc0 = X0) | ~ isCountable0(X0) | ~ aSet0(X0)), inference(flattening, [], [f101])).
fof(f101, plain, ! [X0] : (~ (slcrc0 = X0) | (~ isCountable0(X0) | ~ aSet0(X0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0] : ((isCountable0(X0) & aSet0(X0)) => ~ (slcrc0 = X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mCountNFin_01)).
fof(f8296, plain, (isCountable0(slcrc0) | (~ spl28_39 | ~ spl28_284)), inference(backward_demodulation, [], [f1208, f8057])).
fof(f8057, plain, ((slcrc0 = sdtlpdtrp0(xN, sK25)) | ~ spl28_284), inference(avatar_component_clause, [], [f8055])).
fof(f1208, plain, (isCountable0(sdtlpdtrp0(xN, sK25)) | ~ spl28_39), inference(avatar_component_clause, [], [f1206])).
fof(f1206, plain, (spl28_39 <=> isCountable0(sdtlpdtrp0(xN, sK25))), introduced(avatar_definition, [new_symbols(naming, [spl28_39])])).
fof(f8182, plain, (~ spl28_19 | spl28_309), inference(avatar_split_clause, [], [f4071, f8180, f963])).
fof(f4071, plain, ! [X0] : (aSubsetOf0(X0, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSubsetOf0(X0, sK26) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))) | ~ aSet0(X0)), inference(resolution, [], [f476, f521])).
fof(f476, plain, aSubsetOf0(sK26, sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25)))), inference(cnf_transformation, [], [f299])).
fof(f8176, plain, (~ spl28_19 | spl28_1), inference(avatar_split_clause, [], [f4075, f539, f963])).
fof(f4075, plain, (aSet0(sK26) | ~ aSet0(sdtmndt0(sdtlpdtrp0(xN, sK25), szmzizndt0(sdtlpdtrp0(xN, sK25))))), inference(resolution, [], [f476, f307])).
fof(f5193, plain, spl28_138, inference(avatar_contradiction_clause, [], [f5192])).
fof(f5192, plain, ($false | spl28_138), inference(subsumption_resolution, [], [f5191, f475])).
fof(f5191, plain, (~ aElementOf0(sK25, szNzAzT0) | spl28_138), inference(resolution, [], [f4316, f619])).
fof(f4316, plain, (~ aSet0(sdtlpdtrp0(xN, sK25)) | spl28_138), inference(avatar_component_clause, [], [f4314])).
fof(f4689, plain, (~ spl28_3 | ~ spl28_109 | spl28_119), inference(avatar_contradiction_clause, [], [f4688])).
fof(f4688, plain, ($false | (~ spl28_3 | ~ spl28_109 | spl28_119)), inference(subsumption_resolution, [], [f4687, f549])).
fof(f549, plain, (aSet0(xS) | ~ spl28_3), inference(avatar_component_clause, [], [f548])).
fof(f548, plain, (spl28_3 <=> aSet0(xS)), introduced(avatar_definition, [new_symbols(naming, [spl28_3])])).
fof(f4687, plain, (~ aSet0(xS) | (~ spl28_109 | spl28_119)), inference(subsumption_resolution, [], [f4686, f3806])).
fof(f3806, plain, (aElement0(szmzizndt0(xS)) | ~ spl28_109), inference(subsumption_resolution, [], [f3752, f345])).
fof(f3752, plain, (aElement0(szmzizndt0(xS)) | ~ aSet0(szNzAzT0) | ~ spl28_109), inference(resolution, [], [f3510, f300])).
fof(f3510, plain, (aElementOf0(szmzizndt0(xS), szNzAzT0) | ~ spl28_109), inference(avatar_component_clause, [], [f3508])).
fof(f3508, plain, (spl28_109 <=> aElementOf0(szmzizndt0(xS), szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl28_109])])).
fof(f4686, plain, (~ aElement0(szmzizndt0(xS)) | ~ aSet0(xS) | spl28_119), inference(resolution, [], [f4685, f338])).
fof(f4685, plain, (~ sP3(xS, szmzizndt0(xS)) | spl28_119), inference(resolution, [], [f3801, f954])).
fof(f3801, plain, (~ aSet0(sdtmndt0(xS, szmzizndt0(xS))) | spl28_119), inference(avatar_component_clause, [], [f3799])).
fof(f3799, plain, (spl28_119 <=> aSet0(sdtmndt0(xS, szmzizndt0(xS)))), introduced(avatar_definition, [new_symbols(naming, [spl28_119])])).
fof(f3831, plain, (~ spl28_119 | spl28_1 | ~ spl28_16), inference(avatar_split_clause, [], [f1307, f925, f539, f3799])).
fof(f925, plain, (spl28_16 <=> (sz00 = sK25)), introduced(avatar_definition, [new_symbols(naming, [spl28_16])])).
fof(f1307, plain, (aSet0(sK26) | ~ aSet0(sdtmndt0(xS, szmzizndt0(xS))) | ~ spl28_16), inference(resolution, [], [f1288, f307])).
fof(f1288, plain, (aSubsetOf0(sK26, sdtmndt0(xS, szmzizndt0(xS))) | ~ spl28_16), inference(forward_demodulation, [], [f1271, f461])).
fof(f461, plain, (xS = sdtlpdtrp0(xN, sz00)), inference(cnf_transformation, [], [f199])).
fof(f199, plain, (! [X0] : ((isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) | ~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), inference(flattening, [], [f198])).
fof(f198, plain, (! [X0] : (((isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) | (~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0))) | ~ aElementOf0(X0, szNzAzT0)) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, (! [X0] : (aElementOf0(X0, szNzAzT0) => ((isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0)) => (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))))) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', m__3623)).
fof(f1271, plain, (aSubsetOf0(sK26, sdtmndt0(sdtlpdtrp0(xN, sz00), szmzizndt0(sdtlpdtrp0(xN, sz00)))) | ~ spl28_16), inference(backward_demodulation, [], [f476, f926])).
fof(f926, plain, ((sz00 = sK25) | ~ spl28_16), inference(avatar_component_clause, [], [f925])).
fof(f3778, plain, (~ spl28_3 | ~ spl28_16 | spl28_19 | ~ spl28_109), inference(avatar_contradiction_clause, [], [f3777])).
fof(f3777, plain, ($false | (~ spl28_3 | ~ spl28_16 | spl28_19 | ~ spl28_109)), inference(subsumption_resolution, [], [f3776, f345])).
fof(f3776, plain, (~ aSet0(szNzAzT0) | (~ spl28_3 | ~ spl28_16 | spl28_19 | ~ spl28_109)), inference(subsumption_resolution, [], [f3752, f1343])).
fof(f1343, plain, (~ aElement0(szmzizndt0(xS)) | (~ spl28_3 | ~ spl28_16 | spl28_19)), inference(subsumption_resolution, [], [f1342, f549])).
fof(f1342, plain, (~ aElement0(szmzizndt0(xS)) | ~ aSet0(xS) | (~ spl28_16 | spl28_19)), inference(resolution, [], [f1339, f338])).
fof(f1339, plain, (~ sP3(xS, szmzizndt0(xS)) | (~ spl28_16 | spl28_19)), inference(resolution, [], [f1293, f954])).
fof(f1293, plain, (~ aSet0(sdtmndt0(xS, szmzizndt0(xS))) | (~ spl28_16 | spl28_19)), inference(forward_demodulation, [], [f1280, f461])).
fof(f1280, plain, (~ aSet0(sdtmndt0(sdtlpdtrp0(xN, sz00), szmzizndt0(sdtlpdtrp0(xN, sz00)))) | (~ spl28_16 | spl28_19)), inference(backward_demodulation, [], [f965, f926])).
fof(f3580, plain, ~ spl28_108, inference(avatar_contradiction_clause, [], [f3579])).
fof(f3579, plain, ($false | ~ spl28_108), inference(subsumption_resolution, [], [f3535, f519])).
fof(f3535, plain, (isCountable0(slcrc0) | ~ spl28_108), inference(backward_demodulation, [], [f447, f3506])).
fof(f3506, plain, ((slcrc0 = xS) | ~ spl28_108), inference(avatar_component_clause, [], [f3504])).
fof(f3504, plain, (spl28_108 <=> (slcrc0 = xS)), introduced(avatar_definition, [new_symbols(naming, [spl28_108])])).
fof(f447, plain, isCountable0(xS), inference(cnf_transformation, [], [f75])).
fof(f75, plain, (isCountable0(xS) & aSubsetOf0(xS, szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', m__3435)).
fof(f3511, plain, (spl28_108 | spl28_109), inference(avatar_split_clause, [], [f3502, f3508, f3504])).
fof(f3502, plain, (aElementOf0(szmzizndt0(xS), szNzAzT0) | (slcrc0 = xS)), inference(subsumption_resolution, [], [f3480, f446])).
fof(f446, plain, aSubsetOf0(xS, szNzAzT0), inference(cnf_transformation, [], [f75])).
fof(f3480, plain, (aElementOf0(szmzizndt0(xS), szNzAzT0) | (slcrc0 = xS) | ~ aSubsetOf0(xS, szNzAzT0)), inference(resolution, [], [f1127, f490])).
fof(f1127, plain, ! [X11] : (~ aElementOf0(X11, xS) | aElementOf0(X11, szNzAzT0)), inference(subsumption_resolution, [], [f1121, f345])).
fof(f1121, plain, ! [X11] : (~ aElementOf0(X11, xS) | aElementOf0(X11, szNzAzT0) | ~ aSet0(szNzAzT0)), inference(resolution, [], [f308, f446])).
fof(f308, plain, ! [X0, X3, X1] : (~ aSubsetOf0(X1, X0) | ~ aElementOf0(X3, X1) | aElementOf0(X3, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f227])).
fof(f1245, plain, (spl28_16 | spl28_33), inference(avatar_contradiction_clause, [], [f1244])).
fof(f1244, plain, ($false | (spl28_16 | spl28_33)), inference(subsumption_resolution, [], [f1243, f475])).
fof(f1243, plain, (~ aElementOf0(sK25, szNzAzT0) | (spl28_16 | spl28_33)), inference(subsumption_resolution, [], [f1242, f927])).
fof(f927, plain, (~ (sz00 = sK25) | spl28_16), inference(avatar_component_clause, [], [f925])).
fof(f1242, plain, ((sz00 = sK25) | ~ aElementOf0(sK25, szNzAzT0) | spl28_33), inference(resolution, [], [f1179, f351])).
fof(f351, plain, ! [X0] : (aElementOf0(sK8(X0), szNzAzT0) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f241])).
fof(f241, plain, ! [X0] : (((szszuzczcdt0(sK8(X0)) = X0) & aElementOf0(sK8(X0), szNzAzT0)) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f130, f240])).
fof(f240, plain, ! [X0] : (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) => ((szszuzczcdt0(sK8(X0)) = X0) & aElementOf0(sK8(X0), szNzAzT0))), introduced(choice_axiom, [])).
fof(f130, plain, ! [X0] : (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f129])).
fof(f129, plain, ! [X0] : ((? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM588+1.p', mNatExtra)).
fof(f1179, plain, (~ aElementOf0(sK8(sK25), szNzAzT0) | spl28_33), inference(avatar_component_clause, [], [f1177])).
fof(f1177, plain, (spl28_33 <=> aElementOf0(sK8(sK25), szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl28_33])])).
fof(f1209, plain, (~ spl28_33 | spl28_39 | spl28_16), inference(avatar_split_clause, [], [f1174, f925, f1206, f1177])).
fof(f1174, plain, (isCountable0(sdtlpdtrp0(xN, sK25)) | ~ aElementOf0(sK8(sK25), szNzAzT0) | spl28_16), inference(superposition, [], [f797, f1143])).
fof(f1143, plain, ((sK25 = szszuzczcdt0(sK8(sK25))) | spl28_16), inference(subsumption_resolution, [], [f1136, f927])).
fof(f1136, plain, ((sz00 = sK25) | (sK25 = szszuzczcdt0(sK8(sK25)))), inference(resolution, [], [f352, f475])).
fof(f352, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | (sz00 = X0) | (szszuzczcdt0(sK8(X0)) = X0)), inference(cnf_transformation, [], [f241])).
fof(f797, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f796, f464])).
fof(f796, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f463, f465])).
fof(f465, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f200])).
fof(f463, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) | ~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f199])).
fof(f599, plain, spl28_3, inference(avatar_split_clause, [], [f598, f548])).
fof(f598, plain, aSet0(xS), inference(subsumption_resolution, [], [f594, f345])).
fof(f594, plain, (aSet0(xS) | ~ aSet0(szNzAzT0)), inference(resolution, [], [f307, f446])).