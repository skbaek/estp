fof(f159014, plain, $false, inference(avatar_sat_refutation, [], [f1260, f1264, f1870, f1937, f1985, f1986, f1988, f5331, f5354, f5463, f22914, f43382, f136703, f137016, f143069, f146139, f147634, f158993, f159013])).
fof(f159013, plain, (~ spl29_2993 | ~ spl29_52 | spl29_53 | spl29_86 | ~ spl29_88 | ~ spl29_91 | spl29_302), inference(avatar_split_clause, [], [f159012, f7535, f1949, f1932, f1883, f1403, f1399, f137648])).
fof(f137648, plain, (spl29_2993 <=> aElementOf0(xx, xQ)), introduced(avatar_definition, [new_symbols(naming, [spl29_2993])])).
fof(f1399, plain, (spl29_52 <=> aSubsetOf0(sdtlpdtrp0(xN, xm), szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl29_52])])).
fof(f1403, plain, (spl29_53 <=> (slcrc0 = sdtlpdtrp0(xN, xm))), introduced(avatar_definition, [new_symbols(naming, [spl29_53])])).
fof(f1883, plain, (spl29_86 <=> (slcrc0 = sdtlpdtrp0(xN, xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_86])])).
fof(f1932, plain, (spl29_88 <=> aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl29_88])])).
fof(f1949, plain, (spl29_91 <=> ! [X5] : (~ aElementOf0(X5, sdtlpdtrp0(xN, xn)) | aElementOf0(X5, sdtlpdtrp0(xN, xm)))), introduced(avatar_definition, [new_symbols(naming, [spl29_91])])).
fof(f7535, plain, (spl29_302 <=> (xp = xx)), introduced(avatar_definition, [new_symbols(naming, [spl29_302])])).
fof(f159012, plain, (~ aElementOf0(xx, xQ) | (~ spl29_52 | spl29_53 | spl29_86 | ~ spl29_88 | ~ spl29_91 | spl29_302)), inference(subsumption_resolution, [], [f159011, f544])).
fof(f544, plain, aSubsetOf0(xQ, szNzAzT0), inference(cnf_transformation, [], [f101])).
fof(f101, plain, aSubsetOf0(xQ, szNzAzT0), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5106)).
fof(f159011, plain, (~ aElementOf0(xx, xQ) | ~ aSubsetOf0(xQ, szNzAzT0) | (~ spl29_52 | spl29_53 | spl29_86 | ~ spl29_88 | ~ spl29_91 | spl29_302)), inference(subsumption_resolution, [], [f159010, f543])).
fof(f543, plain, ~ (slcrc0 = xQ), inference(cnf_transformation, [], [f100])).
fof(f100, plain, (~ (slcrc0 = xQ) & aSubsetOf0(xQ, xO)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5093)).
fof(f159010, plain, (~ aElementOf0(xx, xQ) | (slcrc0 = xQ) | ~ aSubsetOf0(xQ, szNzAzT0) | (~ spl29_52 | spl29_53 | spl29_86 | ~ spl29_88 | ~ spl29_91 | spl29_302)), inference(subsumption_resolution, [], [f159009, f7536])).
fof(f7536, plain, (~ (xp = xx) | spl29_302), inference(avatar_component_clause, [], [f7535])).
fof(f159009, plain, ((xp = xx) | ~ aElementOf0(xx, xQ) | (slcrc0 = xQ) | ~ aSubsetOf0(xQ, szNzAzT0) | (~ spl29_52 | spl29_53 | spl29_86 | ~ spl29_88 | ~ spl29_91)), inference(subsumption_resolution, [], [f140225, f147643])).
fof(f147643, plain, (aElementOf0(xp, sdtlpdtrp0(xN, xm)) | (spl29_86 | ~ spl29_88 | ~ spl29_91)), inference(forward_demodulation, [], [f138733, f23010])).
fof(f23010, plain, (xp = szmzizndt0(sdtlpdtrp0(xN, xn))), inference(forward_demodulation, [], [f22910, f557])).
fof(f557, plain, (xp = sdtlpdtrp0(xe, xn)), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ((xp = sdtlpdtrp0(xe, xn)) & aElementOf0(xn, szNzAzT0) & aElementOf0(xn, sdtlbdtrb0(xd, szDzizrdt0(xd)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5309)).
fof(f22910, plain, (sdtlpdtrp0(xe, xn) = szmzizndt0(sdtlpdtrp0(xN, xn))), inference(backward_demodulation, [], [f19625, f19512])).
fof(f19512, plain, (xn = sbrdtbr0(slbdtrb0(xn))), inference(resolution, [], [f556, f441])).
fof(f441, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | (sbrdtbr0(slbdtrb0(X0)) = X0)), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ! [X0] : ((sbrdtbr0(slbdtrb0(X0)) = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f56])).
fof(f56, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (sbrdtbr0(slbdtrb0(X0)) = X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mCardSeg)).
fof(f556, plain, aElementOf0(xn, szNzAzT0), inference(cnf_transformation, [], [f111])).
fof(f19625, plain, (szmzizndt0(sdtlpdtrp0(xN, sbrdtbr0(slbdtrb0(xn)))) = sdtlpdtrp0(xe, sbrdtbr0(slbdtrb0(xn)))), inference(subsumption_resolution, [], [f19622, f740])).
fof(f740, plain, aSet0(slbdtrb0(xn)), inference(resolution, [], [f583, f556])).
fof(f583, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | aSet0(slbdtrb0(X0))), inference(equality_resolution, [], [f425])).
fof(f425, plain, ! [X0, X1] : (aSet0(X1) | ~ (slbdtrb0(X0) = X1) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f300])).
fof(f300, plain, ! [X0] : (! [X1] : (((slbdtrb0(X0) = X1) | ((~ sdtlseqdt0(szszuzczcdt0(sK12(X0, X1)), X0) | ~ aElementOf0(sK12(X0, X1), szNzAzT0) | ~ aElementOf0(sK12(X0, X1), X1)) & ((sdtlseqdt0(szszuzczcdt0(sK12(X0, X1)), X0) & aElementOf0(sK12(X0, X1), szNzAzT0)) | aElementOf0(sK12(X0, X1), X1))) | ~ aSet0(X1)) & ((! [X3] : ((aElementOf0(X3, X1) | ~ sdtlseqdt0(szszuzczcdt0(X3), X0) | ~ aElementOf0(X3, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X3), X0) & aElementOf0(X3, szNzAzT0)) | ~ aElementOf0(X3, X1))) & aSet0(X1)) | ~ (slbdtrb0(X0) = X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK12])], [f298, f299])).
fof(f299, plain, ! [X1, X0] : (? [X2] : ((~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X2, X1)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | aElementOf0(X2, X1))) => ((~ sdtlseqdt0(szszuzczcdt0(sK12(X0, X1)), X0) | ~ aElementOf0(sK12(X0, X1), szNzAzT0) | ~ aElementOf0(sK12(X0, X1), X1)) & ((sdtlseqdt0(szszuzczcdt0(sK12(X0, X1)), X0) & aElementOf0(sK12(X0, X1), szNzAzT0)) | aElementOf0(sK12(X0, X1), X1)))), introduced(choice_axiom, [])).
fof(f298, plain, ! [X0] : (! [X1] : (((slbdtrb0(X0) = X1) | ? [X2] : ((~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X2, X1)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | aElementOf0(X2, X1))) | ~ aSet0(X1)) & ((! [X3] : ((aElementOf0(X3, X1) | ~ sdtlseqdt0(szszuzczcdt0(X3), X0) | ~ aElementOf0(X3, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X3), X0) & aElementOf0(X3, szNzAzT0)) | ~ aElementOf0(X3, X1))) & aSet0(X1)) | ~ (slbdtrb0(X0) = X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(rectify, [], [f297])).
fof(f297, plain, ! [X0] : (! [X1] : (((slbdtrb0(X0) = X1) | ? [X2] : ((~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X2, X1)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | aElementOf0(X2, X1))) | ~ aSet0(X1)) & ((! [X2] : ((aElementOf0(X2, X1) | ~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | ~ aElementOf0(X2, X1))) & aSet0(X1)) | ~ (slbdtrb0(X0) = X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f296])).
fof(f296, plain, ! [X0] : (! [X1] : (((slbdtrb0(X0) = X1) | (? [X2] : (((~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0)) | ~ aElementOf0(X2, X1)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | aElementOf0(X2, X1))) | ~ aSet0(X1))) & ((! [X2] : ((aElementOf0(X2, X1) | (~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0))) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | ~ aElementOf0(X2, X1))) & aSet0(X1)) | ~ (slbdtrb0(X0) = X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(nnf_transformation, [], [f192])).
fof(f192, plain, ! [X0] : (! [X1] : ((slbdtrb0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X1) <=> (sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0))) & aSet0(X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f50])).
fof(f50, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => ! [X1] : ((slbdtrb0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X1) <=> (sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0))) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mDefSeg)).
fof(f19622, plain, ((szmzizndt0(sdtlpdtrp0(xN, sbrdtbr0(slbdtrb0(xn)))) = sdtlpdtrp0(xe, sbrdtbr0(slbdtrb0(xn)))) | ~ aSet0(slbdtrb0(xn))), inference(resolution, [], [f723, f1458])).
fof(f1458, plain, ! [X1] : (~ isFinite0(X1) | (szmzizndt0(sdtlpdtrp0(xN, sbrdtbr0(X1))) = sdtlpdtrp0(xe, sbrdtbr0(X1))) | ~ aSet0(X1)), inference(resolution, [], [f526, f408])).
fof(f408, plain, ! [X0] : (aElementOf0(sbrdtbr0(X0), szNzAzT0) | ~ isFinite0(X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f282])).
fof(f282, plain, ! [X0] : (((aElementOf0(sbrdtbr0(X0), szNzAzT0) | ~ isFinite0(X0)) & (isFinite0(X0) | ~ aElementOf0(sbrdtbr0(X0), szNzAzT0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f176])).
fof(f176, plain, ! [X0] : ((aElementOf0(sbrdtbr0(X0), szNzAzT0) <=> isFinite0(X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f41])).
fof(f41, plain, ! [X0] : (aSet0(X0) => (aElementOf0(sbrdtbr0(X0), szNzAzT0) <=> isFinite0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mCardNum)).
fof(f526, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | (szmzizndt0(sdtlpdtrp0(xN, X0)) = sdtlpdtrp0(xe, X0))), inference(cnf_transformation, [], [f246])).
fof(f246, plain, (! [X0] : ((szmzizndt0(sdtlpdtrp0(xN, X0)) = sdtlpdtrp0(xe, X0)) | ~ aElementOf0(X0, szNzAzT0)) & (szNzAzT0 = szDzozmdt0(xe)) & aFunction0(xe)), inference(ennf_transformation, [], [f91])).
fof(f91, plain, (! [X0] : (aElementOf0(X0, szNzAzT0) => (szmzizndt0(sdtlpdtrp0(xN, X0)) = sdtlpdtrp0(xe, X0))) & (szNzAzT0 = szDzozmdt0(xe)) & aFunction0(xe)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__4660)).
fof(f723, plain, isFinite0(slbdtrb0(xn)), inference(resolution, [], [f432, f556])).
fof(f432, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | isFinite0(slbdtrb0(X0))), inference(cnf_transformation, [], [f193])).
fof(f193, plain, ! [X0] : (isFinite0(slbdtrb0(X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => isFinite0(slbdtrb0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mSegFin)).
fof(f138733, plain, (aElementOf0(szmzizndt0(sdtlpdtrp0(xN, xn)), sdtlpdtrp0(xN, xm)) | (spl29_86 | ~ spl29_88 | ~ spl29_91)), inference(subsumption_resolution, [], [f138732, f1933])).
fof(f1933, plain, (aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0) | ~ spl29_88), inference(avatar_component_clause, [], [f1932])).
fof(f138732, plain, (aElementOf0(szmzizndt0(sdtlpdtrp0(xN, xn)), sdtlpdtrp0(xN, xm)) | ~ aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0) | (spl29_86 | ~ spl29_91)), inference(subsumption_resolution, [], [f138530, f1884])).
fof(f1884, plain, (~ (slcrc0 = sdtlpdtrp0(xN, xn)) | spl29_86), inference(avatar_component_clause, [], [f1883])).
fof(f138530, plain, (aElementOf0(szmzizndt0(sdtlpdtrp0(xN, xn)), sdtlpdtrp0(xN, xm)) | (slcrc0 = sdtlpdtrp0(xN, xn)) | ~ aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0) | ~ spl29_91), inference(resolution, [], [f1950, f577])).
fof(f577, plain, ! [X0] : (aElementOf0(szmzizndt0(X0), X0) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(equality_resolution, [], [f416])).
fof(f416, plain, ! [X0, X1] : (aElementOf0(X1, X0) | ~ (szmzizndt0(X0) = X1) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f290])).
fof(f290, plain, ! [X0] : (! [X1] : (((szmzizndt0(X0) = X1) | (~ sdtlseqdt0(X1, sK10(X0, X1)) & aElementOf0(sK10(X0, X1), X0)) | ~ aElementOf0(X1, X0)) & ((! [X3] : (sdtlseqdt0(X1, X3) | ~ aElementOf0(X3, X0)) & aElementOf0(X1, X0)) | ~ (szmzizndt0(X0) = X1))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10])], [f288, f289])).
fof(f289, plain, ! [X1, X0] : (? [X2] : (~ sdtlseqdt0(X1, X2) & aElementOf0(X2, X0)) => (~ sdtlseqdt0(X1, sK10(X0, X1)) & aElementOf0(sK10(X0, X1), X0))), introduced(choice_axiom, [])).
fof(f288, plain, ! [X0] : (! [X1] : (((szmzizndt0(X0) = X1) | ? [X2] : (~ sdtlseqdt0(X1, X2) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0)) & ((! [X3] : (sdtlseqdt0(X1, X3) | ~ aElementOf0(X3, X0)) & aElementOf0(X1, X0)) | ~ (szmzizndt0(X0) = X1))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(rectify, [], [f287])).
fof(f287, plain, ! [X0] : (! [X1] : (((szmzizndt0(X0) = X1) | ? [X2] : (~ sdtlseqdt0(X1, X2) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0)) & ((! [X2] : (sdtlseqdt0(X1, X2) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0)) | ~ (szmzizndt0(X0) = X1))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(flattening, [], [f286])).
fof(f286, plain, ! [X0] : (! [X1] : (((szmzizndt0(X0) = X1) | (? [X2] : (~ sdtlseqdt0(X1, X2) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0))) & ((! [X2] : (sdtlseqdt0(X1, X2) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0)) | ~ (szmzizndt0(X0) = X1))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(nnf_transformation, [], [f187])).
fof(f187, plain, ! [X0] : (! [X1] : ((szmzizndt0(X0) = X1) <=> (! [X2] : (sdtlseqdt0(X1, X2) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0))) | (slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(flattening, [], [f186])).
fof(f186, plain, ! [X0] : (! [X1] : ((szmzizndt0(X0) = X1) <=> (! [X2] : (sdtlseqdt0(X1, X2) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0))) | ((slcrc0 = X0) | ~ aSubsetOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f47])).
fof(f47, plain, ! [X0] : ((~ (slcrc0 = X0) & aSubsetOf0(X0, szNzAzT0)) => ! [X1] : ((szmzizndt0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X0) => sdtlseqdt0(X1, X2)) & aElementOf0(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mDefMin)).
fof(f1950, plain, (! [X5] : (~ aElementOf0(X5, sdtlpdtrp0(xN, xn)) | aElementOf0(X5, sdtlpdtrp0(xN, xm))) | ~ spl29_91), inference(avatar_component_clause, [], [f1949])).
fof(f140225, plain, (~ aElementOf0(xp, sdtlpdtrp0(xN, xm)) | (xp = xx) | ~ aElementOf0(xx, xQ) | (slcrc0 = xQ) | ~ aSubsetOf0(xQ, szNzAzT0) | (~ spl29_52 | spl29_53)), inference(superposition, [], [f4755, f546])).
fof(f546, plain, (xp = szmzizndt0(xQ)), inference(cnf_transformation, [], [f103])).
fof(f103, plain, (xp = szmzizndt0(xQ)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5147)).
fof(f4755, plain, (! [X1] : (~ aElementOf0(szmzizndt0(X1), sdtlpdtrp0(xN, xm)) | (szmzizndt0(X1) = xx) | ~ aElementOf0(xx, X1) | (slcrc0 = X1) | ~ aSubsetOf0(X1, szNzAzT0)) | (~ spl29_52 | spl29_53)), inference(subsumption_resolution, [], [f4754, f1400])).
fof(f1400, plain, (aSubsetOf0(sdtlpdtrp0(xN, xm), szNzAzT0) | ~ spl29_52), inference(avatar_component_clause, [], [f1399])).
fof(f4754, plain, (! [X1] : (~ aElementOf0(xx, X1) | (szmzizndt0(X1) = xx) | ~ aElementOf0(szmzizndt0(X1), sdtlpdtrp0(xN, xm)) | (slcrc0 = X1) | ~ aSubsetOf0(sdtlpdtrp0(xN, xm), szNzAzT0) | ~ aSubsetOf0(X1, szNzAzT0)) | spl29_53), inference(subsumption_resolution, [], [f4748, f1404])).
fof(f1404, plain, (~ (slcrc0 = sdtlpdtrp0(xN, xm)) | spl29_53), inference(avatar_component_clause, [], [f1403])).
fof(f4748, plain, ! [X1] : (~ aElementOf0(xx, X1) | (szmzizndt0(X1) = xx) | ~ aElementOf0(szmzizndt0(X1), sdtlpdtrp0(xN, xm)) | (slcrc0 = sdtlpdtrp0(xN, xm)) | (slcrc0 = X1) | ~ aSubsetOf0(sdtlpdtrp0(xN, xm), szNzAzT0) | ~ aSubsetOf0(X1, szNzAzT0)), inference(superposition, [], [f424, f564])).
fof(f564, plain, (xx = szmzizndt0(sdtlpdtrp0(xN, xm))), inference(cnf_transformation, [], [f116])).
fof(f116, plain, (xx = szmzizndt0(sdtlpdtrp0(xN, xm))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5401)).
fof(f424, plain, ! [X0, X1] : (~ aElementOf0(szmzizndt0(X1), X0) | (szmzizndt0(X0) = szmzizndt0(X1)) | ~ aElementOf0(szmzizndt0(X0), X1) | (slcrc0 = X1) | (slcrc0 = X0) | ~ aSubsetOf0(X1, szNzAzT0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f191])).
fof(f191, plain, ! [X0, X1] : ((szmzizndt0(X0) = szmzizndt0(X1)) | ~ aElementOf0(szmzizndt0(X1), X0) | ~ aElementOf0(szmzizndt0(X0), X1) | (slcrc0 = X1) | (slcrc0 = X0) | ~ aSubsetOf0(X1, szNzAzT0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(flattening, [], [f190])).
fof(f190, plain, ! [X0, X1] : (((szmzizndt0(X0) = szmzizndt0(X1)) | (~ aElementOf0(szmzizndt0(X1), X0) | ~ aElementOf0(szmzizndt0(X0), X1))) | ((slcrc0 = X1) | (slcrc0 = X0) | ~ aSubsetOf0(X1, szNzAzT0) | ~ aSubsetOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f49])).
fof(f49, plain, ! [X0, X1] : ((~ (slcrc0 = X1) & ~ (slcrc0 = X0) & aSubsetOf0(X1, szNzAzT0) & aSubsetOf0(X0, szNzAzT0)) => ((aElementOf0(szmzizndt0(X1), X0) & aElementOf0(szmzizndt0(X0), X1)) => (szmzizndt0(X0) = szmzizndt0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mMinMin)).
fof(f158993, plain, (spl29_2993 | ~ spl29_40), inference(avatar_split_clause, [], [f158804, f1257, f137648])).
fof(f1257, plain, (spl29_40 <=> sP2(xp, xQ, xP)), introduced(avatar_definition, [new_symbols(naming, [spl29_40])])).
fof(f158804, plain, (aElementOf0(xx, xQ) | ~ spl29_40), inference(resolution, [], [f1290, f559])).
fof(f559, plain, aElementOf0(xx, xP), inference(cnf_transformation, [], [f113])).
fof(f113, plain, aElementOf0(xx, xP), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5348)).
fof(f1290, plain, (! [X0] : (~ aElementOf0(X0, xP) | aElementOf0(X0, xQ)) | ~ spl29_40), inference(resolution, [], [f1259, f373])).
fof(f373, plain, ! [X4, X2, X0, X1] : (~ sP2(X0, X1, X2) | ~ aElementOf0(X4, X2) | aElementOf0(X4, X1)), inference(cnf_transformation, [], [f278])).
fof(f278, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7])], [f276, f277])).
fof(f277, plain, ! [X2, X1, X0] : (? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) => (((sK7(X0, X1, X2) = X0) | ~ aElementOf0(sK7(X0, X1, X2), X1) | ~ aElement0(sK7(X0, X1, X2)) | ~ aElementOf0(sK7(X0, X1, X2), X2)) & ((~ (sK7(X0, X1, X2) = X0) & aElementOf0(sK7(X0, X1, X2), X1) & aElement0(sK7(X0, X1, X2))) | aElementOf0(sK7(X0, X1, X2), X2)))), introduced(choice_axiom, [])).
fof(f276, plain, ! [X0, X1, X2] : ((sP2(X0, X1, X2) | ? [X3] : (((X0 = X3) | ~ aElementOf0(X3, X1) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X0 = X3) & aElementOf0(X3, X1) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X4] : ((aElementOf0(X4, X2) | (X0 = X4) | ~ aElementOf0(X4, X1) | ~ aElement0(X4)) & ((~ (X0 = X4) & aElementOf0(X4, X1) & aElement0(X4)) | ~ aElementOf0(X4, X2))) & aSet0(X2)) | ~ sP2(X0, X1, X2))), inference(rectify, [], [f275])).
fof(f275, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | ? [X3] : (((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2)) & ((! [X3] : ((aElementOf0(X3, X2) | (X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(flattening, [], [f274])).
fof(f274, plain, ! [X1, X0, X2] : ((sP2(X1, X0, X2) | (? [X3] : ((((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3)) | ~ aElementOf0(X3, X2)) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | aElementOf0(X3, X2))) | ~ aSet0(X2))) & ((! [X3] : ((aElementOf0(X3, X2) | ((X1 = X3) | ~ aElementOf0(X3, X0) | ~ aElement0(X3))) & ((~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3)) | ~ aElementOf0(X3, X2))) & aSet0(X2)) | ~ sP2(X1, X0, X2))), inference(nnf_transformation, [], [f254])).
fof(f254, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), inference(usedef, [], [e254])).
fof(e254, plain, ! [X1, X0, X2] : (sP2(X1, X0, X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f1259, plain, (sP2(xp, xQ, xP) | ~ spl29_40), inference(avatar_component_clause, [], [f1257])).
fof(f147634, plain, (~ spl29_93 | spl29_302), inference(avatar_contradiction_clause, [], [f147633])).
fof(f147633, plain, ($false | (~ spl29_93 | spl29_302)), inference(subsumption_resolution, [], [f147632, f7536])).
fof(f147632, plain, ((xp = xx) | ~ spl29_93), inference(backward_demodulation, [], [f147521, f23010])).
fof(f147521, plain, ((xx = szmzizndt0(sdtlpdtrp0(xN, xn))) | ~ spl29_93), inference(forward_demodulation, [], [f564, f1959])).
fof(f1959, plain, ((sdtlpdtrp0(xN, xm) = sdtlpdtrp0(xN, xn)) | ~ spl29_93), inference(avatar_component_clause, [], [f1957])).
fof(f1957, plain, (spl29_93 <=> (sdtlpdtrp0(xN, xm) = sdtlpdtrp0(xN, xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_93])])).
fof(f146139, plain, (~ spl29_40 | ~ spl29_302), inference(avatar_contradiction_clause, [], [f146138])).
fof(f146138, plain, ($false | (~ spl29_40 | ~ spl29_302)), inference(subsumption_resolution, [], [f145264, f1292])).
fof(f1292, plain, (~ aElementOf0(xp, xP) | ~ spl29_40), inference(resolution, [], [f1259, f574])).
fof(f574, plain, ! [X4, X2, X1] : (~ sP2(X4, X1, X2) | ~ aElementOf0(X4, X2)), inference(equality_resolution, [], [f374])).
fof(f374, plain, ! [X4, X2, X0, X1] : (~ (X0 = X4) | ~ aElementOf0(X4, X2) | ~ sP2(X0, X1, X2)), inference(cnf_transformation, [], [f278])).
fof(f145264, plain, (aElementOf0(xp, xP) | ~ spl29_302), inference(backward_demodulation, [], [f559, f7537])).
fof(f7537, plain, ((xp = xx) | ~ spl29_302), inference(avatar_component_clause, [], [f7535])).
fof(f143069, plain, (~ spl29_140 | spl29_93 | ~ spl29_1492), inference(avatar_split_clause, [], [f143068, f43187, f1957, f2918])).
fof(f2918, plain, (spl29_140 <=> sdtlseqdt0(xn, xm)), introduced(avatar_definition, [new_symbols(naming, [spl29_140])])).
fof(f43187, plain, (spl29_1492 <=> aElementOf0(xn, szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl29_1492])])).
fof(f143068, plain, ((sdtlpdtrp0(xN, xm) = sdtlpdtrp0(xN, xn)) | ~ sdtlseqdt0(xn, xm) | ~ spl29_1492), inference(subsumption_resolution, [], [f143067, f562])).
fof(f562, plain, aElementOf0(xm, szNzAzT0), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ((xx = sdtlpdtrp0(xe, xm)) & aElementOf0(xm, szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5389)).
fof(f143067, plain, (~ aElementOf0(xm, szNzAzT0) | (sdtlpdtrp0(xN, xm) = sdtlpdtrp0(xN, xn)) | ~ sdtlseqdt0(xn, xm) | ~ spl29_1492), inference(subsumption_resolution, [], [f129377, f43188])).
fof(f43188, plain, (aElementOf0(xn, szNzAzT0) | ~ spl29_1492), inference(avatar_component_clause, [], [f43187])).
fof(f129377, plain, (~ aElementOf0(xn, szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | (sdtlpdtrp0(xN, xm) = sdtlpdtrp0(xN, xn)) | ~ sdtlseqdt0(xn, xm)), inference(resolution, [], [f566, f2690])).
fof(f2690, plain, ! [X4, X3] : (~ aSubsetOf0(sdtlpdtrp0(xN, X3), sdtlpdtrp0(xN, X4)) | ~ aElementOf0(X3, szNzAzT0) | ~ aElementOf0(X4, szNzAzT0) | (sdtlpdtrp0(xN, X3) = sdtlpdtrp0(xN, X4)) | ~ sdtlseqdt0(X3, X4)), inference(subsumption_resolution, [], [f2673, f845])).
fof(f845, plain, ! [X0] : (aSet0(sdtlpdtrp0(xN, X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f843, f387])).
fof(f387, plain, aSet0(szNzAzT0), inference(cnf_transformation, [], [f23])).
fof(f23, plain, (isCountable0(szNzAzT0) & aSet0(szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mNATSet)).
fof(f843, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | aSet0(sdtlpdtrp0(xN, X0)) | ~ aSet0(szNzAzT0)), inference(resolution, [], [f506, f349])).
fof(f349, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f266])).
fof(f266, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK5])], [f264, f265])).
fof(f265, plain, ! [X1, X0] : (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) => (~ aElementOf0(sK5(X0, X1), X0) & aElementOf0(sK5(X0, X1), X1))), introduced(choice_axiom, [])).
fof(f264, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X3] : (aElementOf0(X3, X0) | ~ aElementOf0(X3, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(rectify, [], [f263])).
fof(f263, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | ? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1)) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(flattening, [], [f262])).
fof(f262, plain, ! [X0] : (! [X1] : ((aSubsetOf0(X1, X0) | (? [X2] : (~ aElementOf0(X2, X0) & aElementOf0(X2, X1)) | ~ aSet0(X1))) & ((! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1)) | ~ aSubsetOf0(X1, X0))) | ~ aSet0(X0)), inference(nnf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X0) | ~ aElementOf0(X2, X1)) & aSet0(X1))) | ~ aSet0(X0)), inference(ennf_transformation, [], [f10])).
fof(f10, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aSubsetOf0(X1, X0) <=> (! [X2] : (aElementOf0(X2, X1) => aElementOf0(X2, X0)) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mDefSub)).
fof(f506, plain, ! [X0] : (aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f230])).
fof(f230, plain, ! [X0] : ((isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f82])).
fof(f82, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__3671)).
fof(f2673, plain, ! [X4, X3] : (~ sdtlseqdt0(X3, X4) | ~ aElementOf0(X3, szNzAzT0) | ~ aElementOf0(X4, szNzAzT0) | (sdtlpdtrp0(xN, X3) = sdtlpdtrp0(xN, X4)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X3), sdtlpdtrp0(xN, X4)) | ~ aSet0(sdtlpdtrp0(xN, X3))), inference(resolution, [], [f508, f607])).
fof(f607, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | (X0 = X1) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f355, f349])).
fof(f355, plain, ! [X0, X1] : ((X0 = X1) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f138])).
fof(f138, plain, ! [X0, X1] : ((X0 = X1) | ~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f137])).
fof(f137, plain, ! [X0, X1] : (((X0 = X1) | (~ aSubsetOf0(X1, X0) | ~ aSubsetOf0(X0, X1))) | (~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f13])).
fof(f13, plain, ! [X0, X1] : ((aSet0(X1) & aSet0(X0)) => ((aSubsetOf0(X1, X0) & aSubsetOf0(X0, X1)) => (X0 = X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mSubASymm)).
fof(f508, plain, ! [X0, X1] : (aSubsetOf0(sdtlpdtrp0(xN, X0), sdtlpdtrp0(xN, X1)) | ~ sdtlseqdt0(X1, X0) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f232])).
fof(f232, plain, ! [X0, X1] : (aSubsetOf0(sdtlpdtrp0(xN, X0), sdtlpdtrp0(xN, X1)) | ~ sdtlseqdt0(X1, X0) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f231])).
fof(f231, plain, ! [X0, X1] : ((aSubsetOf0(sdtlpdtrp0(xN, X0), sdtlpdtrp0(xN, X1)) | ~ sdtlseqdt0(X1, X0)) | (~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f83])).
fof(f83, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aElementOf0(X0, szNzAzT0)) => (sdtlseqdt0(X1, X0) => aSubsetOf0(sdtlpdtrp0(xN, X0), sdtlpdtrp0(xN, X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__3754)).
fof(f566, plain, aSubsetOf0(sdtlpdtrp0(xN, xn), sdtlpdtrp0(xN, xm)), inference(cnf_transformation, [], [f250])).
fof(f250, plain, (~ aElementOf0(xx, sdtlpdtrp0(xN, szszuzczcdt0(xn))) & aSubsetOf0(sdtlpdtrp0(xN, xn), sdtlpdtrp0(xN, xm))), inference(ennf_transformation, [], [f119])).
fof(f119, plain, ~ (aSubsetOf0(sdtlpdtrp0(xN, xn), sdtlpdtrp0(xN, xm)) => aElementOf0(xx, sdtlpdtrp0(xN, szszuzczcdt0(xn)))), inference(negated_conjecture, [], [f118])).
fof(f118, plain, ~ (aSubsetOf0(sdtlpdtrp0(xN, xn), sdtlpdtrp0(xN, xm)) => aElementOf0(xx, sdtlpdtrp0(xN, szszuzczcdt0(xn)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__)).
fof(f137016, plain, (~ spl29_86 | ~ spl29_214), inference(avatar_contradiction_clause, [], [f137015])).
fof(f137015, plain, ($false | (~ spl29_86 | ~ spl29_214)), inference(subsumption_resolution, [], [f136725, f606])).
fof(f606, plain, ~ isCountable0(slcrc0), inference(subsumption_resolution, [], [f570, f569])).
fof(f569, plain, aSet0(slcrc0), inference(equality_resolution, [], [f343])).
fof(f343, plain, ! [X0] : (aSet0(X0) | ~ (slcrc0 = X0)), inference(cnf_transformation, [], [f261])).
fof(f261, plain, ! [X0] : (((slcrc0 = X0) | aElementOf0(sK4(X0), X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4])], [f259, f260])).
fof(f260, plain, ! [X0] : (? [X1] : aElementOf0(X1, X0) => aElementOf0(sK4(X0), X0)), introduced(choice_axiom, [])).
fof(f259, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(rectify, [], [f258])).
fof(f258, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(flattening, [], [f257])).
fof(f257, plain, ! [X0] : (((slcrc0 = X0) | (? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0))) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(nnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : ((slcrc0 = X0) <=> (! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : ((slcrc0 = X0) <=> (~ ? [X1] : aElementOf0(X1, X0) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mDefEmp)).
fof(f570, plain, (~ isCountable0(slcrc0) | ~ aSet0(slcrc0)), inference(equality_resolution, [], [f348])).
fof(f348, plain, ! [X0] : (~ (slcrc0 = X0) | ~ isCountable0(X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (~ (slcrc0 = X0) | ~ isCountable0(X0) | ~ aSet0(X0)), inference(flattening, [], [f131])).
fof(f131, plain, ! [X0] : (~ (slcrc0 = X0) | (~ isCountable0(X0) | ~ aSet0(X0))), inference(ennf_transformation, [], [f9])).
fof(f9, plain, ! [X0] : ((isCountable0(X0) & aSet0(X0)) => ~ (slcrc0 = X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mCountNFin_01)).
fof(f136725, plain, (isCountable0(slcrc0) | (~ spl29_86 | ~ spl29_214)), inference(backward_demodulation, [], [f5330, f1885])).
fof(f1885, plain, ((slcrc0 = sdtlpdtrp0(xN, xn)) | ~ spl29_86), inference(avatar_component_clause, [], [f1883])).
fof(f5330, plain, (isCountable0(sdtlpdtrp0(xN, xn)) | ~ spl29_214), inference(avatar_component_clause, [], [f5328])).
fof(f5328, plain, (spl29_214 <=> isCountable0(sdtlpdtrp0(xN, xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_214])])).
fof(f136703, plain, (spl29_88 | ~ spl29_23), inference(avatar_split_clause, [], [f136702, f817, f1932])).
fof(f817, plain, (spl29_23 <=> aSet0(sdtlpdtrp0(xN, xn))), introduced(avatar_definition, [new_symbols(naming, [spl29_23])])).
fof(f136702, plain, (aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0) | ~ spl29_23), inference(subsumption_resolution, [], [f136701, f562])).
fof(f136701, plain, (aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | ~ spl29_23), inference(subsumption_resolution, [], [f129379, f819])).
fof(f819, plain, (aSet0(sdtlpdtrp0(xN, xn)) | ~ spl29_23), inference(avatar_component_clause, [], [f817])).
fof(f129379, plain, (aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0) | ~ aSet0(sdtlpdtrp0(xN, xn)) | ~ aElementOf0(xm, szNzAzT0)), inference(resolution, [], [f566, f2087])).
fof(f2087, plain, ! [X6, X5] : (~ aSubsetOf0(X5, sdtlpdtrp0(xN, X6)) | aSubsetOf0(X5, szNzAzT0) | ~ aSet0(X5) | ~ aElementOf0(X6, szNzAzT0)), inference(subsumption_resolution, [], [f2072, f387])).
fof(f2072, plain, ! [X6, X5] : (aSubsetOf0(X5, szNzAzT0) | ~ aSubsetOf0(X5, sdtlpdtrp0(xN, X6)) | ~ aSet0(szNzAzT0) | ~ aSet0(X5) | ~ aElementOf0(X6, szNzAzT0)), inference(resolution, [], [f608, f506])).
fof(f608, plain, ! [X2, X0, X1] : (~ aSubsetOf0(X1, X2) | aSubsetOf0(X0, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X0)), inference(subsumption_resolution, [], [f356, f349])).
fof(f356, plain, ! [X2, X0, X1] : (aSubsetOf0(X0, X2) | ~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f140])).
fof(f140, plain, ! [X0, X1, X2] : (aSubsetOf0(X0, X2) | ~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1) | ~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0)), inference(flattening, [], [f139])).
fof(f139, plain, ! [X0, X1, X2] : ((aSubsetOf0(X0, X2) | (~ aSubsetOf0(X1, X2) | ~ aSubsetOf0(X0, X1))) | (~ aSet0(X2) | ~ aSet0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f14])).
fof(f14, plain, ! [X0, X1, X2] : ((aSet0(X2) & aSet0(X1) & aSet0(X0)) => ((aSubsetOf0(X1, X2) & aSubsetOf0(X0, X1)) => aSubsetOf0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mSubTrans)).
fof(f43382, plain, spl29_1492, inference(avatar_split_clause, [], [f556, f43187])).
fof(f22914, plain, (spl29_57 | spl29_58), inference(avatar_split_clause, [], [f19535, f1521, f1517])).
fof(f1517, plain, (spl29_57 <=> (xn = szszuzczcdt0(sK8(xn)))), introduced(avatar_definition, [new_symbols(naming, [spl29_57])])).
fof(f1521, plain, (spl29_58 <=> (sz00 = xn)), introduced(avatar_definition, [new_symbols(naming, [spl29_58])])).
fof(f19535, plain, ((xn = szszuzczcdt0(sK8(xn))) | spl29_58), inference(subsumption_resolution, [], [f19508, f1522])).
fof(f1522, plain, (~ (sz00 = xn) | spl29_58), inference(avatar_component_clause, [], [f1521])).
fof(f19508, plain, ((sz00 = xn) | (xn = szszuzczcdt0(sK8(xn)))), inference(resolution, [], [f556, f394])).
fof(f394, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | (sz00 = X0) | (szszuzczcdt0(sK8(X0)) = X0)), inference(cnf_transformation, [], [f280])).
fof(f280, plain, ! [X0] : (((szszuzczcdt0(sK8(X0)) = X0) & aElementOf0(sK8(X0), szNzAzT0)) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f160, f279])).
fof(f279, plain, ! [X0] : (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) => ((szszuzczcdt0(sK8(X0)) = X0) & aElementOf0(sK8(X0), szNzAzT0))), introduced(choice_axiom, [])).
fof(f160, plain, ! [X0] : (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f159])).
fof(f159, plain, ! [X0] : ((? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mNatExtra)).
fof(f5463, plain, (~ spl29_58 | spl29_140), inference(avatar_contradiction_clause, [], [f5462])).
fof(f5462, plain, ($false | (~ spl29_58 | spl29_140)), inference(subsumption_resolution, [], [f5414, f693])).
fof(f693, plain, sdtlseqdt0(sz00, xm), inference(resolution, [], [f396, f562])).
fof(f396, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | sdtlseqdt0(sz00, X0)), inference(cnf_transformation, [], [f162])).
fof(f162, plain, ! [X0] : (sdtlseqdt0(sz00, X0) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => sdtlseqdt0(sz00, X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mZeroLess)).
fof(f5414, plain, (~ sdtlseqdt0(sz00, xm) | (~ spl29_58 | spl29_140)), inference(backward_demodulation, [], [f2920, f1523])).
fof(f1523, plain, ((sz00 = xn) | ~ spl29_58), inference(avatar_component_clause, [], [f1521])).
fof(f2920, plain, (~ sdtlseqdt0(xn, xm) | spl29_140), inference(avatar_component_clause, [], [f2918])).
fof(f5354, plain, (spl29_58 | spl29_198), inference(avatar_split_clause, [], [f5353, f5259, f1521])).
fof(f5259, plain, (spl29_198 <=> aElementOf0(sK8(xn), szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl29_198])])).
fof(f5353, plain, ((sz00 = xn) | spl29_198), inference(subsumption_resolution, [], [f5337, f556])).
fof(f5337, plain, ((sz00 = xn) | ~ aElementOf0(xn, szNzAzT0) | spl29_198), inference(resolution, [], [f5261, f393])).
fof(f393, plain, ! [X0] : (aElementOf0(sK8(X0), szNzAzT0) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f280])).
fof(f5261, plain, (~ aElementOf0(sK8(xn), szNzAzT0) | spl29_198), inference(avatar_component_clause, [], [f5259])).
fof(f5331, plain, (~ spl29_198 | spl29_214 | ~ spl29_57), inference(avatar_split_clause, [], [f5256, f1517, f5328, f5259])).
fof(f5256, plain, (isCountable0(sdtlpdtrp0(xN, xn)) | ~ aElementOf0(sK8(xn), szNzAzT0) | ~ spl29_57), inference(superposition, [], [f1075, f1519])).
fof(f1519, plain, ((xn = szszuzczcdt0(sK8(xn))) | ~ spl29_57), inference(avatar_component_clause, [], [f1517])).
fof(f1075, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f1074, f506])).
fof(f1074, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(subsumption_resolution, [], [f505, f507])).
fof(f507, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f230])).
fof(f505, plain, ! [X0] : (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) | ~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f229])).
fof(f229, plain, (! [X0] : ((isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) | ~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), inference(flattening, [], [f228])).
fof(f228, plain, (! [X0] : (((isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))) | (~ isCountable0(sdtlpdtrp0(xN, X0)) | ~ aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0))) | ~ aElementOf0(X0, szNzAzT0)) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), inference(ennf_transformation, [], [f81])).
fof(f81, plain, (! [X0] : (aElementOf0(X0, szNzAzT0) => ((isCountable0(sdtlpdtrp0(xN, X0)) & aSubsetOf0(sdtlpdtrp0(xN, X0), szNzAzT0)) => (isCountable0(sdtlpdtrp0(xN, szszuzczcdt0(X0))) & aSubsetOf0(sdtlpdtrp0(xN, szszuzczcdt0(X0)), sdtmndt0(sdtlpdtrp0(xN, X0), szmzizndt0(sdtlpdtrp0(xN, X0))))))) & (xS = sdtlpdtrp0(xN, sz00)) & (szNzAzT0 = szDzozmdt0(xN)) & aFunction0(xN)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__3623)).
fof(f1988, plain, (~ spl29_22 | spl29_91), inference(avatar_split_clause, [], [f1963, f1949, f813])).
fof(f813, plain, (spl29_22 <=> aSet0(sdtlpdtrp0(xN, xm))), introduced(avatar_definition, [new_symbols(naming, [spl29_22])])).
fof(f1963, plain, ! [X0] : (~ aElementOf0(X0, sdtlpdtrp0(xN, xn)) | aElementOf0(X0, sdtlpdtrp0(xN, xm)) | ~ aSet0(sdtlpdtrp0(xN, xm))), inference(resolution, [], [f566, f350])).
fof(f350, plain, ! [X0, X3, X1] : (~ aSubsetOf0(X1, X0) | ~ aElementOf0(X3, X1) | aElementOf0(X3, X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f266])).
fof(f1986, plain, (~ spl29_22 | spl29_23), inference(avatar_split_clause, [], [f1965, f817, f813])).
fof(f1965, plain, (aSet0(sdtlpdtrp0(xN, xn)) | ~ aSet0(sdtlpdtrp0(xN, xm))), inference(resolution, [], [f566, f349])).
fof(f1985, plain, (spl29_22 | ~ spl29_52), inference(avatar_split_clause, [], [f1984, f1399, f813])).
fof(f1984, plain, (aSet0(sdtlpdtrp0(xN, xm)) | ~ spl29_52), inference(subsumption_resolution, [], [f1969, f387])).
fof(f1969, plain, (aSet0(sdtlpdtrp0(xN, xm)) | ~ aSet0(szNzAzT0) | ~ spl29_52), inference(resolution, [], [f1400, f349])).
fof(f1937, plain, (~ spl29_88 | spl29_86 | ~ spl29_53), inference(avatar_split_clause, [], [f1936, f1403, f1883, f1932])).
fof(f1936, plain, ((slcrc0 = sdtlpdtrp0(xN, xn)) | ~ aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0) | ~ spl29_53), inference(subsumption_resolution, [], [f1927, f1898])).
fof(f1898, plain, (isFinite0(sdtlpdtrp0(xN, xn)) | ~ spl29_53), inference(subsumption_resolution, [], [f1897, f569])).
fof(f1897, plain, (~ aSet0(slcrc0) | isFinite0(sdtlpdtrp0(xN, xn)) | ~ spl29_53), inference(forward_demodulation, [], [f1896, f1405])).
fof(f1405, plain, ((slcrc0 = sdtlpdtrp0(xN, xm)) | ~ spl29_53), inference(avatar_component_clause, [], [f1403])).
fof(f1896, plain, (isFinite0(sdtlpdtrp0(xN, xn)) | ~ aSet0(sdtlpdtrp0(xN, xm)) | ~ spl29_53), inference(subsumption_resolution, [], [f1895, f346])).
fof(f346, plain, isFinite0(slcrc0), inference(cnf_transformation, [], [f6])).
fof(f6, plain, isFinite0(slcrc0), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mEmpFin)).
fof(f1895, plain, (~ isFinite0(slcrc0) | isFinite0(sdtlpdtrp0(xN, xn)) | ~ aSet0(sdtlpdtrp0(xN, xm)) | ~ spl29_53), inference(forward_demodulation, [], [f1206, f1405])).
fof(f1206, plain, (isFinite0(sdtlpdtrp0(xN, xn)) | ~ isFinite0(sdtlpdtrp0(xN, xm)) | ~ aSet0(sdtlpdtrp0(xN, xm))), inference(resolution, [], [f353, f566])).
fof(f353, plain, ! [X0, X1] : (~ aSubsetOf0(X1, X0) | isFinite0(X1) | ~ isFinite0(X0) | ~ aSet0(X0)), inference(cnf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : (! [X1] : (isFinite0(X1) | ~ aSubsetOf0(X1, X0)) | ~ isFinite0(X0) | ~ aSet0(X0)), inference(flattening, [], [f134])).
fof(f134, plain, ! [X0] : (! [X1] : (isFinite0(X1) | ~ aSubsetOf0(X1, X0)) | (~ isFinite0(X0) | ~ aSet0(X0))), inference(ennf_transformation, [], [f11])).
fof(f11, plain, ! [X0] : ((isFinite0(X0) & aSet0(X0)) => ! [X1] : (aSubsetOf0(X1, X0) => isFinite0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mSubFSet)).
fof(f1927, plain, ((slcrc0 = sdtlpdtrp0(xN, xn)) | ~ isFinite0(sdtlpdtrp0(xN, xn)) | ~ aSubsetOf0(sdtlpdtrp0(xN, xn), szNzAzT0) | ~ spl29_53), inference(resolution, [], [f1894, f579])).
fof(f579, plain, ! [X0] : (aElementOf0(szmzazxdt0(X0), X0) | (slcrc0 = X0) | ~ isFinite0(X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(equality_resolution, [], [f420])).
fof(f420, plain, ! [X0, X1] : (aElementOf0(X1, X0) | ~ (szmzazxdt0(X0) = X1) | (slcrc0 = X0) | ~ isFinite0(X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f295])).
fof(f295, plain, ! [X0] : (! [X1] : (((szmzazxdt0(X0) = X1) | (~ sdtlseqdt0(sK11(X0, X1), X1) & aElementOf0(sK11(X0, X1), X0)) | ~ aElementOf0(X1, X0)) & ((! [X3] : (sdtlseqdt0(X3, X1) | ~ aElementOf0(X3, X0)) & aElementOf0(X1, X0)) | ~ (szmzazxdt0(X0) = X1))) | (slcrc0 = X0) | ~ isFinite0(X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11])], [f293, f294])).
fof(f294, plain, ! [X1, X0] : (? [X2] : (~ sdtlseqdt0(X2, X1) & aElementOf0(X2, X0)) => (~ sdtlseqdt0(sK11(X0, X1), X1) & aElementOf0(sK11(X0, X1), X0))), introduced(choice_axiom, [])).
fof(f293, plain, ! [X0] : (! [X1] : (((szmzazxdt0(X0) = X1) | ? [X2] : (~ sdtlseqdt0(X2, X1) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0)) & ((! [X3] : (sdtlseqdt0(X3, X1) | ~ aElementOf0(X3, X0)) & aElementOf0(X1, X0)) | ~ (szmzazxdt0(X0) = X1))) | (slcrc0 = X0) | ~ isFinite0(X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(rectify, [], [f292])).
fof(f292, plain, ! [X0] : (! [X1] : (((szmzazxdt0(X0) = X1) | ? [X2] : (~ sdtlseqdt0(X2, X1) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0)) & ((! [X2] : (sdtlseqdt0(X2, X1) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0)) | ~ (szmzazxdt0(X0) = X1))) | (slcrc0 = X0) | ~ isFinite0(X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(flattening, [], [f291])).
fof(f291, plain, ! [X0] : (! [X1] : (((szmzazxdt0(X0) = X1) | (? [X2] : (~ sdtlseqdt0(X2, X1) & aElementOf0(X2, X0)) | ~ aElementOf0(X1, X0))) & ((! [X2] : (sdtlseqdt0(X2, X1) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0)) | ~ (szmzazxdt0(X0) = X1))) | (slcrc0 = X0) | ~ isFinite0(X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(nnf_transformation, [], [f189])).
fof(f189, plain, ! [X0] : (! [X1] : ((szmzazxdt0(X0) = X1) <=> (! [X2] : (sdtlseqdt0(X2, X1) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0))) | (slcrc0 = X0) | ~ isFinite0(X0) | ~ aSubsetOf0(X0, szNzAzT0)), inference(flattening, [], [f188])).
fof(f188, plain, ! [X0] : (! [X1] : ((szmzazxdt0(X0) = X1) <=> (! [X2] : (sdtlseqdt0(X2, X1) | ~ aElementOf0(X2, X0)) & aElementOf0(X1, X0))) | ((slcrc0 = X0) | ~ isFinite0(X0) | ~ aSubsetOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f48])).
fof(f48, plain, ! [X0] : ((~ (slcrc0 = X0) & isFinite0(X0) & aSubsetOf0(X0, szNzAzT0)) => ! [X1] : ((szmzazxdt0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X0) => sdtlseqdt0(X2, X1)) & aElementOf0(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mDefMax)).
fof(f1894, plain, (! [X5] : ~ aElementOf0(X5, sdtlpdtrp0(xN, xn)) | ~ spl29_53), inference(subsumption_resolution, [], [f1893, f569])).
fof(f1893, plain, (! [X5] : (~ aSet0(slcrc0) | ~ aElementOf0(X5, sdtlpdtrp0(xN, xn))) | ~ spl29_53), inference(forward_demodulation, [], [f1892, f1405])).
fof(f1892, plain, (! [X5] : (~ aElementOf0(X5, sdtlpdtrp0(xN, xn)) | ~ aSet0(sdtlpdtrp0(xN, xm))) | ~ spl29_53), inference(subsumption_resolution, [], [f1891, f568])).
fof(f568, plain, ! [X2] : ~ aElementOf0(X2, slcrc0), inference(equality_resolution, [], [f344])).
fof(f344, plain, ! [X2, X0] : (~ aElementOf0(X2, X0) | ~ (slcrc0 = X0)), inference(cnf_transformation, [], [f261])).
fof(f1891, plain, (! [X5] : (aElementOf0(X5, slcrc0) | ~ aElementOf0(X5, sdtlpdtrp0(xN, xn)) | ~ aSet0(sdtlpdtrp0(xN, xm))) | ~ spl29_53), inference(forward_demodulation, [], [f1479, f1405])).
fof(f1479, plain, ! [X5] : (~ aElementOf0(X5, sdtlpdtrp0(xN, xn)) | aElementOf0(X5, sdtlpdtrp0(xN, xm)) | ~ aSet0(sdtlpdtrp0(xN, xm))), inference(resolution, [], [f350, f566])).
fof(f1870, plain, spl29_52, inference(avatar_contradiction_clause, [], [f1869])).
fof(f1869, plain, ($false | spl29_52), inference(subsumption_resolution, [], [f1868, f562])).
fof(f1868, plain, (~ aElementOf0(xm, szNzAzT0) | spl29_52), inference(resolution, [], [f1401, f506])).
fof(f1401, plain, (~ aSubsetOf0(sdtlpdtrp0(xN, xm), szNzAzT0) | spl29_52), inference(avatar_component_clause, [], [f1399])).
fof(f1264, plain, spl29_39, inference(avatar_contradiction_clause, [], [f1263])).
fof(f1263, plain, ($false | spl29_39), inference(subsumption_resolution, [], [f1262, f825])).
fof(f825, plain, aSet0(xQ), inference(subsumption_resolution, [], [f807, f533])).
fof(f533, plain, aSet0(xO), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((xO = sdtlcdtrc0(xe, sdtlbdtrb0(xd, szDzizrdt0(xd)))) & aSet0(xO)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__4891)).
fof(f807, plain, (aSet0(xQ) | ~ aSet0(xO)), inference(resolution, [], [f349, f542])).
fof(f542, plain, aSubsetOf0(xQ, xO), inference(cnf_transformation, [], [f100])).
fof(f1262, plain, (~ aSet0(xQ) | spl29_39), inference(subsumption_resolution, [], [f1261, f800])).
fof(f800, plain, aElement0(xp), inference(subsumption_resolution, [], [f765, f533])).
fof(f765, plain, (aElement0(xp) | ~ aSet0(xO)), inference(resolution, [], [f342, f550])).
fof(f550, plain, aElementOf0(xp, xO), inference(cnf_transformation, [], [f106])).
fof(f106, plain, aElementOf0(xp, xO), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5182)).
fof(f342, plain, ! [X0, X1] : (~ aElementOf0(X1, X0) | aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f127])).
fof(f127, plain, ! [X0] : (! [X1] : (aElement0(X1) | ~ aElementOf0(X1, X0)) | ~ aSet0(X0)), inference(ennf_transformation, [], [f3])).
fof(f3, plain, ! [X0] : (aSet0(X0) => ! [X1] : (aElementOf0(X1, X0) => aElement0(X1))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mEOfElem)).
fof(f1261, plain, (~ aElement0(xp) | ~ aSet0(xQ) | spl29_39), inference(resolution, [], [f1255, f380])).
fof(f380, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(cnf_transformation, [], [f256])).
fof(f256, plain, ! [X0, X1] : (sP3(X0, X1) | ~ aElement0(X1) | ~ aSet0(X0)), inference(definition_folding, [], [f144, e255, e254])).
fof(f255, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2)) | ~ sP3(X0, X1)), inference(usedef, [], [e255])).
fof(e255, plain, ! [X0, X1] : (sP3(X0, X1) <=> ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> sP2(X1, X0, X2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f144, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | ~ aElement0(X1) | ~ aSet0(X0)), inference(flattening, [], [f143])).
fof(f143, plain, ! [X0, X1] : (! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2))) | (~ aElement0(X1) | ~ aSet0(X0))), inference(ennf_transformation, [], [f16])).
fof(f16, plain, ! [X0, X1] : ((aElement0(X1) & aSet0(X0)) => ! [X2] : ((sdtmndt0(X0, X1) = X2) <=> (! [X3] : (aElementOf0(X3, X2) <=> (~ (X1 = X3) & aElementOf0(X3, X0) & aElement0(X3))) & aSet0(X2)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', mDefDiff)).
fof(f1255, plain, (~ sP3(xQ, xp) | spl29_39), inference(avatar_component_clause, [], [f1253])).
fof(f1253, plain, (spl29_39 <=> sP3(xQ, xp)), introduced(avatar_definition, [new_symbols(naming, [spl29_39])])).
fof(f1260, plain, (~ spl29_39 | spl29_40), inference(avatar_split_clause, [], [f1251, f1257, f1253])).
fof(f1251, plain, (sP2(xp, xQ, xP) | ~ sP3(xQ, xp)), inference(superposition, [], [f573, f619])).
fof(f619, plain, (xP = sdtmndt0(xQ, xp)), inference(forward_demodulation, [], [f548, f546])).
fof(f548, plain, (xP = sdtmndt0(xQ, szmzizndt0(xQ))), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ((xP = sdtmndt0(xQ, szmzizndt0(xQ))) & aSet0(xP)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM621+1.p', m__5164)).
fof(f573, plain, ! [X0, X1] : (sP2(X1, X0, sdtmndt0(X0, X1)) | ~ sP3(X0, X1)), inference(equality_resolution, [], [f369])).
fof(f369, plain, ! [X2, X0, X1] : (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2) | ~ sP3(X0, X1)), inference(cnf_transformation, [], [f273])).
fof(f273, plain, ! [X0, X1] : (! [X2] : (((sdtmndt0(X0, X1) = X2) | ~ sP2(X1, X0, X2)) & (sP2(X1, X0, X2) | ~ (sdtmndt0(X0, X1) = X2))) | ~ sP3(X0, X1)), inference(nnf_transformation, [], [f255])).