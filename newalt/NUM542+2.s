fof(f4270, plain, $false, inference(avatar_sat_refutation, [], [f373, f378, f383, f393, f403, f436, f440, f441, f442, f447, f675, f1014, f1020, f1061, f2621, f2788, f3215, f3234, f3437, f3839, f3875, f4044])).
fof(f4044, plain, (spl19_9 | ~ spl19_45), inference(avatar_contradiction_clause, [], [f4043])).
fof(f4043, plain, ($false | (spl19_9 | ~ spl19_45)), inference(subsumption_resolution, [], [f3880, f457])).
fof(f457, plain, sdtlseqdt0(sz00, xn), inference(resolution, [], [f263, f304])).
fof(f304, plain, aElementOf0(xn, szNzAzT0), inference(cnf_transformation, [], [f54])).
fof(f54, plain, (aElementOf0(xn, szNzAzT0) & aElementOf0(xm, szNzAzT0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', m__1964)).
fof(f263, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | sdtlseqdt0(sz00, X0)), inference(cnf_transformation, [], [f100])).
fof(f100, plain, ! [X0] : (sdtlseqdt0(sz00, X0) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f30])).
fof(f30, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => sdtlseqdt0(sz00, X0)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', mZeroLess)).
fof(f3880, plain, (~ sdtlseqdt0(sz00, xn) | (spl19_9 | ~ spl19_45)), inference(backward_demodulation, [], [f401, f879])).
fof(f879, plain, ((sz00 = xm) | ~ spl19_45), inference(avatar_component_clause, [], [f877])).
fof(f877, plain, (spl19_45 <=> (sz00 = xm)), introduced(avatar_definition, [new_symbols(naming, [spl19_45])])).
fof(f401, plain, (~ sdtlseqdt0(xm, xn) | spl19_9), inference(avatar_component_clause, [], [f400])).
fof(f400, plain, (spl19_9 <=> sdtlseqdt0(xm, xn)), introduced(avatar_definition, [new_symbols(naming, [spl19_9])])).
fof(f3875, plain, (spl19_44 | spl19_45), inference(avatar_split_clause, [], [f1085, f877, f873])).
fof(f873, plain, (spl19_44 <=> (xm = szszuzczcdt0(sK13(xm)))), introduced(avatar_definition, [new_symbols(naming, [spl19_44])])).
fof(f1085, plain, ((sz00 = xm) | (xm = szszuzczcdt0(sK13(xm)))), inference(resolution, [], [f303, f261])).
fof(f261, plain, ! [X0] : (~ aElementOf0(X0, szNzAzT0) | (sz00 = X0) | (szszuzczcdt0(sK13(X0)) = X0)), inference(cnf_transformation, [], [f170])).
fof(f170, plain, ! [X0] : (((szszuzczcdt0(sK13(X0)) = X0) & aElementOf0(sK13(X0), szNzAzT0)) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13])], [f98, f169])).
fof(f169, plain, ! [X0] : (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) => ((szszuzczcdt0(sK13(X0)) = X0) & aElementOf0(sK13(X0), szNzAzT0))), introduced(choice_axiom, [])).
fof(f98, plain, ! [X0] : (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f97])).
fof(f97, plain, ! [X0] : ((? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f27])).
fof(f27, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (? [X1] : ((szszuzczcdt0(X1) = X0) & aElementOf0(X1, szNzAzT0)) | (sz00 = X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', mNatExtra)).
fof(f303, plain, aElementOf0(xm, szNzAzT0), inference(cnf_transformation, [], [f54])).
fof(f3839, plain, (spl19_9 | ~ spl19_44 | ~ spl19_192), inference(avatar_contradiction_clause, [], [f3838])).
fof(f3838, plain, ($false | (spl19_9 | ~ spl19_44 | ~ spl19_192)), inference(subsumption_resolution, [], [f3837, f401])).
fof(f3837, plain, (sdtlseqdt0(xm, xn) | (~ spl19_44 | ~ spl19_192)), inference(forward_demodulation, [], [f3836, f875])).
fof(f875, plain, ((xm = szszuzczcdt0(sK13(xm))) | ~ spl19_44), inference(avatar_component_clause, [], [f873])).
fof(f3836, plain, (sdtlseqdt0(szszuzczcdt0(sK13(xm)), xn) | ~ spl19_192), inference(subsumption_resolution, [], [f3828, f304])).
fof(f3828, plain, (sdtlseqdt0(szszuzczcdt0(sK13(xm)), xn) | ~ aElementOf0(xn, szNzAzT0) | ~ spl19_192), inference(resolution, [], [f3233, f345])).
fof(f345, plain, ! [X0, X3] : (~ aElementOf0(X3, slbdtrb0(X0)) | sdtlseqdt0(szszuzczcdt0(X3), X0) | ~ aElementOf0(X0, szNzAzT0)), inference(equality_resolution, [], [f293])).
fof(f293, plain, ! [X0, X3, X1] : (sdtlseqdt0(szszuzczcdt0(X3), X0) | ~ aElementOf0(X3, X1) | ~ (slbdtrb0(X0) = X1) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f190])).
fof(f190, plain, ! [X0] : (! [X1] : (((slbdtrb0(X0) = X1) | ((~ sdtlseqdt0(szszuzczcdt0(sK17(X0, X1)), X0) | ~ aElementOf0(sK17(X0, X1), szNzAzT0) | ~ aElementOf0(sK17(X0, X1), X1)) & ((sdtlseqdt0(szszuzczcdt0(sK17(X0, X1)), X0) & aElementOf0(sK17(X0, X1), szNzAzT0)) | aElementOf0(sK17(X0, X1), X1))) | ~ aSet0(X1)) & ((! [X3] : ((aElementOf0(X3, X1) | ~ sdtlseqdt0(szszuzczcdt0(X3), X0) | ~ aElementOf0(X3, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X3), X0) & aElementOf0(X3, szNzAzT0)) | ~ aElementOf0(X3, X1))) & aSet0(X1)) | ~ (slbdtrb0(X0) = X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK17])], [f188, f189])).
fof(f189, plain, ! [X1, X0] : (? [X2] : ((~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X2, X1)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | aElementOf0(X2, X1))) => ((~ sdtlseqdt0(szszuzczcdt0(sK17(X0, X1)), X0) | ~ aElementOf0(sK17(X0, X1), szNzAzT0) | ~ aElementOf0(sK17(X0, X1), X1)) & ((sdtlseqdt0(szszuzczcdt0(sK17(X0, X1)), X0) & aElementOf0(sK17(X0, X1), szNzAzT0)) | aElementOf0(sK17(X0, X1), X1)))), introduced(choice_axiom, [])).
fof(f188, plain, ! [X0] : (! [X1] : (((slbdtrb0(X0) = X1) | ? [X2] : ((~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X2, X1)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | aElementOf0(X2, X1))) | ~ aSet0(X1)) & ((! [X3] : ((aElementOf0(X3, X1) | ~ sdtlseqdt0(szszuzczcdt0(X3), X0) | ~ aElementOf0(X3, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X3), X0) & aElementOf0(X3, szNzAzT0)) | ~ aElementOf0(X3, X1))) & aSet0(X1)) | ~ (slbdtrb0(X0) = X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(rectify, [], [f187])).
fof(f187, plain, ! [X0] : (! [X1] : (((slbdtrb0(X0) = X1) | ? [X2] : ((~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X2, X1)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | aElementOf0(X2, X1))) | ~ aSet0(X1)) & ((! [X2] : ((aElementOf0(X2, X1) | ~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | ~ aElementOf0(X2, X1))) & aSet0(X1)) | ~ (slbdtrb0(X0) = X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f186])).
fof(f186, plain, ! [X0] : (! [X1] : (((slbdtrb0(X0) = X1) | (? [X2] : (((~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0)) | ~ aElementOf0(X2, X1)) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | aElementOf0(X2, X1))) | ~ aSet0(X1))) & ((! [X2] : ((aElementOf0(X2, X1) | (~ sdtlseqdt0(szszuzczcdt0(X2), X0) | ~ aElementOf0(X2, szNzAzT0))) & ((sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0)) | ~ aElementOf0(X2, X1))) & aSet0(X1)) | ~ (slbdtrb0(X0) = X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(nnf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (! [X1] : ((slbdtrb0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X1) <=> (sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0))) & aSet0(X1))) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f50])).
fof(f50, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => ! [X1] : ((slbdtrb0(X0) = X1) <=> (! [X2] : (aElementOf0(X2, X1) <=> (sdtlseqdt0(szszuzczcdt0(X2), X0) & aElementOf0(X2, szNzAzT0))) & aSet0(X1)))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', mDefSeg)).
fof(f3233, plain, (aElementOf0(sK13(xm), slbdtrb0(xn)) | ~ spl19_192), inference(avatar_component_clause, [], [f3231])).
fof(f3231, plain, (spl19_192 <=> aElementOf0(sK13(xm), slbdtrb0(xn))), introduced(avatar_definition, [new_symbols(naming, [spl19_192])])).
fof(f3437, plain, (spl19_192 | ~ spl19_18 | ~ spl19_57), inference(avatar_split_clause, [], [f3428, f1011, f445, f3231])).
fof(f445, plain, (spl19_18 <=> ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) | ~ aElementOf0(X0, slbdtrb0(xm)))), introduced(avatar_definition, [new_symbols(naming, [spl19_18])])).
fof(f1011, plain, (spl19_57 <=> aElementOf0(sK13(xm), slbdtrb0(xm))), introduced(avatar_definition, [new_symbols(naming, [spl19_57])])).
fof(f3428, plain, (aElementOf0(sK13(xm), slbdtrb0(xn)) | (~ spl19_18 | ~ spl19_57)), inference(resolution, [], [f1013, f446])).
fof(f446, plain, (! [X0] : (~ aElementOf0(X0, slbdtrb0(xm)) | aElementOf0(X0, slbdtrb0(xn))) | ~ spl19_18), inference(avatar_component_clause, [], [f445])).
fof(f1013, plain, (aElementOf0(sK13(xm), slbdtrb0(xm)) | ~ spl19_57), inference(avatar_component_clause, [], [f1011])).
fof(f3234, plain, (spl19_192 | ~ spl19_9 | ~ spl19_11 | ~ spl19_44 | ~ spl19_54), inference(avatar_split_clause, [], [f3229, f994, f873, f409, f400, f3231])).
fof(f409, plain, (spl19_11 <=> ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) | ~ aElementOf0(X0, szNzAzT0) | ~ sdtlseqdt0(szszuzczcdt0(X0), xn))), introduced(avatar_definition, [new_symbols(naming, [spl19_11])])).
fof(f994, plain, (spl19_54 <=> aElementOf0(sK13(xm), szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl19_54])])).
fof(f3229, plain, (~ sdtlseqdt0(xm, xn) | aElementOf0(sK13(xm), slbdtrb0(xn)) | (~ spl19_11 | ~ spl19_44 | ~ spl19_54)), inference(subsumption_resolution, [], [f2054, f995])).
fof(f995, plain, (aElementOf0(sK13(xm), szNzAzT0) | ~ spl19_54), inference(avatar_component_clause, [], [f994])).
fof(f2054, plain, (~ sdtlseqdt0(xm, xn) | ~ aElementOf0(sK13(xm), szNzAzT0) | aElementOf0(sK13(xm), slbdtrb0(xn)) | (~ spl19_11 | ~ spl19_44)), inference(superposition, [], [f410, f875])).
fof(f410, plain, (! [X0] : (~ sdtlseqdt0(szszuzczcdt0(X0), xn) | ~ aElementOf0(X0, szNzAzT0) | aElementOf0(X0, slbdtrb0(xn))) | ~ spl19_11), inference(avatar_component_clause, [], [f409])).
fof(f3215, plain, (spl19_3 | ~ spl19_4 | ~ spl19_17 | ~ spl19_143), inference(avatar_contradiction_clause, [], [f3214])).
fof(f3214, plain, ($false | (spl19_3 | ~ spl19_4 | ~ spl19_17 | ~ spl19_143)), inference(subsumption_resolution, [], [f3213, f304])).
fof(f3213, plain, (~ aElementOf0(xn, szNzAzT0) | (spl19_3 | ~ spl19_4 | ~ spl19_17 | ~ spl19_143)), inference(subsumption_resolution, [], [f3212, f716])).
fof(f716, plain, (aElementOf0(sK18, szNzAzT0) | (~ spl19_4 | ~ spl19_17)), inference(resolution, [], [f434, f377])).
fof(f377, plain, (aElementOf0(sK18, slbdtrb0(xm)) | ~ spl19_4), inference(avatar_component_clause, [], [f375])).
fof(f375, plain, (spl19_4 <=> aElementOf0(sK18, slbdtrb0(xm))), introduced(avatar_definition, [new_symbols(naming, [spl19_4])])).
fof(f434, plain, (! [X0] : (~ aElementOf0(X0, slbdtrb0(xm)) | aElementOf0(X0, szNzAzT0)) | ~ spl19_17), inference(avatar_component_clause, [], [f433])).
fof(f433, plain, (spl19_17 <=> ! [X0] : (aElementOf0(X0, szNzAzT0) | ~ aElementOf0(X0, slbdtrb0(xm)))), introduced(avatar_definition, [new_symbols(naming, [spl19_17])])).
fof(f3212, plain, (~ aElementOf0(sK18, szNzAzT0) | ~ aElementOf0(xn, szNzAzT0) | (spl19_3 | ~ spl19_143)), inference(subsumption_resolution, [], [f3206, f372])).
fof(f372, plain, (~ aElementOf0(sK18, slbdtrb0(xn)) | spl19_3), inference(avatar_component_clause, [], [f370])).
fof(f370, plain, (spl19_3 <=> aElementOf0(sK18, slbdtrb0(xn))), introduced(avatar_definition, [new_symbols(naming, [spl19_3])])).
fof(f3206, plain, (aElementOf0(sK18, slbdtrb0(xn)) | ~ aElementOf0(sK18, szNzAzT0) | ~ aElementOf0(xn, szNzAzT0) | ~ spl19_143), inference(resolution, [], [f2620, f344])).
fof(f344, plain, ! [X0, X3] : (~ sdtlseqdt0(szszuzczcdt0(X3), X0) | aElementOf0(X3, slbdtrb0(X0)) | ~ aElementOf0(X3, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(equality_resolution, [], [f294])).
fof(f294, plain, ! [X0, X3, X1] : (aElementOf0(X3, X1) | ~ sdtlseqdt0(szszuzczcdt0(X3), X0) | ~ aElementOf0(X3, szNzAzT0) | ~ (slbdtrb0(X0) = X1) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f190])).
fof(f2620, plain, (sdtlseqdt0(szszuzczcdt0(sK18), xn) | ~ spl19_143), inference(avatar_component_clause, [], [f2618])).
fof(f2618, plain, (spl19_143 <=> sdtlseqdt0(szszuzczcdt0(sK18), xn)), introduced(avatar_definition, [new_symbols(naming, [spl19_143])])).
fof(f2788, plain, (~ spl19_4 | ~ spl19_17 | spl19_142), inference(avatar_contradiction_clause, [], [f2787])).
fof(f2787, plain, ($false | (~ spl19_4 | ~ spl19_17 | spl19_142)), inference(subsumption_resolution, [], [f2784, f716])).
fof(f2784, plain, (~ aElementOf0(sK18, szNzAzT0) | spl19_142), inference(resolution, [], [f2616, f257])).
fof(f257, plain, ! [X0] : (aElementOf0(szszuzczcdt0(X0), szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ! [X0] : ((~ (sz00 = szszuzczcdt0(X0)) & aElementOf0(szszuzczcdt0(X0), szNzAzT0)) | ~ aElementOf0(X0, szNzAzT0)), inference(ennf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) => (~ (sz00 = szszuzczcdt0(X0)) & aElementOf0(szszuzczcdt0(X0), szNzAzT0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', mSuccNum)).
fof(f2616, plain, (~ aElementOf0(szszuzczcdt0(sK18), szNzAzT0) | spl19_142), inference(avatar_component_clause, [], [f2614])).
fof(f2614, plain, (spl19_142 <=> aElementOf0(szszuzczcdt0(sK18), szNzAzT0)), introduced(avatar_definition, [new_symbols(naming, [spl19_142])])).
fof(f2621, plain, (~ spl19_142 | spl19_143 | ~ spl19_4 | ~ spl19_9 | ~ spl19_16), inference(avatar_split_clause, [], [f2610, f429, f400, f375, f2618, f2614])).
fof(f429, plain, (spl19_16 <=> ! [X0] : (sdtlseqdt0(szszuzczcdt0(X0), xm) | ~ aElementOf0(X0, slbdtrb0(xm)))), introduced(avatar_definition, [new_symbols(naming, [spl19_16])])).
fof(f2610, plain, (sdtlseqdt0(szszuzczcdt0(sK18), xn) | ~ aElementOf0(szszuzczcdt0(sK18), szNzAzT0) | (~ spl19_4 | ~ spl19_9 | ~ spl19_16)), inference(resolution, [], [f735, f1549])).
fof(f1549, plain, (! [X25] : (~ sdtlseqdt0(X25, xm) | sdtlseqdt0(X25, xn) | ~ aElementOf0(X25, szNzAzT0)) | ~ spl19_9), inference(subsumption_resolution, [], [f1548, f303])).
fof(f1548, plain, (! [X25] : (sdtlseqdt0(X25, xn) | ~ sdtlseqdt0(X25, xm) | ~ aElementOf0(xm, szNzAzT0) | ~ aElementOf0(X25, szNzAzT0)) | ~ spl19_9), inference(subsumption_resolution, [], [f1514, f304])).
fof(f1514, plain, (! [X25] : (sdtlseqdt0(X25, xn) | ~ sdtlseqdt0(X25, xm) | ~ aElementOf0(xn, szNzAzT0) | ~ aElementOf0(xm, szNzAzT0) | ~ aElementOf0(X25, szNzAzT0)) | ~ spl19_9), inference(resolution, [], [f270, f402])).
fof(f402, plain, (sdtlseqdt0(xm, xn) | ~ spl19_9), inference(avatar_component_clause, [], [f400])).
fof(f270, plain, ! [X2, X0, X1] : (~ sdtlseqdt0(X1, X2) | sdtlseqdt0(X0, X2) | ~ sdtlseqdt0(X0, X1) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ! [X0, X1, X2] : (sdtlseqdt0(X0, X2) | ~ sdtlseqdt0(X1, X2) | ~ sdtlseqdt0(X0, X1) | ~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f108])).
fof(f108, plain, ! [X0, X1, X2] : ((sdtlseqdt0(X0, X2) | (~ sdtlseqdt0(X1, X2) | ~ sdtlseqdt0(X0, X1))) | (~ aElementOf0(X2, szNzAzT0) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f36])).
fof(f36, plain, ! [X0, X1, X2] : ((aElementOf0(X2, szNzAzT0) & aElementOf0(X1, szNzAzT0) & aElementOf0(X0, szNzAzT0)) => ((sdtlseqdt0(X1, X2) & sdtlseqdt0(X0, X1)) => sdtlseqdt0(X0, X2))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', mLessTrans)).
fof(f735, plain, (sdtlseqdt0(szszuzczcdt0(sK18), xm) | (~ spl19_4 | ~ spl19_16)), inference(resolution, [], [f430, f377])).
fof(f430, plain, (! [X0] : (~ aElementOf0(X0, slbdtrb0(xm)) | sdtlseqdt0(szszuzczcdt0(X0), xm)) | ~ spl19_16), inference(avatar_component_clause, [], [f429])).
fof(f1061, plain, (spl19_29 | ~ spl19_45), inference(avatar_contradiction_clause, [], [f1060])).
fof(f1060, plain, ($false | (spl19_29 | ~ spl19_45)), inference(subsumption_resolution, [], [f1032, f299])).
fof(f299, plain, (slcrc0 = slbdtrb0(sz00)), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (slcrc0 = slbdtrb0(sz00)), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', mSegZero)).
fof(f1032, plain, (~ (slcrc0 = slbdtrb0(sz00)) | (spl19_29 | ~ spl19_45)), inference(backward_demodulation, [], [f576, f879])).
fof(f576, plain, (~ (slcrc0 = slbdtrb0(xm)) | spl19_29), inference(avatar_component_clause, [], [f575])).
fof(f575, plain, (spl19_29 <=> (slcrc0 = slbdtrb0(xm))), introduced(avatar_definition, [new_symbols(naming, [spl19_29])])).
fof(f1020, plain, (spl19_45 | spl19_54), inference(avatar_split_clause, [], [f1019, f994, f877])).
fof(f1019, plain, ((sz00 = xm) | spl19_54), inference(subsumption_resolution, [], [f1018, f303])).
fof(f1018, plain, ((sz00 = xm) | ~ aElementOf0(xm, szNzAzT0) | spl19_54), inference(resolution, [], [f996, f260])).
fof(f260, plain, ! [X0] : (aElementOf0(sK13(X0), szNzAzT0) | (sz00 = X0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f170])).
fof(f996, plain, (~ aElementOf0(sK13(xm), szNzAzT0) | spl19_54), inference(avatar_component_clause, [], [f994])).
fof(f1014, plain, (~ spl19_54 | spl19_57 | ~ spl19_44), inference(avatar_split_clause, [], [f988, f873, f1011, f994])).
fof(f988, plain, (aElementOf0(sK13(xm), slbdtrb0(xm)) | ~ aElementOf0(sK13(xm), szNzAzT0) | ~ spl19_44), inference(superposition, [], [f349, f875])).
fof(f349, plain, ! [X1] : (aElementOf0(X1, slbdtrb0(szszuzczcdt0(X1))) | ~ aElementOf0(X1, szNzAzT0)), inference(duplicate_literal_removal, [], [f348])).
fof(f348, plain, ! [X1] : (aElementOf0(X1, slbdtrb0(szszuzczcdt0(X1))) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X1, szNzAzT0)), inference(equality_resolution, [], [f302])).
fof(f302, plain, ! [X0, X1] : (aElementOf0(X0, slbdtrb0(szszuzczcdt0(X1))) | ~ (X0 = X1) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ! [X0, X1] : (((aElementOf0(X0, slbdtrb0(szszuzczcdt0(X1))) | (~ (X0 = X1) & ~ aElementOf0(X0, slbdtrb0(X1)))) & ((X0 = X1) | aElementOf0(X0, slbdtrb0(X1)) | ~ aElementOf0(X0, slbdtrb0(szszuzczcdt0(X1))))) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f191])).
fof(f191, plain, ! [X0, X1] : (((aElementOf0(X0, slbdtrb0(szszuzczcdt0(X1))) | (~ (X0 = X1) & ~ aElementOf0(X0, slbdtrb0(X1)))) & (((X0 = X1) | aElementOf0(X0, slbdtrb0(X1))) | ~ aElementOf0(X0, slbdtrb0(szszuzczcdt0(X1))))) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(nnf_transformation, [], [f132])).
fof(f132, plain, ! [X0, X1] : ((aElementOf0(X0, slbdtrb0(szszuzczcdt0(X1))) <=> ((X0 = X1) | aElementOf0(X0, slbdtrb0(X1)))) | ~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0)), inference(flattening, [], [f131])).
fof(f131, plain, ! [X0, X1] : ((aElementOf0(X0, slbdtrb0(szszuzczcdt0(X1))) <=> ((X0 = X1) | aElementOf0(X0, slbdtrb0(X1)))) | (~ aElementOf0(X1, szNzAzT0) | ~ aElementOf0(X0, szNzAzT0))), inference(ennf_transformation, [], [f53])).
fof(f53, plain, ! [X0, X1] : ((aElementOf0(X1, szNzAzT0) & aElementOf0(X0, szNzAzT0)) => (aElementOf0(X0, slbdtrb0(szszuzczcdt0(X1))) <=> ((X0 = X1) | aElementOf0(X0, slbdtrb0(X1))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', mSegSucc)).
fof(f675, plain, (~ spl19_4 | ~ spl19_29), inference(avatar_contradiction_clause, [], [f674])).
fof(f674, plain, ($false | (~ spl19_4 | ~ spl19_29)), inference(subsumption_resolution, [], [f673, f332])).
fof(f332, plain, ! [X2] : ~ aElementOf0(X2, slcrc0), inference(equality_resolution, [], [f211])).
fof(f211, plain, ! [X2, X0] : (~ aElementOf0(X2, X0) | ~ (slcrc0 = X0)), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ! [X0] : (((slcrc0 = X0) | aElementOf0(sK9(X0), X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9])], [f149, f150])).
fof(f150, plain, ! [X0] : (? [X1] : aElementOf0(X1, X0) => aElementOf0(sK9(X0), X0)), introduced(choice_axiom, [])).
fof(f149, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X2] : ~ aElementOf0(X2, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(rectify, [], [f148])).
fof(f148, plain, ! [X0] : (((slcrc0 = X0) | ? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0)) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(flattening, [], [f147])).
fof(f147, plain, ! [X0] : (((slcrc0 = X0) | (? [X1] : aElementOf0(X1, X0) | ~ aSet0(X0))) & ((! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0)) | ~ (slcrc0 = X0))), inference(nnf_transformation, [], [f66])).
fof(f66, plain, ! [X0] : ((slcrc0 = X0) <=> (! [X1] : ~ aElementOf0(X1, X0) & aSet0(X0))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ! [X0] : ((slcrc0 = X0) <=> (~ ? [X1] : aElementOf0(X1, X0) & aSet0(X0))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', mDefEmp)).
fof(f673, plain, (aElementOf0(sK18, slcrc0) | (~ spl19_4 | ~ spl19_29)), inference(forward_demodulation, [], [f377, f577])).
fof(f577, plain, ((slcrc0 = slbdtrb0(xm)) | ~ spl19_29), inference(avatar_component_clause, [], [f575])).
fof(f447, plain, (spl19_1 | spl19_18), inference(avatar_split_clause, [], [f329, f445, f361])).
fof(f361, plain, (spl19_1 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl19_1])])).
fof(f329, plain, ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) | ~ aElementOf0(X0, slbdtrb0(xm)) | sP8), inference(cnf_transformation, [], [f146])).
fof(f146, plain, ((~ sdtlseqdt0(xm, xn) & aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) | ~ aElementOf0(X0, slbdtrb0(xm))) & sP7 & aSet0(slbdtrb0(xn)) & sP6 & aSet0(slbdtrb0(xm))) | sP8), inference(definition_folding, [], [f134, e145, e144, e143, e142, e141])).
fof(f141, plain, (! [X3] : (aElementOf0(X3, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X3), xm) & aElementOf0(X3, szNzAzT0))) | ~ sP4), inference(usedef, [], [e141])).
fof(e141, plain, (sP4 <=> ! [X3] : (aElementOf0(X3, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X3), xm) & aElementOf0(X3, szNzAzT0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f142, plain, (! [X4] : (aElementOf0(X4, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X4), xn) & aElementOf0(X4, szNzAzT0))) | ~ sP5), inference(usedef, [], [e142])).
fof(e142, plain, (sP5 <=> ! [X4] : (aElementOf0(X4, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X4), xn) & aElementOf0(X4, szNzAzT0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f143, plain, (! [X2] : (aElementOf0(X2, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X2), xm) & aElementOf0(X2, szNzAzT0))) | ~ sP6), inference(usedef, [], [e143])).
fof(e143, plain, (sP6 <=> ! [X2] : (aElementOf0(X2, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X2), xm) & aElementOf0(X2, szNzAzT0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f144, plain, (! [X1] : (aElementOf0(X1, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X1), xn) & aElementOf0(X1, szNzAzT0))) | ~ sP7), inference(usedef, [], [e144])).
fof(e144, plain, (sP7 <=> ! [X1] : (aElementOf0(X1, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X1), xn) & aElementOf0(X1, szNzAzT0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f145, plain, ((~ aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ? [X5] : (~ aElementOf0(X5, slbdtrb0(xn)) & aElementOf0(X5, slbdtrb0(xm))) & sP5 & aSet0(slbdtrb0(xn)) & sP4 & aSet0(slbdtrb0(xm)) & sdtlseqdt0(xm, xn)) | ~ sP8), inference(usedef, [], [e145])).
fof(e145, plain, (sP8 <=> (~ aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ? [X5] : (~ aElementOf0(X5, slbdtrb0(xn)) & aElementOf0(X5, slbdtrb0(xm))) & sP5 & aSet0(slbdtrb0(xn)) & sP4 & aSet0(slbdtrb0(xm)) & sdtlseqdt0(xm, xn))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f134, plain, ((~ sdtlseqdt0(xm, xn) & aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) | ~ aElementOf0(X0, slbdtrb0(xm))) & ! [X1] : (aElementOf0(X1, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X1), xn) & aElementOf0(X1, szNzAzT0))) & aSet0(slbdtrb0(xn)) & ! [X2] : (aElementOf0(X2, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X2), xm) & aElementOf0(X2, szNzAzT0))) & aSet0(slbdtrb0(xm))) | (~ aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ? [X5] : (~ aElementOf0(X5, slbdtrb0(xn)) & aElementOf0(X5, slbdtrb0(xm))) & ! [X4] : (aElementOf0(X4, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X4), xn) & aElementOf0(X4, szNzAzT0))) & aSet0(slbdtrb0(xn)) & ! [X3] : (aElementOf0(X3, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X3), xm) & aElementOf0(X3, szNzAzT0))) & aSet0(slbdtrb0(xm)) & sdtlseqdt0(xm, xn))), inference(flattening, [], [f133])).
fof(f133, plain, ((~ sdtlseqdt0(xm, xn) & (aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) | ~ aElementOf0(X0, slbdtrb0(xm))) & ! [X1] : (aElementOf0(X1, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X1), xn) & aElementOf0(X1, szNzAzT0))) & aSet0(slbdtrb0(xn)) & ! [X2] : (aElementOf0(X2, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X2), xm) & aElementOf0(X2, szNzAzT0))) & aSet0(slbdtrb0(xm)))) | ((((~ aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ? [X5] : (~ aElementOf0(X5, slbdtrb0(xn)) & aElementOf0(X5, slbdtrb0(xm)))) & (! [X4] : (aElementOf0(X4, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X4), xn) & aElementOf0(X4, szNzAzT0))) & aSet0(slbdtrb0(xn)))) & (! [X3] : (aElementOf0(X3, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X3), xm) & aElementOf0(X3, szNzAzT0))) & aSet0(slbdtrb0(xm)))) & sdtlseqdt0(xm, xn))), inference(ennf_transformation, [], [f63])).
fof(f63, plain, ~ (((aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ! [X0] : (aElementOf0(X0, slbdtrb0(xm)) => aElementOf0(X0, slbdtrb0(xn))) & ! [X1] : (aElementOf0(X1, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X1), xn) & aElementOf0(X1, szNzAzT0))) & aSet0(slbdtrb0(xn)) & ! [X2] : (aElementOf0(X2, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X2), xm) & aElementOf0(X2, szNzAzT0))) & aSet0(slbdtrb0(xm))) => sdtlseqdt0(xm, xn)) & (sdtlseqdt0(xm, xn) => ((! [X3] : (aElementOf0(X3, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X3), xm) & aElementOf0(X3, szNzAzT0))) & aSet0(slbdtrb0(xm))) => ((! [X4] : (aElementOf0(X4, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X4), xn) & aElementOf0(X4, szNzAzT0))) & aSet0(slbdtrb0(xn))) => (aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) | ! [X5] : (aElementOf0(X5, slbdtrb0(xm)) => aElementOf0(X5, slbdtrb0(xn)))))))), inference(rectify, [], [f56])).
fof(f56, plain, ~ (((aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ! [X0] : (aElementOf0(X0, slbdtrb0(xm)) => aElementOf0(X0, slbdtrb0(xn))) & ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X0), xn) & aElementOf0(X0, szNzAzT0))) & aSet0(slbdtrb0(xn)) & ! [X0] : (aElementOf0(X0, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X0), xm) & aElementOf0(X0, szNzAzT0))) & aSet0(slbdtrb0(xm))) => sdtlseqdt0(xm, xn)) & (sdtlseqdt0(xm, xn) => ((! [X0] : (aElementOf0(X0, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X0), xm) & aElementOf0(X0, szNzAzT0))) & aSet0(slbdtrb0(xm))) => ((! [X0] : (aElementOf0(X0, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X0), xn) & aElementOf0(X0, szNzAzT0))) & aSet0(slbdtrb0(xn))) => (aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) | ! [X0] : (aElementOf0(X0, slbdtrb0(xm)) => aElementOf0(X0, slbdtrb0(xn)))))))), inference(negated_conjecture, [], [f55])).
fof(f55, plain, ~ (((aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ! [X0] : (aElementOf0(X0, slbdtrb0(xm)) => aElementOf0(X0, slbdtrb0(xn))) & ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X0), xn) & aElementOf0(X0, szNzAzT0))) & aSet0(slbdtrb0(xn)) & ! [X0] : (aElementOf0(X0, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X0), xm) & aElementOf0(X0, szNzAzT0))) & aSet0(slbdtrb0(xm))) => sdtlseqdt0(xm, xn)) & (sdtlseqdt0(xm, xn) => ((! [X0] : (aElementOf0(X0, slbdtrb0(xm)) <=> (sdtlseqdt0(szszuzczcdt0(X0), xm) & aElementOf0(X0, szNzAzT0))) & aSet0(slbdtrb0(xm))) => ((! [X0] : (aElementOf0(X0, slbdtrb0(xn)) <=> (sdtlseqdt0(szszuzczcdt0(X0), xn) & aElementOf0(X0, szNzAzT0))) & aSet0(slbdtrb0(xn))) => (aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) | ! [X0] : (aElementOf0(X0, slbdtrb0(xm)) => aElementOf0(X0, slbdtrb0(xn)))))))), file('/home/ubuntu/library/tptp/Problems/NUM/NUM542+2.p', m__)).
fof(f442, plain, (spl19_1 | ~ spl19_9), inference(avatar_split_clause, [], [f331, f400, f361])).
fof(f331, plain, (~ sdtlseqdt0(xm, xn) | sP8), inference(cnf_transformation, [], [f146])).
fof(f441, plain, (~ spl19_7 | spl19_17), inference(avatar_split_clause, [], [f322, f433, f390])).
fof(f390, plain, (spl19_7 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl19_7])])).
fof(f322, plain, ! [X0] : (aElementOf0(X0, szNzAzT0) | ~ aElementOf0(X0, slbdtrb0(xm)) | ~ sP4), inference(cnf_transformation, [], [f208])).
fof(f208, plain, (! [X0] : ((aElementOf0(X0, slbdtrb0(xm)) | ~ sdtlseqdt0(szszuzczcdt0(X0), xm) | ~ aElementOf0(X0, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X0), xm) & aElementOf0(X0, szNzAzT0)) | ~ aElementOf0(X0, slbdtrb0(xm)))) | ~ sP4), inference(rectify, [], [f207])).
fof(f207, plain, (! [X3] : ((aElementOf0(X3, slbdtrb0(xm)) | ~ sdtlseqdt0(szszuzczcdt0(X3), xm) | ~ aElementOf0(X3, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X3), xm) & aElementOf0(X3, szNzAzT0)) | ~ aElementOf0(X3, slbdtrb0(xm)))) | ~ sP4), inference(flattening, [], [f206])).
fof(f206, plain, (! [X3] : ((aElementOf0(X3, slbdtrb0(xm)) | (~ sdtlseqdt0(szszuzczcdt0(X3), xm) | ~ aElementOf0(X3, szNzAzT0))) & ((sdtlseqdt0(szszuzczcdt0(X3), xm) & aElementOf0(X3, szNzAzT0)) | ~ aElementOf0(X3, slbdtrb0(xm)))) | ~ sP4), inference(nnf_transformation, [], [f141])).
fof(f440, plain, (~ spl19_7 | spl19_16), inference(avatar_split_clause, [], [f323, f429, f390])).
fof(f323, plain, ! [X0] : (sdtlseqdt0(szszuzczcdt0(X0), xm) | ~ aElementOf0(X0, slbdtrb0(xm)) | ~ sP4), inference(cnf_transformation, [], [f208])).
fof(f436, plain, (~ spl19_5 | spl19_11), inference(avatar_split_clause, [], [f321, f409, f380])).
fof(f380, plain, (spl19_5 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl19_5])])).
fof(f321, plain, ! [X0] : (aElementOf0(X0, slbdtrb0(xn)) | ~ sdtlseqdt0(szszuzczcdt0(X0), xn) | ~ aElementOf0(X0, szNzAzT0) | ~ sP5), inference(cnf_transformation, [], [f205])).
fof(f205, plain, (! [X0] : ((aElementOf0(X0, slbdtrb0(xn)) | ~ sdtlseqdt0(szszuzczcdt0(X0), xn) | ~ aElementOf0(X0, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X0), xn) & aElementOf0(X0, szNzAzT0)) | ~ aElementOf0(X0, slbdtrb0(xn)))) | ~ sP5), inference(rectify, [], [f204])).
fof(f204, plain, (! [X4] : ((aElementOf0(X4, slbdtrb0(xn)) | ~ sdtlseqdt0(szszuzczcdt0(X4), xn) | ~ aElementOf0(X4, szNzAzT0)) & ((sdtlseqdt0(szszuzczcdt0(X4), xn) & aElementOf0(X4, szNzAzT0)) | ~ aElementOf0(X4, slbdtrb0(xn)))) | ~ sP5), inference(flattening, [], [f203])).
fof(f203, plain, (! [X4] : ((aElementOf0(X4, slbdtrb0(xn)) | (~ sdtlseqdt0(szszuzczcdt0(X4), xn) | ~ aElementOf0(X4, szNzAzT0))) & ((sdtlseqdt0(szszuzczcdt0(X4), xn) & aElementOf0(X4, szNzAzT0)) | ~ aElementOf0(X4, slbdtrb0(xn)))) | ~ sP5), inference(nnf_transformation, [], [f142])).
fof(f403, plain, (~ spl19_1 | spl19_9), inference(avatar_split_clause, [], [f305, f400, f361])).
fof(f305, plain, (sdtlseqdt0(xm, xn) | ~ sP8), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ((~ aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & (~ aElementOf0(sK18, slbdtrb0(xn)) & aElementOf0(sK18, slbdtrb0(xm))) & sP5 & aSet0(slbdtrb0(xn)) & sP4 & aSet0(slbdtrb0(xm)) & sdtlseqdt0(xm, xn)) | ~ sP8), inference(skolemisation, [status(esa), new_symbols(skolem, [sK18])], [f194, f195])).
fof(f195, plain, (? [X0] : (~ aElementOf0(X0, slbdtrb0(xn)) & aElementOf0(X0, slbdtrb0(xm))) => (~ aElementOf0(sK18, slbdtrb0(xn)) & aElementOf0(sK18, slbdtrb0(xm)))), introduced(choice_axiom, [])).
fof(f194, plain, ((~ aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ? [X0] : (~ aElementOf0(X0, slbdtrb0(xn)) & aElementOf0(X0, slbdtrb0(xm))) & sP5 & aSet0(slbdtrb0(xn)) & sP4 & aSet0(slbdtrb0(xm)) & sdtlseqdt0(xm, xn)) | ~ sP8), inference(rectify, [], [f193])).
fof(f193, plain, ((~ aSubsetOf0(slbdtrb0(xm), slbdtrb0(xn)) & ? [X5] : (~ aElementOf0(X5, slbdtrb0(xn)) & aElementOf0(X5, slbdtrb0(xm))) & sP5 & aSet0(slbdtrb0(xn)) & sP4 & aSet0(slbdtrb0(xm)) & sdtlseqdt0(xm, xn)) | ~ sP8), inference(nnf_transformation, [], [f145])).
fof(f393, plain, (~ spl19_1 | spl19_7), inference(avatar_split_clause, [], [f307, f390, f361])).
fof(f307, plain, (sP4 | ~ sP8), inference(cnf_transformation, [], [f196])).
fof(f383, plain, (~ spl19_1 | spl19_5), inference(avatar_split_clause, [], [f309, f380, f361])).
fof(f309, plain, (sP5 | ~ sP8), inference(cnf_transformation, [], [f196])).
fof(f378, plain, (~ spl19_1 | spl19_4), inference(avatar_split_clause, [], [f310, f375, f361])).
fof(f310, plain, (aElementOf0(sK18, slbdtrb0(xm)) | ~ sP8), inference(cnf_transformation, [], [f196])).
fof(f373, plain, (~ spl19_1 | ~ spl19_3), inference(avatar_split_clause, [], [f311, f370, f361])).
fof(f311, plain, (~ aElementOf0(sK18, slbdtrb0(xn)) | ~ sP8), inference(cnf_transformation, [], [f196])).