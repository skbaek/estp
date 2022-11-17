fof(f11604, plain, $false, inference(avatar_sat_refutation, [], [f114, f122, f126, f138, f146, f232, f258, f397, f676, f906, f939, f962, f1146, f1148, f2601, f3705, f4539, f4614, f5771, f5968, f6005, f6084, f6654, f6695, f6991, f6995, f7063, f7106, f8157, f8178, f8207, f8368, f8803, f10383, f10387, f10388, f10519, f10594, f10639, f10656, f10710, f11182, f11285, f11349, f11359, f11365, f11367, f11372, f11603])).
fof(f11603, plain, (~ spl17_19 | ~ spl17_146), inference(avatar_contradiction_clause, [], [f11602])).
fof(f11602, plain, ($false | (~ spl17_19 | ~ spl17_146)), inference(subsumption_resolution, [], [f11600, f230])).
fof(f230, plain, (g_false_only(sK12, sK12) | ~ spl17_19), inference(avatar_component_clause, [], [f229])).
fof(f229, plain, (spl17_19 <=> g_false_only(sK12, sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_19])])).
fof(f11600, plain, (~ g_false_only(sK12, sK12) | ~ spl17_146), inference(resolution, [], [f4053, f83])).
fof(f83, plain, ! [X5] : (~ g_false_only(X5, sK14(X5)) | ~ g_false_only(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((sP3 | ! [X0] : (sP2(X0) | ! [X1] : ((g_true_only(sK11(X1), X1) & g_true_only(X1, sK11(X1))) | ! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3) | (~ g_both(X3, X1) & ~ g_both(X1, X3))) | ~ g_both(X1, X0)))) & ! [X5] : (((((g_true_only(sK13(X5), X5) & g_true_only(X5, sK13(X5))) | ! [X7] : (g_false_only(X7, X5) | g_false_only(X5, X7) | (~ g_both(X7, X5) & ~ g_both(X5, X7)))) & (~ g_false_only(sK14(X5), X5) & ~ g_false_only(X5, sK14(X5)))) | ~ g_false_only(X5, sK12)) & ((! [X9] : (~ g_true_only(X9, X5) | ~ g_true_only(X5, X9)) & (~ g_false_only(sK15(X5), X5) & ~ g_false_only(X5, sK15(X5)))) | ~ g_both(X5, sK12)) & ((! [X11] : (~ g_true_only(X11, X5) | ~ g_true_only(X5, X11)) & ((g_true_only(sK16(X5), X5) & g_true_only(X5, sK16(X5))) | ! [X13] : (g_false_only(X13, X5) | g_false_only(X5, X13) | (~ g_both(X13, X5) & ~ g_both(X5, X13))))) | ~ g_true_only(X5, sK12)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK11, sK12, sK13, sK14, sK15, sK16])], [f36, f42, f41, f40, f39, f38, f37])).
fof(f37, plain, ! [X1] : (? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) => (g_true_only(sK11(X1), X1) & g_true_only(X1, sK11(X1)))), introduced(choice_axiom, [])).
fof(f38, plain, (? [X4] : ! [X5] : ((((? [X6] : (g_true_only(X6, X5) & g_true_only(X5, X6)) | ! [X7] : (g_false_only(X7, X5) | g_false_only(X5, X7) | (~ g_both(X7, X5) & ~ g_both(X5, X7)))) & ? [X8] : (~ g_false_only(X8, X5) & ~ g_false_only(X5, X8))) | ~ g_false_only(X5, X4)) & ((! [X9] : (~ g_true_only(X9, X5) | ~ g_true_only(X5, X9)) & ? [X10] : (~ g_false_only(X10, X5) & ~ g_false_only(X5, X10))) | ~ g_both(X5, X4)) & ((! [X11] : (~ g_true_only(X11, X5) | ~ g_true_only(X5, X11)) & (? [X12] : (g_true_only(X12, X5) & g_true_only(X5, X12)) | ! [X13] : (g_false_only(X13, X5) | g_false_only(X5, X13) | (~ g_both(X13, X5) & ~ g_both(X5, X13))))) | ~ g_true_only(X5, X4))) => ! [X5] : ((((? [X6] : (g_true_only(X6, X5) & g_true_only(X5, X6)) | ! [X7] : (g_false_only(X7, X5) | g_false_only(X5, X7) | (~ g_both(X7, X5) & ~ g_both(X5, X7)))) & ? [X8] : (~ g_false_only(X8, X5) & ~ g_false_only(X5, X8))) | ~ g_false_only(X5, sK12)) & ((! [X9] : (~ g_true_only(X9, X5) | ~ g_true_only(X5, X9)) & ? [X10] : (~ g_false_only(X10, X5) & ~ g_false_only(X5, X10))) | ~ g_both(X5, sK12)) & ((! [X11] : (~ g_true_only(X11, X5) | ~ g_true_only(X5, X11)) & (? [X12] : (g_true_only(X12, X5) & g_true_only(X5, X12)) | ! [X13] : (g_false_only(X13, X5) | g_false_only(X5, X13) | (~ g_both(X13, X5) & ~ g_both(X5, X13))))) | ~ g_true_only(X5, sK12)))), introduced(choice_axiom, [])).
fof(f39, plain, ! [X5] : (? [X6] : (g_true_only(X6, X5) & g_true_only(X5, X6)) => (g_true_only(sK13(X5), X5) & g_true_only(X5, sK13(X5)))), introduced(choice_axiom, [])).
fof(f40, plain, ! [X5] : (? [X8] : (~ g_false_only(X8, X5) & ~ g_false_only(X5, X8)) => (~ g_false_only(sK14(X5), X5) & ~ g_false_only(X5, sK14(X5)))), introduced(choice_axiom, [])).
fof(f41, plain, ! [X5] : (? [X10] : (~ g_false_only(X10, X5) & ~ g_false_only(X5, X10)) => (~ g_false_only(sK15(X5), X5) & ~ g_false_only(X5, sK15(X5)))), introduced(choice_axiom, [])).
fof(f42, plain, ! [X5] : (? [X12] : (g_true_only(X12, X5) & g_true_only(X5, X12)) => (g_true_only(sK16(X5), X5) & g_true_only(X5, sK16(X5)))), introduced(choice_axiom, [])).
fof(f36, plain, ((sP3 | ! [X0] : (sP2(X0) | ! [X1] : (? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | ! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3) | (~ g_both(X3, X1) & ~ g_both(X1, X3))) | ~ g_both(X1, X0)))) & ? [X4] : ! [X5] : ((((? [X6] : (g_true_only(X6, X5) & g_true_only(X5, X6)) | ! [X7] : (g_false_only(X7, X5) | g_false_only(X5, X7) | (~ g_both(X7, X5) & ~ g_both(X5, X7)))) & ? [X8] : (~ g_false_only(X8, X5) & ~ g_false_only(X5, X8))) | ~ g_false_only(X5, X4)) & ((! [X9] : (~ g_true_only(X9, X5) | ~ g_true_only(X5, X9)) & ? [X10] : (~ g_false_only(X10, X5) & ~ g_false_only(X5, X10))) | ~ g_both(X5, X4)) & ((! [X11] : (~ g_true_only(X11, X5) | ~ g_true_only(X5, X11)) & (? [X12] : (g_true_only(X12, X5) & g_true_only(X5, X12)) | ! [X13] : (g_false_only(X13, X5) | g_false_only(X5, X13) | (~ g_both(X13, X5) & ~ g_both(X5, X13))))) | ~ g_true_only(X5, X4)))), inference(rectify, [], [f17])).
fof(f17, plain, ((sP3 | ! [X4] : (sP2(X4) | ! [X14] : (? [X15] : (g_true_only(X15, X14) & g_true_only(X14, X15)) | ! [X16] : (g_false_only(X16, X14) | g_false_only(X14, X16) | (~ g_both(X16, X14) & ~ g_both(X14, X16))) | ~ g_both(X14, X4)))) & ? [X17] : ! [X18] : ((((? [X19] : (g_true_only(X19, X18) & g_true_only(X18, X19)) | ! [X20] : (g_false_only(X20, X18) | g_false_only(X18, X20) | (~ g_both(X20, X18) & ~ g_both(X18, X20)))) & ? [X21] : (~ g_false_only(X21, X18) & ~ g_false_only(X18, X21))) | ~ g_false_only(X18, X17)) & ((! [X22] : (~ g_true_only(X22, X18) | ~ g_true_only(X18, X22)) & ? [X23] : (~ g_false_only(X23, X18) & ~ g_false_only(X18, X23))) | ~ g_both(X18, X17)) & ((! [X24] : (~ g_true_only(X24, X18) | ~ g_true_only(X18, X24)) & (? [X25] : (g_true_only(X25, X18) & g_true_only(X18, X25)) | ! [X26] : (g_false_only(X26, X18) | g_false_only(X18, X26) | (~ g_both(X26, X18) & ~ g_both(X18, X26))))) | ~ g_true_only(X18, X17)))), inference(definition_folding, [], [f12, e16, e15, e14, e13])).
fof(f13, plain, ! [X5] : (? [X11] : (g_true_only(X11, X5) & g_true_only(X5, X11)) | (! [X12] : (~ g_true_only(X12, X5) | ~ g_true_only(X5, X12)) & ? [X13] : (~ g_false_only(X13, X5) & ~ g_false_only(X5, X13) & (g_both(X13, X5) | g_both(X5, X13)))) | ~ sP0(X5)), inference(usedef, [], [e13])).
fof(e13, plain, ! [X5] : (sP0(X5) <=> (? [X11] : (g_true_only(X11, X5) & g_true_only(X5, X11)) | (! [X12] : (~ g_true_only(X12, X5) | ~ g_true_only(X5, X12)) & ? [X13] : (~ g_false_only(X13, X5) & ~ g_false_only(X5, X13) & (g_both(X13, X5) | g_both(X5, X13)))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f14, plain, ! [X5, X4] : ((((! [X6] : (~ g_true_only(X6, X5) | ~ g_true_only(X5, X6)) & ? [X7] : (~ g_false_only(X7, X5) & ~ g_false_only(X5, X7) & (g_both(X7, X5) | g_both(X5, X7)))) | ! [X8] : (g_false_only(X8, X5) | g_false_only(X5, X8))) & g_false_only(X5, X4)) | ~ sP1(X5, X4)), inference(usedef, [], [e14])).
fof(e14, plain, ! [X5, X4] : (sP1(X5, X4) <=> (((! [X6] : (~ g_true_only(X6, X5) | ~ g_true_only(X5, X6)) & ? [X7] : (~ g_false_only(X7, X5) & ~ g_false_only(X5, X7) & (g_both(X7, X5) | g_both(X5, X7)))) | ! [X8] : (g_false_only(X8, X5) | g_false_only(X5, X8))) & g_false_only(X5, X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f15, plain, ! [X4] : (? [X5] : (sP1(X5, X4) | ((? [X9] : (g_true_only(X9, X5) & g_true_only(X5, X9)) | ! [X10] : (g_false_only(X10, X5) | g_false_only(X5, X10))) & g_both(X5, X4)) | (sP0(X5) & g_true_only(X5, X4))) | ~ sP2(X4)), inference(usedef, [], [e15])).
fof(e15, plain, ! [X4] : (sP2(X4) <=> ? [X5] : (sP1(X5, X4) | ((? [X9] : (g_true_only(X9, X5) & g_true_only(X5, X9)) | ! [X10] : (g_false_only(X10, X5) | g_false_only(X5, X10))) & g_both(X5, X4)) | (sP0(X5) & g_true_only(X5, X4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f16, plain, (? [X0] : ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, X0)) | (! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3)) & g_true_only(X1, X0))) | ~ sP3), inference(usedef, [], [e16])).
fof(e16, plain, (sP3 <=> ? [X0] : ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, X0)) | (! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3)) & g_true_only(X1, X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f12, plain, ((? [X0] : ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, X0)) | (! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3)) & g_true_only(X1, X0))) | ! [X4] : (? [X5] : ((((! [X6] : (~ g_true_only(X6, X5) | ~ g_true_only(X5, X6)) & ? [X7] : (~ g_false_only(X7, X5) & ~ g_false_only(X5, X7) & (g_both(X7, X5) | g_both(X5, X7)))) | ! [X8] : (g_false_only(X8, X5) | g_false_only(X5, X8))) & g_false_only(X5, X4)) | ((? [X9] : (g_true_only(X9, X5) & g_true_only(X5, X9)) | ! [X10] : (g_false_only(X10, X5) | g_false_only(X5, X10))) & g_both(X5, X4)) | ((? [X11] : (g_true_only(X11, X5) & g_true_only(X5, X11)) | (! [X12] : (~ g_true_only(X12, X5) | ~ g_true_only(X5, X12)) & ? [X13] : (~ g_false_only(X13, X5) & ~ g_false_only(X5, X13) & (g_both(X13, X5) | g_both(X5, X13))))) & g_true_only(X5, X4))) | ! [X14] : (? [X15] : (g_true_only(X15, X14) & g_true_only(X14, X15)) | ! [X16] : (g_false_only(X16, X14) | g_false_only(X14, X16) | (~ g_both(X16, X14) & ~ g_both(X14, X16))) | ~ g_both(X14, X4)))) & ? [X17] : ! [X18] : ((((? [X19] : (g_true_only(X19, X18) & g_true_only(X18, X19)) | ! [X20] : (g_false_only(X20, X18) | g_false_only(X18, X20) | (~ g_both(X20, X18) & ~ g_both(X18, X20)))) & ? [X21] : (~ g_false_only(X21, X18) & ~ g_false_only(X18, X21))) | ~ g_false_only(X18, X17)) & ((! [X22] : (~ g_true_only(X22, X18) | ~ g_true_only(X18, X22)) & ? [X23] : (~ g_false_only(X23, X18) & ~ g_false_only(X18, X23))) | ~ g_both(X18, X17)) & ((! [X24] : (~ g_true_only(X24, X18) | ~ g_true_only(X18, X24)) & (? [X25] : (g_true_only(X25, X18) & g_true_only(X18, X25)) | ! [X26] : (g_false_only(X26, X18) | g_false_only(X18, X26) | (~ g_both(X26, X18) & ~ g_both(X18, X26))))) | ~ g_true_only(X18, X17)))), inference(ennf_transformation, [], [f7])).
fof(f7, plain, ~ ((~ ? [X0] : ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, X0)) | (! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3)) & g_true_only(X1, X0))) & ? [X4] : (~ ? [X5] : ((((~ ? [X6] : (g_true_only(X6, X5) & g_true_only(X5, X6)) & ? [X7] : (~ g_false_only(X7, X5) & ~ g_false_only(X5, X7) & (g_both(X7, X5) | g_both(X5, X7)))) | ! [X8] : (g_false_only(X8, X5) | g_false_only(X5, X8))) & g_false_only(X5, X4)) | ((? [X9] : (g_true_only(X9, X5) & g_true_only(X5, X9)) | ! [X10] : (g_false_only(X10, X5) | g_false_only(X5, X10))) & g_both(X5, X4)) | ((? [X11] : (g_true_only(X11, X5) & g_true_only(X5, X11)) | (~ ? [X12] : (g_true_only(X12, X5) & g_true_only(X5, X12)) & ? [X13] : (~ g_false_only(X13, X5) & ~ g_false_only(X5, X13) & (g_both(X13, X5) | g_both(X5, X13))))) & g_true_only(X5, X4))) & ? [X14] : (~ ? [X15] : (g_true_only(X15, X14) & g_true_only(X14, X15)) & ? [X16] : (~ g_false_only(X16, X14) & ~ g_false_only(X14, X16) & (g_both(X16, X14) | g_both(X14, X16))) & g_both(X14, X4)))) | ! [X17] : ? [X18] : ((((~ ? [X19] : (g_true_only(X19, X18) & g_true_only(X18, X19)) & ? [X20] : (~ g_false_only(X20, X18) & ~ g_false_only(X18, X20) & (g_both(X20, X18) | g_both(X18, X20)))) | ! [X21] : (g_false_only(X21, X18) | g_false_only(X18, X21))) & g_false_only(X18, X17)) | ((? [X22] : (g_true_only(X22, X18) & g_true_only(X18, X22)) | ! [X23] : (g_false_only(X23, X18) | g_false_only(X18, X23))) & g_both(X18, X17)) | ((? [X24] : (g_true_only(X24, X18) & g_true_only(X18, X24)) | (~ ? [X25] : (g_true_only(X25, X18) & g_true_only(X18, X25)) & ? [X26] : (~ g_false_only(X26, X18) & ~ g_false_only(X18, X26) & (g_both(X26, X18) | g_both(X18, X26))))) & g_true_only(X18, X17)))), inference(rectify, [], [f2])).
fof(f2, plain, ~ ((~ ? [X0] : ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, X0)) | (! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2)) & g_true_only(X1, X0))) & ? [X0] : (~ ? [X1] : ((((~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2)))) | ! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2))) & g_false_only(X1, X0)) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | ! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2))) & g_both(X1, X0)) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | (~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2))))) & g_true_only(X1, X0))) & ? [X1] : (~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2))) & g_both(X1, X0)))) | ! [X0] : ? [X1] : ((((~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2)))) | ! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2))) & g_false_only(X1, X0)) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | ! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2))) & g_both(X1, X0)) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | (~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2))))) & g_true_only(X1, X0)))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ((~ ? [X0] : ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, X0)) | (! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2)) & g_true_only(X1, X0))) & ? [X0] : (~ ? [X1] : ((((~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2)))) | ! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2))) & g_false_only(X1, X0)) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | ! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2))) & g_both(X1, X0)) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | (~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2))))) & g_true_only(X1, X0))) & ? [X1] : (~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2))) & g_both(X1, X0)))) | ! [X0] : ? [X1] : ((((~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2)))) | ! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2))) & g_false_only(X1, X0)) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | ! [X2] : (g_false_only(X2, X1) | g_false_only(X1, X2))) & g_both(X1, X0)) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | (~ ? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & ? [X2] : (~ g_false_only(X2, X1) & ~ g_false_only(X1, X2) & (g_both(X2, X1) | g_both(X1, X2))))) & g_true_only(X1, X0)))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO604+1.p', nc3)).
fof(f4053, plain, (g_false_only(sK12, sK14(sK12)) | ~ spl17_146), inference(avatar_component_clause, [], [f4051])).
fof(f4051, plain, (spl17_146 <=> g_false_only(sK12, sK14(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_146])])).
fof(f11372, plain, (spl17_310 | spl17_311 | spl17_146 | spl17_13 | spl17_139), inference(avatar_split_clause, [], [f11371, f3812, f188, f4051, f10681, f10674])).
fof(f10674, plain, (spl17_310 <=> g_true_only(sK12, sK14(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_310])])).
fof(f10681, plain, (spl17_311 <=> g_true_only(sK12, sK13(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_311])])).
fof(f188, plain, (spl17_13 <=> g_both(sK12, sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_13])])).
fof(f3812, plain, (spl17_139 <=> g_false_only(sK14(sK12), sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_139])])).
fof(f11371, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK12, sK13(sK12)) | g_true_only(sK12, sK14(sK12)) | (spl17_13 | spl17_139)), inference(subsumption_resolution, [], [f11303, f79])).
fof(f79, plain, ! [X5, X11] : (~ g_true_only(X11, X5) | ~ g_true_only(X5, X11) | ~ g_true_only(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f11303, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK12, sK13(sK12)) | g_true_only(sK12, sK12) | g_true_only(sK12, sK14(sK12)) | (spl17_13 | spl17_139)), inference(subsumption_resolution, [], [f11219, f190])).
fof(f190, plain, (~ g_both(sK12, sK12) | spl17_13), inference(avatar_component_clause, [], [f188])).
fof(f11219, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK12, sK13(sK12)) | g_both(sK12, sK12) | g_true_only(sK12, sK12) | g_true_only(sK12, sK14(sK12)) | spl17_139), inference(resolution, [], [f3814, f2186])).
fof(f2186, plain, ! [X1] : (g_false_only(sK14(X1), X1) | g_false_only(X1, sK14(X1)) | g_true_only(X1, sK13(X1)) | g_both(X1, sK12) | g_true_only(X1, sK12) | g_true_only(X1, sK14(X1))), inference(subsumption_resolution, [], [f2175, f102])).
fof(f102, plain, ! [X0, X1] : (g_false_only(X0, X1) | g_both(X0, X1) | g_true_only(X0, X1)), inference(cnf_transformation, [], [f11])).
fof(f11, plain, ! [X0, X1] : (g_false_only(X0, X1) | g_both(X0, X1) | g_true_only(X0, X1)), inference(rectify, [], [f6])).
fof(f6, plain, ! [X3, X4] : (g_false_only(X3, X4) | g_both(X3, X4) | g_true_only(X3, X4)), file('/home/ubuntu/library/tptp/Problems/SYO/SYO604+1.p', exhaustion_g)).
fof(f2175, plain, ! [X1] : (g_false_only(X1, sK14(X1)) | g_false_only(sK14(X1), X1) | g_true_only(X1, sK13(X1)) | g_both(X1, sK12) | g_true_only(X1, sK12) | g_true_only(X1, sK14(X1)) | ~ g_false_only(X1, sK12)), inference(resolution, [], [f348, f149])).
fof(f149, plain, ! [X3] : (g_both(X3, sK14(X3)) | g_true_only(X3, sK14(X3)) | ~ g_false_only(X3, sK12)), inference(resolution, [], [f102, f83])).
fof(f348, plain, ! [X0, X1] : (~ g_both(X1, X0) | g_false_only(X1, X0) | g_false_only(X0, X1) | g_true_only(X1, sK13(X1)) | g_both(X1, sK12) | g_true_only(X1, sK12)), inference(resolution, [], [f85, f102])).
fof(f85, plain, ! [X7, X5] : (~ g_false_only(X5, sK12) | g_false_only(X7, X5) | g_false_only(X5, X7) | ~ g_both(X5, X7) | g_true_only(X5, sK13(X5))), inference(cnf_transformation, [], [f43])).
fof(f3814, plain, (~ g_false_only(sK14(sK12), sK12) | spl17_139), inference(avatar_component_clause, [], [f3812])).
fof(f11367, plain, (spl17_310 | spl17_20 | spl17_309 | spl17_146 | spl17_13 | spl17_139), inference(avatar_split_clause, [], [f11286, f3812, f188, f4051, f10668, f288, f10674])).
fof(f288, plain, (spl17_20 <=> g_true_only(sK12, sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_20])])).
fof(f10668, plain, (spl17_309 <=> g_true_only(sK13(sK12), sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_309])])).
fof(f11286, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK13(sK12), sK12) | g_true_only(sK12, sK12) | g_true_only(sK12, sK14(sK12)) | (spl17_13 | spl17_139)), inference(subsumption_resolution, [], [f11217, f190])).
fof(f11217, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK13(sK12), sK12) | g_both(sK12, sK12) | g_true_only(sK12, sK12) | g_true_only(sK12, sK14(sK12)) | spl17_139), inference(resolution, [], [f3814, f2262])).
fof(f2262, plain, ! [X1] : (g_false_only(sK14(X1), X1) | g_false_only(X1, sK14(X1)) | g_true_only(sK13(X1), X1) | g_both(X1, sK12) | g_true_only(X1, sK12) | g_true_only(X1, sK14(X1))), inference(subsumption_resolution, [], [f2251, f102])).
fof(f2251, plain, ! [X1] : (g_false_only(X1, sK14(X1)) | g_false_only(sK14(X1), X1) | g_true_only(sK13(X1), X1) | g_both(X1, sK12) | g_true_only(X1, sK12) | g_true_only(X1, sK14(X1)) | ~ g_false_only(X1, sK12)), inference(resolution, [], [f350, f149])).
fof(f350, plain, ! [X0, X1] : (~ g_both(X1, X0) | g_false_only(X1, X0) | g_false_only(X0, X1) | g_true_only(sK13(X1), X1) | g_both(X1, sK12) | g_true_only(X1, sK12)), inference(resolution, [], [f87, f102])).
fof(f87, plain, ! [X7, X5] : (~ g_false_only(X5, sK12) | g_false_only(X7, X5) | g_false_only(X5, X7) | ~ g_both(X5, X7) | g_true_only(sK13(X5), X5)), inference(cnf_transformation, [], [f43])).
fof(f11365, plain, (spl17_135 | spl17_20 | spl17_309 | spl17_146 | spl17_13 | spl17_139), inference(avatar_split_clause, [], [f11364, f3812, f188, f4051, f10668, f288, f3772])).
fof(f3772, plain, (spl17_135 <=> g_true_only(sK14(sK12), sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_135])])).
fof(f11364, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK13(sK12), sK12) | g_true_only(sK12, sK12) | g_true_only(sK14(sK12), sK12) | (spl17_13 | spl17_139)), inference(subsumption_resolution, [], [f11216, f190])).
fof(f11216, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK13(sK12), sK12) | g_both(sK12, sK12) | g_true_only(sK12, sK12) | g_true_only(sK14(sK12), sK12) | spl17_139), inference(resolution, [], [f3814, f2322])).
fof(f2322, plain, ! [X8] : (g_false_only(sK14(X8), X8) | g_false_only(X8, sK14(X8)) | g_true_only(sK13(X8), X8) | g_both(X8, sK12) | g_true_only(X8, sK12) | g_true_only(sK14(X8), X8)), inference(subsumption_resolution, [], [f2297, f102])).
fof(f2297, plain, ! [X8] : (g_false_only(X8, sK14(X8)) | g_false_only(sK14(X8), X8) | g_true_only(sK13(X8), X8) | g_both(X8, sK12) | g_true_only(X8, sK12) | g_true_only(sK14(X8), X8) | ~ g_false_only(X8, sK12)), inference(resolution, [], [f354, f151])).
fof(f151, plain, ! [X5] : (g_both(sK14(X5), X5) | g_true_only(sK14(X5), X5) | ~ g_false_only(X5, sK12)), inference(resolution, [], [f102, f84])).
fof(f84, plain, ! [X5] : (~ g_false_only(sK14(X5), X5) | ~ g_false_only(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f354, plain, ! [X0, X1] : (~ g_both(X0, X1) | g_false_only(X1, X0) | g_false_only(X0, X1) | g_true_only(sK13(X1), X1) | g_both(X1, sK12) | g_true_only(X1, sK12)), inference(resolution, [], [f88, f102])).
fof(f88, plain, ! [X7, X5] : (~ g_false_only(X5, sK12) | g_false_only(X7, X5) | g_false_only(X5, X7) | ~ g_both(X7, X5) | g_true_only(sK13(X5), X5)), inference(cnf_transformation, [], [f43])).
fof(f11359, plain, (spl17_135 | spl17_311 | spl17_146 | spl17_13 | spl17_139), inference(avatar_split_clause, [], [f11358, f3812, f188, f4051, f10681, f3772])).
fof(f11358, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK12, sK13(sK12)) | g_true_only(sK14(sK12), sK12) | (spl17_13 | spl17_139)), inference(subsumption_resolution, [], [f11357, f79])).
fof(f11357, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK12, sK13(sK12)) | g_true_only(sK12, sK12) | g_true_only(sK14(sK12), sK12) | (spl17_13 | spl17_139)), inference(subsumption_resolution, [], [f11218, f190])).
fof(f11218, plain, (g_false_only(sK12, sK14(sK12)) | g_true_only(sK12, sK13(sK12)) | g_both(sK12, sK12) | g_true_only(sK12, sK12) | g_true_only(sK14(sK12), sK12) | spl17_139), inference(resolution, [], [f3814, f2246])).
fof(f2246, plain, ! [X8] : (g_false_only(sK14(X8), X8) | g_false_only(X8, sK14(X8)) | g_true_only(X8, sK13(X8)) | g_both(X8, sK12) | g_true_only(X8, sK12) | g_true_only(sK14(X8), X8)), inference(subsumption_resolution, [], [f2221, f102])).
fof(f2221, plain, ! [X8] : (g_false_only(X8, sK14(X8)) | g_false_only(sK14(X8), X8) | g_true_only(X8, sK13(X8)) | g_both(X8, sK12) | g_true_only(X8, sK12) | g_true_only(sK14(X8), X8) | ~ g_false_only(X8, sK12)), inference(resolution, [], [f349, f151])).
fof(f349, plain, ! [X0, X1] : (~ g_both(X0, X1) | g_false_only(X1, X0) | g_false_only(X0, X1) | g_true_only(X1, sK13(X1)) | g_both(X1, sK12) | g_true_only(X1, sK12)), inference(resolution, [], [f86, f102])).
fof(f86, plain, ! [X7, X5] : (~ g_false_only(X5, sK12) | g_false_only(X7, X5) | g_false_only(X5, X7) | ~ g_both(X7, X5) | g_true_only(X5, sK13(X5))), inference(cnf_transformation, [], [f43])).
fof(f11349, plain, ~ spl17_20, inference(avatar_contradiction_clause, [], [f11348])).
fof(f11348, plain, ($false | ~ spl17_20), inference(subsumption_resolution, [], [f289, f79])).
fof(f289, plain, (g_true_only(sK12, sK12) | ~ spl17_20), inference(avatar_component_clause, [], [f288])).
fof(f11285, plain, (~ spl17_309 | ~ spl17_311), inference(avatar_split_clause, [], [f11258, f10681, f10668])).
fof(f11258, plain, (~ g_true_only(sK13(sK12), sK12) | ~ spl17_311), inference(duplicate_literal_removal, [], [f11256])).
fof(f11256, plain, (~ g_true_only(sK13(sK12), sK12) | ~ g_true_only(sK13(sK12), sK12) | ~ spl17_311), inference(resolution, [], [f10683, f79])).
fof(f10683, plain, (g_true_only(sK12, sK13(sK12)) | ~ spl17_311), inference(avatar_component_clause, [], [f10681])).
fof(f11182, plain, (~ spl17_310 | ~ spl17_18 | ~ spl17_135), inference(avatar_split_clause, [], [f11174, f3772, f226, f10674])).
fof(f226, plain, (spl17_18 <=> ! [X2] : (~ g_true_only(sK14(sK12), X2) | ~ g_true_only(X2, sK14(sK12)))), introduced(avatar_definition, [new_symbols(naming, [spl17_18])])).
fof(f11174, plain, (~ g_true_only(sK12, sK14(sK12)) | (~ spl17_18 | ~ spl17_135)), inference(resolution, [], [f3773, f227])).
fof(f227, plain, (! [X2] : (~ g_true_only(sK14(sK12), X2) | ~ g_true_only(X2, sK14(sK12))) | ~ spl17_18), inference(avatar_component_clause, [], [f226])).
fof(f3773, plain, (g_true_only(sK14(sK12), sK12) | ~ spl17_135), inference(avatar_component_clause, [], [f3772])).
fof(f10710, plain, (~ spl17_19 | ~ spl17_139), inference(avatar_split_clause, [], [f4194, f3812, f229])).
fof(f4194, plain, (~ g_false_only(sK12, sK12) | ~ spl17_139), inference(resolution, [], [f3813, f84])).
fof(f3813, plain, (g_false_only(sK14(sK12), sK12) | ~ spl17_139), inference(avatar_component_clause, [], [f3812])).
fof(f10656, plain, (~ spl17_219 | ~ spl17_80), inference(avatar_split_clause, [], [f8440, f1569, f6028])).
fof(f6028, plain, (spl17_219 <=> g_true_only(sK6(sK12), sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_219])])).
fof(f1569, plain, (spl17_80 <=> g_both(sK6(sK12), sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_80])])).
fof(f8440, plain, (~ g_true_only(sK6(sK12), sK12) | ~ spl17_80), inference(resolution, [], [f8409, f94])).
fof(f94, plain, ! [X0, X1] : (~ g_false(X0, X1) | ~ g_true_only(X0, X1)), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ! [X0, X1] : ((g_true_only(X0, X1) | g_false(X0, X1) | ~ g_true(X0, X1)) & ((~ g_false(X0, X1) & g_true(X0, X1)) | ~ g_true_only(X0, X1))), inference(flattening, [], [f44])).
fof(f44, plain, ! [X0, X1] : ((g_true_only(X0, X1) | (g_false(X0, X1) | ~ g_true(X0, X1))) & ((~ g_false(X0, X1) & g_true(X0, X1)) | ~ g_true_only(X0, X1))), inference(nnf_transformation, [], [f8])).
fof(f8, plain, ! [X0, X1] : (g_true_only(X0, X1) <=> (~ g_false(X0, X1) & g_true(X0, X1))), inference(rectify, [], [f3])).
fof(f3, plain, ! [X3, X4] : (g_true_only(X3, X4) <=> (~ g_false(X3, X4) & g_true(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO604+1.p', true_only_g)).
fof(f8409, plain, (g_false(sK6(sK12), sK12) | ~ spl17_80), inference(resolution, [], [f1571, f97])).
fof(f97, plain, ! [X0, X1] : (~ g_both(X0, X1) | g_false(X0, X1)), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ! [X0, X1] : ((g_both(X0, X1) | ~ g_false(X0, X1) | ~ g_true(X0, X1)) & ((g_false(X0, X1) & g_true(X0, X1)) | ~ g_both(X0, X1))), inference(flattening, [], [f46])).
fof(f46, plain, ! [X0, X1] : ((g_both(X0, X1) | (~ g_false(X0, X1) | ~ g_true(X0, X1))) & ((g_false(X0, X1) & g_true(X0, X1)) | ~ g_both(X0, X1))), inference(nnf_transformation, [], [f9])).
fof(f9, plain, ! [X0, X1] : (g_both(X0, X1) <=> (g_false(X0, X1) & g_true(X0, X1))), inference(rectify, [], [f4])).
fof(f4, plain, ! [X3, X4] : (g_both(X3, X4) <=> (g_false(X3, X4) & g_true(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO604+1.p', both_g)).
fof(f1571, plain, (g_both(sK6(sK12), sK12) | ~ spl17_80), inference(avatar_component_clause, [], [f1569])).
fof(f10639, plain, (~ spl17_13 | ~ spl17_22 | ~ spl17_80 | spl17_98 | spl17_99 | spl17_219), inference(avatar_contradiction_clause, [], [f10638])).
fof(f10638, plain, ($false | (~ spl17_13 | ~ spl17_22 | ~ spl17_80 | spl17_98 | spl17_99 | spl17_219)), inference(subsumption_resolution, [], [f10637, f2516])).
fof(f2516, plain, (~ sP1(sK6(sK12), sK12) | spl17_98), inference(avatar_component_clause, [], [f2515])).
fof(f2515, plain, (spl17_98 <=> sP1(sK6(sK12), sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_98])])).
fof(f10637, plain, (sP1(sK6(sK12), sK12) | (~ spl17_13 | ~ spl17_22 | ~ spl17_80 | spl17_99 | spl17_219)), inference(subsumption_resolution, [], [f10636, f1571])).
fof(f10636, plain, (~ g_both(sK6(sK12), sK12) | sP1(sK6(sK12), sK12) | (~ spl17_13 | ~ spl17_22 | spl17_99 | spl17_219)), inference(subsumption_resolution, [], [f10635, f1472])).
fof(f1472, plain, (sP2(sK12) | (~ spl17_13 | ~ spl17_22)), inference(resolution, [], [f386, f189])).
fof(f189, plain, (g_both(sK12, sK12) | ~ spl17_13), inference(avatar_component_clause, [], [f188])).
fof(f386, plain, (! [X8] : (~ g_both(sK12, X8) | sP2(X8)) | ~ spl17_22), inference(avatar_component_clause, [], [f385])).
fof(f385, plain, (spl17_22 <=> ! [X8] : (~ g_both(sK12, X8) | sP2(X8))), introduced(avatar_definition, [new_symbols(naming, [spl17_22])])).
fof(f10635, plain, (~ sP2(sK12) | ~ g_both(sK6(sK12), sK12) | sP1(sK6(sK12), sK12) | (spl17_99 | spl17_219)), inference(subsumption_resolution, [], [f10614, f6029])).
fof(f6029, plain, (~ g_true_only(sK6(sK12), sK12) | spl17_219), inference(avatar_component_clause, [], [f6028])).
fof(f10614, plain, (g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | ~ g_both(sK6(sK12), sK12) | sP1(sK6(sK12), sK12) | spl17_99), inference(resolution, [], [f2520, f2364])).
fof(f2364, plain, ! [X3] : (g_true(sK7(X3), sK6(X3)) | g_true_only(sK6(X3), X3) | ~ sP2(X3) | ~ g_both(sK6(X3), sK12) | sP1(sK6(X3), X3)), inference(resolution, [], [f838, f93])).
fof(f93, plain, ! [X0, X1] : (~ g_true_only(X0, X1) | g_true(X0, X1)), inference(cnf_transformation, [], [f45])).
fof(f838, plain, ! [X16] : (g_true_only(sK7(X16), sK6(X16)) | sP1(sK6(X16), X16) | g_true_only(sK6(X16), X16) | ~ sP2(X16) | ~ g_both(sK6(X16), sK12)), inference(subsumption_resolution, [], [f827, f81])).
fof(f81, plain, ! [X5] : (~ g_false_only(sK15(X5), X5) | ~ g_both(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f827, plain, ! [X16] : (g_true_only(sK7(X16), sK6(X16)) | g_false_only(sK15(sK6(X16)), sK6(X16)) | sP1(sK6(X16), X16) | g_true_only(sK6(X16), X16) | ~ sP2(X16) | ~ g_both(sK6(X16), sK12)), inference(resolution, [], [f60, f80])).
fof(f80, plain, ! [X5] : (~ g_false_only(X5, sK15(X5)) | ~ g_both(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f60, plain, ! [X0, X3] : (g_true_only(sK7(X0), sK6(X0)) | g_false_only(sK6(X0), X3) | g_false_only(X3, sK6(X0)) | sP1(sK6(X0), X0) | g_true_only(sK6(X0), X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : ((sP1(sK6(X0), X0) | (((g_true_only(sK7(X0), sK6(X0)) & g_true_only(sK6(X0), sK7(X0))) | ! [X3] : (g_false_only(X3, sK6(X0)) | g_false_only(sK6(X0), X3))) & g_both(sK6(X0), X0)) | (sP0(sK6(X0)) & g_true_only(sK6(X0), X0))) | ~ sP2(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7])], [f23, f25, f24])).
fof(f24, plain, ! [X0] : (? [X1] : (sP1(X1, X0) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | ! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3))) & g_both(X1, X0)) | (sP0(X1) & g_true_only(X1, X0))) => (sP1(sK6(X0), X0) | ((? [X2] : (g_true_only(X2, sK6(X0)) & g_true_only(sK6(X0), X2)) | ! [X3] : (g_false_only(X3, sK6(X0)) | g_false_only(sK6(X0), X3))) & g_both(sK6(X0), X0)) | (sP0(sK6(X0)) & g_true_only(sK6(X0), X0)))), introduced(choice_axiom, [])).
fof(f25, plain, ! [X0] : (? [X2] : (g_true_only(X2, sK6(X0)) & g_true_only(sK6(X0), X2)) => (g_true_only(sK7(X0), sK6(X0)) & g_true_only(sK6(X0), sK7(X0)))), introduced(choice_axiom, [])).
fof(f23, plain, ! [X0] : (? [X1] : (sP1(X1, X0) | ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) | ! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3))) & g_both(X1, X0)) | (sP0(X1) & g_true_only(X1, X0))) | ~ sP2(X0)), inference(rectify, [], [f22])).
fof(f22, plain, ! [X4] : (? [X5] : (sP1(X5, X4) | ((? [X9] : (g_true_only(X9, X5) & g_true_only(X5, X9)) | ! [X10] : (g_false_only(X10, X5) | g_false_only(X5, X10))) & g_both(X5, X4)) | (sP0(X5) & g_true_only(X5, X4))) | ~ sP2(X4)), inference(nnf_transformation, [], [f15])).
fof(f2520, plain, (~ g_true(sK7(sK12), sK6(sK12)) | spl17_99), inference(avatar_component_clause, [], [f2519])).
fof(f2519, plain, (spl17_99 <=> g_true(sK7(sK12), sK6(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_99])])).
fof(f10594, plain, (~ spl17_13 | ~ spl17_22 | ~ spl17_80 | spl17_98 | spl17_100 | spl17_219), inference(avatar_contradiction_clause, [], [f10593])).
fof(f10593, plain, ($false | (~ spl17_13 | ~ spl17_22 | ~ spl17_80 | spl17_98 | spl17_100 | spl17_219)), inference(subsumption_resolution, [], [f10592, f2516])).
fof(f10592, plain, (sP1(sK6(sK12), sK12) | (~ spl17_13 | ~ spl17_22 | ~ spl17_80 | spl17_100 | spl17_219)), inference(subsumption_resolution, [], [f10591, f1571])).
fof(f10591, plain, (~ g_both(sK6(sK12), sK12) | sP1(sK6(sK12), sK12) | (~ spl17_13 | ~ spl17_22 | spl17_100 | spl17_219)), inference(subsumption_resolution, [], [f10590, f1472])).
fof(f10590, plain, (~ sP2(sK12) | ~ g_both(sK6(sK12), sK12) | sP1(sK6(sK12), sK12) | (spl17_100 | spl17_219)), inference(subsumption_resolution, [], [f10569, f6029])).
fof(f10569, plain, (g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | ~ g_both(sK6(sK12), sK12) | sP1(sK6(sK12), sK12) | spl17_100), inference(resolution, [], [f2528, f2339])).
fof(f2339, plain, ! [X3] : (g_true(sK6(X3), sK7(X3)) | g_true_only(sK6(X3), X3) | ~ sP2(X3) | ~ g_both(sK6(X3), sK12) | sP1(sK6(X3), X3)), inference(resolution, [], [f813, f93])).
fof(f813, plain, ! [X16] : (g_true_only(sK6(X16), sK7(X16)) | sP1(sK6(X16), X16) | g_true_only(sK6(X16), X16) | ~ sP2(X16) | ~ g_both(sK6(X16), sK12)), inference(subsumption_resolution, [], [f799, f81])).
fof(f799, plain, ! [X16] : (g_true_only(sK6(X16), sK7(X16)) | g_false_only(sK15(sK6(X16)), sK6(X16)) | sP1(sK6(X16), X16) | g_true_only(sK6(X16), X16) | ~ sP2(X16) | ~ g_both(sK6(X16), sK12)), inference(resolution, [], [f58, f80])).
fof(f58, plain, ! [X0, X3] : (g_true_only(sK6(X0), sK7(X0)) | g_false_only(sK6(X0), X3) | g_false_only(X3, sK6(X0)) | sP1(sK6(X0), X0) | g_true_only(sK6(X0), X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f26])).
fof(f2528, plain, (~ g_true(sK6(sK12), sK7(sK12)) | spl17_100), inference(avatar_component_clause, [], [f2527])).
fof(f2527, plain, (spl17_100 <=> g_true(sK6(sK12), sK7(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_100])])).
fof(f10519, plain, (spl17_282 | ~ spl17_13 | ~ spl17_22 | spl17_98 | ~ spl17_99 | spl17_101 | spl17_219), inference(avatar_split_clause, [], [f10518, f6028, f2535, f2519, f2515, f385, f188, f8585])).
fof(f8585, plain, (spl17_282 <=> g_false_only(sK6(sK12), sK7(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_282])])).
fof(f2535, plain, (spl17_101 <=> g_true_only(sK7(sK12), sK6(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_101])])).
fof(f10518, plain, (g_false_only(sK6(sK12), sK7(sK12)) | (~ spl17_13 | ~ spl17_22 | spl17_98 | ~ spl17_99 | spl17_101 | spl17_219)), inference(subsumption_resolution, [], [f10517, f1472])).
fof(f10517, plain, (g_false_only(sK6(sK12), sK7(sK12)) | ~ sP2(sK12) | (spl17_98 | ~ spl17_99 | spl17_101 | spl17_219)), inference(subsumption_resolution, [], [f10516, f6029])).
fof(f10516, plain, (g_false_only(sK6(sK12), sK7(sK12)) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | (spl17_98 | ~ spl17_99 | spl17_101)), inference(subsumption_resolution, [], [f10515, f2516])).
fof(f10515, plain, (g_false_only(sK6(sK12), sK7(sK12)) | sP1(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | (~ spl17_99 | spl17_101)), inference(subsumption_resolution, [], [f9068, f2536])).
fof(f2536, plain, (~ g_true_only(sK7(sK12), sK6(sK12)) | spl17_101), inference(avatar_component_clause, [], [f2535])).
fof(f9068, plain, (g_true_only(sK7(sK12), sK6(sK12)) | g_false_only(sK6(sK12), sK7(sK12)) | sP1(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | ~ spl17_99), inference(resolution, [], [f8583, f60])).
fof(f8583, plain, (~ g_false_only(sK7(sK12), sK6(sK12)) | ~ spl17_99), inference(resolution, [], [f2521, f100])).
fof(f100, plain, ! [X0, X1] : (~ g_true(X0, X1) | ~ g_false_only(X0, X1)), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ! [X0, X1] : ((g_false_only(X0, X1) | g_true(X0, X1) | ~ g_false(X0, X1)) & ((~ g_true(X0, X1) & g_false(X0, X1)) | ~ g_false_only(X0, X1))), inference(flattening, [], [f48])).
fof(f48, plain, ! [X0, X1] : ((g_false_only(X0, X1) | (g_true(X0, X1) | ~ g_false(X0, X1))) & ((~ g_true(X0, X1) & g_false(X0, X1)) | ~ g_false_only(X0, X1))), inference(nnf_transformation, [], [f10])).
fof(f10, plain, ! [X0, X1] : (g_false_only(X0, X1) <=> (~ g_true(X0, X1) & g_false(X0, X1))), inference(rectify, [], [f5])).
fof(f5, plain, ! [X3, X4] : (g_false_only(X3, X4) <=> (~ g_true(X3, X4) & g_false(X3, X4))), file('/home/ubuntu/library/tptp/Problems/SYO/SYO604+1.p', false_only_g)).
fof(f2521, plain, (g_true(sK7(sK12), sK6(sK12)) | ~ spl17_99), inference(avatar_component_clause, [], [f2519])).
fof(f10388, plain, (spl17_109 | ~ spl17_80), inference(avatar_split_clause, [], [f8397, f1569, f2599])).
fof(f2599, plain, (spl17_109 <=> ! [X4] : (~ g_true_only(sK6(sK12), X4) | ~ g_true_only(X4, sK6(sK12)))), introduced(avatar_definition, [new_symbols(naming, [spl17_109])])).
fof(f8397, plain, (! [X0] : (~ g_true_only(sK6(sK12), X0) | ~ g_true_only(X0, sK6(sK12))) | ~ spl17_80), inference(resolution, [], [f1571, f82])).
fof(f82, plain, ! [X5, X9] : (~ g_both(X5, sK12) | ~ g_true_only(X5, X9) | ~ g_true_only(X9, X5)), inference(cnf_transformation, [], [f43])).
fof(f10387, plain, (~ spl17_78 | ~ spl17_83 | ~ spl17_109), inference(avatar_contradiction_clause, [], [f10386])).
fof(f10386, plain, ($false | (~ spl17_78 | ~ spl17_83 | ~ spl17_109)), inference(subsumption_resolution, [], [f10342, f1564])).
fof(f1564, plain, (g_true_only(sK13(sK6(sK12)), sK6(sK12)) | ~ spl17_78), inference(avatar_component_clause, [], [f1562])).
fof(f1562, plain, (spl17_78 <=> g_true_only(sK13(sK6(sK12)), sK6(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_78])])).
fof(f10342, plain, (~ g_true_only(sK13(sK6(sK12)), sK6(sK12)) | (~ spl17_83 | ~ spl17_109)), inference(resolution, [], [f2600, f1586])).
fof(f1586, plain, (g_true_only(sK6(sK12), sK13(sK6(sK12))) | ~ spl17_83), inference(avatar_component_clause, [], [f1584])).
fof(f1584, plain, (spl17_83 <=> g_true_only(sK6(sK12), sK13(sK6(sK12)))), introduced(avatar_definition, [new_symbols(naming, [spl17_83])])).
fof(f2600, plain, (! [X4] : (~ g_true_only(sK6(sK12), X4) | ~ g_true_only(X4, sK6(sK12))) | ~ spl17_109), inference(avatar_component_clause, [], [f2599])).
fof(f10383, plain, (~ spl17_13 | ~ spl17_22 | ~ spl17_80 | spl17_98 | ~ spl17_101 | ~ spl17_109 | spl17_219), inference(avatar_contradiction_clause, [], [f10382])).
fof(f10382, plain, ($false | (~ spl17_13 | ~ spl17_22 | ~ spl17_80 | spl17_98 | ~ spl17_101 | ~ spl17_109 | spl17_219)), inference(subsumption_resolution, [], [f10381, f1571])).
fof(f10381, plain, (~ g_both(sK6(sK12), sK12) | (~ spl17_13 | ~ spl17_22 | spl17_98 | ~ spl17_101 | ~ spl17_109 | spl17_219)), inference(subsumption_resolution, [], [f10380, f1472])).
fof(f10380, plain, (~ sP2(sK12) | ~ g_both(sK6(sK12), sK12) | (spl17_98 | ~ spl17_101 | ~ spl17_109 | spl17_219)), inference(subsumption_resolution, [], [f10379, f6029])).
fof(f10379, plain, (g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | ~ g_both(sK6(sK12), sK12) | (spl17_98 | ~ spl17_101 | ~ spl17_109)), inference(subsumption_resolution, [], [f10378, f2516])).
fof(f10378, plain, (sP1(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | ~ g_both(sK6(sK12), sK12) | (~ spl17_101 | ~ spl17_109)), inference(subsumption_resolution, [], [f10322, f2537])).
fof(f2537, plain, (g_true_only(sK7(sK12), sK6(sK12)) | ~ spl17_101), inference(avatar_component_clause, [], [f2535])).
fof(f10322, plain, (~ g_true_only(sK7(sK12), sK6(sK12)) | sP1(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | ~ g_both(sK6(sK12), sK12) | ~ spl17_109), inference(resolution, [], [f2600, f813])).
fof(f8803, plain, (~ spl17_282 | ~ spl17_100), inference(avatar_split_clause, [], [f8802, f2527, f8585])).
fof(f8802, plain, (~ g_false_only(sK6(sK12), sK7(sK12)) | ~ spl17_100), inference(resolution, [], [f2529, f100])).
fof(f2529, plain, (g_true(sK6(sK12), sK7(sK12)) | ~ spl17_100), inference(avatar_component_clause, [], [f2527])).
fof(f8368, plain, (spl17_80 | ~ spl17_13 | ~ spl17_22 | spl17_81 | spl17_98), inference(avatar_split_clause, [], [f8367, f2515, f1573, f385, f188, f1569])).
fof(f1573, plain, (spl17_81 <=> sP0(sK6(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_81])])).
fof(f8367, plain, (g_both(sK6(sK12), sK12) | (~ spl17_13 | ~ spl17_22 | spl17_81 | spl17_98)), inference(subsumption_resolution, [], [f8366, f1472])).
fof(f8366, plain, (g_both(sK6(sK12), sK12) | ~ sP2(sK12) | (spl17_81 | spl17_98)), inference(subsumption_resolution, [], [f8357, f1574])).
fof(f1574, plain, (~ sP0(sK6(sK12)) | spl17_81), inference(avatar_component_clause, [], [f1573])).
fof(f8357, plain, (g_both(sK6(sK12), sK12) | sP0(sK6(sK12)) | ~ sP2(sK12) | spl17_98), inference(resolution, [], [f2516, f57])).
fof(f57, plain, ! [X0] : (sP1(sK6(X0), X0) | g_both(sK6(X0), X0) | sP0(sK6(X0)) | ~ sP2(X0)), inference(cnf_transformation, [], [f26])).
fof(f8207, plain, (spl17_107 | spl17_105 | spl17_232 | ~ spl17_13 | ~ spl17_22 | ~ spl17_79 | spl17_80 | spl17_219 | spl17_233), inference(avatar_split_clause, [], [f8206, f6651, f6028, f1569, f1566, f385, f188, f6647, f2580, f2588])).
fof(f2588, plain, (spl17_107 <=> g_both(sK6(sK12), sK8(sK6(sK12)))), introduced(avatar_definition, [new_symbols(naming, [spl17_107])])).
fof(f2580, plain, (spl17_105 <=> ! [X0] : (g_false_only(sK6(sK12), X0) | g_false_only(X0, sK6(sK12)))), introduced(avatar_definition, [new_symbols(naming, [spl17_105])])).
fof(f6647, plain, (spl17_232 <=> g_false_only(sK6(sK12), sK8(sK6(sK12)))), introduced(avatar_definition, [new_symbols(naming, [spl17_232])])).
fof(f1566, plain, (spl17_79 <=> ! [X1] : (g_false_only(X1, sK6(sK12)) | ~ g_both(X1, sK6(sK12)) | g_false_only(sK6(sK12), X1))), introduced(avatar_definition, [new_symbols(naming, [spl17_79])])).
fof(f6651, plain, (spl17_233 <=> g_false_only(sK8(sK6(sK12)), sK6(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_233])])).
fof(f8206, plain, (! [X0] : (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK6(sK12), X0) | g_false_only(X0, sK6(sK12)) | g_both(sK6(sK12), sK8(sK6(sK12)))) | (~ spl17_13 | ~ spl17_22 | ~ spl17_79 | spl17_80 | spl17_219 | spl17_233)), inference(subsumption_resolution, [], [f8205, f1472])).
fof(f8205, plain, (! [X0] : (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK6(sK12), X0) | g_false_only(X0, sK6(sK12)) | g_both(sK6(sK12), sK8(sK6(sK12))) | ~ sP2(sK12)) | (~ spl17_79 | spl17_80 | spl17_219 | spl17_233)), inference(subsumption_resolution, [], [f8204, f6029])).
fof(f8204, plain, (! [X0] : (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK6(sK12), X0) | g_false_only(X0, sK6(sK12)) | g_both(sK6(sK12), sK8(sK6(sK12))) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12)) | (~ spl17_79 | spl17_80 | spl17_233)), inference(subsumption_resolution, [], [f7066, f1570])).
fof(f1570, plain, (~ g_both(sK6(sK12), sK12) | spl17_80), inference(avatar_component_clause, [], [f1569])).
fof(f7066, plain, (! [X0] : (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK6(sK12), X0) | g_false_only(X0, sK6(sK12)) | g_both(sK6(sK12), sK8(sK6(sK12))) | g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12)) | (~ spl17_79 | spl17_233)), inference(subsumption_resolution, [], [f6085, f6652])).
fof(f6652, plain, (~ g_false_only(sK8(sK6(sK12)), sK6(sK12)) | spl17_233), inference(avatar_component_clause, [], [f6651])).
fof(f6085, plain, (! [X0] : (g_false_only(sK8(sK6(sK12)), sK6(sK12)) | g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK6(sK12), X0) | g_false_only(X0, sK6(sK12)) | g_both(sK6(sK12), sK8(sK6(sK12))) | g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12)) | ~ spl17_79), inference(resolution, [], [f1567, f355])).
fof(f355, plain, ! [X0, X1] : (g_both(sK8(sK6(X0)), sK6(X0)) | g_false_only(sK6(X0), X1) | g_false_only(X1, sK6(X0)) | g_both(sK6(X0), sK8(sK6(X0))) | g_both(sK6(X0), X0) | g_true_only(sK6(X0), X0) | ~ sP2(X0)), inference(resolution, [], [f63, f56])).
fof(f56, plain, ! [X0] : (sP1(sK6(X0), X0) | g_both(sK6(X0), X0) | g_true_only(sK6(X0), X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f26])).
fof(f63, plain, ! [X4, X0, X1] : (~ sP1(X0, X1) | g_both(X0, sK8(X0)) | g_false_only(X4, X0) | g_false_only(X0, X4) | g_both(sK8(X0), X0)), inference(cnf_transformation, [], [f30])).
fof(f30, plain, ! [X0, X1] : ((((! [X2] : (~ g_true_only(X2, X0) | ~ g_true_only(X0, X2)) & (~ g_false_only(sK8(X0), X0) & ~ g_false_only(X0, sK8(X0)) & (g_both(sK8(X0), X0) | g_both(X0, sK8(X0))))) | ! [X4] : (g_false_only(X4, X0) | g_false_only(X0, X4))) & g_false_only(X0, X1)) | ~ sP1(X0, X1)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8])], [f28, f29])).
fof(f29, plain, ! [X0] : (? [X3] : (~ g_false_only(X3, X0) & ~ g_false_only(X0, X3) & (g_both(X3, X0) | g_both(X0, X3))) => (~ g_false_only(sK8(X0), X0) & ~ g_false_only(X0, sK8(X0)) & (g_both(sK8(X0), X0) | g_both(X0, sK8(X0))))), introduced(choice_axiom, [])).
fof(f28, plain, ! [X0, X1] : ((((! [X2] : (~ g_true_only(X2, X0) | ~ g_true_only(X0, X2)) & ? [X3] : (~ g_false_only(X3, X0) & ~ g_false_only(X0, X3) & (g_both(X3, X0) | g_both(X0, X3)))) | ! [X4] : (g_false_only(X4, X0) | g_false_only(X0, X4))) & g_false_only(X0, X1)) | ~ sP1(X0, X1)), inference(rectify, [], [f27])).
fof(f27, plain, ! [X5, X4] : ((((! [X6] : (~ g_true_only(X6, X5) | ~ g_true_only(X5, X6)) & ? [X7] : (~ g_false_only(X7, X5) & ~ g_false_only(X5, X7) & (g_both(X7, X5) | g_both(X5, X7)))) | ! [X8] : (g_false_only(X8, X5) | g_false_only(X5, X8))) & g_false_only(X5, X4)) | ~ sP1(X5, X4)), inference(nnf_transformation, [], [f14])).
fof(f1567, plain, (! [X1] : (~ g_both(X1, sK6(sK12)) | g_false_only(X1, sK6(sK12)) | g_false_only(sK6(sK12), X1)) | ~ spl17_79), inference(avatar_component_clause, [], [f1566])).
fof(f8178, plain, (spl17_107 | spl17_232 | spl17_104 | ~ spl17_13 | ~ spl17_22 | spl17_80 | spl17_219 | spl17_233), inference(avatar_split_clause, [], [f8177, f6651, f6028, f1569, f385, f188, f2576, f6647, f2588])).
fof(f2576, plain, (spl17_104 <=> g_both(sK8(sK6(sK12)), sK6(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_104])])).
fof(f8177, plain, (g_both(sK8(sK6(sK12)), sK6(sK12)) | g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_both(sK6(sK12), sK8(sK6(sK12))) | (~ spl17_13 | ~ spl17_22 | spl17_80 | spl17_219 | spl17_233)), inference(subsumption_resolution, [], [f8176, f1472])).
fof(f8176, plain, (g_both(sK8(sK6(sK12)), sK6(sK12)) | g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_both(sK6(sK12), sK8(sK6(sK12))) | ~ sP2(sK12) | (spl17_80 | spl17_219 | spl17_233)), inference(subsumption_resolution, [], [f8175, f6029])).
fof(f8175, plain, (g_both(sK8(sK6(sK12)), sK6(sK12)) | g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_both(sK6(sK12), sK8(sK6(sK12))) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | (spl17_80 | spl17_233)), inference(subsumption_resolution, [], [f7119, f1570])).
fof(f7119, plain, (g_both(sK8(sK6(sK12)), sK6(sK12)) | g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_both(sK6(sK12), sK8(sK6(sK12))) | g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | spl17_233), inference(resolution, [], [f6652, f355])).
fof(f8157, plain, (~ spl17_98 | ~ spl17_105), inference(avatar_contradiction_clause, [], [f8156])).
fof(f8156, plain, ($false | (~ spl17_98 | ~ spl17_105)), inference(subsumption_resolution, [], [f8154, f6044])).
fof(f6044, plain, (g_false_only(sK6(sK12), sK12) | ~ spl17_98), inference(resolution, [], [f2517, f62])).
fof(f62, plain, ! [X0, X1] : (~ sP1(X0, X1) | g_false_only(X0, X1)), inference(cnf_transformation, [], [f30])).
fof(f2517, plain, (sP1(sK6(sK12), sK12) | ~ spl17_98), inference(avatar_component_clause, [], [f2515])).
fof(f8154, plain, (~ g_false_only(sK6(sK12), sK12) | (~ spl17_98 | ~ spl17_105)), inference(resolution, [], [f8060, f84])).
fof(f8060, plain, (g_false_only(sK14(sK6(sK12)), sK6(sK12)) | (~ spl17_98 | ~ spl17_105)), inference(subsumption_resolution, [], [f8033, f6044])).
fof(f8033, plain, (g_false_only(sK14(sK6(sK12)), sK6(sK12)) | ~ g_false_only(sK6(sK12), sK12) | ~ spl17_105), inference(resolution, [], [f2581, f83])).
fof(f2581, plain, (! [X0] : (g_false_only(sK6(sK12), X0) | g_false_only(X0, sK6(sK12))) | ~ spl17_105), inference(avatar_component_clause, [], [f2580])).
fof(f7106, plain, (spl17_249 | spl17_105 | ~ spl17_232), inference(avatar_split_clause, [], [f7104, f6647, f2580, f6989])).
fof(f6989, plain, (spl17_249 <=> ! [X1] : ~ sP1(sK6(sK12), X1)), introduced(avatar_definition, [new_symbols(naming, [spl17_249])])).
fof(f7104, plain, (! [X0, X1] : (g_false_only(X0, sK6(sK12)) | g_false_only(sK6(sK12), X0) | ~ sP1(sK6(sK12), X1)) | ~ spl17_232), inference(resolution, [], [f6649, f64])).
fof(f64, plain, ! [X4, X0, X1] : (~ g_false_only(X0, sK8(X0)) | g_false_only(X4, X0) | g_false_only(X0, X4) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f30])).
fof(f6649, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | ~ spl17_232), inference(avatar_component_clause, [], [f6647])).
fof(f7063, plain, (spl17_232 | spl17_78 | spl17_80 | ~ spl17_104 | spl17_219 | spl17_233), inference(avatar_split_clause, [], [f7062, f6651, f6028, f2576, f1569, f1562, f6647])).
fof(f7062, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | (spl17_78 | spl17_80 | ~ spl17_104 | spl17_219 | spl17_233)), inference(subsumption_resolution, [], [f7061, f6029])).
fof(f7061, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_true_only(sK6(sK12), sK12) | (spl17_78 | spl17_80 | ~ spl17_104 | spl17_233)), inference(subsumption_resolution, [], [f7060, f1570])).
fof(f7060, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | (spl17_78 | ~ spl17_104 | spl17_233)), inference(subsumption_resolution, [], [f7059, f1563])).
fof(f1563, plain, (~ g_true_only(sK13(sK6(sK12)), sK6(sK12)) | spl17_78), inference(avatar_component_clause, [], [f1562])).
fof(f7059, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_true_only(sK13(sK6(sK12)), sK6(sK12)) | g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | (~ spl17_104 | spl17_233)), inference(subsumption_resolution, [], [f6895, f6652])).
fof(f6895, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK8(sK6(sK12)), sK6(sK12)) | g_true_only(sK13(sK6(sK12)), sK6(sK12)) | g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ spl17_104), inference(resolution, [], [f2578, f354])).
fof(f2578, plain, (g_both(sK8(sK6(sK12)), sK6(sK12)) | ~ spl17_104), inference(avatar_component_clause, [], [f2576])).
fof(f6995, plain, (~ spl17_98 | ~ spl17_249), inference(avatar_contradiction_clause, [], [f6994])).
fof(f6994, plain, ($false | (~ spl17_98 | ~ spl17_249)), inference(resolution, [], [f6990, f2517])).
fof(f6990, plain, (! [X1] : ~ sP1(sK6(sK12), X1) | ~ spl17_249), inference(avatar_component_clause, [], [f6989])).
fof(f6991, plain, (spl17_249 | spl17_105 | ~ spl17_233), inference(avatar_split_clause, [], [f6986, f6651, f2580, f6989])).
fof(f6986, plain, (! [X0, X1] : (g_false_only(X0, sK6(sK12)) | g_false_only(sK6(sK12), X0) | ~ sP1(sK6(sK12), X1)) | ~ spl17_233), inference(resolution, [], [f6653, f65])).
fof(f65, plain, ! [X4, X0, X1] : (~ g_false_only(sK8(X0), X0) | g_false_only(X4, X0) | g_false_only(X0, X4) | ~ sP1(X0, X1)), inference(cnf_transformation, [], [f30])).
fof(f6653, plain, (g_false_only(sK8(sK6(sK12)), sK6(sK12)) | ~ spl17_233), inference(avatar_component_clause, [], [f6651])).
fof(f6695, plain, (spl17_233 | spl17_232 | spl17_78 | spl17_80 | ~ spl17_107 | spl17_219), inference(avatar_split_clause, [], [f6694, f6028, f2588, f1569, f1562, f6647, f6651])).
fof(f6694, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK8(sK6(sK12)), sK6(sK12)) | (spl17_78 | spl17_80 | ~ spl17_107 | spl17_219)), inference(subsumption_resolution, [], [f6693, f6029])).
fof(f6693, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK8(sK6(sK12)), sK6(sK12)) | g_true_only(sK6(sK12), sK12) | (spl17_78 | spl17_80 | ~ spl17_107)), inference(subsumption_resolution, [], [f6692, f1570])).
fof(f6692, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK8(sK6(sK12)), sK6(sK12)) | g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | (spl17_78 | ~ spl17_107)), inference(subsumption_resolution, [], [f6639, f1563])).
fof(f6639, plain, (g_false_only(sK6(sK12), sK8(sK6(sK12))) | g_false_only(sK8(sK6(sK12)), sK6(sK12)) | g_true_only(sK13(sK6(sK12)), sK6(sK12)) | g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ spl17_107), inference(resolution, [], [f2590, f350])).
fof(f2590, plain, (g_both(sK6(sK12), sK8(sK6(sK12))) | ~ spl17_107), inference(avatar_component_clause, [], [f2588])).
fof(f6654, plain, (spl17_232 | spl17_233 | ~ spl17_82 | ~ spl17_107), inference(avatar_split_clause, [], [f6626, f2588, f1579, f6651, f6647])).
fof(f1579, plain, (spl17_82 <=> ! [X2] : (g_false_only(X2, sK6(sK12)) | ~ g_both(sK6(sK12), X2) | g_false_only(sK6(sK12), X2))), introduced(avatar_definition, [new_symbols(naming, [spl17_82])])).
fof(f6626, plain, (g_false_only(sK8(sK6(sK12)), sK6(sK12)) | g_false_only(sK6(sK12), sK8(sK6(sK12))) | (~ spl17_82 | ~ spl17_107)), inference(resolution, [], [f2590, f1580])).
fof(f1580, plain, (! [X2] : (~ g_both(sK6(sK12), X2) | g_false_only(X2, sK6(sK12)) | g_false_only(sK6(sK12), X2)) | ~ spl17_82), inference(avatar_component_clause, [], [f1579])).
fof(f6084, plain, (~ spl17_219 | ~ spl17_98), inference(avatar_split_clause, [], [f6083, f2515, f6028])).
fof(f6083, plain, (~ g_true_only(sK6(sK12), sK12) | ~ spl17_98), inference(resolution, [], [f6069, f94])).
fof(f6069, plain, (g_false(sK6(sK12), sK12) | ~ spl17_98), inference(resolution, [], [f6044, f99])).
fof(f99, plain, ! [X0, X1] : (~ g_false_only(X0, X1) | g_false(X0, X1)), inference(cnf_transformation, [], [f49])).
fof(f6005, plain, (~ spl17_98 | ~ spl17_110), inference(avatar_contradiction_clause, [], [f6004])).
fof(f6004, plain, ($false | (~ spl17_98 | ~ spl17_110)), inference(subsumption_resolution, [], [f6003, f4552])).
fof(f4552, plain, (g_false_only(sK6(sK12), sK12) | ~ spl17_98), inference(resolution, [], [f2517, f62])).
fof(f6003, plain, (~ g_false_only(sK6(sK12), sK12) | ~ spl17_110), inference(resolution, [], [f2649, f100])).
fof(f2649, plain, (g_true(sK6(sK12), sK12) | ~ spl17_110), inference(avatar_component_clause, [], [f2648])).
fof(f2648, plain, (spl17_110 <=> g_true(sK6(sK12), sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_110])])).
fof(f5968, plain, (spl17_110 | ~ spl17_80), inference(avatar_split_clause, [], [f5950, f1569, f2648])).
fof(f5950, plain, (g_true(sK6(sK12), sK12) | ~ spl17_80), inference(resolution, [], [f1571, f96])).
fof(f96, plain, ! [X0, X1] : (~ g_both(X0, X1) | g_true(X0, X1)), inference(cnf_transformation, [], [f47])).
fof(f5771, plain, (spl17_83 | spl17_82 | spl17_110), inference(avatar_split_clause, [], [f4600, f2648, f1579, f1584])).
fof(f4600, plain, (! [X0] : (g_false_only(X0, sK6(sK12)) | g_false_only(sK6(sK12), X0) | ~ g_both(sK6(sK12), X0) | g_true_only(sK6(sK12), sK13(sK6(sK12)))) | spl17_110), inference(resolution, [], [f2662, f85])).
fof(f2662, plain, (g_false_only(sK6(sK12), sK12) | spl17_110), inference(resolution, [], [f2650, f156])).
fof(f156, plain, ! [X0, X1] : (g_true(X0, X1) | g_false_only(X0, X1)), inference(subsumption_resolution, [], [f153, f93])).
fof(f153, plain, ! [X0, X1] : (g_true_only(X0, X1) | g_true(X0, X1) | g_false_only(X0, X1)), inference(resolution, [], [f152, f101])).
fof(f101, plain, ! [X0, X1] : (~ g_false(X0, X1) | g_true(X0, X1) | g_false_only(X0, X1)), inference(cnf_transformation, [], [f49])).
fof(f152, plain, ! [X0, X1] : (g_false(X0, X1) | g_true_only(X0, X1)), inference(subsumption_resolution, [], [f147, f97])).
fof(f147, plain, ! [X0, X1] : (g_both(X0, X1) | g_true_only(X0, X1) | g_false(X0, X1)), inference(resolution, [], [f102, f99])).
fof(f2650, plain, (~ g_true(sK6(sK12), sK12) | spl17_110), inference(avatar_component_clause, [], [f2648])).
fof(f4614, plain, (spl17_79 | spl17_83 | spl17_110), inference(avatar_split_clause, [], [f4613, f2648, f1584, f1566])).
fof(f4613, plain, (! [X1] : (g_false_only(X1, sK6(sK12)) | g_false_only(sK6(sK12), X1) | ~ g_both(X1, sK6(sK12))) | (spl17_83 | spl17_110)), inference(subsumption_resolution, [], [f4601, f1585])).
fof(f1585, plain, (~ g_true_only(sK6(sK12), sK13(sK6(sK12))) | spl17_83), inference(avatar_component_clause, [], [f1584])).
fof(f4601, plain, (! [X1] : (g_false_only(X1, sK6(sK12)) | g_false_only(sK6(sK12), X1) | ~ g_both(X1, sK6(sK12)) | g_true_only(sK6(sK12), sK13(sK6(sK12)))) | spl17_110), inference(resolution, [], [f2662, f86])).
fof(f4539, plain, (~ spl17_13 | ~ spl17_22 | spl17_80 | ~ spl17_81 | spl17_98), inference(avatar_contradiction_clause, [], [f4538])).
fof(f4538, plain, ($false | (~ spl17_13 | ~ spl17_22 | spl17_80 | ~ spl17_81 | spl17_98)), inference(subsumption_resolution, [], [f4531, f1575])).
fof(f1575, plain, (sP0(sK6(sK12)) | ~ spl17_81), inference(avatar_component_clause, [], [f1573])).
fof(f4531, plain, (~ sP0(sK6(sK12)) | (~ spl17_13 | ~ spl17_22 | spl17_80 | spl17_98)), inference(resolution, [], [f4475, f3254])).
fof(f3254, plain, ! [X2] : (~ g_true_only(X2, sK12) | ~ sP0(X2)), inference(subsumption_resolution, [], [f3251, f1549])).
fof(f1549, plain, ! [X1] : (g_true_only(X1, sK9(X1)) | ~ g_true_only(X1, sK12) | ~ sP0(X1)), inference(subsumption_resolution, [], [f1547, f274])).
fof(f274, plain, ! [X3] : (g_true_only(X3, sK16(X3)) | ~ g_true_only(X3, sK12) | g_true_only(X3, sK9(X3)) | ~ sP0(X3)), inference(subsumption_resolution, [], [f273, f68])).
fof(f68, plain, ! [X0] : (~ g_false_only(X0, sK10(X0)) | g_true_only(X0, sK9(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ! [X0] : ((g_true_only(sK9(X0), X0) & g_true_only(X0, sK9(X0))) | (! [X2] : (~ g_true_only(X2, X0) | ~ g_true_only(X0, X2)) & (~ g_false_only(sK10(X0), X0) & ~ g_false_only(X0, sK10(X0)) & (g_both(sK10(X0), X0) | g_both(X0, sK10(X0))))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK9, sK10])], [f32, f34, f33])).
fof(f33, plain, ! [X0] : (? [X1] : (g_true_only(X1, X0) & g_true_only(X0, X1)) => (g_true_only(sK9(X0), X0) & g_true_only(X0, sK9(X0)))), introduced(choice_axiom, [])).
fof(f34, plain, ! [X0] : (? [X3] : (~ g_false_only(X3, X0) & ~ g_false_only(X0, X3) & (g_both(X3, X0) | g_both(X0, X3))) => (~ g_false_only(sK10(X0), X0) & ~ g_false_only(X0, sK10(X0)) & (g_both(sK10(X0), X0) | g_both(X0, sK10(X0))))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X0] : (? [X1] : (g_true_only(X1, X0) & g_true_only(X0, X1)) | (! [X2] : (~ g_true_only(X2, X0) | ~ g_true_only(X0, X2)) & ? [X3] : (~ g_false_only(X3, X0) & ~ g_false_only(X0, X3) & (g_both(X3, X0) | g_both(X0, X3)))) | ~ sP0(X0)), inference(rectify, [], [f31])).
fof(f31, plain, ! [X5] : (? [X11] : (g_true_only(X11, X5) & g_true_only(X5, X11)) | (! [X12] : (~ g_true_only(X12, X5) | ~ g_true_only(X5, X12)) & ? [X13] : (~ g_false_only(X13, X5) & ~ g_false_only(X5, X13) & (g_both(X13, X5) | g_both(X5, X13)))) | ~ sP0(X5)), inference(nnf_transformation, [], [f13])).
fof(f273, plain, ! [X3] : (g_false_only(X3, sK10(X3)) | g_true_only(X3, sK16(X3)) | ~ g_true_only(X3, sK12) | g_true_only(X3, sK9(X3)) | ~ sP0(X3)), inference(subsumption_resolution, [], [f272, f69])).
fof(f69, plain, ! [X0] : (~ g_false_only(sK10(X0), X0) | g_true_only(X0, sK9(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f35])).
fof(f272, plain, ! [X3] : (g_false_only(sK10(X3), X3) | g_false_only(X3, sK10(X3)) | g_true_only(X3, sK16(X3)) | ~ g_true_only(X3, sK12) | g_true_only(X3, sK9(X3)) | ~ sP0(X3)), inference(subsumption_resolution, [], [f262, f75])).
fof(f75, plain, ! [X5, X13] : (~ g_both(X5, X13) | g_false_only(X13, X5) | g_false_only(X5, X13) | g_true_only(X5, sK16(X5)) | ~ g_true_only(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f262, plain, ! [X3] : (g_false_only(sK10(X3), X3) | g_false_only(X3, sK10(X3)) | g_true_only(X3, sK16(X3)) | ~ g_true_only(X3, sK12) | g_true_only(X3, sK9(X3)) | g_both(X3, sK10(X3)) | ~ sP0(X3)), inference(resolution, [], [f76, f67])).
fof(f67, plain, ! [X0] : (g_both(sK10(X0), X0) | g_true_only(X0, sK9(X0)) | g_both(X0, sK10(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f35])).
fof(f76, plain, ! [X5, X13] : (~ g_both(X13, X5) | g_false_only(X13, X5) | g_false_only(X5, X13) | g_true_only(X5, sK16(X5)) | ~ g_true_only(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f1547, plain, ! [X1] : (~ g_true_only(X1, sK12) | g_true_only(X1, sK9(X1)) | ~ sP0(X1) | ~ g_true_only(X1, sK16(X1))), inference(duplicate_literal_removal, [], [f1542])).
fof(f1542, plain, ! [X1] : (~ g_true_only(X1, sK12) | g_true_only(X1, sK9(X1)) | ~ sP0(X1) | g_true_only(X1, sK9(X1)) | ~ g_true_only(X1, sK16(X1)) | ~ sP0(X1)), inference(resolution, [], [f340, f70])).
fof(f70, plain, ! [X2, X0] : (~ g_true_only(X2, X0) | g_true_only(X0, sK9(X0)) | ~ g_true_only(X0, X2) | ~ sP0(X0)), inference(cnf_transformation, [], [f35])).
fof(f340, plain, ! [X3] : (g_true_only(sK16(X3), X3) | ~ g_true_only(X3, sK12) | g_true_only(X3, sK9(X3)) | ~ sP0(X3)), inference(subsumption_resolution, [], [f339, f68])).
fof(f339, plain, ! [X3] : (g_false_only(X3, sK10(X3)) | g_true_only(sK16(X3), X3) | ~ g_true_only(X3, sK12) | g_true_only(X3, sK9(X3)) | ~ sP0(X3)), inference(subsumption_resolution, [], [f338, f69])).
fof(f338, plain, ! [X3] : (g_false_only(sK10(X3), X3) | g_false_only(X3, sK10(X3)) | g_true_only(sK16(X3), X3) | ~ g_true_only(X3, sK12) | g_true_only(X3, sK9(X3)) | ~ sP0(X3)), inference(subsumption_resolution, [], [f326, f77])).
fof(f77, plain, ! [X5, X13] : (~ g_both(X5, X13) | g_false_only(X13, X5) | g_false_only(X5, X13) | g_true_only(sK16(X5), X5) | ~ g_true_only(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f326, plain, ! [X3] : (g_false_only(sK10(X3), X3) | g_false_only(X3, sK10(X3)) | g_true_only(sK16(X3), X3) | ~ g_true_only(X3, sK12) | g_true_only(X3, sK9(X3)) | g_both(X3, sK10(X3)) | ~ sP0(X3)), inference(resolution, [], [f78, f67])).
fof(f78, plain, ! [X5, X13] : (~ g_both(X13, X5) | g_false_only(X13, X5) | g_false_only(X5, X13) | g_true_only(sK16(X5), X5) | ~ g_true_only(X5, sK12)), inference(cnf_transformation, [], [f43])).
fof(f3251, plain, ! [X2] : (~ g_true_only(X2, sK12) | ~ sP0(X2) | ~ g_true_only(X2, sK9(X2))), inference(duplicate_literal_removal, [], [f3243])).
fof(f3243, plain, ! [X2] : (~ g_true_only(X2, sK12) | ~ sP0(X2) | ~ g_true_only(X2, sK9(X2)) | ~ g_true_only(X2, sK12)), inference(resolution, [], [f1528, f79])).
fof(f1528, plain, ! [X0] : (g_true_only(sK9(X0), X0) | ~ g_true_only(X0, sK12) | ~ sP0(X0)), inference(subsumption_resolution, [], [f1527, f271])).
fof(f271, plain, ! [X2] : (g_true_only(sK9(X2), X2) | ~ g_true_only(X2, sK12) | g_true_only(X2, sK16(X2)) | ~ sP0(X2)), inference(subsumption_resolution, [], [f270, f72])).
fof(f72, plain, ! [X0] : (~ g_false_only(X0, sK10(X0)) | g_true_only(sK9(X0), X0) | ~ sP0(X0)), inference(cnf_transformation, [], [f35])).
fof(f270, plain, ! [X2] : (g_false_only(X2, sK10(X2)) | g_true_only(X2, sK16(X2)) | ~ g_true_only(X2, sK12) | g_true_only(sK9(X2), X2) | ~ sP0(X2)), inference(subsumption_resolution, [], [f269, f73])).
fof(f73, plain, ! [X0] : (~ g_false_only(sK10(X0), X0) | g_true_only(sK9(X0), X0) | ~ sP0(X0)), inference(cnf_transformation, [], [f35])).
fof(f269, plain, ! [X2] : (g_false_only(sK10(X2), X2) | g_false_only(X2, sK10(X2)) | g_true_only(X2, sK16(X2)) | ~ g_true_only(X2, sK12) | g_true_only(sK9(X2), X2) | ~ sP0(X2)), inference(subsumption_resolution, [], [f261, f75])).
fof(f261, plain, ! [X2] : (g_false_only(sK10(X2), X2) | g_false_only(X2, sK10(X2)) | g_true_only(X2, sK16(X2)) | ~ g_true_only(X2, sK12) | g_true_only(sK9(X2), X2) | g_both(X2, sK10(X2)) | ~ sP0(X2)), inference(resolution, [], [f76, f71])).
fof(f71, plain, ! [X0] : (g_both(sK10(X0), X0) | g_true_only(sK9(X0), X0) | g_both(X0, sK10(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f35])).
fof(f1527, plain, ! [X0] : (~ g_true_only(X0, sK12) | g_true_only(sK9(X0), X0) | ~ sP0(X0) | ~ g_true_only(X0, sK16(X0))), inference(duplicate_literal_removal, [], [f1520])).
fof(f1520, plain, ! [X0] : (~ g_true_only(X0, sK12) | g_true_only(sK9(X0), X0) | ~ sP0(X0) | g_true_only(sK9(X0), X0) | ~ g_true_only(X0, sK16(X0)) | ~ sP0(X0)), inference(resolution, [], [f337, f74])).
fof(f74, plain, ! [X2, X0] : (~ g_true_only(X2, X0) | g_true_only(sK9(X0), X0) | ~ g_true_only(X0, X2) | ~ sP0(X0)), inference(cnf_transformation, [], [f35])).
fof(f337, plain, ! [X2] : (g_true_only(sK16(X2), X2) | ~ g_true_only(X2, sK12) | g_true_only(sK9(X2), X2) | ~ sP0(X2)), inference(subsumption_resolution, [], [f336, f72])).
fof(f336, plain, ! [X2] : (g_false_only(X2, sK10(X2)) | g_true_only(sK16(X2), X2) | ~ g_true_only(X2, sK12) | g_true_only(sK9(X2), X2) | ~ sP0(X2)), inference(subsumption_resolution, [], [f335, f73])).
fof(f335, plain, ! [X2] : (g_false_only(sK10(X2), X2) | g_false_only(X2, sK10(X2)) | g_true_only(sK16(X2), X2) | ~ g_true_only(X2, sK12) | g_true_only(sK9(X2), X2) | ~ sP0(X2)), inference(subsumption_resolution, [], [f325, f77])).
fof(f325, plain, ! [X2] : (g_false_only(sK10(X2), X2) | g_false_only(X2, sK10(X2)) | g_true_only(sK16(X2), X2) | ~ g_true_only(X2, sK12) | g_true_only(sK9(X2), X2) | g_both(X2, sK10(X2)) | ~ sP0(X2)), inference(resolution, [], [f78, f71])).
fof(f4475, plain, (g_true_only(sK6(sK12), sK12) | (~ spl17_13 | ~ spl17_22 | spl17_80 | spl17_98)), inference(subsumption_resolution, [], [f4474, f1472])).
fof(f4474, plain, (g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | (spl17_80 | spl17_98)), inference(subsumption_resolution, [], [f4472, f1570])).
fof(f4472, plain, (g_both(sK6(sK12), sK12) | g_true_only(sK6(sK12), sK12) | ~ sP2(sK12) | spl17_98), inference(resolution, [], [f2516, f56])).
fof(f3705, plain, (~ spl17_80 | ~ spl17_105), inference(avatar_split_clause, [], [f3704, f2580, f1569])).
fof(f3704, plain, (~ g_both(sK6(sK12), sK12) | ~ spl17_105), inference(subsumption_resolution, [], [f3681, f80])).
fof(f3681, plain, (g_false_only(sK6(sK12), sK15(sK6(sK12))) | ~ g_both(sK6(sK12), sK12) | ~ spl17_105), inference(resolution, [], [f2581, f81])).
fof(f2601, plain, (spl17_105 | spl17_109 | ~ spl17_98), inference(avatar_split_clause, [], [f2569, f2515, f2599, f2580])).
fof(f2569, plain, (! [X4, X5] : (~ g_true_only(sK6(sK12), X4) | g_false_only(X5, sK6(sK12)) | g_false_only(sK6(sK12), X5) | ~ g_true_only(X4, sK6(sK12))) | ~ spl17_98), inference(resolution, [], [f2517, f66])).
fof(f66, plain, ! [X4, X2, X0, X1] : (~ sP1(X0, X1) | ~ g_true_only(X0, X2) | g_false_only(X4, X0) | g_false_only(X0, X4) | ~ g_true_only(X2, X0)), inference(cnf_transformation, [], [f30])).
fof(f1148, plain, (spl17_22 | spl17_42 | ~ spl17_11 | ~ spl17_13 | spl17_19), inference(avatar_split_clause, [], [f1147, f229, f188, f144, f696, f385])).
fof(f696, plain, (spl17_42 <=> g_true_only(sK12, sK11(sK12))), introduced(avatar_definition, [new_symbols(naming, [spl17_42])])).
fof(f144, plain, (spl17_11 <=> ! [X1, X3, X0] : (sP2(X0) | g_true_only(X1, sK11(X1)) | ~ g_both(X1, X0) | ~ g_both(X1, X3) | g_false_only(X1, X3) | g_false_only(X3, X1))), introduced(avatar_definition, [new_symbols(naming, [spl17_11])])).
fof(f1147, plain, (! [X8] : (g_true_only(sK12, sK11(sK12)) | ~ g_both(sK12, X8) | sP2(X8)) | (~ spl17_11 | ~ spl17_13 | spl17_19)), inference(subsumption_resolution, [], [f715, f231])).
fof(f231, plain, (~ g_false_only(sK12, sK12) | spl17_19), inference(avatar_component_clause, [], [f229])).
fof(f715, plain, (! [X8] : (g_true_only(sK12, sK11(sK12)) | ~ g_both(sK12, X8) | sP2(X8) | g_false_only(sK12, sK12)) | (~ spl17_11 | ~ spl17_13)), inference(duplicate_literal_removal, [], [f712])).
fof(f712, plain, (! [X8] : (g_true_only(sK12, sK11(sK12)) | ~ g_both(sK12, X8) | sP2(X8) | g_false_only(sK12, sK12) | g_false_only(sK12, sK12)) | (~ spl17_11 | ~ spl17_13)), inference(resolution, [], [f145, f189])).
fof(f145, plain, (! [X0, X3, X1] : (~ g_both(X1, X3) | g_true_only(X1, sK11(X1)) | ~ g_both(X1, X0) | sP2(X0) | g_false_only(X1, X3) | g_false_only(X3, X1)) | ~ spl17_11), inference(avatar_component_clause, [], [f144])).
fof(f1146, plain, (~ spl17_23 | ~ spl17_42), inference(avatar_contradiction_clause, [], [f1145])).
fof(f1145, plain, ($false | (~ spl17_23 | ~ spl17_42)), inference(subsumption_resolution, [], [f1126, f390])).
fof(f390, plain, (g_true_only(sK11(sK12), sK12) | ~ spl17_23), inference(avatar_component_clause, [], [f388])).
fof(f388, plain, (spl17_23 <=> g_true_only(sK11(sK12), sK12)), introduced(avatar_definition, [new_symbols(naming, [spl17_23])])).
fof(f1126, plain, (~ g_true_only(sK11(sK12), sK12) | ~ spl17_42), inference(duplicate_literal_removal, [], [f1124])).
fof(f1124, plain, (~ g_true_only(sK11(sK12), sK12) | ~ g_true_only(sK11(sK12), sK12) | ~ spl17_42), inference(resolution, [], [f698, f79])).
fof(f698, plain, (g_true_only(sK12, sK11(sK12)) | ~ spl17_42), inference(avatar_component_clause, [], [f696])).
fof(f962, plain, (~ spl17_3 | ~ spl17_5 | spl17_47), inference(avatar_contradiction_clause, [], [f961])).
fof(f961, plain, ($false | (~ spl17_3 | ~ spl17_5 | spl17_47)), inference(subsumption_resolution, [], [f958, f860])).
fof(f860, plain, (~ g_true_only(sK12, sK4) | spl17_47), inference(avatar_component_clause, [], [f859])).
fof(f859, plain, (spl17_47 <=> g_true_only(sK12, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl17_47])])).
fof(f958, plain, (g_true_only(sK12, sK4) | (~ spl17_3 | ~ spl17_5)), inference(duplicate_literal_removal, [], [f957])).
fof(f957, plain, (g_true_only(sK12, sK4) | g_true_only(sK12, sK4) | (~ spl17_3 | ~ spl17_5)), inference(resolution, [], [f468, f113])).
fof(f113, plain, (! [X1] : (g_true_only(sK5(X1), X1) | g_true_only(X1, sK4)) | ~ spl17_3), inference(avatar_component_clause, [], [f112])).
fof(f112, plain, (spl17_3 <=> ! [X1] : (g_true_only(sK5(X1), X1) | g_true_only(X1, sK4))), introduced(avatar_definition, [new_symbols(naming, [spl17_3])])).
fof(f468, plain, (! [X2] : (~ g_true_only(sK5(X2), sK12) | g_true_only(X2, sK4)) | (~ spl17_3 | ~ spl17_5)), inference(subsumption_resolution, [], [f462, f113])).
fof(f462, plain, (! [X2] : (~ g_true_only(sK5(X2), sK12) | ~ g_true_only(sK5(X2), X2) | g_true_only(X2, sK4)) | ~ spl17_5), inference(resolution, [], [f121, f79])).
fof(f121, plain, (! [X1] : (g_true_only(X1, sK5(X1)) | g_true_only(X1, sK4)) | ~ spl17_5), inference(avatar_component_clause, [], [f120])).
fof(f120, plain, (spl17_5 <=> ! [X1] : (g_true_only(X1, sK5(X1)) | g_true_only(X1, sK4))), introduced(avatar_definition, [new_symbols(naming, [spl17_5])])).
fof(f939, plain, (~ spl17_47 | ~ spl17_35), inference(avatar_split_clause, [], [f929, f518, f859])).
fof(f518, plain, (spl17_35 <=> g_false_only(sK12, sK4)), introduced(avatar_definition, [new_symbols(naming, [spl17_35])])).
fof(f929, plain, (~ g_true_only(sK12, sK4) | ~ spl17_35), inference(resolution, [], [f907, f94])).
fof(f907, plain, (g_false(sK12, sK4) | ~ spl17_35), inference(resolution, [], [f520, f99])).
fof(f520, plain, (g_false_only(sK12, sK4) | ~ spl17_35), inference(avatar_component_clause, [], [f518])).
fof(f906, plain, (spl17_35 | ~ spl17_6 | ~ spl17_13), inference(avatar_split_clause, [], [f903, f188, f124, f518])).
fof(f124, plain, (spl17_6 <=> ! [X1, X3] : (g_false_only(X1, sK4) | g_false_only(X1, X3) | g_false_only(X3, X1))), introduced(avatar_definition, [new_symbols(naming, [spl17_6])])).
fof(f903, plain, (g_false_only(sK12, sK4) | (~ spl17_6 | ~ spl17_13)), inference(resolution, [], [f516, f189])).
fof(f516, plain, (! [X16] : (~ g_both(X16, sK12) | g_false_only(X16, sK4)) | ~ spl17_6), inference(subsumption_resolution, [], [f485, f81])).
fof(f485, plain, (! [X16] : (g_false_only(X16, sK4) | g_false_only(sK15(X16), X16) | ~ g_both(X16, sK12)) | ~ spl17_6), inference(resolution, [], [f125, f80])).
fof(f125, plain, (! [X3, X1] : (g_false_only(X1, sK4) | g_false_only(X1, X3) | g_false_only(X3, X1)) | ~ spl17_6), inference(avatar_component_clause, [], [f124])).
fof(f676, plain, (spl17_22 | spl17_23 | ~ spl17_9 | ~ spl17_13 | spl17_19), inference(avatar_split_clause, [], [f675, f229, f188, f136, f388, f385])).
fof(f136, plain, (spl17_9 <=> ! [X1, X3, X0] : (sP2(X0) | g_true_only(sK11(X1), X1) | ~ g_both(X1, X0) | ~ g_both(X1, X3) | g_false_only(X1, X3) | g_false_only(X3, X1))), introduced(avatar_definition, [new_symbols(naming, [spl17_9])])).
fof(f675, plain, (! [X8] : (g_true_only(sK11(sK12), sK12) | ~ g_both(sK12, X8) | sP2(X8)) | (~ spl17_9 | ~ spl17_13 | spl17_19)), inference(subsumption_resolution, [], [f670, f231])).
fof(f670, plain, (! [X8] : (g_true_only(sK11(sK12), sK12) | ~ g_both(sK12, X8) | sP2(X8) | g_false_only(sK12, sK12)) | (~ spl17_9 | ~ spl17_13)), inference(duplicate_literal_removal, [], [f667])).
fof(f667, plain, (! [X8] : (g_true_only(sK11(sK12), sK12) | ~ g_both(sK12, X8) | sP2(X8) | g_false_only(sK12, sK12) | g_false_only(sK12, sK12)) | (~ spl17_9 | ~ spl17_13)), inference(resolution, [], [f137, f189])).
fof(f137, plain, (! [X0, X3, X1] : (~ g_both(X1, X3) | g_true_only(sK11(X1), X1) | ~ g_both(X1, X0) | sP2(X0) | g_false_only(X1, X3) | g_false_only(X3, X1)) | ~ spl17_9), inference(avatar_component_clause, [], [f136])).
fof(f397, plain, (~ spl17_19 | ~ spl17_13), inference(avatar_split_clause, [], [f347, f188, f229])).
fof(f347, plain, (~ g_false_only(sK12, sK12) | ~ spl17_13), inference(resolution, [], [f283, f100])).
fof(f283, plain, (g_true(sK12, sK12) | ~ spl17_13), inference(resolution, [], [f189, f96])).
fof(f258, plain, (spl17_13 | spl17_19), inference(avatar_split_clause, [], [f257, f229, f188])).
fof(f257, plain, (g_both(sK12, sK12) | spl17_19), inference(subsumption_resolution, [], [f253, f79])).
fof(f253, plain, (g_both(sK12, sK12) | g_true_only(sK12, sK12) | spl17_19), inference(resolution, [], [f231, f102])).
fof(f232, plain, (spl17_18 | ~ spl17_19), inference(avatar_split_clause, [], [f224, f229, f226])).
fof(f224, plain, ! [X2] : (~ g_false_only(sK12, sK12) | ~ g_true_only(sK14(sK12), X2) | ~ g_true_only(X2, sK14(sK12))), inference(subsumption_resolution, [], [f222, f79])).
fof(f222, plain, ! [X2] : (g_true_only(sK14(sK12), sK12) | ~ g_false_only(sK12, sK12) | ~ g_true_only(sK14(sK12), X2) | ~ g_true_only(X2, sK14(sK12))), inference(resolution, [], [f151, f82])).
fof(f146, plain, (spl17_11 | spl17_1), inference(avatar_split_clause, [], [f89, f104, f144])).
fof(f104, plain, (spl17_1 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl17_1])])).
fof(f89, plain, ! [X0, X3, X1] : (sP3 | sP2(X0) | g_true_only(X1, sK11(X1)) | g_false_only(X3, X1) | g_false_only(X1, X3) | ~ g_both(X1, X3) | ~ g_both(X1, X0)), inference(cnf_transformation, [], [f43])).
fof(f138, plain, (spl17_9 | spl17_1), inference(avatar_split_clause, [], [f91, f104, f136])).
fof(f91, plain, ! [X0, X3, X1] : (sP3 | sP2(X0) | g_true_only(sK11(X1), X1) | g_false_only(X3, X1) | g_false_only(X1, X3) | ~ g_both(X1, X3) | ~ g_both(X1, X0)), inference(cnf_transformation, [], [f43])).
fof(f126, plain, (~ spl17_1 | spl17_6), inference(avatar_split_clause, [], [f51, f124, f104])).
fof(f51, plain, ! [X3, X1] : (g_false_only(X1, sK4) | g_false_only(X3, X1) | g_false_only(X1, X3) | ~ sP3), inference(cnf_transformation, [], [f21])).
fof(f21, plain, (! [X1] : (((g_true_only(sK5(X1), X1) & g_true_only(X1, sK5(X1))) & g_false_only(X1, sK4)) | (! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3)) & g_true_only(X1, sK4))) | ~ sP3), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5])], [f18, f20, f19])).
fof(f19, plain, (? [X0] : ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, X0)) | (! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3)) & g_true_only(X1, X0))) => ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, sK4)) | (! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3)) & g_true_only(X1, sK4)))), introduced(choice_axiom, [])).
fof(f20, plain, ! [X1] : (? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) => (g_true_only(sK5(X1), X1) & g_true_only(X1, sK5(X1)))), introduced(choice_axiom, [])).
fof(f18, plain, (? [X0] : ! [X1] : ((? [X2] : (g_true_only(X2, X1) & g_true_only(X1, X2)) & g_false_only(X1, X0)) | (! [X3] : (g_false_only(X3, X1) | g_false_only(X1, X3)) & g_true_only(X1, X0))) | ~ sP3), inference(nnf_transformation, [], [f16])).
fof(f122, plain, (~ spl17_1 | spl17_5), inference(avatar_split_clause, [], [f52, f120, f104])).
fof(f52, plain, ! [X1] : (g_true_only(X1, sK5(X1)) | g_true_only(X1, sK4) | ~ sP3), inference(cnf_transformation, [], [f21])).
fof(f114, plain, (~ spl17_1 | spl17_3), inference(avatar_split_clause, [], [f54, f112, f104])).
fof(f54, plain, ! [X1] : (g_true_only(sK5(X1), X1) | g_true_only(X1, sK4) | ~ sP3), inference(cnf_transformation, [], [f21])).