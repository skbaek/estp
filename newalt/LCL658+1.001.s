fof(f51865, plain, $false, inference(avatar_sat_refutation, [], [f264, f403, f464, f783, f880, f1078, f1136, f1795, f1800, f1808, f2321, f2339, f2689, f2800, f2802, f3506, f8577, f8632, f10189, f22956, f22957, f22962, f22994, f23523, f48078, f48159, f48188, f48190, f48730, f49248, f49334, f51368, f51861])).
fof(f51861, plain, ~ spl21_94, inference(avatar_contradiction_clause, [], [f51860])).
fof(f51860, plain, ($false | ~ spl21_94), inference(subsumption_resolution, [], [f51834, f75])).
fof(f75, plain, r1(sK14, sK17), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ((! [X2] : ((p1(sK15(X2)) & (~ p1(sK16(X2)) & r1(sK15(X2), sK16(X2))) & r1(X2, sK15(X2))) | p1(X2) | ~ r1(sK14, X2)) & (! [X6] : (p1(X6) | ~ r1(sK17, X6)) & r1(sK14, sK17)) & (~ p1(sK18) & r1(sK14, sK18)) & r1(sK13, sK14)) & ! [X8] : ((((! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(sK19(X8), X10)) & ~ p1(sK19(X8)) & r1(X8, sK19(X8))) | sP1(X8)) & ! [X12] : ((~ p1(sK20(X12)) & r1(X12, sK20(X12))) | ! [X14] : (! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X12, X14)) | ~ r1(X8, X12)) & sP2(X8) & sP3(X8)) | ~ r1(sK13, X8))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14, sK15, sK16, sK17, sK18, sK19, sK20])], [f33, f41, f40, f39, f38, f37, f36, f35, f34])).
fof(f34, plain, (? [X0] : (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(X0, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | sP1(X8)) & ! [X12] : (? [X13] : (~ p1(X13) & r1(X12, X13)) | ! [X14] : (! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X12, X14)) | ~ r1(X8, X12)) & sP2(X8) & sP3(X8)) | ~ r1(X0, X8))) => (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(sK13, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | sP1(X8)) & ! [X12] : (? [X13] : (~ p1(X13) & r1(X12, X13)) | ! [X14] : (! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X12, X14)) | ~ r1(X8, X12)) & sP2(X8) & sP3(X8)) | ~ r1(sK13, X8)))), introduced(choice_axiom, [])).
fof(f35, plain, (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(sK13, X1)) => (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(sK14, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(sK14, X5)) & ? [X7] : (~ p1(X7) & r1(sK14, X7)) & r1(sK13, sK14))), introduced(choice_axiom, [])).
fof(f36, plain, ! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) => (p1(sK15(X2)) & ? [X4] : (~ p1(X4) & r1(sK15(X2), X4)) & r1(X2, sK15(X2)))), introduced(choice_axiom, [])).
fof(f37, plain, ! [X2] : (? [X4] : (~ p1(X4) & r1(sK15(X2), X4)) => (~ p1(sK16(X2)) & r1(sK15(X2), sK16(X2)))), introduced(choice_axiom, [])).
fof(f38, plain, (? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(sK14, X5)) => (! [X6] : (p1(X6) | ~ r1(sK17, X6)) & r1(sK14, sK17))), introduced(choice_axiom, [])).
fof(f39, plain, (? [X7] : (~ p1(X7) & r1(sK14, X7)) => (~ p1(sK18) & r1(sK14, sK18))), introduced(choice_axiom, [])).
fof(f40, plain, ! [X8] : (? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) => (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(sK19(X8), X10)) & ~ p1(sK19(X8)) & r1(X8, sK19(X8)))), introduced(choice_axiom, [])).
fof(f41, plain, ! [X12] : (? [X13] : (~ p1(X13) & r1(X12, X13)) => (~ p1(sK20(X12)) & r1(X12, sK20(X12)))), introduced(choice_axiom, [])).
fof(f33, plain, ? [X0] : (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(X0, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | sP1(X8)) & ! [X12] : (? [X13] : (~ p1(X13) & r1(X12, X13)) | ! [X14] : (! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X12, X14)) | ~ r1(X8, X12)) & sP2(X8) & sP3(X8)) | ~ r1(X0, X8))), inference(rectify, [], [f11])).
fof(f11, plain, ? [X0] : (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(X0, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | sP1(X8)) & ! [X16] : (? [X17] : (~ p1(X17) & r1(X16, X17)) | ! [X18] : (! [X19] : (p1(X19) | ~ r1(X18, X19)) | ~ r1(X16, X18)) | ~ r1(X8, X16)) & sP2(X8) & sP3(X8)) | ~ r1(X0, X8))), inference(definition_folding, [], [f6, e10, e9, e8, e7])).
fof(f7, plain, ! [X8] : (? [X25] : (! [X26] : ((p1(X26) & ? [X27] : (~ p1(X27) & r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) & p1(X25) & ? [X30] : (~ p1(X30) & r1(X25, X30)) & r1(X8, X25)) | ~ sP0(X8)), inference(usedef, [], [e7])).
fof(e7, plain, ! [X8] : (sP0(X8) <=> ? [X25] : (! [X26] : ((p1(X26) & ? [X27] : (~ p1(X27) & r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) & p1(X25) & ? [X30] : (~ p1(X30) & r1(X25, X30)) & r1(X8, X25))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f8, plain, ! [X8] : (! [X12] : (! [X13] : (? [X14] : (p1(X14) & ? [X15] : (~ p1(X15) & r1(X14, X15)) & r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12)) | ~ sP1(X8)), inference(usedef, [], [e8])).
fof(e8, plain, ! [X8] : (sP1(X8) <=> ! [X12] : (! [X13] : (? [X14] : (p1(X14) & ? [X15] : (~ p1(X15) & r1(X14, X15)) & r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f9, plain, ! [X8] : (? [X20] : (! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) & ~ p1(X20) & r1(X8, X20)) | ! [X23] : (? [X24] : (~ p1(X24) & r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8) | ~ sP2(X8)), inference(usedef, [], [e9])).
fof(e9, plain, ! [X8] : (sP2(X8) <=> (? [X20] : (! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) & ~ p1(X20) & r1(X8, X20)) | ! [X23] : (? [X24] : (~ p1(X24) & r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f10, plain, ! [X8] : (sP0(X8) | ! [X31] : (? [X32] : (p1(X32) & ? [X33] : (~ p1(X33) & r1(X32, X33)) & r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)) | ~ sP3(X8)), inference(usedef, [], [e10])).
fof(e10, plain, ! [X8] : (sP3(X8) <=> (sP0(X8) | ! [X31] : (? [X32] : (p1(X32) & ? [X33] : (~ p1(X33) & r1(X32, X33)) & r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f6, plain, ? [X0] : (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(X0, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | ! [X12] : (! [X13] : (? [X14] : (p1(X14) & ? [X15] : (~ p1(X15) & r1(X14, X15)) & r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12))) & ! [X16] : (? [X17] : (~ p1(X17) & r1(X16, X17)) | ! [X18] : (! [X19] : (p1(X19) | ~ r1(X18, X19)) | ~ r1(X16, X18)) | ~ r1(X8, X16)) & (? [X20] : (! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) & ~ p1(X20) & r1(X8, X20)) | ! [X23] : (? [X24] : (~ p1(X24) & r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8)) & (? [X25] : (! [X26] : ((p1(X26) & ? [X27] : (~ p1(X27) & r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) & p1(X25) & ? [X30] : (~ p1(X30) & r1(X25, X30)) & r1(X8, X25)) | ! [X31] : (? [X32] : (p1(X32) & ? [X33] : (~ p1(X33) & r1(X32, X33)) & r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)))) | ~ r1(X0, X8))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ? [X0] : ~ (! [X1] : (~ ! [X2] : (~ ! [X3] : (~ p1(X3) | ! [X4] : (p1(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) | ! [X5] : (~ ! [X6] : (p1(X6) | ~ r1(X5, X6)) | ~ r1(X1, X5)) | ! [X7] : (p1(X7) | ~ r1(X1, X7)) | ~ r1(X0, X1)) | ~ ! [X8] : (((~ ! [X9] : (~ ! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) | p1(X9) | ~ r1(X8, X9)) | ! [X12] : (! [X13] : (~ ! [X14] : (~ p1(X14) | ! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12))) & ! [X16] : (~ ! [X17] : (p1(X17) | ~ r1(X16, X17)) | ! [X18] : (! [X19] : (p1(X19) | ~ r1(X18, X19)) | ~ r1(X16, X18)) | ~ r1(X8, X16)) & (~ ! [X20] : (~ ! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) | p1(X20) | ~ r1(X8, X20)) | ! [X23] : (~ ! [X24] : (p1(X24) | ~ r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8)) & (~ ! [X25] : (~ ! [X26] : (~ (~ p1(X26) | ! [X27] : (p1(X27) | ~ r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) | ~ p1(X25) | ! [X30] : (p1(X30) | ~ r1(X25, X30)) | ~ r1(X8, X25)) | ! [X31] : (~ ! [X32] : (~ p1(X32) | ! [X33] : (p1(X33) | ~ r1(X32, X33)) | ~ r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)))) | ~ r1(X0, X8))), inference(flattening, [], [f4])).
fof(f4, plain, ~ ~ ? [X0] : ~ (~ ~ ! [X1] : (~ ! [X2] : (~ ! [X3] : (~ p1(X3) | ! [X4] : (p1(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) | ! [X5] : (~ ! [X6] : (p1(X6) | ~ r1(X5, X6)) | ~ r1(X1, X5)) | ! [X7] : (p1(X7) | ~ r1(X1, X7)) | ~ r1(X0, X1)) | ~ ! [X8] : (((~ ! [X9] : (~ ! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) | p1(X9) | ~ r1(X8, X9)) | ! [X12] : (! [X13] : (~ ! [X14] : (~ p1(X14) | ! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12))) & ! [X16] : (~ ! [X17] : (p1(X17) | ~ r1(X16, X17)) | ! [X18] : (! [X19] : (p1(X19) | ~ r1(X18, X19)) | ~ r1(X16, X18)) | ~ r1(X8, X16)) & (~ ! [X20] : (~ ! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) | p1(X20) | ~ r1(X8, X20)) | ! [X23] : (~ ! [X24] : (p1(X24) | ~ r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8)) & (~ ! [X25] : (~ ! [X26] : (~ (~ p1(X26) | ! [X27] : (p1(X27) | ~ r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) | ~ p1(X25) | ! [X30] : (p1(X30) | ~ r1(X25, X30)) | ~ r1(X8, X25)) | ! [X31] : (~ ! [X32] : (~ p1(X32) | ! [X33] : (p1(X33) | ~ r1(X32, X33)) | ~ r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)))) | ~ r1(X0, X8))), inference(rectify, [], [f3])).
fof(f3, plain, ~ ~ ? [X0] : ~ (~ ~ ! [X1] : (~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ ! [X1] : (((~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (! [X1] : (~ ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0))) & ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ! [X1] : (! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ r1(X1, X0)) & (~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1)) & (~ ! [X0] : (~ ! [X1] : (~ (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0))) | ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)))) | ~ r1(X0, X1))), inference(negated_conjecture, [], [f2])).
fof(f2, plain, ~ ~ ? [X0] : ~ (~ ~ ! [X1] : (~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ ! [X1] : (((~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (! [X1] : (~ ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0))) & ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ! [X1] : (! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ r1(X1, X0)) & (~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1)) & (~ ! [X0] : (~ ! [X1] : (~ (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0))) | ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)))) | ~ r1(X0, X1))), file('/home/ubuntu/library/tptp/Problems/LCL/LCL658+1.001.p', main)).
fof(f51834, plain, (~ r1(sK14, sK17) | ~ spl21_94), inference(resolution, [], [f1077, f72])).
fof(f72, plain, r1(sK13, sK14), inference(cnf_transformation, [], [f42])).
fof(f1077, plain, (! [X2] : (~ r1(sK13, X2) | ~ r1(X2, sK17)) | ~ spl21_94), inference(avatar_component_clause, [], [f1076])).
fof(f1076, plain, (spl21_94 <=> ! [X2] : (~ r1(X2, sK17) | ~ r1(sK13, X2))), introduced(avatar_definition, [new_symbols(naming, [spl21_94])])).
fof(f51368, plain, (spl21_5 | spl21_24 | ~ spl21_26), inference(avatar_split_clause, [], [f51367, f261, f251, f113])).
fof(f113, plain, (spl21_5 <=> p1(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl21_5])])).
fof(f251, plain, (spl21_24 <=> r1(sK14, sK6(sK14))), introduced(avatar_definition, [new_symbols(naming, [spl21_24])])).
fof(f261, plain, (spl21_26 <=> r1(sK17, sK7(sK17))), introduced(avatar_definition, [new_symbols(naming, [spl21_26])])).
fof(f51367, plain, (p1(sK14) | (spl21_24 | ~ spl21_26)), inference(subsumption_resolution, [], [f51354, f83])).
fof(f83, plain, sP2(sK14), inference(resolution, [], [f66, f72])).
fof(f66, plain, ! [X8] : (~ r1(sK13, X8) | sP2(X8)), inference(cnf_transformation, [], [f42])).
fof(f51354, plain, (p1(sK14) | ~ sP2(sK14) | (spl21_24 | ~ spl21_26)), inference(subsumption_resolution, [], [f51096, f75])).
fof(f51096, plain, (~ r1(sK14, sK17) | p1(sK14) | ~ sP2(sK14) | (spl21_24 | ~ spl21_26)), inference(resolution, [], [f49424, f252])).
fof(f252, plain, (~ r1(sK14, sK6(sK14)) | spl21_24), inference(avatar_component_clause, [], [f251])).
fof(f49424, plain, (! [X0] : (r1(X0, sK6(X0)) | ~ r1(X0, sK17) | p1(X0) | ~ sP2(X0)) | ~ spl21_26), inference(resolution, [], [f49406, f49])).
fof(f49, plain, ! [X4, X0] : (~ p1(sK7(X4)) | r1(X0, sK6(X0)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : ((! [X2] : (~ p1(X2) | ! [X3] : (p1(X3) | ~ r1(X2, X3)) | ~ r1(sK6(X0), X2)) & ~ p1(sK6(X0)) & r1(X0, sK6(X0))) | ! [X4] : ((~ p1(sK7(X4)) & r1(X4, sK7(X4))) | ~ r1(X0, X4)) | p1(X0) | ~ sP2(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7])], [f18, f20, f19])).
fof(f19, plain, ! [X0] : (? [X1] : (! [X2] : (~ p1(X2) | ! [X3] : (p1(X3) | ~ r1(X2, X3)) | ~ r1(X1, X2)) & ~ p1(X1) & r1(X0, X1)) => (! [X2] : (~ p1(X2) | ! [X3] : (p1(X3) | ~ r1(X2, X3)) | ~ r1(sK6(X0), X2)) & ~ p1(sK6(X0)) & r1(X0, sK6(X0)))), introduced(choice_axiom, [])).
fof(f20, plain, ! [X4] : (? [X5] : (~ p1(X5) & r1(X4, X5)) => (~ p1(sK7(X4)) & r1(X4, sK7(X4)))), introduced(choice_axiom, [])).
fof(f18, plain, ! [X0] : (? [X1] : (! [X2] : (~ p1(X2) | ! [X3] : (p1(X3) | ~ r1(X2, X3)) | ~ r1(X1, X2)) & ~ p1(X1) & r1(X0, X1)) | ! [X4] : (? [X5] : (~ p1(X5) & r1(X4, X5)) | ~ r1(X0, X4)) | p1(X0) | ~ sP2(X0)), inference(rectify, [], [f17])).
fof(f17, plain, ! [X8] : (? [X20] : (! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) & ~ p1(X20) & r1(X8, X20)) | ! [X23] : (? [X24] : (~ p1(X24) & r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8) | ~ sP2(X8)), inference(nnf_transformation, [], [f9])).
fof(f49406, plain, (p1(sK7(sK17)) | ~ spl21_26), inference(resolution, [], [f263, f76])).
fof(f76, plain, ! [X6] : (~ r1(sK17, X6) | p1(X6)), inference(cnf_transformation, [], [f42])).
fof(f263, plain, (r1(sK17, sK7(sK17)) | ~ spl21_26), inference(avatar_component_clause, [], [f261])).
fof(f49334, plain, (~ spl21_24 | spl21_31 | ~ spl21_2639), inference(avatar_contradiction_clause, [], [f49333])).
fof(f49333, plain, ($false | (~ spl21_24 | spl21_31 | ~ spl21_2639)), inference(subsumption_resolution, [], [f49332, f253])).
fof(f253, plain, (r1(sK14, sK6(sK14)) | ~ spl21_24), inference(avatar_component_clause, [], [f251])).
fof(f49332, plain, (~ r1(sK14, sK6(sK14)) | (spl21_31 | ~ spl21_2639)), inference(subsumption_resolution, [], [f49331, f401])).
fof(f401, plain, (~ p1(sK6(sK14)) | spl21_31), inference(avatar_component_clause, [], [f400])).
fof(f400, plain, (spl21_31 <=> p1(sK6(sK14))), introduced(avatar_definition, [new_symbols(naming, [spl21_31])])).
fof(f49331, plain, (p1(sK6(sK14)) | ~ r1(sK14, sK6(sK14)) | ~ spl21_2639), inference(resolution, [], [f48187, f79])).
fof(f79, plain, ! [X2] : (~ p1(sK16(X2)) | p1(X2) | ~ r1(sK14, X2)), inference(cnf_transformation, [], [f42])).
fof(f48187, plain, (p1(sK16(sK6(sK14))) | ~ spl21_2639), inference(avatar_component_clause, [], [f48185])).
fof(f48185, plain, (spl21_2639 <=> p1(sK16(sK6(sK14)))), introduced(avatar_definition, [new_symbols(naming, [spl21_2639])])).
fof(f49248, plain, (spl21_26 | ~ spl21_272), inference(avatar_contradiction_clause, [], [f49247])).
fof(f49247, plain, ($false | (spl21_26 | ~ spl21_272)), inference(subsumption_resolution, [], [f48994, f75])).
fof(f48994, plain, (~ r1(sK14, sK17) | (spl21_26 | ~ spl21_272)), inference(resolution, [], [f2799, f262])).
fof(f262, plain, (~ r1(sK17, sK7(sK17)) | spl21_26), inference(avatar_component_clause, [], [f261])).
fof(f2799, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0)) | ~ spl21_272), inference(avatar_component_clause, [], [f2798])).
fof(f2798, plain, (spl21_272 <=> ! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0))), introduced(avatar_definition, [new_symbols(naming, [spl21_272])])).
fof(f48730, plain, (~ spl21_26 | ~ spl21_2638), inference(avatar_contradiction_clause, [], [f48729])).
fof(f48729, plain, ($false | (~ spl21_26 | ~ spl21_2638)), inference(subsumption_resolution, [], [f48726, f75])).
fof(f48726, plain, (~ r1(sK14, sK17) | (~ spl21_26 | ~ spl21_2638)), inference(resolution, [], [f48181, f477])).
fof(f477, plain, (p1(sK7(sK17)) | ~ spl21_26), inference(resolution, [], [f263, f76])).
fof(f48181, plain, (! [X0] : (~ p1(sK7(X0)) | ~ r1(sK14, X0)) | ~ spl21_2638), inference(avatar_component_clause, [], [f48180])).
fof(f48180, plain, (spl21_2638 <=> ! [X0] : (~ p1(sK7(X0)) | ~ r1(sK14, X0))), introduced(avatar_definition, [new_symbols(naming, [spl21_2638])])).
fof(f48190, plain, (~ spl21_24 | spl21_5 | spl21_2638 | spl21_2639 | ~ spl21_30), inference(avatar_split_clause, [], [f48189, f396, f48185, f48180, f113, f251])).
fof(f396, plain, (spl21_30 <=> p1(sK15(sK6(sK14)))), introduced(avatar_definition, [new_symbols(naming, [spl21_30])])).
fof(f48189, plain, (! [X0] : (p1(sK16(sK6(sK14))) | ~ p1(sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ r1(sK14, sK6(sK14))) | ~ spl21_30), inference(subsumption_resolution, [], [f4832, f83])).
fof(f4832, plain, (! [X0] : (p1(sK16(sK6(sK14))) | ~ p1(sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ sP2(sK14) | ~ r1(sK14, sK6(sK14))) | ~ spl21_30), inference(resolution, [], [f2011, f398])).
fof(f398, plain, (p1(sK15(sK6(sK14))) | ~ spl21_30), inference(avatar_component_clause, [], [f396])).
fof(f2011, plain, ! [X0, X1] : (~ p1(sK15(sK6(X0))) | p1(sK16(sK6(X0))) | ~ p1(sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0))), inference(subsumption_resolution, [], [f2010, f51])).
fof(f51, plain, ! [X4, X0] : (~ p1(sK7(X4)) | ~ p1(sK6(X0)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f21])).
fof(f2010, plain, ! [X0, X1] : (p1(sK16(sK6(X0))) | ~ p1(sK15(sK6(X0))) | ~ p1(sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0)) | p1(sK6(X0))), inference(duplicate_literal_removal, [], [f2000])).
fof(f2000, plain, ! [X0, X1] : (p1(sK16(sK6(X0))) | ~ p1(sK15(sK6(X0))) | ~ p1(sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0)) | p1(sK6(X0)) | ~ r1(sK14, sK6(X0))), inference(resolution, [], [f415, f78])).
fof(f78, plain, ! [X2] : (r1(sK15(X2), sK16(X2)) | p1(X2) | ~ r1(sK14, X2)), inference(cnf_transformation, [], [f42])).
fof(f415, plain, ! [X6, X8, X7] : (~ r1(sK15(sK6(X7)), X6) | p1(X6) | ~ p1(sK15(sK6(X7))) | ~ p1(sK7(X8)) | ~ r1(X7, X8) | p1(X7) | ~ sP2(X7) | ~ r1(sK14, sK6(X7))), inference(subsumption_resolution, [], [f412, f51])).
fof(f412, plain, ! [X6, X8, X7] : (p1(X6) | ~ r1(sK15(sK6(X7)), X6) | ~ p1(sK15(sK6(X7))) | ~ p1(sK7(X8)) | ~ r1(X7, X8) | p1(X7) | ~ sP2(X7) | p1(sK6(X7)) | ~ r1(sK14, sK6(X7))), inference(resolution, [], [f53, f77])).
fof(f77, plain, ! [X2] : (r1(X2, sK15(X2)) | p1(X2) | ~ r1(sK14, X2)), inference(cnf_transformation, [], [f42])).
fof(f53, plain, ! [X4, X2, X0, X3] : (~ r1(sK6(X0), X2) | p1(X3) | ~ r1(X2, X3) | ~ p1(X2) | ~ p1(sK7(X4)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f21])).
fof(f48188, plain, (~ spl21_24 | spl21_5 | spl21_272 | spl21_2639 | ~ spl21_30), inference(avatar_split_clause, [], [f48183, f396, f48185, f2798, f113, f251])).
fof(f48183, plain, (! [X0] : (p1(sK16(sK6(sK14))) | r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ r1(sK14, sK6(sK14))) | ~ spl21_30), inference(subsumption_resolution, [], [f5385, f83])).
fof(f5385, plain, (! [X0] : (p1(sK16(sK6(sK14))) | r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ sP2(sK14) | ~ r1(sK14, sK6(sK14))) | ~ spl21_30), inference(resolution, [], [f2025, f398])).
fof(f2025, plain, ! [X0, X1] : (~ p1(sK15(sK6(X0))) | p1(sK16(sK6(X0))) | r1(X1, sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0))), inference(subsumption_resolution, [], [f2024, f50])).
fof(f50, plain, ! [X4, X0] : (~ p1(sK6(X0)) | r1(X4, sK7(X4)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f21])).
fof(f2024, plain, ! [X0, X1] : (p1(sK16(sK6(X0))) | ~ p1(sK15(sK6(X0))) | r1(X1, sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0)) | p1(sK6(X0))), inference(duplicate_literal_removal, [], [f2014])).
fof(f2014, plain, ! [X0, X1] : (p1(sK16(sK6(X0))) | ~ p1(sK15(sK6(X0))) | r1(X1, sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0)) | p1(sK6(X0)) | ~ r1(sK14, sK6(X0))), inference(resolution, [], [f476, f78])).
fof(f476, plain, ! [X6, X8, X7] : (~ r1(sK15(sK6(X7)), X6) | p1(X6) | ~ p1(sK15(sK6(X7))) | r1(X8, sK7(X8)) | ~ r1(X7, X8) | p1(X7) | ~ sP2(X7) | ~ r1(sK14, sK6(X7))), inference(subsumption_resolution, [], [f473, f50])).
fof(f473, plain, ! [X6, X8, X7] : (p1(X6) | ~ r1(sK15(sK6(X7)), X6) | ~ p1(sK15(sK6(X7))) | r1(X8, sK7(X8)) | ~ r1(X7, X8) | p1(X7) | ~ sP2(X7) | p1(sK6(X7)) | ~ r1(sK14, sK6(X7))), inference(resolution, [], [f52, f77])).
fof(f52, plain, ! [X4, X2, X0, X3] : (~ r1(sK6(X0), X2) | p1(X3) | ~ r1(X2, X3) | ~ p1(X2) | r1(X4, sK7(X4)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f21])).
fof(f48159, plain, (~ spl21_5 | spl21_41 | spl21_27 | ~ spl21_60 | spl21_98), inference(avatar_split_clause, [], [f48158, f1111, f785, f304, f468, f113])).
fof(f468, plain, (spl21_41 <=> ! [X11] : (r1(X11, sK4(X11)) | ~ r1(sK14, X11))), introduced(avatar_definition, [new_symbols(naming, [spl21_41])])).
fof(f304, plain, (spl21_27 <=> sP0(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl21_27])])).
fof(f785, plain, (spl21_60 <=> r1(sK14, sK20(sK14))), introduced(avatar_definition, [new_symbols(naming, [spl21_60])])).
fof(f1111, plain, (spl21_98 <=> p1(sK20(sK14))), introduced(avatar_definition, [new_symbols(naming, [spl21_98])])).
fof(f48158, plain, (! [X2] : (r1(X2, sK4(X2)) | ~ r1(sK14, X2) | ~ p1(sK14)) | (spl21_27 | ~ spl21_60 | spl21_98)), inference(subsumption_resolution, [], [f48157, f81])).
fof(f81, plain, sP3(sK14), inference(resolution, [], [f65, f72])).
fof(f65, plain, ! [X8] : (~ r1(sK13, X8) | sP3(X8)), inference(cnf_transformation, [], [f42])).
fof(f48157, plain, (! [X2] : (r1(X2, sK4(X2)) | ~ r1(sK14, X2) | ~ p1(sK14) | ~ sP3(sK14)) | (spl21_27 | ~ spl21_60 | spl21_98)), inference(subsumption_resolution, [], [f48156, f305])).
fof(f305, plain, (~ sP0(sK14) | spl21_27), inference(avatar_component_clause, [], [f304])).
fof(f48156, plain, (! [X2] : (r1(X2, sK4(X2)) | ~ r1(sK14, X2) | ~ p1(sK14) | sP0(sK14) | ~ sP3(sK14)) | (~ spl21_60 | spl21_98)), inference(subsumption_resolution, [], [f2325, f1112])).
fof(f1112, plain, (~ p1(sK20(sK14)) | spl21_98), inference(avatar_component_clause, [], [f1111])).
fof(f2325, plain, (! [X2] : (r1(X2, sK4(X2)) | ~ r1(sK14, X2) | ~ p1(sK14) | p1(sK20(sK14)) | sP0(sK14) | ~ sP3(sK14)) | ~ spl21_60), inference(resolution, [], [f787, f44])).
fof(f44, plain, ! [X4, X0, X1] : (~ r1(X0, X4) | r1(X1, sK4(X1)) | ~ r1(X0, X1) | ~ p1(X0) | p1(X4) | sP0(X0) | ~ sP3(X0)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ! [X0] : (sP0(X0) | ! [X1] : ((p1(sK4(X1)) & (~ p1(sK5(X1)) & r1(sK4(X1), sK5(X1))) & r1(X1, sK4(X1))) | ~ r1(X0, X1)) | ~ p1(X0) | ! [X4] : (p1(X4) | ~ r1(X0, X4)) | ~ sP3(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5])], [f13, f15, f14])).
fof(f14, plain, ! [X1] : (? [X2] : (p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3)) & r1(X1, X2)) => (p1(sK4(X1)) & ? [X3] : (~ p1(X3) & r1(sK4(X1), X3)) & r1(X1, sK4(X1)))), introduced(choice_axiom, [])).
fof(f15, plain, ! [X1] : (? [X3] : (~ p1(X3) & r1(sK4(X1), X3)) => (~ p1(sK5(X1)) & r1(sK4(X1), sK5(X1)))), introduced(choice_axiom, [])).
fof(f13, plain, ! [X0] : (sP0(X0) | ! [X1] : (? [X2] : (p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3)) & r1(X1, X2)) | ~ r1(X0, X1)) | ~ p1(X0) | ! [X4] : (p1(X4) | ~ r1(X0, X4)) | ~ sP3(X0)), inference(rectify, [], [f12])).
fof(f12, plain, ! [X8] : (sP0(X8) | ! [X31] : (? [X32] : (p1(X32) & ? [X33] : (~ p1(X33) & r1(X32, X33)) & r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)) | ~ sP3(X8)), inference(nnf_transformation, [], [f10])).
fof(f787, plain, (r1(sK14, sK20(sK14)) | ~ spl21_60), inference(avatar_component_clause, [], [f785])).
fof(f48078, plain, (~ spl21_5 | spl21_27 | ~ spl21_60 | spl21_98 | ~ spl21_1691), inference(avatar_contradiction_clause, [], [f48077])).
fof(f48077, plain, ($false | (~ spl21_5 | spl21_27 | ~ spl21_60 | spl21_98 | ~ spl21_1691)), inference(subsumption_resolution, [], [f48043, f1112])).
fof(f48043, plain, (p1(sK20(sK14)) | (~ spl21_5 | spl21_27 | ~ spl21_60 | ~ spl21_1691)), inference(resolution, [], [f47739, f787])).
fof(f47739, plain, (! [X1] : (~ r1(sK14, X1) | p1(X1)) | (~ spl21_5 | spl21_27 | ~ spl21_1691)), inference(subsumption_resolution, [], [f47738, f81])).
fof(f47738, plain, (! [X1] : (p1(X1) | ~ r1(sK14, X1) | ~ sP3(sK14)) | (~ spl21_5 | spl21_27 | ~ spl21_1691)), inference(subsumption_resolution, [], [f47737, f115])).
fof(f115, plain, (p1(sK14) | ~ spl21_5), inference(avatar_component_clause, [], [f113])).
fof(f47737, plain, (! [X1] : (~ p1(sK14) | p1(X1) | ~ r1(sK14, X1) | ~ sP3(sK14)) | (spl21_27 | ~ spl21_1691)), inference(subsumption_resolution, [], [f47736, f305])).
fof(f47736, plain, (! [X1] : (sP0(sK14) | ~ p1(sK14) | p1(X1) | ~ r1(sK14, X1) | ~ sP3(sK14)) | ~ spl21_1691), inference(resolution, [], [f23539, f75])).
fof(f23539, plain, (! [X0, X1] : (~ r1(X0, sK17) | sP0(X0) | ~ p1(X0) | p1(X1) | ~ r1(X0, X1) | ~ sP3(X0)) | ~ spl21_1691), inference(resolution, [], [f23520, f46])).
fof(f46, plain, ! [X4, X0, X1] : (~ p1(sK5(X1)) | sP0(X0) | ~ r1(X0, X1) | ~ p1(X0) | p1(X4) | ~ r1(X0, X4) | ~ sP3(X0)), inference(cnf_transformation, [], [f16])).
fof(f23520, plain, (p1(sK5(sK17)) | ~ spl21_1691), inference(avatar_component_clause, [], [f23518])).
fof(f23518, plain, (spl21_1691 <=> p1(sK5(sK17))), introduced(avatar_definition, [new_symbols(naming, [spl21_1691])])).
fof(f23523, plain, (spl21_1691 | ~ spl21_40 | ~ spl21_41 | ~ spl21_59), inference(avatar_split_clause, [], [f23522, f781, f468, f462, f23518])).
fof(f462, plain, (spl21_40 <=> ! [X12] : (r1(sK4(X12), sK5(X12)) | ~ r1(sK14, X12))), introduced(avatar_definition, [new_symbols(naming, [spl21_40])])).
fof(f781, plain, (spl21_59 <=> ! [X3, X2] : (~ r1(X2, X3) | p1(X3) | ~ r1(sK17, X2))), introduced(avatar_definition, [new_symbols(naming, [spl21_59])])).
fof(f23522, plain, (p1(sK5(sK17)) | (~ spl21_40 | ~ spl21_41 | ~ spl21_59)), inference(subsumption_resolution, [], [f23495, f75])).
fof(f23495, plain, (p1(sK5(sK17)) | ~ r1(sK14, sK17) | (~ spl21_40 | ~ spl21_41 | ~ spl21_59)), inference(resolution, [], [f23272, f463])).
fof(f463, plain, (! [X12] : (r1(sK4(X12), sK5(X12)) | ~ r1(sK14, X12)) | ~ spl21_40), inference(avatar_component_clause, [], [f462])).
fof(f23272, plain, (! [X51] : (~ r1(sK4(sK17), X51) | p1(X51)) | (~ spl21_41 | ~ spl21_59)), inference(subsumption_resolution, [], [f23113, f75])).
fof(f23113, plain, (! [X51] : (~ r1(sK14, sK17) | p1(X51) | ~ r1(sK4(sK17), X51)) | (~ spl21_41 | ~ spl21_59)), inference(resolution, [], [f469, f782])).
fof(f782, plain, (! [X2, X3] : (~ r1(sK17, X2) | p1(X3) | ~ r1(X2, X3)) | ~ spl21_59), inference(avatar_component_clause, [], [f781])).
fof(f469, plain, (! [X11] : (r1(X11, sK4(X11)) | ~ r1(sK14, X11)) | ~ spl21_41), inference(avatar_component_clause, [], [f468])).
fof(f22994, plain, (~ spl21_27 | ~ spl21_1678), inference(avatar_contradiction_clause, [], [f22993])).
fof(f22993, plain, ($false | (~ spl21_27 | ~ spl21_1678)), inference(subsumption_resolution, [], [f22992, f306])).
fof(f306, plain, (sP0(sK14) | ~ spl21_27), inference(avatar_component_clause, [], [f304])).
fof(f22992, plain, (~ sP0(sK14) | ~ spl21_1678), inference(subsumption_resolution, [], [f22989, f72])).
fof(f22989, plain, (~ r1(sK13, sK14) | ~ sP0(sK14) | ~ spl21_1678), inference(resolution, [], [f22961, f58])).
fof(f58, plain, ! [X0] : (r1(X0, sK10(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f32])).
fof(f32, plain, ! [X0] : ((! [X2] : ((p1(X2) & (~ p1(sK11(X2)) & r1(X2, sK11(X2)))) | ! [X4] : (~ p1(X4) | ! [X5] : (p1(X5) | ~ r1(X4, X5)) | ~ r1(X2, X4)) | ~ r1(sK10(X0), X2)) & p1(sK10(X0)) & (~ p1(sK12(X0)) & r1(sK10(X0), sK12(X0))) & r1(X0, sK10(X0))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11, sK12])], [f28, f31, f30, f29])).
fof(f29, plain, ! [X0] : (? [X1] : (! [X2] : ((p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3))) | ! [X4] : (~ p1(X4) | ! [X5] : (p1(X5) | ~ r1(X4, X5)) | ~ r1(X2, X4)) | ~ r1(X1, X2)) & p1(X1) & ? [X6] : (~ p1(X6) & r1(X1, X6)) & r1(X0, X1)) => (! [X2] : ((p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3))) | ! [X4] : (~ p1(X4) | ! [X5] : (p1(X5) | ~ r1(X4, X5)) | ~ r1(X2, X4)) | ~ r1(sK10(X0), X2)) & p1(sK10(X0)) & ? [X6] : (~ p1(X6) & r1(sK10(X0), X6)) & r1(X0, sK10(X0)))), introduced(choice_axiom, [])).
fof(f30, plain, ! [X2] : (? [X3] : (~ p1(X3) & r1(X2, X3)) => (~ p1(sK11(X2)) & r1(X2, sK11(X2)))), introduced(choice_axiom, [])).
fof(f31, plain, ! [X0] : (? [X6] : (~ p1(X6) & r1(sK10(X0), X6)) => (~ p1(sK12(X0)) & r1(sK10(X0), sK12(X0)))), introduced(choice_axiom, [])).
fof(f28, plain, ! [X0] : (? [X1] : (! [X2] : ((p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3))) | ! [X4] : (~ p1(X4) | ! [X5] : (p1(X5) | ~ r1(X4, X5)) | ~ r1(X2, X4)) | ~ r1(X1, X2)) & p1(X1) & ? [X6] : (~ p1(X6) & r1(X1, X6)) & r1(X0, X1)) | ~ sP0(X0)), inference(rectify, [], [f27])).
fof(f27, plain, ! [X8] : (? [X25] : (! [X26] : ((p1(X26) & ? [X27] : (~ p1(X27) & r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) & p1(X25) & ? [X30] : (~ p1(X30) & r1(X25, X30)) & r1(X8, X25)) | ~ sP0(X8)), inference(nnf_transformation, [], [f7])).
fof(f22961, plain, (! [X2] : (~ r1(X2, sK10(sK14)) | ~ r1(sK13, X2)) | ~ spl21_1678), inference(avatar_component_clause, [], [f22960])).
fof(f22960, plain, (spl21_1678 <=> ! [X2] : (~ r1(X2, sK10(sK14)) | ~ r1(sK13, X2))), introduced(avatar_definition, [new_symbols(naming, [spl21_1678])])).
fof(f22962, plain, (spl21_1678 | spl21_63 | ~ spl21_181), inference(avatar_split_clause, [], [f22958, f1750, f798, f22960])).
fof(f798, plain, (spl21_63 <=> ! [X11, X10] : (~ r1(X10, X11) | p1(X11) | ~ r1(sK10(sK14), X10))), introduced(avatar_definition, [new_symbols(naming, [spl21_63])])).
fof(f1750, plain, (spl21_181 <=> p1(sK20(sK10(sK14)))), introduced(avatar_definition, [new_symbols(naming, [spl21_181])])).
fof(f22958, plain, (! [X2, X0, X1] : (p1(X0) | ~ r1(X1, X0) | ~ r1(sK10(sK14), X1) | ~ r1(X2, sK10(sK14)) | ~ r1(sK13, X2)) | ~ spl21_181), inference(resolution, [], [f1752, f68])).
fof(f68, plain, ! [X14, X12, X8, X15] : (~ p1(sK20(X12)) | p1(X15) | ~ r1(X14, X15) | ~ r1(X12, X14) | ~ r1(X8, X12) | ~ r1(sK13, X8)), inference(cnf_transformation, [], [f42])).
fof(f1752, plain, (p1(sK20(sK10(sK14))) | ~ spl21_181), inference(avatar_component_clause, [], [f1750])).
fof(f22957, plain, (spl21_189 | spl21_190 | spl21_181 | ~ spl21_62), inference(avatar_split_clause, [], [f3520, f794, f1750, f1787, f1784])).
fof(f1784, plain, (spl21_189 <=> ! [X9] : (~ r1(X9, sK10(sK14)) | ~ sP1(X9))), introduced(avatar_definition, [new_symbols(naming, [spl21_189])])).
fof(f1787, plain, (spl21_190 <=> r1(sK20(sK10(sK14)), sK8(sK20(sK10(sK14))))), introduced(avatar_definition, [new_symbols(naming, [spl21_190])])).
fof(f794, plain, (spl21_62 <=> r1(sK10(sK14), sK20(sK10(sK14)))), introduced(avatar_definition, [new_symbols(naming, [spl21_62])])).
fof(f3520, plain, (! [X10] : (p1(sK20(sK10(sK14))) | r1(sK20(sK10(sK14)), sK8(sK20(sK10(sK14)))) | ~ r1(X10, sK10(sK14)) | ~ sP1(X10)) | ~ spl21_62), inference(resolution, [], [f796, f54])).
fof(f54, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p1(X2) | r1(X2, sK8(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f26])).
fof(f26, plain, ! [X0] : (! [X1] : (! [X2] : ((p1(sK8(X2)) & (~ p1(sK9(X2)) & r1(sK8(X2), sK9(X2))) & r1(X2, sK8(X2))) | p1(X2) | ~ r1(X1, X2)) | ~ r1(X0, X1)) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f23, f25, f24])).
fof(f24, plain, ! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) => (p1(sK8(X2)) & ? [X4] : (~ p1(X4) & r1(sK8(X2), X4)) & r1(X2, sK8(X2)))), introduced(choice_axiom, [])).
fof(f25, plain, ! [X2] : (? [X4] : (~ p1(X4) & r1(sK8(X2), X4)) => (~ p1(sK9(X2)) & r1(sK8(X2), sK9(X2)))), introduced(choice_axiom, [])).
fof(f23, plain, ! [X0] : (! [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) | ~ r1(X0, X1)) | ~ sP1(X0)), inference(rectify, [], [f22])).
fof(f22, plain, ! [X8] : (! [X12] : (! [X13] : (? [X14] : (p1(X14) & ? [X15] : (~ p1(X15) & r1(X14, X15)) & r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12)) | ~ sP1(X8)), inference(nnf_transformation, [], [f8])).
fof(f796, plain, (r1(sK10(sK14), sK20(sK10(sK14))) | ~ spl21_62), inference(avatar_component_clause, [], [f794])).
fof(f22956, plain, (spl21_189 | ~ spl21_62 | spl21_181 | ~ spl21_611), inference(avatar_split_clause, [], [f22955, f7076, f1750, f794, f1784])).
fof(f7076, plain, (spl21_611 <=> p1(sK9(sK20(sK10(sK14))))), introduced(avatar_definition, [new_symbols(naming, [spl21_611])])).
fof(f22955, plain, (! [X1] : (~ r1(X1, sK10(sK14)) | ~ sP1(X1)) | (~ spl21_62 | spl21_181 | ~ spl21_611)), inference(resolution, [], [f10201, f796])).
fof(f10201, plain, (! [X0, X1] : (~ r1(X0, sK20(sK10(sK14))) | ~ r1(X1, X0) | ~ sP1(X1)) | (spl21_181 | ~ spl21_611)), inference(subsumption_resolution, [], [f10200, f1751])).
fof(f1751, plain, (~ p1(sK20(sK10(sK14))) | spl21_181), inference(avatar_component_clause, [], [f1750])).
fof(f10200, plain, (! [X0, X1] : (p1(sK20(sK10(sK14))) | ~ r1(X0, sK20(sK10(sK14))) | ~ r1(X1, X0) | ~ sP1(X1)) | ~ spl21_611), inference(resolution, [], [f7078, f56])).
fof(f56, plain, ! [X2, X0, X1] : (~ p1(sK9(X2)) | p1(X2) | ~ r1(X1, X2) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f26])).
fof(f7078, plain, (p1(sK9(sK20(sK10(sK14)))) | ~ spl21_611), inference(avatar_component_clause, [], [f7076])).
fof(f10189, plain, (spl21_611 | ~ spl21_179 | ~ spl21_190 | ~ spl21_191 | ~ spl21_192), inference(avatar_split_clause, [], [f10171, f1797, f1792, f1787, f1739, f7076])).
fof(f1739, plain, (spl21_179 <=> ! [X1, X0] : (~ p1(X0) | ~ r1(sK20(sK10(sK14)), X0) | ~ r1(X0, X1) | p1(X1))), introduced(avatar_definition, [new_symbols(naming, [spl21_179])])).
fof(f1792, plain, (spl21_191 <=> r1(sK8(sK20(sK10(sK14))), sK9(sK20(sK10(sK14))))), introduced(avatar_definition, [new_symbols(naming, [spl21_191])])).
fof(f1797, plain, (spl21_192 <=> p1(sK8(sK20(sK10(sK14))))), introduced(avatar_definition, [new_symbols(naming, [spl21_192])])).
fof(f10171, plain, (p1(sK9(sK20(sK10(sK14)))) | (~ spl21_179 | ~ spl21_190 | ~ spl21_191 | ~ spl21_192)), inference(resolution, [], [f7715, f1794])).
fof(f1794, plain, (r1(sK8(sK20(sK10(sK14))), sK9(sK20(sK10(sK14)))) | ~ spl21_191), inference(avatar_component_clause, [], [f1792])).
fof(f7715, plain, (! [X0] : (~ r1(sK8(sK20(sK10(sK14))), X0) | p1(X0)) | (~ spl21_179 | ~ spl21_190 | ~ spl21_192)), inference(subsumption_resolution, [], [f7700, f1799])).
fof(f1799, plain, (p1(sK8(sK20(sK10(sK14)))) | ~ spl21_192), inference(avatar_component_clause, [], [f1797])).
fof(f7700, plain, (! [X0] : (~ p1(sK8(sK20(sK10(sK14)))) | ~ r1(sK8(sK20(sK10(sK14))), X0) | p1(X0)) | (~ spl21_179 | ~ spl21_190)), inference(resolution, [], [f1740, f1789])).
fof(f1789, plain, (r1(sK20(sK10(sK14)), sK8(sK20(sK10(sK14)))) | ~ spl21_190), inference(avatar_component_clause, [], [f1787])).
fof(f1740, plain, (! [X0, X1] : (~ r1(sK20(sK10(sK14)), X0) | ~ p1(X0) | ~ r1(X0, X1) | p1(X1)) | ~ spl21_179), inference(avatar_component_clause, [], [f1739])).
fof(f8632, plain, spl21_13, inference(avatar_split_clause, [], [f8631, f157])).
fof(f157, plain, (spl21_13 <=> sP1(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl21_13])])).
fof(f8631, plain, sP1(sK14), inference(subsumption_resolution, [], [f2217, f72])).
fof(f2217, plain, (~ r1(sK13, sK14) | sP1(sK14)), inference(duplicate_literal_removal, [], [f2216])).
fof(f2216, plain, (~ r1(sK13, sK14) | sP1(sK14) | sP1(sK14) | ~ r1(sK13, sK14)), inference(resolution, [], [f1146, f69])).
fof(f69, plain, ! [X8] : (r1(X8, sK19(X8)) | sP1(X8) | ~ r1(sK13, X8)), inference(cnf_transformation, [], [f42])).
fof(f1146, plain, ! [X0] : (~ r1(sK14, sK19(X0)) | ~ r1(sK13, X0) | sP1(X0)), inference(subsumption_resolution, [], [f1145, f70])).
fof(f70, plain, ! [X8] : (~ p1(sK19(X8)) | sP1(X8) | ~ r1(sK13, X8)), inference(cnf_transformation, [], [f42])).
fof(f1145, plain, ! [X0] : (sP1(X0) | ~ r1(sK13, X0) | ~ r1(sK14, sK19(X0)) | p1(sK19(X0))), inference(subsumption_resolution, [], [f1144, f79])).
fof(f1144, plain, ! [X0] : (p1(sK16(sK19(X0))) | sP1(X0) | ~ r1(sK13, X0) | ~ r1(sK14, sK19(X0)) | p1(sK19(X0))), inference(duplicate_literal_removal, [], [f1137])).
fof(f1137, plain, ! [X0] : (p1(sK16(sK19(X0))) | sP1(X0) | ~ r1(sK13, X0) | ~ r1(sK14, sK19(X0)) | p1(sK19(X0)) | ~ r1(sK14, sK19(X0))), inference(resolution, [], [f283, f78])).
fof(f283, plain, ! [X4, X5] : (~ r1(sK15(sK19(X5)), X4) | p1(X4) | sP1(X5) | ~ r1(sK13, X5) | ~ r1(sK14, sK19(X5))), inference(subsumption_resolution, [], [f282, f70])).
fof(f282, plain, ! [X4, X5] : (p1(X4) | ~ r1(sK15(sK19(X5)), X4) | sP1(X5) | ~ r1(sK13, X5) | p1(sK19(X5)) | ~ r1(sK14, sK19(X5))), inference(subsumption_resolution, [], [f279, f80])).
fof(f80, plain, ! [X2] : (~ r1(sK14, X2) | p1(X2) | p1(sK15(X2))), inference(cnf_transformation, [], [f42])).
fof(f279, plain, ! [X4, X5] : (p1(X4) | ~ r1(sK15(sK19(X5)), X4) | ~ p1(sK15(sK19(X5))) | sP1(X5) | ~ r1(sK13, X5) | p1(sK19(X5)) | ~ r1(sK14, sK19(X5))), inference(resolution, [], [f71, f77])).
fof(f71, plain, ! [X10, X8, X11] : (~ r1(sK19(X8), X10) | p1(X11) | ~ r1(X10, X11) | ~ p1(X10) | sP1(X8) | ~ r1(sK13, X8)), inference(cnf_transformation, [], [f42])).
fof(f8577, plain, ~ spl21_244, inference(avatar_contradiction_clause, [], [f8576])).
fof(f8576, plain, ($false | ~ spl21_244), inference(subsumption_resolution, [], [f8560, f72])).
fof(f8560, plain, (~ r1(sK13, sK14) | ~ spl21_244), inference(resolution, [], [f2338, f43])).
fof(f43, plain, ! [X0] : r1(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : r1(X0, X0), file('/home/ubuntu/library/tptp/Problems/LCL/LCL658+1.001.p', reflexivity)).
fof(f2338, plain, (! [X2] : (~ r1(sK13, X2) | ~ r1(X2, sK14)) | ~ spl21_244), inference(avatar_component_clause, [], [f2337])).
fof(f2337, plain, (spl21_244 <=> ! [X2] : (~ r1(X2, sK14) | ~ r1(sK13, X2))), introduced(avatar_definition, [new_symbols(naming, [spl21_244])])).
fof(f3506, plain, (~ spl21_27 | ~ spl21_63), inference(avatar_contradiction_clause, [], [f3505])).
fof(f3505, plain, ($false | (~ spl21_27 | ~ spl21_63)), inference(subsumption_resolution, [], [f3504, f306])).
fof(f3504, plain, (~ sP0(sK14) | (~ spl21_27 | ~ spl21_63)), inference(resolution, [], [f3490, f60])).
fof(f60, plain, ! [X0] : (~ p1(sK12(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f32])).
fof(f3490, plain, (p1(sK12(sK14)) | (~ spl21_27 | ~ spl21_63)), inference(subsumption_resolution, [], [f3480, f306])).
fof(f3480, plain, (p1(sK12(sK14)) | ~ sP0(sK14) | ~ spl21_63), inference(resolution, [], [f3111, f59])).
fof(f59, plain, ! [X0] : (r1(sK10(X0), sK12(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f32])).
fof(f3111, plain, (! [X3] : (~ r1(sK10(sK14), X3) | p1(X3)) | ~ spl21_63), inference(resolution, [], [f799, f43])).
fof(f799, plain, (! [X10, X11] : (~ r1(sK10(sK14), X10) | p1(X11) | ~ r1(X10, X11)) | ~ spl21_63), inference(avatar_component_clause, [], [f798])).
fof(f2802, plain, (spl21_5 | ~ spl21_26 | ~ spl21_31), inference(avatar_split_clause, [], [f2801, f400, f261, f113])).
fof(f2801, plain, (p1(sK14) | (~ spl21_26 | ~ spl21_31)), inference(subsumption_resolution, [], [f2790, f83])).
fof(f2790, plain, (p1(sK14) | ~ sP2(sK14) | (~ spl21_26 | ~ spl21_31)), inference(subsumption_resolution, [], [f2788, f75])).
fof(f2788, plain, (~ r1(sK14, sK17) | p1(sK14) | ~ sP2(sK14) | (~ spl21_26 | ~ spl21_31)), inference(resolution, [], [f402, f486])).
fof(f486, plain, (! [X1] : (~ p1(sK6(X1)) | ~ r1(X1, sK17) | p1(X1) | ~ sP2(X1)) | ~ spl21_26), inference(resolution, [], [f477, f51])).
fof(f402, plain, (p1(sK6(sK14)) | ~ spl21_31), inference(avatar_component_clause, [], [f400])).
fof(f2800, plain, (spl21_5 | spl21_272 | ~ spl21_31), inference(avatar_split_clause, [], [f2796, f400, f2798, f113])).
fof(f2796, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14)) | ~ spl21_31), inference(subsumption_resolution, [], [f2789, f83])).
fof(f2789, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ sP2(sK14)) | ~ spl21_31), inference(resolution, [], [f402, f50])).
fof(f2689, plain, (~ spl21_27 | ~ spl21_13 | ~ spl21_189), inference(avatar_split_clause, [], [f2688, f1784, f157, f304])).
fof(f2688, plain, (~ sP0(sK14) | (~ spl21_13 | ~ spl21_189)), inference(subsumption_resolution, [], [f2666, f159])).
fof(f159, plain, (sP1(sK14) | ~ spl21_13), inference(avatar_component_clause, [], [f157])).
fof(f2666, plain, (~ sP1(sK14) | ~ sP0(sK14) | ~ spl21_189), inference(resolution, [], [f1785, f58])).
fof(f1785, plain, (! [X9] : (~ r1(X9, sK10(sK14)) | ~ sP1(X9)) | ~ spl21_189), inference(avatar_component_clause, [], [f1784])).
fof(f2339, plain, (spl21_244 | spl21_61 | ~ spl21_98), inference(avatar_split_clause, [], [f2335, f1111, f789, f2337])).
fof(f789, plain, (spl21_61 <=> ! [X5, X4] : (~ r1(X4, X5) | p1(X5) | ~ r1(sK14, X4))), introduced(avatar_definition, [new_symbols(naming, [spl21_61])])).
fof(f2335, plain, (! [X2, X0, X1] : (p1(X0) | ~ r1(X1, X0) | ~ r1(sK14, X1) | ~ r1(X2, sK14) | ~ r1(sK13, X2)) | ~ spl21_98), inference(resolution, [], [f1113, f68])).
fof(f1113, plain, (p1(sK20(sK14)) | ~ spl21_98), inference(avatar_component_clause, [], [f1111])).
fof(f2321, plain, ~ spl21_61, inference(avatar_contradiction_clause, [], [f2320])).
fof(f2320, plain, ($false | ~ spl21_61), inference(subsumption_resolution, [], [f2313, f74])).
fof(f74, plain, ~ p1(sK18), inference(cnf_transformation, [], [f42])).
fof(f2313, plain, (p1(sK18) | ~ spl21_61), inference(resolution, [], [f2198, f43])).
fof(f2198, plain, (! [X0] : (~ r1(sK18, X0) | p1(X0)) | ~ spl21_61), inference(resolution, [], [f790, f73])).
fof(f73, plain, r1(sK14, sK18), inference(cnf_transformation, [], [f42])).
fof(f790, plain, (! [X4, X5] : (~ r1(sK14, X4) | p1(X5) | ~ r1(X4, X5)) | ~ spl21_61), inference(avatar_component_clause, [], [f789])).
fof(f1808, plain, (~ spl21_27 | spl21_181 | spl21_179 | ~ spl21_62), inference(avatar_split_clause, [], [f1696, f794, f1739, f1750, f304])).
fof(f1696, plain, (! [X4, X5] : (~ p1(X4) | p1(X5) | ~ r1(X4, X5) | ~ r1(sK20(sK10(sK14)), X4) | p1(sK20(sK10(sK14))) | ~ sP0(sK14)) | ~ spl21_62), inference(resolution, [], [f796, f64])).
fof(f64, plain, ! [X4, X2, X0, X5] : (~ r1(sK10(X0), X2) | ~ p1(X4) | p1(X5) | ~ r1(X4, X5) | ~ r1(X2, X4) | p1(X2) | ~ sP0(X0)), inference(cnf_transformation, [], [f32])).
fof(f1800, plain, (spl21_189 | spl21_192 | spl21_181 | ~ spl21_62), inference(avatar_split_clause, [], [f1703, f794, f1750, f1797, f1784])).
fof(f1703, plain, (! [X11] : (p1(sK20(sK10(sK14))) | p1(sK8(sK20(sK10(sK14)))) | ~ r1(X11, sK10(sK14)) | ~ sP1(X11)) | ~ spl21_62), inference(resolution, [], [f796, f57])).
fof(f57, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p1(X2) | p1(sK8(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f26])).
fof(f1795, plain, (spl21_189 | spl21_191 | spl21_181 | ~ spl21_62), inference(avatar_split_clause, [], [f1702, f794, f1750, f1792, f1784])).
fof(f1702, plain, (! [X10] : (p1(sK20(sK10(sK14))) | r1(sK8(sK20(sK10(sK14))), sK9(sK20(sK10(sK14)))) | ~ r1(X10, sK10(sK14)) | ~ sP1(X10)) | ~ spl21_62), inference(resolution, [], [f796, f55])).
fof(f55, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p1(X2) | r1(sK8(X2), sK9(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f26])).
fof(f1136, plain, (spl21_60 | spl21_61), inference(avatar_split_clause, [], [f1007, f789, f785])).
fof(f1007, plain, ! [X0, X1] : (~ r1(X0, X1) | ~ r1(sK14, X0) | p1(X1) | r1(sK14, sK20(sK14))), inference(resolution, [], [f328, f72])).
fof(f328, plain, ! [X4, X5, X3] : (~ r1(sK13, X5) | ~ r1(X4, X3) | ~ r1(X5, X4) | p1(X3) | r1(X5, sK20(X5))), inference(resolution, [], [f67, f43])).
fof(f67, plain, ! [X14, X12, X8, X15] : (~ r1(sK13, X8) | p1(X15) | ~ r1(X14, X15) | ~ r1(X12, X14) | ~ r1(X8, X12) | r1(X12, sK20(X12))), inference(cnf_transformation, [], [f42])).
fof(f1078, plain, (spl21_94 | spl21_59 | ~ spl21_58), inference(avatar_split_clause, [], [f1074, f777, f781, f1076])).
fof(f777, plain, (spl21_58 <=> r1(sK17, sK20(sK17))), introduced(avatar_definition, [new_symbols(naming, [spl21_58])])).
fof(f1074, plain, (! [X2, X0, X1] : (p1(X0) | ~ r1(X1, X0) | ~ r1(sK17, X1) | ~ r1(X2, sK17) | ~ r1(sK13, X2)) | ~ spl21_58), inference(resolution, [], [f1054, f68])).
fof(f1054, plain, (p1(sK20(sK17)) | ~ spl21_58), inference(resolution, [], [f779, f76])).
fof(f779, plain, (r1(sK17, sK20(sK17)) | ~ spl21_58), inference(avatar_component_clause, [], [f777])).
fof(f880, plain, (~ spl21_27 | spl21_62 | spl21_63), inference(avatar_split_clause, [], [f765, f798, f794, f304])).
fof(f765, plain, ! [X10, X11] : (~ r1(X10, X11) | ~ r1(sK10(sK14), X10) | p1(X11) | r1(sK10(sK14), sK20(sK10(sK14))) | ~ sP0(sK14)), inference(resolution, [], [f327, f58])).
fof(f327, plain, ! [X2, X0, X1] : (~ r1(sK14, X2) | ~ r1(X1, X0) | ~ r1(X2, X1) | p1(X0) | r1(X2, sK20(X2))), inference(resolution, [], [f67, f72])).
fof(f783, plain, (spl21_58 | spl21_59), inference(avatar_split_clause, [], [f761, f781, f777])).
fof(f761, plain, ! [X2, X3] : (~ r1(X2, X3) | ~ r1(sK17, X2) | p1(X3) | r1(sK17, sK20(sK17))), inference(resolution, [], [f327, f75])).
fof(f464, plain, (spl21_27 | ~ spl21_5 | spl21_40), inference(avatar_split_clause, [], [f460, f462, f113, f304])).
fof(f460, plain, ! [X12] : (r1(sK4(X12), sK5(X12)) | ~ r1(sK14, X12) | ~ p1(sK14) | sP0(sK14)), inference(subsumption_resolution, [], [f459, f81])).
fof(f459, plain, ! [X12] : (r1(sK4(X12), sK5(X12)) | ~ r1(sK14, X12) | ~ p1(sK14) | sP0(sK14) | ~ sP3(sK14)), inference(subsumption_resolution, [], [f372, f74])).
fof(f372, plain, ! [X12] : (r1(sK4(X12), sK5(X12)) | ~ r1(sK14, X12) | ~ p1(sK14) | p1(sK18) | sP0(sK14) | ~ sP3(sK14)), inference(resolution, [], [f45, f73])).
fof(f45, plain, ! [X4, X0, X1] : (~ r1(X0, X4) | r1(sK4(X1), sK5(X1)) | ~ r1(X0, X1) | ~ p1(X0) | p1(X4) | sP0(X0) | ~ sP3(X0)), inference(cnf_transformation, [], [f16])).
fof(f403, plain, (spl21_30 | spl21_31 | ~ spl21_24), inference(avatar_split_clause, [], [f387, f251, f400, f396])).
fof(f387, plain, (p1(sK6(sK14)) | p1(sK15(sK6(sK14))) | ~ spl21_24), inference(resolution, [], [f253, f80])).
fof(f264, plain, (spl21_5 | spl21_24 | spl21_26), inference(avatar_split_clause, [], [f259, f261, f251, f113])).
fof(f259, plain, (r1(sK17, sK7(sK17)) | r1(sK14, sK6(sK14)) | p1(sK14)), inference(subsumption_resolution, [], [f235, f83])).
fof(f235, plain, (r1(sK17, sK7(sK17)) | r1(sK14, sK6(sK14)) | p1(sK14) | ~ sP2(sK14)), inference(resolution, [], [f48, f75])).
fof(f48, plain, ! [X4, X0] : (~ r1(X0, X4) | r1(X4, sK7(X4)) | r1(X0, sK6(X0)) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f21])).