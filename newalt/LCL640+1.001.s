fof(f2282, plain, $false, inference(avatar_sat_refutation, [], [f267, f437, f445, f540, f628, f1017, f1366, f1718, f1727, f1925, f1958, f1959, f1967, f1978, f1981, f2089, f2267, f2270, f2281])).
fof(f2281, plain, ~ spl21_63, inference(avatar_contradiction_clause, [], [f2280])).
fof(f2280, plain, ($false | ~ spl21_63), inference(subsumption_resolution, [], [f2273, f73])).
fof(f73, plain, r1(sK14, sK17), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ((! [X2] : ((p1(sK15(X2)) & (~ p1(sK16(X2)) & r1(sK15(X2), sK16(X2))) & r1(X2, sK15(X2))) | p1(X2) | ~ r1(sK14, X2)) & (! [X6] : (p1(X6) | ~ r1(sK17, X6)) & r1(sK14, sK17)) & (~ p1(sK18) & r1(sK14, sK18)) & r1(sK13, sK14)) & ! [X8] : ((((! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(sK19(X8), X10)) & ~ p1(sK19(X8)) & r1(X8, sK19(X8))) | sP1(X8)) & ! [X12] : ((~ p1(sK20(X12)) & r1(X12, sK20(X12))) | ! [X14] : (! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X12, X14)) | ~ r1(X8, X12)) & sP2(X8) & sP3(X8)) | ~ r1(sK13, X8))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14, sK15, sK16, sK17, sK18, sK19, sK20])], [f32, f40, f39, f38, f37, f36, f35, f34, f33])).
fof(f33, plain, (? [X0] : (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(X0, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | sP1(X8)) & ! [X12] : (? [X13] : (~ p1(X13) & r1(X12, X13)) | ! [X14] : (! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X12, X14)) | ~ r1(X8, X12)) & sP2(X8) & sP3(X8)) | ~ r1(X0, X8))) => (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(sK13, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | sP1(X8)) & ! [X12] : (? [X13] : (~ p1(X13) & r1(X12, X13)) | ! [X14] : (! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X12, X14)) | ~ r1(X8, X12)) & sP2(X8) & sP3(X8)) | ~ r1(sK13, X8)))), introduced(choice_axiom, [])).
fof(f34, plain, (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(sK13, X1)) => (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(sK14, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(sK14, X5)) & ? [X7] : (~ p1(X7) & r1(sK14, X7)) & r1(sK13, sK14))), introduced(choice_axiom, [])).
fof(f35, plain, ! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) => (p1(sK15(X2)) & ? [X4] : (~ p1(X4) & r1(sK15(X2), X4)) & r1(X2, sK15(X2)))), introduced(choice_axiom, [])).
fof(f36, plain, ! [X2] : (? [X4] : (~ p1(X4) & r1(sK15(X2), X4)) => (~ p1(sK16(X2)) & r1(sK15(X2), sK16(X2)))), introduced(choice_axiom, [])).
fof(f37, plain, (? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(sK14, X5)) => (! [X6] : (p1(X6) | ~ r1(sK17, X6)) & r1(sK14, sK17))), introduced(choice_axiom, [])).
fof(f38, plain, (? [X7] : (~ p1(X7) & r1(sK14, X7)) => (~ p1(sK18) & r1(sK14, sK18))), introduced(choice_axiom, [])).
fof(f39, plain, ! [X8] : (? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) => (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(sK19(X8), X10)) & ~ p1(sK19(X8)) & r1(X8, sK19(X8)))), introduced(choice_axiom, [])).
fof(f40, plain, ! [X12] : (? [X13] : (~ p1(X13) & r1(X12, X13)) => (~ p1(sK20(X12)) & r1(X12, sK20(X12)))), introduced(choice_axiom, [])).
fof(f32, plain, ? [X0] : (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(X0, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | sP1(X8)) & ! [X12] : (? [X13] : (~ p1(X13) & r1(X12, X13)) | ! [X14] : (! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X12, X14)) | ~ r1(X8, X12)) & sP2(X8) & sP3(X8)) | ~ r1(X0, X8))), inference(rectify, [], [f10])).
fof(f10, plain, ? [X0] : (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(X0, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | sP1(X8)) & ! [X16] : (? [X17] : (~ p1(X17) & r1(X16, X17)) | ! [X18] : (! [X19] : (p1(X19) | ~ r1(X18, X19)) | ~ r1(X16, X18)) | ~ r1(X8, X16)) & sP2(X8) & sP3(X8)) | ~ r1(X0, X8))), inference(definition_folding, [], [f5, e9, e8, e7, e6])).
fof(f6, plain, ! [X8] : (? [X25] : (! [X26] : ((p1(X26) & ? [X27] : (~ p1(X27) & r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) & p1(X25) & ? [X30] : (~ p1(X30) & r1(X25, X30)) & r1(X8, X25)) | ~ sP0(X8)), inference(usedef, [], [e6])).
fof(e6, plain, ! [X8] : (sP0(X8) <=> ? [X25] : (! [X26] : ((p1(X26) & ? [X27] : (~ p1(X27) & r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) & p1(X25) & ? [X30] : (~ p1(X30) & r1(X25, X30)) & r1(X8, X25))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f7, plain, ! [X8] : (! [X12] : (! [X13] : (? [X14] : (p1(X14) & ? [X15] : (~ p1(X15) & r1(X14, X15)) & r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12)) | ~ sP1(X8)), inference(usedef, [], [e7])).
fof(e7, plain, ! [X8] : (sP1(X8) <=> ! [X12] : (! [X13] : (? [X14] : (p1(X14) & ? [X15] : (~ p1(X15) & r1(X14, X15)) & r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f8, plain, ! [X8] : (? [X20] : (! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) & ~ p1(X20) & r1(X8, X20)) | ! [X23] : (? [X24] : (~ p1(X24) & r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8) | ~ sP2(X8)), inference(usedef, [], [e8])).
fof(e8, plain, ! [X8] : (sP2(X8) <=> (? [X20] : (! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) & ~ p1(X20) & r1(X8, X20)) | ! [X23] : (? [X24] : (~ p1(X24) & r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f9, plain, ! [X8] : (sP0(X8) | ! [X31] : (? [X32] : (p1(X32) & ? [X33] : (~ p1(X33) & r1(X32, X33)) & r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)) | ~ sP3(X8)), inference(usedef, [], [e9])).
fof(e9, plain, ! [X8] : (sP3(X8) <=> (sP0(X8) | ! [X31] : (? [X32] : (p1(X32) & ? [X33] : (~ p1(X33) & r1(X32, X33)) & r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f5, plain, ? [X0] : (? [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) & ? [X5] : (! [X6] : (p1(X6) | ~ r1(X5, X6)) & r1(X1, X5)) & ? [X7] : (~ p1(X7) & r1(X1, X7)) & r1(X0, X1)) & ! [X8] : (((? [X9] : (! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) & ~ p1(X9) & r1(X8, X9)) | ! [X12] : (! [X13] : (? [X14] : (p1(X14) & ? [X15] : (~ p1(X15) & r1(X14, X15)) & r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12))) & ! [X16] : (? [X17] : (~ p1(X17) & r1(X16, X17)) | ! [X18] : (! [X19] : (p1(X19) | ~ r1(X18, X19)) | ~ r1(X16, X18)) | ~ r1(X8, X16)) & (? [X20] : (! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) & ~ p1(X20) & r1(X8, X20)) | ! [X23] : (? [X24] : (~ p1(X24) & r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8)) & (? [X25] : (! [X26] : ((p1(X26) & ? [X27] : (~ p1(X27) & r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) & p1(X25) & ? [X30] : (~ p1(X30) & r1(X25, X30)) & r1(X8, X25)) | ! [X31] : (? [X32] : (p1(X32) & ? [X33] : (~ p1(X33) & r1(X32, X33)) & r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)))) | ~ r1(X0, X8))), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ? [X0] : ~ (! [X1] : (~ ! [X2] : (~ ! [X3] : (~ p1(X3) | ! [X4] : (p1(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) | ! [X5] : (~ ! [X6] : (p1(X6) | ~ r1(X5, X6)) | ~ r1(X1, X5)) | ! [X7] : (p1(X7) | ~ r1(X1, X7)) | ~ r1(X0, X1)) | ~ ! [X8] : (((~ ! [X9] : (~ ! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) | p1(X9) | ~ r1(X8, X9)) | ! [X12] : (! [X13] : (~ ! [X14] : (~ p1(X14) | ! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12))) & ! [X16] : (~ ! [X17] : (p1(X17) | ~ r1(X16, X17)) | ! [X18] : (! [X19] : (p1(X19) | ~ r1(X18, X19)) | ~ r1(X16, X18)) | ~ r1(X8, X16)) & (~ ! [X20] : (~ ! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) | p1(X20) | ~ r1(X8, X20)) | ! [X23] : (~ ! [X24] : (p1(X24) | ~ r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8)) & (~ ! [X25] : (~ ! [X26] : (~ (~ p1(X26) | ! [X27] : (p1(X27) | ~ r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) | ~ p1(X25) | ! [X30] : (p1(X30) | ~ r1(X25, X30)) | ~ r1(X8, X25)) | ! [X31] : (~ ! [X32] : (~ p1(X32) | ! [X33] : (p1(X33) | ~ r1(X32, X33)) | ~ r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)))) | ~ r1(X0, X8))), inference(flattening, [], [f3])).
fof(f3, plain, ~ ~ ? [X0] : ~ (~ ~ ! [X1] : (~ ! [X2] : (~ ! [X3] : (~ p1(X3) | ! [X4] : (p1(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) | ! [X5] : (~ ! [X6] : (p1(X6) | ~ r1(X5, X6)) | ~ r1(X1, X5)) | ! [X7] : (p1(X7) | ~ r1(X1, X7)) | ~ r1(X0, X1)) | ~ ! [X8] : (((~ ! [X9] : (~ ! [X10] : (~ p1(X10) | ! [X11] : (p1(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) | p1(X9) | ~ r1(X8, X9)) | ! [X12] : (! [X13] : (~ ! [X14] : (~ p1(X14) | ! [X15] : (p1(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12))) & ! [X16] : (~ ! [X17] : (p1(X17) | ~ r1(X16, X17)) | ! [X18] : (! [X19] : (p1(X19) | ~ r1(X18, X19)) | ~ r1(X16, X18)) | ~ r1(X8, X16)) & (~ ! [X20] : (~ ! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) | p1(X20) | ~ r1(X8, X20)) | ! [X23] : (~ ! [X24] : (p1(X24) | ~ r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8)) & (~ ! [X25] : (~ ! [X26] : (~ (~ p1(X26) | ! [X27] : (p1(X27) | ~ r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) | ~ p1(X25) | ! [X30] : (p1(X30) | ~ r1(X25, X30)) | ~ r1(X8, X25)) | ! [X31] : (~ ! [X32] : (~ p1(X32) | ! [X33] : (p1(X33) | ~ r1(X32, X33)) | ~ r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)))) | ~ r1(X0, X8))), inference(rectify, [], [f2])).
fof(f2, plain, ~ ~ ? [X0] : ~ (~ ~ ! [X1] : (~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ ! [X1] : (((~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (! [X1] : (~ ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0))) & ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ! [X1] : (! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ r1(X1, X0)) & (~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1)) & (~ ! [X0] : (~ ! [X1] : (~ (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0))) | ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)))) | ~ r1(X0, X1))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ ~ ? [X0] : ~ (~ ~ ! [X1] : (~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ ! [X1] : (((~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (! [X1] : (~ ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0))) & ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ! [X1] : (! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ r1(X1, X0)) & (~ ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p1(X0) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1)) & (~ ! [X0] : (~ ! [X1] : (~ (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0))) | ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ! [X0] : (~ ! [X1] : (~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ~ p1(X1) | ! [X0] : (p1(X0) | ~ r1(X1, X0)))) | ~ r1(X0, X1))), file('/home/ubuntu/library/tptp/Problems/LCL/LCL640+1.001.p', main)).
fof(f2273, plain, (~ r1(sK14, sK17) | ~ spl21_63), inference(resolution, [], [f627, f70])).
fof(f70, plain, r1(sK13, sK14), inference(cnf_transformation, [], [f41])).
fof(f627, plain, (! [X2] : (~ r1(sK13, X2) | ~ r1(X2, sK17)) | ~ spl21_63), inference(avatar_component_clause, [], [f626])).
fof(f626, plain, (spl21_63 <=> ! [X2] : (~ r1(X2, sK17) | ~ r1(sK13, X2))), introduced(avatar_definition, [new_symbols(naming, [spl21_63])])).
fof(f2270, plain, (spl21_21 | spl21_144 | ~ spl21_28 | ~ spl21_35 | spl21_160), inference(avatar_split_clause, [], [f2269, f1922, f359, f254, f1850, f197])).
fof(f197, plain, (spl21_21 <=> p1(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl21_21])])).
fof(f1850, plain, (spl21_144 <=> ! [X1] : (r1(X1, sK7(X1)) | ~ r1(sK14, X1))), introduced(avatar_definition, [new_symbols(naming, [spl21_144])])).
fof(f254, plain, (spl21_28 <=> r1(sK14, sK6(sK14))), introduced(avatar_definition, [new_symbols(naming, [spl21_28])])).
fof(f359, plain, (spl21_35 <=> p1(sK15(sK6(sK14)))), introduced(avatar_definition, [new_symbols(naming, [spl21_35])])).
fof(f1922, plain, (spl21_160 <=> p1(sK16(sK6(sK14)))), introduced(avatar_definition, [new_symbols(naming, [spl21_160])])).
fof(f2269, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14)) | (~ spl21_28 | ~ spl21_35 | spl21_160)), inference(subsumption_resolution, [], [f2268, f256])).
fof(f256, plain, (r1(sK14, sK6(sK14)) | ~ spl21_28), inference(avatar_component_clause, [], [f254])).
fof(f2268, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ r1(sK14, sK6(sK14))) | (~ spl21_35 | spl21_160)), inference(subsumption_resolution, [], [f2121, f80])).
fof(f80, plain, sP2(sK14), inference(resolution, [], [f64, f70])).
fof(f64, plain, ! [X8] : (~ r1(sK13, X8) | sP2(X8)), inference(cnf_transformation, [], [f41])).
fof(f2121, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ sP2(sK14) | ~ r1(sK14, sK6(sK14))) | (~ spl21_35 | spl21_160)), inference(subsumption_resolution, [], [f2119, f1923])).
fof(f1923, plain, (~ p1(sK16(sK6(sK14))) | spl21_160), inference(avatar_component_clause, [], [f1922])).
fof(f2119, plain, (! [X0] : (p1(sK16(sK6(sK14))) | r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ sP2(sK14) | ~ r1(sK14, sK6(sK14))) | ~ spl21_35), inference(resolution, [], [f361, f977])).
fof(f977, plain, ! [X0, X1] : (~ p1(sK15(sK6(X0))) | p1(sK16(sK6(X0))) | r1(X1, sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0))), inference(subsumption_resolution, [], [f976, f48])).
fof(f48, plain, ! [X4, X0] : (~ p1(sK6(X0)) | r1(X4, sK7(X4)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f20])).
fof(f20, plain, ! [X0] : ((! [X2] : (~ p1(X2) | ! [X3] : (p1(X3) | ~ r1(X2, X3)) | ~ r1(sK6(X0), X2)) & ~ p1(sK6(X0)) & r1(X0, sK6(X0))) | ! [X4] : ((~ p1(sK7(X4)) & r1(X4, sK7(X4))) | ~ r1(X0, X4)) | p1(X0) | ~ sP2(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK6, sK7])], [f17, f19, f18])).
fof(f18, plain, ! [X0] : (? [X1] : (! [X2] : (~ p1(X2) | ! [X3] : (p1(X3) | ~ r1(X2, X3)) | ~ r1(X1, X2)) & ~ p1(X1) & r1(X0, X1)) => (! [X2] : (~ p1(X2) | ! [X3] : (p1(X3) | ~ r1(X2, X3)) | ~ r1(sK6(X0), X2)) & ~ p1(sK6(X0)) & r1(X0, sK6(X0)))), introduced(choice_axiom, [])).
fof(f19, plain, ! [X4] : (? [X5] : (~ p1(X5) & r1(X4, X5)) => (~ p1(sK7(X4)) & r1(X4, sK7(X4)))), introduced(choice_axiom, [])).
fof(f17, plain, ! [X0] : (? [X1] : (! [X2] : (~ p1(X2) | ! [X3] : (p1(X3) | ~ r1(X2, X3)) | ~ r1(X1, X2)) & ~ p1(X1) & r1(X0, X1)) | ! [X4] : (? [X5] : (~ p1(X5) & r1(X4, X5)) | ~ r1(X0, X4)) | p1(X0) | ~ sP2(X0)), inference(rectify, [], [f16])).
fof(f16, plain, ! [X8] : (? [X20] : (! [X21] : (~ p1(X21) | ! [X22] : (p1(X22) | ~ r1(X21, X22)) | ~ r1(X20, X21)) & ~ p1(X20) & r1(X8, X20)) | ! [X23] : (? [X24] : (~ p1(X24) & r1(X23, X24)) | ~ r1(X8, X23)) | p1(X8) | ~ sP2(X8)), inference(nnf_transformation, [], [f8])).
fof(f976, plain, ! [X0, X1] : (p1(sK16(sK6(X0))) | ~ p1(sK15(sK6(X0))) | r1(X1, sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0)) | p1(sK6(X0))), inference(duplicate_literal_removal, [], [f970])).
fof(f970, plain, ! [X0, X1] : (p1(sK16(sK6(X0))) | ~ p1(sK15(sK6(X0))) | r1(X1, sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0)) | p1(sK6(X0)) | ~ r1(sK14, sK6(X0))), inference(resolution, [], [f465, f76])).
fof(f76, plain, ! [X2] : (r1(sK15(X2), sK16(X2)) | p1(X2) | ~ r1(sK14, X2)), inference(cnf_transformation, [], [f41])).
fof(f465, plain, ! [X6, X8, X7] : (~ r1(sK15(sK6(X7)), X6) | p1(X6) | ~ p1(sK15(sK6(X7))) | r1(X8, sK7(X8)) | ~ r1(X7, X8) | p1(X7) | ~ sP2(X7) | ~ r1(sK14, sK6(X7))), inference(subsumption_resolution, [], [f463, f48])).
fof(f463, plain, ! [X6, X8, X7] : (p1(X6) | ~ r1(sK15(sK6(X7)), X6) | ~ p1(sK15(sK6(X7))) | r1(X8, sK7(X8)) | ~ r1(X7, X8) | p1(X7) | ~ sP2(X7) | p1(sK6(X7)) | ~ r1(sK14, sK6(X7))), inference(resolution, [], [f50, f75])).
fof(f75, plain, ! [X2] : (r1(X2, sK15(X2)) | p1(X2) | ~ r1(sK14, X2)), inference(cnf_transformation, [], [f41])).
fof(f50, plain, ! [X4, X2, X0, X3] : (~ r1(sK6(X0), X2) | p1(X3) | ~ r1(X2, X3) | ~ p1(X2) | r1(X4, sK7(X4)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f20])).
fof(f361, plain, (p1(sK15(sK6(sK14))) | ~ spl21_35), inference(avatar_component_clause, [], [f359])).
fof(f2267, plain, (~ spl21_30 | ~ spl21_147), inference(avatar_contradiction_clause, [], [f2266])).
fof(f2266, plain, ($false | (~ spl21_30 | ~ spl21_147)), inference(subsumption_resolution, [], [f2265, f73])).
fof(f2265, plain, (~ r1(sK14, sK17) | (~ spl21_30 | ~ spl21_147)), inference(resolution, [], [f1864, f2134])).
fof(f2134, plain, (p1(sK7(sK17)) | ~ spl21_30), inference(resolution, [], [f266, f74])).
fof(f74, plain, ! [X6] : (~ r1(sK17, X6) | p1(X6)), inference(cnf_transformation, [], [f41])).
fof(f266, plain, (r1(sK17, sK7(sK17)) | ~ spl21_30), inference(avatar_component_clause, [], [f264])).
fof(f264, plain, (spl21_30 <=> r1(sK17, sK7(sK17))), introduced(avatar_definition, [new_symbols(naming, [spl21_30])])).
fof(f1864, plain, (! [X3] : (~ p1(sK7(X3)) | ~ r1(sK14, X3)) | ~ spl21_147), inference(avatar_component_clause, [], [f1863])).
fof(f1863, plain, (spl21_147 <=> ! [X3] : (~ p1(sK7(X3)) | ~ r1(sK14, X3))), introduced(avatar_definition, [new_symbols(naming, [spl21_147])])).
fof(f2089, plain, (spl21_30 | ~ spl21_144), inference(avatar_contradiction_clause, [], [f2088])).
fof(f2088, plain, ($false | (spl21_30 | ~ spl21_144)), inference(subsumption_resolution, [], [f2031, f73])).
fof(f2031, plain, (~ r1(sK14, sK17) | (spl21_30 | ~ spl21_144)), inference(resolution, [], [f1851, f265])).
fof(f265, plain, (~ r1(sK17, sK7(sK17)) | spl21_30), inference(avatar_component_clause, [], [f264])).
fof(f1851, plain, (! [X1] : (r1(X1, sK7(X1)) | ~ r1(sK14, X1)) | ~ spl21_144), inference(avatar_component_clause, [], [f1850])).
fof(f1981, plain, (spl21_144 | spl21_21 | ~ spl21_36), inference(avatar_split_clause, [], [f1980, f363, f197, f1850])).
fof(f363, plain, (spl21_36 <=> p1(sK6(sK14))), introduced(avatar_definition, [new_symbols(naming, [spl21_36])])).
fof(f1980, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0)) | (spl21_21 | ~ spl21_36)), inference(subsumption_resolution, [], [f1979, f80])).
fof(f1979, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0) | ~ sP2(sK14)) | (spl21_21 | ~ spl21_36)), inference(subsumption_resolution, [], [f1970, f198])).
fof(f198, plain, (~ p1(sK14) | spl21_21), inference(avatar_component_clause, [], [f197])).
fof(f1970, plain, (! [X0] : (r1(X0, sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ sP2(sK14)) | ~ spl21_36), inference(resolution, [], [f365, f48])).
fof(f365, plain, (p1(sK6(sK14)) | ~ spl21_36), inference(avatar_component_clause, [], [f363])).
fof(f1978, plain, (spl21_21 | ~ spl21_30 | ~ spl21_36), inference(avatar_contradiction_clause, [], [f1977])).
fof(f1977, plain, ($false | (spl21_21 | ~ spl21_30 | ~ spl21_36)), inference(subsumption_resolution, [], [f1976, f80])).
fof(f1976, plain, (~ sP2(sK14) | (spl21_21 | ~ spl21_30 | ~ spl21_36)), inference(subsumption_resolution, [], [f1975, f198])).
fof(f1975, plain, (p1(sK14) | ~ sP2(sK14) | (~ spl21_30 | ~ spl21_36)), inference(subsumption_resolution, [], [f1969, f73])).
fof(f1969, plain, (~ r1(sK14, sK17) | p1(sK14) | ~ sP2(sK14) | (~ spl21_30 | ~ spl21_36)), inference(resolution, [], [f365, f496])).
fof(f496, plain, (! [X1] : (~ p1(sK6(X1)) | ~ r1(X1, sK17) | p1(X1) | ~ sP2(X1)) | ~ spl21_30), inference(resolution, [], [f466, f49])).
fof(f49, plain, ! [X4, X0] : (~ p1(sK7(X4)) | ~ p1(sK6(X0)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f20])).
fof(f466, plain, (p1(sK7(sK17)) | ~ spl21_30), inference(resolution, [], [f266, f74])).
fof(f1967, plain, (spl21_35 | spl21_36 | spl21_21 | ~ spl21_30), inference(avatar_split_clause, [], [f1966, f264, f197, f363, f359])).
fof(f1966, plain, (p1(sK6(sK14)) | p1(sK15(sK6(sK14))) | (spl21_21 | ~ spl21_30)), inference(subsumption_resolution, [], [f1965, f80])).
fof(f1965, plain, (~ sP2(sK14) | p1(sK6(sK14)) | p1(sK15(sK6(sK14))) | (spl21_21 | ~ spl21_30)), inference(subsumption_resolution, [], [f1964, f198])).
fof(f1964, plain, (p1(sK14) | ~ sP2(sK14) | p1(sK6(sK14)) | p1(sK15(sK6(sK14))) | ~ spl21_30), inference(subsumption_resolution, [], [f1307, f73])).
fof(f1307, plain, (~ r1(sK14, sK17) | p1(sK14) | ~ sP2(sK14) | p1(sK6(sK14)) | p1(sK15(sK6(sK14))) | ~ spl21_30), inference(resolution, [], [f495, f78])).
fof(f78, plain, ! [X2] : (~ r1(sK14, X2) | p1(X2) | p1(sK15(X2))), inference(cnf_transformation, [], [f41])).
fof(f495, plain, (! [X0] : (r1(X0, sK6(X0)) | ~ r1(X0, sK17) | p1(X0) | ~ sP2(X0)) | ~ spl21_30), inference(resolution, [], [f466, f47])).
fof(f47, plain, ! [X4, X0] : (~ p1(sK7(X4)) | r1(X0, sK6(X0)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f20])).
fof(f1959, plain, (spl21_35 | spl21_36 | ~ spl21_28), inference(avatar_split_clause, [], [f1766, f254, f363, f359])).
fof(f1766, plain, (p1(sK6(sK14)) | p1(sK15(sK6(sK14))) | ~ spl21_28), inference(resolution, [], [f256, f78])).
fof(f1958, plain, (~ spl21_28 | spl21_36 | ~ spl21_160), inference(avatar_contradiction_clause, [], [f1957])).
fof(f1957, plain, ($false | (~ spl21_28 | spl21_36 | ~ spl21_160)), inference(subsumption_resolution, [], [f1956, f256])).
fof(f1956, plain, (~ r1(sK14, sK6(sK14)) | (spl21_36 | ~ spl21_160)), inference(subsumption_resolution, [], [f1955, f364])).
fof(f364, plain, (~ p1(sK6(sK14)) | spl21_36), inference(avatar_component_clause, [], [f363])).
fof(f1955, plain, (p1(sK6(sK14)) | ~ r1(sK14, sK6(sK14)) | ~ spl21_160), inference(resolution, [], [f1924, f77])).
fof(f77, plain, ! [X2] : (~ p1(sK16(X2)) | p1(X2) | ~ r1(sK14, X2)), inference(cnf_transformation, [], [f41])).
fof(f1924, plain, (p1(sK16(sK6(sK14))) | ~ spl21_160), inference(avatar_component_clause, [], [f1922])).
fof(f1925, plain, (spl21_147 | spl21_160 | spl21_21 | ~ spl21_28 | ~ spl21_35), inference(avatar_split_clause, [], [f1920, f359, f254, f197, f1922, f1863])).
fof(f1920, plain, (! [X0] : (p1(sK16(sK6(sK14))) | ~ p1(sK7(X0)) | ~ r1(sK14, X0)) | (spl21_21 | ~ spl21_28 | ~ spl21_35)), inference(subsumption_resolution, [], [f1919, f256])).
fof(f1919, plain, (! [X0] : (p1(sK16(sK6(sK14))) | ~ p1(sK7(X0)) | ~ r1(sK14, X0) | ~ r1(sK14, sK6(sK14))) | (spl21_21 | ~ spl21_35)), inference(subsumption_resolution, [], [f1918, f80])).
fof(f1918, plain, (! [X0] : (p1(sK16(sK6(sK14))) | ~ p1(sK7(X0)) | ~ r1(sK14, X0) | ~ sP2(sK14) | ~ r1(sK14, sK6(sK14))) | (spl21_21 | ~ spl21_35)), inference(subsumption_resolution, [], [f1917, f198])).
fof(f1917, plain, (! [X0] : (p1(sK16(sK6(sK14))) | ~ p1(sK7(X0)) | ~ r1(sK14, X0) | p1(sK14) | ~ sP2(sK14) | ~ r1(sK14, sK6(sK14))) | ~ spl21_35), inference(resolution, [], [f946, f361])).
fof(f946, plain, ! [X0, X1] : (~ p1(sK15(sK6(X0))) | p1(sK16(sK6(X0))) | ~ p1(sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0))), inference(subsumption_resolution, [], [f945, f49])).
fof(f945, plain, ! [X0, X1] : (p1(sK16(sK6(X0))) | ~ p1(sK15(sK6(X0))) | ~ p1(sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0)) | p1(sK6(X0))), inference(duplicate_literal_removal, [], [f939])).
fof(f939, plain, ! [X0, X1] : (p1(sK16(sK6(X0))) | ~ p1(sK15(sK6(X0))) | ~ p1(sK7(X1)) | ~ r1(X0, X1) | p1(X0) | ~ sP2(X0) | ~ r1(sK14, sK6(X0)) | p1(sK6(X0)) | ~ r1(sK14, sK6(X0))), inference(resolution, [], [f460, f76])).
fof(f460, plain, ! [X6, X8, X7] : (~ r1(sK15(sK6(X7)), X6) | p1(X6) | ~ p1(sK15(sK6(X7))) | ~ p1(sK7(X8)) | ~ r1(X7, X8) | p1(X7) | ~ sP2(X7) | ~ r1(sK14, sK6(X7))), inference(subsumption_resolution, [], [f458, f49])).
fof(f458, plain, ! [X6, X8, X7] : (p1(X6) | ~ r1(sK15(sK6(X7)), X6) | ~ p1(sK15(sK6(X7))) | ~ p1(sK7(X8)) | ~ r1(X7, X8) | p1(X7) | ~ sP2(X7) | p1(sK6(X7)) | ~ r1(sK14, sK6(X7))), inference(resolution, [], [f51, f75])).
fof(f51, plain, ! [X4, X2, X0, X3] : (~ r1(sK6(X0), X2) | p1(X3) | ~ r1(X2, X3) | ~ p1(X2) | ~ p1(sK7(X4)) | ~ r1(X0, X4) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f20])).
fof(f1727, plain, (~ spl21_31 | ~ spl21_14), inference(avatar_split_clause, [], [f1607, f153, f307])).
fof(f307, plain, (spl21_31 <=> sP0(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl21_31])])).
fof(f153, plain, (spl21_14 <=> sP1(sK14)), introduced(avatar_definition, [new_symbols(naming, [spl21_14])])).
fof(f1607, plain, (~ sP0(sK14) | ~ spl21_14), inference(resolution, [], [f1594, f155])).
fof(f155, plain, (sP1(sK14) | ~ spl21_14), inference(avatar_component_clause, [], [f153])).
fof(f1594, plain, ! [X0] : (~ sP1(X0) | ~ sP0(X0)), inference(duplicate_literal_removal, [], [f1593])).
fof(f1593, plain, ! [X0] : (~ sP0(X0) | ~ sP1(X0) | ~ sP1(X0) | ~ sP0(X0)), inference(resolution, [], [f1575, f56])).
fof(f56, plain, ! [X0] : (r1(X0, sK10(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f31])).
fof(f31, plain, ! [X0] : ((! [X2] : ((p1(X2) & (~ p1(sK11(X2)) & r1(X2, sK11(X2)))) | ! [X4] : (~ p1(X4) | ! [X5] : (p1(X5) | ~ r1(X4, X5)) | ~ r1(X2, X4)) | ~ r1(sK10(X0), X2)) & p1(sK10(X0)) & (~ p1(sK12(X0)) & r1(sK10(X0), sK12(X0))) & r1(X0, sK10(X0))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11, sK12])], [f27, f30, f29, f28])).
fof(f28, plain, ! [X0] : (? [X1] : (! [X2] : ((p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3))) | ! [X4] : (~ p1(X4) | ! [X5] : (p1(X5) | ~ r1(X4, X5)) | ~ r1(X2, X4)) | ~ r1(X1, X2)) & p1(X1) & ? [X6] : (~ p1(X6) & r1(X1, X6)) & r1(X0, X1)) => (! [X2] : ((p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3))) | ! [X4] : (~ p1(X4) | ! [X5] : (p1(X5) | ~ r1(X4, X5)) | ~ r1(X2, X4)) | ~ r1(sK10(X0), X2)) & p1(sK10(X0)) & ? [X6] : (~ p1(X6) & r1(sK10(X0), X6)) & r1(X0, sK10(X0)))), introduced(choice_axiom, [])).
fof(f29, plain, ! [X2] : (? [X3] : (~ p1(X3) & r1(X2, X3)) => (~ p1(sK11(X2)) & r1(X2, sK11(X2)))), introduced(choice_axiom, [])).
fof(f30, plain, ! [X0] : (? [X6] : (~ p1(X6) & r1(sK10(X0), X6)) => (~ p1(sK12(X0)) & r1(sK10(X0), sK12(X0)))), introduced(choice_axiom, [])).
fof(f27, plain, ! [X0] : (? [X1] : (! [X2] : ((p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3))) | ! [X4] : (~ p1(X4) | ! [X5] : (p1(X5) | ~ r1(X4, X5)) | ~ r1(X2, X4)) | ~ r1(X1, X2)) & p1(X1) & ? [X6] : (~ p1(X6) & r1(X1, X6)) & r1(X0, X1)) | ~ sP0(X0)), inference(rectify, [], [f26])).
fof(f26, plain, ! [X8] : (? [X25] : (! [X26] : ((p1(X26) & ? [X27] : (~ p1(X27) & r1(X26, X27))) | ! [X28] : (~ p1(X28) | ! [X29] : (p1(X29) | ~ r1(X28, X29)) | ~ r1(X26, X28)) | ~ r1(X25, X26)) & p1(X25) & ? [X30] : (~ p1(X30) & r1(X25, X30)) & r1(X8, X25)) | ~ sP0(X8)), inference(nnf_transformation, [], [f6])).
fof(f1575, plain, ! [X0, X1] : (~ r1(X1, sK10(X0)) | ~ sP0(X0) | ~ sP1(X0) | ~ sP1(X1)), inference(duplicate_literal_removal, [], [f1574])).
fof(f1574, plain, ! [X0, X1] : (~ sP1(X0) | ~ sP0(X0) | ~ r1(X1, sK10(X0)) | ~ sP1(X1) | ~ sP0(X0)), inference(resolution, [], [f1546, f57])).
fof(f57, plain, ! [X0] : (r1(sK10(X0), sK12(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f31])).
fof(f1546, plain, ! [X2, X0, X1] : (~ r1(X1, sK12(X0)) | ~ sP1(X0) | ~ sP0(X0) | ~ r1(X2, X1) | ~ sP1(X2)), inference(subsumption_resolution, [], [f1545, f58])).
fof(f58, plain, ! [X0] : (~ p1(sK12(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f31])).
fof(f1545, plain, ! [X2, X0, X1] : (~ sP0(X0) | ~ sP1(X0) | p1(sK12(X0)) | ~ r1(X1, sK12(X0)) | ~ r1(X2, X1) | ~ sP1(X2)), inference(resolution, [], [f1538, f54])).
fof(f54, plain, ! [X2, X0, X1] : (~ p1(sK9(X2)) | p1(X2) | ~ r1(X1, X2) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f25])).
fof(f25, plain, ! [X0] : (! [X1] : (! [X2] : ((p1(sK8(X2)) & (~ p1(sK9(X2)) & r1(sK8(X2), sK9(X2))) & r1(X2, sK8(X2))) | p1(X2) | ~ r1(X1, X2)) | ~ r1(X0, X1)) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK8, sK9])], [f22, f24, f23])).
fof(f23, plain, ! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) => (p1(sK8(X2)) & ? [X4] : (~ p1(X4) & r1(sK8(X2), X4)) & r1(X2, sK8(X2)))), introduced(choice_axiom, [])).
fof(f24, plain, ! [X2] : (? [X4] : (~ p1(X4) & r1(sK8(X2), X4)) => (~ p1(sK9(X2)) & r1(sK8(X2), sK9(X2)))), introduced(choice_axiom, [])).
fof(f22, plain, ! [X0] : (! [X1] : (! [X2] : (? [X3] : (p1(X3) & ? [X4] : (~ p1(X4) & r1(X3, X4)) & r1(X2, X3)) | p1(X2) | ~ r1(X1, X2)) | ~ r1(X0, X1)) | ~ sP1(X0)), inference(rectify, [], [f21])).
fof(f21, plain, ! [X8] : (! [X12] : (! [X13] : (? [X14] : (p1(X14) & ? [X15] : (~ p1(X15) & r1(X14, X15)) & r1(X13, X14)) | p1(X13) | ~ r1(X12, X13)) | ~ r1(X8, X12)) | ~ sP1(X8)), inference(nnf_transformation, [], [f7])).
fof(f1538, plain, ! [X0] : (p1(sK9(sK12(X0))) | ~ sP0(X0) | ~ sP1(X0)), inference(duplicate_literal_removal, [], [f1530])).
fof(f1530, plain, ! [X0] : (~ sP0(X0) | p1(sK9(sK12(X0))) | ~ sP1(X0) | ~ sP1(X0) | ~ sP0(X0)), inference(resolution, [], [f508, f598])).
fof(f598, plain, ! [X0] : (r1(sK8(sK12(X0)), sK9(sK12(X0))) | ~ sP1(X0) | ~ sP0(X0)), inference(duplicate_literal_removal, [], [f597])).
fof(f597, plain, ! [X0] : (r1(sK8(sK12(X0)), sK9(sK12(X0))) | ~ sP1(X0) | ~ sP0(X0) | ~ sP0(X0)), inference(resolution, [], [f277, f56])).
fof(f277, plain, ! [X6, X7] : (~ r1(X7, sK10(X6)) | r1(sK8(sK12(X6)), sK9(sK12(X6))) | ~ sP1(X7) | ~ sP0(X6)), inference(subsumption_resolution, [], [f271, f58])).
fof(f271, plain, ! [X6, X7] : (p1(sK12(X6)) | r1(sK8(sK12(X6)), sK9(sK12(X6))) | ~ r1(X7, sK10(X6)) | ~ sP1(X7) | ~ sP0(X6)), inference(resolution, [], [f53, f57])).
fof(f53, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p1(X2) | r1(sK8(X2), sK9(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f25])).
fof(f508, plain, ! [X0, X1] : (~ r1(sK8(sK12(X0)), X1) | ~ sP0(X0) | p1(X1) | ~ sP1(X0)), inference(subsumption_resolution, [], [f507, f286])).
fof(f286, plain, ! [X0] : (p1(sK8(sK12(X0))) | ~ sP1(X0) | ~ sP0(X0)), inference(duplicate_literal_removal, [], [f285])).
fof(f285, plain, ! [X0] : (p1(sK8(sK12(X0))) | ~ sP1(X0) | ~ sP0(X0) | ~ sP0(X0)), inference(resolution, [], [f188, f56])).
fof(f188, plain, ! [X6, X7] : (~ r1(X7, sK10(X6)) | p1(sK8(sK12(X6))) | ~ sP1(X7) | ~ sP0(X6)), inference(subsumption_resolution, [], [f182, f58])).
fof(f182, plain, ! [X6, X7] : (p1(sK12(X6)) | p1(sK8(sK12(X6))) | ~ r1(X7, sK10(X6)) | ~ sP1(X7) | ~ sP0(X6)), inference(resolution, [], [f55, f57])).
fof(f55, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p1(X2) | p1(sK8(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f25])).
fof(f507, plain, ! [X0, X1] : (~ sP1(X0) | ~ sP0(X0) | p1(X1) | ~ r1(sK8(sK12(X0)), X1) | ~ p1(sK8(sK12(X0)))), inference(duplicate_literal_removal, [], [f499])).
fof(f499, plain, ! [X0, X1] : (~ sP1(X0) | ~ sP0(X0) | p1(X1) | ~ r1(sK8(sK12(X0)), X1) | ~ p1(sK8(sK12(X0))) | ~ sP0(X0)), inference(resolution, [], [f494, f372])).
fof(f372, plain, ! [X2, X0, X1] : (~ r1(sK12(X2), X0) | p1(X1) | ~ r1(X0, X1) | ~ p1(X0) | ~ sP0(X2)), inference(subsumption_resolution, [], [f371, f58])).
fof(f371, plain, ! [X2, X0, X1] : (~ p1(X0) | p1(X1) | ~ r1(X0, X1) | ~ r1(sK12(X2), X0) | p1(sK12(X2)) | ~ sP0(X2)), inference(duplicate_literal_removal, [], [f367])).
fof(f367, plain, ! [X2, X0, X1] : (~ p1(X0) | p1(X1) | ~ r1(X0, X1) | ~ r1(sK12(X2), X0) | p1(sK12(X2)) | ~ sP0(X2) | ~ sP0(X2)), inference(resolution, [], [f62, f57])).
fof(f62, plain, ! [X4, X2, X0, X5] : (~ r1(sK10(X0), X2) | ~ p1(X4) | p1(X5) | ~ r1(X4, X5) | ~ r1(X2, X4) | p1(X2) | ~ sP0(X0)), inference(cnf_transformation, [], [f31])).
fof(f494, plain, ! [X0] : (r1(sK12(X0), sK8(sK12(X0))) | ~ sP1(X0) | ~ sP0(X0)), inference(duplicate_literal_removal, [], [f493])).
fof(f493, plain, ! [X0] : (r1(sK12(X0), sK8(sK12(X0))) | ~ sP1(X0) | ~ sP0(X0) | ~ sP0(X0)), inference(resolution, [], [f227, f56])).
fof(f227, plain, ! [X6, X7] : (~ r1(X7, sK10(X6)) | r1(sK12(X6), sK8(sK12(X6))) | ~ sP1(X7) | ~ sP0(X6)), inference(subsumption_resolution, [], [f221, f58])).
fof(f221, plain, ! [X6, X7] : (p1(sK12(X6)) | r1(sK12(X6), sK8(sK12(X6))) | ~ r1(X7, sK10(X6)) | ~ sP1(X7) | ~ sP0(X6)), inference(resolution, [], [f52, f57])).
fof(f52, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p1(X2) | r1(X2, sK8(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f25])).
fof(f1718, plain, (~ spl21_21 | spl21_31 | ~ spl21_45 | ~ spl21_46 | ~ spl21_53), inference(avatar_contradiction_clause, [], [f1717])).
fof(f1717, plain, ($false | (~ spl21_21 | spl21_31 | ~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(subsumption_resolution, [], [f1707, f72])).
fof(f72, plain, ~ p1(sK18), inference(cnf_transformation, [], [f41])).
fof(f1707, plain, (p1(sK18) | (~ spl21_21 | spl21_31 | ~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(resolution, [], [f1650, f71])).
fof(f71, plain, r1(sK14, sK18), inference(cnf_transformation, [], [f41])).
fof(f1650, plain, (! [X0] : (~ r1(sK14, X0) | p1(X0)) | (~ spl21_21 | spl21_31 | ~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(subsumption_resolution, [], [f1649, f79])).
fof(f79, plain, sP3(sK14), inference(resolution, [], [f63, f70])).
fof(f63, plain, ! [X8] : (~ r1(sK13, X8) | sP3(X8)), inference(cnf_transformation, [], [f41])).
fof(f1649, plain, (! [X0] : (p1(X0) | ~ r1(sK14, X0) | ~ sP3(sK14)) | (~ spl21_21 | spl21_31 | ~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(subsumption_resolution, [], [f1648, f199])).
fof(f199, plain, (p1(sK14) | ~ spl21_21), inference(avatar_component_clause, [], [f197])).
fof(f1648, plain, (! [X0] : (~ p1(sK14) | p1(X0) | ~ r1(sK14, X0) | ~ sP3(sK14)) | (spl21_31 | ~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(subsumption_resolution, [], [f1647, f308])).
fof(f308, plain, (~ sP0(sK14) | spl21_31), inference(avatar_component_clause, [], [f307])).
fof(f1647, plain, (! [X0] : (sP0(sK14) | ~ p1(sK14) | p1(X0) | ~ r1(sK14, X0) | ~ sP3(sK14)) | (~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(resolution, [], [f1459, f73])).
fof(f1459, plain, (! [X0, X1] : (~ r1(X0, sK17) | sP0(X0) | ~ p1(X0) | p1(X1) | ~ r1(X0, X1) | ~ sP3(X0)) | (~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(resolution, [], [f1443, f44])).
fof(f44, plain, ! [X4, X0, X1] : (~ p1(sK5(X1)) | sP0(X0) | ~ r1(X0, X1) | ~ p1(X0) | p1(X4) | ~ r1(X0, X4) | ~ sP3(X0)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ! [X0] : (sP0(X0) | ! [X1] : ((p1(sK4(X1)) & (~ p1(sK5(X1)) & r1(sK4(X1), sK5(X1))) & r1(X1, sK4(X1))) | ~ r1(X0, X1)) | ~ p1(X0) | ! [X4] : (p1(X4) | ~ r1(X0, X4)) | ~ sP3(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK4, sK5])], [f12, f14, f13])).
fof(f13, plain, ! [X1] : (? [X2] : (p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3)) & r1(X1, X2)) => (p1(sK4(X1)) & ? [X3] : (~ p1(X3) & r1(sK4(X1), X3)) & r1(X1, sK4(X1)))), introduced(choice_axiom, [])).
fof(f14, plain, ! [X1] : (? [X3] : (~ p1(X3) & r1(sK4(X1), X3)) => (~ p1(sK5(X1)) & r1(sK4(X1), sK5(X1)))), introduced(choice_axiom, [])).
fof(f12, plain, ! [X0] : (sP0(X0) | ! [X1] : (? [X2] : (p1(X2) & ? [X3] : (~ p1(X3) & r1(X2, X3)) & r1(X1, X2)) | ~ r1(X0, X1)) | ~ p1(X0) | ! [X4] : (p1(X4) | ~ r1(X0, X4)) | ~ sP3(X0)), inference(rectify, [], [f11])).
fof(f11, plain, ! [X8] : (sP0(X8) | ! [X31] : (? [X32] : (p1(X32) & ? [X33] : (~ p1(X33) & r1(X32, X33)) & r1(X31, X32)) | ~ r1(X8, X31)) | ~ p1(X8) | ! [X34] : (p1(X34) | ~ r1(X8, X34)) | ~ sP3(X8)), inference(nnf_transformation, [], [f9])).
fof(f1443, plain, (p1(sK5(sK17)) | (~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(subsumption_resolution, [], [f1436, f73])).
fof(f1436, plain, (p1(sK5(sK17)) | ~ r1(sK14, sK17) | (~ spl21_45 | ~ spl21_46 | ~ spl21_53)), inference(resolution, [], [f1422, f433])).
fof(f433, plain, (! [X10] : (r1(sK4(X10), sK5(X10)) | ~ r1(sK14, X10)) | ~ spl21_45), inference(avatar_component_clause, [], [f432])).
fof(f432, plain, (spl21_45 <=> ! [X10] : (r1(sK4(X10), sK5(X10)) | ~ r1(sK14, X10))), introduced(avatar_definition, [new_symbols(naming, [spl21_45])])).
fof(f1422, plain, (! [X11] : (~ r1(sK4(sK17), X11) | p1(X11)) | (~ spl21_46 | ~ spl21_53)), inference(subsumption_resolution, [], [f1388, f73])).
fof(f1388, plain, (! [X11] : (~ r1(sK14, sK17) | p1(X11) | ~ r1(sK4(sK17), X11)) | (~ spl21_46 | ~ spl21_53)), inference(resolution, [], [f441, f539])).
fof(f539, plain, (! [X2, X3] : (~ r1(sK17, X2) | p1(X3) | ~ r1(X2, X3)) | ~ spl21_53), inference(avatar_component_clause, [], [f538])).
fof(f538, plain, (spl21_53 <=> ! [X3, X2] : (~ r1(X2, X3) | p1(X3) | ~ r1(sK17, X2))), introduced(avatar_definition, [new_symbols(naming, [spl21_53])])).
fof(f441, plain, (! [X10] : (r1(X10, sK4(X10)) | ~ r1(sK14, X10)) | ~ spl21_46), inference(avatar_component_clause, [], [f440])).
fof(f440, plain, (spl21_46 <=> ! [X10] : (r1(X10, sK4(X10)) | ~ r1(sK14, X10))), introduced(avatar_definition, [new_symbols(naming, [spl21_46])])).
fof(f1366, plain, (spl21_21 | spl21_28 | ~ spl21_30), inference(avatar_split_clause, [], [f1365, f264, f254, f197])).
fof(f1365, plain, (p1(sK14) | (spl21_28 | ~ spl21_30)), inference(subsumption_resolution, [], [f1344, f80])).
fof(f1344, plain, (p1(sK14) | ~ sP2(sK14) | (spl21_28 | ~ spl21_30)), inference(subsumption_resolution, [], [f1295, f73])).
fof(f1295, plain, (~ r1(sK14, sK17) | p1(sK14) | ~ sP2(sK14) | (spl21_28 | ~ spl21_30)), inference(resolution, [], [f495, f255])).
fof(f255, plain, (~ r1(sK14, sK6(sK14)) | spl21_28), inference(avatar_component_clause, [], [f254])).
fof(f1017, plain, spl21_14, inference(avatar_split_clause, [], [f1014, f153])).
fof(f1014, plain, sP1(sK14), inference(subsumption_resolution, [], [f1013, f70])).
fof(f1013, plain, (~ r1(sK13, sK14) | sP1(sK14)), inference(duplicate_literal_removal, [], [f1012])).
fof(f1012, plain, (~ r1(sK13, sK14) | sP1(sK14) | sP1(sK14) | ~ r1(sK13, sK14)), inference(resolution, [], [f635, f67])).
fof(f67, plain, ! [X8] : (r1(X8, sK19(X8)) | sP1(X8) | ~ r1(sK13, X8)), inference(cnf_transformation, [], [f41])).
fof(f635, plain, ! [X0] : (~ r1(sK14, sK19(X0)) | ~ r1(sK13, X0) | sP1(X0)), inference(subsumption_resolution, [], [f634, f68])).
fof(f68, plain, ! [X8] : (~ p1(sK19(X8)) | sP1(X8) | ~ r1(sK13, X8)), inference(cnf_transformation, [], [f41])).
fof(f634, plain, ! [X0] : (sP1(X0) | ~ r1(sK13, X0) | ~ r1(sK14, sK19(X0)) | p1(sK19(X0))), inference(subsumption_resolution, [], [f633, f77])).
fof(f633, plain, ! [X0] : (p1(sK16(sK19(X0))) | sP1(X0) | ~ r1(sK13, X0) | ~ r1(sK14, sK19(X0)) | p1(sK19(X0))), inference(duplicate_literal_removal, [], [f629])).
fof(f629, plain, ! [X0] : (p1(sK16(sK19(X0))) | sP1(X0) | ~ r1(sK13, X0) | ~ r1(sK14, sK19(X0)) | p1(sK19(X0)) | ~ r1(sK14, sK19(X0))), inference(resolution, [], [f284, f76])).
fof(f284, plain, ! [X4, X5] : (~ r1(sK15(sK19(X5)), X4) | p1(X4) | sP1(X5) | ~ r1(sK13, X5) | ~ r1(sK14, sK19(X5))), inference(subsumption_resolution, [], [f283, f68])).
fof(f283, plain, ! [X4, X5] : (p1(X4) | ~ r1(sK15(sK19(X5)), X4) | sP1(X5) | ~ r1(sK13, X5) | p1(sK19(X5)) | ~ r1(sK14, sK19(X5))), inference(subsumption_resolution, [], [f281, f78])).
fof(f281, plain, ! [X4, X5] : (p1(X4) | ~ r1(sK15(sK19(X5)), X4) | ~ p1(sK15(sK19(X5))) | sP1(X5) | ~ r1(sK13, X5) | p1(sK19(X5)) | ~ r1(sK14, sK19(X5))), inference(resolution, [], [f69, f75])).
fof(f69, plain, ! [X10, X8, X11] : (~ r1(sK19(X8), X10) | p1(X11) | ~ r1(X10, X11) | ~ p1(X10) | sP1(X8) | ~ r1(sK13, X8)), inference(cnf_transformation, [], [f41])).
fof(f628, plain, (spl21_63 | spl21_53 | ~ spl21_52), inference(avatar_split_clause, [], [f624, f534, f538, f626])).
fof(f534, plain, (spl21_52 <=> r1(sK17, sK20(sK17))), introduced(avatar_definition, [new_symbols(naming, [spl21_52])])).
fof(f624, plain, (! [X2, X0, X1] : (p1(X0) | ~ r1(X1, X0) | ~ r1(sK17, X1) | ~ r1(X2, sK17) | ~ r1(sK13, X2)) | ~ spl21_52), inference(resolution, [], [f612, f66])).
fof(f66, plain, ! [X14, X12, X8, X15] : (~ p1(sK20(X12)) | p1(X15) | ~ r1(X14, X15) | ~ r1(X12, X14) | ~ r1(X8, X12) | ~ r1(sK13, X8)), inference(cnf_transformation, [], [f41])).
fof(f612, plain, (p1(sK20(sK17)) | ~ spl21_52), inference(resolution, [], [f536, f74])).
fof(f536, plain, (r1(sK17, sK20(sK17)) | ~ spl21_52), inference(avatar_component_clause, [], [f534])).
fof(f540, plain, (spl21_52 | spl21_53), inference(avatar_split_clause, [], [f521, f538, f534])).
fof(f521, plain, ! [X2, X3] : (~ r1(X2, X3) | ~ r1(sK17, X2) | p1(X3) | r1(sK17, sK20(sK17))), inference(resolution, [], [f333, f73])).
fof(f333, plain, ! [X2, X0, X1] : (~ r1(sK14, X2) | ~ r1(X1, X0) | ~ r1(X2, X1) | p1(X0) | r1(X2, sK20(X2))), inference(resolution, [], [f65, f70])).
fof(f65, plain, ! [X14, X12, X8, X15] : (~ r1(sK13, X8) | p1(X15) | ~ r1(X14, X15) | ~ r1(X12, X14) | ~ r1(X8, X12) | r1(X12, sK20(X12))), inference(cnf_transformation, [], [f41])).
fof(f445, plain, (spl21_31 | ~ spl21_21 | spl21_46), inference(avatar_split_clause, [], [f444, f440, f197, f307])).
fof(f444, plain, ! [X9] : (r1(X9, sK4(X9)) | ~ r1(sK14, X9) | ~ p1(sK14) | sP0(sK14)), inference(subsumption_resolution, [], [f443, f79])).
fof(f443, plain, ! [X9] : (r1(X9, sK4(X9)) | ~ r1(sK14, X9) | ~ p1(sK14) | sP0(sK14) | ~ sP3(sK14)), inference(subsumption_resolution, [], [f342, f72])).
fof(f342, plain, ! [X9] : (r1(X9, sK4(X9)) | ~ r1(sK14, X9) | ~ p1(sK14) | p1(sK18) | sP0(sK14) | ~ sP3(sK14)), inference(resolution, [], [f42, f71])).
fof(f42, plain, ! [X4, X0, X1] : (~ r1(X0, X4) | r1(X1, sK4(X1)) | ~ r1(X0, X1) | ~ p1(X0) | p1(X4) | sP0(X0) | ~ sP3(X0)), inference(cnf_transformation, [], [f15])).
fof(f437, plain, (spl21_31 | ~ spl21_21 | spl21_45), inference(avatar_split_clause, [], [f436, f432, f197, f307])).
fof(f436, plain, ! [X9] : (r1(sK4(X9), sK5(X9)) | ~ r1(sK14, X9) | ~ p1(sK14) | sP0(sK14)), inference(subsumption_resolution, [], [f435, f79])).
fof(f435, plain, ! [X9] : (r1(sK4(X9), sK5(X9)) | ~ r1(sK14, X9) | ~ p1(sK14) | sP0(sK14) | ~ sP3(sK14)), inference(subsumption_resolution, [], [f378, f72])).
fof(f378, plain, ! [X9] : (r1(sK4(X9), sK5(X9)) | ~ r1(sK14, X9) | ~ p1(sK14) | p1(sK18) | sP0(sK14) | ~ sP3(sK14)), inference(resolution, [], [f43, f71])).
fof(f43, plain, ! [X4, X0, X1] : (~ r1(X0, X4) | r1(sK4(X1), sK5(X1)) | ~ r1(X0, X1) | ~ p1(X0) | p1(X4) | sP0(X0) | ~ sP3(X0)), inference(cnf_transformation, [], [f15])).
fof(f267, plain, (spl21_21 | spl21_28 | spl21_30), inference(avatar_split_clause, [], [f262, f264, f254, f197])).
fof(f262, plain, (r1(sK17, sK7(sK17)) | r1(sK14, sK6(sK14)) | p1(sK14)), inference(subsumption_resolution, [], [f235, f80])).
fof(f235, plain, (r1(sK17, sK7(sK17)) | r1(sK14, sK6(sK14)) | p1(sK14) | ~ sP2(sK14)), inference(resolution, [], [f46, f73])).
fof(f46, plain, ! [X4, X0] : (~ r1(X0, X4) | r1(X4, sK7(X4)) | r1(X0, sK6(X0)) | p1(X0) | ~ sP2(X0)), inference(cnf_transformation, [], [f20])).