fof(f36926, plain, $false, inference(avatar_sat_refutation, [], [f153, f157, f165, f170, f175, f183, f187, f194, f198, f202, f206, f211, f216, f334, f555, f1501, f7524, f7527, f7543, f7701, f8625, f10185, f11298, f11304, f11310, f12133, f14512, f14567, f15671, f16183, f16812, f16846, f17264, f18002, f18106, f18209, f20539, f20704, f20736, f20768, f23759, f23953, f23988, f24093, f29021, f29181, f29234, f36717, f36727, f36729, f36925])).
fof(f36925, plain, (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15 | ~ spl41_2688), inference(avatar_contradiction_clause, [], [f36924])).
fof(f36924, plain, ($false | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15 | ~ spl41_2688)), inference(subsumption_resolution, [], [f36923, f169])).
fof(f169, plain, (~ p2(sK30) | spl41_6), inference(avatar_component_clause, [], [f167])).
fof(f167, plain, (spl41_6 <=> p2(sK30)), introduced(avatar_definition, [new_symbols(naming, [spl41_6])])).
fof(f36923, plain, (p2(sK30) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15 | ~ spl41_2688)), inference(subsumption_resolution, [], [f36922, f174])).
fof(f174, plain, (r1(sK28, sK30) | ~ spl41_7), inference(avatar_component_clause, [], [f172])).
fof(f172, plain, (spl41_7 <=> r1(sK28, sK30)), introduced(avatar_definition, [new_symbols(naming, [spl41_7])])).
fof(f36922, plain, (~ r1(sK28, sK30) | p2(sK30) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15 | ~ spl41_2688)), inference(resolution, [], [f36859, f197])).
fof(f197, plain, (! [X21] : (~ p2(sK39(X21)) | ~ r1(sK28, X21) | p2(X21)) | ~ spl41_13), inference(avatar_component_clause, [], [f196])).
fof(f196, plain, (spl41_13 <=> ! [X21] : (~ p2(sK39(X21)) | ~ r1(sK28, X21) | p2(X21))), introduced(avatar_definition, [new_symbols(naming, [spl41_13])])).
fof(f36859, plain, (p2(sK39(sK30)) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_14 | ~ spl41_15 | ~ spl41_2688)), inference(subsumption_resolution, [], [f36858, f169])).
fof(f36858, plain, (p2(sK39(sK30)) | p2(sK30) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_14 | ~ spl41_15 | ~ spl41_2688)), inference(subsumption_resolution, [], [f36838, f174])).
fof(f36838, plain, (p2(sK39(sK30)) | ~ r1(sK28, sK30) | p2(sK30) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_14 | ~ spl41_15 | ~ spl41_2688)), inference(resolution, [], [f36793, f201])).
fof(f201, plain, (! [X21] : (r1(sK38(X21), sK39(X21)) | ~ r1(sK28, X21) | p2(X21)) | ~ spl41_14), inference(avatar_component_clause, [], [f200])).
fof(f200, plain, (spl41_14 <=> ! [X21] : (r1(sK38(X21), sK39(X21)) | ~ r1(sK28, X21) | p2(X21))), introduced(avatar_definition, [new_symbols(naming, [spl41_14])])).
fof(f36793, plain, (! [X18] : (~ r1(sK38(sK30), X18) | p2(X18)) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_15 | ~ spl41_2688)), inference(subsumption_resolution, [], [f36792, f169])).
fof(f36792, plain, (! [X18] : (~ r1(sK38(sK30), X18) | p2(X18) | p2(sK30)) | (~ spl41_5 | ~ spl41_7 | ~ spl41_15 | ~ spl41_2688)), inference(subsumption_resolution, [], [f36791, f174])).
fof(f36791, plain, (! [X18] : (~ r1(sK38(sK30), X18) | p2(X18) | ~ r1(sK28, sK30) | p2(sK30)) | (~ spl41_5 | ~ spl41_15 | ~ spl41_2688)), inference(subsumption_resolution, [], [f36779, f36716])).
fof(f36716, plain, (p2(sK38(sK30)) | ~ spl41_2688), inference(avatar_component_clause, [], [f36714])).
fof(f36714, plain, (spl41_2688 <=> p2(sK38(sK30))), introduced(avatar_definition, [new_symbols(naming, [spl41_2688])])).
fof(f36779, plain, (! [X18] : (~ p2(sK38(sK30)) | ~ r1(sK38(sK30), X18) | p2(X18) | ~ r1(sK28, sK30) | p2(sK30)) | (~ spl41_5 | ~ spl41_15)), inference(resolution, [], [f164, f205])).
fof(f205, plain, (! [X21] : (r1(X21, sK38(X21)) | ~ r1(sK28, X21) | p2(X21)) | ~ spl41_15), inference(avatar_component_clause, [], [f204])).
fof(f204, plain, (spl41_15 <=> ! [X21] : (r1(X21, sK38(X21)) | ~ r1(sK28, X21) | p2(X21))), introduced(avatar_definition, [new_symbols(naming, [spl41_15])])).
fof(f164, plain, (! [X10, X9] : (~ r1(sK30, X9) | ~ p2(X9) | ~ r1(X9, X10) | p2(X10)) | ~ spl41_5), inference(avatar_component_clause, [], [f163])).
fof(f163, plain, (spl41_5 <=> ! [X9, X10] : (~ p2(X9) | ~ r1(sK30, X9) | ~ r1(X9, X10) | p2(X10))), introduced(avatar_definition, [new_symbols(naming, [spl41_5])])).
fof(f36729, plain, (~ spl41_1 | spl41_46 | ~ spl41_12 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15), inference(avatar_split_clause, [], [f23746, f204, f200, f196, f192, f388, f147])).
fof(f147, plain, (spl41_1 <=> sP6(sK28)), introduced(avatar_definition, [new_symbols(naming, [spl41_1])])).
fof(f388, plain, (spl41_46 <=> sP1(sK28)), introduced(avatar_definition, [new_symbols(naming, [spl41_46])])).
fof(f192, plain, (spl41_12 <=> ! [X21] : (p2(sK38(X21)) | ~ r1(sK28, X21) | p2(X21))), introduced(avatar_definition, [new_symbols(naming, [spl41_12])])).
fof(f23746, plain, (sP1(sK28) | ~ sP6(sK28) | (~ spl41_12 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15)), inference(duplicate_literal_removal, [], [f23745])).
fof(f23745, plain, (sP1(sK28) | ~ sP6(sK28) | sP1(sK28) | ~ sP6(sK28) | (~ spl41_12 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15)), inference(resolution, [], [f23396, f74])).
fof(f74, plain, ! [X0] : (r1(X0, sK9(X0)) | sP1(X0) | ~ sP6(X0)), inference(cnf_transformation, [], [f21])).
fof(f21, plain, ! [X0] : ((((p2(sK7(X0)) & (~ p2(sK8(X0)) & r1(sK7(X0), sK8(X0))) & r1(X0, sK7(X0))) | p2(X0)) & ((! [X4] : (~ p2(X4) | ! [X5] : (p2(X5) | ~ r1(X4, X5)) | ~ r1(sK9(X0), X4)) & ~ p2(sK9(X0)) & r1(X0, sK9(X0))) | sP1(X0))) | ~ sP6(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK7, sK8, sK9])], [f17, f20, f19, f18])).
fof(f18, plain, ! [X0] : (? [X1] : (p2(X1) & ? [X2] : (~ p2(X2) & r1(X1, X2)) & r1(X0, X1)) => (p2(sK7(X0)) & ? [X2] : (~ p2(X2) & r1(sK7(X0), X2)) & r1(X0, sK7(X0)))), introduced(choice_axiom, [])).
fof(f19, plain, ! [X0] : (? [X2] : (~ p2(X2) & r1(sK7(X0), X2)) => (~ p2(sK8(X0)) & r1(sK7(X0), sK8(X0)))), introduced(choice_axiom, [])).
fof(f20, plain, ! [X0] : (? [X3] : (! [X4] : (~ p2(X4) | ! [X5] : (p2(X5) | ~ r1(X4, X5)) | ~ r1(X3, X4)) & ~ p2(X3) & r1(X0, X3)) => (! [X4] : (~ p2(X4) | ! [X5] : (p2(X5) | ~ r1(X4, X5)) | ~ r1(sK9(X0), X4)) & ~ p2(sK9(X0)) & r1(X0, sK9(X0)))), introduced(choice_axiom, [])).
fof(f17, plain, ! [X0] : (((? [X1] : (p2(X1) & ? [X2] : (~ p2(X2) & r1(X1, X2)) & r1(X0, X1)) | p2(X0)) & (? [X3] : (! [X4] : (~ p2(X4) | ! [X5] : (p2(X5) | ~ r1(X4, X5)) | ~ r1(X3, X4)) & ~ p2(X3) & r1(X0, X3)) | sP1(X0))) | ~ sP6(X0)), inference(rectify, [], [f16])).
fof(f16, plain, ! [X0] : (((? [X35] : (p2(X35) & ? [X36] : (~ p2(X36) & r1(X35, X36)) & r1(X0, X35)) | p2(X0)) & (? [X37] : (! [X38] : (~ p2(X38) | ! [X39] : (p2(X39) | ~ r1(X38, X39)) | ~ r1(X37, X38)) & ~ p2(X37) & r1(X0, X37)) | sP1(X0))) | ~ sP6(X0)), inference(nnf_transformation, [], [f14])).
fof(f14, plain, ! [X0] : (((? [X35] : (p2(X35) & ? [X36] : (~ p2(X36) & r1(X35, X36)) & r1(X0, X35)) | p2(X0)) & (? [X37] : (! [X38] : (~ p2(X38) | ! [X39] : (p2(X39) | ~ r1(X38, X39)) | ~ r1(X37, X38)) & ~ p2(X37) & r1(X0, X37)) | sP1(X0))) | ~ sP6(X0)), inference(usedef, [], [e14])).
fof(e14, plain, ! [X0] : (sP6(X0) <=> ((? [X35] : (p2(X35) & ? [X36] : (~ p2(X36) & r1(X35, X36)) & r1(X0, X35)) | p2(X0)) & (? [X37] : (! [X38] : (~ p2(X38) | ! [X39] : (p2(X39) | ~ r1(X38, X39)) | ~ r1(X37, X38)) & ~ p2(X37) & r1(X0, X37)) | sP1(X0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f23396, plain, (! [X0] : (~ r1(sK28, sK9(X0)) | sP1(X0) | ~ sP6(X0)) | (~ spl41_12 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15)), inference(subsumption_resolution, [], [f23395, f75])).
fof(f75, plain, ! [X0] : (~ p2(sK9(X0)) | sP1(X0) | ~ sP6(X0)), inference(cnf_transformation, [], [f21])).
fof(f23395, plain, (! [X0] : (~ r1(sK28, sK9(X0)) | sP1(X0) | ~ sP6(X0) | p2(sK9(X0))) | (~ spl41_12 | ~ spl41_13 | ~ spl41_14 | ~ spl41_15)), inference(subsumption_resolution, [], [f23394, f193])).
fof(f193, plain, (! [X21] : (~ r1(sK28, X21) | p2(sK38(X21)) | p2(X21)) | ~ spl41_12), inference(avatar_component_clause, [], [f192])).
fof(f23394, plain, (! [X0] : (~ r1(sK28, sK9(X0)) | ~ p2(sK38(sK9(X0))) | sP1(X0) | ~ sP6(X0) | p2(sK9(X0))) | (~ spl41_13 | ~ spl41_14 | ~ spl41_15)), inference(subsumption_resolution, [], [f23393, f197])).
fof(f23393, plain, (! [X0] : (p2(sK39(sK9(X0))) | ~ r1(sK28, sK9(X0)) | ~ p2(sK38(sK9(X0))) | sP1(X0) | ~ sP6(X0) | p2(sK9(X0))) | (~ spl41_14 | ~ spl41_15)), inference(duplicate_literal_removal, [], [f23372])).
fof(f23372, plain, (! [X0] : (p2(sK39(sK9(X0))) | ~ r1(sK28, sK9(X0)) | ~ p2(sK38(sK9(X0))) | sP1(X0) | ~ sP6(X0) | ~ r1(sK28, sK9(X0)) | p2(sK9(X0))) | (~ spl41_14 | ~ spl41_15)), inference(resolution, [], [f21252, f201])).
fof(f21252, plain, (! [X83, X84] : (~ r1(sK38(sK9(X83)), X84) | p2(X84) | ~ r1(sK28, sK9(X83)) | ~ p2(sK38(sK9(X83))) | sP1(X83) | ~ sP6(X83)) | ~ spl41_15), inference(subsumption_resolution, [], [f21193, f75])).
fof(f21193, plain, (! [X83, X84] : (~ r1(sK28, sK9(X83)) | p2(sK9(X83)) | p2(X84) | ~ r1(sK38(sK9(X83)), X84) | ~ p2(sK38(sK9(X83))) | sP1(X83) | ~ sP6(X83)) | ~ spl41_15), inference(resolution, [], [f205, f76])).
fof(f76, plain, ! [X4, X0, X5] : (~ r1(sK9(X0), X4) | p2(X5) | ~ r1(X4, X5) | ~ p2(X4) | sP1(X0) | ~ sP6(X0)), inference(cnf_transformation, [], [f21])).
fof(f36727, plain, (spl41_1426 | spl41_16 | ~ spl41_17 | ~ spl41_2250), inference(avatar_split_clause, [], [f36726, f29175, f213, f208, f20687])).
fof(f20687, plain, (spl41_1426 <=> ! [X3] : (~ r1(X3, sK40) | ~ sP1(X3))), introduced(avatar_definition, [new_symbols(naming, [spl41_1426])])).
fof(f208, plain, (spl41_16 <=> p2(sK40)), introduced(avatar_definition, [new_symbols(naming, [spl41_16])])).
fof(f213, plain, (spl41_17 <=> r1(sK28, sK40)), introduced(avatar_definition, [new_symbols(naming, [spl41_17])])).
fof(f29175, plain, (spl41_2250 <=> ! [X1, X0] : (~ r1(X0, sK29(sK40)) | ~ sP1(X1) | ~ r1(X1, X0))), introduced(avatar_definition, [new_symbols(naming, [spl41_2250])])).
fof(f36726, plain, (! [X0] : (~ sP1(X0) | ~ r1(X0, sK40)) | (spl41_16 | ~ spl41_17 | ~ spl41_2250)), inference(subsumption_resolution, [], [f36725, f215])).
fof(f215, plain, (r1(sK28, sK40) | ~ spl41_17), inference(avatar_component_clause, [], [f213])).
fof(f36725, plain, (! [X0] : (~ sP1(X0) | ~ r1(X0, sK40) | ~ r1(sK28, sK40)) | (spl41_16 | ~ spl41_2250)), inference(subsumption_resolution, [], [f36723, f210])).
fof(f210, plain, (~ p2(sK40) | spl41_16), inference(avatar_component_clause, [], [f208])).
fof(f36723, plain, (! [X0] : (~ sP1(X0) | ~ r1(X0, sK40) | p2(sK40) | ~ r1(sK28, sK40)) | ~ spl41_2250), inference(resolution, [], [f29176, f143])).
fof(f143, plain, ! [X1] : (r1(X1, sK29(X1)) | p2(X1) | ~ r1(sK28, X1)), inference(cnf_transformation, [], [f72])).
fof(f72, plain, (! [X1] : ((! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(sK29(X1), X3)) & ~ p2(sK29(X1)) & r1(X1, sK29(X1))) | p2(X1) | ~ r1(sK28, X1)) & ((! [X6] : ((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | sP4(X6) | sP5(X6) | ~ r1(sK30, X6)) & ((! [X9] : (~ p2(X9) | ! [X10] : (p2(X10) | ~ r1(X9, X10)) | ~ r1(sK30, X9)) & ~ p2(sK30)) | sP2(sK30)) & r1(sK28, sK30)) | sP6(sK28)) & ! [X11] : ((p1(sK31(X11)) & (~ p1(sK32(X11)) & r1(sK31(X11), sK32(X11))) & r1(X11, sK31(X11))) | p1(X11) | ~ r1(sK28, X11)) & (~ p1(sK33) & r1(sK28, sK33)) & (sP0(sK28) | ! [X15] : ((p3(sK34(X15)) & r1(X15, sK34(X15))) | ~ r1(sK28, X15))) & ! [X17] : ((p4(sK35(X17)) & (~ p4(sK36(X17)) & r1(sK35(X17), sK36(X17))) & r1(X17, sK35(X17))) | p4(X17) | ~ r1(sK28, X17)) & (~ p4(sK37) & r1(sK28, sK37)) & ((! [X21] : ((p2(sK38(X21)) & (~ p2(sK39(X21)) & r1(sK38(X21), sK39(X21))) & r1(X21, sK38(X21))) | p2(X21) | ~ r1(sK28, X21)) & (~ p2(sK40) & r1(sK28, sK40))) | ! [X25] : (~ p3(X25) | ~ r1(sK28, X25)))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK28, sK29, sK30, sK31, sK32, sK33, sK34, sK35, sK36, sK37, sK38, sK39, sK40])], [f58, f71, f70, f69, f68, f67, f66, f65, f64, f63, f62, f61, f60, f59])).
fof(f59, plain, (? [X0] : (! [X1] : (? [X2] : (! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) & ~ p2(X2) & r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & (? [X5] : (! [X6] : ((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | sP4(X6) | sP5(X6) | ~ r1(X5, X6)) & ((! [X9] : (~ p2(X9) | ! [X10] : (p2(X10) | ~ r1(X9, X10)) | ~ r1(X5, X9)) & ~ p2(X5)) | sP2(X5)) & r1(X0, X5)) | sP6(X0)) & ! [X11] : (? [X12] : (p1(X12) & ? [X13] : (~ p1(X13) & r1(X12, X13)) & r1(X11, X12)) | p1(X11) | ~ r1(X0, X11)) & ? [X14] : (~ p1(X14) & r1(X0, X14)) & (sP0(X0) | ! [X15] : (? [X16] : (p3(X16) & r1(X15, X16)) | ~ r1(X0, X15))) & ! [X17] : (? [X18] : (p4(X18) & ? [X19] : (~ p4(X19) & r1(X18, X19)) & r1(X17, X18)) | p4(X17) | ~ r1(X0, X17)) & ? [X20] : (~ p4(X20) & r1(X0, X20)) & ((! [X21] : (? [X22] : (p2(X22) & ? [X23] : (~ p2(X23) & r1(X22, X23)) & r1(X21, X22)) | p2(X21) | ~ r1(X0, X21)) & ? [X24] : (~ p2(X24) & r1(X0, X24))) | ! [X25] : (~ p3(X25) | ~ r1(X0, X25)))) => (! [X1] : (? [X2] : (! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) & ~ p2(X2) & r1(X1, X2)) | p2(X1) | ~ r1(sK28, X1)) & (? [X5] : (! [X6] : ((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | sP4(X6) | sP5(X6) | ~ r1(X5, X6)) & ((! [X9] : (~ p2(X9) | ! [X10] : (p2(X10) | ~ r1(X9, X10)) | ~ r1(X5, X9)) & ~ p2(X5)) | sP2(X5)) & r1(sK28, X5)) | sP6(sK28)) & ! [X11] : (? [X12] : (p1(X12) & ? [X13] : (~ p1(X13) & r1(X12, X13)) & r1(X11, X12)) | p1(X11) | ~ r1(sK28, X11)) & ? [X14] : (~ p1(X14) & r1(sK28, X14)) & (sP0(sK28) | ! [X15] : (? [X16] : (p3(X16) & r1(X15, X16)) | ~ r1(sK28, X15))) & ! [X17] : (? [X18] : (p4(X18) & ? [X19] : (~ p4(X19) & r1(X18, X19)) & r1(X17, X18)) | p4(X17) | ~ r1(sK28, X17)) & ? [X20] : (~ p4(X20) & r1(sK28, X20)) & ((! [X21] : (? [X22] : (p2(X22) & ? [X23] : (~ p2(X23) & r1(X22, X23)) & r1(X21, X22)) | p2(X21) | ~ r1(sK28, X21)) & ? [X24] : (~ p2(X24) & r1(sK28, X24))) | ! [X25] : (~ p3(X25) | ~ r1(sK28, X25))))), introduced(choice_axiom, [])).
fof(f60, plain, ! [X1] : (? [X2] : (! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) & ~ p2(X2) & r1(X1, X2)) => (! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(sK29(X1), X3)) & ~ p2(sK29(X1)) & r1(X1, sK29(X1)))), introduced(choice_axiom, [])).
fof(f61, plain, (? [X5] : (! [X6] : ((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | sP4(X6) | sP5(X6) | ~ r1(X5, X6)) & ((! [X9] : (~ p2(X9) | ! [X10] : (p2(X10) | ~ r1(X9, X10)) | ~ r1(X5, X9)) & ~ p2(X5)) | sP2(X5)) & r1(sK28, X5)) => (! [X6] : ((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | sP4(X6) | sP5(X6) | ~ r1(sK30, X6)) & ((! [X9] : (~ p2(X9) | ! [X10] : (p2(X10) | ~ r1(X9, X10)) | ~ r1(sK30, X9)) & ~ p2(sK30)) | sP2(sK30)) & r1(sK28, sK30))), introduced(choice_axiom, [])).
fof(f62, plain, ! [X11] : (? [X12] : (p1(X12) & ? [X13] : (~ p1(X13) & r1(X12, X13)) & r1(X11, X12)) => (p1(sK31(X11)) & ? [X13] : (~ p1(X13) & r1(sK31(X11), X13)) & r1(X11, sK31(X11)))), introduced(choice_axiom, [])).
fof(f63, plain, ! [X11] : (? [X13] : (~ p1(X13) & r1(sK31(X11), X13)) => (~ p1(sK32(X11)) & r1(sK31(X11), sK32(X11)))), introduced(choice_axiom, [])).
fof(f64, plain, (? [X14] : (~ p1(X14) & r1(sK28, X14)) => (~ p1(sK33) & r1(sK28, sK33))), introduced(choice_axiom, [])).
fof(f65, plain, ! [X15] : (? [X16] : (p3(X16) & r1(X15, X16)) => (p3(sK34(X15)) & r1(X15, sK34(X15)))), introduced(choice_axiom, [])).
fof(f66, plain, ! [X17] : (? [X18] : (p4(X18) & ? [X19] : (~ p4(X19) & r1(X18, X19)) & r1(X17, X18)) => (p4(sK35(X17)) & ? [X19] : (~ p4(X19) & r1(sK35(X17), X19)) & r1(X17, sK35(X17)))), introduced(choice_axiom, [])).
fof(f67, plain, ! [X17] : (? [X19] : (~ p4(X19) & r1(sK35(X17), X19)) => (~ p4(sK36(X17)) & r1(sK35(X17), sK36(X17)))), introduced(choice_axiom, [])).
fof(f68, plain, (? [X20] : (~ p4(X20) & r1(sK28, X20)) => (~ p4(sK37) & r1(sK28, sK37))), introduced(choice_axiom, [])).
fof(f69, plain, ! [X21] : (? [X22] : (p2(X22) & ? [X23] : (~ p2(X23) & r1(X22, X23)) & r1(X21, X22)) => (p2(sK38(X21)) & ? [X23] : (~ p2(X23) & r1(sK38(X21), X23)) & r1(X21, sK38(X21)))), introduced(choice_axiom, [])).
fof(f70, plain, ! [X21] : (? [X23] : (~ p2(X23) & r1(sK38(X21), X23)) => (~ p2(sK39(X21)) & r1(sK38(X21), sK39(X21)))), introduced(choice_axiom, [])).
fof(f71, plain, (? [X24] : (~ p2(X24) & r1(sK28, X24)) => (~ p2(sK40) & r1(sK28, sK40))), introduced(choice_axiom, [])).
fof(f58, plain, ? [X0] : (! [X1] : (? [X2] : (! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) & ~ p2(X2) & r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & (? [X5] : (! [X6] : ((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | sP4(X6) | sP5(X6) | ~ r1(X5, X6)) & ((! [X9] : (~ p2(X9) | ! [X10] : (p2(X10) | ~ r1(X9, X10)) | ~ r1(X5, X9)) & ~ p2(X5)) | sP2(X5)) & r1(X0, X5)) | sP6(X0)) & ! [X11] : (? [X12] : (p1(X12) & ? [X13] : (~ p1(X13) & r1(X12, X13)) & r1(X11, X12)) | p1(X11) | ~ r1(X0, X11)) & ? [X14] : (~ p1(X14) & r1(X0, X14)) & (sP0(X0) | ! [X15] : (? [X16] : (p3(X16) & r1(X15, X16)) | ~ r1(X0, X15))) & ! [X17] : (? [X18] : (p4(X18) & ? [X19] : (~ p4(X19) & r1(X18, X19)) & r1(X17, X18)) | p4(X17) | ~ r1(X0, X17)) & ? [X20] : (~ p4(X20) & r1(X0, X20)) & ((! [X21] : (? [X22] : (p2(X22) & ? [X23] : (~ p2(X23) & r1(X22, X23)) & r1(X21, X22)) | p2(X21) | ~ r1(X0, X21)) & ? [X24] : (~ p2(X24) & r1(X0, X24))) | ! [X25] : (~ p3(X25) | ~ r1(X0, X25)))), inference(rectify, [], [f15])).
fof(f15, plain, ? [X0] : (! [X1] : (? [X2] : (! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) & ~ p2(X2) & r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & (? [X5] : (! [X6] : ((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | sP4(X6) | sP5(X6) | ~ r1(X5, X6)) & ((! [X26] : (~ p2(X26) | ! [X27] : (p2(X27) | ~ r1(X26, X27)) | ~ r1(X5, X26)) & ~ p2(X5)) | sP2(X5)) & r1(X0, X5)) | sP6(X0)) & ! [X44] : (? [X45] : (p1(X45) & ? [X46] : (~ p1(X46) & r1(X45, X46)) & r1(X44, X45)) | p1(X44) | ~ r1(X0, X44)) & ? [X47] : (~ p1(X47) & r1(X0, X47)) & (sP0(X0) | ! [X52] : (? [X53] : (p3(X53) & r1(X52, X53)) | ~ r1(X0, X52))) & ! [X54] : (? [X55] : (p4(X55) & ? [X56] : (~ p4(X56) & r1(X55, X56)) & r1(X54, X55)) | p4(X54) | ~ r1(X0, X54)) & ? [X57] : (~ p4(X57) & r1(X0, X57)) & ((! [X58] : (? [X59] : (p2(X59) & ? [X60] : (~ p2(X60) & r1(X59, X60)) & r1(X58, X59)) | p2(X58) | ~ r1(X0, X58)) & ? [X61] : (~ p2(X61) & r1(X0, X61))) | ! [X62] : (~ p3(X62) | ~ r1(X0, X62)))), inference(definition_folding, [], [f7, e14, e13, e12, e11, e10, e9, e8])).
fof(f8, plain, ! [X0] : ((! [X48] : (? [X49] : (p2(X49) & ? [X50] : (~ p2(X50) & r1(X49, X50)) & r1(X48, X49)) | p2(X48) | ~ r1(X0, X48)) & ? [X51] : (~ p2(X51) & r1(X0, X51))) | ~ sP0(X0)), inference(usedef, [], [e8])).
fof(e8, plain, ! [X0] : (sP0(X0) <=> (! [X48] : (? [X49] : (p2(X49) & ? [X50] : (~ p2(X50) & r1(X49, X50)) & r1(X48, X49)) | p2(X48) | ~ r1(X0, X48)) & ? [X51] : (~ p2(X51) & r1(X0, X51)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f9, plain, ! [X0] : (! [X40] : (! [X41] : (? [X42] : (p2(X42) & ? [X43] : (~ p2(X43) & r1(X42, X43)) & r1(X41, X42)) | p2(X41) | ~ r1(X40, X41)) | ~ r1(X0, X40)) | ~ sP1(X0)), inference(usedef, [], [e9])).
fof(e9, plain, ! [X0] : (sP1(X0) <=> ! [X40] : (! [X41] : (? [X42] : (p2(X42) & ? [X43] : (~ p2(X43) & r1(X42, X43)) & r1(X41, X42)) | p2(X41) | ~ r1(X40, X41)) | ~ r1(X0, X40))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f10, plain, ! [X5] : ((! [X28] : (? [X29] : (p2(X29) & ? [X30] : (~ p2(X30) & r1(X29, X30)) & r1(X28, X29)) | p2(X28) | ~ r1(X5, X28)) & ? [X31] : (? [X32] : (! [X33] : (~ p2(X33) | ! [X34] : (p2(X34) | ~ r1(X33, X34)) | ~ r1(X32, X33)) & ~ p2(X32) & r1(X31, X32)) & r1(X5, X31))) | ~ sP2(X5)), inference(usedef, [], [e10])).
fof(e10, plain, ! [X5] : (sP2(X5) <=> (! [X28] : (? [X29] : (p2(X29) & ? [X30] : (~ p2(X30) & r1(X29, X30)) & r1(X28, X29)) | p2(X28) | ~ r1(X5, X28)) & ? [X31] : (? [X32] : (! [X33] : (~ p2(X33) | ! [X34] : (p2(X34) | ~ r1(X33, X34)) | ~ r1(X32, X33)) & ~ p2(X32) & r1(X31, X32)) & r1(X5, X31)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f11, plain, ! [X16] : (! [X22] : (! [X23] : (? [X24] : (p2(X24) & ? [X25] : (~ p2(X25) & r1(X24, X25)) & r1(X23, X24)) | p2(X23) | ~ r1(X22, X23)) | ~ r1(X16, X22)) | ~ sP3(X16)), inference(usedef, [], [e11])).
fof(e11, plain, ! [X16] : (sP3(X16) <=> ! [X22] : (! [X23] : (? [X24] : (p2(X24) & ? [X25] : (~ p2(X25) & r1(X24, X25)) & r1(X23, X24)) | p2(X23) | ~ r1(X22, X23)) | ~ r1(X16, X22))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f12, plain, ! [X6] : ((! [X9] : (? [X10] : (p2(X10) & ? [X11] : (~ p2(X11) & r1(X10, X11)) & r1(X9, X10)) | p2(X9) | ~ r1(X6, X9)) & ? [X12] : (? [X13] : (! [X14] : (~ p2(X14) | ! [X15] : (p2(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) & ~ p2(X13) & r1(X12, X13)) & r1(X6, X12))) | ~ sP4(X6)), inference(usedef, [], [e12])).
fof(e12, plain, ! [X6] : (sP4(X6) <=> (! [X9] : (? [X10] : (p2(X10) & ? [X11] : (~ p2(X11) & r1(X10, X11)) & r1(X9, X10)) | p2(X9) | ~ r1(X6, X9)) & ? [X12] : (? [X13] : (! [X14] : (~ p2(X14) | ! [X15] : (p2(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) & ~ p2(X13) & r1(X12, X13)) & r1(X6, X12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f13, plain, ! [X6] : (! [X16] : (((? [X17] : (p2(X17) & ? [X18] : (~ p2(X18) & r1(X17, X18)) & r1(X16, X17)) | p2(X16)) & (? [X19] : (! [X20] : (~ p2(X20) | ! [X21] : (p2(X21) | ~ r1(X20, X21)) | ~ r1(X19, X20)) & ~ p2(X19) & r1(X16, X19)) | sP3(X16))) | ~ r1(X6, X16)) | ~ sP5(X6)), inference(usedef, [], [e13])).
fof(e13, plain, ! [X6] : (sP5(X6) <=> ! [X16] : (((? [X17] : (p2(X17) & ? [X18] : (~ p2(X18) & r1(X17, X18)) & r1(X16, X17)) | p2(X16)) & (? [X19] : (! [X20] : (~ p2(X20) | ! [X21] : (p2(X21) | ~ r1(X20, X21)) | ~ r1(X19, X20)) & ~ p2(X19) & r1(X16, X19)) | sP3(X16))) | ~ r1(X6, X16))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f7, plain, ? [X0] : (! [X1] : (? [X2] : (! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) & ~ p2(X2) & r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & (? [X5] : (! [X6] : ((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | (! [X9] : (? [X10] : (p2(X10) & ? [X11] : (~ p2(X11) & r1(X10, X11)) & r1(X9, X10)) | p2(X9) | ~ r1(X6, X9)) & ? [X12] : (? [X13] : (! [X14] : (~ p2(X14) | ! [X15] : (p2(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) & ~ p2(X13) & r1(X12, X13)) & r1(X6, X12))) | ! [X16] : (((? [X17] : (p2(X17) & ? [X18] : (~ p2(X18) & r1(X17, X18)) & r1(X16, X17)) | p2(X16)) & (? [X19] : (! [X20] : (~ p2(X20) | ! [X21] : (p2(X21) | ~ r1(X20, X21)) | ~ r1(X19, X20)) & ~ p2(X19) & r1(X16, X19)) | ! [X22] : (! [X23] : (? [X24] : (p2(X24) & ? [X25] : (~ p2(X25) & r1(X24, X25)) & r1(X23, X24)) | p2(X23) | ~ r1(X22, X23)) | ~ r1(X16, X22)))) | ~ r1(X6, X16)) | ~ r1(X5, X6)) & ((! [X26] : (~ p2(X26) | ! [X27] : (p2(X27) | ~ r1(X26, X27)) | ~ r1(X5, X26)) & ~ p2(X5)) | (! [X28] : (? [X29] : (p2(X29) & ? [X30] : (~ p2(X30) & r1(X29, X30)) & r1(X28, X29)) | p2(X28) | ~ r1(X5, X28)) & ? [X31] : (? [X32] : (! [X33] : (~ p2(X33) | ! [X34] : (p2(X34) | ~ r1(X33, X34)) | ~ r1(X32, X33)) & ~ p2(X32) & r1(X31, X32)) & r1(X5, X31)))) & r1(X0, X5)) | ((? [X35] : (p2(X35) & ? [X36] : (~ p2(X36) & r1(X35, X36)) & r1(X0, X35)) | p2(X0)) & (? [X37] : (! [X38] : (~ p2(X38) | ! [X39] : (p2(X39) | ~ r1(X38, X39)) | ~ r1(X37, X38)) & ~ p2(X37) & r1(X0, X37)) | ! [X40] : (! [X41] : (? [X42] : (p2(X42) & ? [X43] : (~ p2(X43) & r1(X42, X43)) & r1(X41, X42)) | p2(X41) | ~ r1(X40, X41)) | ~ r1(X0, X40))))) & ! [X44] : (? [X45] : (p1(X45) & ? [X46] : (~ p1(X46) & r1(X45, X46)) & r1(X44, X45)) | p1(X44) | ~ r1(X0, X44)) & ? [X47] : (~ p1(X47) & r1(X0, X47)) & ((! [X48] : (? [X49] : (p2(X49) & ? [X50] : (~ p2(X50) & r1(X49, X50)) & r1(X48, X49)) | p2(X48) | ~ r1(X0, X48)) & ? [X51] : (~ p2(X51) & r1(X0, X51))) | ! [X52] : (? [X53] : (p3(X53) & r1(X52, X53)) | ~ r1(X0, X52))) & ! [X54] : (? [X55] : (p4(X55) & ? [X56] : (~ p4(X56) & r1(X55, X56)) & r1(X54, X55)) | p4(X54) | ~ r1(X0, X54)) & ? [X57] : (~ p4(X57) & r1(X0, X57)) & ((! [X58] : (? [X59] : (p2(X59) & ? [X60] : (~ p2(X60) & r1(X59, X60)) & r1(X58, X59)) | p2(X58) | ~ r1(X0, X58)) & ? [X61] : (~ p2(X61) & r1(X0, X61))) | ! [X62] : (~ p3(X62) | ~ r1(X0, X62)))), inference(flattening, [], [f6])).
fof(f6, plain, ? [X0] : ((! [X1] : (? [X2] : (! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) & ~ p2(X2) & r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & (? [X5] : (! [X6] : (((! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) & ~ p2(X6)) | (! [X9] : (? [X10] : (p2(X10) & ? [X11] : (~ p2(X11) & r1(X10, X11)) & r1(X9, X10)) | p2(X9) | ~ r1(X6, X9)) & ? [X12] : (? [X13] : (! [X14] : (~ p2(X14) | ! [X15] : (p2(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) & ~ p2(X13) & r1(X12, X13)) & r1(X6, X12)))) | ! [X16] : (((? [X17] : (p2(X17) & ? [X18] : (~ p2(X18) & r1(X17, X18)) & r1(X16, X17)) | p2(X16)) & (? [X19] : (! [X20] : (~ p2(X20) | ! [X21] : (p2(X21) | ~ r1(X20, X21)) | ~ r1(X19, X20)) & ~ p2(X19) & r1(X16, X19)) | ! [X22] : (! [X23] : (? [X24] : (p2(X24) & ? [X25] : (~ p2(X25) & r1(X24, X25)) & r1(X23, X24)) | p2(X23) | ~ r1(X22, X23)) | ~ r1(X16, X22)))) | ~ r1(X6, X16)) | ~ r1(X5, X6)) & ((! [X26] : (~ p2(X26) | ! [X27] : (p2(X27) | ~ r1(X26, X27)) | ~ r1(X5, X26)) & ~ p2(X5)) | (! [X28] : (? [X29] : (p2(X29) & ? [X30] : (~ p2(X30) & r1(X29, X30)) & r1(X28, X29)) | p2(X28) | ~ r1(X5, X28)) & ? [X31] : (? [X32] : (! [X33] : (~ p2(X33) | ! [X34] : (p2(X34) | ~ r1(X33, X34)) | ~ r1(X32, X33)) & ~ p2(X32) & r1(X31, X32)) & r1(X5, X31)))) & r1(X0, X5)) | ((? [X35] : (p2(X35) & ? [X36] : (~ p2(X36) & r1(X35, X36)) & r1(X0, X35)) | p2(X0)) & (? [X37] : (! [X38] : (~ p2(X38) | ! [X39] : (p2(X39) | ~ r1(X38, X39)) | ~ r1(X37, X38)) & ~ p2(X37) & r1(X0, X37)) | ! [X40] : (! [X41] : (? [X42] : (p2(X42) & ? [X43] : (~ p2(X43) & r1(X42, X43)) & r1(X41, X42)) | p2(X41) | ~ r1(X40, X41)) | ~ r1(X0, X40)))))) & ! [X44] : (? [X45] : (p1(X45) & ? [X46] : (~ p1(X46) & r1(X45, X46)) & r1(X44, X45)) | p1(X44) | ~ r1(X0, X44)) & ? [X47] : (~ p1(X47) & r1(X0, X47)) & ((! [X48] : (? [X49] : (p2(X49) & ? [X50] : (~ p2(X50) & r1(X49, X50)) & r1(X48, X49)) | p2(X48) | ~ r1(X0, X48)) & ? [X51] : (~ p2(X51) & r1(X0, X51))) | ! [X52] : (? [X53] : (p3(X53) & r1(X52, X53)) | ~ r1(X0, X52))) & ! [X54] : (? [X55] : (p4(X55) & ? [X56] : (~ p4(X56) & r1(X55, X56)) & r1(X54, X55)) | p4(X54) | ~ r1(X0, X54)) & ? [X57] : (~ p4(X57) & r1(X0, X57)) & ((! [X58] : (? [X59] : (p2(X59) & ? [X60] : (~ p2(X60) & r1(X59, X60)) & r1(X58, X59)) | p2(X58) | ~ r1(X0, X58)) & ? [X61] : (~ p2(X61) & r1(X0, X61))) | ! [X62] : (~ p3(X62) | ~ r1(X0, X62)))), inference(ennf_transformation, [], [f5])).
fof(f5, plain, ? [X0] : ~ (~ (! [X1] : (~ ! [X2] : (~ ! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) | p2(X2) | ~ r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & (~ ! [X5] : (~ ! [X6] : (~ ((~ ! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) | p2(X6)) & (~ ! [X9] : (~ ! [X10] : (~ p2(X10) | ! [X11] : (p2(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) | p2(X9) | ~ r1(X6, X9)) | ! [X12] : (! [X13] : (~ ! [X14] : (~ p2(X14) | ! [X15] : (p2(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) | p2(X13) | ~ r1(X12, X13)) | ~ r1(X6, X12)))) | ! [X16] : (((~ ! [X17] : (~ p2(X17) | ! [X18] : (p2(X18) | ~ r1(X17, X18)) | ~ r1(X16, X17)) | p2(X16)) & (~ ! [X19] : (~ ! [X20] : (~ p2(X20) | ! [X21] : (p2(X21) | ~ r1(X20, X21)) | ~ r1(X19, X20)) | p2(X19) | ~ r1(X16, X19)) | ! [X22] : (! [X23] : (~ ! [X24] : (~ p2(X24) | ! [X25] : (p2(X25) | ~ r1(X24, X25)) | ~ r1(X23, X24)) | p2(X23) | ~ r1(X22, X23)) | ~ r1(X16, X22)))) | ~ r1(X6, X16)) | ~ r1(X5, X6)) | ((~ ! [X26] : (~ p2(X26) | ! [X27] : (p2(X27) | ~ r1(X26, X27)) | ~ r1(X5, X26)) | p2(X5)) & (~ ! [X28] : (~ ! [X29] : (~ p2(X29) | ! [X30] : (p2(X30) | ~ r1(X29, X30)) | ~ r1(X28, X29)) | p2(X28) | ~ r1(X5, X28)) | ! [X31] : (! [X32] : (~ ! [X33] : (~ p2(X33) | ! [X34] : (p2(X34) | ~ r1(X33, X34)) | ~ r1(X32, X33)) | p2(X32) | ~ r1(X31, X32)) | ~ r1(X5, X31)))) | ~ r1(X0, X5)) | ((~ ! [X35] : (~ p2(X35) | ! [X36] : (p2(X36) | ~ r1(X35, X36)) | ~ r1(X0, X35)) | p2(X0)) & (~ ! [X37] : (~ ! [X38] : (~ p2(X38) | ! [X39] : (p2(X39) | ~ r1(X38, X39)) | ~ r1(X37, X38)) | p2(X37) | ~ r1(X0, X37)) | ! [X40] : (! [X41] : (~ ! [X42] : (~ p2(X42) | ! [X43] : (p2(X43) | ~ r1(X42, X43)) | ~ r1(X41, X42)) | p2(X41) | ~ r1(X40, X41)) | ~ r1(X0, X40)))))) | ~ ! [X44] : (~ ! [X45] : (~ p1(X45) | ! [X46] : (p1(X46) | ~ r1(X45, X46)) | ~ r1(X44, X45)) | p1(X44) | ~ r1(X0, X44)) | ! [X47] : (p1(X47) | ~ r1(X0, X47)) | ((~ ! [X48] : (~ ! [X49] : (~ p2(X49) | ! [X50] : (p2(X50) | ~ r1(X49, X50)) | ~ r1(X48, X49)) | p2(X48) | ~ r1(X0, X48)) | ! [X51] : (p2(X51) | ~ r1(X0, X51))) & ~ ! [X52] : (~ ! [X53] : (~ p3(X53) | ~ r1(X52, X53)) | ~ r1(X0, X52))) | ~ ! [X54] : (~ ! [X55] : (~ p4(X55) | ! [X56] : (p4(X56) | ~ r1(X55, X56)) | ~ r1(X54, X55)) | p4(X54) | ~ r1(X0, X54)) | ! [X57] : (p4(X57) | ~ r1(X0, X57)) | ((~ ! [X58] : (~ ! [X59] : (~ p2(X59) | ! [X60] : (p2(X60) | ~ r1(X59, X60)) | ~ r1(X58, X59)) | p2(X58) | ~ r1(X0, X58)) | ! [X61] : (p2(X61) | ~ r1(X0, X61))) & ~ ! [X62] : (~ p3(X62) | ~ r1(X0, X62)))), inference(flattening, [], [f4])).
fof(f4, plain, ~ ~ ? [X0] : ~ (~ (! [X1] : (~ ! [X2] : (~ ! [X3] : (~ p2(X3) | ! [X4] : (p2(X4) | ~ r1(X3, X4)) | ~ r1(X2, X3)) | p2(X2) | ~ r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & (~ ! [X5] : (~ ! [X6] : (~ ((~ ! [X7] : (~ p2(X7) | ! [X8] : (p2(X8) | ~ r1(X7, X8)) | ~ r1(X6, X7)) | p2(X6)) & (~ ! [X9] : (~ ! [X10] : (~ p2(X10) | ! [X11] : (p2(X11) | ~ r1(X10, X11)) | ~ r1(X9, X10)) | p2(X9) | ~ r1(X6, X9)) | ! [X12] : (! [X13] : (~ ! [X14] : (~ p2(X14) | ! [X15] : (p2(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) | p2(X13) | ~ r1(X12, X13)) | ~ r1(X6, X12)))) | ! [X16] : (((~ ! [X17] : (~ p2(X17) | ! [X18] : (p2(X18) | ~ r1(X17, X18)) | ~ r1(X16, X17)) | p2(X16)) & (~ ! [X19] : (~ ! [X20] : (~ p2(X20) | ! [X21] : (p2(X21) | ~ r1(X20, X21)) | ~ r1(X19, X20)) | p2(X19) | ~ r1(X16, X19)) | ! [X22] : (! [X23] : (~ ! [X24] : (~ p2(X24) | ! [X25] : (p2(X25) | ~ r1(X24, X25)) | ~ r1(X23, X24)) | p2(X23) | ~ r1(X22, X23)) | ~ r1(X16, X22)))) | ~ r1(X6, X16)) | ~ r1(X5, X6)) | ((~ ! [X26] : (~ p2(X26) | ! [X27] : (p2(X27) | ~ r1(X26, X27)) | ~ r1(X5, X26)) | p2(X5)) & (~ ! [X28] : (~ ! [X29] : (~ p2(X29) | ! [X30] : (p2(X30) | ~ r1(X29, X30)) | ~ r1(X28, X29)) | p2(X28) | ~ r1(X5, X28)) | ! [X31] : (! [X32] : (~ ! [X33] : (~ p2(X33) | ! [X34] : (p2(X34) | ~ r1(X33, X34)) | ~ r1(X32, X33)) | p2(X32) | ~ r1(X31, X32)) | ~ r1(X5, X31)))) | ~ r1(X0, X5)) | ((~ ! [X35] : (~ p2(X35) | ! [X36] : (p2(X36) | ~ r1(X35, X36)) | ~ r1(X0, X35)) | p2(X0)) & (~ ! [X37] : (~ ! [X38] : (~ p2(X38) | ! [X39] : (p2(X39) | ~ r1(X38, X39)) | ~ r1(X37, X38)) | p2(X37) | ~ r1(X0, X37)) | ! [X40] : (! [X41] : (~ ! [X42] : (~ p2(X42) | ! [X43] : (p2(X43) | ~ r1(X42, X43)) | ~ r1(X41, X42)) | p2(X41) | ~ r1(X40, X41)) | ~ r1(X0, X40)))))) | ~ ! [X44] : (~ ! [X45] : (~ p1(X45) | ! [X46] : (p1(X46) | ~ r1(X45, X46)) | ~ r1(X44, X45)) | p1(X44) | ~ r1(X0, X44)) | ! [X47] : (p1(X47) | ~ r1(X0, X47)) | ((~ ! [X48] : (~ ! [X49] : (~ p2(X49) | ! [X50] : (p2(X50) | ~ r1(X49, X50)) | ~ r1(X48, X49)) | p2(X48) | ~ r1(X0, X48)) | ! [X51] : (p2(X51) | ~ r1(X0, X51))) & ~ ! [X52] : (~ ! [X53] : (~ p3(X53) | ~ r1(X52, X53)) | ~ r1(X0, X52))) | ~ ! [X54] : (~ ! [X55] : (~ p4(X55) | ! [X56] : (p4(X56) | ~ r1(X55, X56)) | ~ r1(X54, X55)) | p4(X54) | ~ r1(X0, X54)) | ! [X57] : (p4(X57) | ~ r1(X0, X57)) | ((~ ! [X58] : (~ ! [X59] : (~ p2(X59) | ! [X60] : (p2(X60) | ~ r1(X59, X60)) | ~ r1(X58, X59)) | p2(X58) | ~ r1(X0, X58)) | ! [X61] : (p2(X61) | ~ r1(X0, X61))) & ~ ! [X62] : (~ p3(X62) | ~ r1(X0, X62)))), inference(rectify, [], [f3])).
fof(f3, plain, ~ ~ ? [X0] : ~ (~ (! [X1] : (~ ! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) & (~ ! [X1] : (~ ! [X0] : (~ ((~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0)) & (~ ! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ! [X1] : (! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)))) | ! [X1] : (((~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1)) & (~ ! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | ! [X0] : (! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)))) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ((~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1)) & (~ ! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | ! [X0] : (! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)))) | ~ r1(X0, X1)) | ((~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0)) & (~ ! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ! [X1] : (! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)))))) | ~ ! [X1] : (~ ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1) | ~ r1(X0, X1)) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ((~ ! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ! [X1] : (p2(X1) | ~ r1(X0, X1))) & ~ ! [X1] : (~ ! [X0] : (~ p3(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1))) | ~ ! [X1] : (~ ! [X0] : (~ p4(X0) | ! [X1] : (p4(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p4(X1) | ~ r1(X0, X1)) | ! [X1] : (p4(X1) | ~ r1(X0, X1)) | ((~ ! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ! [X1] : (p2(X1) | ~ r1(X0, X1))) & ~ ! [X1] : (~ p3(X1) | ~ r1(X0, X1)))), inference(negated_conjecture, [], [f2])).
fof(f2, plain, ~ ~ ? [X0] : ~ (~ (! [X1] : (~ ! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) & (~ ! [X1] : (~ ! [X0] : (~ ((~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0)) & (~ ! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ! [X1] : (! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)))) | ! [X1] : (((~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1)) & (~ ! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | ! [X0] : (! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)))) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | ((~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1)) & (~ ! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | ! [X0] : (! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)))) | ~ r1(X0, X1)) | ((~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0)) & (~ ! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ! [X1] : (! [X0] : (~ ! [X1] : (~ p2(X1) | ! [X0] : (p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)) | p2(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1)))))) | ~ ! [X1] : (~ ! [X0] : (~ p1(X0) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p1(X1) | ~ r1(X0, X1)) | ! [X1] : (p1(X1) | ~ r1(X0, X1)) | ((~ ! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ! [X1] : (p2(X1) | ~ r1(X0, X1))) & ~ ! [X1] : (~ ! [X0] : (~ p3(X0) | ~ r1(X1, X0)) | ~ r1(X0, X1))) | ~ ! [X1] : (~ ! [X0] : (~ p4(X0) | ! [X1] : (p4(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p4(X1) | ~ r1(X0, X1)) | ! [X1] : (p4(X1) | ~ r1(X0, X1)) | ((~ ! [X1] : (~ ! [X0] : (~ p2(X0) | ! [X1] : (p2(X1) | ~ r1(X0, X1)) | ~ r1(X1, X0)) | p2(X1) | ~ r1(X0, X1)) | ! [X1] : (p2(X1) | ~ r1(X0, X1))) & ~ ! [X1] : (~ p3(X1) | ~ r1(X0, X1)))), file('/home/ubuntu/library/tptp/Problems/LCL/LCL660+1.001.p', main)).
fof(f29176, plain, (! [X0, X1] : (~ r1(X0, sK29(sK40)) | ~ sP1(X1) | ~ r1(X1, X0)) | ~ spl41_2250), inference(avatar_component_clause, [], [f29175])).
fof(f36717, plain, (spl41_6 | spl41_2688 | ~ spl41_7 | ~ spl41_12), inference(avatar_split_clause, [], [f21078, f192, f172, f36714, f167])).
fof(f21078, plain, (p2(sK38(sK30)) | p2(sK30) | (~ spl41_7 | ~ spl41_12)), inference(resolution, [], [f193, f174])).
fof(f29234, plain, (spl41_16 | ~ spl41_17 | ~ spl41_2251), inference(avatar_contradiction_clause, [], [f29233])).
fof(f29233, plain, ($false | (spl41_16 | ~ spl41_17 | ~ spl41_2251)), inference(subsumption_resolution, [], [f29232, f215])).
fof(f29232, plain, (~ r1(sK28, sK40) | (spl41_16 | ~ spl41_2251)), inference(subsumption_resolution, [], [f29231, f210])).
fof(f29231, plain, (p2(sK40) | ~ r1(sK28, sK40) | ~ spl41_2251), inference(resolution, [], [f29180, f144])).
fof(f144, plain, ! [X1] : (~ p2(sK29(X1)) | p2(X1) | ~ r1(sK28, X1)), inference(cnf_transformation, [], [f72])).
fof(f29180, plain, (p2(sK29(sK40)) | ~ spl41_2251), inference(avatar_component_clause, [], [f29178])).
fof(f29178, plain, (spl41_2251 <=> p2(sK29(sK40))), introduced(avatar_definition, [new_symbols(naming, [spl41_2251])])).
fof(f29181, plain, (spl41_2250 | spl41_2251 | ~ spl41_1972), inference(avatar_split_clause, [], [f29173, f25921, f29178, f29175])).
fof(f25921, plain, (spl41_1972 <=> p2(sK24(sK29(sK40)))), introduced(avatar_definition, [new_symbols(naming, [spl41_1972])])).
fof(f29173, plain, (! [X0, X1] : (p2(sK29(sK40)) | ~ r1(X0, sK29(sK40)) | ~ r1(X1, X0) | ~ sP1(X1)) | ~ spl41_1972), inference(resolution, [], [f25923, f110])).
fof(f110, plain, ! [X2, X0, X1] : (~ p2(sK24(X2)) | p2(X2) | ~ r1(X1, X2) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ! [X0] : (! [X1] : (! [X2] : ((p2(sK23(X2)) & (~ p2(sK24(X2)) & r1(sK23(X2), sK24(X2))) & r1(X2, sK23(X2))) | p2(X2) | ~ r1(X1, X2)) | ~ r1(X0, X1)) | ~ sP1(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK23, sK24])], [f48, f50, f49])).
fof(f49, plain, ! [X2] : (? [X3] : (p2(X3) & ? [X4] : (~ p2(X4) & r1(X3, X4)) & r1(X2, X3)) => (p2(sK23(X2)) & ? [X4] : (~ p2(X4) & r1(sK23(X2), X4)) & r1(X2, sK23(X2)))), introduced(choice_axiom, [])).
fof(f50, plain, ! [X2] : (? [X4] : (~ p2(X4) & r1(sK23(X2), X4)) => (~ p2(sK24(X2)) & r1(sK23(X2), sK24(X2)))), introduced(choice_axiom, [])).
fof(f48, plain, ! [X0] : (! [X1] : (! [X2] : (? [X3] : (p2(X3) & ? [X4] : (~ p2(X4) & r1(X3, X4)) & r1(X2, X3)) | p2(X2) | ~ r1(X1, X2)) | ~ r1(X0, X1)) | ~ sP1(X0)), inference(rectify, [], [f47])).
fof(f47, plain, ! [X0] : (! [X40] : (! [X41] : (? [X42] : (p2(X42) & ? [X43] : (~ p2(X43) & r1(X42, X43)) & r1(X41, X42)) | p2(X41) | ~ r1(X40, X41)) | ~ r1(X0, X40)) | ~ sP1(X0)), inference(nnf_transformation, [], [f9])).
fof(f25923, plain, (p2(sK24(sK29(sK40))) | ~ spl41_1972), inference(avatar_component_clause, [], [f25921])).
fof(f29021, plain, (spl41_1972 | spl41_16 | ~ spl41_17 | ~ spl41_1429 | ~ spl41_1435 | ~ spl41_1441), inference(avatar_split_clause, [], [f29000, f20765, f20733, f20701, f213, f208, f25921])).
fof(f20701, plain, (spl41_1429 <=> p2(sK23(sK29(sK40)))), introduced(avatar_definition, [new_symbols(naming, [spl41_1429])])).
fof(f20733, plain, (spl41_1435 <=> r1(sK29(sK40), sK23(sK29(sK40)))), introduced(avatar_definition, [new_symbols(naming, [spl41_1435])])).
fof(f20765, plain, (spl41_1441 <=> r1(sK23(sK29(sK40)), sK24(sK29(sK40)))), introduced(avatar_definition, [new_symbols(naming, [spl41_1441])])).
fof(f29000, plain, (p2(sK24(sK29(sK40))) | (spl41_16 | ~ spl41_17 | ~ spl41_1429 | ~ spl41_1435 | ~ spl41_1441)), inference(resolution, [], [f25081, f20767])).
fof(f20767, plain, (r1(sK23(sK29(sK40)), sK24(sK29(sK40))) | ~ spl41_1441), inference(avatar_component_clause, [], [f20765])).
fof(f25081, plain, (! [X0] : (~ r1(sK23(sK29(sK40)), X0) | p2(X0)) | (spl41_16 | ~ spl41_17 | ~ spl41_1429 | ~ spl41_1435)), inference(subsumption_resolution, [], [f25080, f215])).
fof(f25080, plain, (! [X0] : (p2(X0) | ~ r1(sK23(sK29(sK40)), X0) | ~ r1(sK28, sK40)) | (spl41_16 | ~ spl41_1429 | ~ spl41_1435)), inference(subsumption_resolution, [], [f25079, f210])).
fof(f25079, plain, (! [X0] : (p2(X0) | ~ r1(sK23(sK29(sK40)), X0) | p2(sK40) | ~ r1(sK28, sK40)) | (~ spl41_1429 | ~ spl41_1435)), inference(subsumption_resolution, [], [f25029, f20703])).
fof(f20703, plain, (p2(sK23(sK29(sK40))) | ~ spl41_1429), inference(avatar_component_clause, [], [f20701])).
fof(f25029, plain, (! [X0] : (p2(X0) | ~ r1(sK23(sK29(sK40)), X0) | ~ p2(sK23(sK29(sK40))) | p2(sK40) | ~ r1(sK28, sK40)) | ~ spl41_1435), inference(resolution, [], [f20735, f145])).
fof(f145, plain, ! [X4, X3, X1] : (~ r1(sK29(X1), X3) | p2(X4) | ~ r1(X3, X4) | ~ p2(X3) | p2(X1) | ~ r1(sK28, X1)), inference(cnf_transformation, [], [f72])).
fof(f20735, plain, (r1(sK29(sK40), sK23(sK29(sK40))) | ~ spl41_1435), inference(avatar_component_clause, [], [f20733])).
fof(f24093, plain, (spl41_267 | spl41_196 | spl41_187 | ~ spl41_268), inference(avatar_split_clause, [], [f24092, f8345, f4729, f4786, f7598])).
fof(f7598, plain, (spl41_267 <=> r1(sK29(sK27(sK28)), sK23(sK29(sK27(sK28))))), introduced(avatar_definition, [new_symbols(naming, [spl41_267])])).
fof(f4786, plain, (spl41_196 <=> ! [X14] : (~ r1(X14, sK27(sK28)) | ~ sP1(X14))), introduced(avatar_definition, [new_symbols(naming, [spl41_196])])).
fof(f4729, plain, (spl41_187 <=> p2(sK27(sK28))), introduced(avatar_definition, [new_symbols(naming, [spl41_187])])).
fof(f8345, plain, (spl41_268 <=> r1(sK28, sK27(sK28))), introduced(avatar_definition, [new_symbols(naming, [spl41_268])])).
fof(f24092, plain, (! [X3] : (~ r1(X3, sK27(sK28)) | ~ sP1(X3) | r1(sK29(sK27(sK28)), sK23(sK29(sK27(sK28))))) | (spl41_187 | ~ spl41_268)), inference(subsumption_resolution, [], [f8859, f4730])).
fof(f4730, plain, (~ p2(sK27(sK28)) | spl41_187), inference(avatar_component_clause, [], [f4729])).
fof(f8859, plain, (! [X3] : (~ r1(X3, sK27(sK28)) | ~ sP1(X3) | p2(sK27(sK28)) | r1(sK29(sK27(sK28)), sK23(sK29(sK27(sK28))))) | ~ spl41_268), inference(resolution, [], [f8346, f1241])).
fof(f1241, plain, ! [X12, X13] : (~ r1(sK28, X12) | ~ r1(X13, X12) | ~ sP1(X13) | p2(X12) | r1(sK29(X12), sK23(sK29(X12)))), inference(subsumption_resolution, [], [f1226, f144])).
fof(f1226, plain, ! [X12, X13] : (p2(sK29(X12)) | r1(sK29(X12), sK23(sK29(X12))) | ~ r1(X13, X12) | ~ sP1(X13) | p2(X12) | ~ r1(sK28, X12)), inference(resolution, [], [f108, f143])).
fof(f108, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p2(X2) | r1(X2, sK23(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f51])).
fof(f8346, plain, (r1(sK28, sK27(sK28)) | ~ spl41_268), inference(avatar_component_clause, [], [f8345])).
fof(f23988, plain, (spl41_195 | spl41_196 | spl41_187 | ~ spl41_268), inference(avatar_split_clause, [], [f23987, f8345, f4729, f4786, f4782])).
fof(f4782, plain, (spl41_195 <=> p2(sK23(sK29(sK27(sK28))))), introduced(avatar_definition, [new_symbols(naming, [spl41_195])])).
fof(f23987, plain, (! [X1] : (~ r1(X1, sK27(sK28)) | ~ sP1(X1) | p2(sK23(sK29(sK27(sK28))))) | (spl41_187 | ~ spl41_268)), inference(subsumption_resolution, [], [f8857, f4730])).
fof(f8857, plain, (! [X1] : (~ r1(X1, sK27(sK28)) | ~ sP1(X1) | p2(sK27(sK28)) | p2(sK23(sK29(sK27(sK28))))) | ~ spl41_268), inference(resolution, [], [f8346, f1182])).
fof(f1182, plain, ! [X12, X13] : (~ r1(sK28, X12) | ~ r1(X13, X12) | ~ sP1(X13) | p2(X12) | p2(sK23(sK29(X12)))), inference(subsumption_resolution, [], [f1167, f144])).
fof(f1167, plain, ! [X12, X13] : (p2(sK29(X12)) | p2(sK23(sK29(X12))) | ~ r1(X13, X12) | ~ sP1(X13) | p2(X12) | ~ r1(sK28, X12)), inference(resolution, [], [f111, f143])).
fof(f111, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p2(X2) | p2(sK23(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f51])).
fof(f23953, plain, (~ spl41_46 | ~ spl41_196 | ~ spl41_268), inference(avatar_split_clause, [], [f15675, f8345, f4786, f388])).
fof(f15675, plain, (~ sP1(sK28) | (~ spl41_196 | ~ spl41_268)), inference(resolution, [], [f4787, f8346])).
fof(f4787, plain, (! [X14] : (~ r1(X14, sK27(sK28)) | ~ sP1(X14)) | ~ spl41_196), inference(avatar_component_clause, [], [f4786])).
fof(f23759, plain, (~ spl41_46 | ~ spl41_17 | ~ spl41_1426), inference(avatar_split_clause, [], [f21335, f20687, f213, f388])).
fof(f21335, plain, (~ sP1(sK28) | (~ spl41_17 | ~ spl41_1426)), inference(resolution, [], [f20688, f215])).
fof(f20688, plain, (! [X3] : (~ r1(X3, sK40) | ~ sP1(X3)) | ~ spl41_1426), inference(avatar_component_clause, [], [f20687])).
fof(f20768, plain, (spl41_1441 | spl41_1426 | spl41_16 | ~ spl41_17), inference(avatar_split_clause, [], [f20763, f213, f208, f20687, f20765])).
fof(f20763, plain, (! [X17] : (~ r1(X17, sK40) | ~ sP1(X17) | r1(sK23(sK29(sK40)), sK24(sK29(sK40)))) | (spl41_16 | ~ spl41_17)), inference(subsumption_resolution, [], [f20591, f210])).
fof(f20591, plain, (! [X17] : (~ r1(X17, sK40) | ~ sP1(X17) | p2(sK40) | r1(sK23(sK29(sK40)), sK24(sK29(sK40)))) | ~ spl41_17), inference(resolution, [], [f215, f1449])).
fof(f1449, plain, ! [X12, X13] : (~ r1(sK28, X12) | ~ r1(X13, X12) | ~ sP1(X13) | p2(X12) | r1(sK23(sK29(X12)), sK24(sK29(X12)))), inference(subsumption_resolution, [], [f1436, f144])).
fof(f1436, plain, ! [X12, X13] : (p2(sK29(X12)) | r1(sK23(sK29(X12)), sK24(sK29(X12))) | ~ r1(X13, X12) | ~ sP1(X13) | p2(X12) | ~ r1(sK28, X12)), inference(resolution, [], [f109, f143])).
fof(f109, plain, ! [X2, X0, X1] : (~ r1(X1, X2) | p2(X2) | r1(sK23(X2), sK24(X2)) | ~ r1(X0, X1) | ~ sP1(X0)), inference(cnf_transformation, [], [f51])).
fof(f20736, plain, (spl41_1435 | spl41_1426 | spl41_16 | ~ spl41_17), inference(avatar_split_clause, [], [f20731, f213, f208, f20687, f20733])).
fof(f20731, plain, (! [X11] : (~ r1(X11, sK40) | ~ sP1(X11) | r1(sK29(sK40), sK23(sK29(sK40)))) | (spl41_16 | ~ spl41_17)), inference(subsumption_resolution, [], [f20585, f210])).
fof(f20585, plain, (! [X11] : (~ r1(X11, sK40) | ~ sP1(X11) | p2(sK40) | r1(sK29(sK40), sK23(sK29(sK40)))) | ~ spl41_17), inference(resolution, [], [f215, f1241])).
fof(f20704, plain, (spl41_1429 | spl41_1426 | spl41_16 | ~ spl41_17), inference(avatar_split_clause, [], [f20699, f213, f208, f20687, f20701])).
fof(f20699, plain, (! [X5] : (~ r1(X5, sK40) | ~ sP1(X5) | p2(sK23(sK29(sK40)))) | (spl41_16 | ~ spl41_17)), inference(subsumption_resolution, [], [f20579, f210])).
fof(f20579, plain, (! [X5] : (~ r1(X5, sK40) | ~ sP1(X5) | p2(sK40) | p2(sK23(sK29(sK40)))) | ~ spl41_17), inference(resolution, [], [f215, f1182])).
fof(f20539, plain, (~ spl41_4 | spl41_74 | ~ spl41_1101), inference(avatar_contradiction_clause, [], [f20538])).
fof(f20538, plain, ($false | (~ spl41_4 | spl41_74 | ~ spl41_1101)), inference(subsumption_resolution, [], [f20537, f161])).
fof(f161, plain, (sP2(sK30) | ~ spl41_4), inference(avatar_component_clause, [], [f159])).
fof(f159, plain, (spl41_4 <=> sP2(sK30)), introduced(avatar_definition, [new_symbols(naming, [spl41_4])])).
fof(f20537, plain, (~ sP2(sK30) | (~ spl41_4 | spl41_74 | ~ spl41_1101)), inference(duplicate_literal_removal, [], [f20535])).
fof(f20535, plain, (~ sP2(sK30) | ~ sP2(sK30) | (~ spl41_4 | spl41_74 | ~ spl41_1101)), inference(resolution, [], [f19178, f100])).
fof(f100, plain, ! [X0] : (r1(X0, sK21(X0)) | ~ sP2(X0)), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ! [X0] : ((! [X1] : ((p2(sK19(X1)) & (~ p2(sK20(X1)) & r1(sK19(X1), sK20(X1))) & r1(X1, sK19(X1))) | p2(X1) | ~ r1(X0, X1)) & ((! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(sK22(X0), X6)) & ~ p2(sK22(X0)) & r1(sK21(X0), sK22(X0))) & r1(X0, sK21(X0)))) | ~ sP2(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK19, sK20, sK21, sK22])], [f41, f45, f44, f43, f42])).
fof(f42, plain, ! [X1] : (? [X2] : (p2(X2) & ? [X3] : (~ p2(X3) & r1(X2, X3)) & r1(X1, X2)) => (p2(sK19(X1)) & ? [X3] : (~ p2(X3) & r1(sK19(X1), X3)) & r1(X1, sK19(X1)))), introduced(choice_axiom, [])).
fof(f43, plain, ! [X1] : (? [X3] : (~ p2(X3) & r1(sK19(X1), X3)) => (~ p2(sK20(X1)) & r1(sK19(X1), sK20(X1)))), introduced(choice_axiom, [])).
fof(f44, plain, ! [X0] : (? [X4] : (? [X5] : (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(X5, X6)) & ~ p2(X5) & r1(X4, X5)) & r1(X0, X4)) => (? [X5] : (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(X5, X6)) & ~ p2(X5) & r1(sK21(X0), X5)) & r1(X0, sK21(X0)))), introduced(choice_axiom, [])).
fof(f45, plain, ! [X0] : (? [X5] : (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(X5, X6)) & ~ p2(X5) & r1(sK21(X0), X5)) => (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(sK22(X0), X6)) & ~ p2(sK22(X0)) & r1(sK21(X0), sK22(X0)))), introduced(choice_axiom, [])).
fof(f41, plain, ! [X0] : ((! [X1] : (? [X2] : (p2(X2) & ? [X3] : (~ p2(X3) & r1(X2, X3)) & r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & ? [X4] : (? [X5] : (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(X5, X6)) & ~ p2(X5) & r1(X4, X5)) & r1(X0, X4))) | ~ sP2(X0)), inference(rectify, [], [f40])).
fof(f40, plain, ! [X5] : ((! [X28] : (? [X29] : (p2(X29) & ? [X30] : (~ p2(X30) & r1(X29, X30)) & r1(X28, X29)) | p2(X28) | ~ r1(X5, X28)) & ? [X31] : (? [X32] : (! [X33] : (~ p2(X33) | ! [X34] : (p2(X34) | ~ r1(X33, X34)) | ~ r1(X32, X33)) & ~ p2(X32) & r1(X31, X32)) & r1(X5, X31))) | ~ sP2(X5)), inference(nnf_transformation, [], [f10])).
fof(f19178, plain, (! [X0] : (~ r1(X0, sK21(sK30)) | ~ sP2(X0)) | (~ spl41_4 | spl41_74 | ~ spl41_1101)), inference(subsumption_resolution, [], [f19177, f554])).
fof(f554, plain, (~ p2(sK21(sK30)) | spl41_74), inference(avatar_component_clause, [], [f552])).
fof(f552, plain, (spl41_74 <=> p2(sK21(sK30))), introduced(avatar_definition, [new_symbols(naming, [spl41_74])])).
fof(f19177, plain, (! [X0] : (p2(sK21(sK30)) | ~ r1(X0, sK21(sK30)) | ~ sP2(X0)) | (~ spl41_4 | spl41_74 | ~ spl41_1101)), inference(resolution, [], [f18867, f106])).
fof(f106, plain, ! [X0, X1] : (~ p2(sK20(X1)) | p2(X1) | ~ r1(X0, X1) | ~ sP2(X0)), inference(cnf_transformation, [], [f46])).
fof(f18867, plain, (p2(sK20(sK21(sK30))) | (~ spl41_4 | spl41_74 | ~ spl41_1101)), inference(subsumption_resolution, [], [f18866, f161])).
fof(f18866, plain, (p2(sK20(sK21(sK30))) | ~ sP2(sK30) | (spl41_74 | ~ spl41_1101)), inference(subsumption_resolution, [], [f18848, f554])).
fof(f18848, plain, (p2(sK20(sK21(sK30))) | p2(sK21(sK30)) | ~ sP2(sK30) | ~ spl41_1101), inference(resolution, [], [f18105, f891])).
fof(f891, plain, ! [X4] : (r1(sK19(sK21(X4)), sK20(sK21(X4))) | p2(sK21(X4)) | ~ sP2(X4)), inference(duplicate_literal_removal, [], [f878])).
fof(f878, plain, ! [X4] : (p2(sK21(X4)) | r1(sK19(sK21(X4)), sK20(sK21(X4))) | ~ sP2(X4) | ~ sP2(X4)), inference(resolution, [], [f105, f100])).
fof(f105, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | r1(sK19(X1), sK20(X1)) | ~ sP2(X0)), inference(cnf_transformation, [], [f46])).
fof(f18105, plain, (! [X4] : (~ r1(sK19(sK21(sK30)), X4) | p2(X4)) | ~ spl41_1101), inference(avatar_component_clause, [], [f18104])).
fof(f18104, plain, (spl41_1101 <=> ! [X4] : (p2(X4) | ~ r1(sK19(sK21(sK30)), X4))), introduced(avatar_definition, [new_symbols(naming, [spl41_1101])])).
fof(f18209, plain, (~ spl41_4 | spl41_74 | spl41_1100), inference(avatar_contradiction_clause, [], [f18208])).
fof(f18208, plain, ($false | (~ spl41_4 | spl41_74 | spl41_1100)), inference(subsumption_resolution, [], [f18207, f161])).
fof(f18207, plain, (~ sP2(sK30) | (spl41_74 | spl41_1100)), inference(subsumption_resolution, [], [f18205, f554])).
fof(f18205, plain, (p2(sK21(sK30)) | ~ sP2(sK30) | spl41_1100), inference(resolution, [], [f18102, f662])).
fof(f662, plain, ! [X4] : (p2(sK19(sK21(X4))) | p2(sK21(X4)) | ~ sP2(X4)), inference(duplicate_literal_removal, [], [f649])).
fof(f649, plain, ! [X4] : (p2(sK21(X4)) | p2(sK19(sK21(X4))) | ~ sP2(X4) | ~ sP2(X4)), inference(resolution, [], [f107, f100])).
fof(f107, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | p2(sK19(X1)) | ~ sP2(X0)), inference(cnf_transformation, [], [f46])).
fof(f18102, plain, (~ p2(sK19(sK21(sK30))) | spl41_1100), inference(avatar_component_clause, [], [f18100])).
fof(f18100, plain, (spl41_1100 <=> p2(sK19(sK21(sK30)))), introduced(avatar_definition, [new_symbols(naming, [spl41_1100])])).
fof(f18106, plain, (~ spl41_1100 | spl41_1101 | ~ spl41_4 | spl41_74 | ~ spl41_150), inference(avatar_split_clause, [], [f18098, f1482, f552, f159, f18104, f18100])).
fof(f1482, plain, (spl41_150 <=> ! [X9, X8] : (~ p2(X8) | p2(X9) | ~ r1(X8, X9) | ~ r1(sK21(sK30), X8))), introduced(avatar_definition, [new_symbols(naming, [spl41_150])])).
fof(f18098, plain, (! [X4] : (p2(X4) | ~ r1(sK19(sK21(sK30)), X4) | ~ p2(sK19(sK21(sK30)))) | (~ spl41_4 | spl41_74 | ~ spl41_150)), inference(subsumption_resolution, [], [f18097, f161])).
fof(f18097, plain, (! [X4] : (p2(X4) | ~ r1(sK19(sK21(sK30)), X4) | ~ p2(sK19(sK21(sK30))) | ~ sP2(sK30)) | (spl41_74 | ~ spl41_150)), inference(subsumption_resolution, [], [f18052, f554])).
fof(f18052, plain, (! [X4] : (p2(X4) | ~ r1(sK19(sK21(sK30)), X4) | ~ p2(sK19(sK21(sK30))) | p2(sK21(sK30)) | ~ sP2(sK30)) | ~ spl41_150), inference(resolution, [], [f1483, f788])).
fof(f788, plain, ! [X4] : (r1(sK21(X4), sK19(sK21(X4))) | p2(sK21(X4)) | ~ sP2(X4)), inference(duplicate_literal_removal, [], [f775])).
fof(f775, plain, ! [X4] : (p2(sK21(X4)) | r1(sK21(X4), sK19(sK21(X4))) | ~ sP2(X4) | ~ sP2(X4)), inference(resolution, [], [f104, f100])).
fof(f104, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | r1(X1, sK19(X1)) | ~ sP2(X0)), inference(cnf_transformation, [], [f46])).
fof(f1483, plain, (! [X8, X9] : (~ r1(sK21(sK30), X8) | p2(X9) | ~ r1(X8, X9) | ~ p2(X8)) | ~ spl41_150), inference(avatar_component_clause, [], [f1482])).
fof(f18002, plain, (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_9 | spl41_124), inference(avatar_contradiction_clause, [], [f18001])).
fof(f18001, plain, ($false | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_9 | spl41_124)), inference(subsumption_resolution, [], [f17983, f1279])).
fof(f1279, plain, (~ p2(sK26(sK30)) | spl41_124), inference(avatar_component_clause, [], [f1278])).
fof(f1278, plain, (spl41_124 <=> p2(sK26(sK30))), introduced(avatar_definition, [new_symbols(naming, [spl41_124])])).
fof(f17983, plain, (p2(sK26(sK30)) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_9)), inference(resolution, [], [f17053, f16998])).
fof(f16998, plain, (r1(sK25(sK30), sK26(sK30)) | (spl41_6 | ~ spl41_7 | ~ spl41_9)), inference(subsumption_resolution, [], [f16997, f182])).
fof(f182, plain, (sP0(sK28) | ~ spl41_9), inference(avatar_component_clause, [], [f180])).
fof(f180, plain, (spl41_9 <=> sP0(sK28)), introduced(avatar_definition, [new_symbols(naming, [spl41_9])])).
fof(f16997, plain, (r1(sK25(sK30), sK26(sK30)) | ~ sP0(sK28) | (spl41_6 | ~ spl41_7)), inference(subsumption_resolution, [], [f16885, f169])).
fof(f16885, plain, (p2(sK30) | r1(sK25(sK30), sK26(sK30)) | ~ sP0(sK28) | ~ spl41_7), inference(resolution, [], [f174, f115])).
fof(f115, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | r1(sK25(X1), sK26(X1)) | ~ sP0(X0)), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ! [X0] : ((! [X1] : ((p2(sK25(X1)) & (~ p2(sK26(X1)) & r1(sK25(X1), sK26(X1))) & r1(X1, sK25(X1))) | p2(X1) | ~ r1(X0, X1)) & (~ p2(sK27(X0)) & r1(X0, sK27(X0)))) | ~ sP0(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK25, sK26, sK27])], [f53, f56, f55, f54])).
fof(f54, plain, ! [X1] : (? [X2] : (p2(X2) & ? [X3] : (~ p2(X3) & r1(X2, X3)) & r1(X1, X2)) => (p2(sK25(X1)) & ? [X3] : (~ p2(X3) & r1(sK25(X1), X3)) & r1(X1, sK25(X1)))), introduced(choice_axiom, [])).
fof(f55, plain, ! [X1] : (? [X3] : (~ p2(X3) & r1(sK25(X1), X3)) => (~ p2(sK26(X1)) & r1(sK25(X1), sK26(X1)))), introduced(choice_axiom, [])).
fof(f56, plain, ! [X0] : (? [X4] : (~ p2(X4) & r1(X0, X4)) => (~ p2(sK27(X0)) & r1(X0, sK27(X0)))), introduced(choice_axiom, [])).
fof(f53, plain, ! [X0] : ((! [X1] : (? [X2] : (p2(X2) & ? [X3] : (~ p2(X3) & r1(X2, X3)) & r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & ? [X4] : (~ p2(X4) & r1(X0, X4))) | ~ sP0(X0)), inference(rectify, [], [f52])).
fof(f52, plain, ! [X0] : ((! [X48] : (? [X49] : (p2(X49) & ? [X50] : (~ p2(X50) & r1(X49, X50)) & r1(X48, X49)) | p2(X48) | ~ r1(X0, X48)) & ? [X51] : (~ p2(X51) & r1(X0, X51))) | ~ sP0(X0)), inference(nnf_transformation, [], [f8])).
fof(f17053, plain, (! [X0] : (~ r1(sK25(sK30), X0) | p2(X0)) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_9)), inference(subsumption_resolution, [], [f17001, f17000])).
fof(f17000, plain, (p2(sK25(sK30)) | (spl41_6 | ~ spl41_7 | ~ spl41_9)), inference(subsumption_resolution, [], [f16999, f182])).
fof(f16999, plain, (p2(sK25(sK30)) | ~ sP0(sK28) | (spl41_6 | ~ spl41_7)), inference(subsumption_resolution, [], [f16886, f169])).
fof(f16886, plain, (p2(sK30) | p2(sK25(sK30)) | ~ sP0(sK28) | ~ spl41_7), inference(resolution, [], [f174, f117])).
fof(f117, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | p2(sK25(X1)) | ~ sP0(X0)), inference(cnf_transformation, [], [f57])).
fof(f17001, plain, (! [X0] : (~ p2(sK25(sK30)) | ~ r1(sK25(sK30), X0) | p2(X0)) | (~ spl41_5 | spl41_6 | ~ spl41_7 | ~ spl41_9)), inference(resolution, [], [f16996, f164])).
fof(f16996, plain, (r1(sK30, sK25(sK30)) | (spl41_6 | ~ spl41_7 | ~ spl41_9)), inference(subsumption_resolution, [], [f16995, f182])).
fof(f16995, plain, (r1(sK30, sK25(sK30)) | ~ sP0(sK28) | (spl41_6 | ~ spl41_7)), inference(subsumption_resolution, [], [f16884, f169])).
fof(f16884, plain, (p2(sK30) | r1(sK30, sK25(sK30)) | ~ sP0(sK28) | ~ spl41_7), inference(resolution, [], [f174, f114])).
fof(f114, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | r1(X1, sK25(X1)) | ~ sP0(X0)), inference(cnf_transformation, [], [f57])).
fof(f17264, plain, (spl41_6 | ~ spl41_7 | ~ spl41_9 | ~ spl41_124), inference(avatar_contradiction_clause, [], [f17263])).
fof(f17263, plain, ($false | (spl41_6 | ~ spl41_7 | ~ spl41_9 | ~ spl41_124)), inference(subsumption_resolution, [], [f17262, f182])).
fof(f17262, plain, (~ sP0(sK28) | (spl41_6 | ~ spl41_7 | ~ spl41_124)), inference(resolution, [], [f17055, f174])).
fof(f17055, plain, (! [X0] : (~ r1(X0, sK30) | ~ sP0(X0)) | (spl41_6 | ~ spl41_124)), inference(subsumption_resolution, [], [f17054, f169])).
fof(f17054, plain, (! [X0] : (p2(sK30) | ~ r1(X0, sK30) | ~ sP0(X0)) | ~ spl41_124), inference(resolution, [], [f1280, f116])).
fof(f116, plain, ! [X0, X1] : (~ p2(sK26(X1)) | p2(X1) | ~ r1(X0, X1) | ~ sP0(X0)), inference(cnf_transformation, [], [f57])).
fof(f1280, plain, (p2(sK26(sK30)) | ~ spl41_124), inference(avatar_component_clause, [], [f1278])).
fof(f16846, plain, (~ spl41_1 | spl41_519 | spl41_46 | ~ spl41_438 | ~ spl41_440), inference(avatar_split_clause, [], [f16845, f11307, f11295, f388, f12136, f147])).
fof(f12136, plain, (spl41_519 <=> ! [X0] : (p2(X0) | ~ r1(sK25(sK9(sK28)), X0))), introduced(avatar_definition, [new_symbols(naming, [spl41_519])])).
fof(f11295, plain, (spl41_438 <=> r1(sK9(sK28), sK25(sK9(sK28)))), introduced(avatar_definition, [new_symbols(naming, [spl41_438])])).
fof(f11307, plain, (spl41_440 <=> p2(sK25(sK9(sK28)))), introduced(avatar_definition, [new_symbols(naming, [spl41_440])])).
fof(f16845, plain, (! [X0] : (p2(X0) | ~ r1(sK25(sK9(sK28)), X0) | ~ sP6(sK28)) | (spl41_46 | ~ spl41_438 | ~ spl41_440)), inference(subsumption_resolution, [], [f11459, f389])).
fof(f389, plain, (~ sP1(sK28) | spl41_46), inference(avatar_component_clause, [], [f388])).
fof(f11459, plain, (! [X0] : (p2(X0) | ~ r1(sK25(sK9(sK28)), X0) | sP1(sK28) | ~ sP6(sK28)) | (~ spl41_438 | ~ spl41_440)), inference(subsumption_resolution, [], [f11409, f11309])).
fof(f11309, plain, (p2(sK25(sK9(sK28))) | ~ spl41_440), inference(avatar_component_clause, [], [f11307])).
fof(f11409, plain, (! [X0] : (p2(X0) | ~ r1(sK25(sK9(sK28)), X0) | ~ p2(sK25(sK9(sK28))) | sP1(sK28) | ~ sP6(sK28)) | ~ spl41_438), inference(resolution, [], [f11297, f76])).
fof(f11297, plain, (r1(sK9(sK28), sK25(sK9(sK28))) | ~ spl41_438), inference(avatar_component_clause, [], [f11295])).
fof(f16812, plain, (~ spl41_439 | spl41_468 | ~ spl41_519), inference(avatar_contradiction_clause, [], [f16811])).
fof(f16811, plain, ($false | (~ spl41_439 | spl41_468 | ~ spl41_519)), inference(subsumption_resolution, [], [f16792, f11833])).
fof(f11833, plain, (~ p2(sK26(sK9(sK28))) | spl41_468), inference(avatar_component_clause, [], [f11832])).
fof(f11832, plain, (spl41_468 <=> p2(sK26(sK9(sK28)))), introduced(avatar_definition, [new_symbols(naming, [spl41_468])])).
fof(f16792, plain, (p2(sK26(sK9(sK28))) | (~ spl41_439 | ~ spl41_519)), inference(resolution, [], [f12137, f11303])).
fof(f11303, plain, (r1(sK25(sK9(sK28)), sK26(sK9(sK28))) | ~ spl41_439), inference(avatar_component_clause, [], [f11301])).
fof(f11301, plain, (spl41_439 <=> r1(sK25(sK9(sK28)), sK26(sK9(sK28)))), introduced(avatar_definition, [new_symbols(naming, [spl41_439])])).
fof(f12137, plain, (! [X0] : (~ r1(sK25(sK9(sK28)), X0) | p2(X0)) | ~ spl41_519), inference(avatar_component_clause, [], [f12136])).
fof(f16183, plain, (~ spl41_1 | ~ spl41_9 | spl41_46 | spl41_200 | ~ spl41_468), inference(avatar_contradiction_clause, [], [f16182])).
fof(f16182, plain, ($false | (~ spl41_1 | ~ spl41_9 | spl41_46 | spl41_200 | ~ spl41_468)), inference(subsumption_resolution, [], [f16181, f149])).
fof(f149, plain, (sP6(sK28) | ~ spl41_1), inference(avatar_component_clause, [], [f147])).
fof(f16181, plain, (~ sP6(sK28) | (~ spl41_9 | spl41_46 | spl41_200 | ~ spl41_468)), inference(subsumption_resolution, [], [f16180, f389])).
fof(f16180, plain, (sP1(sK28) | ~ sP6(sK28) | (~ spl41_9 | spl41_200 | ~ spl41_468)), inference(subsumption_resolution, [], [f16177, f182])).
fof(f16177, plain, (~ sP0(sK28) | sP1(sK28) | ~ sP6(sK28) | (spl41_200 | ~ spl41_468)), inference(resolution, [], [f15992, f74])).
fof(f15992, plain, (! [X0] : (~ r1(X0, sK9(sK28)) | ~ sP0(X0)) | (spl41_200 | ~ spl41_468)), inference(subsumption_resolution, [], [f15991, f4909])).
fof(f4909, plain, (~ p2(sK9(sK28)) | spl41_200), inference(avatar_component_clause, [], [f4908])).
fof(f4908, plain, (spl41_200 <=> p2(sK9(sK28))), introduced(avatar_definition, [new_symbols(naming, [spl41_200])])).
fof(f15991, plain, (! [X0] : (p2(sK9(sK28)) | ~ r1(X0, sK9(sK28)) | ~ sP0(X0)) | ~ spl41_468), inference(resolution, [], [f11834, f116])).
fof(f11834, plain, (p2(sK26(sK9(sK28))) | ~ spl41_468), inference(avatar_component_clause, [], [f11832])).
fof(f15671, plain, (spl41_196 | spl41_187 | ~ spl41_268 | ~ spl41_771), inference(avatar_split_clause, [], [f15670, f14506, f8345, f4729, f4786])).
fof(f14506, plain, (spl41_771 <=> ! [X1, X0] : (~ r1(X0, sK29(sK27(sK28))) | ~ sP1(X1) | ~ r1(X1, X0))), introduced(avatar_definition, [new_symbols(naming, [spl41_771])])).
fof(f15670, plain, (! [X0] : (~ sP1(X0) | ~ r1(X0, sK27(sK28))) | (spl41_187 | ~ spl41_268 | ~ spl41_771)), inference(subsumption_resolution, [], [f15669, f8346])).
fof(f15669, plain, (! [X0] : (~ sP1(X0) | ~ r1(X0, sK27(sK28)) | ~ r1(sK28, sK27(sK28))) | (spl41_187 | ~ spl41_771)), inference(subsumption_resolution, [], [f15667, f4730])).
fof(f15667, plain, (! [X0] : (~ sP1(X0) | ~ r1(X0, sK27(sK28)) | p2(sK27(sK28)) | ~ r1(sK28, sK27(sK28))) | ~ spl41_771), inference(resolution, [], [f14507, f143])).
fof(f14507, plain, (! [X0, X1] : (~ r1(X0, sK29(sK27(sK28))) | ~ sP1(X1) | ~ r1(X1, X0)) | ~ spl41_771), inference(avatar_component_clause, [], [f14506])).
fof(f14567, plain, (spl41_187 | ~ spl41_268 | ~ spl41_772), inference(avatar_contradiction_clause, [], [f14566])).
fof(f14566, plain, ($false | (spl41_187 | ~ spl41_268 | ~ spl41_772)), inference(subsumption_resolution, [], [f14565, f8346])).
fof(f14565, plain, (~ r1(sK28, sK27(sK28)) | (spl41_187 | ~ spl41_772)), inference(subsumption_resolution, [], [f14564, f4730])).
fof(f14564, plain, (p2(sK27(sK28)) | ~ r1(sK28, sK27(sK28)) | ~ spl41_772), inference(resolution, [], [f14511, f144])).
fof(f14511, plain, (p2(sK29(sK27(sK28))) | ~ spl41_772), inference(avatar_component_clause, [], [f14509])).
fof(f14509, plain, (spl41_772 <=> p2(sK29(sK27(sK28)))), introduced(avatar_definition, [new_symbols(naming, [spl41_772])])).
fof(f14512, plain, (spl41_771 | spl41_772 | spl41_187 | ~ spl41_195 | ~ spl41_267 | ~ spl41_268 | ~ spl41_379), inference(avatar_split_clause, [], [f14504, f10182, f8345, f7598, f4782, f4729, f14509, f14506])).
fof(f10182, plain, (spl41_379 <=> r1(sK23(sK29(sK27(sK28))), sK24(sK29(sK27(sK28))))), introduced(avatar_definition, [new_symbols(naming, [spl41_379])])).
fof(f14504, plain, (! [X0, X1] : (p2(sK29(sK27(sK28))) | ~ r1(X0, sK29(sK27(sK28))) | ~ r1(X1, X0) | ~ sP1(X1)) | (spl41_187 | ~ spl41_195 | ~ spl41_267 | ~ spl41_268 | ~ spl41_379)), inference(resolution, [], [f14094, f110])).
fof(f14094, plain, (p2(sK24(sK29(sK27(sK28)))) | (spl41_187 | ~ spl41_195 | ~ spl41_267 | ~ spl41_268 | ~ spl41_379)), inference(resolution, [], [f10184, f12364])).
fof(f12364, plain, (! [X0] : (~ r1(sK23(sK29(sK27(sK28))), X0) | p2(X0)) | (spl41_187 | ~ spl41_195 | ~ spl41_267 | ~ spl41_268)), inference(subsumption_resolution, [], [f12363, f8346])).
fof(f12363, plain, (! [X0] : (p2(X0) | ~ r1(sK23(sK29(sK27(sK28))), X0) | ~ r1(sK28, sK27(sK28))) | (spl41_187 | ~ spl41_195 | ~ spl41_267)), inference(subsumption_resolution, [], [f12362, f4730])).
fof(f12362, plain, (! [X0] : (p2(X0) | ~ r1(sK23(sK29(sK27(sK28))), X0) | p2(sK27(sK28)) | ~ r1(sK28, sK27(sK28))) | (~ spl41_195 | ~ spl41_267)), inference(subsumption_resolution, [], [f12312, f4784])).
fof(f4784, plain, (p2(sK23(sK29(sK27(sK28)))) | ~ spl41_195), inference(avatar_component_clause, [], [f4782])).
fof(f12312, plain, (! [X0] : (p2(X0) | ~ r1(sK23(sK29(sK27(sK28))), X0) | ~ p2(sK23(sK29(sK27(sK28)))) | p2(sK27(sK28)) | ~ r1(sK28, sK27(sK28))) | ~ spl41_267), inference(resolution, [], [f7600, f145])).
fof(f7600, plain, (r1(sK29(sK27(sK28)), sK23(sK29(sK27(sK28)))) | ~ spl41_267), inference(avatar_component_clause, [], [f7598])).
fof(f10184, plain, (r1(sK23(sK29(sK27(sK28))), sK24(sK29(sK27(sK28)))) | ~ spl41_379), inference(avatar_component_clause, [], [f10182])).
fof(f12133, plain, (~ spl41_1 | spl41_46 | ~ spl41_200), inference(avatar_contradiction_clause, [], [f12132])).
fof(f12132, plain, ($false | (~ spl41_1 | spl41_46 | ~ spl41_200)), inference(subsumption_resolution, [], [f12131, f149])).
fof(f12131, plain, (~ sP6(sK28) | (spl41_46 | ~ spl41_200)), inference(subsumption_resolution, [], [f12130, f389])).
fof(f12130, plain, (sP1(sK28) | ~ sP6(sK28) | ~ spl41_200), inference(resolution, [], [f4910, f75])).
fof(f4910, plain, (p2(sK9(sK28)) | ~ spl41_200), inference(avatar_component_clause, [], [f4908])).
fof(f11310, plain, (spl41_440 | spl41_200 | ~ spl41_9 | ~ spl41_210), inference(avatar_split_clause, [], [f11305, f7255, f180, f4908, f11307])).
fof(f7255, plain, (spl41_210 <=> r1(sK28, sK9(sK28))), introduced(avatar_definition, [new_symbols(naming, [spl41_210])])).
fof(f11305, plain, (p2(sK9(sK28)) | p2(sK25(sK9(sK28))) | (~ spl41_9 | ~ spl41_210)), inference(subsumption_resolution, [], [f11262, f182])).
fof(f11262, plain, (p2(sK9(sK28)) | p2(sK25(sK9(sK28))) | ~ sP0(sK28) | ~ spl41_210), inference(resolution, [], [f7256, f117])).
fof(f7256, plain, (r1(sK28, sK9(sK28)) | ~ spl41_210), inference(avatar_component_clause, [], [f7255])).
fof(f11304, plain, (spl41_439 | spl41_200 | ~ spl41_9 | ~ spl41_210), inference(avatar_split_clause, [], [f11299, f7255, f180, f4908, f11301])).
fof(f11299, plain, (p2(sK9(sK28)) | r1(sK25(sK9(sK28)), sK26(sK9(sK28))) | (~ spl41_9 | ~ spl41_210)), inference(subsumption_resolution, [], [f11261, f182])).
fof(f11261, plain, (p2(sK9(sK28)) | r1(sK25(sK9(sK28)), sK26(sK9(sK28))) | ~ sP0(sK28) | ~ spl41_210), inference(resolution, [], [f7256, f115])).
fof(f11298, plain, (spl41_438 | spl41_200 | ~ spl41_9 | ~ spl41_210), inference(avatar_split_clause, [], [f11293, f7255, f180, f4908, f11295])).
fof(f11293, plain, (p2(sK9(sK28)) | r1(sK9(sK28), sK25(sK9(sK28))) | (~ spl41_9 | ~ spl41_210)), inference(subsumption_resolution, [], [f11260, f182])).
fof(f11260, plain, (p2(sK9(sK28)) | r1(sK9(sK28), sK25(sK9(sK28))) | ~ sP0(sK28) | ~ spl41_210), inference(resolution, [], [f7256, f114])).
fof(f10185, plain, (spl41_379 | spl41_196 | spl41_187 | ~ spl41_268), inference(avatar_split_clause, [], [f10180, f8345, f4729, f4786, f10182])).
fof(f10180, plain, (! [X2] : (~ r1(X2, sK27(sK28)) | ~ sP1(X2) | r1(sK23(sK29(sK27(sK28))), sK24(sK29(sK27(sK28))))) | (spl41_187 | ~ spl41_268)), inference(subsumption_resolution, [], [f10163, f4730])).
fof(f10163, plain, (! [X2] : (~ r1(X2, sK27(sK28)) | ~ sP1(X2) | p2(sK27(sK28)) | r1(sK23(sK29(sK27(sK28))), sK24(sK29(sK27(sK28))))) | ~ spl41_268), inference(resolution, [], [f1449, f8346])).
fof(f8625, plain, (~ spl41_9 | spl41_268), inference(avatar_contradiction_clause, [], [f8624])).
fof(f8624, plain, ($false | (~ spl41_9 | spl41_268)), inference(subsumption_resolution, [], [f8623, f182])).
fof(f8623, plain, (~ sP0(sK28) | spl41_268), inference(resolution, [], [f8347, f112])).
fof(f112, plain, ! [X0] : (r1(X0, sK27(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f57])).
fof(f8347, plain, (~ r1(sK28, sK27(sK28)) | spl41_268), inference(avatar_component_clause, [], [f8345])).
fof(f7701, plain, (~ spl41_9 | ~ spl41_187), inference(avatar_contradiction_clause, [], [f7700])).
fof(f7700, plain, ($false | (~ spl41_9 | ~ spl41_187)), inference(subsumption_resolution, [], [f7699, f182])).
fof(f7699, plain, (~ sP0(sK28) | ~ spl41_187), inference(resolution, [], [f4731, f113])).
fof(f113, plain, ! [X0] : (~ p2(sK27(X0)) | ~ sP0(X0)), inference(cnf_transformation, [], [f57])).
fof(f4731, plain, (p2(sK27(sK28)) | ~ spl41_187), inference(avatar_component_clause, [], [f4729])).
fof(f7543, plain, (~ spl41_1 | spl41_46 | spl41_210), inference(avatar_split_clause, [], [f7399, f7255, f388, f147])).
fof(f7399, plain, (sP1(sK28) | ~ sP6(sK28) | spl41_210), inference(resolution, [], [f7257, f74])).
fof(f7257, plain, (~ r1(sK28, sK9(sK28)) | spl41_210), inference(avatar_component_clause, [], [f7255])).
fof(f7527, plain, (~ spl41_4 | ~ spl41_73), inference(avatar_contradiction_clause, [], [f7526])).
fof(f7526, plain, ($false | (~ spl41_4 | ~ spl41_73)), inference(subsumption_resolution, [], [f7525, f161])).
fof(f7525, plain, (~ sP2(sK30) | ~ spl41_73), inference(resolution, [], [f550, f3956])).
fof(f3956, plain, ! [X1] : (~ sP5(sK21(X1)) | ~ sP2(X1)), inference(duplicate_literal_removal, [], [f3955])).
fof(f3955, plain, ! [X1] : (~ sP5(sK21(X1)) | ~ sP2(X1) | ~ sP5(sK21(X1)) | ~ sP2(X1)), inference(resolution, [], [f3899, f101])).
fof(f101, plain, ! [X0] : (r1(sK21(X0), sK22(X0)) | ~ sP2(X0)), inference(cnf_transformation, [], [f46])).
fof(f3899, plain, ! [X0, X1] : (~ r1(X1, sK22(X0)) | ~ sP5(sK21(X0)) | ~ sP2(X0) | ~ sP5(X1)), inference(subsumption_resolution, [], [f3898, f102])).
fof(f102, plain, ! [X0] : (~ p2(sK22(X0)) | ~ sP2(X0)), inference(cnf_transformation, [], [f46])).
fof(f3898, plain, ! [X0, X1] : (~ sP2(X0) | ~ sP5(sK21(X0)) | p2(sK22(X0)) | ~ r1(X1, sK22(X0)) | ~ sP5(X1)), inference(resolution, [], [f3868, f86])).
fof(f86, plain, ! [X0, X1] : (~ p2(sK11(X1)) | p2(X1) | ~ r1(X0, X1) | ~ sP5(X0)), inference(cnf_transformation, [], [f27])).
fof(f27, plain, ! [X0] : (! [X1] : ((((p2(sK10(X1)) & (~ p2(sK11(X1)) & r1(sK10(X1), sK11(X1))) & r1(X1, sK10(X1))) | p2(X1)) & ((! [X5] : (~ p2(X5) | ! [X6] : (p2(X6) | ~ r1(X5, X6)) | ~ r1(sK12(X1), X5)) & ~ p2(sK12(X1)) & r1(X1, sK12(X1))) | sP3(X1))) | ~ r1(X0, X1)) | ~ sP5(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK10, sK11, sK12])], [f23, f26, f25, f24])).
fof(f24, plain, ! [X1] : (? [X2] : (p2(X2) & ? [X3] : (~ p2(X3) & r1(X2, X3)) & r1(X1, X2)) => (p2(sK10(X1)) & ? [X3] : (~ p2(X3) & r1(sK10(X1), X3)) & r1(X1, sK10(X1)))), introduced(choice_axiom, [])).
fof(f25, plain, ! [X1] : (? [X3] : (~ p2(X3) & r1(sK10(X1), X3)) => (~ p2(sK11(X1)) & r1(sK10(X1), sK11(X1)))), introduced(choice_axiom, [])).
fof(f26, plain, ! [X1] : (? [X4] : (! [X5] : (~ p2(X5) | ! [X6] : (p2(X6) | ~ r1(X5, X6)) | ~ r1(X4, X5)) & ~ p2(X4) & r1(X1, X4)) => (! [X5] : (~ p2(X5) | ! [X6] : (p2(X6) | ~ r1(X5, X6)) | ~ r1(sK12(X1), X5)) & ~ p2(sK12(X1)) & r1(X1, sK12(X1)))), introduced(choice_axiom, [])).
fof(f23, plain, ! [X0] : (! [X1] : (((? [X2] : (p2(X2) & ? [X3] : (~ p2(X3) & r1(X2, X3)) & r1(X1, X2)) | p2(X1)) & (? [X4] : (! [X5] : (~ p2(X5) | ! [X6] : (p2(X6) | ~ r1(X5, X6)) | ~ r1(X4, X5)) & ~ p2(X4) & r1(X1, X4)) | sP3(X1))) | ~ r1(X0, X1)) | ~ sP5(X0)), inference(rectify, [], [f22])).
fof(f22, plain, ! [X6] : (! [X16] : (((? [X17] : (p2(X17) & ? [X18] : (~ p2(X18) & r1(X17, X18)) & r1(X16, X17)) | p2(X16)) & (? [X19] : (! [X20] : (~ p2(X20) | ! [X21] : (p2(X21) | ~ r1(X20, X21)) | ~ r1(X19, X20)) & ~ p2(X19) & r1(X16, X19)) | sP3(X16))) | ~ r1(X6, X16)) | ~ sP5(X6)), inference(nnf_transformation, [], [f13])).
fof(f3868, plain, ! [X0] : (p2(sK11(sK22(X0))) | ~ sP2(X0) | ~ sP5(sK21(X0))), inference(duplicate_literal_removal, [], [f3850])).
fof(f3850, plain, ! [X0] : (~ sP2(X0) | p2(sK11(sK22(X0))) | ~ sP5(sK21(X0)) | ~ sP5(sK21(X0)) | ~ sP2(X0)), inference(resolution, [], [f2030, f849])).
fof(f849, plain, ! [X11] : (r1(sK10(sK22(X11)), sK11(sK22(X11))) | ~ sP5(sK21(X11)) | ~ sP2(X11)), inference(subsumption_resolution, [], [f838, f102])).
fof(f838, plain, ! [X11] : (p2(sK22(X11)) | r1(sK10(sK22(X11)), sK11(sK22(X11))) | ~ sP5(sK21(X11)) | ~ sP2(X11)), inference(resolution, [], [f85, f101])).
fof(f85, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | r1(sK10(X1), sK11(X1)) | ~ sP5(X0)), inference(cnf_transformation, [], [f27])).
fof(f2030, plain, ! [X0, X1] : (~ r1(sK10(sK22(X0)), X1) | ~ sP2(X0) | p2(X1) | ~ sP5(sK21(X0))), inference(subsumption_resolution, [], [f2029, f592])).
fof(f592, plain, ! [X11] : (p2(sK10(sK22(X11))) | ~ sP5(sK21(X11)) | ~ sP2(X11)), inference(subsumption_resolution, [], [f581, f102])).
fof(f581, plain, ! [X11] : (p2(sK22(X11)) | p2(sK10(sK22(X11))) | ~ sP5(sK21(X11)) | ~ sP2(X11)), inference(resolution, [], [f87, f101])).
fof(f87, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | p2(sK10(X1)) | ~ sP5(X0)), inference(cnf_transformation, [], [f27])).
fof(f2029, plain, ! [X0, X1] : (~ sP5(sK21(X0)) | ~ sP2(X0) | p2(X1) | ~ r1(sK10(sK22(X0)), X1) | ~ p2(sK10(sK22(X0)))), inference(duplicate_literal_removal, [], [f2007])).
fof(f2007, plain, ! [X0, X1] : (~ sP5(sK21(X0)) | ~ sP2(X0) | p2(X1) | ~ r1(sK10(sK22(X0)), X1) | ~ p2(sK10(sK22(X0))) | ~ sP2(X0)), inference(resolution, [], [f746, f103])).
fof(f103, plain, ! [X6, X0, X7] : (~ r1(sK22(X0), X6) | p2(X7) | ~ r1(X6, X7) | ~ p2(X6) | ~ sP2(X0)), inference(cnf_transformation, [], [f46])).
fof(f746, plain, ! [X11] : (r1(sK22(X11), sK10(sK22(X11))) | ~ sP5(sK21(X11)) | ~ sP2(X11)), inference(subsumption_resolution, [], [f735, f102])).
fof(f735, plain, ! [X11] : (p2(sK22(X11)) | r1(sK22(X11), sK10(sK22(X11))) | ~ sP5(sK21(X11)) | ~ sP2(X11)), inference(resolution, [], [f84, f101])).
fof(f84, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | r1(X1, sK10(X1)) | ~ sP5(X0)), inference(cnf_transformation, [], [f27])).
fof(f550, plain, (sP5(sK21(sK30)) | ~ spl41_73), inference(avatar_component_clause, [], [f548])).
fof(f548, plain, (spl41_73 <=> sP5(sK21(sK30))), introduced(avatar_definition, [new_symbols(naming, [spl41_73])])).
fof(f7524, plain, (~ spl41_4 | ~ spl41_72), inference(avatar_contradiction_clause, [], [f7523])).
fof(f7523, plain, ($false | (~ spl41_4 | ~ spl41_72)), inference(subsumption_resolution, [], [f7522, f161])).
fof(f7522, plain, (~ sP2(sK30) | ~ spl41_72), inference(resolution, [], [f546, f4557])).
fof(f4557, plain, ! [X1] : (~ sP4(sK21(X1)) | ~ sP2(X1)), inference(duplicate_literal_removal, [], [f4556])).
fof(f4556, plain, ! [X1] : (~ sP4(sK21(X1)) | ~ sP2(X1) | ~ sP4(sK21(X1)) | ~ sP2(X1)), inference(resolution, [], [f4554, f101])).
fof(f4554, plain, ! [X0, X1] : (~ r1(X1, sK22(X0)) | ~ sP4(sK21(X0)) | ~ sP2(X0) | ~ sP4(X1)), inference(subsumption_resolution, [], [f4553, f102])).
fof(f4553, plain, ! [X0, X1] : (~ sP2(X0) | ~ sP4(sK21(X0)) | p2(sK22(X0)) | ~ r1(X1, sK22(X0)) | ~ sP4(X1)), inference(resolution, [], [f4550, f94])).
fof(f94, plain, ! [X0, X1] : (~ p2(sK14(X1)) | p2(X1) | ~ r1(X0, X1) | ~ sP4(X0)), inference(cnf_transformation, [], [f34])).
fof(f34, plain, ! [X0] : ((! [X1] : ((p2(sK13(X1)) & (~ p2(sK14(X1)) & r1(sK13(X1), sK14(X1))) & r1(X1, sK13(X1))) | p2(X1) | ~ r1(X0, X1)) & ((! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(sK16(X0), X6)) & ~ p2(sK16(X0)) & r1(sK15(X0), sK16(X0))) & r1(X0, sK15(X0)))) | ~ sP4(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK13, sK14, sK15, sK16])], [f29, f33, f32, f31, f30])).
fof(f30, plain, ! [X1] : (? [X2] : (p2(X2) & ? [X3] : (~ p2(X3) & r1(X2, X3)) & r1(X1, X2)) => (p2(sK13(X1)) & ? [X3] : (~ p2(X3) & r1(sK13(X1), X3)) & r1(X1, sK13(X1)))), introduced(choice_axiom, [])).
fof(f31, plain, ! [X1] : (? [X3] : (~ p2(X3) & r1(sK13(X1), X3)) => (~ p2(sK14(X1)) & r1(sK13(X1), sK14(X1)))), introduced(choice_axiom, [])).
fof(f32, plain, ! [X0] : (? [X4] : (? [X5] : (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(X5, X6)) & ~ p2(X5) & r1(X4, X5)) & r1(X0, X4)) => (? [X5] : (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(X5, X6)) & ~ p2(X5) & r1(sK15(X0), X5)) & r1(X0, sK15(X0)))), introduced(choice_axiom, [])).
fof(f33, plain, ! [X0] : (? [X5] : (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(X5, X6)) & ~ p2(X5) & r1(sK15(X0), X5)) => (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(sK16(X0), X6)) & ~ p2(sK16(X0)) & r1(sK15(X0), sK16(X0)))), introduced(choice_axiom, [])).
fof(f29, plain, ! [X0] : ((! [X1] : (? [X2] : (p2(X2) & ? [X3] : (~ p2(X3) & r1(X2, X3)) & r1(X1, X2)) | p2(X1) | ~ r1(X0, X1)) & ? [X4] : (? [X5] : (! [X6] : (~ p2(X6) | ! [X7] : (p2(X7) | ~ r1(X6, X7)) | ~ r1(X5, X6)) & ~ p2(X5) & r1(X4, X5)) & r1(X0, X4))) | ~ sP4(X0)), inference(rectify, [], [f28])).
fof(f28, plain, ! [X6] : ((! [X9] : (? [X10] : (p2(X10) & ? [X11] : (~ p2(X11) & r1(X10, X11)) & r1(X9, X10)) | p2(X9) | ~ r1(X6, X9)) & ? [X12] : (? [X13] : (! [X14] : (~ p2(X14) | ! [X15] : (p2(X15) | ~ r1(X14, X15)) | ~ r1(X13, X14)) & ~ p2(X13) & r1(X12, X13)) & r1(X6, X12))) | ~ sP4(X6)), inference(nnf_transformation, [], [f12])).
fof(f4550, plain, ! [X0] : (p2(sK14(sK22(X0))) | ~ sP2(X0) | ~ sP4(sK21(X0))), inference(duplicate_literal_removal, [], [f4532])).
fof(f4532, plain, ! [X0] : (~ sP2(X0) | p2(sK14(sK22(X0))) | ~ sP4(sK21(X0)) | ~ sP4(sK21(X0)) | ~ sP2(X0)), inference(resolution, [], [f2099, f873])).
fof(f873, plain, ! [X11] : (r1(sK13(sK22(X11)), sK14(sK22(X11))) | ~ sP4(sK21(X11)) | ~ sP2(X11)), inference(subsumption_resolution, [], [f861, f102])).
fof(f861, plain, ! [X11] : (p2(sK22(X11)) | r1(sK13(sK22(X11)), sK14(sK22(X11))) | ~ sP4(sK21(X11)) | ~ sP2(X11)), inference(resolution, [], [f93, f101])).
fof(f93, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | r1(sK13(X1), sK14(X1)) | ~ sP4(X0)), inference(cnf_transformation, [], [f34])).
fof(f2099, plain, ! [X0, X1] : (~ r1(sK13(sK22(X0)), X1) | ~ sP2(X0) | p2(X1) | ~ sP4(sK21(X0))), inference(subsumption_resolution, [], [f2098, f644])).
fof(f644, plain, ! [X11] : (p2(sK13(sK22(X11))) | ~ sP4(sK21(X11)) | ~ sP2(X11)), inference(subsumption_resolution, [], [f632, f102])).
fof(f632, plain, ! [X11] : (p2(sK22(X11)) | p2(sK13(sK22(X11))) | ~ sP4(sK21(X11)) | ~ sP2(X11)), inference(resolution, [], [f95, f101])).
fof(f95, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | p2(sK13(X1)) | ~ sP4(X0)), inference(cnf_transformation, [], [f34])).
fof(f2098, plain, ! [X0, X1] : (~ sP4(sK21(X0)) | ~ sP2(X0) | p2(X1) | ~ r1(sK13(sK22(X0)), X1) | ~ p2(sK13(sK22(X0)))), inference(duplicate_literal_removal, [], [f2076])).
fof(f2076, plain, ! [X0, X1] : (~ sP4(sK21(X0)) | ~ sP2(X0) | p2(X1) | ~ r1(sK13(sK22(X0)), X1) | ~ p2(sK13(sK22(X0))) | ~ sP2(X0)), inference(resolution, [], [f770, f103])).
fof(f770, plain, ! [X11] : (r1(sK22(X11), sK13(sK22(X11))) | ~ sP4(sK21(X11)) | ~ sP2(X11)), inference(subsumption_resolution, [], [f758, f102])).
fof(f758, plain, ! [X11] : (p2(sK22(X11)) | r1(sK22(X11), sK13(sK22(X11))) | ~ sP4(sK21(X11)) | ~ sP2(X11)), inference(resolution, [], [f92, f101])).
fof(f92, plain, ! [X0, X1] : (~ r1(X0, X1) | p2(X1) | r1(X1, sK13(X1)) | ~ sP4(X0)), inference(cnf_transformation, [], [f34])).
fof(f546, plain, (sP4(sK21(sK30)) | ~ spl41_72), inference(avatar_component_clause, [], [f544])).
fof(f544, plain, (spl41_72 <=> sP4(sK21(sK30))), introduced(avatar_definition, [new_symbols(naming, [spl41_72])])).
fof(f1501, plain, (~ spl41_4 | spl41_72 | spl41_73 | spl41_150 | ~ spl41_2), inference(avatar_split_clause, [], [f1469, f151, f1482, f548, f544, f159])).
fof(f151, plain, (spl41_2 <=> ! [X7, X8, X6] : (~ p2(X7) | ~ r1(sK30, X6) | sP5(X6) | sP4(X6) | ~ r1(X6, X7) | ~ r1(X7, X8) | p2(X8))), introduced(avatar_definition, [new_symbols(naming, [spl41_2])])).
fof(f1469, plain, (! [X8, X9] : (~ p2(X8) | sP5(sK21(sK30)) | sP4(sK21(sK30)) | ~ r1(sK21(sK30), X8) | ~ r1(X8, X9) | p2(X9) | ~ sP2(sK30)) | ~ spl41_2), inference(resolution, [], [f152, f100])).
fof(f152, plain, (! [X6, X8, X7] : (~ r1(sK30, X6) | ~ p2(X7) | sP5(X6) | sP4(X6) | ~ r1(X6, X7) | ~ r1(X7, X8) | p2(X8)) | ~ spl41_2), inference(avatar_component_clause, [], [f151])).
fof(f555, plain, (spl41_72 | spl41_73 | ~ spl41_74 | ~ spl41_3 | ~ spl41_4), inference(avatar_split_clause, [], [f542, f159, f155, f552, f548, f544])).
fof(f155, plain, (spl41_3 <=> ! [X6] : (~ p2(X6) | ~ r1(sK30, X6) | sP5(X6) | sP4(X6))), introduced(avatar_definition, [new_symbols(naming, [spl41_3])])).
fof(f542, plain, (~ p2(sK21(sK30)) | sP5(sK21(sK30)) | sP4(sK21(sK30)) | (~ spl41_3 | ~ spl41_4)), inference(subsumption_resolution, [], [f501, f161])).
fof(f501, plain, (~ p2(sK21(sK30)) | sP5(sK21(sK30)) | sP4(sK21(sK30)) | ~ sP2(sK30) | ~ spl41_3), inference(resolution, [], [f156, f100])).
fof(f156, plain, (! [X6] : (~ r1(sK30, X6) | ~ p2(X6) | sP5(X6) | sP4(X6)) | ~ spl41_3), inference(avatar_component_clause, [], [f155])).
fof(f334, plain, (~ spl41_8 | ~ spl41_10 | ~ spl41_11), inference(avatar_contradiction_clause, [], [f333])).
fof(f333, plain, ($false | (~ spl41_8 | ~ spl41_10 | ~ spl41_11)), inference(subsumption_resolution, [], [f332, f314])).
fof(f314, plain, (p3(sK34(sK28)) | ~ spl41_8), inference(resolution, [], [f178, f73])).
fof(f73, plain, ! [X0] : r1(X0, X0), inference(cnf_transformation, [], [f1])).
fof(f1, plain, ! [X0] : r1(X0, X0), file('/home/ubuntu/library/tptp/Problems/LCL/LCL660+1.001.p', reflexivity)).
fof(f178, plain, (! [X15] : (~ r1(sK28, X15) | p3(sK34(X15))) | ~ spl41_8), inference(avatar_component_clause, [], [f177])).
fof(f177, plain, (spl41_8 <=> ! [X15] : (p3(sK34(X15)) | ~ r1(sK28, X15))), introduced(avatar_definition, [new_symbols(naming, [spl41_8])])).
fof(f332, plain, (~ p3(sK34(sK28)) | (~ spl41_10 | ~ spl41_11)), inference(subsumption_resolution, [], [f320, f73])).
fof(f320, plain, (~ r1(sK28, sK28) | ~ p3(sK34(sK28)) | (~ spl41_10 | ~ spl41_11)), inference(resolution, [], [f186, f190])).
fof(f190, plain, (! [X25] : (~ r1(sK28, X25) | ~ p3(X25)) | ~ spl41_11), inference(avatar_component_clause, [], [f189])).
fof(f189, plain, (spl41_11 <=> ! [X25] : (~ p3(X25) | ~ r1(sK28, X25))), introduced(avatar_definition, [new_symbols(naming, [spl41_11])])).
fof(f186, plain, (! [X15] : (r1(X15, sK34(X15)) | ~ r1(sK28, X15)) | ~ spl41_10), inference(avatar_component_clause, [], [f185])).
fof(f185, plain, (spl41_10 <=> ! [X15] : (r1(X15, sK34(X15)) | ~ r1(sK28, X15))), introduced(avatar_definition, [new_symbols(naming, [spl41_10])])).
fof(f216, plain, (spl41_11 | spl41_17), inference(avatar_split_clause, [], [f118, f213, f189])).
fof(f118, plain, ! [X25] : (r1(sK28, sK40) | ~ p3(X25) | ~ r1(sK28, X25)), inference(cnf_transformation, [], [f72])).
fof(f211, plain, (spl41_11 | ~ spl41_16), inference(avatar_split_clause, [], [f119, f208, f189])).
fof(f119, plain, ! [X25] : (~ p2(sK40) | ~ p3(X25) | ~ r1(sK28, X25)), inference(cnf_transformation, [], [f72])).
fof(f206, plain, (spl41_11 | spl41_15), inference(avatar_split_clause, [], [f120, f204, f189])).
fof(f120, plain, ! [X21, X25] : (r1(X21, sK38(X21)) | p2(X21) | ~ r1(sK28, X21) | ~ p3(X25) | ~ r1(sK28, X25)), inference(cnf_transformation, [], [f72])).
fof(f202, plain, (spl41_11 | spl41_14), inference(avatar_split_clause, [], [f121, f200, f189])).
fof(f121, plain, ! [X21, X25] : (r1(sK38(X21), sK39(X21)) | p2(X21) | ~ r1(sK28, X21) | ~ p3(X25) | ~ r1(sK28, X25)), inference(cnf_transformation, [], [f72])).
fof(f198, plain, (spl41_11 | spl41_13), inference(avatar_split_clause, [], [f122, f196, f189])).
fof(f122, plain, ! [X21, X25] : (~ p2(sK39(X21)) | p2(X21) | ~ r1(sK28, X21) | ~ p3(X25) | ~ r1(sK28, X25)), inference(cnf_transformation, [], [f72])).
fof(f194, plain, (spl41_11 | spl41_12), inference(avatar_split_clause, [], [f123, f192, f189])).
fof(f123, plain, ! [X21, X25] : (p2(sK38(X21)) | p2(X21) | ~ r1(sK28, X21) | ~ p3(X25) | ~ r1(sK28, X25)), inference(cnf_transformation, [], [f72])).
fof(f187, plain, (spl41_10 | spl41_9), inference(avatar_split_clause, [], [f130, f180, f185])).
fof(f130, plain, ! [X15] : (sP0(sK28) | r1(X15, sK34(X15)) | ~ r1(sK28, X15)), inference(cnf_transformation, [], [f72])).
fof(f183, plain, (spl41_8 | spl41_9), inference(avatar_split_clause, [], [f131, f180, f177])).
fof(f131, plain, ! [X15] : (sP0(sK28) | p3(sK34(X15)) | ~ r1(sK28, X15)), inference(cnf_transformation, [], [f72])).
fof(f175, plain, (spl41_1 | spl41_7), inference(avatar_split_clause, [], [f138, f172, f147])).
fof(f138, plain, (r1(sK28, sK30) | sP6(sK28)), inference(cnf_transformation, [], [f72])).
fof(f170, plain, (spl41_1 | spl41_4 | ~ spl41_6), inference(avatar_split_clause, [], [f139, f167, f159, f147])).
fof(f139, plain, (~ p2(sK30) | sP2(sK30) | sP6(sK28)), inference(cnf_transformation, [], [f72])).
fof(f165, plain, (spl41_1 | spl41_4 | spl41_5), inference(avatar_split_clause, [], [f140, f163, f159, f147])).
fof(f140, plain, ! [X10, X9] : (~ p2(X9) | p2(X10) | ~ r1(X9, X10) | ~ r1(sK30, X9) | sP2(sK30) | sP6(sK28)), inference(cnf_transformation, [], [f72])).
fof(f157, plain, (spl41_1 | spl41_3), inference(avatar_split_clause, [], [f141, f155, f147])).
fof(f141, plain, ! [X6] : (~ p2(X6) | sP4(X6) | sP5(X6) | ~ r1(sK30, X6) | sP6(sK28)), inference(cnf_transformation, [], [f72])).
fof(f153, plain, (spl41_1 | spl41_2), inference(avatar_split_clause, [], [f142, f151, f147])).
fof(f142, plain, ! [X6, X8, X7] : (~ p2(X7) | p2(X8) | ~ r1(X7, X8) | ~ r1(X6, X7) | sP4(X6) | sP5(X6) | ~ r1(sK30, X6) | sP6(sK28)), inference(cnf_transformation, [], [f72])).