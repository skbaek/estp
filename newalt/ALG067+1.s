fof(f1147, plain, $false, inference(avatar_sat_refutation, [], [f960, f962, f967, f969, f974, f976, f981, f983, f988, f990, f995, f997, f1002, f1004, f1009, f1011, f1016, f1018, f1023, f1025, f1030, f1032, f1037, f1039, f1044, f1046, f1051, f1053, f1058, f1060, f1065, f1067, f1072, f1074, f1079, f1081, f1086, f1088, f1093, f1095, f1100, f1102, f1107, f1109, f1114, f1116, f1121, f1123, f1124, f1126, f1127, f1128, f1129, f1130, f1131, f1132, f1133, f1134, f1135, f1136, f1137, f1138, f1139, f1140, f1141, f1142, f1143, f1144, f1145, f1146])).
fof(f1146, plain, (~ spl24_122 | spl24_116), inference(avatar_split_clause, [], [f331, f842, f867])).
fof(f867, plain, (spl24_122 <=> (op(e0, e0) = e1)), introduced(avatar_definition, [new_symbols(naming, [spl24_122])])).
fof(f842, plain, (spl24_116 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_116])])).
fof(f331, plain, ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)), inference(cnf_transformation, [], [f34])).
fof(f34, plain, (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(definition_folding, [], [f9, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
fof(f10, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(usedef, [], [e10])).
fof(e10, plain, (sP0 <=> (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f11, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(usedef, [], [e11])).
fof(e11, plain, (sP1 <=> (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f12, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(usedef, [], [e12])).
fof(e12, plain, (sP2 <=> (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f13, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(usedef, [], [e13])).
fof(e13, plain, (sP3 <=> (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f14, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP4), inference(usedef, [], [e14])).
fof(e14, plain, (sP4 <=> (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f15, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP5), inference(usedef, [], [e15])).
fof(e15, plain, (sP5 <=> (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f16, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP6), inference(usedef, [], [e16])).
fof(e16, plain, (sP6 <=> (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f17, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP7), inference(usedef, [], [e17])).
fof(e17, plain, (sP7 <=> (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f18, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP8), inference(usedef, [], [e18])).
fof(e18, plain, (sP8 <=> (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f19, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP9), inference(usedef, [], [e19])).
fof(e19, plain, (sP9 <=> (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f20, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP10), inference(usedef, [], [e20])).
fof(e20, plain, (sP10 <=> (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f21, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP11), inference(usedef, [], [e21])).
fof(e21, plain, (sP11 <=> (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f22, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP12), inference(usedef, [], [e22])).
fof(e22, plain, (sP12 <=> (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f23, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP13), inference(usedef, [], [e23])).
fof(e23, plain, (sP13 <=> (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f24, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP14), inference(usedef, [], [e24])).
fof(e24, plain, (sP14 <=> (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f25, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP15), inference(usedef, [], [e25])).
fof(e25, plain, (sP15 <=> (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f26, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP16), inference(usedef, [], [e26])).
fof(e26, plain, (sP16 <=> (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f27, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP17), inference(usedef, [], [e27])).
fof(e27, plain, (sP17 <=> (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f28, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP18), inference(usedef, [], [e28])).
fof(e28, plain, (sP18 <=> (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f29, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP19), inference(usedef, [], [e29])).
fof(e29, plain, (sP19 <=> (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f30, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP20), inference(usedef, [], [e30])).
fof(e30, plain, (sP20 <=> (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f31, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP21), inference(usedef, [], [e31])).
fof(e31, plain, (sP21 <=> (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f32, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP22), inference(usedef, [], [e32])).
fof(e32, plain, (sP22 <=> (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f33, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP23), inference(usedef, [], [e33])).
fof(e33, plain, (sP23 <=> (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f9, plain, (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0)))) & ((e4 = op(e4, e4)) | ~ (e4 = op(e4, e4))) & ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))) & ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))) & ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))) & ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))) & ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))) & ((e3 = op(e3, e3)) | ~ (e3 = op(e3, e3))) & ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))) & ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))) & ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))) & ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))) & ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))) & ((e2 = op(e2, e2)) | ~ (e2 = op(e2, e2))) & ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))) & ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))) & ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))) & ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))) & ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))) & ((e1 = op(e1, e1)) | ~ (e1 = op(e1, e1))) & ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))) & ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)) & ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)) & ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)) & ((e0 = op(e0, e1)) | ~ (op(e0, e0) = e1)) & ((e0 = op(e0, e0)) | ~ (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG067+1.p', co1)).
fof(f1145, plain, (~ spl24_123 | spl24_111), inference(avatar_split_clause, [], [f332, f821, f871])).
fof(f871, plain, (spl24_123 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl24_123])])).
fof(f821, plain, (spl24_111 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_111])])).
fof(f332, plain, ((e0 = op(e0, e2)) | ~ (op(e0, e0) = e2)), inference(cnf_transformation, [], [f34])).
fof(f1144, plain, (~ spl24_124 | spl24_106), inference(avatar_split_clause, [], [f333, f800, f875])).
fof(f875, plain, (spl24_124 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl24_124])])).
fof(f800, plain, (spl24_106 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_106])])).
fof(f333, plain, ((e0 = op(e0, e3)) | ~ (op(e0, e0) = e3)), inference(cnf_transformation, [], [f34])).
fof(f1143, plain, (~ spl24_125 | spl24_101), inference(avatar_split_clause, [], [f334, f779, f879])).
fof(f879, plain, (spl24_125 <=> (op(e0, e0) = e4)), introduced(avatar_definition, [new_symbols(naming, [spl24_125])])).
fof(f779, plain, (spl24_101 <=> (e0 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_101])])).
fof(f334, plain, ((e0 = op(e0, e4)) | ~ (op(e0, e0) = e4)), inference(cnf_transformation, [], [f34])).
fof(f1142, plain, (~ spl24_91 | spl24_97), inference(avatar_split_clause, [], [f335, f762, f737])).
fof(f737, plain, (spl24_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_91])])).
fof(f762, plain, (spl24_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_97])])).
fof(f335, plain, ((e1 = op(e1, e0)) | ~ (e0 = op(e1, e1))), inference(cnf_transformation, [], [f34])).
fof(f1141, plain, (~ spl24_93 | spl24_87), inference(avatar_split_clause, [], [f337, f720, f745])).
fof(f745, plain, (spl24_93 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_93])])).
fof(f720, plain, (spl24_87 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_87])])).
fof(f337, plain, ((e1 = op(e1, e2)) | ~ (e2 = op(e1, e1))), inference(cnf_transformation, [], [f34])).
fof(f1140, plain, (~ spl24_94 | spl24_82), inference(avatar_split_clause, [], [f338, f699, f749])).
fof(f749, plain, (spl24_94 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_94])])).
fof(f699, plain, (spl24_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_82])])).
fof(f338, plain, ((e1 = op(e1, e3)) | ~ (e3 = op(e1, e1))), inference(cnf_transformation, [], [f34])).
fof(f1139, plain, (~ spl24_95 | spl24_77), inference(avatar_split_clause, [], [f339, f678, f753])).
fof(f753, plain, (spl24_95 <=> (e4 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_95])])).
fof(f678, plain, (spl24_77 <=> (e1 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_77])])).
fof(f339, plain, ((e1 = op(e1, e4)) | ~ (e4 = op(e1, e1))), inference(cnf_transformation, [], [f34])).
fof(f1138, plain, (~ spl24_61 | spl24_73), inference(avatar_split_clause, [], [f340, f661, f611])).
fof(f611, plain, (spl24_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_61])])).
fof(f661, plain, (spl24_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_73])])).
fof(f340, plain, ((e2 = op(e2, e0)) | ~ (e0 = op(e2, e2))), inference(cnf_transformation, [], [f34])).
fof(f1137, plain, (~ spl24_62 | spl24_68), inference(avatar_split_clause, [], [f341, f640, f615])).
fof(f615, plain, (spl24_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_62])])).
fof(f640, plain, (spl24_68 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_68])])).
fof(f341, plain, ((e2 = op(e2, e1)) | ~ (e1 = op(e2, e2))), inference(cnf_transformation, [], [f34])).
fof(f1136, plain, (~ spl24_64 | spl24_58), inference(avatar_split_clause, [], [f343, f598, f623])).
fof(f623, plain, (spl24_64 <=> (e3 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_64])])).
fof(f598, plain, (spl24_58 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_58])])).
fof(f343, plain, ((e2 = op(e2, e3)) | ~ (e3 = op(e2, e2))), inference(cnf_transformation, [], [f34])).
fof(f1135, plain, (~ spl24_65 | spl24_53), inference(avatar_split_clause, [], [f344, f577, f627])).
fof(f627, plain, (spl24_65 <=> (e4 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_65])])).
fof(f577, plain, (spl24_53 <=> (e2 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_53])])).
fof(f344, plain, ((e2 = op(e2, e4)) | ~ (e4 = op(e2, e2))), inference(cnf_transformation, [], [f34])).
fof(f1134, plain, (~ spl24_31 | spl24_49), inference(avatar_split_clause, [], [f345, f560, f485])).
fof(f485, plain, (spl24_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_31])])).
fof(f560, plain, (spl24_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_49])])).
fof(f345, plain, ((e3 = op(e3, e0)) | ~ (e0 = op(e3, e3))), inference(cnf_transformation, [], [f34])).
fof(f1133, plain, (~ spl24_32 | spl24_44), inference(avatar_split_clause, [], [f346, f539, f489])).
fof(f489, plain, (spl24_32 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_32])])).
fof(f539, plain, (spl24_44 <=> (e3 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_44])])).
fof(f346, plain, ((e3 = op(e3, e1)) | ~ (e1 = op(e3, e3))), inference(cnf_transformation, [], [f34])).
fof(f1132, plain, (~ spl24_33 | spl24_39), inference(avatar_split_clause, [], [f347, f518, f493])).
fof(f493, plain, (spl24_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_33])])).
fof(f518, plain, (spl24_39 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_39])])).
fof(f347, plain, ((e3 = op(e3, e2)) | ~ (e2 = op(e3, e3))), inference(cnf_transformation, [], [f34])).
fof(f1131, plain, (~ spl24_35 | spl24_29), inference(avatar_split_clause, [], [f349, f476, f501])).
fof(f501, plain, (spl24_35 <=> (e4 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_35])])).
fof(f476, plain, (spl24_29 <=> (e3 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_29])])).
fof(f349, plain, ((e3 = op(e3, e4)) | ~ (e4 = op(e3, e3))), inference(cnf_transformation, [], [f34])).
fof(f1130, plain, (~ spl24_1 | spl24_25), inference(avatar_split_clause, [], [f350, f459, f359])).
fof(f359, plain, (spl24_1 <=> (e0 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_1])])).
fof(f459, plain, (spl24_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_25])])).
fof(f350, plain, ((e4 = op(e4, e0)) | ~ (e0 = op(e4, e4))), inference(cnf_transformation, [], [f34])).
fof(f1129, plain, (~ spl24_2 | spl24_20), inference(avatar_split_clause, [], [f351, f438, f363])).
fof(f363, plain, (spl24_2 <=> (e1 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_2])])).
fof(f438, plain, (spl24_20 <=> (e4 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_20])])).
fof(f351, plain, ((e4 = op(e4, e1)) | ~ (e1 = op(e4, e4))), inference(cnf_transformation, [], [f34])).
fof(f1128, plain, (~ spl24_3 | spl24_15), inference(avatar_split_clause, [], [f352, f417, f367])).
fof(f367, plain, (spl24_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_3])])).
fof(f417, plain, (spl24_15 <=> (e4 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_15])])).
fof(f352, plain, ((e4 = op(e4, e2)) | ~ (e2 = op(e4, e4))), inference(cnf_transformation, [], [f34])).
fof(f1127, plain, (~ spl24_4 | spl24_10), inference(avatar_split_clause, [], [f353, f396, f371])).
fof(f371, plain, (spl24_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_4])])).
fof(f396, plain, (spl24_10 <=> (e4 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_10])])).
fof(f353, plain, ((e4 = op(e4, e3)) | ~ (e3 = op(e4, e4))), inference(cnf_transformation, [], [f34])).
fof(f1126, plain, (spl24_154 | spl24_153 | spl24_152 | spl24_151 | spl24_150 | spl24_149 | spl24_148 | spl24_147 | spl24_146 | spl24_145 | spl24_144 | spl24_143 | spl24_142 | spl24_141 | spl24_140 | spl24_139 | spl24_138 | spl24_137 | spl24_136 | spl24_135 | spl24_134 | spl24_133 | spl24_132 | spl24_131 | spl24_5), inference(avatar_split_clause, [], [f355, f375, f957, f964, f971, f978, f985, f992, f999, f1006, f1013, f1020, f1027, f1034, f1041, f1048, f1055, f1062, f1069, f1076, f1083, f1090, f1097, f1104, f1111, f1118])).
fof(f1118, plain, (spl24_154 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl24_154])])).
fof(f1111, plain, (spl24_153 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl24_153])])).
fof(f1104, plain, (spl24_152 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl24_152])])).
fof(f1097, plain, (spl24_151 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl24_151])])).
fof(f1090, plain, (spl24_150 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl24_150])])).
fof(f1083, plain, (spl24_149 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl24_149])])).
fof(f1076, plain, (spl24_148 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl24_148])])).
fof(f1069, plain, (spl24_147 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl24_147])])).
fof(f1062, plain, (spl24_146 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl24_146])])).
fof(f1055, plain, (spl24_145 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl24_145])])).
fof(f1048, plain, (spl24_144 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl24_144])])).
fof(f1041, plain, (spl24_143 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl24_143])])).
fof(f1034, plain, (spl24_142 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl24_142])])).
fof(f1027, plain, (spl24_141 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl24_141])])).
fof(f1020, plain, (spl24_140 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl24_140])])).
fof(f1013, plain, (spl24_139 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl24_139])])).
fof(f1006, plain, (spl24_138 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl24_138])])).
fof(f999, plain, (spl24_137 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl24_137])])).
fof(f992, plain, (spl24_136 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl24_136])])).
fof(f985, plain, (spl24_135 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl24_135])])).
fof(f978, plain, (spl24_134 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl24_134])])).
fof(f971, plain, (spl24_133 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl24_133])])).
fof(f964, plain, (spl24_132 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl24_132])])).
fof(f957, plain, (spl24_131 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl24_131])])).
fof(f375, plain, (spl24_5 <=> (e4 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl24_5])])).
fof(f355, plain, ((e4 = op(e4, e4)) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f34])).
fof(f1124, plain, (spl24_154 | spl24_153 | spl24_152 | spl24_151 | spl24_150 | spl24_149 | spl24_148 | spl24_147 | spl24_146 | spl24_145 | spl24_144 | spl24_143 | spl24_142 | spl24_141 | spl24_140 | spl24_139 | spl24_138 | spl24_137 | spl24_136 | spl24_135 | spl24_134 | spl24_133 | spl24_132 | spl24_131 | ~ spl24_5), inference(avatar_split_clause, [], [f357, f375, f957, f964, f971, f978, f985, f992, f999, f1006, f1013, f1020, f1027, f1034, f1041, f1048, f1055, f1062, f1069, f1076, f1083, f1090, f1097, f1104, f1111, f1118])).
fof(f357, plain, (~ (e4 = op(e4, e4)) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f34])).
fof(f1123, plain, (~ spl24_154 | spl24_121), inference(avatar_split_clause, [], [f327, f863, f1118])).
fof(f863, plain, (spl24_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl24_121])])).
fof(f327, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f1121, plain, (~ spl24_154 | ~ spl24_121), inference(avatar_split_clause, [], [f329, f863, f1118])).
fof(f329, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f58])).
fof(f1116, plain, (~ spl24_153 | spl24_122), inference(avatar_split_clause, [], [f324, f867, f1111])).
fof(f324, plain, ((op(e0, e0) = e1) | ~ sP1), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f1114, plain, (~ spl24_153 | ~ spl24_116), inference(avatar_split_clause, [], [f326, f842, f1111])).
fof(f326, plain, (~ (e0 = op(e0, e1)) | ~ sP1), inference(cnf_transformation, [], [f57])).
fof(f1109, plain, (~ spl24_152 | spl24_123), inference(avatar_split_clause, [], [f321, f871, f1104])).
fof(f321, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f12])).
fof(f1107, plain, (~ spl24_152 | ~ spl24_111), inference(avatar_split_clause, [], [f323, f821, f1104])).
fof(f323, plain, (~ (e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f56])).
fof(f1102, plain, (~ spl24_151 | spl24_124), inference(avatar_split_clause, [], [f318, f875, f1097])).
fof(f318, plain, ((op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(nnf_transformation, [], [f13])).
fof(f1100, plain, (~ spl24_151 | ~ spl24_106), inference(avatar_split_clause, [], [f320, f800, f1097])).
fof(f320, plain, (~ (e0 = op(e0, e3)) | ~ sP3), inference(cnf_transformation, [], [f55])).
fof(f1095, plain, (~ spl24_150 | spl24_125), inference(avatar_split_clause, [], [f315, f879, f1090])).
fof(f315, plain, ((op(e0, e0) = e4) | ~ sP4), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP4), inference(nnf_transformation, [], [f14])).
fof(f1093, plain, (~ spl24_150 | ~ spl24_101), inference(avatar_split_clause, [], [f317, f779, f1090])).
fof(f317, plain, (~ (e0 = op(e0, e4)) | ~ sP4), inference(cnf_transformation, [], [f54])).
fof(f1088, plain, (~ spl24_149 | spl24_91), inference(avatar_split_clause, [], [f312, f737, f1083])).
fof(f312, plain, ((e0 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f53])).
fof(f53, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f15])).
fof(f1086, plain, (~ spl24_149 | ~ spl24_97), inference(avatar_split_clause, [], [f314, f762, f1083])).
fof(f314, plain, (~ (e1 = op(e1, e0)) | ~ sP5), inference(cnf_transformation, [], [f53])).
fof(f1081, plain, (~ spl24_148 | spl24_92), inference(avatar_split_clause, [], [f309, f741, f1076])).
fof(f741, plain, (spl24_92 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl24_92])])).
fof(f309, plain, ((e1 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f1079, plain, (~ spl24_148 | ~ spl24_92), inference(avatar_split_clause, [], [f311, f741, f1076])).
fof(f311, plain, (~ (e1 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f52])).
fof(f1074, plain, (~ spl24_147 | spl24_93), inference(avatar_split_clause, [], [f306, f745, f1069])).
fof(f306, plain, ((e2 = op(e1, e1)) | ~ sP7), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP7), inference(nnf_transformation, [], [f17])).
fof(f1072, plain, (~ spl24_147 | ~ spl24_87), inference(avatar_split_clause, [], [f308, f720, f1069])).
fof(f308, plain, (~ (e1 = op(e1, e2)) | ~ sP7), inference(cnf_transformation, [], [f51])).
fof(f1067, plain, (~ spl24_146 | spl24_94), inference(avatar_split_clause, [], [f303, f749, f1062])).
fof(f303, plain, ((e3 = op(e1, e1)) | ~ sP8), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP8), inference(nnf_transformation, [], [f18])).
fof(f1065, plain, (~ spl24_146 | ~ spl24_82), inference(avatar_split_clause, [], [f305, f699, f1062])).
fof(f305, plain, (~ (e1 = op(e1, e3)) | ~ sP8), inference(cnf_transformation, [], [f50])).
fof(f1060, plain, (~ spl24_145 | spl24_95), inference(avatar_split_clause, [], [f300, f753, f1055])).
fof(f300, plain, ((e4 = op(e1, e1)) | ~ sP9), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP9), inference(nnf_transformation, [], [f19])).
fof(f1058, plain, (~ spl24_145 | ~ spl24_77), inference(avatar_split_clause, [], [f302, f678, f1055])).
fof(f302, plain, (~ (e1 = op(e1, e4)) | ~ sP9), inference(cnf_transformation, [], [f49])).
fof(f1053, plain, (~ spl24_144 | spl24_61), inference(avatar_split_clause, [], [f297, f611, f1048])).
fof(f297, plain, ((e0 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f20])).
fof(f1051, plain, (~ spl24_144 | ~ spl24_73), inference(avatar_split_clause, [], [f299, f661, f1048])).
fof(f299, plain, (~ (e2 = op(e2, e0)) | ~ sP10), inference(cnf_transformation, [], [f48])).
fof(f1046, plain, (~ spl24_143 | spl24_62), inference(avatar_split_clause, [], [f294, f615, f1041])).
fof(f294, plain, ((e1 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP11), inference(nnf_transformation, [], [f21])).
fof(f1044, plain, (~ spl24_143 | ~ spl24_68), inference(avatar_split_clause, [], [f296, f640, f1041])).
fof(f296, plain, (~ (e2 = op(e2, e1)) | ~ sP11), inference(cnf_transformation, [], [f47])).
fof(f1039, plain, (~ spl24_142 | spl24_63), inference(avatar_split_clause, [], [f291, f619, f1034])).
fof(f619, plain, (spl24_63 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl24_63])])).
fof(f291, plain, ((e2 = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP12), inference(nnf_transformation, [], [f22])).
fof(f1037, plain, (~ spl24_142 | ~ spl24_63), inference(avatar_split_clause, [], [f293, f619, f1034])).
fof(f293, plain, (~ (e2 = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f46])).
fof(f1032, plain, (~ spl24_141 | spl24_64), inference(avatar_split_clause, [], [f288, f623, f1027])).
fof(f288, plain, ((e3 = op(e2, e2)) | ~ sP13), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP13), inference(nnf_transformation, [], [f23])).
fof(f1030, plain, (~ spl24_141 | ~ spl24_58), inference(avatar_split_clause, [], [f290, f598, f1027])).
fof(f290, plain, (~ (e2 = op(e2, e3)) | ~ sP13), inference(cnf_transformation, [], [f45])).
fof(f1025, plain, (~ spl24_140 | spl24_65), inference(avatar_split_clause, [], [f285, f627, f1020])).
fof(f285, plain, ((e4 = op(e2, e2)) | ~ sP14), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP14), inference(nnf_transformation, [], [f24])).
fof(f1023, plain, (~ spl24_140 | ~ spl24_53), inference(avatar_split_clause, [], [f287, f577, f1020])).
fof(f287, plain, (~ (e2 = op(e2, e4)) | ~ sP14), inference(cnf_transformation, [], [f44])).
fof(f1018, plain, (~ spl24_139 | spl24_31), inference(avatar_split_clause, [], [f282, f485, f1013])).
fof(f282, plain, ((e0 = op(e3, e3)) | ~ sP15), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP15), inference(nnf_transformation, [], [f25])).
fof(f1016, plain, (~ spl24_139 | ~ spl24_49), inference(avatar_split_clause, [], [f284, f560, f1013])).
fof(f284, plain, (~ (e3 = op(e3, e0)) | ~ sP15), inference(cnf_transformation, [], [f43])).
fof(f1011, plain, (~ spl24_138 | spl24_32), inference(avatar_split_clause, [], [f279, f489, f1006])).
fof(f279, plain, ((e1 = op(e3, e3)) | ~ sP16), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP16), inference(nnf_transformation, [], [f26])).
fof(f1009, plain, (~ spl24_138 | ~ spl24_44), inference(avatar_split_clause, [], [f281, f539, f1006])).
fof(f281, plain, (~ (e3 = op(e3, e1)) | ~ sP16), inference(cnf_transformation, [], [f42])).
fof(f1004, plain, (~ spl24_137 | spl24_33), inference(avatar_split_clause, [], [f276, f493, f999])).
fof(f276, plain, ((e2 = op(e3, e3)) | ~ sP17), inference(cnf_transformation, [], [f41])).
fof(f41, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP17), inference(nnf_transformation, [], [f27])).
fof(f1002, plain, (~ spl24_137 | ~ spl24_39), inference(avatar_split_clause, [], [f278, f518, f999])).
fof(f278, plain, (~ (e3 = op(e3, e2)) | ~ sP17), inference(cnf_transformation, [], [f41])).
fof(f997, plain, (~ spl24_136 | spl24_34), inference(avatar_split_clause, [], [f273, f497, f992])).
fof(f497, plain, (spl24_34 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl24_34])])).
fof(f273, plain, ((e3 = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f40])).
fof(f40, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP18), inference(nnf_transformation, [], [f28])).
fof(f995, plain, (~ spl24_136 | ~ spl24_34), inference(avatar_split_clause, [], [f275, f497, f992])).
fof(f275, plain, (~ (e3 = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f40])).
fof(f990, plain, (~ spl24_135 | spl24_35), inference(avatar_split_clause, [], [f270, f501, f985])).
fof(f270, plain, ((e4 = op(e3, e3)) | ~ sP19), inference(cnf_transformation, [], [f39])).
fof(f39, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP19), inference(nnf_transformation, [], [f29])).
fof(f988, plain, (~ spl24_135 | ~ spl24_29), inference(avatar_split_clause, [], [f272, f476, f985])).
fof(f272, plain, (~ (e3 = op(e3, e4)) | ~ sP19), inference(cnf_transformation, [], [f39])).
fof(f983, plain, (~ spl24_134 | spl24_1), inference(avatar_split_clause, [], [f267, f359, f978])).
fof(f267, plain, ((e0 = op(e4, e4)) | ~ sP20), inference(cnf_transformation, [], [f38])).
fof(f38, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP20), inference(nnf_transformation, [], [f30])).
fof(f981, plain, (~ spl24_134 | ~ spl24_25), inference(avatar_split_clause, [], [f269, f459, f978])).
fof(f269, plain, (~ (e4 = op(e4, e0)) | ~ sP20), inference(cnf_transformation, [], [f38])).
fof(f976, plain, (~ spl24_133 | spl24_2), inference(avatar_split_clause, [], [f264, f363, f971])).
fof(f264, plain, ((e1 = op(e4, e4)) | ~ sP21), inference(cnf_transformation, [], [f37])).
fof(f37, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP21), inference(nnf_transformation, [], [f31])).
fof(f974, plain, (~ spl24_133 | ~ spl24_20), inference(avatar_split_clause, [], [f266, f438, f971])).
fof(f266, plain, (~ (e4 = op(e4, e1)) | ~ sP21), inference(cnf_transformation, [], [f37])).
fof(f969, plain, (~ spl24_132 | spl24_3), inference(avatar_split_clause, [], [f261, f367, f964])).
fof(f261, plain, ((e2 = op(e4, e4)) | ~ sP22), inference(cnf_transformation, [], [f36])).
fof(f36, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP22), inference(nnf_transformation, [], [f32])).
fof(f967, plain, (~ spl24_132 | ~ spl24_15), inference(avatar_split_clause, [], [f263, f417, f964])).
fof(f263, plain, (~ (e4 = op(e4, e2)) | ~ sP22), inference(cnf_transformation, [], [f36])).
fof(f962, plain, (~ spl24_131 | spl24_4), inference(avatar_split_clause, [], [f258, f371, f957])).
fof(f258, plain, ((e3 = op(e4, e4)) | ~ sP23), inference(cnf_transformation, [], [f35])).
fof(f35, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP23), inference(nnf_transformation, [], [f33])).
fof(f960, plain, (~ spl24_131 | ~ spl24_10), inference(avatar_split_clause, [], [f260, f396, f957])).
fof(f260, plain, (~ (e4 = op(e4, e3)) | ~ sP23), inference(cnf_transformation, [], [f35])).