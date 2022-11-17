fof(f1632, plain, $false, inference(avatar_sat_refutation, [], [f380, f384, f392, f397, f405, f409, f414, f419, f424, f433, f446, f447, f460, f461, f469, f478, f486, f490, f495, f496, f509, f510, f519, f523, f533, f538, f542, f550, f559, f567, f571, f576, f581, f585, f594, f599, f604, f608, f617, f618, f622, f630, f634, f646, f647, f656, f657, f665, f673, f677, f685, f698, f700, f701, f706, f707, f709, f718, f722, f727, f728, f737, f742, f751, f757, f765, f770, f775, f779, f795, f796, f803, f808, f821, f822, f823, f827, f840, f846, f850, f863, f866, f867, f880, f883, f884, f897, f898, f899, f903, f904, f913, f918, f922, f926, f930, f934, f942, f946, f958, f967, f968, f969, f970, f976, f984, f988, f992, f997, f1002, f1012, f1013, f1014, f1019, f1024, f1037, f1041, f1045, f1049, f1053, f1058, f1063, f1072, f1077, f1081, f1118, f1119, f1124, f1125, f1126, f1127, f1128, f1129, f1131, f1133, f1135, f1137, f1139, f1141, f1143, f1145, f1147, f1149, f1151, f1153, f1155, f1170, f1172, f1174, f1181, f1187, f1200, f1206, f1208, f1212, f1220, f1224, f1228, f1230, f1234, f1236, f1238, f1302, f1308, f1310, f1335, f1337, f1339, f1341, f1343, f1347, f1356, f1358, f1362, f1364, f1372, f1374, f1376, f1378, f1380, f1384, f1406, f1411, f1434, f1438, f1450, f1458, f1470, f1482, f1488, f1492, f1497, f1499, f1503, f1535, f1547, f1563, f1569, f1578, f1580, f1582, f1584, f1609, f1631])).
fof(f1631, plain, (~ spl103_94 | ~ spl103_95 | ~ spl103_97), inference(avatar_contradiction_clause, [], [f1630])).
fof(f1630, plain, ($false | (~ spl103_94 | ~ spl103_95 | ~ spl103_97)), inference(resolution, [], [f1615, f807])).
fof(f807, plain, (q1(f(sK75)) | ~ spl103_97), inference(avatar_component_clause, [], [f805])).
fof(f805, plain, (spl103_97 <=> q1(f(sK75))), introduced(avatar_definition, [new_symbols(naming, [spl103_97])])).
fof(f1615, plain, (! [X0] : ~ q1(f(X0)) | (~ spl103_94 | ~ spl103_95)), inference(resolution, [], [f794, f799])).
fof(f799, plain, (! [X3] : p1(f(X3)) | ~ spl103_95), inference(avatar_component_clause, [], [f798])).
fof(f798, plain, (spl103_95 <=> ! [X3] : p1(f(X3))), introduced(avatar_definition, [new_symbols(naming, [spl103_95])])).
fof(f794, plain, (! [X2] : (~ p1(X2) | ~ q1(X2)) | ~ spl103_94), inference(avatar_component_clause, [], [f793])).
fof(f793, plain, (spl103_94 <=> ! [X2] : (~ q1(X2) | ~ p1(X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_94])])).
fof(f1609, plain, (~ spl103_56 | spl103_77 | ~ spl103_78 | ~ spl103_79), inference(avatar_contradiction_clause, [], [f1608])).
fof(f1608, plain, ($false | (~ spl103_56 | spl103_77 | ~ spl103_78 | ~ spl103_79)), inference(subsumption_resolution, [], [f1607, f717])).
fof(f717, plain, (~ q1(sK69) | spl103_77), inference(avatar_component_clause, [], [f715])).
fof(f715, plain, (spl103_77 <=> q1(sK69)), introduced(avatar_definition, [new_symbols(naming, [spl103_77])])).
fof(f1607, plain, (q1(sK69) | (~ spl103_56 | ~ spl103_78 | ~ spl103_79)), inference(resolution, [], [f1594, f621])).
fof(f621, plain, (! [X2] : (~ p1(X2) | q1(X2)) | ~ spl103_56), inference(avatar_component_clause, [], [f620])).
fof(f620, plain, (spl103_56 <=> ! [X2] : (q1(X2) | ~ p1(X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_56])])).
fof(f1594, plain, (p1(sK69) | (~ spl103_78 | ~ spl103_79)), inference(resolution, [], [f721, f726])).
fof(f726, plain, (r1(sK69) | ~ spl103_79), inference(avatar_component_clause, [], [f724])).
fof(f724, plain, (spl103_79 <=> r1(sK69)), introduced(avatar_definition, [new_symbols(naming, [spl103_79])])).
fof(f721, plain, (! [X1] : (~ r1(X1) | p1(X1)) | ~ spl103_78), inference(avatar_component_clause, [], [f720])).
fof(f720, plain, (spl103_78 <=> ! [X1] : (p1(X1) | ~ r1(X1))), introduced(avatar_definition, [new_symbols(naming, [spl103_78])])).
fof(f1584, plain, (~ spl103_5 | ~ spl103_132), inference(avatar_contradiction_clause, [], [f1583])).
fof(f1583, plain, ($false | (~ spl103_5 | ~ spl103_132)), inference(subsumption_resolution, [], [f975, f391])).
fof(f391, plain, (! [X0] : ~ p1(X0) | ~ spl103_5), inference(avatar_component_clause, [], [f390])).
fof(f390, plain, (spl103_5 <=> ! [X0] : ~ p1(X0)), introduced(avatar_definition, [new_symbols(naming, [spl103_5])])).
fof(f975, plain, (p1(sK91) | ~ spl103_132), inference(avatar_component_clause, [], [f973])).
fof(f973, plain, (spl103_132 <=> p1(sK91)), introduced(avatar_definition, [new_symbols(naming, [spl103_132])])).
fof(f1582, plain, (~ spl103_5 | ~ spl103_13), inference(avatar_contradiction_clause, [], [f1581])).
fof(f1581, plain, ($false | (~ spl103_5 | ~ spl103_13)), inference(subsumption_resolution, [], [f428, f391])).
fof(f428, plain, (p1(sK46) | ~ spl103_13), inference(avatar_component_clause, [], [f426])).
fof(f426, plain, (spl103_13 <=> p1(sK46)), introduced(avatar_definition, [new_symbols(naming, [spl103_13])])).
fof(f1580, plain, (~ spl103_5 | ~ spl103_14), inference(avatar_contradiction_clause, [], [f1579])).
fof(f1579, plain, ($false | (~ spl103_5 | ~ spl103_14)), inference(subsumption_resolution, [], [f432, f391])).
fof(f432, plain, (p1(sK45) | ~ spl103_14), inference(avatar_component_clause, [], [f430])).
fof(f430, plain, (spl103_14 <=> p1(sK45)), introduced(avatar_definition, [new_symbols(naming, [spl103_14])])).
fof(f1578, plain, (~ spl103_5 | ~ spl103_11), inference(avatar_contradiction_clause, [], [f1577])).
fof(f1577, plain, ($false | (~ spl103_5 | ~ spl103_11)), inference(subsumption_resolution, [], [f418, f391])).
fof(f418, plain, (p1(sK44) | ~ spl103_11), inference(avatar_component_clause, [], [f416])).
fof(f416, plain, (spl103_11 <=> p1(sK44)), introduced(avatar_definition, [new_symbols(naming, [spl103_11])])).
fof(f1569, plain, (~ spl103_5 | ~ spl103_37), inference(avatar_contradiction_clause, [], [f1556])).
fof(f1556, plain, ($false | (~ spl103_5 | ~ spl103_37)), inference(resolution, [], [f391, f537])).
fof(f537, plain, (p1(sK59) | ~ spl103_37), inference(avatar_component_clause, [], [f535])).
fof(f535, plain, (spl103_37 <=> p1(sK59)), introduced(avatar_definition, [new_symbols(naming, [spl103_37])])).
fof(f1563, plain, (~ spl103_5 | ~ spl103_164), inference(avatar_contradiction_clause, [], [f1562])).
fof(f1562, plain, ($false | (~ spl103_5 | ~ spl103_164)), inference(resolution, [], [f391, f1123])).
fof(f1123, plain, (p1(sK102) | ~ spl103_164), inference(avatar_component_clause, [], [f1121])).
fof(f1121, plain, (spl103_164 <=> p1(sK102)), introduced(avatar_definition, [new_symbols(naming, [spl103_164])])).
fof(f1547, plain, (~ spl103_94 | ~ spl103_95 | ~ spl103_101), inference(avatar_contradiction_clause, [], [f1546])).
fof(f1546, plain, ($false | (~ spl103_94 | ~ spl103_95 | ~ spl103_101)), inference(subsumption_resolution, [], [f1545, f826])).
fof(f826, plain, (! [X4] : q1(f(X4)) | ~ spl103_101), inference(avatar_component_clause, [], [f825])).
fof(f825, plain, (spl103_101 <=> ! [X4] : q1(f(X4))), introduced(avatar_definition, [new_symbols(naming, [spl103_101])])).
fof(f1545, plain, (! [X1] : ~ q1(f(X1)) | (~ spl103_94 | ~ spl103_95)), inference(resolution, [], [f799, f794])).
fof(f1535, plain, (spl103_38 | ~ spl103_94 | ~ spl103_101 | ~ spl103_116), inference(avatar_split_clause, [], [f1534, f901, f825, f793, f540])).
fof(f540, plain, (spl103_38 <=> ! [X3] : r1(X3)), introduced(avatar_definition, [new_symbols(naming, [spl103_38])])).
fof(f901, plain, (spl103_116 <=> ! [X3] : (r1(X3) | p1(f(X3)))), introduced(avatar_definition, [new_symbols(naming, [spl103_116])])).
fof(f1534, plain, (! [X1] : r1(X1) | (~ spl103_94 | ~ spl103_101 | ~ spl103_116)), inference(subsumption_resolution, [], [f1533, f826])).
fof(f1533, plain, (! [X1] : (r1(X1) | ~ q1(f(X1))) | (~ spl103_94 | ~ spl103_116)), inference(resolution, [], [f902, f794])).
fof(f902, plain, (! [X3] : (p1(f(X3)) | r1(X3)) | ~ spl103_116), inference(avatar_component_clause, [], [f901])).
fof(f1503, plain, (spl103_97 | ~ spl103_101), inference(avatar_contradiction_clause, [], [f1502])).
fof(f1502, plain, ($false | (spl103_97 | ~ spl103_101)), inference(resolution, [], [f826, f806])).
fof(f806, plain, (~ q1(f(sK75)) | spl103_97), inference(avatar_component_clause, [], [f805])).
fof(f1499, plain, (spl103_9 | ~ spl103_105 | ~ spl103_106), inference(avatar_split_clause, [], [f1498, f848, f844, f407])).
fof(f407, plain, (spl103_9 <=> ! [X0] : p1(X0)), introduced(avatar_definition, [new_symbols(naming, [spl103_9])])).
fof(f844, plain, (spl103_105 <=> ! [X2] : (q1(X2) | p1(X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_105])])).
fof(f848, plain, (spl103_106 <=> ! [X3] : (p1(X3) | ~ q1(X3))), introduced(avatar_definition, [new_symbols(naming, [spl103_106])])).
fof(f1498, plain, (! [X2] : p1(X2) | (~ spl103_105 | ~ spl103_106)), inference(subsumption_resolution, [], [f845, f849])).
fof(f849, plain, (! [X3] : (p1(X3) | ~ q1(X3)) | ~ spl103_106), inference(avatar_component_clause, [], [f848])).
fof(f845, plain, (! [X2] : (p1(X2) | q1(X2)) | ~ spl103_105), inference(avatar_component_clause, [], [f844])).
fof(f1497, plain, (spl103_165 | ~ spl103_145 | ~ spl103_148 | ~ spl103_150 | ~ spl103_166), inference(avatar_split_clause, [], [f1494, f1479, f1055, f1047, f1035, f1475])).
fof(f1475, plain, (spl103_165 <=> g(sK98)), introduced(avatar_definition, [new_symbols(naming, [spl103_165])])).
fof(f1035, plain, (spl103_145 <=> ! [X4] : (~ c(X4) | ~ p1(X4))), introduced(avatar_definition, [new_symbols(naming, [spl103_145])])).
fof(f1047, plain, (spl103_148 <=> ! [X2] : (c(f(X2)) | ~ e(X2) | g(X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_148])])).
fof(f1055, plain, (spl103_150 <=> e(sK98)), introduced(avatar_definition, [new_symbols(naming, [spl103_150])])).
fof(f1479, plain, (spl103_166 <=> p1(f(sK98))), introduced(avatar_definition, [new_symbols(naming, [spl103_166])])).
fof(f1494, plain, (g(sK98) | (~ spl103_145 | ~ spl103_148 | ~ spl103_150 | ~ spl103_166)), inference(subsumption_resolution, [], [f1493, f1057])).
fof(f1057, plain, (e(sK98) | ~ spl103_150), inference(avatar_component_clause, [], [f1055])).
fof(f1493, plain, (~ e(sK98) | g(sK98) | (~ spl103_145 | ~ spl103_148 | ~ spl103_166)), inference(resolution, [], [f1489, f1481])).
fof(f1481, plain, (p1(f(sK98)) | ~ spl103_166), inference(avatar_component_clause, [], [f1479])).
fof(f1489, plain, (! [X0] : (~ p1(f(X0)) | ~ e(X0) | g(X0)) | (~ spl103_145 | ~ spl103_148)), inference(resolution, [], [f1036, f1048])).
fof(f1048, plain, (! [X2] : (c(f(X2)) | ~ e(X2) | g(X2)) | ~ spl103_148), inference(avatar_component_clause, [], [f1047])).
fof(f1036, plain, (! [X4] : (~ c(X4) | ~ p1(X4)) | ~ spl103_145), inference(avatar_component_clause, [], [f1035])).
fof(f1492, plain, (~ spl103_146 | ~ spl103_151 | ~ spl103_165), inference(avatar_contradiction_clause, [], [f1491])).
fof(f1491, plain, ($false | (~ spl103_146 | ~ spl103_151 | ~ spl103_165)), inference(subsumption_resolution, [], [f1490, f1062])).
fof(f1062, plain, (p1(sK98) | ~ spl103_151), inference(avatar_component_clause, [], [f1060])).
fof(f1060, plain, (spl103_151 <=> p1(sK98)), introduced(avatar_definition, [new_symbols(naming, [spl103_151])])).
fof(f1490, plain, (~ p1(sK98) | (~ spl103_146 | ~ spl103_165)), inference(resolution, [], [f1040, f1477])).
fof(f1477, plain, (g(sK98) | ~ spl103_165), inference(avatar_component_clause, [], [f1475])).
fof(f1040, plain, (! [X3] : (~ g(X3) | ~ p1(X3)) | ~ spl103_146), inference(avatar_component_clause, [], [f1039])).
fof(f1039, plain, (spl103_146 <=> ! [X3] : (~ g(X3) | ~ p1(X3))), introduced(avatar_definition, [new_symbols(naming, [spl103_146])])).
fof(f1488, plain, (~ spl103_134 | ~ spl103_135 | ~ spl103_136 | ~ spl103_137 | ~ spl103_138), inference(avatar_contradiction_clause, [], [f1487])).
fof(f1487, plain, ($false | (~ spl103_134 | ~ spl103_135 | ~ spl103_136 | ~ spl103_137 | ~ spl103_138)), inference(subsumption_resolution, [], [f1486, f1461])).
fof(f1461, plain, (p1(sK93) | (~ spl103_136 | ~ spl103_138)), inference(resolution, [], [f991, f1001])).
fof(f1001, plain, (s1(sK93) | ~ spl103_138), inference(avatar_component_clause, [], [f999])).
fof(f999, plain, (spl103_138 <=> s1(sK93)), introduced(avatar_definition, [new_symbols(naming, [spl103_138])])).
fof(f991, plain, (! [X7] : (~ s1(X7) | p1(X7)) | ~ spl103_136), inference(avatar_component_clause, [], [f990])).
fof(f990, plain, (spl103_136 <=> ! [X7] : (p1(X7) | ~ s1(X7))), introduced(avatar_definition, [new_symbols(naming, [spl103_136])])).
fof(f1486, plain, (~ p1(sK93) | (~ spl103_134 | ~ spl103_135 | ~ spl103_137)), inference(resolution, [], [f1471, f983])).
fof(f983, plain, (! [X4, X3] : (~ q(X3, X4) | ~ p1(X3)) | ~ spl103_134), inference(avatar_component_clause, [], [f982])).
fof(f982, plain, (spl103_134 <=> ! [X3, X4] : (~ q(X3, X4) | ~ p1(X3))), introduced(avatar_definition, [new_symbols(naming, [spl103_134])])).
fof(f1471, plain, (q(sK93, sK94) | (~ spl103_135 | ~ spl103_137)), inference(resolution, [], [f996, f987])).
fof(f987, plain, (! [X6, X5] : (~ r(X5, X6) | q(X5, X6)) | ~ spl103_135), inference(avatar_component_clause, [], [f986])).
fof(f986, plain, (spl103_135 <=> ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6))), introduced(avatar_definition, [new_symbols(naming, [spl103_135])])).
fof(f996, plain, (r(sK93, sK94) | ~ spl103_137), inference(avatar_component_clause, [], [f994])).
fof(f994, plain, (spl103_137 <=> r(sK93, sK94)), introduced(avatar_definition, [new_symbols(naming, [spl103_137])])).
fof(f1482, plain, (spl103_165 | spl103_166 | ~ spl103_147 | ~ spl103_149 | ~ spl103_150), inference(avatar_split_clause, [], [f1473, f1055, f1051, f1043, f1479, f1475])).
fof(f1043, plain, (spl103_147 <=> ! [X5] : (p1(X5) | ~ s(sK98, X5))), introduced(avatar_definition, [new_symbols(naming, [spl103_147])])).
fof(f1051, plain, (spl103_149 <=> ! [X1] : (s(X1, f(X1)) | ~ e(X1) | g(X1))), introduced(avatar_definition, [new_symbols(naming, [spl103_149])])).
fof(f1473, plain, (p1(f(sK98)) | g(sK98) | (~ spl103_147 | ~ spl103_149 | ~ spl103_150)), inference(subsumption_resolution, [], [f1472, f1057])).
fof(f1472, plain, (p1(f(sK98)) | ~ e(sK98) | g(sK98) | (~ spl103_147 | ~ spl103_149)), inference(resolution, [], [f1044, f1052])).
fof(f1052, plain, (! [X1] : (s(X1, f(X1)) | ~ e(X1) | g(X1)) | ~ spl103_149), inference(avatar_component_clause, [], [f1051])).
fof(f1044, plain, (! [X5] : (~ s(sK98, X5) | p1(X5)) | ~ spl103_147), inference(avatar_component_clause, [], [f1043])).
fof(f1470, plain, (~ spl103_134 | ~ spl103_135 | ~ spl103_136 | ~ spl103_141 | ~ spl103_142), inference(avatar_contradiction_clause, [], [f1469])).
fof(f1469, plain, ($false | (~ spl103_134 | ~ spl103_135 | ~ spl103_136 | ~ spl103_141 | ~ spl103_142)), inference(subsumption_resolution, [], [f1466, f1463])).
fof(f1463, plain, (p1(sK96) | (~ spl103_136 | ~ spl103_142)), inference(resolution, [], [f991, f1023])).
fof(f1023, plain, (s1(sK96) | ~ spl103_142), inference(avatar_component_clause, [], [f1021])).
fof(f1021, plain, (spl103_142 <=> s1(sK96)), introduced(avatar_definition, [new_symbols(naming, [spl103_142])])).
fof(f1466, plain, (~ p1(sK96) | (~ spl103_134 | ~ spl103_135 | ~ spl103_141)), inference(resolution, [], [f983, f1459])).
fof(f1459, plain, (q(sK96, sK97) | (~ spl103_135 | ~ spl103_141)), inference(resolution, [], [f1018, f987])).
fof(f1018, plain, (r(sK96, sK97) | ~ spl103_141), inference(avatar_component_clause, [], [f1016])).
fof(f1016, plain, (spl103_141 <=> r(sK96, sK97)), introduced(avatar_definition, [new_symbols(naming, [spl103_141])])).
fof(f1458, plain, (~ spl103_70 | ~ spl103_66 | ~ spl103_68 | ~ spl103_69), inference(avatar_split_clause, [], [f1453, f675, f670, f663, f679])).
fof(f679, plain, (spl103_70 <=> r1(sK68)), introduced(avatar_definition, [new_symbols(naming, [spl103_70])])).
fof(f663, plain, (spl103_66 <=> ! [X1, X2] : (~ q(X1, X2) | ~ p(X1, X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_66])])).
fof(f670, plain, (spl103_68 <=> q(f(sK68), sK68)), introduced(avatar_definition, [new_symbols(naming, [spl103_68])])).
fof(f675, plain, (spl103_69 <=> ! [X3] : (p(f(X3), X3) | ~ r1(X3))), introduced(avatar_definition, [new_symbols(naming, [spl103_69])])).
fof(f1453, plain, (~ r1(sK68) | (~ spl103_66 | ~ spl103_68 | ~ spl103_69)), inference(resolution, [], [f1452, f672])).
fof(f672, plain, (q(f(sK68), sK68) | ~ spl103_68), inference(avatar_component_clause, [], [f670])).
fof(f1452, plain, (! [X0] : (~ q(f(X0), X0) | ~ r1(X0)) | (~ spl103_66 | ~ spl103_69)), inference(resolution, [], [f676, f664])).
fof(f664, plain, (! [X2, X1] : (~ p(X1, X2) | ~ q(X1, X2)) | ~ spl103_66), inference(avatar_component_clause, [], [f663])).
fof(f676, plain, (! [X3] : (p(f(X3), X3) | ~ r1(X3)) | ~ spl103_69), inference(avatar_component_clause, [], [f675])).
fof(f1450, plain, (~ spl103_66 | ~ spl103_68 | ~ spl103_71), inference(avatar_contradiction_clause, [], [f1449])).
fof(f1449, plain, ($false | (~ spl103_66 | ~ spl103_68 | ~ spl103_71)), inference(resolution, [], [f1446, f672])).
fof(f1446, plain, (! [X0] : ~ q(f(X0), X0) | (~ spl103_66 | ~ spl103_71)), inference(resolution, [], [f664, f684])).
fof(f684, plain, (! [X3] : p(f(X3), X3) | ~ spl103_71), inference(avatar_component_clause, [], [f683])).
fof(f683, plain, (spl103_71 <=> ! [X3] : p(f(X3), X3)), introduced(avatar_definition, [new_symbols(naming, [spl103_71])])).
fof(f1438, plain, (~ spl103_5 | ~ spl103_9), inference(avatar_contradiction_clause, [], [f1437])).
fof(f1437, plain, ($false | (~ spl103_5 | ~ spl103_9)), inference(subsumption_resolution, [], [f391, f408])).
fof(f408, plain, (! [X0] : p1(X0) | ~ spl103_9), inference(avatar_component_clause, [], [f407])).
fof(f1434, plain, (~ spl103_53 | spl103_88 | ~ spl103_89), inference(avatar_contradiction_clause, [], [f1433])).
fof(f1433, plain, ($false | (~ spl103_53 | spl103_88 | ~ spl103_89)), inference(subsumption_resolution, [], [f1432, f769])).
fof(f769, plain, (~ b(sK74) | spl103_88), inference(avatar_component_clause, [], [f767])).
fof(f767, plain, (spl103_88 <=> b(sK74)), introduced(avatar_definition, [new_symbols(naming, [spl103_88])])).
fof(f1432, plain, (b(sK74) | (~ spl103_53 | ~ spl103_89)), inference(resolution, [], [f607, f774])).
fof(f774, plain, (a1(sK74) | ~ spl103_89), inference(avatar_component_clause, [], [f772])).
fof(f772, plain, (spl103_89 <=> a1(sK74)), introduced(avatar_definition, [new_symbols(naming, [spl103_89])])).
fof(f607, plain, (! [X2] : (~ a1(X2) | b(X2)) | ~ spl103_53), inference(avatar_component_clause, [], [f606])).
fof(f606, plain, (spl103_53 <=> ! [X2] : (b(X2) | ~ a1(X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_53])])).
fof(f1411, plain, (spl103_53 | ~ spl103_87 | ~ spl103_90), inference(avatar_split_clause, [], [f1410, f777, f763, f606])).
fof(f763, plain, (spl103_87 <=> ! [X0] : (~ c(X0) | ~ a1(X0))), introduced(avatar_definition, [new_symbols(naming, [spl103_87])])).
fof(f777, plain, (spl103_90 <=> ! [X2] : (c(X2) | ~ a1(X2) | b(X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_90])])).
fof(f1410, plain, (! [X2] : (~ a1(X2) | b(X2)) | (~ spl103_87 | ~ spl103_90)), inference(subsumption_resolution, [], [f778, f764])).
fof(f764, plain, (! [X0] : (~ c(X0) | ~ a1(X0)) | ~ spl103_87), inference(avatar_component_clause, [], [f763])).
fof(f778, plain, (! [X2] : (c(X2) | ~ a1(X2) | b(X2)) | ~ spl103_90), inference(avatar_component_clause, [], [f777])).
fof(f1406, plain, (~ spl103_9 | ~ spl103_84), inference(avatar_contradiction_clause, [], [f1405])).
fof(f1405, plain, ($false | (~ spl103_9 | ~ spl103_84)), inference(subsumption_resolution, [], [f750, f408])).
fof(f750, plain, (! [X0] : ~ p1(sK72(X0)) | ~ spl103_84), inference(avatar_component_clause, [], [f749])).
fof(f749, plain, (spl103_84 <=> ! [X0] : ~ p1(sK72(X0))), introduced(avatar_definition, [new_symbols(naming, [spl103_84])])).
fof(f1384, plain, (spl103_41 | ~ spl103_44 | ~ spl103_45), inference(avatar_split_clause, [], [f1383, f569, f565, f552])).
fof(f552, plain, (spl103_41 <=> ! [X0] : ~ a1(X0)), introduced(avatar_definition, [new_symbols(naming, [spl103_41])])).
fof(f565, plain, (spl103_44 <=> ! [X0] : (~ b(X0) | ~ a1(X0))), introduced(avatar_definition, [new_symbols(naming, [spl103_44])])).
fof(f569, plain, (spl103_45 <=> ! [X1] : b(X1)), introduced(avatar_definition, [new_symbols(naming, [spl103_45])])).
fof(f1383, plain, (! [X0] : ~ a1(X0) | (~ spl103_44 | ~ spl103_45)), inference(subsumption_resolution, [], [f566, f570])).
fof(f570, plain, (! [X1] : b(X1) | ~ spl103_45), inference(avatar_component_clause, [], [f569])).
fof(f566, plain, (! [X0] : (~ b(X0) | ~ a1(X0)) | ~ spl103_44), inference(avatar_component_clause, [], [f565])).
fof(f1380, plain, (~ spl103_38 | spl103_114), inference(avatar_contradiction_clause, [], [f1379])).
fof(f1379, plain, ($false | (~ spl103_38 | spl103_114)), inference(subsumption_resolution, [], [f892, f541])).
fof(f541, plain, (! [X3] : r1(X3) | ~ spl103_38), inference(avatar_component_clause, [], [f540])).
fof(f892, plain, (~ r1(sK86) | spl103_114), inference(avatar_component_clause, [], [f890])).
fof(f890, plain, (spl103_114 <=> r1(sK86)), introduced(avatar_definition, [new_symbols(naming, [spl103_114])])).
fof(f1378, plain, (~ spl103_38 | spl103_115), inference(avatar_contradiction_clause, [], [f1377])).
fof(f1377, plain, ($false | (~ spl103_38 | spl103_115)), inference(subsumption_resolution, [], [f896, f541])).
fof(f896, plain, (~ r1(sK85) | spl103_115), inference(avatar_component_clause, [], [f894])).
fof(f894, plain, (spl103_115 <=> r1(sK85)), introduced(avatar_definition, [new_symbols(naming, [spl103_115])])).
fof(f1376, plain, (~ spl103_9 | spl103_104), inference(avatar_contradiction_clause, [], [f1375])).
fof(f1375, plain, ($false | (~ spl103_9 | spl103_104)), inference(subsumption_resolution, [], [f839, f408])).
fof(f839, plain, (~ p1(sK80) | spl103_104), inference(avatar_component_clause, [], [f837])).
fof(f837, plain, (spl103_104 <=> p1(sK80)), introduced(avatar_definition, [new_symbols(naming, [spl103_104])])).
fof(f1374, plain, (~ spl103_38 | spl103_99), inference(avatar_contradiction_clause, [], [f1373])).
fof(f1373, plain, ($false | (~ spl103_38 | spl103_99)), inference(subsumption_resolution, [], [f816, f541])).
fof(f816, plain, (~ r1(sK77) | spl103_99), inference(avatar_component_clause, [], [f814])).
fof(f814, plain, (spl103_99 <=> r1(sK77)), introduced(avatar_definition, [new_symbols(naming, [spl103_99])])).
fof(f1372, plain, (~ spl103_38 | spl103_100), inference(avatar_contradiction_clause, [], [f1371])).
fof(f1371, plain, ($false | (~ spl103_38 | spl103_100)), inference(resolution, [], [f820, f541])).
fof(f820, plain, (~ r1(sK78) | spl103_100), inference(avatar_component_clause, [], [f818])).
fof(f818, plain, (spl103_100 <=> r1(sK78)), introduced(avatar_definition, [new_symbols(naming, [spl103_100])])).
fof(f1364, plain, (~ spl103_62 | ~ spl103_64), inference(avatar_contradiction_clause, [], [f1363])).
fof(f1363, plain, ($false | (~ spl103_62 | ~ spl103_64)), inference(subsumption_resolution, [], [f655, f645])).
fof(f645, plain, (! [X0] : ~ r1(X0) | ~ spl103_62), inference(avatar_component_clause, [], [f644])).
fof(f644, plain, (spl103_62 <=> ! [X0] : ~ r1(X0)), introduced(avatar_definition, [new_symbols(naming, [spl103_62])])).
fof(f655, plain, (r1(sK67) | ~ spl103_64), inference(avatar_component_clause, [], [f653])).
fof(f653, plain, (spl103_64 <=> r1(sK67)), introduced(avatar_definition, [new_symbols(naming, [spl103_64])])).
fof(f1362, plain, (~ spl103_34 | spl103_63), inference(avatar_contradiction_clause, [], [f1361])).
fof(f1361, plain, ($false | (~ spl103_34 | spl103_63)), inference(resolution, [], [f651, f522])).
fof(f522, plain, (! [X1] : q1(X1) | ~ spl103_34), inference(avatar_component_clause, [], [f521])).
fof(f521, plain, (spl103_34 <=> ! [X1] : q1(X1)), introduced(avatar_definition, [new_symbols(naming, [spl103_34])])).
fof(f651, plain, (~ q1(sK67) | spl103_63), inference(avatar_component_clause, [], [f649])).
fof(f649, plain, (spl103_63 <=> q1(sK67)), introduced(avatar_definition, [new_symbols(naming, [spl103_63])])).
fof(f1358, plain, (spl103_34 | ~ spl103_9 | ~ spl103_56), inference(avatar_split_clause, [], [f1357, f620, f407, f521])).
fof(f1357, plain, (! [X2] : q1(X2) | (~ spl103_9 | ~ spl103_56)), inference(subsumption_resolution, [], [f621, f408])).
fof(f1356, plain, (spl103_153 | ~ spl103_155), inference(avatar_contradiction_clause, [], [f1355])).
fof(f1355, plain, ($false | (spl103_153 | ~ spl103_155)), inference(subsumption_resolution, [], [f1071, f1080])).
fof(f1080, plain, (! [X2, X3] : p(X2, X3) | ~ spl103_155), inference(avatar_component_clause, [], [f1079])).
fof(f1079, plain, (spl103_155 <=> ! [X3, X2] : p(X2, X3)), introduced(avatar_definition, [new_symbols(naming, [spl103_155])])).
fof(f1071, plain, (~ p(sK99, sK100) | spl103_153), inference(avatar_component_clause, [], [f1069])).
fof(f1069, plain, (spl103_153 <=> p(sK99, sK100)), introduced(avatar_definition, [new_symbols(naming, [spl103_153])])).
fof(f1347, plain, (~ spl103_155 | spl103_163), inference(avatar_contradiction_clause, [], [f1346])).
fof(f1346, plain, ($false | (~ spl103_155 | spl103_163)), inference(subsumption_resolution, [], [f1117, f1080])).
fof(f1117, plain, (~ p(sK101, sK101) | spl103_163), inference(avatar_component_clause, [], [f1115])).
fof(f1115, plain, (spl103_163 <=> p(sK101, sK101)), introduced(avatar_definition, [new_symbols(naming, [spl103_163])])).
fof(f1343, plain, (~ spl103_22 | ~ spl103_23), inference(avatar_contradiction_clause, [], [f1342])).
fof(f1342, plain, ($false | (~ spl103_22 | ~ spl103_23)), inference(subsumption_resolution, [], [f473, f468])).
fof(f468, plain, (! [X2, X3] : ~ a(X2, X3) | ~ spl103_22), inference(avatar_component_clause, [], [f467])).
fof(f467, plain, (spl103_22 <=> ! [X3, X2] : ~ a(X2, X3)), introduced(avatar_definition, [new_symbols(naming, [spl103_22])])).
fof(f473, plain, (a(sK53, sK54) | ~ spl103_23), inference(avatar_component_clause, [], [f471])).
fof(f471, plain, (spl103_23 <=> a(sK53, sK54)), introduced(avatar_definition, [new_symbols(naming, [spl103_23])])).
fof(f1341, plain, (~ spl103_22 | ~ spl103_24), inference(avatar_contradiction_clause, [], [f1340])).
fof(f1340, plain, ($false | (~ spl103_22 | ~ spl103_24)), inference(resolution, [], [f477, f468])).
fof(f477, plain, (a(sK52, sK51) | ~ spl103_24), inference(avatar_component_clause, [], [f475])).
fof(f475, plain, (spl103_24 <=> a(sK52, sK51)), introduced(avatar_definition, [new_symbols(naming, [spl103_24])])).
fof(f1339, plain, (~ spl103_9 | spl103_103), inference(avatar_contradiction_clause, [], [f1338])).
fof(f1338, plain, ($false | (~ spl103_9 | spl103_103)), inference(subsumption_resolution, [], [f835, f408])).
fof(f835, plain, (~ p1(sK79) | spl103_103), inference(avatar_component_clause, [], [f833])).
fof(f833, plain, (spl103_103 <=> p1(sK79)), introduced(avatar_definition, [new_symbols(naming, [spl103_103])])).
fof(f1337, plain, (~ spl103_40 | ~ spl103_50), inference(avatar_contradiction_clause, [], [f1336])).
fof(f1336, plain, ($false | (~ spl103_40 | ~ spl103_50)), inference(subsumption_resolution, [], [f593, f549])).
fof(f549, plain, (! [X0] : ~ b(X0) | ~ spl103_40), inference(avatar_component_clause, [], [f548])).
fof(f548, plain, (spl103_40 <=> ! [X0] : ~ b(X0)), introduced(avatar_definition, [new_symbols(naming, [spl103_40])])).
fof(f593, plain, (b(sK63) | ~ spl103_50), inference(avatar_component_clause, [], [f591])).
fof(f591, plain, (spl103_50 <=> b(sK63)), introduced(avatar_definition, [new_symbols(naming, [spl103_50])])).
fof(f1335, plain, (~ spl103_48 | spl103_49), inference(avatar_contradiction_clause, [], [f1334])).
fof(f1334, plain, ($false | (~ spl103_48 | spl103_49)), inference(resolution, [], [f589, f584])).
fof(f584, plain, (! [X1] : a1(X1) | ~ spl103_48), inference(avatar_component_clause, [], [f583])).
fof(f583, plain, (spl103_48 <=> ! [X1] : a1(X1)), introduced(avatar_definition, [new_symbols(naming, [spl103_48])])).
fof(f589, plain, (~ a1(sK63) | spl103_49), inference(avatar_component_clause, [], [f587])).
fof(f587, plain, (spl103_49 <=> a1(sK63)), introduced(avatar_definition, [new_symbols(naming, [spl103_49])])).
fof(f1310, plain, (~ spl103_96 | ~ spl103_101), inference(avatar_contradiction_clause, [], [f1309])).
fof(f1309, plain, ($false | (~ spl103_96 | ~ spl103_101)), inference(subsumption_resolution, [], [f826, f802])).
fof(f802, plain, (! [X2] : ~ q1(X2) | ~ spl103_96), inference(avatar_component_clause, [], [f801])).
fof(f801, plain, (spl103_96 <=> ! [X2] : ~ q1(X2)), introduced(avatar_definition, [new_symbols(naming, [spl103_96])])).
fof(f1308, plain, (~ spl103_26 | ~ spl103_27), inference(avatar_contradiction_clause, [], [f1307])).
fof(f1307, plain, ($false | (~ spl103_26 | ~ spl103_27)), inference(subsumption_resolution, [], [f1306, f489])).
fof(f489, plain, (! [X1] : (~ a(X1, X1) | ~ a(X1, sK55)) | ~ spl103_27), inference(avatar_component_clause, [], [f488])).
fof(f488, plain, (spl103_27 <=> ! [X1] : (~ a(X1, X1) | ~ a(X1, sK55))), introduced(avatar_definition, [new_symbols(naming, [spl103_27])])).
fof(f1306, plain, (a(sK55, sK55) | ~ spl103_26), inference(factoring, [], [f485])).
fof(f485, plain, (! [X1] : (a(X1, sK55) | a(X1, X1)) | ~ spl103_26), inference(avatar_component_clause, [], [f484])).
fof(f484, plain, (spl103_26 <=> ! [X1] : (a(X1, sK55) | a(X1, X1))), introduced(avatar_definition, [new_symbols(naming, [spl103_26])])).
fof(f1302, plain, (spl103_118 | ~ spl103_119 | ~ spl103_120 | ~ spl103_121 | ~ spl103_122 | ~ spl103_123), inference(avatar_contradiction_clause, [], [f1301])).
fof(f1301, plain, ($false | (spl103_118 | ~ spl103_119 | ~ spl103_120 | ~ spl103_121 | ~ spl103_122 | ~ spl103_123)), inference(subsumption_resolution, [], [f1299, f912])).
fof(f912, plain, (~ eq(sK88, sK87) | spl103_118), inference(avatar_component_clause, [], [f910])).
fof(f910, plain, (spl103_118 <=> eq(sK88, sK87)), introduced(avatar_definition, [new_symbols(naming, [spl103_118])])).
fof(f1299, plain, (eq(sK88, sK87) | (~ spl103_119 | ~ spl103_120 | ~ spl103_121 | ~ spl103_122 | ~ spl103_123)), inference(resolution, [], [f1289, f917])).
fof(f917, plain, (eq(sK87, sK88) | ~ spl103_119), inference(avatar_component_clause, [], [f915])).
fof(f915, plain, (spl103_119 <=> eq(sK87, sK88)), introduced(avatar_definition, [new_symbols(naming, [spl103_119])])).
fof(f1289, plain, (! [X0, X1] : (~ eq(X1, X0) | eq(X0, X1)) | (~ spl103_120 | ~ spl103_121 | ~ spl103_122 | ~ spl103_123)), inference(subsumption_resolution, [], [f1288, f1241])).
fof(f1241, plain, (! [X4, X2, X3] : (a_member_of(sK89(X2, X3), X4) | a_member_of(sK89(X2, X3), X2) | ~ eq(X3, X4) | eq(X2, X3)) | (~ spl103_121 | ~ spl103_123)), inference(resolution, [], [f925, f933])).
fof(f933, plain, (! [X2, X5, X3] : (~ a_member_of(X5, X2) | ~ eq(X2, X3) | a_member_of(X5, X3)) | ~ spl103_123), inference(avatar_component_clause, [], [f932])).
fof(f932, plain, (spl103_123 <=> ! [X3, X5, X2] : (a_member_of(X5, X3) | ~ eq(X2, X3) | ~ a_member_of(X5, X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_123])])).
fof(f925, plain, (! [X2, X3] : (a_member_of(sK89(X2, X3), X3) | a_member_of(sK89(X2, X3), X2) | eq(X2, X3)) | ~ spl103_121), inference(avatar_component_clause, [], [f924])).
fof(f924, plain, (spl103_121 <=> ! [X3, X2] : (eq(X2, X3) | a_member_of(sK89(X2, X3), X2) | a_member_of(sK89(X2, X3), X3))), introduced(avatar_definition, [new_symbols(naming, [spl103_121])])).
fof(f1288, plain, (! [X0, X1] : (eq(X0, X1) | ~ eq(X1, X0) | ~ a_member_of(sK89(X0, X1), X0)) | (~ spl103_120 | ~ spl103_121 | ~ spl103_122 | ~ spl103_123)), inference(duplicate_literal_removal, [], [f1285])).
fof(f1285, plain, (! [X0, X1] : (eq(X0, X1) | ~ eq(X1, X0) | ~ eq(X1, X0) | ~ a_member_of(sK89(X0, X1), X0) | eq(X0, X1)) | (~ spl103_120 | ~ spl103_121 | ~ spl103_122 | ~ spl103_123)), inference(resolution, [], [f1261, f921])).
fof(f921, plain, (! [X2, X3] : (~ a_member_of(sK89(X2, X3), X3) | ~ a_member_of(sK89(X2, X3), X2) | eq(X2, X3)) | ~ spl103_120), inference(avatar_component_clause, [], [f920])).
fof(f920, plain, (spl103_120 <=> ! [X3, X2] : (eq(X2, X3) | ~ a_member_of(sK89(X2, X3), X2) | ~ a_member_of(sK89(X2, X3), X3))), introduced(avatar_definition, [new_symbols(naming, [spl103_120])])).
fof(f1261, plain, (! [X6, X4, X5] : (a_member_of(sK89(X5, X4), X6) | eq(X5, X4) | ~ eq(X6, X5) | ~ eq(X4, X5)) | (~ spl103_121 | ~ spl103_122 | ~ spl103_123)), inference(resolution, [], [f1256, f929])).
fof(f929, plain, (! [X2, X5, X3] : (~ a_member_of(X5, X3) | ~ eq(X2, X3) | a_member_of(X5, X2)) | ~ spl103_122), inference(avatar_component_clause, [], [f928])).
fof(f928, plain, (spl103_122 <=> ! [X3, X5, X2] : (a_member_of(X5, X2) | ~ eq(X2, X3) | ~ a_member_of(X5, X3))), introduced(avatar_definition, [new_symbols(naming, [spl103_122])])).
fof(f1256, plain, (! [X0, X1] : (a_member_of(sK89(X0, X1), X0) | ~ eq(X1, X0) | eq(X0, X1)) | (~ spl103_121 | ~ spl103_123)), inference(factoring, [], [f1241])).
fof(f1238, plain, (~ spl103_8 | ~ spl103_9), inference(avatar_contradiction_clause, [], [f1237])).
fof(f1237, plain, ($false | (~ spl103_8 | ~ spl103_9)), inference(subsumption_resolution, [], [f404, f408])).
fof(f404, plain, (! [X0] : ~ p1(sK43(X0)) | ~ spl103_8), inference(avatar_component_clause, [], [f403])).
fof(f403, plain, (spl103_8 <=> ! [X0] : ~ p1(sK43(X0))), introduced(avatar_definition, [new_symbols(naming, [spl103_8])])).
fof(f1236, plain, (spl103_36 | ~ spl103_38), inference(avatar_contradiction_clause, [], [f1235])).
fof(f1235, plain, ($false | (spl103_36 | ~ spl103_38)), inference(subsumption_resolution, [], [f532, f541])).
fof(f532, plain, (~ r1(sK60) | spl103_36), inference(avatar_component_clause, [], [f530])).
fof(f530, plain, (spl103_36 <=> r1(sK60)), introduced(avatar_definition, [new_symbols(naming, [spl103_36])])).
fof(f1234, plain, (~ spl103_58 | ~ spl103_59), inference(avatar_contradiction_clause, [], [f1233])).
fof(f1233, plain, ($false | (~ spl103_58 | ~ spl103_59)), inference(resolution, [], [f633, f629])).
fof(f629, plain, (! [X0] : ~ a(X0, X0) | ~ spl103_58), inference(avatar_component_clause, [], [f628])).
fof(f628, plain, (spl103_58 <=> ! [X0] : ~ a(X0, X0)), introduced(avatar_definition, [new_symbols(naming, [spl103_58])])).
fof(f633, plain, (! [X1] : a(sK66(X1), sK66(X1)) | ~ spl103_59), inference(avatar_component_clause, [], [f632])).
fof(f632, plain, (spl103_59 <=> ! [X1] : a(sK66(X1), sK66(X1))), introduced(avatar_definition, [new_symbols(naming, [spl103_59])])).
fof(f1230, plain, (~ spl103_41 | ~ spl103_46), inference(avatar_contradiction_clause, [], [f1229])).
fof(f1229, plain, ($false | (~ spl103_41 | ~ spl103_46)), inference(subsumption_resolution, [], [f575, f553])).
fof(f553, plain, (! [X0] : ~ a1(X0) | ~ spl103_41), inference(avatar_component_clause, [], [f552])).
fof(f575, plain, (a1(sK62) | ~ spl103_46), inference(avatar_component_clause, [], [f573])).
fof(f573, plain, (spl103_46 <=> a1(sK62)), introduced(avatar_definition, [new_symbols(naming, [spl103_46])])).
fof(f1228, plain, (~ spl103_96 | ~ spl103_97), inference(avatar_contradiction_clause, [], [f1227])).
fof(f1227, plain, ($false | (~ spl103_96 | ~ spl103_97)), inference(resolution, [], [f807, f802])).
fof(f1224, plain, (~ spl103_38 | spl103_92), inference(avatar_contradiction_clause, [], [f1223])).
fof(f1223, plain, ($false | (~ spl103_38 | spl103_92)), inference(subsumption_resolution, [], [f787, f541])).
fof(f787, plain, (~ r1(sK75) | spl103_92), inference(avatar_component_clause, [], [f785])).
fof(f785, plain, (spl103_92 <=> r1(sK75)), introduced(avatar_definition, [new_symbols(naming, [spl103_92])])).
fof(f1220, plain, (~ spl103_38 | spl103_93), inference(avatar_contradiction_clause, [], [f1219])).
fof(f1219, plain, ($false | (~ spl103_38 | spl103_93)), inference(subsumption_resolution, [], [f791, f541])).
fof(f791, plain, (~ r1(sK76) | spl103_93), inference(avatar_component_clause, [], [f789])).
fof(f789, plain, (spl103_93 <=> r1(sK76)), introduced(avatar_definition, [new_symbols(naming, [spl103_93])])).
fof(f1212, plain, (~ spl103_40 | ~ spl103_42), inference(avatar_contradiction_clause, [], [f1211])).
fof(f1211, plain, ($false | (~ spl103_40 | ~ spl103_42)), inference(resolution, [], [f558, f549])).
fof(f558, plain, (b(sK61) | ~ spl103_42), inference(avatar_component_clause, [], [f556])).
fof(f556, plain, (spl103_42 <=> b(sK61)), introduced(avatar_definition, [new_symbols(naming, [spl103_42])])).
fof(f1208, plain, (~ spl103_34 | spl103_55), inference(avatar_contradiction_clause, [], [f1207])).
fof(f1207, plain, ($false | (~ spl103_34 | spl103_55)), inference(subsumption_resolution, [], [f616, f522])).
fof(f616, plain, (~ q1(sK65) | spl103_55), inference(avatar_component_clause, [], [f614])).
fof(f614, plain, (spl103_55 <=> q1(sK65)), introduced(avatar_definition, [new_symbols(naming, [spl103_55])])).
fof(f1206, plain, (~ spl103_9 | ~ spl103_81), inference(avatar_contradiction_clause, [], [f1205])).
fof(f1205, plain, ($false | (~ spl103_9 | ~ spl103_81)), inference(subsumption_resolution, [], [f736, f408])).
fof(f736, plain, (! [X0] : ~ p1(sK70(X0)) | ~ spl103_81), inference(avatar_component_clause, [], [f735])).
fof(f735, plain, (spl103_81 <=> ! [X0] : ~ p1(sK70(X0))), introduced(avatar_definition, [new_symbols(naming, [spl103_81])])).
fof(f1200, plain, (~ spl103_67 | ~ spl103_129), inference(avatar_contradiction_clause, [], [f1199])).
fof(f1199, plain, ($false | (~ spl103_67 | ~ spl103_129)), inference(subsumption_resolution, [], [f1192, f668])).
fof(f668, plain, (! [X2, X1] : ~ p(X1, X2) | ~ spl103_67), inference(avatar_component_clause, [], [f667])).
fof(f667, plain, (spl103_67 <=> ! [X1, X2] : ~ p(X1, X2)), introduced(avatar_definition, [new_symbols(naming, [spl103_67])])).
fof(f1192, plain, (! [X6, X7] : p(X6, X7) | (~ spl103_67 | ~ spl103_129)), inference(resolution, [], [f957, f668])).
fof(f957, plain, (! [X2, X0] : (p(X2, sK90(X0)) | p(X0, X2)) | ~ spl103_129), inference(avatar_component_clause, [], [f956])).
fof(f956, plain, (spl103_129 <=> ! [X0, X2] : (p(X0, X2) | p(X2, sK90(X0)))), introduced(avatar_definition, [new_symbols(naming, [spl103_129])])).
fof(f1187, plain, (spl103_33 | ~ spl103_34), inference(avatar_contradiction_clause, [], [f1186])).
fof(f1186, plain, ($false | (spl103_33 | ~ spl103_34)), inference(subsumption_resolution, [], [f518, f522])).
fof(f518, plain, (~ q1(sK58) | spl103_33), inference(avatar_component_clause, [], [f516])).
fof(f516, plain, (spl103_33 <=> q1(sK58)), introduced(avatar_definition, [new_symbols(naming, [spl103_33])])).
fof(f1181, plain, (~ spl103_40 | ~ spl103_52 | ~ spl103_53), inference(avatar_contradiction_clause, [], [f1180])).
fof(f1180, plain, ($false | (~ spl103_40 | ~ spl103_52 | ~ spl103_53)), inference(subsumption_resolution, [], [f1179, f549])).
fof(f1179, plain, (b(sK64) | (~ spl103_52 | ~ spl103_53)), inference(resolution, [], [f607, f603])).
fof(f603, plain, (a1(sK64) | ~ spl103_52), inference(avatar_component_clause, [], [f601])).
fof(f601, plain, (spl103_52 <=> a1(sK64)), introduced(avatar_definition, [new_symbols(naming, [spl103_52])])).
fof(f1174, plain, (~ spl103_62 | ~ spl103_70), inference(avatar_contradiction_clause, [], [f1173])).
fof(f1173, plain, ($false | (~ spl103_62 | ~ spl103_70)), inference(resolution, [], [f681, f645])).
fof(f681, plain, (r1(sK68) | ~ spl103_70), inference(avatar_component_clause, [], [f679])).
fof(f1172, plain, (spl103_62 | ~ spl103_67 | ~ spl103_69), inference(avatar_split_clause, [], [f1171, f675, f667, f644])).
fof(f1171, plain, (! [X3] : ~ r1(X3) | (~ spl103_67 | ~ spl103_69)), inference(subsumption_resolution, [], [f676, f668])).
fof(f1170, plain, (~ spl103_67 | ~ spl103_71), inference(avatar_contradiction_clause, [], [f1169])).
fof(f1169, plain, ($false | (~ spl103_67 | ~ spl103_71)), inference(subsumption_resolution, [], [f684, f668])).
fof(f1155, plain, (spl103_67 | ~ spl103_125 | ~ spl103_126), inference(avatar_split_clause, [], [f1154, f944, f940, f667])).
fof(f940, plain, (spl103_125 <=> ! [X3, X0, X2] : (~ p(X2, sK90(X0)) | ~ p(X3, X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_125])])).
fof(f944, plain, (spl103_126 <=> ! [X3, X0, X2] : (p(X2, X0) | ~ p(X3, X2))), introduced(avatar_definition, [new_symbols(naming, [spl103_126])])).
fof(f1154, plain, (! [X2, X3] : ~ p(X3, X2) | (~ spl103_125 | ~ spl103_126)), inference(subsumption_resolution, [], [f941, f945])).
fof(f945, plain, (! [X2, X0, X3] : (~ p(X3, X2) | p(X2, X0)) | ~ spl103_126), inference(avatar_component_clause, [], [f944])).
fof(f941, plain, (! [X2, X0, X3] : (~ p(X2, sK90(X0)) | ~ p(X3, X2)) | ~ spl103_125), inference(avatar_component_clause, [], [f940])).
fof(f1153, plain, (~ spl103_9 | spl103_109), inference(avatar_contradiction_clause, [], [f1152])).
fof(f1152, plain, ($false | (~ spl103_9 | spl103_109)), inference(subsumption_resolution, [], [f862, f408])).
fof(f862, plain, (~ p1(sK82) | spl103_109), inference(avatar_component_clause, [], [f860])).
fof(f860, plain, (spl103_109 <=> p1(sK82)), introduced(avatar_definition, [new_symbols(naming, [spl103_109])])).
fof(f1151, plain, (~ spl103_9 | spl103_108), inference(avatar_contradiction_clause, [], [f1150])).
fof(f1150, plain, ($false | (~ spl103_9 | spl103_108)), inference(subsumption_resolution, [], [f858, f408])).
fof(f858, plain, (~ p1(sK81) | spl103_108), inference(avatar_component_clause, [], [f856])).
fof(f856, plain, (spl103_108 <=> p1(sK81)), introduced(avatar_definition, [new_symbols(naming, [spl103_108])])).
fof(f1149, plain, (~ spl103_9 | spl103_31), inference(avatar_contradiction_clause, [], [f1148])).
fof(f1148, plain, ($false | (~ spl103_9 | spl103_31)), inference(subsumption_resolution, [], [f508, f408])).
fof(f508, plain, (~ p1(sK57) | spl103_31), inference(avatar_component_clause, [], [f506])).
fof(f506, plain, (spl103_31 <=> p1(sK57)), introduced(avatar_definition, [new_symbols(naming, [spl103_31])])).
fof(f1147, plain, (~ spl103_9 | spl103_30), inference(avatar_contradiction_clause, [], [f1146])).
fof(f1146, plain, ($false | (~ spl103_9 | spl103_30)), inference(subsumption_resolution, [], [f504, f408])).
fof(f504, plain, (~ p1(sK56) | spl103_30), inference(avatar_component_clause, [], [f502])).
fof(f502, plain, (spl103_30 <=> p1(sK56)), introduced(avatar_definition, [new_symbols(naming, [spl103_30])])).
fof(f1145, plain, (~ spl103_9 | spl103_20), inference(avatar_contradiction_clause, [], [f1144])).
fof(f1144, plain, ($false | (~ spl103_9 | spl103_20)), inference(subsumption_resolution, [], [f459, f408])).
fof(f459, plain, (~ p1(sK50) | spl103_20), inference(avatar_component_clause, [], [f457])).
fof(f457, plain, (spl103_20 <=> p1(sK50)), introduced(avatar_definition, [new_symbols(naming, [spl103_20])])).
fof(f1143, plain, (~ spl103_9 | spl103_112), inference(avatar_contradiction_clause, [], [f1142])).
fof(f1142, plain, ($false | (~ spl103_9 | spl103_112)), inference(subsumption_resolution, [], [f879, f408])).
fof(f879, plain, (~ p1(sK84) | spl103_112), inference(avatar_component_clause, [], [f877])).
fof(f877, plain, (spl103_112 <=> p1(sK84)), introduced(avatar_definition, [new_symbols(naming, [spl103_112])])).
fof(f1141, plain, (~ spl103_9 | spl103_111), inference(avatar_contradiction_clause, [], [f1140])).
fof(f1140, plain, ($false | (~ spl103_9 | spl103_111)), inference(subsumption_resolution, [], [f875, f408])).
fof(f875, plain, (~ p1(sK83) | spl103_111), inference(avatar_component_clause, [], [f873])).
fof(f873, plain, (spl103_111 <=> p1(sK83)), introduced(avatar_definition, [new_symbols(naming, [spl103_111])])).
fof(f1139, plain, (~ spl103_9 | spl103_16), inference(avatar_contradiction_clause, [], [f1138])).
fof(f1138, plain, ($false | (~ spl103_9 | spl103_16)), inference(subsumption_resolution, [], [f441, f408])).
fof(f441, plain, (~ p1(sK48) | spl103_16), inference(avatar_component_clause, [], [f439])).
fof(f439, plain, (spl103_16 <=> p1(sK48)), introduced(avatar_definition, [new_symbols(naming, [spl103_16])])).
fof(f1137, plain, (~ spl103_9 | spl103_17), inference(avatar_contradiction_clause, [], [f1136])).
fof(f1136, plain, ($false | (~ spl103_9 | spl103_17)), inference(subsumption_resolution, [], [f445, f408])).
fof(f445, plain, (~ p1(sK47) | spl103_17), inference(avatar_component_clause, [], [f443])).
fof(f443, plain, (spl103_17 <=> p1(sK47)), introduced(avatar_definition, [new_symbols(naming, [spl103_17])])).
fof(f1135, plain, (~ spl103_9 | spl103_19), inference(avatar_contradiction_clause, [], [f1134])).
fof(f1134, plain, ($false | (~ spl103_9 | spl103_19)), inference(resolution, [], [f455, f408])).
fof(f455, plain, (~ p1(sK49) | spl103_19), inference(avatar_component_clause, [], [f453])).
fof(f453, plain, (spl103_19 <=> p1(sK49)), introduced(avatar_definition, [new_symbols(naming, [spl103_19])])).
fof(f1133, plain, (~ spl103_2 | ~ spl103_3), inference(avatar_contradiction_clause, [], [f1132])).
fof(f1132, plain, ($false | (~ spl103_2 | ~ spl103_3)), inference(resolution, [], [f383, f379])).
fof(f379, plain, (! [X1] : ~ p(X1, sK40) | ~ spl103_2), inference(avatar_component_clause, [], [f378])).
fof(f378, plain, (spl103_2 <=> ! [X1] : ~ p(X1, sK40)), introduced(avatar_definition, [new_symbols(naming, [spl103_2])])).
fof(f383, plain, (! [X3] : p(sK41, X3) | ~ spl103_3), inference(avatar_component_clause, [], [f382])).
fof(f382, plain, (spl103_3 <=> ! [X3] : p(sK41, X3)), introduced(avatar_definition, [new_symbols(naming, [spl103_3])])).
fof(f1131, plain, (~ spl103_5 | ~ spl103_6), inference(avatar_contradiction_clause, [], [f1130])).
fof(f1130, plain, ($false | (~ spl103_5 | ~ spl103_6)), inference(resolution, [], [f396, f391])).
fof(f396, plain, (p1(sK42) | ~ spl103_6), inference(avatar_component_clause, [], [f394])).
fof(f394, plain, (spl103_6 <=> p1(sK42)), introduced(avatar_definition, [new_symbols(naming, [spl103_6])])).
fof(f1129, plain, (spl103_113 | spl103_98 | spl103_91 | spl103_86 | spl103_144 | spl103_140 | spl103_29 | spl103_83 | spl103_80 | spl103_57 | spl103_133 | spl103_110 | spl103_130 | spl103_28 | spl103_54 | spl103_107 | spl103_51 | spl103_47 | spl103_25 | spl103_43 | spl103_39 | spl103_21 | spl103_18 | spl103_15 | spl103_12 | spl103_10 | spl103_124 | spl103_117 | spl103_102 | spl103_7 | spl103_76 | spl103_35 | spl103_4 | spl103_1 | spl103_162 | spl103_164 | spl103_155 | spl103_32 | spl103_75 | spl103_72 | spl103_152 | spl103_65 | spl103_61), inference(avatar_split_clause, [], [f365, f640, f659, f1065, f687, f703, f512, f1079, f1121, f1111, f374, f386, f526, f711, f399, f829, f906, f936, f411, f421, f435, f449, f463, f544, f561, f480, f578, f596, f852, f610, f492, f960, f869, f978, f624, f730, f744, f498, f1009, f1031, f759, f781, f810, f886])).
fof(f886, plain, (spl103_113 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl103_113])])).
fof(f810, plain, (spl103_98 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl103_98])])).
fof(f781, plain, (spl103_91 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl103_91])])).
fof(f759, plain, (spl103_86 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl103_86])])).
fof(f1031, plain, (spl103_144 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl103_144])])).
fof(f1009, plain, (spl103_140 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl103_140])])).
fof(f498, plain, (spl103_29 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl103_29])])).
fof(f744, plain, (spl103_83 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl103_83])])).
fof(f730, plain, (spl103_80 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl103_80])])).
fof(f624, plain, (spl103_57 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl103_57])])).
fof(f978, plain, (spl103_133 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl103_133])])).
fof(f869, plain, (spl103_110 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl103_110])])).
fof(f960, plain, (spl103_130 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl103_130])])).
fof(f492, plain, (spl103_28 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl103_28])])).
fof(f610, plain, (spl103_54 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl103_54])])).
fof(f852, plain, (spl103_107 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl103_107])])).
fof(f596, plain, (spl103_51 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl103_51])])).
fof(f578, plain, (spl103_47 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl103_47])])).
fof(f480, plain, (spl103_25 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl103_25])])).
fof(f561, plain, (spl103_43 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl103_43])])).
fof(f544, plain, (spl103_39 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl103_39])])).
fof(f463, plain, (spl103_21 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl103_21])])).
fof(f449, plain, (spl103_18 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl103_18])])).
fof(f435, plain, (spl103_15 <=> sP34), introduced(avatar_definition, [new_symbols(naming, [spl103_15])])).
fof(f421, plain, (spl103_12 <=> sP35), introduced(avatar_definition, [new_symbols(naming, [spl103_12])])).
fof(f411, plain, (spl103_10 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl103_10])])).
fof(f936, plain, (spl103_124 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl103_124])])).
fof(f906, plain, (spl103_117 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl103_117])])).
fof(f829, plain, (spl103_102 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl103_102])])).
fof(f399, plain, (spl103_7 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl103_7])])).
fof(f711, plain, (spl103_76 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl103_76])])).
fof(f526, plain, (spl103_35 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl103_35])])).
fof(f386, plain, (spl103_4 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl103_4])])).
fof(f374, plain, (spl103_1 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl103_1])])).
fof(f1111, plain, (spl103_162 <=> p1(z)), introduced(avatar_definition, [new_symbols(naming, [spl103_162])])).
fof(f512, plain, (spl103_32 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl103_32])])).
fof(f703, plain, (spl103_75 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl103_75])])).
fof(f687, plain, (spl103_72 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl103_72])])).
fof(f1065, plain, (spl103_152 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl103_152])])).
fof(f659, plain, (spl103_65 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl103_65])])).
fof(f640, plain, (spl103_61 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl103_61])])).
fof(f365, plain, ! [X2, X1] : (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | p(X1, X2) | p1(sK102) | p1(z) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(cnf_transformation, [], [f212])).
fof(f212, plain, (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | (~ p(sK101, sK101) & ! [X1, X2] : p(X1, X2)) | (! [X3] : ~ p1(X3) & p1(sK102)) | (~ p1(z) & p1(z)) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(skolemisation, [status(esa), new_symbols(skolem, [sK101, sK102])], [f209, f211, f210])).
fof(f210, plain, (? [X0] : ~ p(X0, X0) => ~ p(sK101, sK101)), introduced(choice_axiom, [])).
fof(f211, plain, (? [X4] : p1(X4) => p1(sK102)), introduced(choice_axiom, [])).
fof(f209, plain, (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | (? [X0] : ~ p(X0, X0) & ! [X1, X2] : p(X1, X2)) | (! [X3] : ~ p1(X3) & ? [X4] : p1(X4)) | (~ p1(z) & p1(z)) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(rectify, [], [f47])).
fof(f47, plain, (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | (? [X15] : ~ p(X15, X15) & ! [X13, X14] : p(X13, X14)) | (! [X17] : ~ p1(X17) & ? [X16] : p1(X16)) | (~ p1(z) & p1(z)) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(definition_folding, [], [f6, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10, e9, e8, e7])).
fof(f7, plain, (? [X7, X8] : ! [X9, X10] : (~ p(X7, X8) & s1(X7) & (p(X9, X10) | ~ s1(X7)) & r1(X8) & r1(X7) & (p(X8, X10) | ~ r1(X10)) & q1(X8) & q1(X7) & (p(X9, X7) | ~ q1(X9))) | ~ sP0), inference(usedef, [], [e7])).
fof(e7, plain, (sP0 <=> ? [X7, X8] : ! [X9, X10] : (~ p(X7, X8) & s1(X7) & (p(X9, X10) | ~ s1(X7)) & r1(X8) & r1(X7) & (p(X8, X10) | ~ r1(X10)) & q1(X8) & q1(X7) & (p(X9, X7) | ~ q1(X9)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f8, plain, (? [X118] : ! [X119, X120, X121, X122, X123] : ((~ c(X122) | ~ p1(X122)) & (~ g(X121) | ~ p1(X121)) & (p1(X123) | ~ s(X118, X123)) & (c(f(X120)) | g(X120) | ~ e(X120)) & (s(X119, f(X119)) | g(X119) | ~ e(X119)) & e(X118) & p1(X118)) | ~ sP1), inference(usedef, [], [e8])).
fof(e8, plain, (sP1 <=> ? [X118] : ! [X119, X120, X121, X122, X123] : ((~ c(X122) | ~ p1(X122)) & (~ g(X121) | ~ p1(X121)) & (p1(X123) | ~ s(X118, X123)) & (c(f(X120)) | g(X120) | ~ e(X120)) & (s(X119, f(X119)) | g(X119) | ~ e(X119)) & e(X118) & p1(X118))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f9, plain, (? [X110, X111, X112] : (! [X116, X117] : (~ q(X116, X117) | ~ p1(X116)) & ! [X113, X114] : (q(X113, X114) | ~ r(X113, X114)) & ! [X115] : (p1(X115) | ~ s1(X115)) & r(X111, X112) & s1(X111) & s1(X110)) | ~ sP2), inference(usedef, [], [e9])).
fof(e9, plain, (sP2 <=> ? [X110, X111, X112] : (! [X116, X117] : (~ q(X116, X117) | ~ p1(X116)) & ! [X113, X114] : (q(X113, X114) | ~ r(X113, X114)) & ! [X115] : (p1(X115) | ~ s1(X115)) & r(X111, X112) & s1(X111) & s1(X110))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f10, plain, (? [X88, X89, X90] : (! [X94, X95] : (~ q(X94, X95) | ~ p1(X94)) & ! [X91, X92] : (q(X91, X92) | ~ r(X91, X92)) & ! [X93] : (p1(X93) | ~ s1(X93)) & r(X89, X90) & s1(X89) & s1(X88)) | ~ sP3), inference(usedef, [], [e10])).
fof(e10, plain, (sP3 <=> ? [X88, X89, X90] : (! [X94, X95] : (~ q(X94, X95) | ~ p1(X94)) & ! [X91, X92] : (q(X91, X92) | ~ r(X91, X92)) & ! [X93] : (p1(X93) | ~ s1(X93)) & r(X89, X90) & s1(X89) & s1(X88))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f11, plain, ((((((~ q0 & q0) | (b0 & ~ b0)) & a0) | ! [X83] : ~ p1(X83)) & ? [X82] : p1(X82)) | ~ sP4), inference(usedef, [], [e11])).
fof(e11, plain, (sP4 <=> (((((~ q0 & q0) | (b0 & ~ b0)) & a0) | ! [X83] : ~ p1(X83)) & ? [X82] : p1(X82))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f12, plain, (! [X42] : ? [X43] : ! [X44] : ((~ p(X44, X43) & p(X44, X42) & p(X42, X44)) | (! [X45] : ~ p(X45, X44) & p(X44, X43))) | ~ sP5), inference(usedef, [], [e12])).
fof(e12, plain, (sP5 <=> ! [X42] : ? [X43] : ! [X44] : ((~ p(X44, X43) & p(X44, X42) & p(X42, X44)) | (! [X45] : ~ p(X45, X44) & p(X44, X43)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f13, plain, ((? [X40, X41] : (~ eq(X41, X40) & eq(X40, X41)) & ! [X37, X38] : (eq(X37, X38) <=> ! [X39] : (a_member_of(X39, X37) <=> a_member_of(X39, X38)))) | ~ sP6), inference(usedef, [], [e13])).
fof(e13, plain, (sP6 <=> (? [X40, X41] : (~ eq(X41, X40) & eq(X40, X41)) & ! [X37, X38] : (eq(X37, X38) <=> ! [X39] : (a_member_of(X39, X37) <=> a_member_of(X39, X38))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f14, plain, (? [X136, X137] : (! [X139, X140] : (~ q1(X139) | ((~ r1(X136) | ~ r1(X137)) & r1(X140)) | (~ p1(X139) & p1(f(X140)))) & ! [X138] : q1(f(X138))) | ~ sP7), inference(usedef, [], [e14])).
fof(e14, plain, (sP7 <=> ? [X136, X137] : (! [X139, X140] : (~ q1(X139) | ((~ r1(X136) | ~ r1(X137)) & r1(X140)) | (~ p1(X139) & p1(f(X140)))) & ! [X138] : q1(f(X138)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f15, plain, (? [X84, X85] : (! [X87] : ((~ p1(X85) & q1(X87)) | (~ p1(X84) & p1(X87))) & ! [X86] : (p1(X86) | ~ q1(X86))) | ~ sP8), inference(usedef, [], [e15])).
fof(e15, plain, (sP8 <=> ? [X84, X85] : (! [X87] : ((~ p1(X85) & q1(X87)) | (~ p1(X84) & p1(X87))) & ! [X86] : (p1(X86) | ~ q1(X86)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f16, plain, (? [X73, X74] : (! [X76] : ((~ p1(X74) & q1(X76)) | (~ p1(X73) & p1(X76))) & ! [X75] : (p1(X75) | ~ q1(X75))) | ~ sP9), inference(usedef, [], [e16])).
fof(e16, plain, (sP9 <=> ? [X73, X74] : (! [X76] : ((~ p1(X74) & q1(X76)) | (~ p1(X73) & p1(X76))) & ! [X75] : (p1(X75) | ~ q1(X75)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f17, plain, (? [X33, X34] : (! [X36] : ((~ p1(X34) & q1(X36)) | (~ p1(X33) & p1(X36))) & ! [X35] : (p1(X35) | ~ q1(X35))) | ~ sP10), inference(usedef, [], [e17])).
fof(e17, plain, (sP10 <=> ? [X33, X34] : (! [X36] : ((~ p1(X34) & q1(X36)) | (~ p1(X33) & p1(X36))) & ! [X35] : (p1(X35) | ~ q1(X35)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f18, plain, (? [X131, X132] : (! [X134, X135] : (~ q1(X134) | ((((~ r1(X132) | ~ r1(X131)) & r1(X135)) | ~ p1(X134)) & p1(f(X135)))) & ! [X133] : q1(f(X133))) | ~ sP11), inference(usedef, [], [e18])).
fof(e18, plain, (sP11 <=> ? [X131, X132] : (! [X134, X135] : (~ q1(X134) | ((((~ r1(X132) | ~ r1(X131)) & r1(X135)) | ~ p1(X134)) & p1(f(X135)))) & ! [X133] : q1(f(X133)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f19, plain, (? [X127, X128] : (! [X129, X130] : (~ q1(X129) | ((((~ r1(X128) | ~ r1(X127)) & r1(X130)) | ~ p1(X129)) & p1(f(X130)))) & q1(f(X127))) | ~ sP12), inference(usedef, [], [e19])).
fof(e19, plain, (sP12 <=> ? [X127, X128] : (! [X129, X130] : (~ q1(X129) | ((((~ r1(X128) | ~ r1(X127)) & r1(X130)) | ~ p1(X129)) & p1(f(X130)))) & q1(f(X127)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f20, plain, ((! [X126] : (~ c(X126) | ~ a1(X126)) & ? [X124] : (~ b(X124) & a1(X124)) & ! [X125] : (c(X125) | b(X125) | ~ a1(X125))) | ~ sP13), inference(usedef, [], [e20])).
fof(e20, plain, (sP13 <=> (! [X126] : (~ c(X126) | ~ a1(X126)) & ? [X124] : (~ b(X124) & a1(X124)) & ! [X125] : (c(X125) | b(X125) | ~ a1(X125)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f21, plain, ((! [X105] : ? [X106] : (~ r1(X105) & ~ p1(X106)) & ? [X103] : q1(X103) & ! [X104] : p1(X104)) | ~ sP14), inference(usedef, [], [e21])).
fof(e21, plain, (sP14 <=> (! [X105] : ? [X106] : (~ r1(X105) & ~ p1(X106)) & ? [X103] : q1(X103) & ! [X104] : p1(X104))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f22, plain, ((! [X101] : ? [X102] : (~ r1(X101) & ~ p1(X102)) & ! [X99] : ? [X100] : (q1(X100) & p1(X99))) | ~ sP15), inference(usedef, [], [e22])).
fof(e22, plain, (sP15 <=> (! [X101] : ? [X102] : (~ r1(X101) & ~ p1(X102)) & ! [X99] : ? [X100] : (q1(X100) & p1(X99)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f23, plain, (? [X28] : (~ q1(X28) & ! [X30] : (p1(X30) | ~ r1(X30)) & r1(X28) & ! [X29] : (q1(X29) | ~ p1(X29))) | ~ sP16), inference(usedef, [], [e23])).
fof(e23, plain, (sP16 <=> ? [X28] : (~ q1(X28) & ! [X30] : (p1(X30) | ~ r1(X30)) & r1(X28) & ! [X29] : (q1(X29) | ~ p1(X29)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f24, plain, ((~ b0 & ~ a0 & ~ (a0 <=> b0)) | ~ sP17), inference(usedef, [], [e24])).
fof(e24, plain, (sP17 <=> (~ b0 & ~ a0 & ~ (a0 <=> b0))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f25, plain, ((~ (a0 <=> b0) & b0 & a0) | ~ sP18), inference(usedef, [], [e25])).
fof(e25, plain, (sP18 <=> (~ (a0 <=> b0) & b0 & a0)), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f26, plain, (? [X3] : (! [X5, X6] : ((~ q(X5, X6) & q(f(X3), X3)) | ~ p(X5, X6)) & ! [X4] : (p(f(X4), X4) | (~ r1(X4) & r1(X3)))) | ~ sP19), inference(usedef, [], [e26])).
fof(e26, plain, (sP19 <=> ? [X3] : (! [X5, X6] : ((~ q(X5, X6) & q(f(X3), X3)) | ~ p(X5, X6)) & ! [X4] : (p(f(X4), X4) | (~ r1(X4) & r1(X3))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f27, plain, ((! [X2] : (~ r1(X2) & p1(X2)) & ? [X0] : (r1(X0) | ~ q1(X0)) & ! [X1] : (q1(X1) | ~ p1(X1))) | ~ sP20), inference(usedef, [], [e27])).
fof(e27, plain, (sP20 <=> (! [X2] : (~ r1(X2) & p1(X2)) & ? [X0] : (r1(X0) | ~ q1(X0)) & ! [X1] : (q1(X1) | ~ p1(X1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f28, plain, ((! [X98] : ~ a(X98, X98) & ! [X96] : ? [X97] : (a(X97, X97) & a(X96, X97))) | ~ sP21), inference(usedef, [], [e28])).
fof(e28, plain, (sP21 <=> (! [X98] : ~ a(X98, X98) & ! [X96] : ? [X97] : (a(X97, X97) & a(X96, X97)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f29, plain, ((? [X79] : ~ q1(X79) & ! [X78] : p1(X78) & ! [X77] : (q1(X77) | ~ p1(X77))) | ~ sP22), inference(usedef, [], [e29])).
fof(e29, plain, (sP22 <=> (? [X79] : ~ q1(X79) & ! [X78] : p1(X78) & ! [X77] : (q1(X77) | ~ p1(X77)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f30, plain, ((! [X72] : ~ b(X72) & ? [X71] : a1(X71) & ! [X70] : (b(X70) | ~ a1(X70))) | ~ sP23), inference(usedef, [], [e30])).
fof(e30, plain, (sP23 <=> (! [X72] : ~ b(X72) & ? [X71] : a1(X71) & ! [X70] : (b(X70) | ~ a1(X70)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f31, plain, ((! [X69] : ~ b(X69) & ! [X68] : a1(X68) & ? [X67] : (b(X67) | ~ a1(X67))) | ~ sP24), inference(usedef, [], [e31])).
fof(e31, plain, (sP24 <=> (! [X69] : ~ b(X69) & ! [X68] : a1(X68) & ? [X67] : (b(X67) | ~ a1(X67)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f32, plain, ((! [X64] : (~ b(X64) | ~ a1(X64)) & ! [X62] : b(X62) & ? [X63] : a1(X63)) | ~ sP25), inference(usedef, [], [e32])).
fof(e32, plain, (sP25 <=> (! [X64] : (~ b(X64) | ~ a1(X64)) & ! [X62] : b(X62) & ? [X63] : a1(X63))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f33, plain, ((! [X61] : (~ b(X61) & ~ a1(X61)) & ? [X60] : b(X60)) | ~ sP26), inference(usedef, [], [e33])).
fof(e33, plain, (sP26 <=> (! [X61] : (~ b(X61) & ~ a1(X61)) & ? [X60] : b(X60))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f34, plain, (? [X24, X25] : ! [X26, X27] : (~ r1(X25) & p1(X24) & (r1(X27) | ~ p1(X26))) | ~ sP27), inference(usedef, [], [e34])).
fof(e34, plain, (sP27 <=> ? [X24, X25] : ! [X26, X27] : (~ r1(X25) & p1(X24) & (r1(X27) | ~ p1(X26)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f35, plain, (? [X11] : (~ q1(X11) & ! [X12] : (q1(X12) & p1(X12))) | ~ sP28), inference(usedef, [], [e35])).
fof(e35, plain, (sP28 <=> ? [X11] : (~ q1(X11) & ! [X12] : (q1(X12) & p1(X12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f36, plain, ((? [X108, X109] : (~ p1(X109) | ~ p1(X108)) & ! [X107] : p1(X107)) | ~ sP29), inference(usedef, [], [e36])).
fof(e36, plain, (sP29 <=> (? [X108, X109] : (~ p1(X109) | ~ p1(X108)) & ! [X107] : p1(X107))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f37, plain, ((! [X81] : ~ p1(X81) & ! [X80] : p1(X80)) | ~ sP30), inference(usedef, [], [e37])).
fof(e37, plain, (sP30 <=> (! [X81] : ~ p1(X81) & ! [X80] : p1(X80))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f38, plain, (? [X65] : ! [X66] : (a(X66, X65) <=> ~ a(X66, X66)) | ~ sP31), inference(usedef, [], [e38])).
fof(e38, plain, (sP31 <=> ? [X65] : ! [X66] : (a(X66, X65) <=> ~ a(X66, X66))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f39, plain, (~ (? [X56, X57] : a(X56, X57) <=> ? [X58, X59] : a(X59, X58)) | ~ sP32), inference(usedef, [], [e39])).
fof(e39, plain, (sP32 <=> ~ (? [X56, X57] : a(X56, X57) <=> ? [X58, X59] : a(X59, X58))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f40, plain, (? [X53, X54] : ((~ p1(X54) | ~ p1(X53)) & ! [X55] : p1(X55)) | ~ sP33), inference(usedef, [], [e40])).
fof(e40, plain, (sP33 <=> ? [X53, X54] : ((~ p1(X54) | ~ p1(X53)) & ! [X55] : p1(X55))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f41, plain, (((? [X51] : ~ p1(X51) | ? [X52] : ~ p1(X52)) & ! [X50] : p1(X50)) | ~ sP34), inference(usedef, [], [e41])).
fof(e41, plain, (sP34 <=> ((? [X51] : ~ p1(X51) | ? [X52] : ~ p1(X52)) & ! [X50] : p1(X50))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f42, plain, (~ (? [X48] : p1(X48) <=> ? [X49] : p1(X49)) | ~ sP35), inference(usedef, [], [e42])).
fof(e42, plain, (sP35 <=> ~ (? [X48] : p1(X48) <=> ? [X49] : p1(X49))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f43, plain, ((! [X47] : ~ p1(X47) & ? [X46] : p1(X46)) | ~ sP36), inference(usedef, [], [e43])).
fof(e43, plain, (sP36 <=> (! [X47] : ~ p1(X47) & ? [X46] : p1(X46))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f44, plain, (! [X31] : ? [X32] : (~ p1(X32) & p1(X31)) | ~ sP37), inference(usedef, [], [e44])).
fof(e44, plain, (sP37 <=> ! [X31] : ? [X32] : (~ p1(X32) & p1(X31))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f45, plain, (! [X22] : (~ p1(X22) & ? [X23] : p1(X23)) | ~ sP38), inference(usedef, [], [e45])).
fof(e45, plain, (sP38 <=> ! [X22] : (~ p1(X22) & ? [X23] : p1(X23))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f46, plain, ((? [X20] : ! [X21] : ~ p(X21, X20) & ? [X18] : ! [X19] : p(X18, X19)) | ~ sP39), inference(usedef, [], [e46])).
fof(e46, plain, (sP39 <=> (? [X20] : ! [X21] : ~ p(X21, X20) & ? [X18] : ! [X19] : p(X18, X19))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f6, plain, ((! [X2] : (~ r1(X2) & p1(X2)) & ? [X0] : (r1(X0) | ~ q1(X0)) & ! [X1] : (q1(X1) | ~ p1(X1))) | ? [X3] : (! [X5, X6] : ((~ q(X5, X6) & q(f(X3), X3)) | ~ p(X5, X6)) & ! [X4] : (p(f(X4), X4) | (~ r1(X4) & r1(X3)))) | ? [X7, X8] : ! [X9, X10] : (~ p(X7, X8) & s1(X7) & (p(X9, X10) | ~ s1(X7)) & r1(X8) & r1(X7) & (p(X8, X10) | ~ r1(X10)) & q1(X8) & q1(X7) & (p(X9, X7) | ~ q1(X9))) | (~ (a0 <=> b0) & b0 & a0) | (~ b0 & ~ a0 & ~ (a0 <=> b0)) | ? [X11] : (~ q1(X11) & ! [X12] : (q1(X12) & p1(X12))) | (? [X15] : ~ p(X15, X15) & ! [X13, X14] : p(X13, X14)) | (! [X17] : ~ p1(X17) & ? [X16] : p1(X16)) | (~ p1(z) & p1(z)) | (? [X20] : ! [X21] : ~ p(X21, X20) & ? [X18] : ! [X19] : p(X18, X19)) | ! [X22] : (~ p1(X22) & ? [X23] : p1(X23)) | ? [X24, X25] : ! [X26, X27] : (~ r1(X25) & p1(X24) & (r1(X27) | ~ p1(X26))) | ? [X28] : (~ q1(X28) & ! [X30] : (p1(X30) | ~ r1(X30)) & r1(X28) & ! [X29] : (q1(X29) | ~ p1(X29))) | ! [X31] : ? [X32] : (~ p1(X32) & p1(X31)) | ? [X33, X34] : (! [X36] : ((~ p1(X34) & q1(X36)) | (~ p1(X33) & p1(X36))) & ! [X35] : (p1(X35) | ~ q1(X35))) | (? [X40, X41] : (~ eq(X41, X40) & eq(X40, X41)) & ! [X37, X38] : (eq(X37, X38) <=> ! [X39] : (a_member_of(X39, X37) <=> a_member_of(X39, X38)))) | ! [X42] : ? [X43] : ! [X44] : ((~ p(X44, X43) & p(X44, X42) & p(X42, X44)) | (! [X45] : ~ p(X45, X44) & p(X44, X43))) | (! [X47] : ~ p1(X47) & ? [X46] : p1(X46)) | ~ (? [X48] : p1(X48) <=> ? [X49] : p1(X49)) | ((? [X51] : ~ p1(X51) | ? [X52] : ~ p1(X52)) & ! [X50] : p1(X50)) | ? [X53, X54] : ((~ p1(X54) | ~ p1(X53)) & ! [X55] : p1(X55)) | ~ (? [X56, X57] : a(X56, X57) <=> ? [X58, X59] : a(X59, X58)) | (! [X61] : (~ b(X61) & ~ a1(X61)) & ? [X60] : b(X60)) | (! [X64] : (~ b(X64) | ~ a1(X64)) & ! [X62] : b(X62) & ? [X63] : a1(X63)) | ? [X65] : ! [X66] : (a(X66, X65) <=> ~ a(X66, X66)) | (! [X69] : ~ b(X69) & ! [X68] : a1(X68) & ? [X67] : (b(X67) | ~ a1(X67))) | (! [X72] : ~ b(X72) & ? [X71] : a1(X71) & ! [X70] : (b(X70) | ~ a1(X70))) | ? [X73, X74] : (! [X76] : ((~ p1(X74) & q1(X76)) | (~ p1(X73) & p1(X76))) & ! [X75] : (p1(X75) | ~ q1(X75))) | (? [X79] : ~ q1(X79) & ! [X78] : p1(X78) & ! [X77] : (q1(X77) | ~ p1(X77))) | (! [X81] : ~ p1(X81) & ! [X80] : p1(X80)) | (((((~ q0 & q0) | (b0 & ~ b0)) & a0) | ! [X83] : ~ p1(X83)) & ? [X82] : p1(X82)) | ? [X84, X85] : (! [X87] : ((~ p1(X85) & q1(X87)) | (~ p1(X84) & p1(X87))) & ! [X86] : (p1(X86) | ~ q1(X86))) | ? [X88, X89, X90] : (! [X94, X95] : (~ q(X94, X95) | ~ p1(X94)) & ! [X91, X92] : (q(X91, X92) | ~ r(X91, X92)) & ! [X93] : (p1(X93) | ~ s1(X93)) & r(X89, X90) & s1(X89) & s1(X88)) | (! [X98] : ~ a(X98, X98) & ! [X96] : ? [X97] : (a(X97, X97) & a(X96, X97))) | (! [X101] : ? [X102] : (~ r1(X101) & ~ p1(X102)) & ! [X99] : ? [X100] : (q1(X100) & p1(X99))) | (! [X105] : ? [X106] : (~ r1(X105) & ~ p1(X106)) & ? [X103] : q1(X103) & ! [X104] : p1(X104)) | (? [X108, X109] : (~ p1(X109) | ~ p1(X108)) & ! [X107] : p1(X107)) | ? [X110, X111, X112] : (! [X116, X117] : (~ q(X116, X117) | ~ p1(X116)) & ! [X113, X114] : (q(X113, X114) | ~ r(X113, X114)) & ! [X115] : (p1(X115) | ~ s1(X115)) & r(X111, X112) & s1(X111) & s1(X110)) | ? [X118] : ! [X119, X120, X121, X122, X123] : ((~ c(X122) | ~ p1(X122)) & (~ g(X121) | ~ p1(X121)) & (p1(X123) | ~ s(X118, X123)) & (c(f(X120)) | g(X120) | ~ e(X120)) & (s(X119, f(X119)) | g(X119) | ~ e(X119)) & e(X118) & p1(X118)) | (! [X126] : (~ c(X126) | ~ a1(X126)) & ? [X124] : (~ b(X124) & a1(X124)) & ! [X125] : (c(X125) | b(X125) | ~ a1(X125))) | ? [X127, X128] : (! [X129, X130] : (~ q1(X129) | ((((~ r1(X128) | ~ r1(X127)) & r1(X130)) | ~ p1(X129)) & p1(f(X130)))) & q1(f(X127))) | ? [X131, X132] : (! [X134, X135] : (~ q1(X134) | ((((~ r1(X132) | ~ r1(X131)) & r1(X135)) | ~ p1(X134)) & p1(f(X135)))) & ! [X133] : q1(f(X133))) | ? [X136, X137] : (! [X139, X140] : (~ q1(X139) | ((~ r1(X136) | ~ r1(X137)) & r1(X140)) | (~ p1(X139) & p1(f(X140)))) & ! [X138] : q1(f(X138)))), inference(flattening, [], [f5])).
fof(f5, plain, ((! [X2] : (~ r1(X2) & p1(X2)) & (? [X0] : (r1(X0) | ~ q1(X0)) & ! [X1] : (q1(X1) | ~ p1(X1)))) | ? [X3] : (! [X5, X6] : ((~ q(X5, X6) & q(f(X3), X3)) | ~ p(X5, X6)) & ! [X4] : (p(f(X4), X4) | (~ r1(X4) & r1(X3)))) | ? [X7, X8] : ! [X9, X10] : (~ p(X7, X8) & (s1(X7) & (p(X9, X10) | ~ s1(X7)) & r1(X8) & r1(X7) & (p(X8, X10) | ~ r1(X10)) & q1(X8) & q1(X7) & (p(X9, X7) | ~ q1(X9)))) | (~ (a0 <=> b0) & (b0 & a0)) | (~ b0 & ~ a0 & ~ (a0 <=> b0)) | ? [X11] : (~ q1(X11) & ! [X12] : (q1(X12) & p1(X12))) | (? [X15] : ~ p(X15, X15) & ! [X13, X14] : p(X13, X14)) | (! [X17] : ~ p1(X17) & ? [X16] : p1(X16)) | (~ p1(z) & p1(z)) | (? [X20] : ! [X21] : ~ p(X21, X20) & ? [X18] : ! [X19] : p(X18, X19)) | ! [X22] : (~ p1(X22) & ? [X23] : p1(X23)) | ? [X24, X25] : ! [X26, X27] : ((~ r1(X25) & p1(X24)) & (r1(X27) | ~ p1(X26))) | ? [X28] : ((~ q1(X28) & ! [X30] : (p1(X30) | ~ r1(X30))) & (r1(X28) & ! [X29] : (q1(X29) | ~ p1(X29)))) | ! [X31] : ? [X32] : (~ p1(X32) & p1(X31)) | ? [X33, X34] : (! [X36] : ((~ p1(X34) & q1(X36)) | (~ p1(X33) & p1(X36))) & ! [X35] : (p1(X35) | ~ q1(X35))) | (? [X40, X41] : (~ eq(X41, X40) & eq(X40, X41)) & ! [X37, X38] : (eq(X37, X38) <=> ! [X39] : (a_member_of(X39, X37) <=> a_member_of(X39, X38)))) | ! [X42] : ? [X43] : ! [X44] : ((~ p(X44, X43) & (p(X44, X42) & p(X42, X44))) | (! [X45] : ~ p(X45, X44) & p(X44, X43))) | (! [X47] : ~ p1(X47) & ? [X46] : p1(X46)) | ~ (? [X48] : p1(X48) <=> ? [X49] : p1(X49)) | ((? [X51] : ~ p1(X51) | ? [X52] : ~ p1(X52)) & ! [X50] : p1(X50)) | ? [X53, X54] : ((~ p1(X54) | ~ p1(X53)) & ! [X55] : p1(X55)) | ~ (? [X56, X57] : a(X56, X57) <=> ? [X58, X59] : a(X59, X58)) | (! [X61] : (~ b(X61) & ~ a1(X61)) & ? [X60] : b(X60)) | (! [X64] : (~ b(X64) | ~ a1(X64)) & (! [X62] : b(X62) & ? [X63] : a1(X63))) | ? [X65] : ! [X66] : (a(X66, X65) <=> ~ a(X66, X66)) | ((! [X69] : ~ b(X69) & ! [X68] : a1(X68)) & ? [X67] : (b(X67) | ~ a1(X67))) | ((! [X72] : ~ b(X72) & ? [X71] : a1(X71)) & ! [X70] : (b(X70) | ~ a1(X70))) | ? [X73, X74] : (! [X76] : ((~ p1(X74) & q1(X76)) | (~ p1(X73) & p1(X76))) & ! [X75] : (p1(X75) | ~ q1(X75))) | ((? [X79] : ~ q1(X79) & ! [X78] : p1(X78)) & ! [X77] : (q1(X77) | ~ p1(X77))) | (! [X81] : ~ p1(X81) & ! [X80] : p1(X80)) | (((((~ q0 & q0) | (b0 & ~ b0)) & a0) | ! [X83] : ~ p1(X83)) & ? [X82] : p1(X82)) | ? [X84, X85] : (! [X87] : ((~ p1(X85) & q1(X87)) | (~ p1(X84) & p1(X87))) & ! [X86] : (p1(X86) | ~ q1(X86))) | ? [X88, X89, X90] : (! [X94, X95] : (~ q(X94, X95) | ~ p1(X94)) & (! [X91, X92] : (q(X91, X92) | ~ r(X91, X92)) & ! [X93] : (p1(X93) | ~ s1(X93)) & r(X89, X90) & s1(X89) & s1(X88))) | (! [X98] : ~ a(X98, X98) & ! [X96] : ? [X97] : (a(X97, X97) & a(X96, X97))) | (! [X101] : ? [X102] : (~ r1(X101) & ~ p1(X102)) & ! [X99] : ? [X100] : (q1(X100) & p1(X99))) | (! [X105] : ? [X106] : (~ r1(X105) & ~ p1(X106)) & (? [X103] : q1(X103) & ! [X104] : p1(X104))) | (? [X108, X109] : (~ p1(X109) | ~ p1(X108)) & ! [X107] : p1(X107)) | ? [X110, X111, X112] : (! [X116, X117] : (~ q(X116, X117) | ~ p1(X116)) & (! [X113, X114] : (q(X113, X114) | ~ r(X113, X114)) & ! [X115] : (p1(X115) | ~ s1(X115)) & r(X111, X112) & s1(X111) & s1(X110))) | ? [X118] : ! [X119, X120, X121, X122, X123] : (((~ c(X122) | ~ p1(X122)) & (~ g(X121) | ~ p1(X121))) & ((p1(X123) | ~ s(X118, X123)) & ((c(f(X120)) | g(X120)) | ~ e(X120)) & ((s(X119, f(X119)) | g(X119)) | ~ e(X119)) & e(X118) & p1(X118))) | (! [X126] : (~ c(X126) | ~ a1(X126)) & (? [X124] : (~ b(X124) & a1(X124)) & ! [X125] : ((c(X125) | b(X125)) | ~ a1(X125)))) | ? [X127, X128] : (! [X129, X130] : (~ q1(X129) | ((((~ r1(X128) | ~ r1(X127)) & r1(X130)) | ~ p1(X129)) & p1(f(X130)))) & q1(f(X127))) | ? [X131, X132] : (! [X134, X135] : (~ q1(X134) | ((((~ r1(X132) | ~ r1(X131)) & r1(X135)) | ~ p1(X134)) & p1(f(X135)))) & ! [X133] : q1(f(X133))) | ? [X136, X137] : (! [X139, X140] : (~ q1(X139) | ((~ r1(X136) | ~ r1(X137)) & r1(X140)) | (~ p1(X139) & p1(f(X140)))) & ! [X138] : q1(f(X138)))), inference(ennf_transformation, [], [f4])).
fof(f4, plain, ~ (((? [X0] : (q1(X0) => r1(X0)) & ! [X1] : (p1(X1) => q1(X1))) => ? [X2] : (p1(X2) => r1(X2))) & ! [X3] : (! [X4] : ((r1(X3) => r1(X4)) => p(f(X4), X4)) => ? [X5, X6] : ((q(f(X3), X3) => q(X5, X6)) & p(X5, X6))) & ! [X7, X8] : ? [X9, X10] : ((s1(X7) & (s1(X7) => p(X9, X10)) & r1(X8) & r1(X7) & (r1(X10) => p(X8, X10)) & q1(X8) & q1(X7) & (q1(X9) => p(X9, X7))) => p(X7, X8)) & ((b0 & a0) => (a0 <=> b0)) & (b0 | a0 | (a0 <=> b0)) & ! [X11] : (! [X12] : (q1(X12) & p1(X12)) => q1(X11)) & (! [X13, X14] : p(X13, X14) => ! [X15] : p(X15, X15)) & (? [X16] : p1(X16) => ? [X17] : p1(X17)) & (p1(z) => p1(z)) & (? [X18] : ! [X19] : p(X18, X19) => ! [X20] : ? [X21] : p(X21, X20)) & ? [X22] : (? [X23] : p1(X23) => p1(X22)) & ! [X24, X25] : ? [X26, X27] : ((p1(X26) => r1(X27)) => (p1(X24) => r1(X25))) & ! [X28] : ((r1(X28) & ! [X29] : (p1(X29) => q1(X29))) => (! [X30] : (r1(X30) => p1(X30)) => q1(X28))) & ? [X31] : ! [X32] : (p1(X31) => p1(X32)) & ! [X33, X34] : (! [X35] : (q1(X35) => p1(X35)) => ? [X36] : ((q1(X36) => p1(X34)) & (p1(X36) => p1(X33)))) & (! [X37, X38] : (eq(X37, X38) <=> ! [X39] : (a_member_of(X39, X37) <=> a_member_of(X39, X38))) => ! [X40, X41] : (eq(X40, X41) => eq(X41, X40))) & ? [X42] : ! [X43] : ? [X44] : (((p(X44, X42) & p(X42, X44)) => p(X44, X43)) & (p(X44, X43) => ? [X45] : p(X45, X44))) & (? [X46] : p1(X46) => ? [X47] : p1(X47)) & (? [X48] : p1(X48) <=> ? [X49] : p1(X49)) & (! [X50] : p1(X50) => (! [X51] : p1(X51) & ! [X52] : p1(X52))) & ! [X53, X54] : (! [X55] : p1(X55) => (p1(X54) & p1(X53))) & (? [X56, X57] : a(X56, X57) <=> ? [X58, X59] : a(X59, X58)) & (? [X60] : b(X60) => ? [X61] : (b(X61) | a1(X61))) & ((! [X62] : b(X62) & ? [X63] : a1(X63)) => ? [X64] : (b(X64) & a1(X64))) & ~ ? [X65] : ! [X66] : (a(X66, X65) <=> ~ a(X66, X66)) & (? [X67] : (a1(X67) => b(X67)) => (! [X68] : a1(X68) => ? [X69] : b(X69))) & (! [X70] : (a1(X70) => b(X70)) => (? [X71] : a1(X71) => ? [X72] : b(X72))) & ! [X73, X74] : (! [X75] : (q1(X75) => p1(X75)) => ? [X76] : ((q1(X76) => p1(X74)) & (p1(X76) => p1(X73)))) & (! [X77] : (p1(X77) => q1(X77)) => (! [X78] : p1(X78) => ! [X79] : q1(X79))) & (! [X80] : p1(X80) => ? [X81] : p1(X81)) & (? [X82] : p1(X82) => ((a0 => ((q0 => q0) & (~ b0 | b0))) & ? [X83] : p1(X83))) & ! [X84, X85] : (! [X86] : (q1(X86) => p1(X86)) => ? [X87] : ((q1(X87) => p1(X85)) & (p1(X87) => p1(X84)))) & ! [X88, X89, X90] : ((! [X91, X92] : (r(X91, X92) => q(X91, X92)) & ! [X93] : (s1(X93) => p1(X93)) & r(X89, X90) & s1(X89) & s1(X88)) => ? [X94, X95] : (q(X94, X95) & p1(X94))) & (! [X96] : ? [X97] : (a(X97, X97) & a(X96, X97)) => ? [X98] : a(X98, X98)) & (! [X99] : ? [X100] : (q1(X100) & p1(X99)) => ? [X101] : ! [X102] : (r1(X101) | p1(X102))) & ((? [X103] : q1(X103) & ! [X104] : p1(X104)) => ? [X105] : ! [X106] : (r1(X105) | p1(X106))) & (! [X107] : p1(X107) => ! [X108, X109] : (p1(X109) & p1(X108))) & ! [X110, X111, X112] : ((! [X113, X114] : (r(X113, X114) => q(X113, X114)) & ! [X115] : (s1(X115) => p1(X115)) & r(X111, X112) & s1(X111) & s1(X110)) => ? [X116, X117] : (q(X116, X117) & p1(X116))) & ! [X118] : ? [X119, X120, X121, X122, X123] : (((s(X118, X123) => p1(X123)) & (e(X120) => (c(f(X120)) | g(X120))) & (e(X119) => (s(X119, f(X119)) | g(X119))) & e(X118) & p1(X118)) => ((c(X122) & p1(X122)) | (g(X121) & p1(X121)))) & ((~ ! [X124] : (a1(X124) => b(X124)) & ! [X125] : (a1(X125) => (c(X125) | b(X125)))) => ? [X126] : (c(X126) & a1(X126))) & ! [X127, X128] : (q1(f(X127)) => ? [X129, X130] : (q1(X129) & (p1(f(X130)) => ((r1(X130) => (r1(X128) & r1(X127))) & p1(X129))))) & ! [X131, X132] : (! [X133] : q1(f(X133)) => ? [X134, X135] : (q1(X134) & (p1(f(X135)) => ((r1(X135) => (r1(X132) & r1(X131))) & p1(X134))))) & ! [X136, X137] : (! [X138] : q1(f(X138)) => ? [X139, X140] : (q1(X139) & (r1(X140) => (r1(X136) & r1(X137))) & (p1(f(X140)) => p1(X139))))), inference(pure_predicate_removal, [], [f3])).
fof(f3, plain, ~ (((? [X0] : (q1(X0) => r1(X0)) & ! [X1] : (p1(X1) => q1(X1))) => ? [X2] : (p1(X2) => r1(X2))) & ! [X3] : (! [X4] : ((r1(X3) => r1(X4)) => p(f(X4), X4)) => ? [X5, X6] : ((q(f(X3), X3) => q(X5, X6)) & p(X5, X6))) & ! [X7, X8] : ? [X9, X10] : ((s1(X7) & (s1(X7) => p(X9, X10)) & r1(X8) & r1(X7) & (r1(X10) => p(X8, X10)) & q1(X8) & q1(X7) & (q1(X9) => p(X9, X7))) => p(X7, X8)) & ((b0 & a0) => (a0 <=> b0)) & (b0 | a0 | (a0 <=> b0)) & ! [X11] : ((! [X12] : (q1(X12) & p1(X12)) & (g0 | f0)) => q1(X11)) & (! [X13, X14] : p(X13, X14) => ! [X15] : p(X15, X15)) & (? [X16] : p1(X16) => ? [X17] : p1(X17)) & (p1(z) => p1(z)) & (? [X18] : ! [X19] : p(X18, X19) => ! [X20] : ? [X21] : p(X21, X20)) & ? [X22] : (? [X23] : p1(X23) => p1(X22)) & ! [X24, X25] : ? [X26, X27] : ((p1(X26) => r1(X27)) => (p1(X24) => r1(X25))) & ! [X28] : ((r1(X28) & ! [X29] : (p1(X29) => q1(X29))) => (! [X30] : (r1(X30) => p1(X30)) => q1(X28))) & ? [X31] : ! [X32] : (p1(X31) => p1(X32)) & ! [X33, X34] : (! [X35] : (q1(X35) => p1(X35)) => ? [X36] : ((q1(X36) => p1(X34)) & (p1(X36) => p1(X33)))) & (! [X37, X38] : (eq(X37, X38) <=> ! [X39] : (a_member_of(X39, X37) <=> a_member_of(X39, X38))) => ! [X40, X41] : (eq(X40, X41) => eq(X41, X40))) & ? [X42] : ! [X43] : ? [X44] : (((p(X44, X42) & p(X42, X44)) => p(X44, X43)) & (p(X44, X43) => ? [X45] : p(X45, X44))) & (? [X46] : p1(X46) => ? [X47] : p1(X47)) & (? [X48] : p1(X48) <=> ? [X49] : p1(X49)) & (! [X50] : p1(X50) => (! [X51] : p1(X51) & ! [X52] : p1(X52))) & ! [X53, X54] : (! [X55] : p1(X55) => (p1(X54) & p1(X53))) & (? [X56, X57] : a(X56, X57) <=> ? [X58, X59] : a(X59, X58)) & (? [X60] : b(X60) => ? [X61] : (b(X61) | a1(X61))) & ((! [X62] : b(X62) & ? [X63] : a1(X63)) => ? [X64] : (b(X64) & a1(X64))) & ~ ? [X65] : ! [X66] : (a(X66, X65) <=> ~ a(X66, X66)) & (? [X67] : (a1(X67) => b(X67)) => (! [X68] : a1(X68) => ? [X69] : b(X69))) & (! [X70] : (a1(X70) => b(X70)) => (? [X71] : a1(X71) => ? [X72] : b(X72))) & ! [X73, X74] : (! [X75] : (q1(X75) => p1(X75)) => ? [X76] : ((q1(X76) => p1(X74)) & (p1(X76) => p1(X73)))) & (! [X77] : (p1(X77) => q1(X77)) => (! [X78] : p1(X78) => ! [X79] : q1(X79))) & (! [X80] : p1(X80) => ? [X81] : p1(X81)) & (? [X82] : p1(X82) => ((a0 => ((q0 => q0) & (~ b0 | b0))) & ? [X83] : p1(X83))) & ! [X84, X85] : (! [X86] : (q1(X86) => p1(X86)) => ? [X87] : ((q1(X87) => p1(X85)) & (p1(X87) => p1(X84)))) & ! [X88, X89, X90] : ((! [X91, X92] : (r(X91, X92) => q(X91, X92)) & ! [X93] : (s1(X93) => p1(X93)) & r(X89, X90) & s1(X89) & s1(X88)) => ? [X94, X95] : (q(X94, X95) & p1(X94))) & (! [X96] : ? [X97] : (a(X97, X97) & a(X96, X97)) => ? [X98] : a(X98, X98)) & (! [X99] : ? [X100] : (q1(X100) & p1(X99)) => ? [X101] : ! [X102] : (r1(X101) | p1(X102))) & ((? [X103] : q1(X103) & ! [X104] : p1(X104)) => ? [X105] : ! [X106] : (r1(X105) | p1(X106))) & (! [X107] : p1(X107) => ! [X108, X109] : (p1(X109) & p1(X108))) & ! [X110, X111, X112] : ((! [X113, X114] : (r(X113, X114) => q(X113, X114)) & ! [X115] : (s1(X115) => p1(X115)) & r(X111, X112) & s1(X111) & s1(X110)) => ? [X116, X117] : (q(X116, X117) & p1(X116))) & ! [X118] : ? [X119, X120, X121, X122, X123] : (((s(X118, X123) => p1(X123)) & (e(X120) => (c(f(X120)) | g(X120))) & (e(X119) => (s(X119, f(X119)) | g(X119))) & e(X118) & p1(X118)) => ((c(X122) & p1(X122)) | (g(X121) & p1(X121)))) & ((~ ! [X124] : (a1(X124) => b(X124)) & ! [X125] : (a1(X125) => (c(X125) | b(X125)))) => ? [X126] : (c(X126) & a1(X126))) & ! [X127, X128] : (q1(f(X127)) => ? [X129, X130] : (q1(X129) & (p1(f(X130)) => ((r1(X130) => (r1(X128) & r1(X127))) & p1(X129))))) & ! [X131, X132] : (! [X133] : q1(f(X133)) => ? [X134, X135] : (q1(X134) & (p1(f(X135)) => ((r1(X135) => (r1(X132) & r1(X131))) & p1(X134))))) & ! [X136, X137] : (! [X138] : q1(f(X138)) => ? [X139, X140] : (q1(X139) & (r1(X140) => (r1(X136) & r1(X137))) & (p1(f(X140)) => p1(X139))))), inference(rectify, [], [f2])).
fof(f2, plain, ~ (((? [X4] : (q1(X4) => r1(X4)) & ! [X3] : (p1(X3) => q1(X3))) => ? [X2] : (p1(X2) => r1(X2))) & ! [X1] : (! [X4] : ((r1(X1) => r1(X4)) => p(f(X4), X4)) => ? [X3, X4] : ((q(f(X1), X1) => q(X3, X4)) & p(X3, X4))) & ! [X5, X1] : ? [X3, X4] : ((s1(X5) & (s1(X5) => p(X3, X4)) & r1(X1) & r1(X5) & (r1(X4) => p(X1, X4)) & q1(X1) & q1(X5) & (q1(X3) => p(X3, X5))) => p(X5, X1)) & ((b0 & a0) => (a0 <=> b0)) & (b0 | a0 | (a0 <=> b0)) & ! [X5] : ((! [X3] : (q1(X3) & p1(X3)) & (g0 | f0)) => q1(X5)) & (! [X3, X4] : p(X3, X4) => ! [X3] : p(X3, X3)) & (? [X3] : p1(X3) => ? [X4] : p1(X4)) & (p1(z) => p1(z)) & (? [X3] : ! [X4] : p(X3, X4) => ! [X4] : ? [X3] : p(X3, X4)) & ? [X4] : (? [X3] : p1(X3) => p1(X4)) & ! [X5, X1] : ? [X3, X4] : ((p1(X3) => r1(X4)) => (p1(X5) => r1(X1))) & ! [X1] : ((r1(X1) & ! [X3] : (p1(X3) => q1(X3))) => (! [X4] : (r1(X4) => p1(X4)) => q1(X1))) & ? [X3] : ! [X4] : (p1(X3) => p1(X4)) & ! [X5, X1] : (! [X4] : (q1(X4) => p1(X4)) => ? [X3] : ((q1(X3) => p1(X1)) & (p1(X3) => p1(X5)))) & (! [X3, X4] : (eq(X3, X4) <=> ! [X2] : (a_member_of(X2, X3) <=> a_member_of(X2, X4))) => ! [X5, X1] : (eq(X5, X1) => eq(X1, X5))) & ? [X2] : ! [X3] : ? [X4] : (((p(X4, X2) & p(X2, X4)) => p(X4, X3)) & (p(X4, X3) => ? [X9] : p(X9, X4))) & (? [X3] : p1(X3) => ? [X2] : p1(X2)) & (? [X3] : p1(X3) <=> ? [X4] : p1(X4)) & (! [X3] : p1(X3) => (! [X4] : p1(X4) & ! [X3] : p1(X3))) & ! [X5, X1] : (! [X3] : p1(X3) => (p1(X1) & p1(X5))) & (? [X3, X4] : a(X3, X4) <=> ? [X4, X3] : a(X3, X4)) & (? [X3] : b(X3) => ? [X3] : (b(X3) | a1(X3))) & ((! [X3] : b(X3) & ? [X3] : a1(X3)) => ? [X3] : (b(X3) & a1(X3))) & ~ ? [X4] : ! [X3] : (a(X3, X4) <=> ~ a(X3, X3)) & (? [X3] : (a1(X3) => b(X3)) => (! [X3] : a1(X3) => ? [X3] : b(X3))) & (! [X3] : (a1(X3) => b(X3)) => (? [X3] : a1(X3) => ? [X3] : b(X3))) & ! [X5, X1] : (! [X2] : (q1(X2) => p1(X2)) => ? [X3] : ((q1(X3) => p1(X1)) & (p1(X3) => p1(X5)))) & (! [X3] : (p1(X3) => q1(X3)) => (! [X3] : p1(X3) => ! [X3] : q1(X3))) & (! [X3] : p1(X3) => ? [X4] : p1(X4)) & (? [X3] : p1(X3) => ((a0 => ((q0 => q0) & (~ b0 | b0))) & ? [X3] : p1(X3))) & ! [X5, X1] : (! [X4] : (q1(X4) => p1(X4)) => ? [X3] : ((q1(X3) => p1(X1)) & (p1(X3) => p1(X5)))) & ! [X5, X1, X0] : ((! [X3, X4] : (r(X3, X4) => q(X3, X4)) & ! [X3] : (s1(X3) => p1(X3)) & r(X1, X0) & s1(X1) & s1(X5)) => ? [X3, X4] : (q(X3, X4) & p1(X3))) & (! [X3] : ? [X4] : (a(X4, X4) & a(X3, X4)) => ? [X2] : a(X2, X2)) & (! [X3] : ? [X4] : (q1(X4) & p1(X3)) => ? [X2] : ! [X4] : (r1(X2) | p1(X4))) & ((? [X4] : q1(X4) & ! [X3] : p1(X3)) => ? [X2] : ! [X4] : (r1(X2) | p1(X4))) & (! [X3] : p1(X3) => ! [X5, X1] : (p1(X1) & p1(X5))) & ! [X5, X1, X0] : ((! [X3, X4] : (r(X3, X4) => q(X3, X4)) & ! [X3] : (s1(X3) => p1(X3)) & r(X1, X0) & s1(X1) & s1(X5)) => ? [X3, X4] : (q(X3, X4) & p1(X3))) & ! [X5] : ? [X3, X6, X7, X8, X4] : (((s(X5, X4) => p1(X4)) & (e(X6) => (c(f(X6)) | g(X6))) & (e(X3) => (s(X3, f(X3)) | g(X3))) & e(X5) & p1(X5)) => ((c(X8) & p1(X8)) | (g(X7) & p1(X7)))) & ((~ ! [X3] : (a1(X3) => b(X3)) & ! [X3] : (a1(X3) => (c(X3) | b(X3)))) => ? [X3] : (c(X3) & a1(X3))) & ! [X1, X0] : (q1(f(X1)) => ? [X3, X4] : (q1(X3) & (p1(f(X4)) => ((r1(X4) => (r1(X0) & r1(X1))) & p1(X3))))) & ! [X1, X0] : (! [X2] : q1(f(X2)) => ? [X3, X4] : (q1(X3) & (p1(f(X4)) => ((r1(X4) => (r1(X0) & r1(X1))) & p1(X3))))) & ! [X0, X1] : (! [X2] : q1(f(X2)) => ? [X3, X4] : (q1(X3) & (r1(X4) => (r1(X0) & r1(X1))) & (p1(f(X4)) => p1(X3))))), inference(negated_conjecture, [], [f1])).
fof(f1, plain, ~ (((? [X4] : (q1(X4) => r1(X4)) & ! [X3] : (p1(X3) => q1(X3))) => ? [X2] : (p1(X2) => r1(X2))) & ! [X1] : (! [X4] : ((r1(X1) => r1(X4)) => p(f(X4), X4)) => ? [X3, X4] : ((q(f(X1), X1) => q(X3, X4)) & p(X3, X4))) & ! [X5, X1] : ? [X3, X4] : ((s1(X5) & (s1(X5) => p(X3, X4)) & r1(X1) & r1(X5) & (r1(X4) => p(X1, X4)) & q1(X1) & q1(X5) & (q1(X3) => p(X3, X5))) => p(X5, X1)) & ((b0 & a0) => (a0 <=> b0)) & (b0 | a0 | (a0 <=> b0)) & ! [X5] : ((! [X3] : (q1(X3) & p1(X3)) & (g0 | f0)) => q1(X5)) & (! [X3, X4] : p(X3, X4) => ! [X3] : p(X3, X3)) & (? [X3] : p1(X3) => ? [X4] : p1(X4)) & (p1(z) => p1(z)) & (? [X3] : ! [X4] : p(X3, X4) => ! [X4] : ? [X3] : p(X3, X4)) & ? [X4] : (? [X3] : p1(X3) => p1(X4)) & ! [X5, X1] : ? [X3, X4] : ((p1(X3) => r1(X4)) => (p1(X5) => r1(X1))) & ! [X1] : ((r1(X1) & ! [X3] : (p1(X3) => q1(X3))) => (! [X4] : (r1(X4) => p1(X4)) => q1(X1))) & ? [X3] : ! [X4] : (p1(X3) => p1(X4)) & ! [X5, X1] : (! [X4] : (q1(X4) => p1(X4)) => ? [X3] : ((q1(X3) => p1(X1)) & (p1(X3) => p1(X5)))) & (! [X3, X4] : (eq(X3, X4) <=> ! [X2] : (a_member_of(X2, X3) <=> a_member_of(X2, X4))) => ! [X5, X1] : (eq(X5, X1) => eq(X1, X5))) & ? [X2] : ! [X3] : ? [X4] : (((p(X4, X2) & p(X2, X4)) => p(X4, X3)) & (p(X4, X3) => ? [X9] : p(X9, X4))) & (? [X3] : p1(X3) => ? [X2] : p1(X2)) & (? [X3] : p1(X3) <=> ? [X4] : p1(X4)) & (! [X3] : p1(X3) => (! [X4] : p1(X4) & ! [X3] : p1(X3))) & ! [X5, X1] : (! [X3] : p1(X3) => (p1(X1) & p1(X5))) & (? [X3, X4] : a(X3, X4) <=> ? [X4, X3] : a(X3, X4)) & (? [X3] : b(X3) => ? [X3] : (b(X3) | a1(X3))) & ((! [X3] : b(X3) & ? [X3] : a1(X3)) => ? [X3] : (b(X3) & a1(X3))) & ~ ? [X4] : ! [X3] : (a(X3, X4) <=> ~ a(X3, X3)) & (? [X3] : (a1(X3) => b(X3)) => (! [X3] : a1(X3) => ? [X3] : b(X3))) & (! [X3] : (a1(X3) => b(X3)) => (? [X3] : a1(X3) => ? [X3] : b(X3))) & ! [X5, X1] : (! [X2] : (q1(X2) => p1(X2)) => ? [X3] : ((q1(X3) => p1(X1)) & (p1(X3) => p1(X5)))) & (! [X3] : (p1(X3) => q1(X3)) => (! [X3] : p1(X3) => ! [X3] : q1(X3))) & (! [X3] : p1(X3) => ? [X4] : p1(X4)) & (? [X3] : p1(X3) => ((a0 => ((q0 => q0) & (~ b0 | b0))) & ? [X3] : p1(X3))) & ! [X5, X1] : (! [X4] : (q1(X4) => p1(X4)) => ? [X3] : ((q1(X3) => p1(X1)) & (p1(X3) => p1(X5)))) & ! [X5, X1, X0] : ((! [X3, X4] : (r(X3, X4) => q(X3, X4)) & ! [X3] : (s1(X3) => p1(X3)) & r(X1, X0) & s1(X1) & s1(X5)) => ? [X3, X4] : (q(X3, X4) & p1(X3))) & (! [X3] : ? [X4] : (a(X4, X4) & a(X3, X4)) => ? [X2] : a(X2, X2)) & (! [X3] : ? [X4] : (q1(X4) & p1(X3)) => ? [X2] : ! [X4] : (r1(X2) | p1(X4))) & ((? [X4] : q1(X4) & ! [X3] : p1(X3)) => ? [X2] : ! [X4] : (r1(X2) | p1(X4))) & (! [X3] : p1(X3) => ! [X5, X1] : (p1(X1) & p1(X5))) & ! [X5, X1, X0] : ((! [X3, X4] : (r(X3, X4) => q(X3, X4)) & ! [X3] : (s1(X3) => p1(X3)) & r(X1, X0) & s1(X1) & s1(X5)) => ? [X3, X4] : (q(X3, X4) & p1(X3))) & ! [X5] : ? [X3, X6, X7, X8, X4] : (((s(X5, X4) => p1(X4)) & (e(X6) => (c(f(X6)) | g(X6))) & (e(X3) => (s(X3, f(X3)) | g(X3))) & e(X5) & p1(X5)) => ((c(X8) & p1(X8)) | (g(X7) & p1(X7)))) & ((~ ! [X3] : (a1(X3) => b(X3)) & ! [X3] : (a1(X3) => (c(X3) | b(X3)))) => ? [X3] : (c(X3) & a1(X3))) & ! [X1, X0] : (q1(f(X1)) => ? [X3, X4] : (q1(X3) & (p1(f(X4)) => ((r1(X4) => (r1(X0) & r1(X1))) & p1(X3))))) & ! [X1, X0] : (! [X2] : q1(f(X2)) => ? [X3, X4] : (q1(X3) & (p1(f(X4)) => ((r1(X4) => (r1(X0) & r1(X1))) & p1(X3))))) & ! [X0, X1] : (! [X2] : q1(f(X2)) => ? [X3, X4] : (q1(X3) & (r1(X4) => (r1(X0) & r1(X1))) & (p1(f(X4)) => p1(X3))))), file('/home/ubuntu/library/tptp/Problems/SYN/SYN938+1.p', prove_this)).
fof(f1128, plain, (spl103_113 | spl103_98 | spl103_91 | spl103_86 | spl103_144 | spl103_140 | spl103_29 | spl103_83 | spl103_80 | spl103_57 | spl103_133 | spl103_110 | spl103_130 | spl103_28 | spl103_54 | spl103_107 | spl103_51 | spl103_47 | spl103_25 | spl103_43 | spl103_39 | spl103_21 | spl103_18 | spl103_15 | spl103_12 | spl103_10 | spl103_124 | spl103_117 | spl103_102 | spl103_7 | spl103_76 | spl103_35 | spl103_4 | spl103_1 | ~ spl103_162 | spl103_164 | spl103_155 | spl103_32 | spl103_75 | spl103_72 | spl103_152 | spl103_65 | spl103_61), inference(avatar_split_clause, [], [f366, f640, f659, f1065, f687, f703, f512, f1079, f1121, f1111, f374, f386, f526, f711, f399, f829, f906, f936, f411, f421, f435, f449, f463, f544, f561, f480, f578, f596, f852, f610, f492, f960, f869, f978, f624, f730, f744, f498, f1009, f1031, f759, f781, f810, f886])).
fof(f366, plain, ! [X2, X1] : (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | p(X1, X2) | p1(sK102) | ~ p1(z) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(cnf_transformation, [], [f212])).
fof(f1127, plain, (spl103_113 | spl103_98 | spl103_91 | spl103_86 | spl103_144 | spl103_140 | spl103_29 | spl103_83 | spl103_80 | spl103_57 | spl103_133 | spl103_110 | spl103_130 | spl103_28 | spl103_54 | spl103_107 | spl103_51 | spl103_47 | spl103_25 | spl103_43 | spl103_39 | spl103_21 | spl103_18 | spl103_15 | spl103_12 | spl103_10 | spl103_124 | spl103_117 | spl103_102 | spl103_7 | spl103_76 | spl103_35 | spl103_4 | spl103_1 | spl103_162 | spl103_5 | spl103_155 | spl103_32 | spl103_75 | spl103_72 | spl103_152 | spl103_65 | spl103_61), inference(avatar_split_clause, [], [f367, f640, f659, f1065, f687, f703, f512, f1079, f390, f1111, f374, f386, f526, f711, f399, f829, f906, f936, f411, f421, f435, f449, f463, f544, f561, f480, f578, f596, f852, f610, f492, f960, f869, f978, f624, f730, f744, f498, f1009, f1031, f759, f781, f810, f886])).
fof(f367, plain, ! [X2, X3, X1] : (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | p(X1, X2) | ~ p1(X3) | p1(z) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(cnf_transformation, [], [f212])).
fof(f1126, plain, (spl103_113 | spl103_98 | spl103_91 | spl103_86 | spl103_144 | spl103_140 | spl103_29 | spl103_83 | spl103_80 | spl103_57 | spl103_133 | spl103_110 | spl103_130 | spl103_28 | spl103_54 | spl103_107 | spl103_51 | spl103_47 | spl103_25 | spl103_43 | spl103_39 | spl103_21 | spl103_18 | spl103_15 | spl103_12 | spl103_10 | spl103_124 | spl103_117 | spl103_102 | spl103_7 | spl103_76 | spl103_35 | spl103_4 | spl103_1 | ~ spl103_162 | spl103_5 | spl103_155 | spl103_32 | spl103_75 | spl103_72 | spl103_152 | spl103_65 | spl103_61), inference(avatar_split_clause, [], [f368, f640, f659, f1065, f687, f703, f512, f1079, f390, f1111, f374, f386, f526, f711, f399, f829, f906, f936, f411, f421, f435, f449, f463, f544, f561, f480, f578, f596, f852, f610, f492, f960, f869, f978, f624, f730, f744, f498, f1009, f1031, f759, f781, f810, f886])).
fof(f368, plain, ! [X2, X3, X1] : (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | p(X1, X2) | ~ p1(X3) | ~ p1(z) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(cnf_transformation, [], [f212])).
fof(f1125, plain, (spl103_113 | spl103_98 | spl103_91 | spl103_86 | spl103_144 | spl103_140 | spl103_29 | spl103_83 | spl103_80 | spl103_57 | spl103_133 | spl103_110 | spl103_130 | spl103_28 | spl103_54 | spl103_107 | spl103_51 | spl103_47 | spl103_25 | spl103_43 | spl103_39 | spl103_21 | spl103_18 | spl103_15 | spl103_12 | spl103_10 | spl103_124 | spl103_117 | spl103_102 | spl103_7 | spl103_76 | spl103_35 | spl103_4 | spl103_1 | spl103_162 | spl103_164 | ~ spl103_163 | spl103_32 | spl103_75 | spl103_72 | spl103_152 | spl103_65 | spl103_61), inference(avatar_split_clause, [], [f369, f640, f659, f1065, f687, f703, f512, f1115, f1121, f1111, f374, f386, f526, f711, f399, f829, f906, f936, f411, f421, f435, f449, f463, f544, f561, f480, f578, f596, f852, f610, f492, f960, f869, f978, f624, f730, f744, f498, f1009, f1031, f759, f781, f810, f886])).
fof(f369, plain, (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | ~ p(sK101, sK101) | p1(sK102) | p1(z) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(cnf_transformation, [], [f212])).
fof(f1124, plain, (spl103_113 | spl103_98 | spl103_91 | spl103_86 | spl103_144 | spl103_140 | spl103_29 | spl103_83 | spl103_80 | spl103_57 | spl103_133 | spl103_110 | spl103_130 | spl103_28 | spl103_54 | spl103_107 | spl103_51 | spl103_47 | spl103_25 | spl103_43 | spl103_39 | spl103_21 | spl103_18 | spl103_15 | spl103_12 | spl103_10 | spl103_124 | spl103_117 | spl103_102 | spl103_7 | spl103_76 | spl103_35 | spl103_4 | spl103_1 | ~ spl103_162 | spl103_164 | ~ spl103_163 | spl103_32 | spl103_75 | spl103_72 | spl103_152 | spl103_65 | spl103_61), inference(avatar_split_clause, [], [f370, f640, f659, f1065, f687, f703, f512, f1115, f1121, f1111, f374, f386, f526, f711, f399, f829, f906, f936, f411, f421, f435, f449, f463, f544, f561, f480, f578, f596, f852, f610, f492, f960, f869, f978, f624, f730, f744, f498, f1009, f1031, f759, f781, f810, f886])).
fof(f370, plain, (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | ~ p(sK101, sK101) | p1(sK102) | ~ p1(z) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(cnf_transformation, [], [f212])).
fof(f1119, plain, (spl103_113 | spl103_98 | spl103_91 | spl103_86 | spl103_144 | spl103_140 | spl103_29 | spl103_83 | spl103_80 | spl103_57 | spl103_133 | spl103_110 | spl103_130 | spl103_28 | spl103_54 | spl103_107 | spl103_51 | spl103_47 | spl103_25 | spl103_43 | spl103_39 | spl103_21 | spl103_18 | spl103_15 | spl103_12 | spl103_10 | spl103_124 | spl103_117 | spl103_102 | spl103_7 | spl103_76 | spl103_35 | spl103_4 | spl103_1 | spl103_162 | spl103_5 | ~ spl103_163 | spl103_32 | spl103_75 | spl103_72 | spl103_152 | spl103_65 | spl103_61), inference(avatar_split_clause, [], [f371, f640, f659, f1065, f687, f703, f512, f1115, f390, f1111, f374, f386, f526, f711, f399, f829, f906, f936, f411, f421, f435, f449, f463, f544, f561, f480, f578, f596, f852, f610, f492, f960, f869, f978, f624, f730, f744, f498, f1009, f1031, f759, f781, f810, f886])).
fof(f371, plain, ! [X3] : (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | ~ p(sK101, sK101) | ~ p1(X3) | p1(z) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(cnf_transformation, [], [f212])).
fof(f1118, plain, (spl103_113 | spl103_98 | spl103_91 | spl103_86 | spl103_144 | spl103_140 | spl103_29 | spl103_83 | spl103_80 | spl103_57 | spl103_133 | spl103_110 | spl103_130 | spl103_28 | spl103_54 | spl103_107 | spl103_51 | spl103_47 | spl103_25 | spl103_43 | spl103_39 | spl103_21 | spl103_18 | spl103_15 | spl103_12 | spl103_10 | spl103_124 | spl103_117 | spl103_102 | spl103_7 | spl103_76 | spl103_35 | spl103_4 | spl103_1 | ~ spl103_162 | spl103_5 | ~ spl103_163 | spl103_32 | spl103_75 | spl103_72 | spl103_152 | spl103_65 | spl103_61), inference(avatar_split_clause, [], [f372, f640, f659, f1065, f687, f703, f512, f1115, f390, f1111, f374, f386, f526, f711, f399, f829, f906, f936, f411, f421, f435, f449, f463, f544, f561, f480, f578, f596, f852, f610, f492, f960, f869, f978, f624, f730, f744, f498, f1009, f1031, f759, f781, f810, f886])).
fof(f372, plain, ! [X3] : (sP20 | sP19 | sP0 | sP18 | sP17 | sP28 | ~ p(sK101, sK101) | ~ p1(X3) | ~ p1(z) | sP39 | sP38 | sP27 | sP16 | sP37 | sP10 | sP6 | sP5 | sP36 | sP35 | sP34 | sP33 | sP32 | sP26 | sP25 | sP31 | sP24 | sP23 | sP9 | sP22 | sP30 | sP4 | sP8 | sP3 | sP21 | sP15 | sP14 | sP29 | sP2 | sP1 | sP13 | sP12 | sP11 | sP7), inference(cnf_transformation, [], [f212])).
fof(f1081, plain, (~ spl103_152 | ~ spl103_154 | spl103_155), inference(avatar_split_clause, [], [f362, f1079, f1074, f1065])).
fof(f1074, plain, (spl103_154 <=> s1(sK99)), introduced(avatar_definition, [new_symbols(naming, [spl103_154])])).
fof(f362, plain, ! [X2, X3] : (p(X2, X3) | ~ s1(sK99) | ~ sP0), inference(cnf_transformation, [], [f208])).
fof(f208, plain, (! [X2, X3] : (~ p(sK99, sK100) & s1(sK99) & (p(X2, X3) | ~ s1(sK99)) & r1(sK100) & r1(sK99) & (p(sK100, X3) | ~ r1(X3)) & q1(sK100) & q1(sK99) & (p(X2, sK99) | ~ q1(X2))) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK99, sK100])], [f206, f207])).
fof(f207, plain, (? [X0, X1] : ! [X2, X3] : (~ p(X0, X1) & s1(X0) & (p(X2, X3) | ~ s1(X0)) & r1(X1) & r1(X0) & (p(X1, X3) | ~ r1(X3)) & q1(X1) & q1(X0) & (p(X2, X0) | ~ q1(X2))) => ! [X3, X2] : (~ p(sK99, sK100) & s1(sK99) & (p(X2, X3) | ~ s1(sK99)) & r1(sK100) & r1(sK99) & (p(sK100, X3) | ~ r1(X3)) & q1(sK100) & q1(sK99) & (p(X2, sK99) | ~ q1(X2)))), introduced(choice_axiom, [])).
fof(f206, plain, (? [X0, X1] : ! [X2, X3] : (~ p(X0, X1) & s1(X0) & (p(X2, X3) | ~ s1(X0)) & r1(X1) & r1(X0) & (p(X1, X3) | ~ r1(X3)) & q1(X1) & q1(X0) & (p(X2, X0) | ~ q1(X2))) | ~ sP0), inference(rectify, [], [f205])).
fof(f205, plain, (? [X7, X8] : ! [X9, X10] : (~ p(X7, X8) & s1(X7) & (p(X9, X10) | ~ s1(X7)) & r1(X8) & r1(X7) & (p(X8, X10) | ~ r1(X10)) & q1(X8) & q1(X7) & (p(X9, X7) | ~ q1(X9))) | ~ sP0), inference(nnf_transformation, [], [f7])).
fof(f1077, plain, (~ spl103_152 | spl103_154), inference(avatar_split_clause, [], [f363, f1074, f1065])).
fof(f363, plain, (s1(sK99) | ~ sP0), inference(cnf_transformation, [], [f208])).
fof(f1072, plain, (~ spl103_152 | ~ spl103_153), inference(avatar_split_clause, [], [f364, f1069, f1065])).
fof(f364, plain, (~ p(sK99, sK100) | ~ sP0), inference(cnf_transformation, [], [f208])).
fof(f1063, plain, (~ spl103_144 | spl103_151), inference(avatar_split_clause, [], [f349, f1060, f1031])).
fof(f349, plain, (p1(sK98) | ~ sP1), inference(cnf_transformation, [], [f204])).
fof(f204, plain, (! [X1, X2, X3, X4, X5] : ((~ c(X4) | ~ p1(X4)) & (~ g(X3) | ~ p1(X3)) & (p1(X5) | ~ s(sK98, X5)) & (c(f(X2)) | g(X2) | ~ e(X2)) & (s(X1, f(X1)) | g(X1) | ~ e(X1)) & e(sK98) & p1(sK98)) | ~ sP1), inference(skolemisation, [status(esa), new_symbols(skolem, [sK98])], [f202, f203])).
fof(f203, plain, (? [X0] : ! [X1, X2, X3, X4, X5] : ((~ c(X4) | ~ p1(X4)) & (~ g(X3) | ~ p1(X3)) & (p1(X5) | ~ s(X0, X5)) & (c(f(X2)) | g(X2) | ~ e(X2)) & (s(X1, f(X1)) | g(X1) | ~ e(X1)) & e(X0) & p1(X0)) => ! [X5, X4, X3, X2, X1] : ((~ c(X4) | ~ p1(X4)) & (~ g(X3) | ~ p1(X3)) & (p1(X5) | ~ s(sK98, X5)) & (c(f(X2)) | g(X2) | ~ e(X2)) & (s(X1, f(X1)) | g(X1) | ~ e(X1)) & e(sK98) & p1(sK98))), introduced(choice_axiom, [])).
fof(f202, plain, (? [X0] : ! [X1, X2, X3, X4, X5] : ((~ c(X4) | ~ p1(X4)) & (~ g(X3) | ~ p1(X3)) & (p1(X5) | ~ s(X0, X5)) & (c(f(X2)) | g(X2) | ~ e(X2)) & (s(X1, f(X1)) | g(X1) | ~ e(X1)) & e(X0) & p1(X0)) | ~ sP1), inference(rectify, [], [f201])).
fof(f201, plain, (? [X118] : ! [X119, X120, X121, X122, X123] : ((~ c(X122) | ~ p1(X122)) & (~ g(X121) | ~ p1(X121)) & (p1(X123) | ~ s(X118, X123)) & (c(f(X120)) | g(X120) | ~ e(X120)) & (s(X119, f(X119)) | g(X119) | ~ e(X119)) & e(X118) & p1(X118)) | ~ sP1), inference(nnf_transformation, [], [f8])).
fof(f1058, plain, (~ spl103_144 | spl103_150), inference(avatar_split_clause, [], [f350, f1055, f1031])).
fof(f350, plain, (e(sK98) | ~ sP1), inference(cnf_transformation, [], [f204])).
fof(f1053, plain, (~ spl103_144 | spl103_149), inference(avatar_split_clause, [], [f351, f1051, f1031])).
fof(f351, plain, ! [X1] : (s(X1, f(X1)) | g(X1) | ~ e(X1) | ~ sP1), inference(cnf_transformation, [], [f204])).
fof(f1049, plain, (~ spl103_144 | spl103_148), inference(avatar_split_clause, [], [f352, f1047, f1031])).
fof(f352, plain, ! [X2] : (c(f(X2)) | g(X2) | ~ e(X2) | ~ sP1), inference(cnf_transformation, [], [f204])).
fof(f1045, plain, (~ spl103_144 | spl103_147), inference(avatar_split_clause, [], [f353, f1043, f1031])).
fof(f353, plain, ! [X5] : (p1(X5) | ~ s(sK98, X5) | ~ sP1), inference(cnf_transformation, [], [f204])).
fof(f1041, plain, (~ spl103_144 | spl103_146), inference(avatar_split_clause, [], [f354, f1039, f1031])).
fof(f354, plain, ! [X3] : (~ g(X3) | ~ p1(X3) | ~ sP1), inference(cnf_transformation, [], [f204])).
fof(f1037, plain, (~ spl103_144 | spl103_145), inference(avatar_split_clause, [], [f355, f1035, f1031])).
fof(f355, plain, ! [X4] : (~ c(X4) | ~ p1(X4) | ~ sP1), inference(cnf_transformation, [], [f204])).
fof(f1024, plain, (~ spl103_140 | spl103_142), inference(avatar_split_clause, [], [f344, f1021, f1009])).
fof(f344, plain, (s1(sK96) | ~ sP2), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ((! [X3, X4] : (~ q(X3, X4) | ~ p1(X3)) & ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6)) & ! [X7] : (p1(X7) | ~ s1(X7)) & r(sK96, sK97) & s1(sK96) & s1(sK95)) | ~ sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK95, sK96, sK97])], [f198, f199])).
fof(f199, plain, (? [X0, X1, X2] : (! [X3, X4] : (~ q(X3, X4) | ~ p1(X3)) & ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6)) & ! [X7] : (p1(X7) | ~ s1(X7)) & r(X1, X2) & s1(X1) & s1(X0)) => (! [X3, X4] : (~ q(X3, X4) | ~ p1(X3)) & ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6)) & ! [X7] : (p1(X7) | ~ s1(X7)) & r(sK96, sK97) & s1(sK96) & s1(sK95))), introduced(choice_axiom, [])).
fof(f198, plain, (? [X0, X1, X2] : (! [X3, X4] : (~ q(X3, X4) | ~ p1(X3)) & ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6)) & ! [X7] : (p1(X7) | ~ s1(X7)) & r(X1, X2) & s1(X1) & s1(X0)) | ~ sP2), inference(rectify, [], [f197])).
fof(f197, plain, (? [X110, X111, X112] : (! [X116, X117] : (~ q(X116, X117) | ~ p1(X116)) & ! [X113, X114] : (q(X113, X114) | ~ r(X113, X114)) & ! [X115] : (p1(X115) | ~ s1(X115)) & r(X111, X112) & s1(X111) & s1(X110)) | ~ sP2), inference(nnf_transformation, [], [f9])).
fof(f1019, plain, (~ spl103_140 | spl103_141), inference(avatar_split_clause, [], [f345, f1016, f1009])).
fof(f345, plain, (r(sK96, sK97) | ~ sP2), inference(cnf_transformation, [], [f200])).
fof(f1014, plain, (~ spl103_140 | spl103_136), inference(avatar_split_clause, [], [f346, f990, f1009])).
fof(f346, plain, ! [X7] : (p1(X7) | ~ s1(X7) | ~ sP2), inference(cnf_transformation, [], [f200])).
fof(f1013, plain, (~ spl103_140 | spl103_135), inference(avatar_split_clause, [], [f347, f986, f1009])).
fof(f347, plain, ! [X6, X5] : (q(X5, X6) | ~ r(X5, X6) | ~ sP2), inference(cnf_transformation, [], [f200])).
fof(f1012, plain, (~ spl103_140 | spl103_134), inference(avatar_split_clause, [], [f348, f982, f1009])).
fof(f348, plain, ! [X4, X3] : (~ q(X3, X4) | ~ p1(X3) | ~ sP2), inference(cnf_transformation, [], [f200])).
fof(f1002, plain, (~ spl103_133 | spl103_138), inference(avatar_split_clause, [], [f338, f999, f978])).
fof(f338, plain, (s1(sK93) | ~ sP3), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ((! [X3, X4] : (~ q(X3, X4) | ~ p1(X3)) & ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6)) & ! [X7] : (p1(X7) | ~ s1(X7)) & r(sK93, sK94) & s1(sK93) & s1(sK92)) | ~ sP3), inference(skolemisation, [status(esa), new_symbols(skolem, [sK92, sK93, sK94])], [f194, f195])).
fof(f195, plain, (? [X0, X1, X2] : (! [X3, X4] : (~ q(X3, X4) | ~ p1(X3)) & ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6)) & ! [X7] : (p1(X7) | ~ s1(X7)) & r(X1, X2) & s1(X1) & s1(X0)) => (! [X3, X4] : (~ q(X3, X4) | ~ p1(X3)) & ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6)) & ! [X7] : (p1(X7) | ~ s1(X7)) & r(sK93, sK94) & s1(sK93) & s1(sK92))), introduced(choice_axiom, [])).
fof(f194, plain, (? [X0, X1, X2] : (! [X3, X4] : (~ q(X3, X4) | ~ p1(X3)) & ! [X5, X6] : (q(X5, X6) | ~ r(X5, X6)) & ! [X7] : (p1(X7) | ~ s1(X7)) & r(X1, X2) & s1(X1) & s1(X0)) | ~ sP3), inference(rectify, [], [f193])).
fof(f193, plain, (? [X88, X89, X90] : (! [X94, X95] : (~ q(X94, X95) | ~ p1(X94)) & ! [X91, X92] : (q(X91, X92) | ~ r(X91, X92)) & ! [X93] : (p1(X93) | ~ s1(X93)) & r(X89, X90) & s1(X89) & s1(X88)) | ~ sP3), inference(nnf_transformation, [], [f10])).
fof(f997, plain, (~ spl103_133 | spl103_137), inference(avatar_split_clause, [], [f339, f994, f978])).
fof(f339, plain, (r(sK93, sK94) | ~ sP3), inference(cnf_transformation, [], [f196])).
fof(f992, plain, (~ spl103_133 | spl103_136), inference(avatar_split_clause, [], [f340, f990, f978])).
fof(f340, plain, ! [X7] : (p1(X7) | ~ s1(X7) | ~ sP3), inference(cnf_transformation, [], [f196])).
fof(f988, plain, (~ spl103_133 | spl103_135), inference(avatar_split_clause, [], [f341, f986, f978])).
fof(f341, plain, ! [X6, X5] : (q(X5, X6) | ~ r(X5, X6) | ~ sP3), inference(cnf_transformation, [], [f196])).
fof(f984, plain, (~ spl103_133 | spl103_134), inference(avatar_split_clause, [], [f342, f982, f978])).
fof(f342, plain, ! [X4, X3] : (~ q(X3, X4) | ~ p1(X3) | ~ sP3), inference(cnf_transformation, [], [f196])).
fof(f976, plain, (~ spl103_130 | spl103_132), inference(avatar_split_clause, [], [f331, f973, f960])).
fof(f331, plain, (p1(sK91) | ~ sP4), inference(cnf_transformation, [], [f192])).
fof(f192, plain, ((((((~ q0 & q0) | (b0 & ~ b0)) & a0) | ! [X0] : ~ p1(X0)) & p1(sK91)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK91])], [f190, f191])).
fof(f191, plain, (? [X1] : p1(X1) => p1(sK91)), introduced(choice_axiom, [])).
fof(f190, plain, ((((((~ q0 & q0) | (b0 & ~ b0)) & a0) | ! [X0] : ~ p1(X0)) & ? [X1] : p1(X1)) | ~ sP4), inference(rectify, [], [f189])).
fof(f189, plain, ((((((~ q0 & q0) | (b0 & ~ b0)) & a0) | ! [X83] : ~ p1(X83)) & ? [X82] : p1(X82)) | ~ sP4), inference(nnf_transformation, [], [f11])).
fof(f970, plain, (~ spl103_130 | spl103_5 | ~ spl103_74 | spl103_131), inference(avatar_split_clause, [], [f333, f964, f695, f390, f960])).
fof(f695, plain, (spl103_74 <=> b0), introduced(avatar_definition, [new_symbols(naming, [spl103_74])])).
fof(f964, plain, (spl103_131 <=> q0), introduced(avatar_definition, [new_symbols(naming, [spl103_131])])).
fof(f333, plain, ! [X0] : (q0 | ~ b0 | ~ p1(X0) | ~ sP4), inference(cnf_transformation, [], [f192])).
fof(f969, plain, (~ spl103_130 | spl103_5 | spl103_74 | spl103_131), inference(avatar_split_clause, [], [f334, f964, f695, f390, f960])).
fof(f334, plain, ! [X0] : (q0 | b0 | ~ p1(X0) | ~ sP4), inference(cnf_transformation, [], [f192])).
fof(f968, plain, (~ spl103_130 | spl103_5 | ~ spl103_74 | ~ spl103_131), inference(avatar_split_clause, [], [f335, f964, f695, f390, f960])).
fof(f335, plain, ! [X0] : (~ q0 | ~ b0 | ~ p1(X0) | ~ sP4), inference(cnf_transformation, [], [f192])).
fof(f967, plain, (~ spl103_130 | spl103_5 | spl103_74 | ~ spl103_131), inference(avatar_split_clause, [], [f336, f964, f695, f390, f960])).
fof(f336, plain, ! [X0] : (~ q0 | b0 | ~ p1(X0) | ~ sP4), inference(cnf_transformation, [], [f192])).
fof(f958, plain, (~ spl103_124 | spl103_129), inference(avatar_split_clause, [], [f325, f956, f936])).
fof(f325, plain, ! [X2, X0] : (p(X0, X2) | p(X2, sK90(X0)) | ~ sP5), inference(cnf_transformation, [], [f188])).
fof(f188, plain, (! [X0] : ! [X2] : ((~ p(X2, sK90(X0)) & p(X2, X0) & p(X0, X2)) | (! [X3] : ~ p(X3, X2) & p(X2, sK90(X0)))) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK90])], [f186, f187])).
fof(f187, plain, ! [X0] : (? [X1] : ! [X2] : ((~ p(X2, X1) & p(X2, X0) & p(X0, X2)) | (! [X3] : ~ p(X3, X2) & p(X2, X1))) => ! [X2] : ((~ p(X2, sK90(X0)) & p(X2, X0) & p(X0, X2)) | (! [X3] : ~ p(X3, X2) & p(X2, sK90(X0))))), introduced(choice_axiom, [])).
fof(f186, plain, (! [X0] : ? [X1] : ! [X2] : ((~ p(X2, X1) & p(X2, X0) & p(X0, X2)) | (! [X3] : ~ p(X3, X2) & p(X2, X1))) | ~ sP5), inference(rectify, [], [f185])).
fof(f185, plain, (! [X42] : ? [X43] : ! [X44] : ((~ p(X44, X43) & p(X44, X42) & p(X42, X44)) | (! [X45] : ~ p(X45, X44) & p(X44, X43))) | ~ sP5), inference(nnf_transformation, [], [f12])).
fof(f946, plain, (~ spl103_124 | spl103_126), inference(avatar_split_clause, [], [f328, f944, f936])).
fof(f328, plain, ! [X2, X0, X3] : (p(X2, X0) | ~ p(X3, X2) | ~ sP5), inference(cnf_transformation, [], [f188])).
fof(f942, plain, (~ spl103_124 | spl103_125), inference(avatar_split_clause, [], [f330, f940, f936])).
fof(f330, plain, ! [X2, X0, X3] : (~ p(X2, sK90(X0)) | ~ p(X3, X2) | ~ sP5), inference(cnf_transformation, [], [f188])).
fof(f934, plain, (~ spl103_117 | spl103_123), inference(avatar_split_clause, [], [f319, f932, f906])).
fof(f319, plain, ! [X2, X5, X3] : (a_member_of(X5, X3) | ~ a_member_of(X5, X2) | ~ eq(X2, X3) | ~ sP6), inference(cnf_transformation, [], [f184])).
fof(f184, plain, (((~ eq(sK88, sK87) & eq(sK87, sK88)) & ! [X2, X3] : ((eq(X2, X3) | ((~ a_member_of(sK89(X2, X3), X3) | ~ a_member_of(sK89(X2, X3), X2)) & (a_member_of(sK89(X2, X3), X3) | a_member_of(sK89(X2, X3), X2)))) & (! [X5] : ((a_member_of(X5, X2) | ~ a_member_of(X5, X3)) & (a_member_of(X5, X3) | ~ a_member_of(X5, X2))) | ~ eq(X2, X3)))) | ~ sP6), inference(skolemisation, [status(esa), new_symbols(skolem, [sK87, sK88, sK89])], [f181, f183, f182])).
fof(f182, plain, (? [X0, X1] : (~ eq(X1, X0) & eq(X0, X1)) => (~ eq(sK88, sK87) & eq(sK87, sK88))), introduced(choice_axiom, [])).
fof(f183, plain, ! [X3, X2] : (? [X4] : ((~ a_member_of(X4, X3) | ~ a_member_of(X4, X2)) & (a_member_of(X4, X3) | a_member_of(X4, X2))) => ((~ a_member_of(sK89(X2, X3), X3) | ~ a_member_of(sK89(X2, X3), X2)) & (a_member_of(sK89(X2, X3), X3) | a_member_of(sK89(X2, X3), X2)))), introduced(choice_axiom, [])).
fof(f181, plain, ((? [X0, X1] : (~ eq(X1, X0) & eq(X0, X1)) & ! [X2, X3] : ((eq(X2, X3) | ? [X4] : ((~ a_member_of(X4, X3) | ~ a_member_of(X4, X2)) & (a_member_of(X4, X3) | a_member_of(X4, X2)))) & (! [X5] : ((a_member_of(X5, X2) | ~ a_member_of(X5, X3)) & (a_member_of(X5, X3) | ~ a_member_of(X5, X2))) | ~ eq(X2, X3)))) | ~ sP6), inference(rectify, [], [f180])).
fof(f180, plain, ((? [X40, X41] : (~ eq(X41, X40) & eq(X40, X41)) & ! [X37, X38] : ((eq(X37, X38) | ? [X39] : ((~ a_member_of(X39, X38) | ~ a_member_of(X39, X37)) & (a_member_of(X39, X38) | a_member_of(X39, X37)))) & (! [X39] : ((a_member_of(X39, X37) | ~ a_member_of(X39, X38)) & (a_member_of(X39, X38) | ~ a_member_of(X39, X37))) | ~ eq(X37, X38)))) | ~ sP6), inference(nnf_transformation, [], [f13])).
fof(f930, plain, (~ spl103_117 | spl103_122), inference(avatar_split_clause, [], [f320, f928, f906])).
fof(f320, plain, ! [X2, X5, X3] : (a_member_of(X5, X2) | ~ a_member_of(X5, X3) | ~ eq(X2, X3) | ~ sP6), inference(cnf_transformation, [], [f184])).
fof(f926, plain, (~ spl103_117 | spl103_121), inference(avatar_split_clause, [], [f321, f924, f906])).
fof(f321, plain, ! [X2, X3] : (eq(X2, X3) | a_member_of(sK89(X2, X3), X3) | a_member_of(sK89(X2, X3), X2) | ~ sP6), inference(cnf_transformation, [], [f184])).
fof(f922, plain, (~ spl103_117 | spl103_120), inference(avatar_split_clause, [], [f322, f920, f906])).
fof(f322, plain, ! [X2, X3] : (eq(X2, X3) | ~ a_member_of(sK89(X2, X3), X3) | ~ a_member_of(sK89(X2, X3), X2) | ~ sP6), inference(cnf_transformation, [], [f184])).
fof(f918, plain, (~ spl103_117 | spl103_119), inference(avatar_split_clause, [], [f323, f915, f906])).
fof(f323, plain, (eq(sK87, sK88) | ~ sP6), inference(cnf_transformation, [], [f184])).
fof(f913, plain, (~ spl103_117 | ~ spl103_118), inference(avatar_split_clause, [], [f324, f910, f906])).
fof(f324, plain, (~ eq(sK88, sK87) | ~ sP6), inference(cnf_transformation, [], [f184])).
fof(f904, plain, (~ spl103_113 | spl103_101), inference(avatar_split_clause, [], [f314, f825, f886])).
fof(f314, plain, ! [X4] : (q1(f(X4)) | ~ sP7), inference(cnf_transformation, [], [f179])).
fof(f179, plain, ((! [X2, X3] : (~ q1(X2) | ((~ r1(sK85) | ~ r1(sK86)) & r1(X3)) | (~ p1(X2) & p1(f(X3)))) & ! [X4] : q1(f(X4))) | ~ sP7), inference(skolemisation, [status(esa), new_symbols(skolem, [sK85, sK86])], [f177, f178])).
fof(f178, plain, (? [X0, X1] : (! [X2, X3] : (~ q1(X2) | ((~ r1(X0) | ~ r1(X1)) & r1(X3)) | (~ p1(X2) & p1(f(X3)))) & ! [X4] : q1(f(X4))) => (! [X3, X2] : (~ q1(X2) | ((~ r1(sK85) | ~ r1(sK86)) & r1(X3)) | (~ p1(X2) & p1(f(X3)))) & ! [X4] : q1(f(X4)))), introduced(choice_axiom, [])).
fof(f177, plain, (? [X0, X1] : (! [X2, X3] : (~ q1(X2) | ((~ r1(X0) | ~ r1(X1)) & r1(X3)) | (~ p1(X2) & p1(f(X3)))) & ! [X4] : q1(f(X4))) | ~ sP7), inference(rectify, [], [f176])).
fof(f176, plain, (? [X136, X137] : (! [X139, X140] : (~ q1(X139) | ((~ r1(X136) | ~ r1(X137)) & r1(X140)) | (~ p1(X139) & p1(f(X140)))) & ! [X138] : q1(f(X138))) | ~ sP7), inference(nnf_transformation, [], [f14])).
fof(f903, plain, (~ spl103_113 | spl103_116 | spl103_96), inference(avatar_split_clause, [], [f315, f801, f901, f886])).
fof(f315, plain, ! [X2, X3] : (~ q1(X2) | r1(X3) | p1(f(X3)) | ~ sP7), inference(cnf_transformation, [], [f179])).
fof(f899, plain, (~ spl103_113 | spl103_38 | spl103_94), inference(avatar_split_clause, [], [f316, f793, f540, f886])).
fof(f316, plain, ! [X2, X3] : (~ q1(X2) | r1(X3) | ~ p1(X2) | ~ sP7), inference(cnf_transformation, [], [f179])).
fof(f898, plain, (~ spl103_113 | spl103_95 | ~ spl103_114 | ~ spl103_115 | spl103_96), inference(avatar_split_clause, [], [f317, f801, f894, f890, f798, f886])).
fof(f317, plain, ! [X2, X3] : (~ q1(X2) | ~ r1(sK85) | ~ r1(sK86) | p1(f(X3)) | ~ sP7), inference(cnf_transformation, [], [f179])).
fof(f897, plain, (~ spl103_113 | ~ spl103_114 | ~ spl103_115 | spl103_94), inference(avatar_split_clause, [], [f318, f793, f894, f890, f886])).
fof(f318, plain, ! [X2] : (~ q1(X2) | ~ r1(sK85) | ~ r1(sK86) | ~ p1(X2) | ~ sP7), inference(cnf_transformation, [], [f179])).
fof(f884, plain, (~ spl103_110 | spl103_106), inference(avatar_split_clause, [], [f309, f848, f869])).
fof(f309, plain, ! [X3] : (p1(X3) | ~ q1(X3) | ~ sP8), inference(cnf_transformation, [], [f175])).
fof(f175, plain, ((! [X2] : ((~ p1(sK84) & q1(X2)) | (~ p1(sK83) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) | ~ sP8), inference(skolemisation, [status(esa), new_symbols(skolem, [sK83, sK84])], [f173, f174])).
fof(f174, plain, (? [X0, X1] : (! [X2] : ((~ p1(X1) & q1(X2)) | (~ p1(X0) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) => (! [X2] : ((~ p1(sK84) & q1(X2)) | (~ p1(sK83) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3)))), introduced(choice_axiom, [])).
fof(f173, plain, (? [X0, X1] : (! [X2] : ((~ p1(X1) & q1(X2)) | (~ p1(X0) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) | ~ sP8), inference(rectify, [], [f172])).
fof(f172, plain, (? [X84, X85] : (! [X87] : ((~ p1(X85) & q1(X87)) | (~ p1(X84) & p1(X87))) & ! [X86] : (p1(X86) | ~ q1(X86))) | ~ sP8), inference(nnf_transformation, [], [f15])).
fof(f883, plain, (~ spl103_110 | spl103_105), inference(avatar_split_clause, [], [f310, f844, f869])).
fof(f310, plain, ! [X2] : (q1(X2) | p1(X2) | ~ sP8), inference(cnf_transformation, [], [f175])).
fof(f880, plain, (~ spl103_110 | ~ spl103_111 | ~ spl103_112), inference(avatar_split_clause, [], [f313, f877, f873, f869])).
fof(f313, plain, (~ p1(sK84) | ~ p1(sK83) | ~ sP8), inference(cnf_transformation, [], [f175])).
fof(f867, plain, (~ spl103_107 | spl103_106), inference(avatar_split_clause, [], [f304, f848, f852])).
fof(f304, plain, ! [X3] : (p1(X3) | ~ q1(X3) | ~ sP9), inference(cnf_transformation, [], [f171])).
fof(f171, plain, ((! [X2] : ((~ p1(sK82) & q1(X2)) | (~ p1(sK81) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) | ~ sP9), inference(skolemisation, [status(esa), new_symbols(skolem, [sK81, sK82])], [f169, f170])).
fof(f170, plain, (? [X0, X1] : (! [X2] : ((~ p1(X1) & q1(X2)) | (~ p1(X0) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) => (! [X2] : ((~ p1(sK82) & q1(X2)) | (~ p1(sK81) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3)))), introduced(choice_axiom, [])).
fof(f169, plain, (? [X0, X1] : (! [X2] : ((~ p1(X1) & q1(X2)) | (~ p1(X0) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) | ~ sP9), inference(rectify, [], [f168])).
fof(f168, plain, (? [X73, X74] : (! [X76] : ((~ p1(X74) & q1(X76)) | (~ p1(X73) & p1(X76))) & ! [X75] : (p1(X75) | ~ q1(X75))) | ~ sP9), inference(nnf_transformation, [], [f16])).
fof(f866, plain, (~ spl103_107 | spl103_105), inference(avatar_split_clause, [], [f305, f844, f852])).
fof(f305, plain, ! [X2] : (q1(X2) | p1(X2) | ~ sP9), inference(cnf_transformation, [], [f171])).
fof(f863, plain, (~ spl103_107 | ~ spl103_108 | ~ spl103_109), inference(avatar_split_clause, [], [f308, f860, f856, f852])).
fof(f308, plain, (~ p1(sK82) | ~ p1(sK81) | ~ sP9), inference(cnf_transformation, [], [f171])).
fof(f850, plain, (~ spl103_102 | spl103_106), inference(avatar_split_clause, [], [f299, f848, f829])).
fof(f299, plain, ! [X3] : (p1(X3) | ~ q1(X3) | ~ sP10), inference(cnf_transformation, [], [f167])).
fof(f167, plain, ((! [X2] : ((~ p1(sK80) & q1(X2)) | (~ p1(sK79) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) | ~ sP10), inference(skolemisation, [status(esa), new_symbols(skolem, [sK79, sK80])], [f165, f166])).
fof(f166, plain, (? [X0, X1] : (! [X2] : ((~ p1(X1) & q1(X2)) | (~ p1(X0) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) => (! [X2] : ((~ p1(sK80) & q1(X2)) | (~ p1(sK79) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3)))), introduced(choice_axiom, [])).
fof(f165, plain, (? [X0, X1] : (! [X2] : ((~ p1(X1) & q1(X2)) | (~ p1(X0) & p1(X2))) & ! [X3] : (p1(X3) | ~ q1(X3))) | ~ sP10), inference(rectify, [], [f164])).
fof(f164, plain, (? [X33, X34] : (! [X36] : ((~ p1(X34) & q1(X36)) | (~ p1(X33) & p1(X36))) & ! [X35] : (p1(X35) | ~ q1(X35))) | ~ sP10), inference(nnf_transformation, [], [f17])).
fof(f846, plain, (~ spl103_102 | spl103_105), inference(avatar_split_clause, [], [f300, f844, f829])).
fof(f300, plain, ! [X2] : (q1(X2) | p1(X2) | ~ sP10), inference(cnf_transformation, [], [f167])).
fof(f840, plain, (~ spl103_102 | ~ spl103_103 | ~ spl103_104), inference(avatar_split_clause, [], [f303, f837, f833, f829])).
fof(f303, plain, (~ p1(sK80) | ~ p1(sK79) | ~ sP10), inference(cnf_transformation, [], [f167])).
fof(f827, plain, (~ spl103_98 | spl103_101), inference(avatar_split_clause, [], [f295, f825, f810])).
fof(f295, plain, ! [X4] : (q1(f(X4)) | ~ sP11), inference(cnf_transformation, [], [f163])).
fof(f163, plain, ((! [X2, X3] : (~ q1(X2) | ((((~ r1(sK78) | ~ r1(sK77)) & r1(X3)) | ~ p1(X2)) & p1(f(X3)))) & ! [X4] : q1(f(X4))) | ~ sP11), inference(skolemisation, [status(esa), new_symbols(skolem, [sK77, sK78])], [f161, f162])).
fof(f162, plain, (? [X0, X1] : (! [X2, X3] : (~ q1(X2) | ((((~ r1(X1) | ~ r1(X0)) & r1(X3)) | ~ p1(X2)) & p1(f(X3)))) & ! [X4] : q1(f(X4))) => (! [X3, X2] : (~ q1(X2) | ((((~ r1(sK78) | ~ r1(sK77)) & r1(X3)) | ~ p1(X2)) & p1(f(X3)))) & ! [X4] : q1(f(X4)))), introduced(choice_axiom, [])).
fof(f161, plain, (? [X0, X1] : (! [X2, X3] : (~ q1(X2) | ((((~ r1(X1) | ~ r1(X0)) & r1(X3)) | ~ p1(X2)) & p1(f(X3)))) & ! [X4] : q1(f(X4))) | ~ sP11), inference(rectify, [], [f160])).
fof(f160, plain, (? [X131, X132] : (! [X134, X135] : (~ q1(X134) | ((((~ r1(X132) | ~ r1(X131)) & r1(X135)) | ~ p1(X134)) & p1(f(X135)))) & ! [X133] : q1(f(X133))) | ~ sP11), inference(nnf_transformation, [], [f18])).
fof(f823, plain, (~ spl103_98 | spl103_95 | spl103_96), inference(avatar_split_clause, [], [f296, f801, f798, f810])).
fof(f296, plain, ! [X2, X3] : (~ q1(X2) | p1(f(X3)) | ~ sP11), inference(cnf_transformation, [], [f163])).
fof(f822, plain, (~ spl103_98 | spl103_38 | spl103_94), inference(avatar_split_clause, [], [f297, f793, f540, f810])).
fof(f297, plain, ! [X2, X3] : (~ q1(X2) | r1(X3) | ~ p1(X2) | ~ sP11), inference(cnf_transformation, [], [f163])).
fof(f821, plain, (~ spl103_98 | ~ spl103_99 | ~ spl103_100 | spl103_94), inference(avatar_split_clause, [], [f298, f793, f818, f814, f810])).
fof(f298, plain, ! [X2] : (~ q1(X2) | ~ r1(sK78) | ~ r1(sK77) | ~ p1(X2) | ~ sP11), inference(cnf_transformation, [], [f163])).
fof(f808, plain, (~ spl103_91 | spl103_97), inference(avatar_split_clause, [], [f291, f805, f781])).
fof(f291, plain, (q1(f(sK75)) | ~ sP12), inference(cnf_transformation, [], [f159])).
fof(f159, plain, ((! [X2, X3] : (~ q1(X2) | ((((~ r1(sK76) | ~ r1(sK75)) & r1(X3)) | ~ p1(X2)) & p1(f(X3)))) & q1(f(sK75))) | ~ sP12), inference(skolemisation, [status(esa), new_symbols(skolem, [sK75, sK76])], [f157, f158])).
fof(f158, plain, (? [X0, X1] : (! [X2, X3] : (~ q1(X2) | ((((~ r1(X1) | ~ r1(X0)) & r1(X3)) | ~ p1(X2)) & p1(f(X3)))) & q1(f(X0))) => (! [X3, X2] : (~ q1(X2) | ((((~ r1(sK76) | ~ r1(sK75)) & r1(X3)) | ~ p1(X2)) & p1(f(X3)))) & q1(f(sK75)))), introduced(choice_axiom, [])).
fof(f157, plain, (? [X0, X1] : (! [X2, X3] : (~ q1(X2) | ((((~ r1(X1) | ~ r1(X0)) & r1(X3)) | ~ p1(X2)) & p1(f(X3)))) & q1(f(X0))) | ~ sP12), inference(rectify, [], [f156])).
fof(f156, plain, (? [X127, X128] : (! [X129, X130] : (~ q1(X129) | ((((~ r1(X128) | ~ r1(X127)) & r1(X130)) | ~ p1(X129)) & p1(f(X130)))) & q1(f(X127))) | ~ sP12), inference(nnf_transformation, [], [f19])).
fof(f803, plain, (~ spl103_91 | spl103_95 | spl103_96), inference(avatar_split_clause, [], [f292, f801, f798, f781])).
fof(f292, plain, ! [X2, X3] : (~ q1(X2) | p1(f(X3)) | ~ sP12), inference(cnf_transformation, [], [f159])).
fof(f796, plain, (~ spl103_91 | spl103_38 | spl103_94), inference(avatar_split_clause, [], [f293, f793, f540, f781])).
fof(f293, plain, ! [X2, X3] : (~ q1(X2) | r1(X3) | ~ p1(X2) | ~ sP12), inference(cnf_transformation, [], [f159])).
fof(f795, plain, (~ spl103_91 | ~ spl103_92 | ~ spl103_93 | spl103_94), inference(avatar_split_clause, [], [f294, f793, f789, f785, f781])).
fof(f294, plain, ! [X2] : (~ q1(X2) | ~ r1(sK76) | ~ r1(sK75) | ~ p1(X2) | ~ sP12), inference(cnf_transformation, [], [f159])).
fof(f779, plain, (~ spl103_86 | spl103_90), inference(avatar_split_clause, [], [f287, f777, f759])).
fof(f287, plain, ! [X2] : (c(X2) | b(X2) | ~ a1(X2) | ~ sP13), inference(cnf_transformation, [], [f155])).
fof(f155, plain, ((! [X0] : (~ c(X0) | ~ a1(X0)) & (~ b(sK74) & a1(sK74)) & ! [X2] : (c(X2) | b(X2) | ~ a1(X2))) | ~ sP13), inference(skolemisation, [status(esa), new_symbols(skolem, [sK74])], [f153, f154])).
fof(f154, plain, (? [X1] : (~ b(X1) & a1(X1)) => (~ b(sK74) & a1(sK74))), introduced(choice_axiom, [])).
fof(f153, plain, ((! [X0] : (~ c(X0) | ~ a1(X0)) & ? [X1] : (~ b(X1) & a1(X1)) & ! [X2] : (c(X2) | b(X2) | ~ a1(X2))) | ~ sP13), inference(rectify, [], [f152])).
fof(f152, plain, ((! [X126] : (~ c(X126) | ~ a1(X126)) & ? [X124] : (~ b(X124) & a1(X124)) & ! [X125] : (c(X125) | b(X125) | ~ a1(X125))) | ~ sP13), inference(nnf_transformation, [], [f20])).
fof(f775, plain, (~ spl103_86 | spl103_89), inference(avatar_split_clause, [], [f288, f772, f759])).
fof(f288, plain, (a1(sK74) | ~ sP13), inference(cnf_transformation, [], [f155])).
fof(f770, plain, (~ spl103_86 | ~ spl103_88), inference(avatar_split_clause, [], [f289, f767, f759])).
fof(f289, plain, (~ b(sK74) | ~ sP13), inference(cnf_transformation, [], [f155])).
fof(f765, plain, (~ spl103_86 | spl103_87), inference(avatar_split_clause, [], [f290, f763, f759])).
fof(f290, plain, ! [X0] : (~ c(X0) | ~ a1(X0) | ~ sP13), inference(cnf_transformation, [], [f155])).
fof(f757, plain, (~ spl103_83 | spl103_9), inference(avatar_split_clause, [], [f283, f407, f744])).
fof(f283, plain, ! [X3] : (p1(X3) | ~ sP14), inference(cnf_transformation, [], [f151])).
fof(f151, plain, ((! [X0] : (~ r1(X0) & ~ p1(sK72(X0))) & q1(sK73) & ! [X3] : p1(X3)) | ~ sP14), inference(skolemisation, [status(esa), new_symbols(skolem, [sK72, sK73])], [f148, f150, f149])).
fof(f149, plain, ! [X0] : (? [X1] : (~ r1(X0) & ~ p1(X1)) => (~ r1(X0) & ~ p1(sK72(X0)))), introduced(choice_axiom, [])).
fof(f150, plain, (? [X2] : q1(X2) => q1(sK73)), introduced(choice_axiom, [])).
fof(f148, plain, ((! [X0] : ? [X1] : (~ r1(X0) & ~ p1(X1)) & ? [X2] : q1(X2) & ! [X3] : p1(X3)) | ~ sP14), inference(rectify, [], [f147])).
fof(f147, plain, ((! [X105] : ? [X106] : (~ r1(X105) & ~ p1(X106)) & ? [X103] : q1(X103) & ! [X104] : p1(X104)) | ~ sP14), inference(nnf_transformation, [], [f21])).
fof(f751, plain, (~ spl103_83 | spl103_84), inference(avatar_split_clause, [], [f285, f749, f744])).
fof(f285, plain, ! [X0] : (~ p1(sK72(X0)) | ~ sP14), inference(cnf_transformation, [], [f151])).
fof(f742, plain, (~ spl103_80 | spl103_9), inference(avatar_split_clause, [], [f279, f407, f730])).
fof(f279, plain, ! [X2] : (p1(X2) | ~ sP15), inference(cnf_transformation, [], [f146])).
fof(f146, plain, ((! [X0] : (~ r1(X0) & ~ p1(sK70(X0))) & ! [X2] : (q1(sK71(X2)) & p1(X2))) | ~ sP15), inference(skolemisation, [status(esa), new_symbols(skolem, [sK70, sK71])], [f143, f145, f144])).
fof(f144, plain, ! [X0] : (? [X1] : (~ r1(X0) & ~ p1(X1)) => (~ r1(X0) & ~ p1(sK70(X0)))), introduced(choice_axiom, [])).
fof(f145, plain, ! [X2] : (? [X3] : (q1(X3) & p1(X2)) => (q1(sK71(X2)) & p1(X2))), introduced(choice_axiom, [])).
fof(f143, plain, ((! [X0] : ? [X1] : (~ r1(X0) & ~ p1(X1)) & ! [X2] : ? [X3] : (q1(X3) & p1(X2))) | ~ sP15), inference(rectify, [], [f142])).
fof(f142, plain, ((! [X101] : ? [X102] : (~ r1(X101) & ~ p1(X102)) & ! [X99] : ? [X100] : (q1(X100) & p1(X99))) | ~ sP15), inference(nnf_transformation, [], [f22])).
fof(f737, plain, (~ spl103_80 | spl103_81), inference(avatar_split_clause, [], [f281, f735, f730])).
fof(f281, plain, ! [X0] : (~ p1(sK70(X0)) | ~ sP15), inference(cnf_transformation, [], [f146])).
fof(f728, plain, (~ spl103_76 | spl103_56), inference(avatar_split_clause, [], [f275, f620, f711])).
fof(f275, plain, ! [X2] : (q1(X2) | ~ p1(X2) | ~ sP16), inference(cnf_transformation, [], [f141])).
fof(f141, plain, ((~ q1(sK69) & ! [X1] : (p1(X1) | ~ r1(X1)) & r1(sK69) & ! [X2] : (q1(X2) | ~ p1(X2))) | ~ sP16), inference(skolemisation, [status(esa), new_symbols(skolem, [sK69])], [f139, f140])).
fof(f140, plain, (? [X0] : (~ q1(X0) & ! [X1] : (p1(X1) | ~ r1(X1)) & r1(X0) & ! [X2] : (q1(X2) | ~ p1(X2))) => (~ q1(sK69) & ! [X1] : (p1(X1) | ~ r1(X1)) & r1(sK69) & ! [X2] : (q1(X2) | ~ p1(X2)))), introduced(choice_axiom, [])).
fof(f139, plain, (? [X0] : (~ q1(X0) & ! [X1] : (p1(X1) | ~ r1(X1)) & r1(X0) & ! [X2] : (q1(X2) | ~ p1(X2))) | ~ sP16), inference(rectify, [], [f138])).
fof(f138, plain, (? [X28] : (~ q1(X28) & ! [X30] : (p1(X30) | ~ r1(X30)) & r1(X28) & ! [X29] : (q1(X29) | ~ p1(X29))) | ~ sP16), inference(nnf_transformation, [], [f23])).
fof(f727, plain, (~ spl103_76 | spl103_79), inference(avatar_split_clause, [], [f276, f724, f711])).
fof(f276, plain, (r1(sK69) | ~ sP16), inference(cnf_transformation, [], [f141])).
fof(f722, plain, (~ spl103_76 | spl103_78), inference(avatar_split_clause, [], [f277, f720, f711])).
fof(f277, plain, ! [X1] : (p1(X1) | ~ r1(X1) | ~ sP16), inference(cnf_transformation, [], [f141])).
fof(f718, plain, (~ spl103_76 | ~ spl103_77), inference(avatar_split_clause, [], [f278, f715, f711])).
fof(f278, plain, (~ q1(sK69) | ~ sP16), inference(cnf_transformation, [], [f141])).
fof(f709, plain, (~ spl103_75 | spl103_73 | spl103_74), inference(avatar_split_clause, [], [f271, f695, f691, f703])).
fof(f691, plain, (spl103_73 <=> a0), introduced(avatar_definition, [new_symbols(naming, [spl103_73])])).
fof(f271, plain, (b0 | a0 | ~ sP17), inference(cnf_transformation, [], [f137])).
fof(f137, plain, ((~ b0 & ~ a0 & (~ b0 | ~ a0) & (b0 | a0)) | ~ sP17), inference(flattening, [], [f136])).
fof(f136, plain, ((~ b0 & ~ a0 & ((~ b0 | ~ a0) & (b0 | a0))) | ~ sP17), inference(nnf_transformation, [], [f24])).
fof(f707, plain, (~ spl103_75 | ~ spl103_73), inference(avatar_split_clause, [], [f273, f691, f703])).
fof(f273, plain, (~ a0 | ~ sP17), inference(cnf_transformation, [], [f137])).
fof(f706, plain, (~ spl103_75 | ~ spl103_74), inference(avatar_split_clause, [], [f274, f695, f703])).
fof(f274, plain, (~ b0 | ~ sP17), inference(cnf_transformation, [], [f137])).
fof(f701, plain, (~ spl103_72 | spl103_73), inference(avatar_split_clause, [], [f267, f691, f687])).
fof(f267, plain, (a0 | ~ sP18), inference(cnf_transformation, [], [f135])).
fof(f135, plain, (((~ b0 | ~ a0) & (b0 | a0) & b0 & a0) | ~ sP18), inference(flattening, [], [f134])).
fof(f134, plain, ((((~ b0 | ~ a0) & (b0 | a0)) & b0 & a0) | ~ sP18), inference(nnf_transformation, [], [f25])).
fof(f700, plain, (~ spl103_72 | spl103_74), inference(avatar_split_clause, [], [f268, f695, f687])).
fof(f268, plain, (b0 | ~ sP18), inference(cnf_transformation, [], [f135])).
fof(f698, plain, (~ spl103_72 | ~ spl103_73 | ~ spl103_74), inference(avatar_split_clause, [], [f270, f695, f691, f687])).
fof(f270, plain, (~ b0 | ~ a0 | ~ sP18), inference(cnf_transformation, [], [f135])).
fof(f685, plain, (~ spl103_65 | spl103_70 | spl103_71), inference(avatar_split_clause, [], [f263, f683, f679, f659])).
fof(f263, plain, ! [X3] : (p(f(X3), X3) | r1(sK68) | ~ sP19), inference(cnf_transformation, [], [f133])).
fof(f133, plain, ((! [X1, X2] : ((~ q(X1, X2) & q(f(sK68), sK68)) | ~ p(X1, X2)) & ! [X3] : (p(f(X3), X3) | (~ r1(X3) & r1(sK68)))) | ~ sP19), inference(skolemisation, [status(esa), new_symbols(skolem, [sK68])], [f131, f132])).
fof(f132, plain, (? [X0] : (! [X1, X2] : ((~ q(X1, X2) & q(f(X0), X0)) | ~ p(X1, X2)) & ! [X3] : (p(f(X3), X3) | (~ r1(X3) & r1(X0)))) => (! [X2, X1] : ((~ q(X1, X2) & q(f(sK68), sK68)) | ~ p(X1, X2)) & ! [X3] : (p(f(X3), X3) | (~ r1(X3) & r1(sK68))))), introduced(choice_axiom, [])).
fof(f131, plain, (? [X0] : (! [X1, X2] : ((~ q(X1, X2) & q(f(X0), X0)) | ~ p(X1, X2)) & ! [X3] : (p(f(X3), X3) | (~ r1(X3) & r1(X0)))) | ~ sP19), inference(rectify, [], [f130])).
fof(f130, plain, (? [X3] : (! [X5, X6] : ((~ q(X5, X6) & q(f(X3), X3)) | ~ p(X5, X6)) & ! [X4] : (p(f(X4), X4) | (~ r1(X4) & r1(X3)))) | ~ sP19), inference(nnf_transformation, [], [f26])).
fof(f677, plain, (~ spl103_65 | spl103_69), inference(avatar_split_clause, [], [f264, f675, f659])).
fof(f264, plain, ! [X3] : (p(f(X3), X3) | ~ r1(X3) | ~ sP19), inference(cnf_transformation, [], [f133])).
fof(f673, plain, (~ spl103_65 | spl103_67 | spl103_68), inference(avatar_split_clause, [], [f265, f670, f667, f659])).
fof(f265, plain, ! [X2, X1] : (q(f(sK68), sK68) | ~ p(X1, X2) | ~ sP19), inference(cnf_transformation, [], [f133])).
fof(f665, plain, (~ spl103_65 | spl103_66), inference(avatar_split_clause, [], [f266, f663, f659])).
fof(f266, plain, ! [X2, X1] : (~ q(X1, X2) | ~ p(X1, X2) | ~ sP19), inference(cnf_transformation, [], [f133])).
fof(f657, plain, (~ spl103_61 | spl103_56), inference(avatar_split_clause, [], [f259, f620, f640])).
fof(f259, plain, ! [X2] : (q1(X2) | ~ p1(X2) | ~ sP20), inference(cnf_transformation, [], [f129])).
fof(f129, plain, ((! [X0] : (~ r1(X0) & p1(X0)) & (r1(sK67) | ~ q1(sK67)) & ! [X2] : (q1(X2) | ~ p1(X2))) | ~ sP20), inference(skolemisation, [status(esa), new_symbols(skolem, [sK67])], [f127, f128])).
fof(f128, plain, (? [X1] : (r1(X1) | ~ q1(X1)) => (r1(sK67) | ~ q1(sK67))), introduced(choice_axiom, [])).
fof(f127, plain, ((! [X0] : (~ r1(X0) & p1(X0)) & ? [X1] : (r1(X1) | ~ q1(X1)) & ! [X2] : (q1(X2) | ~ p1(X2))) | ~ sP20), inference(rectify, [], [f126])).
fof(f126, plain, ((! [X2] : (~ r1(X2) & p1(X2)) & ? [X0] : (r1(X0) | ~ q1(X0)) & ! [X1] : (q1(X1) | ~ p1(X1))) | ~ sP20), inference(nnf_transformation, [], [f27])).
fof(f656, plain, (~ spl103_61 | ~ spl103_63 | spl103_64), inference(avatar_split_clause, [], [f260, f653, f649, f640])).
fof(f260, plain, (r1(sK67) | ~ q1(sK67) | ~ sP20), inference(cnf_transformation, [], [f129])).
fof(f647, plain, (~ spl103_61 | spl103_9), inference(avatar_split_clause, [], [f261, f407, f640])).
fof(f261, plain, ! [X0] : (p1(X0) | ~ sP20), inference(cnf_transformation, [], [f129])).
fof(f646, plain, (~ spl103_61 | spl103_62), inference(avatar_split_clause, [], [f262, f644, f640])).
fof(f262, plain, ! [X0] : (~ r1(X0) | ~ sP20), inference(cnf_transformation, [], [f129])).
fof(f634, plain, (~ spl103_57 | spl103_59), inference(avatar_split_clause, [], [f257, f632, f624])).
fof(f257, plain, ! [X1] : (a(sK66(X1), sK66(X1)) | ~ sP21), inference(cnf_transformation, [], [f125])).
fof(f125, plain, ((! [X0] : ~ a(X0, X0) & ! [X1] : (a(sK66(X1), sK66(X1)) & a(X1, sK66(X1)))) | ~ sP21), inference(skolemisation, [status(esa), new_symbols(skolem, [sK66])], [f123, f124])).
fof(f124, plain, ! [X1] : (? [X2] : (a(X2, X2) & a(X1, X2)) => (a(sK66(X1), sK66(X1)) & a(X1, sK66(X1)))), introduced(choice_axiom, [])).
fof(f123, plain, ((! [X0] : ~ a(X0, X0) & ! [X1] : ? [X2] : (a(X2, X2) & a(X1, X2))) | ~ sP21), inference(rectify, [], [f122])).
fof(f122, plain, ((! [X98] : ~ a(X98, X98) & ! [X96] : ? [X97] : (a(X97, X97) & a(X96, X97))) | ~ sP21), inference(nnf_transformation, [], [f28])).
fof(f630, plain, (~ spl103_57 | spl103_58), inference(avatar_split_clause, [], [f258, f628, f624])).
fof(f258, plain, ! [X0] : (~ a(X0, X0) | ~ sP21), inference(cnf_transformation, [], [f125])).
fof(f622, plain, (~ spl103_54 | spl103_56), inference(avatar_split_clause, [], [f253, f620, f610])).
fof(f253, plain, ! [X2] : (q1(X2) | ~ p1(X2) | ~ sP22), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ((~ q1(sK65) & ! [X1] : p1(X1) & ! [X2] : (q1(X2) | ~ p1(X2))) | ~ sP22), inference(skolemisation, [status(esa), new_symbols(skolem, [sK65])], [f119, f120])).
fof(f120, plain, (? [X0] : ~ q1(X0) => ~ q1(sK65)), introduced(choice_axiom, [])).
fof(f119, plain, ((? [X0] : ~ q1(X0) & ! [X1] : p1(X1) & ! [X2] : (q1(X2) | ~ p1(X2))) | ~ sP22), inference(rectify, [], [f118])).
fof(f118, plain, ((? [X79] : ~ q1(X79) & ! [X78] : p1(X78) & ! [X77] : (q1(X77) | ~ p1(X77))) | ~ sP22), inference(nnf_transformation, [], [f29])).
fof(f618, plain, (~ spl103_54 | spl103_9), inference(avatar_split_clause, [], [f254, f407, f610])).
fof(f254, plain, ! [X1] : (p1(X1) | ~ sP22), inference(cnf_transformation, [], [f121])).
fof(f617, plain, (~ spl103_54 | ~ spl103_55), inference(avatar_split_clause, [], [f255, f614, f610])).
fof(f255, plain, (~ q1(sK65) | ~ sP22), inference(cnf_transformation, [], [f121])).
fof(f608, plain, (~ spl103_51 | spl103_53), inference(avatar_split_clause, [], [f250, f606, f596])).
fof(f250, plain, ! [X2] : (b(X2) | ~ a1(X2) | ~ sP23), inference(cnf_transformation, [], [f117])).
fof(f117, plain, ((! [X0] : ~ b(X0) & a1(sK64) & ! [X2] : (b(X2) | ~ a1(X2))) | ~ sP23), inference(skolemisation, [status(esa), new_symbols(skolem, [sK64])], [f115, f116])).
fof(f116, plain, (? [X1] : a1(X1) => a1(sK64)), introduced(choice_axiom, [])).
fof(f115, plain, ((! [X0] : ~ b(X0) & ? [X1] : a1(X1) & ! [X2] : (b(X2) | ~ a1(X2))) | ~ sP23), inference(rectify, [], [f114])).
fof(f114, plain, ((! [X72] : ~ b(X72) & ? [X71] : a1(X71) & ! [X70] : (b(X70) | ~ a1(X70))) | ~ sP23), inference(nnf_transformation, [], [f30])).
fof(f604, plain, (~ spl103_51 | spl103_52), inference(avatar_split_clause, [], [f251, f601, f596])).
fof(f251, plain, (a1(sK64) | ~ sP23), inference(cnf_transformation, [], [f117])).
fof(f599, plain, (~ spl103_51 | spl103_40), inference(avatar_split_clause, [], [f252, f548, f596])).
fof(f252, plain, ! [X0] : (~ b(X0) | ~ sP23), inference(cnf_transformation, [], [f117])).
fof(f594, plain, (~ spl103_47 | ~ spl103_49 | spl103_50), inference(avatar_split_clause, [], [f247, f591, f587, f578])).
fof(f247, plain, (b(sK63) | ~ a1(sK63) | ~ sP24), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ((! [X0] : ~ b(X0) & ! [X1] : a1(X1) & (b(sK63) | ~ a1(sK63))) | ~ sP24), inference(skolemisation, [status(esa), new_symbols(skolem, [sK63])], [f111, f112])).
fof(f112, plain, (? [X2] : (b(X2) | ~ a1(X2)) => (b(sK63) | ~ a1(sK63))), introduced(choice_axiom, [])).
fof(f111, plain, ((! [X0] : ~ b(X0) & ! [X1] : a1(X1) & ? [X2] : (b(X2) | ~ a1(X2))) | ~ sP24), inference(rectify, [], [f110])).
fof(f110, plain, ((! [X69] : ~ b(X69) & ! [X68] : a1(X68) & ? [X67] : (b(X67) | ~ a1(X67))) | ~ sP24), inference(nnf_transformation, [], [f31])).
fof(f585, plain, (~ spl103_47 | spl103_48), inference(avatar_split_clause, [], [f248, f583, f578])).
fof(f248, plain, ! [X1] : (a1(X1) | ~ sP24), inference(cnf_transformation, [], [f113])).
fof(f581, plain, (~ spl103_47 | spl103_40), inference(avatar_split_clause, [], [f249, f548, f578])).
fof(f249, plain, ! [X0] : (~ b(X0) | ~ sP24), inference(cnf_transformation, [], [f113])).
fof(f576, plain, (~ spl103_43 | spl103_46), inference(avatar_split_clause, [], [f244, f573, f561])).
fof(f244, plain, (a1(sK62) | ~ sP25), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ((! [X0] : (~ b(X0) | ~ a1(X0)) & ! [X1] : b(X1) & a1(sK62)) | ~ sP25), inference(skolemisation, [status(esa), new_symbols(skolem, [sK62])], [f107, f108])).
fof(f108, plain, (? [X2] : a1(X2) => a1(sK62)), introduced(choice_axiom, [])).
fof(f107, plain, ((! [X0] : (~ b(X0) | ~ a1(X0)) & ! [X1] : b(X1) & ? [X2] : a1(X2)) | ~ sP25), inference(rectify, [], [f106])).
fof(f106, plain, ((! [X64] : (~ b(X64) | ~ a1(X64)) & ! [X62] : b(X62) & ? [X63] : a1(X63)) | ~ sP25), inference(nnf_transformation, [], [f32])).
fof(f571, plain, (~ spl103_43 | spl103_45), inference(avatar_split_clause, [], [f245, f569, f561])).
fof(f245, plain, ! [X1] : (b(X1) | ~ sP25), inference(cnf_transformation, [], [f109])).
fof(f567, plain, (~ spl103_43 | spl103_44), inference(avatar_split_clause, [], [f246, f565, f561])).
fof(f246, plain, ! [X0] : (~ b(X0) | ~ a1(X0) | ~ sP25), inference(cnf_transformation, [], [f109])).
fof(f559, plain, (~ spl103_39 | spl103_42), inference(avatar_split_clause, [], [f241, f556, f544])).
fof(f241, plain, (b(sK61) | ~ sP26), inference(cnf_transformation, [], [f105])).
fof(f105, plain, ((! [X0] : (~ b(X0) & ~ a1(X0)) & b(sK61)) | ~ sP26), inference(skolemisation, [status(esa), new_symbols(skolem, [sK61])], [f103, f104])).
fof(f104, plain, (? [X1] : b(X1) => b(sK61)), introduced(choice_axiom, [])).
fof(f103, plain, ((! [X0] : (~ b(X0) & ~ a1(X0)) & ? [X1] : b(X1)) | ~ sP26), inference(rectify, [], [f102])).
fof(f102, plain, ((! [X61] : (~ b(X61) & ~ a1(X61)) & ? [X60] : b(X60)) | ~ sP26), inference(nnf_transformation, [], [f33])).
fof(f550, plain, (~ spl103_39 | spl103_40), inference(avatar_split_clause, [], [f243, f548, f544])).
fof(f243, plain, ! [X0] : (~ b(X0) | ~ sP26), inference(cnf_transformation, [], [f105])).
fof(f542, plain, (~ spl103_35 | spl103_5 | spl103_38), inference(avatar_split_clause, [], [f238, f540, f390, f526])).
fof(f238, plain, ! [X2, X3] : (r1(X3) | ~ p1(X2) | ~ sP27), inference(cnf_transformation, [], [f101])).
fof(f101, plain, (! [X2, X3] : (~ r1(sK60) & p1(sK59) & (r1(X3) | ~ p1(X2))) | ~ sP27), inference(skolemisation, [status(esa), new_symbols(skolem, [sK59, sK60])], [f99, f100])).
fof(f100, plain, (? [X0, X1] : ! [X2, X3] : (~ r1(X1) & p1(X0) & (r1(X3) | ~ p1(X2))) => ! [X3, X2] : (~ r1(sK60) & p1(sK59) & (r1(X3) | ~ p1(X2)))), introduced(choice_axiom, [])).
fof(f99, plain, (? [X0, X1] : ! [X2, X3] : (~ r1(X1) & p1(X0) & (r1(X3) | ~ p1(X2))) | ~ sP27), inference(rectify, [], [f98])).
fof(f98, plain, (? [X24, X25] : ! [X26, X27] : (~ r1(X25) & p1(X24) & (r1(X27) | ~ p1(X26))) | ~ sP27), inference(nnf_transformation, [], [f34])).
fof(f538, plain, (~ spl103_35 | spl103_37), inference(avatar_split_clause, [], [f239, f535, f526])).
fof(f239, plain, (p1(sK59) | ~ sP27), inference(cnf_transformation, [], [f101])).
fof(f533, plain, (~ spl103_35 | ~ spl103_36), inference(avatar_split_clause, [], [f240, f530, f526])).
fof(f240, plain, (~ r1(sK60) | ~ sP27), inference(cnf_transformation, [], [f101])).
fof(f523, plain, (~ spl103_32 | spl103_34), inference(avatar_split_clause, [], [f236, f521, f512])).
fof(f236, plain, ! [X1] : (q1(X1) | ~ sP28), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ q1(sK58) & ! [X1] : (q1(X1) & p1(X1))) | ~ sP28), inference(skolemisation, [status(esa), new_symbols(skolem, [sK58])], [f95, f96])).
fof(f96, plain, (? [X0] : (~ q1(X0) & ! [X1] : (q1(X1) & p1(X1))) => (~ q1(sK58) & ! [X1] : (q1(X1) & p1(X1)))), introduced(choice_axiom, [])).
fof(f95, plain, (? [X0] : (~ q1(X0) & ! [X1] : (q1(X1) & p1(X1))) | ~ sP28), inference(rectify, [], [f94])).
fof(f94, plain, (? [X11] : (~ q1(X11) & ! [X12] : (q1(X12) & p1(X12))) | ~ sP28), inference(nnf_transformation, [], [f35])).
fof(f519, plain, (~ spl103_32 | ~ spl103_33), inference(avatar_split_clause, [], [f237, f516, f512])).
fof(f237, plain, (~ q1(sK58) | ~ sP28), inference(cnf_transformation, [], [f97])).
fof(f510, plain, (~ spl103_29 | spl103_9), inference(avatar_split_clause, [], [f233, f407, f498])).
fof(f233, plain, ! [X2] : (p1(X2) | ~ sP29), inference(cnf_transformation, [], [f93])).
fof(f93, plain, (((~ p1(sK57) | ~ p1(sK56)) & ! [X2] : p1(X2)) | ~ sP29), inference(skolemisation, [status(esa), new_symbols(skolem, [sK56, sK57])], [f91, f92])).
fof(f92, plain, (? [X0, X1] : (~ p1(X1) | ~ p1(X0)) => (~ p1(sK57) | ~ p1(sK56))), introduced(choice_axiom, [])).
fof(f91, plain, ((? [X0, X1] : (~ p1(X1) | ~ p1(X0)) & ! [X2] : p1(X2)) | ~ sP29), inference(rectify, [], [f90])).
fof(f90, plain, ((? [X108, X109] : (~ p1(X109) | ~ p1(X108)) & ! [X107] : p1(X107)) | ~ sP29), inference(nnf_transformation, [], [f36])).
fof(f509, plain, (~ spl103_29 | ~ spl103_30 | ~ spl103_31), inference(avatar_split_clause, [], [f234, f506, f502, f498])).
fof(f234, plain, (~ p1(sK57) | ~ p1(sK56) | ~ sP29), inference(cnf_transformation, [], [f93])).
fof(f496, plain, (~ spl103_28 | spl103_9), inference(avatar_split_clause, [], [f231, f407, f492])).
fof(f231, plain, ! [X1] : (p1(X1) | ~ sP30), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ((! [X0] : ~ p1(X0) & ! [X1] : p1(X1)) | ~ sP30), inference(rectify, [], [f88])).
fof(f88, plain, ((! [X81] : ~ p1(X81) & ! [X80] : p1(X80)) | ~ sP30), inference(nnf_transformation, [], [f37])).
fof(f495, plain, (~ spl103_28 | spl103_5), inference(avatar_split_clause, [], [f232, f390, f492])).
fof(f232, plain, ! [X0] : (~ p1(X0) | ~ sP30), inference(cnf_transformation, [], [f89])).
fof(f490, plain, (~ spl103_25 | spl103_27), inference(avatar_split_clause, [], [f229, f488, f480])).
fof(f229, plain, ! [X1] : (~ a(X1, X1) | ~ a(X1, sK55) | ~ sP31), inference(cnf_transformation, [], [f87])).
fof(f87, plain, (! [X1] : ((a(X1, sK55) | a(X1, X1)) & (~ a(X1, X1) | ~ a(X1, sK55))) | ~ sP31), inference(skolemisation, [status(esa), new_symbols(skolem, [sK55])], [f85, f86])).
fof(f86, plain, (? [X0] : ! [X1] : ((a(X1, X0) | a(X1, X1)) & (~ a(X1, X1) | ~ a(X1, X0))) => ! [X1] : ((a(X1, sK55) | a(X1, X1)) & (~ a(X1, X1) | ~ a(X1, sK55)))), introduced(choice_axiom, [])).
fof(f85, plain, (? [X0] : ! [X1] : ((a(X1, X0) | a(X1, X1)) & (~ a(X1, X1) | ~ a(X1, X0))) | ~ sP31), inference(rectify, [], [f84])).
fof(f84, plain, (? [X65] : ! [X66] : ((a(X66, X65) | a(X66, X66)) & (~ a(X66, X66) | ~ a(X66, X65))) | ~ sP31), inference(nnf_transformation, [], [f38])).
fof(f486, plain, (~ spl103_25 | spl103_26), inference(avatar_split_clause, [], [f230, f484, f480])).
fof(f230, plain, ! [X1] : (a(X1, sK55) | a(X1, X1) | ~ sP31), inference(cnf_transformation, [], [f87])).
fof(f478, plain, (~ spl103_21 | spl103_23 | spl103_24), inference(avatar_split_clause, [], [f227, f475, f471, f463])).
fof(f227, plain, (a(sK52, sK51) | a(sK53, sK54) | ~ sP32), inference(cnf_transformation, [], [f83])).
fof(f83, plain, (((! [X0, X1] : ~ a(X1, X0) | ! [X2, X3] : ~ a(X2, X3)) & (a(sK52, sK51) | a(sK53, sK54))) | ~ sP32), inference(skolemisation, [status(esa), new_symbols(skolem, [sK51, sK52, sK53, sK54])], [f80, f82, f81])).
fof(f81, plain, (? [X4, X5] : a(X5, X4) => a(sK52, sK51)), introduced(choice_axiom, [])).
fof(f82, plain, (? [X6, X7] : a(X6, X7) => a(sK53, sK54)), introduced(choice_axiom, [])).
fof(f80, plain, (((! [X0, X1] : ~ a(X1, X0) | ! [X2, X3] : ~ a(X2, X3)) & (? [X4, X5] : a(X5, X4) | ? [X6, X7] : a(X6, X7))) | ~ sP32), inference(rectify, [], [f79])).
fof(f79, plain, (((! [X58, X59] : ~ a(X59, X58) | ! [X56, X57] : ~ a(X56, X57)) & (? [X58, X59] : a(X59, X58) | ? [X56, X57] : a(X56, X57))) | ~ sP32), inference(nnf_transformation, [], [f39])).
fof(f469, plain, (~ spl103_21 | spl103_22 | spl103_22), inference(avatar_split_clause, [], [f228, f467, f467, f463])).
fof(f228, plain, ! [X2, X0, X3, X1] : (~ a(X1, X0) | ~ a(X2, X3) | ~ sP32), inference(cnf_transformation, [], [f83])).
fof(f461, plain, (~ spl103_18 | spl103_9), inference(avatar_split_clause, [], [f225, f407, f449])).
fof(f225, plain, ! [X2] : (p1(X2) | ~ sP33), inference(cnf_transformation, [], [f78])).
fof(f78, plain, (((~ p1(sK50) | ~ p1(sK49)) & ! [X2] : p1(X2)) | ~ sP33), inference(skolemisation, [status(esa), new_symbols(skolem, [sK49, sK50])], [f76, f77])).
fof(f77, plain, (? [X0, X1] : ((~ p1(X1) | ~ p1(X0)) & ! [X2] : p1(X2)) => ((~ p1(sK50) | ~ p1(sK49)) & ! [X2] : p1(X2))), introduced(choice_axiom, [])).
fof(f76, plain, (? [X0, X1] : ((~ p1(X1) | ~ p1(X0)) & ! [X2] : p1(X2)) | ~ sP33), inference(rectify, [], [f75])).
fof(f75, plain, (? [X53, X54] : ((~ p1(X54) | ~ p1(X53)) & ! [X55] : p1(X55)) | ~ sP33), inference(nnf_transformation, [], [f40])).
fof(f460, plain, (~ spl103_18 | ~ spl103_19 | ~ spl103_20), inference(avatar_split_clause, [], [f226, f457, f453, f449])).
fof(f226, plain, (~ p1(sK50) | ~ p1(sK49) | ~ sP33), inference(cnf_transformation, [], [f78])).
fof(f447, plain, (~ spl103_15 | spl103_9), inference(avatar_split_clause, [], [f223, f407, f435])).
fof(f223, plain, ! [X2] : (p1(X2) | ~ sP34), inference(cnf_transformation, [], [f74])).
fof(f74, plain, (((~ p1(sK47) | ~ p1(sK48)) & ! [X2] : p1(X2)) | ~ sP34), inference(skolemisation, [status(esa), new_symbols(skolem, [sK47, sK48])], [f71, f73, f72])).
fof(f72, plain, (? [X0] : ~ p1(X0) => ~ p1(sK47)), introduced(choice_axiom, [])).
fof(f73, plain, (? [X1] : ~ p1(X1) => ~ p1(sK48)), introduced(choice_axiom, [])).
fof(f71, plain, (((? [X0] : ~ p1(X0) | ? [X1] : ~ p1(X1)) & ! [X2] : p1(X2)) | ~ sP34), inference(rectify, [], [f70])).
fof(f70, plain, (((? [X51] : ~ p1(X51) | ? [X52] : ~ p1(X52)) & ! [X50] : p1(X50)) | ~ sP34), inference(nnf_transformation, [], [f41])).
fof(f446, plain, (~ spl103_15 | ~ spl103_16 | ~ spl103_17), inference(avatar_split_clause, [], [f224, f443, f439, f435])).
fof(f224, plain, (~ p1(sK47) | ~ p1(sK48) | ~ sP34), inference(cnf_transformation, [], [f74])).
fof(f433, plain, (~ spl103_12 | spl103_13 | spl103_14), inference(avatar_split_clause, [], [f221, f430, f426, f421])).
fof(f221, plain, (p1(sK45) | p1(sK46) | ~ sP35), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((! [X0] : ~ p1(X0) | ! [X1] : ~ p1(X1)) & (p1(sK45) | p1(sK46))) | ~ sP35), inference(skolemisation, [status(esa), new_symbols(skolem, [sK45, sK46])], [f66, f68, f67])).
fof(f67, plain, (? [X2] : p1(X2) => p1(sK45)), introduced(choice_axiom, [])).
fof(f68, plain, (? [X3] : p1(X3) => p1(sK46)), introduced(choice_axiom, [])).
fof(f66, plain, (((! [X0] : ~ p1(X0) | ! [X1] : ~ p1(X1)) & (? [X2] : p1(X2) | ? [X3] : p1(X3))) | ~ sP35), inference(rectify, [], [f65])).
fof(f65, plain, (((! [X49] : ~ p1(X49) | ! [X48] : ~ p1(X48)) & (? [X49] : p1(X49) | ? [X48] : p1(X48))) | ~ sP35), inference(nnf_transformation, [], [f42])).
fof(f424, plain, (~ spl103_12 | spl103_5 | spl103_5), inference(avatar_split_clause, [], [f222, f390, f390, f421])).
fof(f222, plain, ! [X0, X1] : (~ p1(X0) | ~ p1(X1) | ~ sP35), inference(cnf_transformation, [], [f69])).
fof(f419, plain, (~ spl103_10 | spl103_11), inference(avatar_split_clause, [], [f219, f416, f411])).
fof(f219, plain, (p1(sK44) | ~ sP36), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ((! [X0] : ~ p1(X0) & p1(sK44)) | ~ sP36), inference(skolemisation, [status(esa), new_symbols(skolem, [sK44])], [f62, f63])).
fof(f63, plain, (? [X1] : p1(X1) => p1(sK44)), introduced(choice_axiom, [])).
fof(f62, plain, ((! [X0] : ~ p1(X0) & ? [X1] : p1(X1)) | ~ sP36), inference(rectify, [], [f61])).
fof(f61, plain, ((! [X47] : ~ p1(X47) & ? [X46] : p1(X46)) | ~ sP36), inference(nnf_transformation, [], [f43])).
fof(f414, plain, (~ spl103_10 | spl103_5), inference(avatar_split_clause, [], [f220, f390, f411])).
fof(f220, plain, ! [X0] : (~ p1(X0) | ~ sP36), inference(cnf_transformation, [], [f64])).
fof(f409, plain, (~ spl103_7 | spl103_9), inference(avatar_split_clause, [], [f217, f407, f399])).
fof(f217, plain, ! [X0] : (p1(X0) | ~ sP37), inference(cnf_transformation, [], [f60])).
fof(f60, plain, (! [X0] : (~ p1(sK43(X0)) & p1(X0)) | ~ sP37), inference(skolemisation, [status(esa), new_symbols(skolem, [sK43])], [f58, f59])).
fof(f59, plain, ! [X0] : (? [X1] : (~ p1(X1) & p1(X0)) => (~ p1(sK43(X0)) & p1(X0))), introduced(choice_axiom, [])).
fof(f58, plain, (! [X0] : ? [X1] : (~ p1(X1) & p1(X0)) | ~ sP37), inference(rectify, [], [f57])).
fof(f57, plain, (! [X31] : ? [X32] : (~ p1(X32) & p1(X31)) | ~ sP37), inference(nnf_transformation, [], [f44])).
fof(f405, plain, (~ spl103_7 | spl103_8), inference(avatar_split_clause, [], [f218, f403, f399])).
fof(f218, plain, ! [X0] : (~ p1(sK43(X0)) | ~ sP37), inference(cnf_transformation, [], [f60])).
fof(f397, plain, (~ spl103_4 | spl103_6), inference(avatar_split_clause, [], [f215, f394, f386])).
fof(f215, plain, (p1(sK42) | ~ sP38), inference(cnf_transformation, [], [f56])).
fof(f56, plain, (! [X0] : (~ p1(X0) & p1(sK42)) | ~ sP38), inference(skolemisation, [status(esa), new_symbols(skolem, [sK42])], [f54, f55])).
fof(f55, plain, (? [X1] : p1(X1) => p1(sK42)), introduced(choice_axiom, [])).
fof(f54, plain, (! [X0] : (~ p1(X0) & ? [X1] : p1(X1)) | ~ sP38), inference(rectify, [], [f53])).
fof(f53, plain, (! [X22] : (~ p1(X22) & ? [X23] : p1(X23)) | ~ sP38), inference(nnf_transformation, [], [f45])).
fof(f392, plain, (~ spl103_4 | spl103_5), inference(avatar_split_clause, [], [f216, f390, f386])).
fof(f216, plain, ! [X0] : (~ p1(X0) | ~ sP38), inference(cnf_transformation, [], [f56])).
fof(f384, plain, (~ spl103_1 | spl103_3), inference(avatar_split_clause, [], [f213, f382, f374])).
fof(f213, plain, ! [X3] : (p(sK41, X3) | ~ sP39), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ((! [X1] : ~ p(X1, sK40) & ! [X3] : p(sK41, X3)) | ~ sP39), inference(skolemisation, [status(esa), new_symbols(skolem, [sK40, sK41])], [f49, f51, f50])).
fof(f50, plain, (? [X0] : ! [X1] : ~ p(X1, X0) => ! [X1] : ~ p(X1, sK40)), introduced(choice_axiom, [])).
fof(f51, plain, (? [X2] : ! [X3] : p(X2, X3) => ! [X3] : p(sK41, X3)), introduced(choice_axiom, [])).
fof(f49, plain, ((? [X0] : ! [X1] : ~ p(X1, X0) & ? [X2] : ! [X3] : p(X2, X3)) | ~ sP39), inference(rectify, [], [f48])).
fof(f48, plain, ((? [X20] : ! [X21] : ~ p(X21, X20) & ? [X18] : ! [X19] : p(X18, X19)) | ~ sP39), inference(nnf_transformation, [], [f46])).
fof(f380, plain, (~ spl103_1 | spl103_2), inference(avatar_split_clause, [], [f214, f378, f374])).
fof(f214, plain, ! [X1] : (~ p(X1, sK40) | ~ sP39), inference(cnf_transformation, [], [f52])).