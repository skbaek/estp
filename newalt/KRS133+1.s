fof(f3063, plain, $false, inference(avatar_sat_refutation, [], [f840, f845, f854, f859, f868, f873, f882, f887, f896, f901, f910, f915, f924, f929, f938, f943, f952, f957, f966, f971, f980, f985, f994, f999, f1008, f1013, f1022, f1027, f1036, f1041, f1050, f1055, f1064, f1069, f1078, f1083, f1092, f1097, f1106, f1111, f1120, f1125, f1134, f1139, f1148, f1153, f1162, f1167, f1176, f1181, f1190, f1195, f1204, f1209, f1218, f1223, f1232, f1237, f1246, f1251, f1260, f1265, f1274, f1279, f1288, f1293, f1302, f1307, f1316, f1321, f1330, f1335, f1344, f1349, f1358, f1363, f1372, f1377, f1386, f1391, f1400, f1405, f1414, f1419, f1428, f1433, f1442, f1447, f1456, f1461, f1470, f1475, f1484, f1489, f1498, f1503, f1512, f1517, f1526, f1531, f1540, f1545, f1554, f1559, f1568, f1573, f1582, f1587, f1596, f1601, f1610, f1615, f1624, f1629, f1638, f1643, f1652, f1657, f1666, f1671, f1680, f1685, f1694, f1699, f1708, f1713, f1723, f1725, f1740, f1747, f1754, f1757, f1764, f1767, f1770, f1773, f1915, f2099, f2120, f2126, f2134, f2146, f2159, f2172, f2186, f2212, f2231, f2240, f2257, f2275, f2284, f2291, f2311, f2339, f2351, f2367, f2379, f2399, f2406, f2423, f2432, f2445, f2462, f2482, f2494, f2511, f2523, f2538, f2547, f2552, f2567, f2575, f2594, f2606, f2624, f2645, f2666, f2687, f2704, f2716, f2732, f2744, f2768, f2774, f2784, f2804, f2827, f2840, f2857, f2869, f2885, f2911, f2920, f2938, f2953, f2977, f2992, f3002, f3017, f3030, f3049, f3062])).
fof(f3062, plain, (~ spl133_62 | ~ spl133_63), inference(avatar_contradiction_clause, [], [f3061])).
fof(f3061, plain, ($false | (~ spl133_62 | ~ spl133_63)), inference(subsumption_resolution, [], [f3056, f1119])).
fof(f1119, plain, (cAnomalepidae(sK85) | ~ spl133_62), inference(avatar_component_clause, [], [f1117])).
fof(f1117, plain, (spl133_62 <=> cAnomalepidae(sK85)), introduced(avatar_definition, [new_symbols(naming, [spl133_62])])).
fof(f3056, plain, (~ cAnomalepidae(sK85) | ~ spl133_63), inference(resolution, [], [f1124, f1884])).
fof(f1884, plain, ! [X7] : (~ cGekkonidae(X7) | ~ cAnomalepidae(X7)), inference(subsumption_resolution, [], [f1873, f636])).
fof(f636, plain, ~ (xsd_string_2 = xsd_string_7), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ~ (xsd_string_2 = xsd_string_7), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_64)).
fof(f1873, plain, ! [X7] : ((xsd_string_2 = xsd_string_7) | ~ cAnomalepidae(X7) | ~ cGekkonidae(X7)), inference(resolution, [], [f1805, f595])).
fof(f595, plain, ! [X0] : (rfamily_name(X0, xsd_string_7) | ~ cGekkonidae(X0)), inference(cnf_transformation, [], [f207])).
fof(f207, plain, ! [X0] : (rfamily_name(X0, xsd_string_7) | ~ cGekkonidae(X0)), inference(ennf_transformation, [], [f143])).
fof(f143, plain, ! [X0] : (cGekkonidae(X0) => rfamily_name(X0, xsd_string_7)), inference(rectify, [], [f44])).
fof(f44, plain, ! [X3] : (cGekkonidae(X3) => rfamily_name(X3, xsd_string_7)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_24)).
fof(f1805, plain, ! [X4, X5] : (~ rfamily_name(X5, X4) | (xsd_string_2 = X4) | ~ cAnomalepidae(X5)), inference(subsumption_resolution, [], [f1791, f581])).
fof(f581, plain, ! [X0] : (~ cAnomalepidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f198])).
fof(f198, plain, ! [X0] : (cReptile(X0) | ~ cAnomalepidae(X0)), inference(ennf_transformation, [], [f134])).
fof(f134, plain, ! [X0] : (cAnomalepidae(X0) => cReptile(X0)), inference(rectify, [], [f30])).
fof(f30, plain, ! [X3] : (cAnomalepidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_10)).
fof(f1791, plain, ! [X4, X5] : ((xsd_string_2 = X4) | ~ rfamily_name(X5, X4) | ~ cReptile(X5) | ~ cAnomalepidae(X5)), inference(resolution, [], [f604, f580])).
fof(f580, plain, ! [X0] : (rfamily_name(X0, xsd_string_2) | ~ cAnomalepidae(X0)), inference(cnf_transformation, [], [f197])).
fof(f197, plain, ! [X0] : (rfamily_name(X0, xsd_string_2) | ~ cAnomalepidae(X0)), inference(ennf_transformation, [], [f133])).
fof(f133, plain, ! [X0] : (cAnomalepidae(X0) => rfamily_name(X0, xsd_string_2)), inference(rectify, [], [f29])).
fof(f29, plain, ! [X3] : (cAnomalepidae(X3) => rfamily_name(X3, xsd_string_2)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_9)).
fof(f604, plain, ! [X2, X0, X1] : (~ rfamily_name(X0, X2) | (X1 = X2) | ~ rfamily_name(X0, X1) | ~ cReptile(X0)), inference(cnf_transformation, [], [f287])).
fof(f287, plain, ! [X0] : ((! [X1, X2] : ((X1 = X2) | ~ rfamily_name(X0, X2) | ~ rfamily_name(X0, X1)) & rfamily_name(X0, sK64(X0))) | ~ cReptile(X0)), inference(skolemisation, [status(esa), new_symbols(skolem, [sK64])], [f214, f286])).
fof(f286, plain, ! [X0] : (? [X3] : rfamily_name(X0, X3) => rfamily_name(X0, sK64(X0))), introduced(choice_axiom, [])).
fof(f214, plain, ! [X0] : ((! [X1, X2] : ((X1 = X2) | ~ rfamily_name(X0, X2) | ~ rfamily_name(X0, X1)) & ? [X3] : rfamily_name(X0, X3)) | ~ cReptile(X0)), inference(flattening, [], [f213])).
fof(f213, plain, ! [X0] : ((! [X1, X2] : ((X1 = X2) | (~ rfamily_name(X0, X2) | ~ rfamily_name(X0, X1))) & ? [X3] : rfamily_name(X0, X3)) | ~ cReptile(X0)), inference(ennf_transformation, [], [f149])).
fof(f149, plain, ! [X0] : (cReptile(X0) => (! [X1, X2] : ((rfamily_name(X0, X2) & rfamily_name(X0, X1)) => (X1 = X2)) & ? [X3] : rfamily_name(X0, X3))), inference(rectify, [], [f52])).
fof(f52, plain, ! [X3] : (cReptile(X3) => (! [X4, X5] : ((rfamily_name(X3, X5) & rfamily_name(X3, X4)) => (X4 = X5)) & ? [X4] : rfamily_name(X3, X4))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_32)).
fof(f1124, plain, (cGekkonidae(sK85) | ~ spl133_63), inference(avatar_component_clause, [], [f1122])).
fof(f1122, plain, (spl133_63 <=> cGekkonidae(sK85)), introduced(avatar_definition, [new_symbols(naming, [spl133_63])])).
fof(f3049, plain, (~ spl133_110 | ~ spl133_111), inference(avatar_split_clause, [], [f3041, f1346, f1341])).
fof(f1341, plain, (spl133_110 <=> cCordylidae(sK101)), introduced(avatar_definition, [new_symbols(naming, [spl133_110])])).
fof(f1346, plain, (spl133_111 <=> cSphenodontidae(sK101)), introduced(avatar_definition, [new_symbols(naming, [spl133_111])])).
fof(f3041, plain, (~ cCordylidae(sK101) | ~ spl133_111), inference(resolution, [], [f1348, f1937])).
fof(f1937, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cCordylidae(X10)), inference(subsumption_resolution, [], [f1928, f654])).
fof(f654, plain, ~ (xsd_string_4 = xsd_string_10), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ~ (xsd_string_4 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_82)).
fof(f1928, plain, ! [X10] : ((xsd_string_4 = xsd_string_10) | ~ cCordylidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1807, f606])).
fof(f606, plain, ! [X0] : (rfamily_name(X0, xsd_string_10) | ~ cSphenodontidae(X0)), inference(cnf_transformation, [], [f215])).
fof(f215, plain, ! [X0] : (rfamily_name(X0, xsd_string_10) | ~ cSphenodontidae(X0)), inference(ennf_transformation, [], [f150])).
fof(f150, plain, ! [X0] : (cSphenodontidae(X0) => rfamily_name(X0, xsd_string_10)), inference(rectify, [], [f54])).
fof(f54, plain, ! [X3] : (cSphenodontidae(X3) => rfamily_name(X3, xsd_string_10)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_34)).
fof(f1807, plain, ! [X8, X9] : (~ rfamily_name(X9, X8) | (xsd_string_4 = X8) | ~ cCordylidae(X9)), inference(subsumption_resolution, [], [f1793, f587])).
fof(f587, plain, ! [X0] : (~ cCordylidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f202])).
fof(f202, plain, ! [X0] : (cReptile(X0) | ~ cCordylidae(X0)), inference(ennf_transformation, [], [f138])).
fof(f138, plain, ! [X0] : (cCordylidae(X0) => cReptile(X0)), inference(rectify, [], [f36])).
fof(f36, plain, ! [X3] : (cCordylidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_16)).
fof(f1793, plain, ! [X8, X9] : ((xsd_string_4 = X8) | ~ rfamily_name(X9, X8) | ~ cReptile(X9) | ~ cCordylidae(X9)), inference(resolution, [], [f604, f586])).
fof(f586, plain, ! [X0] : (rfamily_name(X0, xsd_string_4) | ~ cCordylidae(X0)), inference(cnf_transformation, [], [f201])).
fof(f201, plain, ! [X0] : (rfamily_name(X0, xsd_string_4) | ~ cCordylidae(X0)), inference(ennf_transformation, [], [f137])).
fof(f137, plain, ! [X0] : (cCordylidae(X0) => rfamily_name(X0, xsd_string_4)), inference(rectify, [], [f35])).
fof(f35, plain, ! [X3] : (cCordylidae(X3) => rfamily_name(X3, xsd_string_4)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_15)).
fof(f1348, plain, (cSphenodontidae(sK101) | ~ spl133_111), inference(avatar_component_clause, [], [f1346])).
fof(f3030, plain, (~ spl133_123 | ~ spl133_122), inference(avatar_split_clause, [], [f3022, f1397, f1402])).
fof(f1402, plain, (spl133_123 <=> cBipedidae(sK105)), introduced(avatar_definition, [new_symbols(naming, [spl133_123])])).
fof(f1397, plain, (spl133_122 <=> cLoxocemidae(sK105)), introduced(avatar_definition, [new_symbols(naming, [spl133_122])])).
fof(f3022, plain, (~ cBipedidae(sK105) | ~ spl133_122), inference(resolution, [], [f1399, f1909])).
fof(f1909, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cBipedidae(X9)), inference(subsumption_resolution, [], [f1899, f646])).
fof(f646, plain, ~ (xsd_string_3 = xsd_string_9), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ~ (xsd_string_3 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_74)).
fof(f1899, plain, ! [X9] : ((xsd_string_3 = xsd_string_9) | ~ cBipedidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1806, f601])).
fof(f601, plain, ! [X0] : (rfamily_name(X0, xsd_string_9) | ~ cLoxocemidae(X0)), inference(cnf_transformation, [], [f211])).
fof(f211, plain, ! [X0] : (rfamily_name(X0, xsd_string_9) | ~ cLoxocemidae(X0)), inference(ennf_transformation, [], [f147])).
fof(f147, plain, ! [X0] : (cLoxocemidae(X0) => rfamily_name(X0, xsd_string_9)), inference(rectify, [], [f50])).
fof(f50, plain, ! [X3] : (cLoxocemidae(X3) => rfamily_name(X3, xsd_string_9)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_30)).
fof(f1806, plain, ! [X6, X7] : (~ rfamily_name(X7, X6) | (xsd_string_3 = X6) | ~ cBipedidae(X7)), inference(subsumption_resolution, [], [f1792, f584])).
fof(f584, plain, ! [X0] : (~ cBipedidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f200])).
fof(f200, plain, ! [X0] : (cReptile(X0) | ~ cBipedidae(X0)), inference(ennf_transformation, [], [f136])).
fof(f136, plain, ! [X0] : (cBipedidae(X0) => cReptile(X0)), inference(rectify, [], [f33])).
fof(f33, plain, ! [X3] : (cBipedidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_13)).
fof(f1792, plain, ! [X6, X7] : ((xsd_string_3 = X6) | ~ rfamily_name(X7, X6) | ~ cReptile(X7) | ~ cBipedidae(X7)), inference(resolution, [], [f604, f583])).
fof(f583, plain, ! [X0] : (rfamily_name(X0, xsd_string_3) | ~ cBipedidae(X0)), inference(cnf_transformation, [], [f199])).
fof(f199, plain, ! [X0] : (rfamily_name(X0, xsd_string_3) | ~ cBipedidae(X0)), inference(ennf_transformation, [], [f135])).
fof(f135, plain, ! [X0] : (cBipedidae(X0) => rfamily_name(X0, xsd_string_3)), inference(rectify, [], [f32])).
fof(f32, plain, ! [X3] : (cBipedidae(X3) => rfamily_name(X3, xsd_string_3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_12)).
fof(f1399, plain, (cLoxocemidae(sK105) | ~ spl133_122), inference(avatar_component_clause, [], [f1397])).
fof(f3017, plain, (~ spl133_98 | ~ spl133_99), inference(avatar_contradiction_clause, [], [f3016])).
fof(f3016, plain, ($false | (~ spl133_98 | ~ spl133_99)), inference(subsumption_resolution, [], [f3005, f1287])).
fof(f1287, plain, (cAgamidae(sK97) | ~ spl133_98), inference(avatar_component_clause, [], [f1285])).
fof(f1285, plain, (spl133_98 <=> cAgamidae(sK97)), introduced(avatar_definition, [new_symbols(naming, [spl133_98])])).
fof(f3005, plain, (~ cAgamidae(sK97) | ~ spl133_99), inference(resolution, [], [f1292, f1839])).
fof(f1839, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cAgamidae(X11)), inference(subsumption_resolution, [], [f1826, f621])).
fof(f621, plain, ~ (xsd_string_0 = xsd_string_11), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ~ (xsd_string_0 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_49)).
fof(f1826, plain, ! [X11] : ((xsd_string_0 = xsd_string_11) | ~ cAgamidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1803, f609])).
fof(f609, plain, ! [X0] : (rfamily_name(X0, xsd_string_11) | ~ cXantusiidae(X0)), inference(cnf_transformation, [], [f217])).
fof(f217, plain, ! [X0] : (rfamily_name(X0, xsd_string_11) | ~ cXantusiidae(X0)), inference(ennf_transformation, [], [f152])).
fof(f152, plain, ! [X0] : (cXantusiidae(X0) => rfamily_name(X0, xsd_string_11)), inference(rectify, [], [f57])).
fof(f57, plain, ! [X3] : (cXantusiidae(X3) => rfamily_name(X3, xsd_string_11)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_37)).
fof(f1803, plain, ! [X0, X1] : (~ rfamily_name(X1, X0) | (xsd_string_0 = X0) | ~ cAgamidae(X1)), inference(subsumption_resolution, [], [f1789, f575])).
fof(f575, plain, ! [X0] : (~ cAgamidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f194])).
fof(f194, plain, ! [X0] : (cReptile(X0) | ~ cAgamidae(X0)), inference(ennf_transformation, [], [f130])).
fof(f130, plain, ! [X0] : (cAgamidae(X0) => cReptile(X0)), inference(rectify, [], [f24])).
fof(f24, plain, ! [X3] : (cAgamidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_4)).
fof(f1789, plain, ! [X0, X1] : ((xsd_string_0 = X0) | ~ rfamily_name(X1, X0) | ~ cReptile(X1) | ~ cAgamidae(X1)), inference(resolution, [], [f604, f574])).
fof(f574, plain, ! [X0] : (rfamily_name(X0, xsd_string_0) | ~ cAgamidae(X0)), inference(cnf_transformation, [], [f193])).
fof(f193, plain, ! [X0] : (rfamily_name(X0, xsd_string_0) | ~ cAgamidae(X0)), inference(ennf_transformation, [], [f129])).
fof(f129, plain, ! [X0] : (cAgamidae(X0) => rfamily_name(X0, xsd_string_0)), inference(rectify, [], [f23])).
fof(f23, plain, ! [X3] : (cAgamidae(X3) => rfamily_name(X3, xsd_string_0)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_3)).
fof(f1292, plain, (cXantusiidae(sK97) | ~ spl133_99), inference(avatar_component_clause, [], [f1290])).
fof(f1290, plain, (spl133_99 <=> cXantusiidae(sK97)), introduced(avatar_definition, [new_symbols(naming, [spl133_99])])).
fof(f3002, plain, (~ spl133_185 | ~ spl133_186), inference(avatar_split_clause, [], [f2996, f1696, f1691])).
fof(f1691, plain, (spl133_185 <=> cAnomalepidae(sK126)), introduced(avatar_definition, [new_symbols(naming, [spl133_185])])).
fof(f1696, plain, (spl133_186 <=> cBipedidae(sK126)), introduced(avatar_definition, [new_symbols(naming, [spl133_186])])).
fof(f2996, plain, (~ cAnomalepidae(sK126) | ~ spl133_186), inference(resolution, [], [f1698, f1880])).
fof(f1880, plain, ! [X3] : (~ cBipedidae(X3) | ~ cAnomalepidae(X3)), inference(subsumption_resolution, [], [f1869, f632])).
fof(f632, plain, ~ (xsd_string_2 = xsd_string_3), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ~ (xsd_string_2 = xsd_string_3), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_60)).
fof(f1869, plain, ! [X3] : ((xsd_string_2 = xsd_string_3) | ~ cAnomalepidae(X3) | ~ cBipedidae(X3)), inference(resolution, [], [f1805, f583])).
fof(f1698, plain, (cBipedidae(sK126) | ~ spl133_186), inference(avatar_component_clause, [], [f1696])).
fof(f2992, plain, (~ spl133_188 | ~ spl133_189), inference(avatar_contradiction_clause, [], [f2991])).
fof(f2991, plain, ($false | (~ spl133_188 | ~ spl133_189)), inference(subsumption_resolution, [], [f2986, f1707])).
fof(f1707, plain, (cBipedidae(sK127) | ~ spl133_188), inference(avatar_component_clause, [], [f1705])).
fof(f1705, plain, (spl133_188 <=> cBipedidae(sK127)), introduced(avatar_definition, [new_symbols(naming, [spl133_188])])).
fof(f2986, plain, (~ cBipedidae(sK127) | ~ spl133_189), inference(resolution, [], [f1712, f1908])).
fof(f1908, plain, ! [X8] : (~ cLeptotyphlopidae(X8) | ~ cBipedidae(X8)), inference(subsumption_resolution, [], [f1898, f645])).
fof(f645, plain, ~ (xsd_string_3 = xsd_string_8), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ~ (xsd_string_3 = xsd_string_8), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_73)).
fof(f1898, plain, ! [X8] : ((xsd_string_3 = xsd_string_8) | ~ cBipedidae(X8) | ~ cLeptotyphlopidae(X8)), inference(resolution, [], [f1806, f598])).
fof(f598, plain, ! [X0] : (rfamily_name(X0, xsd_string_8) | ~ cLeptotyphlopidae(X0)), inference(cnf_transformation, [], [f209])).
fof(f209, plain, ! [X0] : (rfamily_name(X0, xsd_string_8) | ~ cLeptotyphlopidae(X0)), inference(ennf_transformation, [], [f145])).
fof(f145, plain, ! [X0] : (cLeptotyphlopidae(X0) => rfamily_name(X0, xsd_string_8)), inference(rectify, [], [f47])).
fof(f47, plain, ! [X3] : (cLeptotyphlopidae(X3) => rfamily_name(X3, xsd_string_8)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_27)).
fof(f1712, plain, (cLeptotyphlopidae(sK127) | ~ spl133_189), inference(avatar_component_clause, [], [f1710])).
fof(f1710, plain, (spl133_189 <=> cLeptotyphlopidae(sK127)), introduced(avatar_definition, [new_symbols(naming, [spl133_189])])).
fof(f2977, plain, (~ spl133_128 | ~ spl133_129), inference(avatar_contradiction_clause, [], [f2976])).
fof(f2976, plain, ($false | (~ spl133_128 | ~ spl133_129)), inference(subsumption_resolution, [], [f2974, f1427])).
fof(f1427, plain, (cLoxocemidae(sK107) | ~ spl133_128), inference(avatar_component_clause, [], [f1425])).
fof(f1425, plain, (spl133_128 <=> cLoxocemidae(sK107)), introduced(avatar_definition, [new_symbols(naming, [spl133_128])])).
fof(f2974, plain, (~ cLoxocemidae(sK107) | ~ spl133_129), inference(resolution, [], [f1432, f2034])).
fof(f2034, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cLoxocemidae(X11)), inference(subsumption_resolution, [], [f2030, f675])).
fof(f675, plain, ~ (xsd_string_9 = xsd_string_11), inference(cnf_transformation, [], [f123])).
fof(f123, plain, ~ (xsd_string_9 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_103)).
fof(f2030, plain, ! [X11] : ((xsd_string_9 = xsd_string_11) | ~ cLoxocemidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1812, f609])).
fof(f1812, plain, ! [X19, X18] : (~ rfamily_name(X19, X18) | (xsd_string_9 = X18) | ~ cLoxocemidae(X19)), inference(subsumption_resolution, [], [f1798, f602])).
fof(f602, plain, ! [X0] : (~ cLoxocemidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f212])).
fof(f212, plain, ! [X0] : (cReptile(X0) | ~ cLoxocemidae(X0)), inference(ennf_transformation, [], [f148])).
fof(f148, plain, ! [X0] : (cLoxocemidae(X0) => cReptile(X0)), inference(rectify, [], [f51])).
fof(f51, plain, ! [X3] : (cLoxocemidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_31)).
fof(f1798, plain, ! [X19, X18] : ((xsd_string_9 = X18) | ~ rfamily_name(X19, X18) | ~ cReptile(X19) | ~ cLoxocemidae(X19)), inference(resolution, [], [f604, f601])).
fof(f1432, plain, (cXantusiidae(sK107) | ~ spl133_129), inference(avatar_component_clause, [], [f1430])).
fof(f1430, plain, (spl133_129 <=> cXantusiidae(sK107)), introduced(avatar_definition, [new_symbols(naming, [spl133_129])])).
fof(f2953, plain, (~ spl133_23 | ~ spl133_24), inference(avatar_split_clause, [], [f2945, f940, f935])).
fof(f935, plain, (spl133_23 <=> cAnomalepidae(sK72)), introduced(avatar_definition, [new_symbols(naming, [spl133_23])])).
fof(f940, plain, (spl133_24 <=> cLeptotyphlopidae(sK72)), introduced(avatar_definition, [new_symbols(naming, [spl133_24])])).
fof(f2945, plain, (~ cAnomalepidae(sK72) | ~ spl133_24), inference(resolution, [], [f942, f1885])).
fof(f1885, plain, ! [X8] : (~ cLeptotyphlopidae(X8) | ~ cAnomalepidae(X8)), inference(subsumption_resolution, [], [f1874, f637])).
fof(f637, plain, ~ (xsd_string_2 = xsd_string_8), inference(cnf_transformation, [], [f85])).
fof(f85, plain, ~ (xsd_string_2 = xsd_string_8), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_65)).
fof(f1874, plain, ! [X8] : ((xsd_string_2 = xsd_string_8) | ~ cAnomalepidae(X8) | ~ cLeptotyphlopidae(X8)), inference(resolution, [], [f1805, f598])).
fof(f942, plain, (cLeptotyphlopidae(sK72) | ~ spl133_24), inference(avatar_component_clause, [], [f940])).
fof(f2938, plain, (~ spl133_17 | ~ spl133_18), inference(avatar_split_clause, [], [f2927, f912, f907])).
fof(f907, plain, (spl133_17 <=> cAnomalepidae(sK70)), introduced(avatar_definition, [new_symbols(naming, [spl133_17])])).
fof(f912, plain, (spl133_18 <=> cXantusiidae(sK70)), introduced(avatar_definition, [new_symbols(naming, [spl133_18])])).
fof(f2927, plain, (~ cAnomalepidae(sK70) | ~ spl133_18), inference(resolution, [], [f914, f1888])).
fof(f1888, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cAnomalepidae(X11)), inference(subsumption_resolution, [], [f1877, f640])).
fof(f640, plain, ~ (xsd_string_2 = xsd_string_11), inference(cnf_transformation, [], [f88])).
fof(f88, plain, ~ (xsd_string_2 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_68)).
fof(f1877, plain, ! [X11] : ((xsd_string_2 = xsd_string_11) | ~ cAnomalepidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1805, f609])).
fof(f914, plain, (cXantusiidae(sK70) | ~ spl133_18), inference(avatar_component_clause, [], [f912])).
fof(f2920, plain, (~ spl133_56 | ~ spl133_57), inference(avatar_split_clause, [], [f2915, f1094, f1089])).
fof(f1089, plain, (spl133_56 <=> cAmphisbaenidae(sK83)), introduced(avatar_definition, [new_symbols(naming, [spl133_56])])).
fof(f1094, plain, (spl133_57 <=> cBipedidae(sK83)), introduced(avatar_definition, [new_symbols(naming, [spl133_57])])).
fof(f2915, plain, (~ cAmphisbaenidae(sK83) | ~ spl133_57), inference(resolution, [], [f1096, f1856])).
fof(f1856, plain, ! [X3] : (~ cBipedidae(X3) | ~ cAmphisbaenidae(X3)), inference(subsumption_resolution, [], [f1844, f623])).
fof(f623, plain, ~ (xsd_string_1 = xsd_string_3), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ~ (xsd_string_1 = xsd_string_3), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_51)).
fof(f1844, plain, ! [X3] : ((xsd_string_1 = xsd_string_3) | ~ cAmphisbaenidae(X3) | ~ cBipedidae(X3)), inference(resolution, [], [f1804, f583])).
fof(f1804, plain, ! [X2, X3] : (~ rfamily_name(X3, X2) | (xsd_string_1 = X2) | ~ cAmphisbaenidae(X3)), inference(subsumption_resolution, [], [f1790, f578])).
fof(f578, plain, ! [X0] : (~ cAmphisbaenidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f196])).
fof(f196, plain, ! [X0] : (cReptile(X0) | ~ cAmphisbaenidae(X0)), inference(ennf_transformation, [], [f132])).
fof(f132, plain, ! [X0] : (cAmphisbaenidae(X0) => cReptile(X0)), inference(rectify, [], [f27])).
fof(f27, plain, ! [X3] : (cAmphisbaenidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_7)).
fof(f1790, plain, ! [X2, X3] : ((xsd_string_1 = X2) | ~ rfamily_name(X3, X2) | ~ cReptile(X3) | ~ cAmphisbaenidae(X3)), inference(resolution, [], [f604, f577])).
fof(f577, plain, ! [X0] : (rfamily_name(X0, xsd_string_1) | ~ cAmphisbaenidae(X0)), inference(cnf_transformation, [], [f195])).
fof(f195, plain, ! [X0] : (rfamily_name(X0, xsd_string_1) | ~ cAmphisbaenidae(X0)), inference(ennf_transformation, [], [f131])).
fof(f131, plain, ! [X0] : (cAmphisbaenidae(X0) => rfamily_name(X0, xsd_string_1)), inference(rectify, [], [f26])).
fof(f26, plain, ! [X3] : (cAmphisbaenidae(X3) => rfamily_name(X3, xsd_string_1)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_6)).
fof(f1096, plain, (cBipedidae(sK83) | ~ spl133_57), inference(avatar_component_clause, [], [f1094])).
fof(f2911, plain, (~ spl133_182 | ~ spl133_183), inference(avatar_contradiction_clause, [], [f2910])).
fof(f2910, plain, ($false | (~ spl133_182 | ~ spl133_183)), inference(subsumption_resolution, [], [f2909, f1679])).
fof(f1679, plain, (cGekkonidae(sK125) | ~ spl133_182), inference(avatar_component_clause, [], [f1677])).
fof(f1677, plain, (spl133_182 <=> cGekkonidae(sK125)), introduced(avatar_definition, [new_symbols(naming, [spl133_182])])).
fof(f2909, plain, (~ cGekkonidae(sK125) | ~ spl133_183), inference(resolution, [], [f1684, f1995])).
fof(f1995, plain, ! [X8] : (~ cLeptotyphlopidae(X8) | ~ cGekkonidae(X8)), inference(subsumption_resolution, [], [f1989, f667])).
fof(f667, plain, ~ (xsd_string_7 = xsd_string_8), inference(cnf_transformation, [], [f115])).
fof(f115, plain, ~ (xsd_string_7 = xsd_string_8), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_95)).
fof(f1989, plain, ! [X8] : ((xsd_string_7 = xsd_string_8) | ~ cGekkonidae(X8) | ~ cLeptotyphlopidae(X8)), inference(resolution, [], [f1810, f598])).
fof(f1810, plain, ! [X14, X15] : (~ rfamily_name(X15, X14) | (xsd_string_7 = X14) | ~ cGekkonidae(X15)), inference(subsumption_resolution, [], [f1796, f596])).
fof(f596, plain, ! [X0] : (~ cGekkonidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f208])).
fof(f208, plain, ! [X0] : (cReptile(X0) | ~ cGekkonidae(X0)), inference(ennf_transformation, [], [f144])).
fof(f144, plain, ! [X0] : (cGekkonidae(X0) => cReptile(X0)), inference(rectify, [], [f45])).
fof(f45, plain, ! [X3] : (cGekkonidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_25)).
fof(f1796, plain, ! [X14, X15] : ((xsd_string_7 = X14) | ~ rfamily_name(X15, X14) | ~ cReptile(X15) | ~ cGekkonidae(X15)), inference(resolution, [], [f604, f595])).
fof(f1684, plain, (cLeptotyphlopidae(sK125) | ~ spl133_183), inference(avatar_component_clause, [], [f1682])).
fof(f1682, plain, (spl133_183 <=> cLeptotyphlopidae(sK125)), introduced(avatar_definition, [new_symbols(naming, [spl133_183])])).
fof(f2885, plain, (~ spl133_89 | ~ spl133_90), inference(avatar_contradiction_clause, [], [f2884])).
fof(f2884, plain, ($false | (~ spl133_89 | ~ spl133_90)), inference(subsumption_resolution, [], [f2882, f1250])).
fof(f1250, plain, (cCordylidae(sK94) | ~ spl133_90), inference(avatar_component_clause, [], [f1248])).
fof(f1248, plain, (spl133_90 <=> cCordylidae(sK94)), introduced(avatar_definition, [new_symbols(naming, [spl133_90])])).
fof(f2882, plain, (~ cCordylidae(sK94) | ~ spl133_89), inference(resolution, [], [f1245, f1933])).
fof(f1933, plain, ! [X6] : (~ cEmydidae(X6) | ~ cCordylidae(X6)), inference(subsumption_resolution, [], [f1924, f650])).
fof(f650, plain, ~ (xsd_string_4 = xsd_string_6), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ~ (xsd_string_4 = xsd_string_6), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_78)).
fof(f1924, plain, ! [X6] : ((xsd_string_4 = xsd_string_6) | ~ cCordylidae(X6) | ~ cEmydidae(X6)), inference(resolution, [], [f1807, f592])).
fof(f592, plain, ! [X0] : (rfamily_name(X0, xsd_string_6) | ~ cEmydidae(X0)), inference(cnf_transformation, [], [f205])).
fof(f205, plain, ! [X0] : (rfamily_name(X0, xsd_string_6) | ~ cEmydidae(X0)), inference(ennf_transformation, [], [f141])).
fof(f141, plain, ! [X0] : (cEmydidae(X0) => rfamily_name(X0, xsd_string_6)), inference(rectify, [], [f41])).
fof(f41, plain, ! [X3] : (cEmydidae(X3) => rfamily_name(X3, xsd_string_6)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_21)).
fof(f1245, plain, (cEmydidae(sK94) | ~ spl133_89), inference(avatar_component_clause, [], [f1243])).
fof(f1243, plain, (spl133_89 <=> cEmydidae(sK94)), introduced(avatar_definition, [new_symbols(naming, [spl133_89])])).
fof(f2869, plain, (~ spl133_192 | ~ spl133_195), inference(avatar_contradiction_clause, [], [f2868])).
fof(f2868, plain, ($false | (~ spl133_192 | ~ spl133_195)), inference(subsumption_resolution, [], [f2865, f1746])).
fof(f1746, plain, (cEmydidae(sK131) | ~ spl133_195), inference(avatar_component_clause, [], [f1744])).
fof(f1744, plain, (spl133_195 <=> cEmydidae(sK131)), introduced(avatar_definition, [new_symbols(naming, [spl133_195])])).
fof(f2865, plain, (~ cEmydidae(sK131) | ~ spl133_192), inference(resolution, [], [f1731, f1977])).
fof(f1977, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cEmydidae(X9)), inference(subsumption_resolution, [], [f1970, f664])).
fof(f664, plain, ~ (xsd_string_6 = xsd_string_9), inference(cnf_transformation, [], [f112])).
fof(f112, plain, ~ (xsd_string_6 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_92)).
fof(f1970, plain, ! [X9] : ((xsd_string_6 = xsd_string_9) | ~ cEmydidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1809, f601])).
fof(f1809, plain, ! [X12, X13] : (~ rfamily_name(X13, X12) | (xsd_string_6 = X12) | ~ cEmydidae(X13)), inference(subsumption_resolution, [], [f1795, f593])).
fof(f593, plain, ! [X0] : (~ cEmydidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f206])).
fof(f206, plain, ! [X0] : (cReptile(X0) | ~ cEmydidae(X0)), inference(ennf_transformation, [], [f142])).
fof(f142, plain, ! [X0] : (cEmydidae(X0) => cReptile(X0)), inference(rectify, [], [f42])).
fof(f42, plain, ! [X3] : (cEmydidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_22)).
fof(f1795, plain, ! [X12, X13] : ((xsd_string_6 = X12) | ~ rfamily_name(X13, X12) | ~ cReptile(X13) | ~ cEmydidae(X13)), inference(resolution, [], [f604, f592])).
fof(f1731, plain, (cLoxocemidae(sK131) | ~ spl133_192), inference(avatar_component_clause, [], [f1729])).
fof(f1729, plain, (spl133_192 <=> cLoxocemidae(sK131)), introduced(avatar_definition, [new_symbols(naming, [spl133_192])])).
fof(f2857, plain, (~ spl133_60 | ~ spl133_59), inference(avatar_split_clause, [], [f2850, f1103, f1108])).
fof(f1108, plain, (spl133_60 <=> cBipedidae(sK84)), introduced(avatar_definition, [new_symbols(naming, [spl133_60])])).
fof(f1103, plain, (spl133_59 <=> cCordylidae(sK84)), introduced(avatar_definition, [new_symbols(naming, [spl133_59])])).
fof(f2850, plain, (~ cBipedidae(sK84) | ~ spl133_59), inference(resolution, [], [f1105, f1904])).
fof(f1904, plain, ! [X4] : (~ cCordylidae(X4) | ~ cBipedidae(X4)), inference(subsumption_resolution, [], [f1894, f641])).
fof(f641, plain, ~ (xsd_string_3 = xsd_string_4), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ~ (xsd_string_3 = xsd_string_4), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_69)).
fof(f1894, plain, ! [X4] : ((xsd_string_3 = xsd_string_4) | ~ cBipedidae(X4) | ~ cCordylidae(X4)), inference(resolution, [], [f1806, f586])).
fof(f1105, plain, (cCordylidae(sK84) | ~ spl133_59), inference(avatar_component_clause, [], [f1103])).
fof(f2840, plain, (~ spl133_193 | ~ spl133_196), inference(avatar_contradiction_clause, [], [f2839])).
fof(f2839, plain, ($false | (~ spl133_193 | ~ spl133_196)), inference(subsumption_resolution, [], [f2837, f1753])).
fof(f1753, plain, (cLeptotyphlopidae(sK130) | ~ spl133_196), inference(avatar_component_clause, [], [f1751])).
fof(f1751, plain, (spl133_196 <=> cLeptotyphlopidae(sK130)), introduced(avatar_definition, [new_symbols(naming, [spl133_196])])).
fof(f2837, plain, (~ cLeptotyphlopidae(sK130) | ~ spl133_193), inference(resolution, [], [f1735, f2016])).
fof(f2016, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cLeptotyphlopidae(X10)), inference(subsumption_resolution, [], [f2011, f672])).
fof(f672, plain, ~ (xsd_string_8 = xsd_string_10), inference(cnf_transformation, [], [f120])).
fof(f120, plain, ~ (xsd_string_8 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_100)).
fof(f2011, plain, ! [X10] : ((xsd_string_8 = xsd_string_10) | ~ cLeptotyphlopidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1811, f606])).
fof(f1811, plain, ! [X17, X16] : (~ rfamily_name(X17, X16) | (xsd_string_8 = X16) | ~ cLeptotyphlopidae(X17)), inference(subsumption_resolution, [], [f1797, f599])).
fof(f599, plain, ! [X0] : (~ cLeptotyphlopidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f210])).
fof(f210, plain, ! [X0] : (cReptile(X0) | ~ cLeptotyphlopidae(X0)), inference(ennf_transformation, [], [f146])).
fof(f146, plain, ! [X0] : (cLeptotyphlopidae(X0) => cReptile(X0)), inference(rectify, [], [f48])).
fof(f48, plain, ! [X3] : (cLeptotyphlopidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_28)).
fof(f1797, plain, ! [X17, X16] : ((xsd_string_8 = X16) | ~ rfamily_name(X17, X16) | ~ cReptile(X17) | ~ cLeptotyphlopidae(X17)), inference(resolution, [], [f604, f598])).
fof(f1735, plain, (cSphenodontidae(sK130) | ~ spl133_193), inference(avatar_component_clause, [], [f1733])).
fof(f1733, plain, (spl133_193 <=> cSphenodontidae(sK130)), introduced(avatar_definition, [new_symbols(naming, [spl133_193])])).
fof(f2827, plain, (~ spl133_83 | ~ spl133_84), inference(avatar_split_clause, [], [f2821, f1220, f1215])).
fof(f1215, plain, (spl133_83 <=> cGekkonidae(sK92)), introduced(avatar_definition, [new_symbols(naming, [spl133_83])])).
fof(f1220, plain, (spl133_84 <=> cXantusiidae(sK92)), introduced(avatar_definition, [new_symbols(naming, [spl133_84])])).
fof(f2821, plain, (~ cGekkonidae(sK92) | ~ spl133_84), inference(resolution, [], [f1222, f1998])).
fof(f1998, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cGekkonidae(X11)), inference(subsumption_resolution, [], [f1992, f670])).
fof(f670, plain, ~ (xsd_string_7 = xsd_string_11), inference(cnf_transformation, [], [f118])).
fof(f118, plain, ~ (xsd_string_7 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_98)).
fof(f1992, plain, ! [X11] : ((xsd_string_7 = xsd_string_11) | ~ cGekkonidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1810, f609])).
fof(f1222, plain, (cXantusiidae(sK92) | ~ spl133_84), inference(avatar_component_clause, [], [f1220])).
fof(f2804, plain, (~ spl133_47 | ~ spl133_48), inference(avatar_split_clause, [], [f2797, f1052, f1047])).
fof(f1047, plain, (spl133_47 <=> cCrocodylidae(sK80)), introduced(avatar_definition, [new_symbols(naming, [spl133_47])])).
fof(f1052, plain, (spl133_48 <=> cSphenodontidae(sK80)), introduced(avatar_definition, [new_symbols(naming, [spl133_48])])).
fof(f2797, plain, (~ cCrocodylidae(sK80) | ~ spl133_48), inference(resolution, [], [f1054, f1958])).
fof(f1958, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cCrocodylidae(X10)), inference(subsumption_resolution, [], [f1950, f660])).
fof(f660, plain, ~ (xsd_string_5 = xsd_string_10), inference(cnf_transformation, [], [f108])).
fof(f108, plain, ~ (xsd_string_5 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_88)).
fof(f1950, plain, ! [X10] : ((xsd_string_5 = xsd_string_10) | ~ cCrocodylidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1808, f606])).
fof(f1808, plain, ! [X10, X11] : (~ rfamily_name(X11, X10) | (xsd_string_5 = X10) | ~ cCrocodylidae(X11)), inference(subsumption_resolution, [], [f1794, f590])).
fof(f590, plain, ! [X0] : (~ cCrocodylidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f204])).
fof(f204, plain, ! [X0] : (cReptile(X0) | ~ cCrocodylidae(X0)), inference(ennf_transformation, [], [f140])).
fof(f140, plain, ! [X0] : (cCrocodylidae(X0) => cReptile(X0)), inference(rectify, [], [f39])).
fof(f39, plain, ! [X3] : (cCrocodylidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_19)).
fof(f1794, plain, ! [X10, X11] : ((xsd_string_5 = X10) | ~ rfamily_name(X11, X10) | ~ cReptile(X11) | ~ cCrocodylidae(X11)), inference(resolution, [], [f604, f589])).
fof(f589, plain, ! [X0] : (rfamily_name(X0, xsd_string_5) | ~ cCrocodylidae(X0)), inference(cnf_transformation, [], [f203])).
fof(f203, plain, ! [X0] : (rfamily_name(X0, xsd_string_5) | ~ cCrocodylidae(X0)), inference(ennf_transformation, [], [f139])).
fof(f139, plain, ! [X0] : (cCrocodylidae(X0) => rfamily_name(X0, xsd_string_5)), inference(rectify, [], [f38])).
fof(f38, plain, ! [X3] : (cCrocodylidae(X3) => rfamily_name(X3, xsd_string_5)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_18)).
fof(f1054, plain, (cSphenodontidae(sK80) | ~ spl133_48), inference(avatar_component_clause, [], [f1052])).
fof(f2784, plain, (~ spl133_93 | ~ spl133_92), inference(avatar_split_clause, [], [f2776, f1257, f1262])).
fof(f1262, plain, (spl133_93 <=> cAgamidae(sK95)), introduced(avatar_definition, [new_symbols(naming, [spl133_93])])).
fof(f1257, plain, (spl133_92 <=> cEmydidae(sK95)), introduced(avatar_definition, [new_symbols(naming, [spl133_92])])).
fof(f2776, plain, (~ cAgamidae(sK95) | ~ spl133_92), inference(resolution, [], [f1259, f1834])).
fof(f1834, plain, ! [X6] : (~ cEmydidae(X6) | ~ cAgamidae(X6)), inference(subsumption_resolution, [], [f1821, f616])).
fof(f616, plain, ~ (xsd_string_0 = xsd_string_6), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ~ (xsd_string_0 = xsd_string_6), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_44)).
fof(f1821, plain, ! [X6] : ((xsd_string_0 = xsd_string_6) | ~ cAgamidae(X6) | ~ cEmydidae(X6)), inference(resolution, [], [f1803, f592])).
fof(f1259, plain, (cEmydidae(sK95) | ~ spl133_92), inference(avatar_component_clause, [], [f1257])).
fof(f2774, plain, (~ spl133_50 | ~ spl133_51), inference(avatar_contradiction_clause, [], [f2773])).
fof(f2773, plain, ($false | (~ spl133_50 | ~ spl133_51)), inference(subsumption_resolution, [], [f2771, f1063])).
fof(f1063, plain, (cAgamidae(sK81) | ~ spl133_50), inference(avatar_component_clause, [], [f1061])).
fof(f1061, plain, (spl133_50 <=> cAgamidae(sK81)), introduced(avatar_definition, [new_symbols(naming, [spl133_50])])).
fof(f2771, plain, (~ cAgamidae(sK81) | ~ spl133_51), inference(resolution, [], [f1068, f1830])).
fof(f1830, plain, ! [X2] : (~ cAnomalepidae(X2) | ~ cAgamidae(X2)), inference(subsumption_resolution, [], [f1817, f612])).
fof(f612, plain, ~ (xsd_string_0 = xsd_string_2), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ~ (xsd_string_0 = xsd_string_2), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_40)).
fof(f1817, plain, ! [X2] : ((xsd_string_0 = xsd_string_2) | ~ cAgamidae(X2) | ~ cAnomalepidae(X2)), inference(resolution, [], [f1803, f580])).
fof(f1068, plain, (cAnomalepidae(sK81) | ~ spl133_51), inference(avatar_component_clause, [], [f1066])).
fof(f1066, plain, (spl133_51 <=> cAnomalepidae(sK81)), introduced(avatar_definition, [new_symbols(naming, [spl133_51])])).
fof(f2768, plain, (~ spl133_179 | ~ spl133_180), inference(avatar_contradiction_clause, [], [f2767])).
fof(f2767, plain, ($false | (~ spl133_179 | ~ spl133_180)), inference(subsumption_resolution, [], [f2758, f1670])).
fof(f1670, plain, (cAmphisbaenidae(sK124) | ~ spl133_180), inference(avatar_component_clause, [], [f1668])).
fof(f1668, plain, (spl133_180 <=> cAmphisbaenidae(sK124)), introduced(avatar_definition, [new_symbols(naming, [spl133_180])])).
fof(f2758, plain, (~ cAmphisbaenidae(sK124) | ~ spl133_179), inference(resolution, [], [f1665, f1863])).
fof(f1863, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cAmphisbaenidae(X10)), inference(subsumption_resolution, [], [f1851, f630])).
fof(f630, plain, ~ (xsd_string_1 = xsd_string_10), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ~ (xsd_string_1 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_58)).
fof(f1851, plain, ! [X10] : ((xsd_string_1 = xsd_string_10) | ~ cAmphisbaenidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1804, f606])).
fof(f1665, plain, (cSphenodontidae(sK124) | ~ spl133_179), inference(avatar_component_clause, [], [f1663])).
fof(f1663, plain, (spl133_179 <=> cSphenodontidae(sK124)), introduced(avatar_definition, [new_symbols(naming, [spl133_179])])).
fof(f2744, plain, (~ spl133_26 | ~ spl133_27), inference(avatar_contradiction_clause, [], [f2743])).
fof(f2743, plain, ($false | (~ spl133_26 | ~ spl133_27)), inference(subsumption_resolution, [], [f2736, f956])).
fof(f956, plain, (cAnomalepidae(sK73) | ~ spl133_27), inference(avatar_component_clause, [], [f954])).
fof(f954, plain, (spl133_27 <=> cAnomalepidae(sK73)), introduced(avatar_definition, [new_symbols(naming, [spl133_27])])).
fof(f2736, plain, (~ cAnomalepidae(sK73) | ~ spl133_26), inference(resolution, [], [f951, f1886])).
fof(f1886, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cAnomalepidae(X9)), inference(subsumption_resolution, [], [f1875, f638])).
fof(f638, plain, ~ (xsd_string_2 = xsd_string_9), inference(cnf_transformation, [], [f86])).
fof(f86, plain, ~ (xsd_string_2 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_66)).
fof(f1875, plain, ! [X9] : ((xsd_string_2 = xsd_string_9) | ~ cAnomalepidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1805, f601])).
fof(f951, plain, (cLoxocemidae(sK73) | ~ spl133_26), inference(avatar_component_clause, [], [f949])).
fof(f949, plain, (spl133_26 <=> cLoxocemidae(sK73)), introduced(avatar_definition, [new_symbols(naming, [spl133_26])])).
fof(f2732, plain, (~ spl133_119 | ~ spl133_120), inference(avatar_split_clause, [], [f2728, f1388, f1383])).
fof(f1383, plain, (spl133_119 <=> cAgamidae(sK104)), introduced(avatar_definition, [new_symbols(naming, [spl133_119])])).
fof(f1388, plain, (spl133_120 <=> cBipedidae(sK104)), introduced(avatar_definition, [new_symbols(naming, [spl133_120])])).
fof(f2728, plain, (~ cAgamidae(sK104) | ~ spl133_120), inference(resolution, [], [f1390, f1831])).
fof(f1831, plain, ! [X3] : (~ cBipedidae(X3) | ~ cAgamidae(X3)), inference(subsumption_resolution, [], [f1818, f613])).
fof(f613, plain, ~ (xsd_string_0 = xsd_string_3), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ~ (xsd_string_0 = xsd_string_3), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_41)).
fof(f1818, plain, ! [X3] : ((xsd_string_0 = xsd_string_3) | ~ cAgamidae(X3) | ~ cBipedidae(X3)), inference(resolution, [], [f1803, f583])).
fof(f1390, plain, (cBipedidae(sK104) | ~ spl133_120), inference(avatar_component_clause, [], [f1388])).
fof(f2716, plain, (~ spl133_29 | ~ spl133_30), inference(avatar_contradiction_clause, [], [f2715])).
fof(f2715, plain, ($false | (~ spl133_29 | ~ spl133_30)), inference(subsumption_resolution, [], [f2713, f970])).
fof(f970, plain, (cGekkonidae(sK74) | ~ spl133_30), inference(avatar_component_clause, [], [f968])).
fof(f968, plain, (spl133_30 <=> cGekkonidae(sK74)), introduced(avatar_definition, [new_symbols(naming, [spl133_30])])).
fof(f2713, plain, (~ cGekkonidae(sK74) | ~ spl133_29), inference(resolution, [], [f965, f1996])).
fof(f1996, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cGekkonidae(X9)), inference(subsumption_resolution, [], [f1990, f668])).
fof(f668, plain, ~ (xsd_string_7 = xsd_string_9), inference(cnf_transformation, [], [f116])).
fof(f116, plain, ~ (xsd_string_7 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_96)).
fof(f1990, plain, ! [X9] : ((xsd_string_7 = xsd_string_9) | ~ cGekkonidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1810, f601])).
fof(f965, plain, (cLoxocemidae(sK74) | ~ spl133_29), inference(avatar_component_clause, [], [f963])).
fof(f963, plain, (spl133_29 <=> cLoxocemidae(sK74)), introduced(avatar_definition, [new_symbols(naming, [spl133_29])])).
fof(f2704, plain, (~ spl133_167 | ~ spl133_168), inference(avatar_split_clause, [], [f2700, f1612, f1607])).
fof(f1607, plain, (spl133_167 <=> cCrocodylidae(sK120)), introduced(avatar_definition, [new_symbols(naming, [spl133_167])])).
fof(f1612, plain, (spl133_168 <=> cGekkonidae(sK120)), introduced(avatar_definition, [new_symbols(naming, [spl133_168])])).
fof(f2700, plain, (~ cCrocodylidae(sK120) | ~ spl133_168), inference(resolution, [], [f1614, f1955])).
fof(f1955, plain, ! [X7] : (~ cGekkonidae(X7) | ~ cCrocodylidae(X7)), inference(subsumption_resolution, [], [f1947, f657])).
fof(f657, plain, ~ (xsd_string_5 = xsd_string_7), inference(cnf_transformation, [], [f105])).
fof(f105, plain, ~ (xsd_string_5 = xsd_string_7), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_85)).
fof(f1947, plain, ! [X7] : ((xsd_string_5 = xsd_string_7) | ~ cCrocodylidae(X7) | ~ cGekkonidae(X7)), inference(resolution, [], [f1808, f595])).
fof(f1614, plain, (cGekkonidae(sK120) | ~ spl133_168), inference(avatar_component_clause, [], [f1612])).
fof(f2687, plain, (~ spl133_125 | ~ spl133_126), inference(avatar_contradiction_clause, [], [f2686])).
fof(f2686, plain, ($false | (~ spl133_125 | ~ spl133_126)), inference(subsumption_resolution, [], [f2681, f1413])).
fof(f1413, plain, (cEmydidae(sK106) | ~ spl133_125), inference(avatar_component_clause, [], [f1411])).
fof(f1411, plain, (spl133_125 <=> cEmydidae(sK106)), introduced(avatar_definition, [new_symbols(naming, [spl133_125])])).
fof(f2681, plain, (~ cEmydidae(sK106) | ~ spl133_126), inference(resolution, [], [f1418, f1979])).
fof(f1979, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cEmydidae(X11)), inference(subsumption_resolution, [], [f1972, f666])).
fof(f666, plain, ~ (xsd_string_6 = xsd_string_11), inference(cnf_transformation, [], [f114])).
fof(f114, plain, ~ (xsd_string_6 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_94)).
fof(f1972, plain, ! [X11] : ((xsd_string_6 = xsd_string_11) | ~ cEmydidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1809, f609])).
fof(f1418, plain, (cXantusiidae(sK106) | ~ spl133_126), inference(avatar_component_clause, [], [f1416])).
fof(f1416, plain, (spl133_126 <=> cXantusiidae(sK106)), introduced(avatar_definition, [new_symbols(naming, [spl133_126])])).
fof(f2666, plain, (~ spl133_170 | ~ spl133_171), inference(avatar_contradiction_clause, [], [f2665])).
fof(f2665, plain, ($false | (~ spl133_170 | ~ spl133_171)), inference(subsumption_resolution, [], [f2658, f1628])).
fof(f1628, plain, (cBipedidae(sK121) | ~ spl133_171), inference(avatar_component_clause, [], [f1626])).
fof(f1626, plain, (spl133_171 <=> cBipedidae(sK121)), introduced(avatar_definition, [new_symbols(naming, [spl133_171])])).
fof(f2658, plain, (~ cBipedidae(sK121) | ~ spl133_170), inference(resolution, [], [f1623, f1910])).
fof(f1910, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cBipedidae(X10)), inference(subsumption_resolution, [], [f1900, f647])).
fof(f647, plain, ~ (xsd_string_3 = xsd_string_10), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ~ (xsd_string_3 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_75)).
fof(f1900, plain, ! [X10] : ((xsd_string_3 = xsd_string_10) | ~ cBipedidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1806, f606])).
fof(f1623, plain, (cSphenodontidae(sK121) | ~ spl133_170), inference(avatar_component_clause, [], [f1621])).
fof(f1621, plain, (spl133_170 <=> cSphenodontidae(sK121)), introduced(avatar_definition, [new_symbols(naming, [spl133_170])])).
fof(f2645, plain, (~ spl133_68 | ~ spl133_69), inference(avatar_split_clause, [], [f2639, f1150, f1145])).
fof(f1145, plain, (spl133_68 <=> cEmydidae(sK87)), introduced(avatar_definition, [new_symbols(naming, [spl133_68])])).
fof(f1150, plain, (spl133_69 <=> cSphenodontidae(sK87)), introduced(avatar_definition, [new_symbols(naming, [spl133_69])])).
fof(f2639, plain, (~ cEmydidae(sK87) | ~ spl133_69), inference(resolution, [], [f1152, f1978])).
fof(f1978, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cEmydidae(X10)), inference(subsumption_resolution, [], [f1971, f665])).
fof(f665, plain, ~ (xsd_string_6 = xsd_string_10), inference(cnf_transformation, [], [f113])).
fof(f113, plain, ~ (xsd_string_6 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_93)).
fof(f1971, plain, ! [X10] : ((xsd_string_6 = xsd_string_10) | ~ cEmydidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1809, f606])).
fof(f1152, plain, (cSphenodontidae(sK87) | ~ spl133_69), inference(avatar_component_clause, [], [f1150])).
fof(f2624, plain, (~ spl133_162 | ~ spl133_161), inference(avatar_split_clause, [], [f2612, f1579, f1584])).
fof(f1584, plain, (spl133_162 <=> cAgamidae(sK118)), introduced(avatar_definition, [new_symbols(naming, [spl133_162])])).
fof(f1579, plain, (spl133_161 <=> cSphenodontidae(sK118)), introduced(avatar_definition, [new_symbols(naming, [spl133_161])])).
fof(f2612, plain, (~ cAgamidae(sK118) | ~ spl133_161), inference(resolution, [], [f1581, f1838])).
fof(f1838, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cAgamidae(X10)), inference(subsumption_resolution, [], [f1825, f620])).
fof(f620, plain, ~ (xsd_string_0 = xsd_string_10), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ~ (xsd_string_0 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_48)).
fof(f1825, plain, ! [X10] : ((xsd_string_0 = xsd_string_10) | ~ cAgamidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1803, f606])).
fof(f1581, plain, (cSphenodontidae(sK118) | ~ spl133_161), inference(avatar_component_clause, [], [f1579])).
fof(f2606, plain, (~ spl133_86 | ~ spl133_87), inference(avatar_contradiction_clause, [], [f2605])).
fof(f2605, plain, ($false | (~ spl133_86 | ~ spl133_87)), inference(subsumption_resolution, [], [f2596, f1236])).
fof(f1236, plain, (cAgamidae(sK93) | ~ spl133_87), inference(avatar_component_clause, [], [f1234])).
fof(f1234, plain, (spl133_87 <=> cAgamidae(sK93)), introduced(avatar_definition, [new_symbols(naming, [spl133_87])])).
fof(f2596, plain, (~ cAgamidae(sK93) | ~ spl133_86), inference(resolution, [], [f1231, f1837])).
fof(f1837, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cAgamidae(X9)), inference(subsumption_resolution, [], [f1824, f619])).
fof(f619, plain, ~ (xsd_string_0 = xsd_string_9), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ~ (xsd_string_0 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_47)).
fof(f1824, plain, ! [X9] : ((xsd_string_0 = xsd_string_9) | ~ cAgamidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1803, f601])).
fof(f1231, plain, (cLoxocemidae(sK93) | ~ spl133_86), inference(avatar_component_clause, [], [f1229])).
fof(f1229, plain, (spl133_86 <=> cLoxocemidae(sK93)), introduced(avatar_definition, [new_symbols(naming, [spl133_86])])).
fof(f2594, plain, (~ spl133_80 | ~ spl133_81), inference(avatar_split_clause, [], [f2584, f1206, f1201])).
fof(f1201, plain, (spl133_80 <=> cBipedidae(sK91)), introduced(avatar_definition, [new_symbols(naming, [spl133_80])])).
fof(f1206, plain, (spl133_81 <=> cXantusiidae(sK91)), introduced(avatar_definition, [new_symbols(naming, [spl133_81])])).
fof(f2584, plain, (~ cBipedidae(sK91) | ~ spl133_81), inference(resolution, [], [f1208, f1911])).
fof(f1911, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cBipedidae(X11)), inference(subsumption_resolution, [], [f1901, f648])).
fof(f648, plain, ~ (xsd_string_3 = xsd_string_11), inference(cnf_transformation, [], [f96])).
fof(f96, plain, ~ (xsd_string_3 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_76)).
fof(f1901, plain, ! [X11] : ((xsd_string_3 = xsd_string_11) | ~ cBipedidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1806, f609])).
fof(f1208, plain, (cXantusiidae(sK91) | ~ spl133_81), inference(avatar_component_clause, [], [f1206])).
fof(f2575, plain, (~ spl133_158 | ~ spl133_159), inference(avatar_contradiction_clause, [], [f2574])).
fof(f2574, plain, ($false | (~ spl133_158 | ~ spl133_159)), inference(subsumption_resolution, [], [f2570, f1572])).
fof(f1572, plain, (cAnomalepidae(sK117) | ~ spl133_159), inference(avatar_component_clause, [], [f1570])).
fof(f1570, plain, (spl133_159 <=> cAnomalepidae(sK117)), introduced(avatar_definition, [new_symbols(naming, [spl133_159])])).
fof(f2570, plain, (~ cAnomalepidae(sK117) | ~ spl133_158), inference(resolution, [], [f1567, f1882])).
fof(f1882, plain, ! [X5] : (~ cCrocodylidae(X5) | ~ cAnomalepidae(X5)), inference(subsumption_resolution, [], [f1871, f634])).
fof(f634, plain, ~ (xsd_string_2 = xsd_string_5), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ~ (xsd_string_2 = xsd_string_5), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_62)).
fof(f1871, plain, ! [X5] : ((xsd_string_2 = xsd_string_5) | ~ cAnomalepidae(X5) | ~ cCrocodylidae(X5)), inference(resolution, [], [f1805, f589])).
fof(f1567, plain, (cCrocodylidae(sK117) | ~ spl133_158), inference(avatar_component_clause, [], [f1565])).
fof(f1565, plain, (spl133_158 <=> cCrocodylidae(sK117)), introduced(avatar_definition, [new_symbols(naming, [spl133_158])])).
fof(f2567, plain, (~ spl133_156 | ~ spl133_155), inference(avatar_split_clause, [], [f2564, f1551, f1556])).
fof(f1556, plain, (spl133_156 <=> cCrocodylidae(sK116)), introduced(avatar_definition, [new_symbols(naming, [spl133_156])])).
fof(f1551, plain, (spl133_155 <=> cEmydidae(sK116)), introduced(avatar_definition, [new_symbols(naming, [spl133_155])])).
fof(f2564, plain, (~ cCrocodylidae(sK116) | ~ spl133_155), inference(resolution, [], [f1553, f1954])).
fof(f1954, plain, ! [X6] : (~ cEmydidae(X6) | ~ cCrocodylidae(X6)), inference(subsumption_resolution, [], [f1946, f656])).
fof(f656, plain, ~ (xsd_string_5 = xsd_string_6), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ~ (xsd_string_5 = xsd_string_6), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_84)).
fof(f1946, plain, ! [X6] : ((xsd_string_5 = xsd_string_6) | ~ cCrocodylidae(X6) | ~ cEmydidae(X6)), inference(resolution, [], [f1808, f592])).
fof(f1553, plain, (cEmydidae(sK116) | ~ spl133_155), inference(avatar_component_clause, [], [f1551])).
fof(f2552, plain, (~ spl133_32 | ~ spl133_33), inference(avatar_contradiction_clause, [], [f2551])).
fof(f2551, plain, ($false | (~ spl133_32 | ~ spl133_33)), inference(subsumption_resolution, [], [f2548, f984])).
fof(f984, plain, (cAmphisbaenidae(sK75) | ~ spl133_33), inference(avatar_component_clause, [], [f982])).
fof(f982, plain, (spl133_33 <=> cAmphisbaenidae(sK75)), introduced(avatar_definition, [new_symbols(naming, [spl133_33])])).
fof(f2548, plain, (~ cAmphisbaenidae(sK75) | ~ spl133_32), inference(resolution, [], [f979, f1855])).
fof(f1855, plain, ! [X2] : (~ cAnomalepidae(X2) | ~ cAmphisbaenidae(X2)), inference(subsumption_resolution, [], [f1843, f622])).
fof(f622, plain, ~ (xsd_string_1 = xsd_string_2), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ~ (xsd_string_1 = xsd_string_2), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_50)).
fof(f1843, plain, ! [X2] : ((xsd_string_1 = xsd_string_2) | ~ cAmphisbaenidae(X2) | ~ cAnomalepidae(X2)), inference(resolution, [], [f1804, f580])).
fof(f979, plain, (cAnomalepidae(sK75) | ~ spl133_32), inference(avatar_component_clause, [], [f977])).
fof(f977, plain, (spl133_32 <=> cAnomalepidae(sK75)), introduced(avatar_definition, [new_symbols(naming, [spl133_32])])).
fof(f2547, plain, (~ spl133_77 | ~ spl133_78), inference(avatar_contradiction_clause, [], [f2546])).
fof(f2546, plain, ($false | (~ spl133_77 | ~ spl133_78)), inference(subsumption_resolution, [], [f2542, f1194])).
fof(f1194, plain, (cAnomalepidae(sK90) | ~ spl133_78), inference(avatar_component_clause, [], [f1192])).
fof(f1192, plain, (spl133_78 <=> cAnomalepidae(sK90)), introduced(avatar_definition, [new_symbols(naming, [spl133_78])])).
fof(f2542, plain, (~ cAnomalepidae(sK90) | ~ spl133_77), inference(resolution, [], [f1189, f1883])).
fof(f1883, plain, ! [X6] : (~ cEmydidae(X6) | ~ cAnomalepidae(X6)), inference(subsumption_resolution, [], [f1872, f635])).
fof(f635, plain, ~ (xsd_string_2 = xsd_string_6), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ~ (xsd_string_2 = xsd_string_6), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_63)).
fof(f1872, plain, ! [X6] : ((xsd_string_2 = xsd_string_6) | ~ cAnomalepidae(X6) | ~ cEmydidae(X6)), inference(resolution, [], [f1805, f592])).
fof(f1189, plain, (cEmydidae(sK90) | ~ spl133_77), inference(avatar_component_clause, [], [f1187])).
fof(f1187, plain, (spl133_77 <=> cEmydidae(sK90)), introduced(avatar_definition, [new_symbols(naming, [spl133_77])])).
fof(f2538, plain, (~ spl133_101 | ~ spl133_102), inference(avatar_contradiction_clause, [], [f2537])).
fof(f2537, plain, ($false | (~ spl133_101 | ~ spl133_102)), inference(subsumption_resolution, [], [f2534, f1301])).
fof(f1301, plain, (cCordylidae(sK98) | ~ spl133_101), inference(avatar_component_clause, [], [f1299])).
fof(f1299, plain, (spl133_101 <=> cCordylidae(sK98)), introduced(avatar_definition, [new_symbols(naming, [spl133_101])])).
fof(f2534, plain, (~ cCordylidae(sK98) | ~ spl133_102), inference(resolution, [], [f1306, f1934])).
fof(f1934, plain, ! [X7] : (~ cGekkonidae(X7) | ~ cCordylidae(X7)), inference(subsumption_resolution, [], [f1925, f651])).
fof(f651, plain, ~ (xsd_string_4 = xsd_string_7), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ~ (xsd_string_4 = xsd_string_7), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_79)).
fof(f1925, plain, ! [X7] : ((xsd_string_4 = xsd_string_7) | ~ cCordylidae(X7) | ~ cGekkonidae(X7)), inference(resolution, [], [f1807, f595])).
fof(f1306, plain, (cGekkonidae(sK98) | ~ spl133_102), inference(avatar_component_clause, [], [f1304])).
fof(f1304, plain, (spl133_102 <=> cGekkonidae(sK98)), introduced(avatar_definition, [new_symbols(naming, [spl133_102])])).
fof(f2523, plain, (~ spl133_116 | ~ spl133_117), inference(avatar_contradiction_clause, [], [f2522])).
fof(f2522, plain, ($false | (~ spl133_116 | ~ spl133_117)), inference(subsumption_resolution, [], [f2516, f1371])).
fof(f1371, plain, (cAmphisbaenidae(sK103) | ~ spl133_116), inference(avatar_component_clause, [], [f1369])).
fof(f1369, plain, (spl133_116 <=> cAmphisbaenidae(sK103)), introduced(avatar_definition, [new_symbols(naming, [spl133_116])])).
fof(f2516, plain, (~ cAmphisbaenidae(sK103) | ~ spl133_117), inference(resolution, [], [f1376, f1860])).
fof(f1860, plain, ! [X7] : (~ cGekkonidae(X7) | ~ cAmphisbaenidae(X7)), inference(subsumption_resolution, [], [f1848, f627])).
fof(f627, plain, ~ (xsd_string_1 = xsd_string_7), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ~ (xsd_string_1 = xsd_string_7), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_55)).
fof(f1848, plain, ! [X7] : ((xsd_string_1 = xsd_string_7) | ~ cAmphisbaenidae(X7) | ~ cGekkonidae(X7)), inference(resolution, [], [f1804, f595])).
fof(f1376, plain, (cGekkonidae(sK103) | ~ spl133_117), inference(avatar_component_clause, [], [f1374])).
fof(f1374, plain, (spl133_117 <=> cGekkonidae(sK103)), introduced(avatar_definition, [new_symbols(naming, [spl133_117])])).
fof(f2511, plain, (~ spl133_113 | ~ spl133_114), inference(avatar_contradiction_clause, [], [f2510])).
fof(f2510, plain, ($false | (~ spl133_113 | ~ spl133_114)), inference(subsumption_resolution, [], [f2507, f1357])).
fof(f1357, plain, (cCrocodylidae(sK102) | ~ spl133_113), inference(avatar_component_clause, [], [f1355])).
fof(f1355, plain, (spl133_113 <=> cCrocodylidae(sK102)), introduced(avatar_definition, [new_symbols(naming, [spl133_113])])).
fof(f2507, plain, (~ cCrocodylidae(sK102) | ~ spl133_114), inference(resolution, [], [f1362, f1956])).
fof(f1956, plain, ! [X8] : (~ cLeptotyphlopidae(X8) | ~ cCrocodylidae(X8)), inference(subsumption_resolution, [], [f1948, f658])).
fof(f658, plain, ~ (xsd_string_5 = xsd_string_8), inference(cnf_transformation, [], [f106])).
fof(f106, plain, ~ (xsd_string_5 = xsd_string_8), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_86)).
fof(f1948, plain, ! [X8] : ((xsd_string_5 = xsd_string_8) | ~ cCrocodylidae(X8) | ~ cLeptotyphlopidae(X8)), inference(resolution, [], [f1808, f598])).
fof(f1362, plain, (cLeptotyphlopidae(sK102) | ~ spl133_114), inference(avatar_component_clause, [], [f1360])).
fof(f1360, plain, (spl133_114 <=> cLeptotyphlopidae(sK102)), introduced(avatar_definition, [new_symbols(naming, [spl133_114])])).
fof(f2494, plain, (~ spl133_149 | ~ spl133_150), inference(avatar_contradiction_clause, [], [f2493])).
fof(f2493, plain, ($false | (~ spl133_149 | ~ spl133_150)), inference(subsumption_resolution, [], [f2485, f1525])).
fof(f1525, plain, (cAgamidae(sK114) | ~ spl133_149), inference(avatar_component_clause, [], [f1523])).
fof(f1523, plain, (spl133_149 <=> cAgamidae(sK114)), introduced(avatar_definition, [new_symbols(naming, [spl133_149])])).
fof(f2485, plain, (~ cAgamidae(sK114) | ~ spl133_150), inference(resolution, [], [f1530, f1836])).
fof(f1836, plain, ! [X8] : (~ cLeptotyphlopidae(X8) | ~ cAgamidae(X8)), inference(subsumption_resolution, [], [f1823, f618])).
fof(f618, plain, ~ (xsd_string_0 = xsd_string_8), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ~ (xsd_string_0 = xsd_string_8), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_46)).
fof(f1823, plain, ! [X8] : ((xsd_string_0 = xsd_string_8) | ~ cAgamidae(X8) | ~ cLeptotyphlopidae(X8)), inference(resolution, [], [f1803, f598])).
fof(f1530, plain, (cLeptotyphlopidae(sK114) | ~ spl133_150), inference(avatar_component_clause, [], [f1528])).
fof(f1528, plain, (spl133_150 <=> cLeptotyphlopidae(sK114)), introduced(avatar_definition, [new_symbols(naming, [spl133_150])])).
fof(f2482, plain, (~ spl133_140 | ~ spl133_141), inference(avatar_contradiction_clause, [], [f2481])).
fof(f2481, plain, ($false | (~ spl133_140 | ~ spl133_141)), inference(subsumption_resolution, [], [f2475, f1483])).
fof(f1483, plain, (cCrocodylidae(sK111) | ~ spl133_140), inference(avatar_component_clause, [], [f1481])).
fof(f1481, plain, (spl133_140 <=> cCrocodylidae(sK111)), introduced(avatar_definition, [new_symbols(naming, [spl133_140])])).
fof(f2475, plain, (~ cCrocodylidae(sK111) | ~ spl133_141), inference(resolution, [], [f1488, f1959])).
fof(f1959, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cCrocodylidae(X11)), inference(subsumption_resolution, [], [f1951, f661])).
fof(f661, plain, ~ (xsd_string_5 = xsd_string_11), inference(cnf_transformation, [], [f109])).
fof(f109, plain, ~ (xsd_string_5 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_89)).
fof(f1951, plain, ! [X11] : ((xsd_string_5 = xsd_string_11) | ~ cCrocodylidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1808, f609])).
fof(f1488, plain, (cXantusiidae(sK111) | ~ spl133_141), inference(avatar_component_clause, [], [f1486])).
fof(f1486, plain, (spl133_141 <=> cXantusiidae(sK111)), introduced(avatar_definition, [new_symbols(naming, [spl133_141])])).
fof(f2462, plain, (~ spl133_44 | ~ spl133_45), inference(avatar_split_clause, [], [f2450, f1038, f1033])).
fof(f1033, plain, (spl133_44 <=> cAmphisbaenidae(sK79)), introduced(avatar_definition, [new_symbols(naming, [spl133_44])])).
fof(f1038, plain, (spl133_45 <=> cXantusiidae(sK79)), introduced(avatar_definition, [new_symbols(naming, [spl133_45])])).
fof(f2450, plain, (~ cAmphisbaenidae(sK79) | ~ spl133_45), inference(resolution, [], [f1040, f1864])).
fof(f1864, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cAmphisbaenidae(X11)), inference(subsumption_resolution, [], [f1852, f631])).
fof(f631, plain, ~ (xsd_string_1 = xsd_string_11), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ~ (xsd_string_1 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_59)).
fof(f1852, plain, ! [X11] : ((xsd_string_1 = xsd_string_11) | ~ cAmphisbaenidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1804, f609])).
fof(f1040, plain, (cXantusiidae(sK79) | ~ spl133_45), inference(avatar_component_clause, [], [f1038])).
fof(f2445, plain, (~ spl133_164 | ~ spl133_165), inference(avatar_contradiction_clause, [], [f2444])).
fof(f2444, plain, ($false | (~ spl133_164 | ~ spl133_165)), inference(subsumption_resolution, [], [f2441, f1600])).
fof(f1600, plain, (cGekkonidae(sK119) | ~ spl133_165), inference(avatar_component_clause, [], [f1598])).
fof(f1598, plain, (spl133_165 <=> cGekkonidae(sK119)), introduced(avatar_definition, [new_symbols(naming, [spl133_165])])).
fof(f2441, plain, (~ cGekkonidae(sK119) | ~ spl133_164), inference(resolution, [], [f1595, f1997])).
fof(f1997, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cGekkonidae(X10)), inference(subsumption_resolution, [], [f1991, f669])).
fof(f669, plain, ~ (xsd_string_7 = xsd_string_10), inference(cnf_transformation, [], [f117])).
fof(f117, plain, ~ (xsd_string_7 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_97)).
fof(f1991, plain, ! [X10] : ((xsd_string_7 = xsd_string_10) | ~ cGekkonidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1810, f606])).
fof(f1595, plain, (cSphenodontidae(sK119) | ~ spl133_164), inference(avatar_component_clause, [], [f1593])).
fof(f1593, plain, (spl133_164 <=> cSphenodontidae(sK119)), introduced(avatar_definition, [new_symbols(naming, [spl133_164])])).
fof(f2432, plain, (~ spl133_147 | ~ spl133_146), inference(avatar_split_clause, [], [f2427, f1509, f1514])).
fof(f1514, plain, (spl133_147 <=> cAmphisbaenidae(sK113)), introduced(avatar_definition, [new_symbols(naming, [spl133_147])])).
fof(f1509, plain, (spl133_146 <=> cCrocodylidae(sK113)), introduced(avatar_definition, [new_symbols(naming, [spl133_146])])).
fof(f2427, plain, (~ cAmphisbaenidae(sK113) | ~ spl133_146), inference(resolution, [], [f1511, f1858])).
fof(f1858, plain, ! [X5] : (~ cCrocodylidae(X5) | ~ cAmphisbaenidae(X5)), inference(subsumption_resolution, [], [f1846, f625])).
fof(f625, plain, ~ (xsd_string_1 = xsd_string_5), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ~ (xsd_string_1 = xsd_string_5), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_53)).
fof(f1846, plain, ! [X5] : ((xsd_string_1 = xsd_string_5) | ~ cAmphisbaenidae(X5) | ~ cCrocodylidae(X5)), inference(resolution, [], [f1804, f589])).
fof(f1511, plain, (cCrocodylidae(sK113) | ~ spl133_146), inference(avatar_component_clause, [], [f1509])).
fof(f2423, plain, (~ spl133_176 | ~ spl133_177), inference(avatar_contradiction_clause, [], [f2422])).
fof(f2422, plain, ($false | (~ spl133_176 | ~ spl133_177)), inference(subsumption_resolution, [], [f2417, f1656])).
fof(f1656, plain, (cBipedidae(sK123) | ~ spl133_177), inference(avatar_component_clause, [], [f1654])).
fof(f1654, plain, (spl133_177 <=> cBipedidae(sK123)), introduced(avatar_definition, [new_symbols(naming, [spl133_177])])).
fof(f2417, plain, (~ cBipedidae(sK123) | ~ spl133_176), inference(resolution, [], [f1651, f1905])).
fof(f1905, plain, ! [X5] : (~ cCrocodylidae(X5) | ~ cBipedidae(X5)), inference(subsumption_resolution, [], [f1895, f642])).
fof(f642, plain, ~ (xsd_string_3 = xsd_string_5), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ~ (xsd_string_3 = xsd_string_5), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_70)).
fof(f1895, plain, ! [X5] : ((xsd_string_3 = xsd_string_5) | ~ cBipedidae(X5) | ~ cCrocodylidae(X5)), inference(resolution, [], [f1806, f589])).
fof(f1651, plain, (cCrocodylidae(sK123) | ~ spl133_176), inference(avatar_component_clause, [], [f1649])).
fof(f1649, plain, (spl133_176 <=> cCrocodylidae(sK123)), introduced(avatar_definition, [new_symbols(naming, [spl133_176])])).
fof(f2406, plain, (~ spl133_95 | ~ spl133_96), inference(avatar_contradiction_clause, [], [f2405])).
fof(f2405, plain, ($false | (~ spl133_95 | ~ spl133_96)), inference(subsumption_resolution, [], [f2401, f1278])).
fof(f1278, plain, (cAnomalepidae(sK96) | ~ spl133_96), inference(avatar_component_clause, [], [f1276])).
fof(f1276, plain, (spl133_96 <=> cAnomalepidae(sK96)), introduced(avatar_definition, [new_symbols(naming, [spl133_96])])).
fof(f2401, plain, (~ cAnomalepidae(sK96) | ~ spl133_95), inference(resolution, [], [f1273, f1881])).
fof(f1881, plain, ! [X4] : (~ cCordylidae(X4) | ~ cAnomalepidae(X4)), inference(subsumption_resolution, [], [f1870, f633])).
fof(f633, plain, ~ (xsd_string_2 = xsd_string_4), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ~ (xsd_string_2 = xsd_string_4), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_61)).
fof(f1870, plain, ! [X4] : ((xsd_string_2 = xsd_string_4) | ~ cAnomalepidae(X4) | ~ cCordylidae(X4)), inference(resolution, [], [f1805, f586])).
fof(f1273, plain, (cCordylidae(sK96) | ~ spl133_95), inference(avatar_component_clause, [], [f1271])).
fof(f1271, plain, (spl133_95 <=> cCordylidae(sK96)), introduced(avatar_definition, [new_symbols(naming, [spl133_95])])).
fof(f2399, plain, (~ spl133_53 | ~ spl133_54), inference(avatar_split_clause, [], [f2390, f1080, f1075])).
fof(f1075, plain, (spl133_53 <=> cCordylidae(sK82)), introduced(avatar_definition, [new_symbols(naming, [spl133_53])])).
fof(f1080, plain, (spl133_54 <=> cXantusiidae(sK82)), introduced(avatar_definition, [new_symbols(naming, [spl133_54])])).
fof(f2390, plain, (~ cCordylidae(sK82) | ~ spl133_54), inference(resolution, [], [f1082, f1938])).
fof(f1938, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cCordylidae(X11)), inference(subsumption_resolution, [], [f1929, f655])).
fof(f655, plain, ~ (xsd_string_4 = xsd_string_11), inference(cnf_transformation, [], [f103])).
fof(f103, plain, ~ (xsd_string_4 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_83)).
fof(f1929, plain, ! [X11] : ((xsd_string_4 = xsd_string_11) | ~ cCordylidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1807, f609])).
fof(f1082, plain, (cXantusiidae(sK82) | ~ spl133_54), inference(avatar_component_clause, [], [f1080])).
fof(f2379, plain, (~ spl133_143 | ~ spl133_144), inference(avatar_contradiction_clause, [], [f2378])).
fof(f2378, plain, ($false | (~ spl133_143 | ~ spl133_144)), inference(subsumption_resolution, [], [f2374, f1502])).
fof(f1502, plain, (cCrocodylidae(sK112) | ~ spl133_144), inference(avatar_component_clause, [], [f1500])).
fof(f1500, plain, (spl133_144 <=> cCrocodylidae(sK112)), introduced(avatar_definition, [new_symbols(naming, [spl133_144])])).
fof(f2374, plain, (~ cCrocodylidae(sK112) | ~ spl133_143), inference(resolution, [], [f1497, f1957])).
fof(f1957, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cCrocodylidae(X9)), inference(subsumption_resolution, [], [f1949, f659])).
fof(f659, plain, ~ (xsd_string_5 = xsd_string_9), inference(cnf_transformation, [], [f107])).
fof(f107, plain, ~ (xsd_string_5 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_87)).
fof(f1949, plain, ! [X9] : ((xsd_string_5 = xsd_string_9) | ~ cCrocodylidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1808, f601])).
fof(f1497, plain, (cLoxocemidae(sK112) | ~ spl133_143), inference(avatar_component_clause, [], [f1495])).
fof(f1495, plain, (spl133_143 <=> cLoxocemidae(sK112)), introduced(avatar_definition, [new_symbols(naming, [spl133_143])])).
fof(f2367, plain, (~ spl133_174 | ~ spl133_173), inference(avatar_split_clause, [], [f2361, f1635, f1640])).
fof(f1640, plain, (spl133_174 <=> cBipedidae(sK122)), introduced(avatar_definition, [new_symbols(naming, [spl133_174])])).
fof(f1635, plain, (spl133_173 <=> cGekkonidae(sK122)), introduced(avatar_definition, [new_symbols(naming, [spl133_173])])).
fof(f2361, plain, (~ cBipedidae(sK122) | ~ spl133_173), inference(resolution, [], [f1637, f1907])).
fof(f1907, plain, ! [X7] : (~ cGekkonidae(X7) | ~ cBipedidae(X7)), inference(subsumption_resolution, [], [f1897, f644])).
fof(f644, plain, ~ (xsd_string_3 = xsd_string_7), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ~ (xsd_string_3 = xsd_string_7), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_72)).
fof(f1897, plain, ! [X7] : ((xsd_string_3 = xsd_string_7) | ~ cBipedidae(X7) | ~ cGekkonidae(X7)), inference(resolution, [], [f1806, f595])).
fof(f1637, plain, (cGekkonidae(sK122) | ~ spl133_173), inference(avatar_component_clause, [], [f1635])).
fof(f2351, plain, (~ spl133_104 | ~ spl133_105), inference(avatar_contradiction_clause, [], [f2350])).
fof(f2350, plain, ($false | (~ spl133_104 | ~ spl133_105)), inference(subsumption_resolution, [], [f2345, f1320])).
fof(f1320, plain, (cCordylidae(sK99) | ~ spl133_105), inference(avatar_component_clause, [], [f1318])).
fof(f1318, plain, (spl133_105 <=> cCordylidae(sK99)), introduced(avatar_definition, [new_symbols(naming, [spl133_105])])).
fof(f2345, plain, (~ cCordylidae(sK99) | ~ spl133_104), inference(resolution, [], [f1315, f1936])).
fof(f1936, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cCordylidae(X9)), inference(subsumption_resolution, [], [f1927, f653])).
fof(f653, plain, ~ (xsd_string_4 = xsd_string_9), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ~ (xsd_string_4 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_81)).
fof(f1927, plain, ! [X9] : ((xsd_string_4 = xsd_string_9) | ~ cCordylidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1807, f601])).
fof(f1315, plain, (cLoxocemidae(sK99) | ~ spl133_104), inference(avatar_component_clause, [], [f1313])).
fof(f1313, plain, (spl133_104 <=> cLoxocemidae(sK99)), introduced(avatar_definition, [new_symbols(naming, [spl133_104])])).
fof(f2339, plain, (~ spl133_74 | ~ spl133_75), inference(avatar_split_clause, [], [f2336, f1178, f1173])).
fof(f1173, plain, (spl133_74 <=> cSphenodontidae(sK89)), introduced(avatar_definition, [new_symbols(naming, [spl133_74])])).
fof(f1178, plain, (spl133_75 <=> cXantusiidae(sK89)), introduced(avatar_definition, [new_symbols(naming, [spl133_75])])).
fof(f2336, plain, (~ cSphenodontidae(sK89) | ~ spl133_75), inference(resolution, [], [f2051, f1180])).
fof(f1180, plain, (cXantusiidae(sK89) | ~ spl133_75), inference(avatar_component_clause, [], [f1178])).
fof(f2051, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cSphenodontidae(X11)), inference(subsumption_resolution, [], [f2048, f676])).
fof(f676, plain, ~ (xsd_string_10 = xsd_string_11), inference(cnf_transformation, [], [f124])).
fof(f124, plain, ~ (xsd_string_10 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_104)).
fof(f2048, plain, ! [X11] : ((xsd_string_10 = xsd_string_11) | ~ cSphenodontidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1813, f609])).
fof(f1813, plain, ! [X21, X20] : (~ rfamily_name(X21, X20) | (xsd_string_10 = X20) | ~ cSphenodontidae(X21)), inference(subsumption_resolution, [], [f1799, f607])).
fof(f607, plain, ! [X0] : (~ cSphenodontidae(X0) | cReptile(X0)), inference(cnf_transformation, [], [f216])).
fof(f216, plain, ! [X0] : (cReptile(X0) | ~ cSphenodontidae(X0)), inference(ennf_transformation, [], [f151])).
fof(f151, plain, ! [X0] : (cSphenodontidae(X0) => cReptile(X0)), inference(rectify, [], [f55])).
fof(f55, plain, ! [X3] : (cSphenodontidae(X3) => cReptile(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_35)).
fof(f1799, plain, ! [X21, X20] : ((xsd_string_10 = X20) | ~ rfamily_name(X21, X20) | ~ cReptile(X21) | ~ cSphenodontidae(X21)), inference(resolution, [], [f604, f606])).
fof(f2311, plain, (~ spl133_152 | ~ spl133_153), inference(avatar_contradiction_clause, [], [f2310])).
fof(f2310, plain, ($false | (~ spl133_152 | ~ spl133_153)), inference(subsumption_resolution, [], [f2302, f1544])).
fof(f1544, plain, (cAmphisbaenidae(sK115) | ~ spl133_153), inference(avatar_component_clause, [], [f1542])).
fof(f1542, plain, (spl133_153 <=> cAmphisbaenidae(sK115)), introduced(avatar_definition, [new_symbols(naming, [spl133_153])])).
fof(f2302, plain, (~ cAmphisbaenidae(sK115) | ~ spl133_152), inference(resolution, [], [f1539, f1862])).
fof(f1862, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cAmphisbaenidae(X9)), inference(subsumption_resolution, [], [f1850, f629])).
fof(f629, plain, ~ (xsd_string_1 = xsd_string_9), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ~ (xsd_string_1 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_57)).
fof(f1850, plain, ! [X9] : ((xsd_string_1 = xsd_string_9) | ~ cAmphisbaenidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1804, f601])).
fof(f1539, plain, (cLoxocemidae(sK115) | ~ spl133_152), inference(avatar_component_clause, [], [f1537])).
fof(f1537, plain, (spl133_152 <=> cLoxocemidae(sK115)), introduced(avatar_definition, [new_symbols(naming, [spl133_152])])).
fof(f2291, plain, (~ spl133_107 | ~ spl133_108), inference(avatar_contradiction_clause, [], [f2290])).
fof(f2290, plain, ($false | (~ spl133_107 | ~ spl133_108)), inference(subsumption_resolution, [], [f2287, f1334])).
fof(f1334, plain, (cAmphisbaenidae(sK100) | ~ spl133_108), inference(avatar_component_clause, [], [f1332])).
fof(f1332, plain, (spl133_108 <=> cAmphisbaenidae(sK100)), introduced(avatar_definition, [new_symbols(naming, [spl133_108])])).
fof(f2287, plain, (~ cAmphisbaenidae(sK100) | ~ spl133_107), inference(resolution, [], [f1329, f1857])).
fof(f1857, plain, ! [X4] : (~ cCordylidae(X4) | ~ cAmphisbaenidae(X4)), inference(subsumption_resolution, [], [f1845, f624])).
fof(f624, plain, ~ (xsd_string_1 = xsd_string_4), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ~ (xsd_string_1 = xsd_string_4), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_52)).
fof(f1845, plain, ! [X4] : ((xsd_string_1 = xsd_string_4) | ~ cAmphisbaenidae(X4) | ~ cCordylidae(X4)), inference(resolution, [], [f1804, f586])).
fof(f1329, plain, (cCordylidae(sK100) | ~ spl133_107), inference(avatar_component_clause, [], [f1327])).
fof(f1327, plain, (spl133_107 <=> cCordylidae(sK100)), introduced(avatar_definition, [new_symbols(naming, [spl133_107])])).
fof(f2284, plain, (~ spl133_134 | ~ spl133_135), inference(avatar_contradiction_clause, [], [f2283])).
fof(f2283, plain, ($false | (~ spl133_134 | ~ spl133_135)), inference(subsumption_resolution, [], [f2278, f1460])).
fof(f1460, plain, (cAmphisbaenidae(sK109) | ~ spl133_135), inference(avatar_component_clause, [], [f1458])).
fof(f1458, plain, (spl133_135 <=> cAmphisbaenidae(sK109)), introduced(avatar_definition, [new_symbols(naming, [spl133_135])])).
fof(f2278, plain, (~ cAmphisbaenidae(sK109) | ~ spl133_134), inference(resolution, [], [f1455, f1859])).
fof(f1859, plain, ! [X6] : (~ cEmydidae(X6) | ~ cAmphisbaenidae(X6)), inference(subsumption_resolution, [], [f1847, f626])).
fof(f626, plain, ~ (xsd_string_1 = xsd_string_6), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ~ (xsd_string_1 = xsd_string_6), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_54)).
fof(f1847, plain, ! [X6] : ((xsd_string_1 = xsd_string_6) | ~ cAmphisbaenidae(X6) | ~ cEmydidae(X6)), inference(resolution, [], [f1804, f592])).
fof(f1455, plain, (cEmydidae(sK109) | ~ spl133_134), inference(avatar_component_clause, [], [f1453])).
fof(f1453, plain, (spl133_134 <=> cEmydidae(sK109)), introduced(avatar_definition, [new_symbols(naming, [spl133_134])])).
fof(f2275, plain, (~ spl133_41 | ~ spl133_42), inference(avatar_split_clause, [], [f2272, f1024, f1019])).
fof(f1019, plain, (spl133_41 <=> cEmydidae(sK78)), introduced(avatar_definition, [new_symbols(naming, [spl133_41])])).
fof(f1024, plain, (spl133_42 <=> cGekkonidae(sK78)), introduced(avatar_definition, [new_symbols(naming, [spl133_42])])).
fof(f2272, plain, (~ cEmydidae(sK78) | ~ spl133_42), inference(resolution, [], [f1026, f1975])).
fof(f1975, plain, ! [X7] : (~ cGekkonidae(X7) | ~ cEmydidae(X7)), inference(subsumption_resolution, [], [f1968, f662])).
fof(f662, plain, ~ (xsd_string_6 = xsd_string_7), inference(cnf_transformation, [], [f110])).
fof(f110, plain, ~ (xsd_string_6 = xsd_string_7), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_90)).
fof(f1968, plain, ! [X7] : ((xsd_string_6 = xsd_string_7) | ~ cEmydidae(X7) | ~ cGekkonidae(X7)), inference(resolution, [], [f1809, f595])).
fof(f1026, plain, (cGekkonidae(sK78) | ~ spl133_42), inference(avatar_component_clause, [], [f1024])).
fof(f2257, plain, (~ spl133_65 | ~ spl133_66), inference(avatar_split_clause, [], [f2251, f1136, f1131])).
fof(f1131, plain, (spl133_65 <=> cCordylidae(sK86)), introduced(avatar_definition, [new_symbols(naming, [spl133_65])])).
fof(f1136, plain, (spl133_66 <=> cLeptotyphlopidae(sK86)), introduced(avatar_definition, [new_symbols(naming, [spl133_66])])).
fof(f2251, plain, (~ cCordylidae(sK86) | ~ spl133_66), inference(resolution, [], [f1138, f1935])).
fof(f1935, plain, ! [X8] : (~ cLeptotyphlopidae(X8) | ~ cCordylidae(X8)), inference(subsumption_resolution, [], [f1926, f652])).
fof(f652, plain, ~ (xsd_string_4 = xsd_string_8), inference(cnf_transformation, [], [f100])).
fof(f100, plain, ~ (xsd_string_4 = xsd_string_8), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_80)).
fof(f1926, plain, ! [X8] : ((xsd_string_4 = xsd_string_8) | ~ cCordylidae(X8) | ~ cLeptotyphlopidae(X8)), inference(resolution, [], [f1807, f598])).
fof(f1138, plain, (cLeptotyphlopidae(sK86) | ~ spl133_66), inference(avatar_component_clause, [], [f1136])).
fof(f2240, plain, (~ spl133_137 | ~ spl133_138), inference(avatar_contradiction_clause, [], [f2239])).
fof(f2239, plain, ($false | (~ spl133_137 | ~ spl133_138)), inference(subsumption_resolution, [], [f2236, f1474])).
fof(f1474, plain, (cBipedidae(sK110) | ~ spl133_138), inference(avatar_component_clause, [], [f1472])).
fof(f1472, plain, (spl133_138 <=> cBipedidae(sK110)), introduced(avatar_definition, [new_symbols(naming, [spl133_138])])).
fof(f2236, plain, (~ cBipedidae(sK110) | ~ spl133_137), inference(resolution, [], [f1469, f1906])).
fof(f1906, plain, ! [X6] : (~ cEmydidae(X6) | ~ cBipedidae(X6)), inference(subsumption_resolution, [], [f1896, f643])).
fof(f643, plain, ~ (xsd_string_3 = xsd_string_6), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ~ (xsd_string_3 = xsd_string_6), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_71)).
fof(f1896, plain, ! [X6] : ((xsd_string_3 = xsd_string_6) | ~ cBipedidae(X6) | ~ cEmydidae(X6)), inference(resolution, [], [f1806, f592])).
fof(f1469, plain, (cEmydidae(sK110) | ~ spl133_137), inference(avatar_component_clause, [], [f1467])).
fof(f1467, plain, (spl133_137 <=> cEmydidae(sK110)), introduced(avatar_definition, [new_symbols(naming, [spl133_137])])).
fof(f2231, plain, (~ spl133_35 | ~ spl133_36), inference(avatar_split_clause, [], [f2227, f996, f991])).
fof(f991, plain, (spl133_35 <=> cEmydidae(sK76)), introduced(avatar_definition, [new_symbols(naming, [spl133_35])])).
fof(f996, plain, (spl133_36 <=> cLeptotyphlopidae(sK76)), introduced(avatar_definition, [new_symbols(naming, [spl133_36])])).
fof(f2227, plain, (~ cEmydidae(sK76) | ~ spl133_36), inference(resolution, [], [f998, f1976])).
fof(f1976, plain, ! [X8] : (~ cLeptotyphlopidae(X8) | ~ cEmydidae(X8)), inference(subsumption_resolution, [], [f1969, f663])).
fof(f663, plain, ~ (xsd_string_6 = xsd_string_8), inference(cnf_transformation, [], [f111])).
fof(f111, plain, ~ (xsd_string_6 = xsd_string_8), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_91)).
fof(f1969, plain, ! [X8] : ((xsd_string_6 = xsd_string_8) | ~ cEmydidae(X8) | ~ cLeptotyphlopidae(X8)), inference(resolution, [], [f1809, f598])).
fof(f998, plain, (cLeptotyphlopidae(sK76) | ~ spl133_36), inference(avatar_component_clause, [], [f996])).
fof(f2212, plain, (~ spl133_38 | ~ spl133_39), inference(avatar_split_clause, [], [f2209, f1010, f1005])).
fof(f1005, plain, (spl133_38 <=> cLoxocemidae(sK77)), introduced(avatar_definition, [new_symbols(naming, [spl133_38])])).
fof(f1010, plain, (spl133_39 <=> cSphenodontidae(sK77)), introduced(avatar_definition, [new_symbols(naming, [spl133_39])])).
fof(f2209, plain, (~ cLoxocemidae(sK77) | ~ spl133_39), inference(resolution, [], [f2033, f1012])).
fof(f1012, plain, (cSphenodontidae(sK77) | ~ spl133_39), inference(avatar_component_clause, [], [f1010])).
fof(f2033, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cLoxocemidae(X10)), inference(subsumption_resolution, [], [f2029, f674])).
fof(f674, plain, ~ (xsd_string_9 = xsd_string_10), inference(cnf_transformation, [], [f122])).
fof(f122, plain, ~ (xsd_string_9 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_102)).
fof(f2029, plain, ! [X10] : ((xsd_string_9 = xsd_string_10) | ~ cLoxocemidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1812, f606])).
fof(f2186, plain, (~ spl133_71 | ~ spl133_72), inference(avatar_split_clause, [], [f2177, f1164, f1159])).
fof(f1159, plain, (spl133_71 <=> cAmphisbaenidae(sK88)), introduced(avatar_definition, [new_symbols(naming, [spl133_71])])).
fof(f1164, plain, (spl133_72 <=> cLeptotyphlopidae(sK88)), introduced(avatar_definition, [new_symbols(naming, [spl133_72])])).
fof(f2177, plain, (~ cAmphisbaenidae(sK88) | ~ spl133_72), inference(resolution, [], [f1166, f1861])).
fof(f1861, plain, ! [X8] : (~ cLeptotyphlopidae(X8) | ~ cAmphisbaenidae(X8)), inference(subsumption_resolution, [], [f1849, f628])).
fof(f628, plain, ~ (xsd_string_1 = xsd_string_8), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ~ (xsd_string_1 = xsd_string_8), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_56)).
fof(f1849, plain, ! [X8] : ((xsd_string_1 = xsd_string_8) | ~ cAmphisbaenidae(X8) | ~ cLeptotyphlopidae(X8)), inference(resolution, [], [f1804, f598])).
fof(f1166, plain, (cLeptotyphlopidae(sK88) | ~ spl133_72), inference(avatar_component_clause, [], [f1164])).
fof(f2172, plain, (~ spl133_131 | ~ spl133_132), inference(avatar_contradiction_clause, [], [f2171])).
fof(f2171, plain, ($false | (~ spl133_131 | ~ spl133_132)), inference(subsumption_resolution, [], [f2169, f1446])).
fof(f1446, plain, (cAgamidae(sK108) | ~ spl133_132), inference(avatar_component_clause, [], [f1444])).
fof(f1444, plain, (spl133_132 <=> cAgamidae(sK108)), introduced(avatar_definition, [new_symbols(naming, [spl133_132])])).
fof(f2169, plain, (~ cAgamidae(sK108) | ~ spl133_131), inference(resolution, [], [f1441, f1833])).
fof(f1833, plain, ! [X5] : (~ cCrocodylidae(X5) | ~ cAgamidae(X5)), inference(subsumption_resolution, [], [f1820, f615])).
fof(f615, plain, ~ (xsd_string_0 = xsd_string_5), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ~ (xsd_string_0 = xsd_string_5), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_43)).
fof(f1820, plain, ! [X5] : ((xsd_string_0 = xsd_string_5) | ~ cAgamidae(X5) | ~ cCrocodylidae(X5)), inference(resolution, [], [f1803, f589])).
fof(f1441, plain, (cCrocodylidae(sK108) | ~ spl133_131), inference(avatar_component_clause, [], [f1439])).
fof(f1439, plain, (spl133_131 <=> cCrocodylidae(sK108)), introduced(avatar_definition, [new_symbols(naming, [spl133_131])])).
fof(f2159, plain, (~ spl133_15 | ~ spl133_14), inference(avatar_split_clause, [], [f2150, f893, f898])).
fof(f898, plain, (spl133_15 <=> cAnomalepidae(sK69)), introduced(avatar_definition, [new_symbols(naming, [spl133_15])])).
fof(f893, plain, (spl133_14 <=> cSphenodontidae(sK69)), introduced(avatar_definition, [new_symbols(naming, [spl133_14])])).
fof(f2150, plain, (~ cAnomalepidae(sK69) | ~ spl133_14), inference(resolution, [], [f895, f1887])).
fof(f1887, plain, ! [X10] : (~ cSphenodontidae(X10) | ~ cAnomalepidae(X10)), inference(subsumption_resolution, [], [f1876, f639])).
fof(f639, plain, ~ (xsd_string_2 = xsd_string_10), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ~ (xsd_string_2 = xsd_string_10), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_67)).
fof(f1876, plain, ! [X10] : ((xsd_string_2 = xsd_string_10) | ~ cAnomalepidae(X10) | ~ cSphenodontidae(X10)), inference(resolution, [], [f1805, f606])).
fof(f895, plain, (cSphenodontidae(sK69) | ~ spl133_14), inference(avatar_component_clause, [], [f893])).
fof(f2146, plain, (~ spl133_8 | ~ spl133_9), inference(avatar_split_clause, [], [f2137, f870, f865])).
fof(f865, plain, (spl133_8 <=> cAgamidae(sK67)), introduced(avatar_definition, [new_symbols(naming, [spl133_8])])).
fof(f870, plain, (spl133_9 <=> cGekkonidae(sK67)), introduced(avatar_definition, [new_symbols(naming, [spl133_9])])).
fof(f2137, plain, (~ cAgamidae(sK67) | ~ spl133_9), inference(resolution, [], [f872, f1835])).
fof(f1835, plain, ! [X7] : (~ cGekkonidae(X7) | ~ cAgamidae(X7)), inference(subsumption_resolution, [], [f1822, f617])).
fof(f617, plain, ~ (xsd_string_0 = xsd_string_7), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ~ (xsd_string_0 = xsd_string_7), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_45)).
fof(f1822, plain, ! [X7] : ((xsd_string_0 = xsd_string_7) | ~ cAgamidae(X7) | ~ cGekkonidae(X7)), inference(resolution, [], [f1803, f595])).
fof(f872, plain, (cGekkonidae(sK67) | ~ spl133_9), inference(avatar_component_clause, [], [f870])).
fof(f2134, plain, (~ spl133_20 | ~ spl133_21), inference(avatar_contradiction_clause, [], [f2133])).
fof(f2133, plain, ($false | (~ spl133_20 | ~ spl133_21)), inference(subsumption_resolution, [], [f2127, f928])).
fof(f928, plain, (cCordylidae(sK71) | ~ spl133_21), inference(avatar_component_clause, [], [f926])).
fof(f926, plain, (spl133_21 <=> cCordylidae(sK71)), introduced(avatar_definition, [new_symbols(naming, [spl133_21])])).
fof(f2127, plain, (~ cCordylidae(sK71) | ~ spl133_20), inference(resolution, [], [f923, f1932])).
fof(f1932, plain, ! [X5] : (~ cCrocodylidae(X5) | ~ cCordylidae(X5)), inference(subsumption_resolution, [], [f1923, f649])).
fof(f649, plain, ~ (xsd_string_4 = xsd_string_5), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ~ (xsd_string_4 = xsd_string_5), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_77)).
fof(f1923, plain, ! [X5] : ((xsd_string_4 = xsd_string_5) | ~ cCordylidae(X5) | ~ cCrocodylidae(X5)), inference(resolution, [], [f1807, f589])).
fof(f923, plain, (cCrocodylidae(sK71) | ~ spl133_20), inference(avatar_component_clause, [], [f921])).
fof(f921, plain, (spl133_20 <=> cCrocodylidae(sK71)), introduced(avatar_definition, [new_symbols(naming, [spl133_20])])).
fof(f2126, plain, (~ spl133_194 | ~ spl133_197), inference(avatar_split_clause, [], [f2122, f1761, f1737])).
fof(f1737, plain, (spl133_194 <=> cAgamidae(sK129)), introduced(avatar_definition, [new_symbols(naming, [spl133_194])])).
fof(f1761, plain, (spl133_197 <=> cAmphisbaenidae(sK129)), introduced(avatar_definition, [new_symbols(naming, [spl133_197])])).
fof(f2122, plain, (~ cAgamidae(sK129) | ~ spl133_197), inference(resolution, [], [f1763, f1829])).
fof(f1829, plain, ! [X1] : (~ cAmphisbaenidae(X1) | ~ cAgamidae(X1)), inference(subsumption_resolution, [], [f1816, f611])).
fof(f611, plain, ~ (xsd_string_0 = xsd_string_1), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ~ (xsd_string_0 = xsd_string_1), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_39)).
fof(f1816, plain, ! [X1] : ((xsd_string_0 = xsd_string_1) | ~ cAgamidae(X1) | ~ cAmphisbaenidae(X1)), inference(resolution, [], [f1803, f577])).
fof(f1763, plain, (cAmphisbaenidae(sK129) | ~ spl133_197), inference(avatar_component_clause, [], [f1761])).
fof(f2120, plain, (~ spl133_11 | ~ spl133_12), inference(avatar_contradiction_clause, [], [f2119])).
fof(f2119, plain, ($false | (~ spl133_11 | ~ spl133_12)), inference(subsumption_resolution, [], [f2118, f886])).
fof(f886, plain, (cLeptotyphlopidae(sK68) | ~ spl133_12), inference(avatar_component_clause, [], [f884])).
fof(f884, plain, (spl133_12 <=> cLeptotyphlopidae(sK68)), introduced(avatar_definition, [new_symbols(naming, [spl133_12])])).
fof(f2118, plain, (~ cLeptotyphlopidae(sK68) | ~ spl133_11), inference(resolution, [], [f2017, f881])).
fof(f881, plain, (cXantusiidae(sK68) | ~ spl133_11), inference(avatar_component_clause, [], [f879])).
fof(f879, plain, (spl133_11 <=> cXantusiidae(sK68)), introduced(avatar_definition, [new_symbols(naming, [spl133_11])])).
fof(f2017, plain, ! [X11] : (~ cXantusiidae(X11) | ~ cLeptotyphlopidae(X11)), inference(subsumption_resolution, [], [f2012, f673])).
fof(f673, plain, ~ (xsd_string_8 = xsd_string_11), inference(cnf_transformation, [], [f121])).
fof(f121, plain, ~ (xsd_string_8 = xsd_string_11), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_101)).
fof(f2012, plain, ! [X11] : ((xsd_string_8 = xsd_string_11) | ~ cLeptotyphlopidae(X11) | ~ cXantusiidae(X11)), inference(resolution, [], [f1811, f609])).
fof(f2099, plain, (~ spl133_3 | ~ spl133_2), inference(avatar_split_clause, [], [f2096, f837, f842])).
fof(f842, plain, (spl133_3 <=> cLeptotyphlopidae(sK65)), introduced(avatar_definition, [new_symbols(naming, [spl133_3])])).
fof(f837, plain, (spl133_2 <=> cLoxocemidae(sK65)), introduced(avatar_definition, [new_symbols(naming, [spl133_2])])).
fof(f2096, plain, (~ cLeptotyphlopidae(sK65) | ~ spl133_2), inference(resolution, [], [f2015, f839])).
fof(f839, plain, (cLoxocemidae(sK65) | ~ spl133_2), inference(avatar_component_clause, [], [f837])).
fof(f2015, plain, ! [X9] : (~ cLoxocemidae(X9) | ~ cLeptotyphlopidae(X9)), inference(subsumption_resolution, [], [f2010, f671])).
fof(f671, plain, ~ (xsd_string_8 = xsd_string_9), inference(cnf_transformation, [], [f119])).
fof(f119, plain, ~ (xsd_string_8 = xsd_string_9), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_99)).
fof(f2010, plain, ! [X9] : ((xsd_string_8 = xsd_string_9) | ~ cLeptotyphlopidae(X9) | ~ cLoxocemidae(X9)), inference(resolution, [], [f1811, f601])).
fof(f1915, plain, (~ spl133_5 | ~ spl133_6), inference(avatar_contradiction_clause, [], [f1914])).
fof(f1914, plain, ($false | (~ spl133_5 | ~ spl133_6)), inference(subsumption_resolution, [], [f1913, f858])).
fof(f858, plain, (cAgamidae(sK66) | ~ spl133_6), inference(avatar_component_clause, [], [f856])).
fof(f856, plain, (spl133_6 <=> cAgamidae(sK66)), introduced(avatar_definition, [new_symbols(naming, [spl133_6])])).
fof(f1913, plain, (~ cAgamidae(sK66) | ~ spl133_5), inference(resolution, [], [f1832, f853])).
fof(f853, plain, (cCordylidae(sK66) | ~ spl133_5), inference(avatar_component_clause, [], [f851])).
fof(f851, plain, (spl133_5 <=> cCordylidae(sK66)), introduced(avatar_definition, [new_symbols(naming, [spl133_5])])).
fof(f1832, plain, ! [X4] : (~ cCordylidae(X4) | ~ cAgamidae(X4)), inference(subsumption_resolution, [], [f1819, f614])).
fof(f614, plain, ~ (xsd_string_0 = xsd_string_4), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ~ (xsd_string_0 = xsd_string_4), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_42)).
fof(f1819, plain, ! [X4] : ((xsd_string_0 = xsd_string_4) | ~ cAgamidae(X4) | ~ cCordylidae(X4)), inference(resolution, [], [f1803, f586])).
fof(f1773, plain, (spl133_190 | spl133_187 | spl133_184 | spl133_181 | spl133_178 | spl133_175 | spl133_172 | spl133_169 | spl133_166 | spl133_163 | spl133_160 | spl133_157 | spl133_154 | spl133_151 | spl133_148 | spl133_145 | spl133_142 | spl133_139 | spl133_136 | spl133_133 | spl133_130 | spl133_127 | spl133_124 | spl133_121 | spl133_118 | spl133_115 | spl133_112 | spl133_109 | spl133_106 | spl133_103 | spl133_100 | spl133_97 | spl133_94 | spl133_91 | spl133_88 | spl133_85 | spl133_82 | spl133_79 | spl133_76 | spl133_73 | spl133_70 | spl133_67 | spl133_64 | spl133_61 | spl133_58 | spl133_55 | spl133_52 | spl133_49 | spl133_46 | spl133_43 | spl133_40 | spl133_37 | spl133_34 | spl133_31 | spl133_28 | spl133_25 | spl133_22 | spl133_19 | spl133_16 | spl133_13 | spl133_10 | spl133_7 | spl133_4 | spl133_1 | spl133_195 | spl133_196 | spl133_197), inference(avatar_split_clause, [], [f1772, f1761, f1751, f1744, f833, f847, f861, f875, f889, f903, f917, f931, f945, f959, f973, f987, f1001, f1015, f1029, f1043, f1057, f1071, f1085, f1099, f1113, f1127, f1141, f1155, f1169, f1183, f1197, f1211, f1225, f1239, f1253, f1267, f1281, f1295, f1309, f1323, f1337, f1351, f1365, f1379, f1393, f1407, f1421, f1435, f1449, f1463, f1477, f1491, f1505, f1519, f1533, f1547, f1561, f1575, f1589, f1603, f1617, f1631, f1645, f1659, f1673, f1687, f1701, f1716])).
fof(f1716, plain, (spl133_190 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl133_190])])).
fof(f1701, plain, (spl133_187 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl133_187])])).
fof(f1687, plain, (spl133_184 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl133_184])])).
fof(f1673, plain, (spl133_181 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl133_181])])).
fof(f1659, plain, (spl133_178 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl133_178])])).
fof(f1645, plain, (spl133_175 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl133_175])])).
fof(f1631, plain, (spl133_172 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl133_172])])).
fof(f1617, plain, (spl133_169 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl133_169])])).
fof(f1603, plain, (spl133_166 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl133_166])])).
fof(f1589, plain, (spl133_163 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl133_163])])).
fof(f1575, plain, (spl133_160 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl133_160])])).
fof(f1561, plain, (spl133_157 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl133_157])])).
fof(f1547, plain, (spl133_154 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl133_154])])).
fof(f1533, plain, (spl133_151 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl133_151])])).
fof(f1519, plain, (spl133_148 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl133_148])])).
fof(f1505, plain, (spl133_145 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl133_145])])).
fof(f1491, plain, (spl133_142 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl133_142])])).
fof(f1477, plain, (spl133_139 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl133_139])])).
fof(f1463, plain, (spl133_136 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl133_136])])).
fof(f1449, plain, (spl133_133 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl133_133])])).
fof(f1435, plain, (spl133_130 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl133_130])])).
fof(f1421, plain, (spl133_127 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl133_127])])).
fof(f1407, plain, (spl133_124 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl133_124])])).
fof(f1393, plain, (spl133_121 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl133_121])])).
fof(f1379, plain, (spl133_118 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl133_118])])).
fof(f1365, plain, (spl133_115 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl133_115])])).
fof(f1351, plain, (spl133_112 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl133_112])])).
fof(f1337, plain, (spl133_109 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl133_109])])).
fof(f1323, plain, (spl133_106 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl133_106])])).
fof(f1309, plain, (spl133_103 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl133_103])])).
fof(f1295, plain, (spl133_100 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl133_100])])).
fof(f1281, plain, (spl133_97 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl133_97])])).
fof(f1267, plain, (spl133_94 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl133_94])])).
fof(f1253, plain, (spl133_91 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl133_91])])).
fof(f1239, plain, (spl133_88 <=> sP34), introduced(avatar_definition, [new_symbols(naming, [spl133_88])])).
fof(f1225, plain, (spl133_85 <=> sP35), introduced(avatar_definition, [new_symbols(naming, [spl133_85])])).
fof(f1211, plain, (spl133_82 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl133_82])])).
fof(f1197, plain, (spl133_79 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl133_79])])).
fof(f1183, plain, (spl133_76 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl133_76])])).
fof(f1169, plain, (spl133_73 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl133_73])])).
fof(f1155, plain, (spl133_70 <=> sP40), introduced(avatar_definition, [new_symbols(naming, [spl133_70])])).
fof(f1141, plain, (spl133_67 <=> sP41), introduced(avatar_definition, [new_symbols(naming, [spl133_67])])).
fof(f1127, plain, (spl133_64 <=> sP42), introduced(avatar_definition, [new_symbols(naming, [spl133_64])])).
fof(f1113, plain, (spl133_61 <=> sP43), introduced(avatar_definition, [new_symbols(naming, [spl133_61])])).
fof(f1099, plain, (spl133_58 <=> sP44), introduced(avatar_definition, [new_symbols(naming, [spl133_58])])).
fof(f1085, plain, (spl133_55 <=> sP45), introduced(avatar_definition, [new_symbols(naming, [spl133_55])])).
fof(f1071, plain, (spl133_52 <=> sP46), introduced(avatar_definition, [new_symbols(naming, [spl133_52])])).
fof(f1057, plain, (spl133_49 <=> sP47), introduced(avatar_definition, [new_symbols(naming, [spl133_49])])).
fof(f1043, plain, (spl133_46 <=> sP48), introduced(avatar_definition, [new_symbols(naming, [spl133_46])])).
fof(f1029, plain, (spl133_43 <=> sP49), introduced(avatar_definition, [new_symbols(naming, [spl133_43])])).
fof(f1015, plain, (spl133_40 <=> sP50), introduced(avatar_definition, [new_symbols(naming, [spl133_40])])).
fof(f1001, plain, (spl133_37 <=> sP51), introduced(avatar_definition, [new_symbols(naming, [spl133_37])])).
fof(f987, plain, (spl133_34 <=> sP52), introduced(avatar_definition, [new_symbols(naming, [spl133_34])])).
fof(f973, plain, (spl133_31 <=> sP53), introduced(avatar_definition, [new_symbols(naming, [spl133_31])])).
fof(f959, plain, (spl133_28 <=> sP54), introduced(avatar_definition, [new_symbols(naming, [spl133_28])])).
fof(f945, plain, (spl133_25 <=> sP55), introduced(avatar_definition, [new_symbols(naming, [spl133_25])])).
fof(f931, plain, (spl133_22 <=> sP56), introduced(avatar_definition, [new_symbols(naming, [spl133_22])])).
fof(f917, plain, (spl133_19 <=> sP57), introduced(avatar_definition, [new_symbols(naming, [spl133_19])])).
fof(f903, plain, (spl133_16 <=> sP58), introduced(avatar_definition, [new_symbols(naming, [spl133_16])])).
fof(f889, plain, (spl133_13 <=> sP59), introduced(avatar_definition, [new_symbols(naming, [spl133_13])])).
fof(f875, plain, (spl133_10 <=> sP60), introduced(avatar_definition, [new_symbols(naming, [spl133_10])])).
fof(f861, plain, (spl133_7 <=> sP61), introduced(avatar_definition, [new_symbols(naming, [spl133_7])])).
fof(f847, plain, (spl133_4 <=> sP62), introduced(avatar_definition, [new_symbols(naming, [spl133_4])])).
fof(f833, plain, (spl133_1 <=> sP63), introduced(avatar_definition, [new_symbols(naming, [spl133_1])])).
fof(f1772, plain, (cAmphisbaenidae(sK129) | cLeptotyphlopidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(subsumption_resolution, [], [f1771, f569])).
fof(f569, plain, ! [X0] : cowlThing(X0), inference(cnf_transformation, [], [f127])).
fof(f127, plain, ! [X0] : (~ cowlNothing(X0) & cowlThing(X0)), inference(rectify, [], [f20])).
fof(f20, plain, ! [X3] : (~ cowlNothing(X3) & cowlThing(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_0)).
fof(f1771, plain, (cAmphisbaenidae(sK129) | cLeptotyphlopidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ~ cowlThing(sK132)), inference(subsumption_resolution, [], [f805, f570])).
fof(f570, plain, ! [X0] : ~ cowlNothing(X0), inference(cnf_transformation, [], [f127])).
fof(f805, plain, (cAmphisbaenidae(sK129) | cLeptotyphlopidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | cowlNothing(sK132) | ~ cowlThing(sK132)), inference(cnf_transformation, [], [f549])).
fof(f549, plain, ((cAgamidae(sK129) & cAmphisbaenidae(sK129)) | (cSphenodontidae(sK130) & cLeptotyphlopidae(sK130)) | (cLoxocemidae(sK131) & cEmydidae(sK131)) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | (cowlNothing(sK132) | ~ cowlThing(sK132))), inference(skolemisation, [status(esa), new_symbols(skolem, [sK129, sK130, sK131, sK132])], [f544, f548, f547, f546, f545])).
fof(f545, plain, (? [X0] : (cAgamidae(X0) & cAmphisbaenidae(X0)) => (cAgamidae(sK129) & cAmphisbaenidae(sK129))), introduced(choice_axiom, [])).
fof(f546, plain, (? [X1] : (cSphenodontidae(X1) & cLeptotyphlopidae(X1)) => (cSphenodontidae(sK130) & cLeptotyphlopidae(sK130))), introduced(choice_axiom, [])).
fof(f547, plain, (? [X2] : (cLoxocemidae(X2) & cEmydidae(X2)) => (cLoxocemidae(sK131) & cEmydidae(sK131))), introduced(choice_axiom, [])).
fof(f548, plain, (? [X3] : (cowlNothing(X3) | ~ cowlThing(X3)) => (cowlNothing(sK132) | ~ cowlThing(sK132))), introduced(choice_axiom, [])).
fof(f544, plain, (? [X0] : (cAgamidae(X0) & cAmphisbaenidae(X0)) | ? [X1] : (cSphenodontidae(X1) & cLeptotyphlopidae(X1)) | ? [X2] : (cLoxocemidae(X2) & cEmydidae(X2)) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ? [X3] : (cowlNothing(X3) | ~ cowlThing(X3))), inference(rectify, [], [f284])).
fof(f284, plain, (? [X0] : (cAgamidae(X0) & cAmphisbaenidae(X0)) | ? [X1] : (cSphenodontidae(X1) & cLeptotyphlopidae(X1)) | ? [X2] : (cLoxocemidae(X2) & cEmydidae(X2)) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ? [X67] : (cowlNothing(X67) | ~ cowlThing(X67))), inference(definition_folding, [], [f219, e283, e282, e281, e280, e279, e278, e277, e276, e275, e274, e273, e272, e271, e270, e269, e268, e267, e266, e265, e264, e263, e262, e261, e260, e259, e258, e257, e256, e255, e254, e253, e252, e251, e250, e249, e248, e247, e246, e245, e244, e243, e242, e241, e240, e239, e238, e237, e236, e235, e234, e233, e232, e231, e230, e229, e228, e227, e226, e225, e224, e223, e222, e221, e220])).
fof(f220, plain, (? [X66] : ~ (xsd_string(X66) <=> ~ xsd_integer(X66)) | ~ sP0), inference(usedef, [], [e220])).
fof(e220, plain, (sP0 <=> ? [X66] : ~ (xsd_string(X66) <=> ~ xsd_integer(X66))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f221, plain, (? [X65] : (cBipedidae(X65) & cLeptotyphlopidae(X65)) | ~ sP1), inference(usedef, [], [e221])).
fof(e221, plain, (sP1 <=> ? [X65] : (cBipedidae(X65) & cLeptotyphlopidae(X65))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f222, plain, (? [X64] : (cAnomalepidae(X64) & cBipedidae(X64)) | ~ sP2), inference(usedef, [], [e222])).
fof(e222, plain, (sP2 <=> ? [X64] : (cAnomalepidae(X64) & cBipedidae(X64))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f223, plain, (? [X63] : (cGekkonidae(X63) & cLeptotyphlopidae(X63)) | ~ sP3), inference(usedef, [], [e223])).
fof(e223, plain, (sP3 <=> ? [X63] : (cGekkonidae(X63) & cLeptotyphlopidae(X63))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f224, plain, (? [X62] : (cSphenodontidae(X62) & cAmphisbaenidae(X62)) | ~ sP4), inference(usedef, [], [e224])).
fof(e224, plain, (sP4 <=> ? [X62] : (cSphenodontidae(X62) & cAmphisbaenidae(X62))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f225, plain, (? [X61] : (cCrocodylidae(X61) & cBipedidae(X61)) | ~ sP5), inference(usedef, [], [e225])).
fof(e225, plain, (sP5 <=> ? [X61] : (cCrocodylidae(X61) & cBipedidae(X61))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f226, plain, (? [X60] : (cGekkonidae(X60) & cBipedidae(X60)) | ~ sP6), inference(usedef, [], [e226])).
fof(e226, plain, (sP6 <=> ? [X60] : (cGekkonidae(X60) & cBipedidae(X60))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f227, plain, (? [X59] : (cSphenodontidae(X59) & cBipedidae(X59)) | ~ sP7), inference(usedef, [], [e227])).
fof(e227, plain, (sP7 <=> ? [X59] : (cSphenodontidae(X59) & cBipedidae(X59))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f228, plain, (? [X58] : (cCrocodylidae(X58) & cGekkonidae(X58)) | ~ sP8), inference(usedef, [], [e228])).
fof(e228, plain, (sP8 <=> ? [X58] : (cCrocodylidae(X58) & cGekkonidae(X58))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f229, plain, (? [X57] : (cSphenodontidae(X57) & cGekkonidae(X57)) | ~ sP9), inference(usedef, [], [e229])).
fof(e229, plain, (sP9 <=> ? [X57] : (cSphenodontidae(X57) & cGekkonidae(X57))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f230, plain, (? [X56] : (cSphenodontidae(X56) & cAgamidae(X56)) | ~ sP10), inference(usedef, [], [e230])).
fof(e230, plain, (sP10 <=> ? [X56] : (cSphenodontidae(X56) & cAgamidae(X56))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f231, plain, (? [X55] : (cCrocodylidae(X55) & cAnomalepidae(X55)) | ~ sP11), inference(usedef, [], [e231])).
fof(e231, plain, (sP11 <=> ? [X55] : (cCrocodylidae(X55) & cAnomalepidae(X55))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f232, plain, (? [X54] : (cEmydidae(X54) & cCrocodylidae(X54)) | ~ sP12), inference(usedef, [], [e232])).
fof(e232, plain, (sP12 <=> ? [X54] : (cEmydidae(X54) & cCrocodylidae(X54))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f233, plain, (? [X53] : (cLoxocemidae(X53) & cAmphisbaenidae(X53)) | ~ sP13), inference(usedef, [], [e233])).
fof(e233, plain, (sP13 <=> ? [X53] : (cLoxocemidae(X53) & cAmphisbaenidae(X53))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f234, plain, (? [X52] : (cAgamidae(X52) & cLeptotyphlopidae(X52)) | ~ sP14), inference(usedef, [], [e234])).
fof(e234, plain, (sP14 <=> ? [X52] : (cAgamidae(X52) & cLeptotyphlopidae(X52))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f235, plain, (? [X51] : (cCrocodylidae(X51) & cAmphisbaenidae(X51)) | ~ sP15), inference(usedef, [], [e235])).
fof(e235, plain, (sP15 <=> ? [X51] : (cCrocodylidae(X51) & cAmphisbaenidae(X51))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f236, plain, (? [X50] : (cLoxocemidae(X50) & cCrocodylidae(X50)) | ~ sP16), inference(usedef, [], [e236])).
fof(e236, plain, (sP16 <=> ? [X50] : (cLoxocemidae(X50) & cCrocodylidae(X50))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f237, plain, (? [X49] : (cCrocodylidae(X49) & cXantusiidae(X49)) | ~ sP17), inference(usedef, [], [e237])).
fof(e237, plain, (sP17 <=> ? [X49] : (cCrocodylidae(X49) & cXantusiidae(X49))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f238, plain, (? [X48] : (cEmydidae(X48) & cBipedidae(X48)) | ~ sP18), inference(usedef, [], [e238])).
fof(e238, plain, (sP18 <=> ? [X48] : (cEmydidae(X48) & cBipedidae(X48))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f239, plain, (? [X47] : (cEmydidae(X47) & cAmphisbaenidae(X47)) | ~ sP19), inference(usedef, [], [e239])).
fof(e239, plain, (sP19 <=> ? [X47] : (cEmydidae(X47) & cAmphisbaenidae(X47))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f240, plain, (? [X46] : (cCrocodylidae(X46) & cAgamidae(X46)) | ~ sP20), inference(usedef, [], [e240])).
fof(e240, plain, (sP20 <=> ? [X46] : (cCrocodylidae(X46) & cAgamidae(X46))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f241, plain, (? [X45] : (cLoxocemidae(X45) & cXantusiidae(X45)) | ~ sP21), inference(usedef, [], [e241])).
fof(e241, plain, (sP21 <=> ? [X45] : (cLoxocemidae(X45) & cXantusiidae(X45))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f242, plain, (? [X44] : (cEmydidae(X44) & cXantusiidae(X44)) | ~ sP22), inference(usedef, [], [e242])).
fof(e242, plain, (sP22 <=> ? [X44] : (cEmydidae(X44) & cXantusiidae(X44))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f243, plain, (? [X43] : (cLoxocemidae(X43) & cBipedidae(X43)) | ~ sP23), inference(usedef, [], [e243])).
fof(e243, plain, (sP23 <=> ? [X43] : (cLoxocemidae(X43) & cBipedidae(X43))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f244, plain, (? [X42] : (cAgamidae(X42) & cBipedidae(X42)) | ~ sP24), inference(usedef, [], [e244])).
fof(e244, plain, (sP24 <=> ? [X42] : (cAgamidae(X42) & cBipedidae(X42))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f245, plain, (? [X41] : (cAmphisbaenidae(X41) & cGekkonidae(X41)) | ~ sP25), inference(usedef, [], [e245])).
fof(e245, plain, (sP25 <=> ? [X41] : (cAmphisbaenidae(X41) & cGekkonidae(X41))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f246, plain, (? [X40] : (cCrocodylidae(X40) & cLeptotyphlopidae(X40)) | ~ sP26), inference(usedef, [], [e246])).
fof(e246, plain, (sP26 <=> ? [X40] : (cCrocodylidae(X40) & cLeptotyphlopidae(X40))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f247, plain, (? [X39] : (cCordylidae(X39) & cSphenodontidae(X39)) | ~ sP27), inference(usedef, [], [e247])).
fof(e247, plain, (sP27 <=> ? [X39] : (cCordylidae(X39) & cSphenodontidae(X39))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f248, plain, (? [X38] : (cCordylidae(X38) & cAmphisbaenidae(X38)) | ~ sP28), inference(usedef, [], [e248])).
fof(e248, plain, (sP28 <=> ? [X38] : (cCordylidae(X38) & cAmphisbaenidae(X38))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f249, plain, (? [X37] : (cLoxocemidae(X37) & cCordylidae(X37)) | ~ sP29), inference(usedef, [], [e249])).
fof(e249, plain, (sP29 <=> ? [X37] : (cLoxocemidae(X37) & cCordylidae(X37))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f250, plain, (? [X36] : (cCordylidae(X36) & cGekkonidae(X36)) | ~ sP30), inference(usedef, [], [e250])).
fof(e250, plain, (sP30 <=> ? [X36] : (cCordylidae(X36) & cGekkonidae(X36))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f251, plain, (? [X35] : (cAgamidae(X35) & cXantusiidae(X35)) | ~ sP31), inference(usedef, [], [e251])).
fof(e251, plain, (sP31 <=> ? [X35] : (cAgamidae(X35) & cXantusiidae(X35))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f252, plain, (? [X34] : (cCordylidae(X34) & cAnomalepidae(X34)) | ~ sP32), inference(usedef, [], [e252])).
fof(e252, plain, (sP32 <=> ? [X34] : (cCordylidae(X34) & cAnomalepidae(X34))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f253, plain, (? [X33] : (cEmydidae(X33) & cAgamidae(X33)) | ~ sP33), inference(usedef, [], [e253])).
fof(e253, plain, (sP33 <=> ? [X33] : (cEmydidae(X33) & cAgamidae(X33))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f254, plain, (? [X32] : (cEmydidae(X32) & cCordylidae(X32)) | ~ sP34), inference(usedef, [], [e254])).
fof(e254, plain, (sP34 <=> ? [X32] : (cEmydidae(X32) & cCordylidae(X32))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f255, plain, (? [X31] : (cLoxocemidae(X31) & cAgamidae(X31)) | ~ sP35), inference(usedef, [], [e255])).
fof(e255, plain, (sP35 <=> ? [X31] : (cLoxocemidae(X31) & cAgamidae(X31))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f256, plain, (? [X30] : (cGekkonidae(X30) & cXantusiidae(X30)) | ~ sP36), inference(usedef, [], [e256])).
fof(e256, plain, (sP36 <=> ? [X30] : (cGekkonidae(X30) & cXantusiidae(X30))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f257, plain, (? [X29] : (cBipedidae(X29) & cXantusiidae(X29)) | ~ sP37), inference(usedef, [], [e257])).
fof(e257, plain, (sP37 <=> ? [X29] : (cBipedidae(X29) & cXantusiidae(X29))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f258, plain, (? [X28] : (cEmydidae(X28) & cAnomalepidae(X28)) | ~ sP38), inference(usedef, [], [e258])).
fof(e258, plain, (sP38 <=> ? [X28] : (cEmydidae(X28) & cAnomalepidae(X28))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f259, plain, (? [X27] : (cSphenodontidae(X27) & cXantusiidae(X27)) | ~ sP39), inference(usedef, [], [e259])).
fof(e259, plain, (sP39 <=> ? [X27] : (cSphenodontidae(X27) & cXantusiidae(X27))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f260, plain, (? [X26] : (cAmphisbaenidae(X26) & cLeptotyphlopidae(X26)) | ~ sP40), inference(usedef, [], [e260])).
fof(e260, plain, (sP40 <=> ? [X26] : (cAmphisbaenidae(X26) & cLeptotyphlopidae(X26))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f261, plain, (? [X25] : (cEmydidae(X25) & cSphenodontidae(X25)) | ~ sP41), inference(usedef, [], [e261])).
fof(e261, plain, (sP41 <=> ? [X25] : (cEmydidae(X25) & cSphenodontidae(X25))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f262, plain, (? [X24] : (cCordylidae(X24) & cLeptotyphlopidae(X24)) | ~ sP42), inference(usedef, [], [e262])).
fof(e262, plain, (sP42 <=> ? [X24] : (cCordylidae(X24) & cLeptotyphlopidae(X24))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f263, plain, (? [X23] : (cAnomalepidae(X23) & cGekkonidae(X23)) | ~ sP43), inference(usedef, [], [e263])).
fof(e263, plain, (sP43 <=> ? [X23] : (cAnomalepidae(X23) & cGekkonidae(X23))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f264, plain, (? [X22] : (cCordylidae(X22) & cBipedidae(X22)) | ~ sP44), inference(usedef, [], [e264])).
fof(e264, plain, (sP44 <=> ? [X22] : (cCordylidae(X22) & cBipedidae(X22))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f265, plain, (? [X21] : (cAmphisbaenidae(X21) & cBipedidae(X21)) | ~ sP45), inference(usedef, [], [e265])).
fof(e265, plain, (sP45 <=> ? [X21] : (cAmphisbaenidae(X21) & cBipedidae(X21))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f266, plain, (? [X20] : (cCordylidae(X20) & cXantusiidae(X20)) | ~ sP46), inference(usedef, [], [e266])).
fof(e266, plain, (sP46 <=> ? [X20] : (cCordylidae(X20) & cXantusiidae(X20))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f267, plain, (? [X19] : (cAgamidae(X19) & cAnomalepidae(X19)) | ~ sP47), inference(usedef, [], [e267])).
fof(e267, plain, (sP47 <=> ? [X19] : (cAgamidae(X19) & cAnomalepidae(X19))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f268, plain, (? [X18] : (cCrocodylidae(X18) & cSphenodontidae(X18)) | ~ sP48), inference(usedef, [], [e268])).
fof(e268, plain, (sP48 <=> ? [X18] : (cCrocodylidae(X18) & cSphenodontidae(X18))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP48])])).
fof(f269, plain, (? [X17] : (cAmphisbaenidae(X17) & cXantusiidae(X17)) | ~ sP49), inference(usedef, [], [e269])).
fof(e269, plain, (sP49 <=> ? [X17] : (cAmphisbaenidae(X17) & cXantusiidae(X17))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP49])])).
fof(f270, plain, (? [X16] : (cEmydidae(X16) & cGekkonidae(X16)) | ~ sP50), inference(usedef, [], [e270])).
fof(e270, plain, (sP50 <=> ? [X16] : (cEmydidae(X16) & cGekkonidae(X16))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP50])])).
fof(f271, plain, (? [X15] : (cLoxocemidae(X15) & cSphenodontidae(X15)) | ~ sP51), inference(usedef, [], [e271])).
fof(e271, plain, (sP51 <=> ? [X15] : (cLoxocemidae(X15) & cSphenodontidae(X15))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP51])])).
fof(f272, plain, (? [X14] : (cEmydidae(X14) & cLeptotyphlopidae(X14)) | ~ sP52), inference(usedef, [], [e272])).
fof(e272, plain, (sP52 <=> ? [X14] : (cEmydidae(X14) & cLeptotyphlopidae(X14))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP52])])).
fof(f273, plain, (? [X13] : (cAnomalepidae(X13) & cAmphisbaenidae(X13)) | ~ sP53), inference(usedef, [], [e273])).
fof(e273, plain, (sP53 <=> ? [X13] : (cAnomalepidae(X13) & cAmphisbaenidae(X13))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP53])])).
fof(f274, plain, (? [X12] : (cLoxocemidae(X12) & cGekkonidae(X12)) | ~ sP54), inference(usedef, [], [e274])).
fof(e274, plain, (sP54 <=> ? [X12] : (cLoxocemidae(X12) & cGekkonidae(X12))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP54])])).
fof(f275, plain, (? [X11] : (cLoxocemidae(X11) & cAnomalepidae(X11)) | ~ sP55), inference(usedef, [], [e275])).
fof(e275, plain, (sP55 <=> ? [X11] : (cLoxocemidae(X11) & cAnomalepidae(X11))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP55])])).
fof(f276, plain, (? [X10] : (cAnomalepidae(X10) & cLeptotyphlopidae(X10)) | ~ sP56), inference(usedef, [], [e276])).
fof(e276, plain, (sP56 <=> ? [X10] : (cAnomalepidae(X10) & cLeptotyphlopidae(X10))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP56])])).
fof(f277, plain, (? [X9] : (cCrocodylidae(X9) & cCordylidae(X9)) | ~ sP57), inference(usedef, [], [e277])).
fof(e277, plain, (sP57 <=> ? [X9] : (cCrocodylidae(X9) & cCordylidae(X9))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP57])])).
fof(f278, plain, (? [X8] : (cAnomalepidae(X8) & cXantusiidae(X8)) | ~ sP58), inference(usedef, [], [e278])).
fof(e278, plain, (sP58 <=> ? [X8] : (cAnomalepidae(X8) & cXantusiidae(X8))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP58])])).
fof(f279, plain, (? [X7] : (cSphenodontidae(X7) & cAnomalepidae(X7)) | ~ sP59), inference(usedef, [], [e279])).
fof(e279, plain, (sP59 <=> ? [X7] : (cSphenodontidae(X7) & cAnomalepidae(X7))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP59])])).
fof(f280, plain, (? [X6] : (cXantusiidae(X6) & cLeptotyphlopidae(X6)) | ~ sP60), inference(usedef, [], [e280])).
fof(e280, plain, (sP60 <=> ? [X6] : (cXantusiidae(X6) & cLeptotyphlopidae(X6))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP60])])).
fof(f281, plain, (? [X5] : (cAgamidae(X5) & cGekkonidae(X5)) | ~ sP61), inference(usedef, [], [e281])).
fof(e281, plain, (sP61 <=> ? [X5] : (cAgamidae(X5) & cGekkonidae(X5))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP61])])).
fof(f282, plain, (? [X4] : (cCordylidae(X4) & cAgamidae(X4)) | ~ sP62), inference(usedef, [], [e282])).
fof(e282, plain, (sP62 <=> ? [X4] : (cCordylidae(X4) & cAgamidae(X4))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP62])])).
fof(f283, plain, (? [X3] : (cLoxocemidae(X3) & cLeptotyphlopidae(X3)) | ~ sP63), inference(usedef, [], [e283])).
fof(e283, plain, (sP63 <=> ? [X3] : (cLoxocemidae(X3) & cLeptotyphlopidae(X3))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP63])])).
fof(f219, plain, (? [X0] : (cAgamidae(X0) & cAmphisbaenidae(X0)) | ? [X1] : (cSphenodontidae(X1) & cLeptotyphlopidae(X1)) | ? [X2] : (cLoxocemidae(X2) & cEmydidae(X2)) | ? [X3] : (cLoxocemidae(X3) & cLeptotyphlopidae(X3)) | ? [X4] : (cCordylidae(X4) & cAgamidae(X4)) | ? [X5] : (cAgamidae(X5) & cGekkonidae(X5)) | ? [X6] : (cXantusiidae(X6) & cLeptotyphlopidae(X6)) | ? [X7] : (cSphenodontidae(X7) & cAnomalepidae(X7)) | ? [X8] : (cAnomalepidae(X8) & cXantusiidae(X8)) | ? [X9] : (cCrocodylidae(X9) & cCordylidae(X9)) | ? [X10] : (cAnomalepidae(X10) & cLeptotyphlopidae(X10)) | ? [X11] : (cLoxocemidae(X11) & cAnomalepidae(X11)) | ? [X12] : (cLoxocemidae(X12) & cGekkonidae(X12)) | ? [X13] : (cAnomalepidae(X13) & cAmphisbaenidae(X13)) | ? [X14] : (cEmydidae(X14) & cLeptotyphlopidae(X14)) | ? [X15] : (cLoxocemidae(X15) & cSphenodontidae(X15)) | ? [X16] : (cEmydidae(X16) & cGekkonidae(X16)) | ? [X17] : (cAmphisbaenidae(X17) & cXantusiidae(X17)) | ? [X18] : (cCrocodylidae(X18) & cSphenodontidae(X18)) | ? [X19] : (cAgamidae(X19) & cAnomalepidae(X19)) | ? [X20] : (cCordylidae(X20) & cXantusiidae(X20)) | ? [X21] : (cAmphisbaenidae(X21) & cBipedidae(X21)) | ? [X22] : (cCordylidae(X22) & cBipedidae(X22)) | ? [X23] : (cAnomalepidae(X23) & cGekkonidae(X23)) | ? [X24] : (cCordylidae(X24) & cLeptotyphlopidae(X24)) | ? [X25] : (cEmydidae(X25) & cSphenodontidae(X25)) | ? [X26] : (cAmphisbaenidae(X26) & cLeptotyphlopidae(X26)) | ? [X27] : (cSphenodontidae(X27) & cXantusiidae(X27)) | ? [X28] : (cEmydidae(X28) & cAnomalepidae(X28)) | ? [X29] : (cBipedidae(X29) & cXantusiidae(X29)) | ? [X30] : (cGekkonidae(X30) & cXantusiidae(X30)) | ? [X31] : (cLoxocemidae(X31) & cAgamidae(X31)) | ? [X32] : (cEmydidae(X32) & cCordylidae(X32)) | ? [X33] : (cEmydidae(X33) & cAgamidae(X33)) | ? [X34] : (cCordylidae(X34) & cAnomalepidae(X34)) | ? [X35] : (cAgamidae(X35) & cXantusiidae(X35)) | ? [X36] : (cCordylidae(X36) & cGekkonidae(X36)) | ? [X37] : (cLoxocemidae(X37) & cCordylidae(X37)) | ? [X38] : (cCordylidae(X38) & cAmphisbaenidae(X38)) | ? [X39] : (cCordylidae(X39) & cSphenodontidae(X39)) | ? [X40] : (cCrocodylidae(X40) & cLeptotyphlopidae(X40)) | ? [X41] : (cAmphisbaenidae(X41) & cGekkonidae(X41)) | ? [X42] : (cAgamidae(X42) & cBipedidae(X42)) | ? [X43] : (cLoxocemidae(X43) & cBipedidae(X43)) | ? [X44] : (cEmydidae(X44) & cXantusiidae(X44)) | ? [X45] : (cLoxocemidae(X45) & cXantusiidae(X45)) | ? [X46] : (cCrocodylidae(X46) & cAgamidae(X46)) | ? [X47] : (cEmydidae(X47) & cAmphisbaenidae(X47)) | ? [X48] : (cEmydidae(X48) & cBipedidae(X48)) | ? [X49] : (cCrocodylidae(X49) & cXantusiidae(X49)) | ? [X50] : (cLoxocemidae(X50) & cCrocodylidae(X50)) | ? [X51] : (cCrocodylidae(X51) & cAmphisbaenidae(X51)) | ? [X52] : (cAgamidae(X52) & cLeptotyphlopidae(X52)) | ? [X53] : (cLoxocemidae(X53) & cAmphisbaenidae(X53)) | ? [X54] : (cEmydidae(X54) & cCrocodylidae(X54)) | ? [X55] : (cCrocodylidae(X55) & cAnomalepidae(X55)) | ? [X56] : (cSphenodontidae(X56) & cAgamidae(X56)) | ? [X57] : (cSphenodontidae(X57) & cGekkonidae(X57)) | ? [X58] : (cCrocodylidae(X58) & cGekkonidae(X58)) | ? [X59] : (cSphenodontidae(X59) & cBipedidae(X59)) | ? [X60] : (cGekkonidae(X60) & cBipedidae(X60)) | ? [X61] : (cCrocodylidae(X61) & cBipedidae(X61)) | ? [X62] : (cSphenodontidae(X62) & cAmphisbaenidae(X62)) | ? [X63] : (cGekkonidae(X63) & cLeptotyphlopidae(X63)) | ? [X64] : (cAnomalepidae(X64) & cBipedidae(X64)) | ? [X65] : (cBipedidae(X65) & cLeptotyphlopidae(X65)) | ? [X66] : ~ (xsd_string(X66) <=> ~ xsd_integer(X66)) | ? [X67] : (cowlNothing(X67) | ~ cowlThing(X67))), inference(ennf_transformation, [], [f154])).
fof(f154, plain, ~ (! [X0] : ~ (cAgamidae(X0) & cAmphisbaenidae(X0)) & ! [X1] : ~ (cSphenodontidae(X1) & cLeptotyphlopidae(X1)) & ! [X2] : ~ (cLoxocemidae(X2) & cEmydidae(X2)) & ! [X3] : ~ (cLoxocemidae(X3) & cLeptotyphlopidae(X3)) & ! [X4] : ~ (cCordylidae(X4) & cAgamidae(X4)) & ! [X5] : ~ (cAgamidae(X5) & cGekkonidae(X5)) & ! [X6] : ~ (cXantusiidae(X6) & cLeptotyphlopidae(X6)) & ! [X7] : ~ (cSphenodontidae(X7) & cAnomalepidae(X7)) & ! [X8] : ~ (cAnomalepidae(X8) & cXantusiidae(X8)) & ! [X9] : ~ (cCrocodylidae(X9) & cCordylidae(X9)) & ! [X10] : ~ (cAnomalepidae(X10) & cLeptotyphlopidae(X10)) & ! [X11] : ~ (cLoxocemidae(X11) & cAnomalepidae(X11)) & ! [X12] : ~ (cLoxocemidae(X12) & cGekkonidae(X12)) & ! [X13] : ~ (cAnomalepidae(X13) & cAmphisbaenidae(X13)) & ! [X14] : ~ (cEmydidae(X14) & cLeptotyphlopidae(X14)) & ! [X15] : ~ (cLoxocemidae(X15) & cSphenodontidae(X15)) & ! [X16] : ~ (cEmydidae(X16) & cGekkonidae(X16)) & ! [X17] : ~ (cAmphisbaenidae(X17) & cXantusiidae(X17)) & ! [X18] : ~ (cCrocodylidae(X18) & cSphenodontidae(X18)) & ! [X19] : ~ (cAgamidae(X19) & cAnomalepidae(X19)) & ! [X20] : ~ (cCordylidae(X20) & cXantusiidae(X20)) & ! [X21] : ~ (cAmphisbaenidae(X21) & cBipedidae(X21)) & ! [X22] : ~ (cCordylidae(X22) & cBipedidae(X22)) & ! [X23] : ~ (cAnomalepidae(X23) & cGekkonidae(X23)) & ! [X24] : ~ (cCordylidae(X24) & cLeptotyphlopidae(X24)) & ! [X25] : ~ (cEmydidae(X25) & cSphenodontidae(X25)) & ! [X26] : ~ (cAmphisbaenidae(X26) & cLeptotyphlopidae(X26)) & ! [X27] : ~ (cSphenodontidae(X27) & cXantusiidae(X27)) & ! [X28] : ~ (cEmydidae(X28) & cAnomalepidae(X28)) & ! [X29] : ~ (cBipedidae(X29) & cXantusiidae(X29)) & ! [X30] : ~ (cGekkonidae(X30) & cXantusiidae(X30)) & ! [X31] : ~ (cLoxocemidae(X31) & cAgamidae(X31)) & ! [X32] : ~ (cEmydidae(X32) & cCordylidae(X32)) & ! [X33] : ~ (cEmydidae(X33) & cAgamidae(X33)) & ! [X34] : ~ (cCordylidae(X34) & cAnomalepidae(X34)) & ! [X35] : ~ (cAgamidae(X35) & cXantusiidae(X35)) & ! [X36] : ~ (cCordylidae(X36) & cGekkonidae(X36)) & ! [X37] : ~ (cLoxocemidae(X37) & cCordylidae(X37)) & ! [X38] : ~ (cCordylidae(X38) & cAmphisbaenidae(X38)) & ! [X39] : ~ (cCordylidae(X39) & cSphenodontidae(X39)) & ! [X40] : ~ (cCrocodylidae(X40) & cLeptotyphlopidae(X40)) & ! [X41] : ~ (cAmphisbaenidae(X41) & cGekkonidae(X41)) & ! [X42] : ~ (cAgamidae(X42) & cBipedidae(X42)) & ! [X43] : ~ (cLoxocemidae(X43) & cBipedidae(X43)) & ! [X44] : ~ (cEmydidae(X44) & cXantusiidae(X44)) & ! [X45] : ~ (cLoxocemidae(X45) & cXantusiidae(X45)) & ! [X46] : ~ (cCrocodylidae(X46) & cAgamidae(X46)) & ! [X47] : ~ (cEmydidae(X47) & cAmphisbaenidae(X47)) & ! [X48] : ~ (cEmydidae(X48) & cBipedidae(X48)) & ! [X49] : ~ (cCrocodylidae(X49) & cXantusiidae(X49)) & ! [X50] : ~ (cLoxocemidae(X50) & cCrocodylidae(X50)) & ! [X51] : ~ (cCrocodylidae(X51) & cAmphisbaenidae(X51)) & ! [X52] : ~ (cAgamidae(X52) & cLeptotyphlopidae(X52)) & ! [X53] : ~ (cLoxocemidae(X53) & cAmphisbaenidae(X53)) & ! [X54] : ~ (cEmydidae(X54) & cCrocodylidae(X54)) & ! [X55] : ~ (cCrocodylidae(X55) & cAnomalepidae(X55)) & ! [X56] : ~ (cSphenodontidae(X56) & cAgamidae(X56)) & ! [X57] : ~ (cSphenodontidae(X57) & cGekkonidae(X57)) & ! [X58] : ~ (cCrocodylidae(X58) & cGekkonidae(X58)) & ! [X59] : ~ (cSphenodontidae(X59) & cBipedidae(X59)) & ! [X60] : ~ (cGekkonidae(X60) & cBipedidae(X60)) & ! [X61] : ~ (cCrocodylidae(X61) & cBipedidae(X61)) & ! [X62] : ~ (cSphenodontidae(X62) & cAmphisbaenidae(X62)) & ! [X63] : ~ (cGekkonidae(X63) & cLeptotyphlopidae(X63)) & ! [X64] : ~ (cAnomalepidae(X64) & cBipedidae(X64)) & ! [X65] : ~ (cBipedidae(X65) & cLeptotyphlopidae(X65)) & ! [X66] : (xsd_string(X66) <=> ~ xsd_integer(X66)) & ! [X67] : (~ cowlNothing(X67) & cowlThing(X67))), inference(rectify, [], [f126])).
fof(f126, plain, ~ (! [X3] : ~ (cAgamidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cEmydidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cXantusiidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cCordylidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cSphenodontidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cAmphisbaenidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cSphenodontidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cAmphisbaenidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cSphenodontidae(X3)) & ! [X3] : ~ (cAmphisbaenidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cBipedidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cGekkonidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cCordylidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cCordylidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cSphenodontidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cAmphisbaenidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cCrocodylidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cCrocodylidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cGekkonidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cGekkonidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cBipedidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), inference(negated_conjecture, [], [f125])).
fof(f125, plain, ~ (! [X3] : ~ (cAgamidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cEmydidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cXantusiidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cCordylidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cSphenodontidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cAmphisbaenidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cSphenodontidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cAmphisbaenidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cSphenodontidae(X3)) & ! [X3] : ~ (cAmphisbaenidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cBipedidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cGekkonidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cCordylidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cCordylidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cCordylidae(X3) & cSphenodontidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cAmphisbaenidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cXantusiidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cCrocodylidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cAgamidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cLoxocemidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cEmydidae(X3) & cCrocodylidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cAnomalepidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cAgamidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cGekkonidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cGekkonidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cCrocodylidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cSphenodontidae(X3) & cAmphisbaenidae(X3)) & ! [X3] : ~ (cGekkonidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : ~ (cAnomalepidae(X3) & cBipedidae(X3)) & ! [X3] : ~ (cBipedidae(X3) & cLeptotyphlopidae(X3)) & ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)) & ! [X3] : (~ cowlNothing(X3) & cowlThing(X3))), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', the_axiom)).
fof(f1770, plain, (spl133_190 | spl133_187 | spl133_184 | spl133_181 | spl133_178 | spl133_175 | spl133_172 | spl133_169 | spl133_166 | spl133_163 | spl133_160 | spl133_157 | spl133_154 | spl133_151 | spl133_148 | spl133_145 | spl133_142 | spl133_139 | spl133_136 | spl133_133 | spl133_130 | spl133_127 | spl133_124 | spl133_121 | spl133_118 | spl133_115 | spl133_112 | spl133_109 | spl133_106 | spl133_103 | spl133_100 | spl133_97 | spl133_94 | spl133_91 | spl133_88 | spl133_85 | spl133_82 | spl133_79 | spl133_76 | spl133_73 | spl133_70 | spl133_67 | spl133_64 | spl133_61 | spl133_58 | spl133_55 | spl133_52 | spl133_49 | spl133_46 | spl133_43 | spl133_40 | spl133_37 | spl133_34 | spl133_31 | spl133_28 | spl133_25 | spl133_22 | spl133_19 | spl133_16 | spl133_13 | spl133_10 | spl133_7 | spl133_4 | spl133_1 | spl133_192 | spl133_196 | spl133_197), inference(avatar_split_clause, [], [f1769, f1761, f1751, f1729, f833, f847, f861, f875, f889, f903, f917, f931, f945, f959, f973, f987, f1001, f1015, f1029, f1043, f1057, f1071, f1085, f1099, f1113, f1127, f1141, f1155, f1169, f1183, f1197, f1211, f1225, f1239, f1253, f1267, f1281, f1295, f1309, f1323, f1337, f1351, f1365, f1379, f1393, f1407, f1421, f1435, f1449, f1463, f1477, f1491, f1505, f1519, f1533, f1547, f1561, f1575, f1589, f1603, f1617, f1631, f1645, f1659, f1673, f1687, f1701, f1716])).
fof(f1769, plain, (cAmphisbaenidae(sK129) | cLeptotyphlopidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(subsumption_resolution, [], [f1768, f569])).
fof(f1768, plain, (cAmphisbaenidae(sK129) | cLeptotyphlopidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ~ cowlThing(sK132)), inference(subsumption_resolution, [], [f806, f570])).
fof(f806, plain, (cAmphisbaenidae(sK129) | cLeptotyphlopidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | cowlNothing(sK132) | ~ cowlThing(sK132)), inference(cnf_transformation, [], [f549])).
fof(f1767, plain, (spl133_190 | spl133_187 | spl133_184 | spl133_181 | spl133_178 | spl133_175 | spl133_172 | spl133_169 | spl133_166 | spl133_163 | spl133_160 | spl133_157 | spl133_154 | spl133_151 | spl133_148 | spl133_145 | spl133_142 | spl133_139 | spl133_136 | spl133_133 | spl133_130 | spl133_127 | spl133_124 | spl133_121 | spl133_118 | spl133_115 | spl133_112 | spl133_109 | spl133_106 | spl133_103 | spl133_100 | spl133_97 | spl133_94 | spl133_91 | spl133_88 | spl133_85 | spl133_82 | spl133_79 | spl133_76 | spl133_73 | spl133_70 | spl133_67 | spl133_64 | spl133_61 | spl133_58 | spl133_55 | spl133_52 | spl133_49 | spl133_46 | spl133_43 | spl133_40 | spl133_37 | spl133_34 | spl133_31 | spl133_28 | spl133_25 | spl133_22 | spl133_19 | spl133_16 | spl133_13 | spl133_10 | spl133_7 | spl133_4 | spl133_1 | spl133_195 | spl133_193 | spl133_197), inference(avatar_split_clause, [], [f1766, f1761, f1733, f1744, f833, f847, f861, f875, f889, f903, f917, f931, f945, f959, f973, f987, f1001, f1015, f1029, f1043, f1057, f1071, f1085, f1099, f1113, f1127, f1141, f1155, f1169, f1183, f1197, f1211, f1225, f1239, f1253, f1267, f1281, f1295, f1309, f1323, f1337, f1351, f1365, f1379, f1393, f1407, f1421, f1435, f1449, f1463, f1477, f1491, f1505, f1519, f1533, f1547, f1561, f1575, f1589, f1603, f1617, f1631, f1645, f1659, f1673, f1687, f1701, f1716])).
fof(f1766, plain, (cAmphisbaenidae(sK129) | cSphenodontidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(subsumption_resolution, [], [f1765, f569])).
fof(f1765, plain, (cAmphisbaenidae(sK129) | cSphenodontidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ~ cowlThing(sK132)), inference(subsumption_resolution, [], [f807, f570])).
fof(f807, plain, (cAmphisbaenidae(sK129) | cSphenodontidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | cowlNothing(sK132) | ~ cowlThing(sK132)), inference(cnf_transformation, [], [f549])).
fof(f1764, plain, (spl133_190 | spl133_187 | spl133_184 | spl133_181 | spl133_178 | spl133_175 | spl133_172 | spl133_169 | spl133_166 | spl133_163 | spl133_160 | spl133_157 | spl133_154 | spl133_151 | spl133_148 | spl133_145 | spl133_142 | spl133_139 | spl133_136 | spl133_133 | spl133_130 | spl133_127 | spl133_124 | spl133_121 | spl133_118 | spl133_115 | spl133_112 | spl133_109 | spl133_106 | spl133_103 | spl133_100 | spl133_97 | spl133_94 | spl133_91 | spl133_88 | spl133_85 | spl133_82 | spl133_79 | spl133_76 | spl133_73 | spl133_70 | spl133_67 | spl133_64 | spl133_61 | spl133_58 | spl133_55 | spl133_52 | spl133_49 | spl133_46 | spl133_43 | spl133_40 | spl133_37 | spl133_34 | spl133_31 | spl133_28 | spl133_25 | spl133_22 | spl133_19 | spl133_16 | spl133_13 | spl133_10 | spl133_7 | spl133_4 | spl133_1 | spl133_192 | spl133_193 | spl133_197), inference(avatar_split_clause, [], [f1759, f1761, f1733, f1729, f833, f847, f861, f875, f889, f903, f917, f931, f945, f959, f973, f987, f1001, f1015, f1029, f1043, f1057, f1071, f1085, f1099, f1113, f1127, f1141, f1155, f1169, f1183, f1197, f1211, f1225, f1239, f1253, f1267, f1281, f1295, f1309, f1323, f1337, f1351, f1365, f1379, f1393, f1407, f1421, f1435, f1449, f1463, f1477, f1491, f1505, f1519, f1533, f1547, f1561, f1575, f1589, f1603, f1617, f1631, f1645, f1659, f1673, f1687, f1701, f1716])).
fof(f1759, plain, (cAmphisbaenidae(sK129) | cSphenodontidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(subsumption_resolution, [], [f1758, f569])).
fof(f1758, plain, (cAmphisbaenidae(sK129) | cSphenodontidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ~ cowlThing(sK132)), inference(subsumption_resolution, [], [f808, f570])).
fof(f808, plain, (cAmphisbaenidae(sK129) | cSphenodontidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | cowlNothing(sK132) | ~ cowlThing(sK132)), inference(cnf_transformation, [], [f549])).
fof(f1757, plain, (spl133_190 | spl133_187 | spl133_184 | spl133_181 | spl133_178 | spl133_175 | spl133_172 | spl133_169 | spl133_166 | spl133_163 | spl133_160 | spl133_157 | spl133_154 | spl133_151 | spl133_148 | spl133_145 | spl133_142 | spl133_139 | spl133_136 | spl133_133 | spl133_130 | spl133_127 | spl133_124 | spl133_121 | spl133_118 | spl133_115 | spl133_112 | spl133_109 | spl133_106 | spl133_103 | spl133_100 | spl133_97 | spl133_94 | spl133_91 | spl133_88 | spl133_85 | spl133_82 | spl133_79 | spl133_76 | spl133_73 | spl133_70 | spl133_67 | spl133_64 | spl133_61 | spl133_58 | spl133_55 | spl133_52 | spl133_49 | spl133_46 | spl133_43 | spl133_40 | spl133_37 | spl133_34 | spl133_31 | spl133_28 | spl133_25 | spl133_22 | spl133_19 | spl133_16 | spl133_13 | spl133_10 | spl133_7 | spl133_4 | spl133_1 | spl133_195 | spl133_196 | spl133_194), inference(avatar_split_clause, [], [f1756, f1737, f1751, f1744, f833, f847, f861, f875, f889, f903, f917, f931, f945, f959, f973, f987, f1001, f1015, f1029, f1043, f1057, f1071, f1085, f1099, f1113, f1127, f1141, f1155, f1169, f1183, f1197, f1211, f1225, f1239, f1253, f1267, f1281, f1295, f1309, f1323, f1337, f1351, f1365, f1379, f1393, f1407, f1421, f1435, f1449, f1463, f1477, f1491, f1505, f1519, f1533, f1547, f1561, f1575, f1589, f1603, f1617, f1631, f1645, f1659, f1673, f1687, f1701, f1716])).
fof(f1756, plain, (cAgamidae(sK129) | cLeptotyphlopidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(subsumption_resolution, [], [f1755, f569])).
fof(f1755, plain, (cAgamidae(sK129) | cLeptotyphlopidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ~ cowlThing(sK132)), inference(subsumption_resolution, [], [f809, f570])).
fof(f809, plain, (cAgamidae(sK129) | cLeptotyphlopidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | cowlNothing(sK132) | ~ cowlThing(sK132)), inference(cnf_transformation, [], [f549])).
fof(f1754, plain, (spl133_190 | spl133_187 | spl133_184 | spl133_181 | spl133_178 | spl133_175 | spl133_172 | spl133_169 | spl133_166 | spl133_163 | spl133_160 | spl133_157 | spl133_154 | spl133_151 | spl133_148 | spl133_145 | spl133_142 | spl133_139 | spl133_136 | spl133_133 | spl133_130 | spl133_127 | spl133_124 | spl133_121 | spl133_118 | spl133_115 | spl133_112 | spl133_109 | spl133_106 | spl133_103 | spl133_100 | spl133_97 | spl133_94 | spl133_91 | spl133_88 | spl133_85 | spl133_82 | spl133_79 | spl133_76 | spl133_73 | spl133_70 | spl133_67 | spl133_64 | spl133_61 | spl133_58 | spl133_55 | spl133_52 | spl133_49 | spl133_46 | spl133_43 | spl133_40 | spl133_37 | spl133_34 | spl133_31 | spl133_28 | spl133_25 | spl133_22 | spl133_19 | spl133_16 | spl133_13 | spl133_10 | spl133_7 | spl133_4 | spl133_1 | spl133_192 | spl133_196 | spl133_194), inference(avatar_split_clause, [], [f1749, f1737, f1751, f1729, f833, f847, f861, f875, f889, f903, f917, f931, f945, f959, f973, f987, f1001, f1015, f1029, f1043, f1057, f1071, f1085, f1099, f1113, f1127, f1141, f1155, f1169, f1183, f1197, f1211, f1225, f1239, f1253, f1267, f1281, f1295, f1309, f1323, f1337, f1351, f1365, f1379, f1393, f1407, f1421, f1435, f1449, f1463, f1477, f1491, f1505, f1519, f1533, f1547, f1561, f1575, f1589, f1603, f1617, f1631, f1645, f1659, f1673, f1687, f1701, f1716])).
fof(f1749, plain, (cAgamidae(sK129) | cLeptotyphlopidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(subsumption_resolution, [], [f1748, f569])).
fof(f1748, plain, (cAgamidae(sK129) | cLeptotyphlopidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ~ cowlThing(sK132)), inference(subsumption_resolution, [], [f810, f570])).
fof(f810, plain, (cAgamidae(sK129) | cLeptotyphlopidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | cowlNothing(sK132) | ~ cowlThing(sK132)), inference(cnf_transformation, [], [f549])).
fof(f1747, plain, (spl133_190 | spl133_187 | spl133_184 | spl133_181 | spl133_178 | spl133_175 | spl133_172 | spl133_169 | spl133_166 | spl133_163 | spl133_160 | spl133_157 | spl133_154 | spl133_151 | spl133_148 | spl133_145 | spl133_142 | spl133_139 | spl133_136 | spl133_133 | spl133_130 | spl133_127 | spl133_124 | spl133_121 | spl133_118 | spl133_115 | spl133_112 | spl133_109 | spl133_106 | spl133_103 | spl133_100 | spl133_97 | spl133_94 | spl133_91 | spl133_88 | spl133_85 | spl133_82 | spl133_79 | spl133_76 | spl133_73 | spl133_70 | spl133_67 | spl133_64 | spl133_61 | spl133_58 | spl133_55 | spl133_52 | spl133_49 | spl133_46 | spl133_43 | spl133_40 | spl133_37 | spl133_34 | spl133_31 | spl133_28 | spl133_25 | spl133_22 | spl133_19 | spl133_16 | spl133_13 | spl133_10 | spl133_7 | spl133_4 | spl133_1 | spl133_195 | spl133_193 | spl133_194), inference(avatar_split_clause, [], [f1742, f1737, f1733, f1744, f833, f847, f861, f875, f889, f903, f917, f931, f945, f959, f973, f987, f1001, f1015, f1029, f1043, f1057, f1071, f1085, f1099, f1113, f1127, f1141, f1155, f1169, f1183, f1197, f1211, f1225, f1239, f1253, f1267, f1281, f1295, f1309, f1323, f1337, f1351, f1365, f1379, f1393, f1407, f1421, f1435, f1449, f1463, f1477, f1491, f1505, f1519, f1533, f1547, f1561, f1575, f1589, f1603, f1617, f1631, f1645, f1659, f1673, f1687, f1701, f1716])).
fof(f1742, plain, (cAgamidae(sK129) | cSphenodontidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(subsumption_resolution, [], [f1741, f569])).
fof(f1741, plain, (cAgamidae(sK129) | cSphenodontidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ~ cowlThing(sK132)), inference(subsumption_resolution, [], [f811, f570])).
fof(f811, plain, (cAgamidae(sK129) | cSphenodontidae(sK130) | cEmydidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | cowlNothing(sK132) | ~ cowlThing(sK132)), inference(cnf_transformation, [], [f549])).
fof(f1740, plain, (spl133_190 | spl133_187 | spl133_184 | spl133_181 | spl133_178 | spl133_175 | spl133_172 | spl133_169 | spl133_166 | spl133_163 | spl133_160 | spl133_157 | spl133_154 | spl133_151 | spl133_148 | spl133_145 | spl133_142 | spl133_139 | spl133_136 | spl133_133 | spl133_130 | spl133_127 | spl133_124 | spl133_121 | spl133_118 | spl133_115 | spl133_112 | spl133_109 | spl133_106 | spl133_103 | spl133_100 | spl133_97 | spl133_94 | spl133_91 | spl133_88 | spl133_85 | spl133_82 | spl133_79 | spl133_76 | spl133_73 | spl133_70 | spl133_67 | spl133_64 | spl133_61 | spl133_58 | spl133_55 | spl133_52 | spl133_49 | spl133_46 | spl133_43 | spl133_40 | spl133_37 | spl133_34 | spl133_31 | spl133_28 | spl133_25 | spl133_22 | spl133_19 | spl133_16 | spl133_13 | spl133_10 | spl133_7 | spl133_4 | spl133_1 | spl133_192 | spl133_193 | spl133_194), inference(avatar_split_clause, [], [f1727, f1737, f1733, f1729, f833, f847, f861, f875, f889, f903, f917, f931, f945, f959, f973, f987, f1001, f1015, f1029, f1043, f1057, f1071, f1085, f1099, f1113, f1127, f1141, f1155, f1169, f1183, f1197, f1211, f1225, f1239, f1253, f1267, f1281, f1295, f1309, f1323, f1337, f1351, f1365, f1379, f1393, f1407, f1421, f1435, f1449, f1463, f1477, f1491, f1505, f1519, f1533, f1547, f1561, f1575, f1589, f1603, f1617, f1631, f1645, f1659, f1673, f1687, f1701, f1716])).
fof(f1727, plain, (cAgamidae(sK129) | cSphenodontidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(subsumption_resolution, [], [f1726, f569])).
fof(f1726, plain, (cAgamidae(sK129) | cSphenodontidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | ~ cowlThing(sK132)), inference(subsumption_resolution, [], [f812, f570])).
fof(f812, plain, (cAgamidae(sK129) | cSphenodontidae(sK130) | cLoxocemidae(sK131) | sP63 | sP62 | sP61 | sP60 | sP59 | sP58 | sP57 | sP56 | sP55 | sP54 | sP53 | sP52 | sP51 | sP50 | sP49 | sP48 | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0 | cowlNothing(sK132) | ~ cowlThing(sK132)), inference(cnf_transformation, [], [f549])).
fof(f1725, plain, (~ spl133_190 | spl133_191), inference(avatar_split_clause, [], [f1724, f1720, f1716])).
fof(f1720, plain, (spl133_191 <=> xsd_string(sK128)), introduced(avatar_definition, [new_symbols(naming, [spl133_191])])).
fof(f1724, plain, (xsd_string(sK128) | ~ sP0), inference(subsumption_resolution, [], [f803, f572])).
fof(f572, plain, ! [X0] : (xsd_string(X0) | xsd_integer(X0)), inference(cnf_transformation, [], [f285])).
fof(f285, plain, ! [X0] : ((xsd_string(X0) | xsd_integer(X0)) & (~ xsd_integer(X0) | ~ xsd_string(X0))), inference(nnf_transformation, [], [f128])).
fof(f128, plain, ! [X0] : (xsd_string(X0) <=> ~ xsd_integer(X0)), inference(rectify, [], [f21])).
fof(f21, plain, ! [X3] : (xsd_string(X3) <=> ~ xsd_integer(X3)), file('/home/ubuntu/library/tptp/Problems/KRS/KRS133+1.p', axiom_1)).
fof(f803, plain, (~ xsd_integer(sK128) | xsd_string(sK128) | ~ sP0), inference(cnf_transformation, [], [f543])).
fof(f543, plain, (((xsd_integer(sK128) | ~ xsd_string(sK128)) & (~ xsd_integer(sK128) | xsd_string(sK128))) | ~ sP0), inference(skolemisation, [status(esa), new_symbols(skolem, [sK128])], [f541, f542])).
fof(f542, plain, (? [X0] : ((xsd_integer(X0) | ~ xsd_string(X0)) & (~ xsd_integer(X0) | xsd_string(X0))) => ((xsd_integer(sK128) | ~ xsd_string(sK128)) & (~ xsd_integer(sK128) | xsd_string(sK128)))), introduced(choice_axiom, [])).
fof(f541, plain, (? [X0] : ((xsd_integer(X0) | ~ xsd_string(X0)) & (~ xsd_integer(X0) | xsd_string(X0))) | ~ sP0), inference(rectify, [], [f540])).
fof(f540, plain, (? [X66] : ((xsd_integer(X66) | ~ xsd_string(X66)) & (~ xsd_integer(X66) | xsd_string(X66))) | ~ sP0), inference(nnf_transformation, [], [f220])).
fof(f1723, plain, (~ spl133_190 | ~ spl133_191), inference(avatar_split_clause, [], [f1714, f1720, f1716])).
fof(f1714, plain, (~ xsd_string(sK128) | ~ sP0), inference(subsumption_resolution, [], [f804, f571])).
fof(f571, plain, ! [X0] : (~ xsd_string(X0) | ~ xsd_integer(X0)), inference(cnf_transformation, [], [f285])).
fof(f804, plain, (xsd_integer(sK128) | ~ xsd_string(sK128) | ~ sP0), inference(cnf_transformation, [], [f543])).
fof(f1713, plain, (~ spl133_187 | spl133_189), inference(avatar_split_clause, [], [f801, f1710, f1701])).
fof(f801, plain, (cLeptotyphlopidae(sK127) | ~ sP1), inference(cnf_transformation, [], [f539])).
fof(f539, plain, ((cBipedidae(sK127) & cLeptotyphlopidae(sK127)) | ~ sP1), inference(skolemisation, [status(esa), new_symbols(skolem, [sK127])], [f537, f538])).
fof(f538, plain, (? [X0] : (cBipedidae(X0) & cLeptotyphlopidae(X0)) => (cBipedidae(sK127) & cLeptotyphlopidae(sK127))), introduced(choice_axiom, [])).
fof(f537, plain, (? [X0] : (cBipedidae(X0) & cLeptotyphlopidae(X0)) | ~ sP1), inference(rectify, [], [f536])).
fof(f536, plain, (? [X65] : (cBipedidae(X65) & cLeptotyphlopidae(X65)) | ~ sP1), inference(nnf_transformation, [], [f221])).
fof(f1708, plain, (~ spl133_187 | spl133_188), inference(avatar_split_clause, [], [f802, f1705, f1701])).
fof(f802, plain, (cBipedidae(sK127) | ~ sP1), inference(cnf_transformation, [], [f539])).
fof(f1699, plain, (~ spl133_184 | spl133_186), inference(avatar_split_clause, [], [f799, f1696, f1687])).
fof(f799, plain, (cBipedidae(sK126) | ~ sP2), inference(cnf_transformation, [], [f535])).
fof(f535, plain, ((cAnomalepidae(sK126) & cBipedidae(sK126)) | ~ sP2), inference(skolemisation, [status(esa), new_symbols(skolem, [sK126])], [f533, f534])).
fof(f534, plain, (? [X0] : (cAnomalepidae(X0) & cBipedidae(X0)) => (cAnomalepidae(sK126) & cBipedidae(sK126))), introduced(choice_axiom, [])).
fof(f533, plain, (? [X0] : (cAnomalepidae(X0) & cBipedidae(X0)) | ~ sP2), inference(rectify, [], [f532])).
fof(f532, plain, (? [X64] : (cAnomalepidae(X64) & cBipedidae(X64)) | ~ sP2), inference(nnf_transformation, [], [f222])).
fof(f1694, plain, (~ spl133_184 | spl133_185), inference(avatar_split_clause, [], [f800, f1691, f1687])).
fof(f800, plain, (cAnomalepidae(sK126) | ~ sP2), inference(cnf_transformation, [], [f535])).
fof(f1685, plain, (~ spl133_181 | spl133_183), inference(avatar_split_clause, [], [f797, f1682, f1673])).
fof(f797, plain, (cLeptotyphlopidae(sK125) | ~ sP3), inference(cnf_transformation, [], [f531])).
fof(f531, plain, ((cGekkonidae(sK125) & cLeptotyphlopidae(sK125)) | ~ sP3), inference(skolemisation, [status(esa), new_symbols(skolem, [sK125])], [f529, f530])).
fof(f530, plain, (? [X0] : (cGekkonidae(X0) & cLeptotyphlopidae(X0)) => (cGekkonidae(sK125) & cLeptotyphlopidae(sK125))), introduced(choice_axiom, [])).
fof(f529, plain, (? [X0] : (cGekkonidae(X0) & cLeptotyphlopidae(X0)) | ~ sP3), inference(rectify, [], [f528])).
fof(f528, plain, (? [X63] : (cGekkonidae(X63) & cLeptotyphlopidae(X63)) | ~ sP3), inference(nnf_transformation, [], [f223])).
fof(f1680, plain, (~ spl133_181 | spl133_182), inference(avatar_split_clause, [], [f798, f1677, f1673])).
fof(f798, plain, (cGekkonidae(sK125) | ~ sP3), inference(cnf_transformation, [], [f531])).
fof(f1671, plain, (~ spl133_178 | spl133_180), inference(avatar_split_clause, [], [f795, f1668, f1659])).
fof(f795, plain, (cAmphisbaenidae(sK124) | ~ sP4), inference(cnf_transformation, [], [f527])).
fof(f527, plain, ((cSphenodontidae(sK124) & cAmphisbaenidae(sK124)) | ~ sP4), inference(skolemisation, [status(esa), new_symbols(skolem, [sK124])], [f525, f526])).
fof(f526, plain, (? [X0] : (cSphenodontidae(X0) & cAmphisbaenidae(X0)) => (cSphenodontidae(sK124) & cAmphisbaenidae(sK124))), introduced(choice_axiom, [])).
fof(f525, plain, (? [X0] : (cSphenodontidae(X0) & cAmphisbaenidae(X0)) | ~ sP4), inference(rectify, [], [f524])).
fof(f524, plain, (? [X62] : (cSphenodontidae(X62) & cAmphisbaenidae(X62)) | ~ sP4), inference(nnf_transformation, [], [f224])).
fof(f1666, plain, (~ spl133_178 | spl133_179), inference(avatar_split_clause, [], [f796, f1663, f1659])).
fof(f796, plain, (cSphenodontidae(sK124) | ~ sP4), inference(cnf_transformation, [], [f527])).
fof(f1657, plain, (~ spl133_175 | spl133_177), inference(avatar_split_clause, [], [f793, f1654, f1645])).
fof(f793, plain, (cBipedidae(sK123) | ~ sP5), inference(cnf_transformation, [], [f523])).
fof(f523, plain, ((cCrocodylidae(sK123) & cBipedidae(sK123)) | ~ sP5), inference(skolemisation, [status(esa), new_symbols(skolem, [sK123])], [f521, f522])).
fof(f522, plain, (? [X0] : (cCrocodylidae(X0) & cBipedidae(X0)) => (cCrocodylidae(sK123) & cBipedidae(sK123))), introduced(choice_axiom, [])).
fof(f521, plain, (? [X0] : (cCrocodylidae(X0) & cBipedidae(X0)) | ~ sP5), inference(rectify, [], [f520])).
fof(f520, plain, (? [X61] : (cCrocodylidae(X61) & cBipedidae(X61)) | ~ sP5), inference(nnf_transformation, [], [f225])).
fof(f1652, plain, (~ spl133_175 | spl133_176), inference(avatar_split_clause, [], [f794, f1649, f1645])).
fof(f794, plain, (cCrocodylidae(sK123) | ~ sP5), inference(cnf_transformation, [], [f523])).
fof(f1643, plain, (~ spl133_172 | spl133_174), inference(avatar_split_clause, [], [f791, f1640, f1631])).
fof(f791, plain, (cBipedidae(sK122) | ~ sP6), inference(cnf_transformation, [], [f519])).
fof(f519, plain, ((cGekkonidae(sK122) & cBipedidae(sK122)) | ~ sP6), inference(skolemisation, [status(esa), new_symbols(skolem, [sK122])], [f517, f518])).
fof(f518, plain, (? [X0] : (cGekkonidae(X0) & cBipedidae(X0)) => (cGekkonidae(sK122) & cBipedidae(sK122))), introduced(choice_axiom, [])).
fof(f517, plain, (? [X0] : (cGekkonidae(X0) & cBipedidae(X0)) | ~ sP6), inference(rectify, [], [f516])).
fof(f516, plain, (? [X60] : (cGekkonidae(X60) & cBipedidae(X60)) | ~ sP6), inference(nnf_transformation, [], [f226])).
fof(f1638, plain, (~ spl133_172 | spl133_173), inference(avatar_split_clause, [], [f792, f1635, f1631])).
fof(f792, plain, (cGekkonidae(sK122) | ~ sP6), inference(cnf_transformation, [], [f519])).
fof(f1629, plain, (~ spl133_169 | spl133_171), inference(avatar_split_clause, [], [f789, f1626, f1617])).
fof(f789, plain, (cBipedidae(sK121) | ~ sP7), inference(cnf_transformation, [], [f515])).
fof(f515, plain, ((cSphenodontidae(sK121) & cBipedidae(sK121)) | ~ sP7), inference(skolemisation, [status(esa), new_symbols(skolem, [sK121])], [f513, f514])).
fof(f514, plain, (? [X0] : (cSphenodontidae(X0) & cBipedidae(X0)) => (cSphenodontidae(sK121) & cBipedidae(sK121))), introduced(choice_axiom, [])).
fof(f513, plain, (? [X0] : (cSphenodontidae(X0) & cBipedidae(X0)) | ~ sP7), inference(rectify, [], [f512])).
fof(f512, plain, (? [X59] : (cSphenodontidae(X59) & cBipedidae(X59)) | ~ sP7), inference(nnf_transformation, [], [f227])).
fof(f1624, plain, (~ spl133_169 | spl133_170), inference(avatar_split_clause, [], [f790, f1621, f1617])).
fof(f790, plain, (cSphenodontidae(sK121) | ~ sP7), inference(cnf_transformation, [], [f515])).
fof(f1615, plain, (~ spl133_166 | spl133_168), inference(avatar_split_clause, [], [f787, f1612, f1603])).
fof(f787, plain, (cGekkonidae(sK120) | ~ sP8), inference(cnf_transformation, [], [f511])).
fof(f511, plain, ((cCrocodylidae(sK120) & cGekkonidae(sK120)) | ~ sP8), inference(skolemisation, [status(esa), new_symbols(skolem, [sK120])], [f509, f510])).
fof(f510, plain, (? [X0] : (cCrocodylidae(X0) & cGekkonidae(X0)) => (cCrocodylidae(sK120) & cGekkonidae(sK120))), introduced(choice_axiom, [])).
fof(f509, plain, (? [X0] : (cCrocodylidae(X0) & cGekkonidae(X0)) | ~ sP8), inference(rectify, [], [f508])).
fof(f508, plain, (? [X58] : (cCrocodylidae(X58) & cGekkonidae(X58)) | ~ sP8), inference(nnf_transformation, [], [f228])).
fof(f1610, plain, (~ spl133_166 | spl133_167), inference(avatar_split_clause, [], [f788, f1607, f1603])).
fof(f788, plain, (cCrocodylidae(sK120) | ~ sP8), inference(cnf_transformation, [], [f511])).
fof(f1601, plain, (~ spl133_163 | spl133_165), inference(avatar_split_clause, [], [f785, f1598, f1589])).
fof(f785, plain, (cGekkonidae(sK119) | ~ sP9), inference(cnf_transformation, [], [f507])).
fof(f507, plain, ((cSphenodontidae(sK119) & cGekkonidae(sK119)) | ~ sP9), inference(skolemisation, [status(esa), new_symbols(skolem, [sK119])], [f505, f506])).
fof(f506, plain, (? [X0] : (cSphenodontidae(X0) & cGekkonidae(X0)) => (cSphenodontidae(sK119) & cGekkonidae(sK119))), introduced(choice_axiom, [])).
fof(f505, plain, (? [X0] : (cSphenodontidae(X0) & cGekkonidae(X0)) | ~ sP9), inference(rectify, [], [f504])).
fof(f504, plain, (? [X57] : (cSphenodontidae(X57) & cGekkonidae(X57)) | ~ sP9), inference(nnf_transformation, [], [f229])).
fof(f1596, plain, (~ spl133_163 | spl133_164), inference(avatar_split_clause, [], [f786, f1593, f1589])).
fof(f786, plain, (cSphenodontidae(sK119) | ~ sP9), inference(cnf_transformation, [], [f507])).
fof(f1587, plain, (~ spl133_160 | spl133_162), inference(avatar_split_clause, [], [f783, f1584, f1575])).
fof(f783, plain, (cAgamidae(sK118) | ~ sP10), inference(cnf_transformation, [], [f503])).
fof(f503, plain, ((cSphenodontidae(sK118) & cAgamidae(sK118)) | ~ sP10), inference(skolemisation, [status(esa), new_symbols(skolem, [sK118])], [f501, f502])).
fof(f502, plain, (? [X0] : (cSphenodontidae(X0) & cAgamidae(X0)) => (cSphenodontidae(sK118) & cAgamidae(sK118))), introduced(choice_axiom, [])).
fof(f501, plain, (? [X0] : (cSphenodontidae(X0) & cAgamidae(X0)) | ~ sP10), inference(rectify, [], [f500])).
fof(f500, plain, (? [X56] : (cSphenodontidae(X56) & cAgamidae(X56)) | ~ sP10), inference(nnf_transformation, [], [f230])).
fof(f1582, plain, (~ spl133_160 | spl133_161), inference(avatar_split_clause, [], [f784, f1579, f1575])).
fof(f784, plain, (cSphenodontidae(sK118) | ~ sP10), inference(cnf_transformation, [], [f503])).
fof(f1573, plain, (~ spl133_157 | spl133_159), inference(avatar_split_clause, [], [f781, f1570, f1561])).
fof(f781, plain, (cAnomalepidae(sK117) | ~ sP11), inference(cnf_transformation, [], [f499])).
fof(f499, plain, ((cCrocodylidae(sK117) & cAnomalepidae(sK117)) | ~ sP11), inference(skolemisation, [status(esa), new_symbols(skolem, [sK117])], [f497, f498])).
fof(f498, plain, (? [X0] : (cCrocodylidae(X0) & cAnomalepidae(X0)) => (cCrocodylidae(sK117) & cAnomalepidae(sK117))), introduced(choice_axiom, [])).
fof(f497, plain, (? [X0] : (cCrocodylidae(X0) & cAnomalepidae(X0)) | ~ sP11), inference(rectify, [], [f496])).
fof(f496, plain, (? [X55] : (cCrocodylidae(X55) & cAnomalepidae(X55)) | ~ sP11), inference(nnf_transformation, [], [f231])).
fof(f1568, plain, (~ spl133_157 | spl133_158), inference(avatar_split_clause, [], [f782, f1565, f1561])).
fof(f782, plain, (cCrocodylidae(sK117) | ~ sP11), inference(cnf_transformation, [], [f499])).
fof(f1559, plain, (~ spl133_154 | spl133_156), inference(avatar_split_clause, [], [f779, f1556, f1547])).
fof(f779, plain, (cCrocodylidae(sK116) | ~ sP12), inference(cnf_transformation, [], [f495])).
fof(f495, plain, ((cEmydidae(sK116) & cCrocodylidae(sK116)) | ~ sP12), inference(skolemisation, [status(esa), new_symbols(skolem, [sK116])], [f493, f494])).
fof(f494, plain, (? [X0] : (cEmydidae(X0) & cCrocodylidae(X0)) => (cEmydidae(sK116) & cCrocodylidae(sK116))), introduced(choice_axiom, [])).
fof(f493, plain, (? [X0] : (cEmydidae(X0) & cCrocodylidae(X0)) | ~ sP12), inference(rectify, [], [f492])).
fof(f492, plain, (? [X54] : (cEmydidae(X54) & cCrocodylidae(X54)) | ~ sP12), inference(nnf_transformation, [], [f232])).
fof(f1554, plain, (~ spl133_154 | spl133_155), inference(avatar_split_clause, [], [f780, f1551, f1547])).
fof(f780, plain, (cEmydidae(sK116) | ~ sP12), inference(cnf_transformation, [], [f495])).
fof(f1545, plain, (~ spl133_151 | spl133_153), inference(avatar_split_clause, [], [f777, f1542, f1533])).
fof(f777, plain, (cAmphisbaenidae(sK115) | ~ sP13), inference(cnf_transformation, [], [f491])).
fof(f491, plain, ((cLoxocemidae(sK115) & cAmphisbaenidae(sK115)) | ~ sP13), inference(skolemisation, [status(esa), new_symbols(skolem, [sK115])], [f489, f490])).
fof(f490, plain, (? [X0] : (cLoxocemidae(X0) & cAmphisbaenidae(X0)) => (cLoxocemidae(sK115) & cAmphisbaenidae(sK115))), introduced(choice_axiom, [])).
fof(f489, plain, (? [X0] : (cLoxocemidae(X0) & cAmphisbaenidae(X0)) | ~ sP13), inference(rectify, [], [f488])).
fof(f488, plain, (? [X53] : (cLoxocemidae(X53) & cAmphisbaenidae(X53)) | ~ sP13), inference(nnf_transformation, [], [f233])).
fof(f1540, plain, (~ spl133_151 | spl133_152), inference(avatar_split_clause, [], [f778, f1537, f1533])).
fof(f778, plain, (cLoxocemidae(sK115) | ~ sP13), inference(cnf_transformation, [], [f491])).
fof(f1531, plain, (~ spl133_148 | spl133_150), inference(avatar_split_clause, [], [f775, f1528, f1519])).
fof(f775, plain, (cLeptotyphlopidae(sK114) | ~ sP14), inference(cnf_transformation, [], [f487])).
fof(f487, plain, ((cAgamidae(sK114) & cLeptotyphlopidae(sK114)) | ~ sP14), inference(skolemisation, [status(esa), new_symbols(skolem, [sK114])], [f485, f486])).
fof(f486, plain, (? [X0] : (cAgamidae(X0) & cLeptotyphlopidae(X0)) => (cAgamidae(sK114) & cLeptotyphlopidae(sK114))), introduced(choice_axiom, [])).
fof(f485, plain, (? [X0] : (cAgamidae(X0) & cLeptotyphlopidae(X0)) | ~ sP14), inference(rectify, [], [f484])).
fof(f484, plain, (? [X52] : (cAgamidae(X52) & cLeptotyphlopidae(X52)) | ~ sP14), inference(nnf_transformation, [], [f234])).
fof(f1526, plain, (~ spl133_148 | spl133_149), inference(avatar_split_clause, [], [f776, f1523, f1519])).
fof(f776, plain, (cAgamidae(sK114) | ~ sP14), inference(cnf_transformation, [], [f487])).
fof(f1517, plain, (~ spl133_145 | spl133_147), inference(avatar_split_clause, [], [f773, f1514, f1505])).
fof(f773, plain, (cAmphisbaenidae(sK113) | ~ sP15), inference(cnf_transformation, [], [f483])).
fof(f483, plain, ((cCrocodylidae(sK113) & cAmphisbaenidae(sK113)) | ~ sP15), inference(skolemisation, [status(esa), new_symbols(skolem, [sK113])], [f481, f482])).
fof(f482, plain, (? [X0] : (cCrocodylidae(X0) & cAmphisbaenidae(X0)) => (cCrocodylidae(sK113) & cAmphisbaenidae(sK113))), introduced(choice_axiom, [])).
fof(f481, plain, (? [X0] : (cCrocodylidae(X0) & cAmphisbaenidae(X0)) | ~ sP15), inference(rectify, [], [f480])).
fof(f480, plain, (? [X51] : (cCrocodylidae(X51) & cAmphisbaenidae(X51)) | ~ sP15), inference(nnf_transformation, [], [f235])).
fof(f1512, plain, (~ spl133_145 | spl133_146), inference(avatar_split_clause, [], [f774, f1509, f1505])).
fof(f774, plain, (cCrocodylidae(sK113) | ~ sP15), inference(cnf_transformation, [], [f483])).
fof(f1503, plain, (~ spl133_142 | spl133_144), inference(avatar_split_clause, [], [f771, f1500, f1491])).
fof(f771, plain, (cCrocodylidae(sK112) | ~ sP16), inference(cnf_transformation, [], [f479])).
fof(f479, plain, ((cLoxocemidae(sK112) & cCrocodylidae(sK112)) | ~ sP16), inference(skolemisation, [status(esa), new_symbols(skolem, [sK112])], [f477, f478])).
fof(f478, plain, (? [X0] : (cLoxocemidae(X0) & cCrocodylidae(X0)) => (cLoxocemidae(sK112) & cCrocodylidae(sK112))), introduced(choice_axiom, [])).
fof(f477, plain, (? [X0] : (cLoxocemidae(X0) & cCrocodylidae(X0)) | ~ sP16), inference(rectify, [], [f476])).
fof(f476, plain, (? [X50] : (cLoxocemidae(X50) & cCrocodylidae(X50)) | ~ sP16), inference(nnf_transformation, [], [f236])).
fof(f1498, plain, (~ spl133_142 | spl133_143), inference(avatar_split_clause, [], [f772, f1495, f1491])).
fof(f772, plain, (cLoxocemidae(sK112) | ~ sP16), inference(cnf_transformation, [], [f479])).
fof(f1489, plain, (~ spl133_139 | spl133_141), inference(avatar_split_clause, [], [f769, f1486, f1477])).
fof(f769, plain, (cXantusiidae(sK111) | ~ sP17), inference(cnf_transformation, [], [f475])).
fof(f475, plain, ((cCrocodylidae(sK111) & cXantusiidae(sK111)) | ~ sP17), inference(skolemisation, [status(esa), new_symbols(skolem, [sK111])], [f473, f474])).
fof(f474, plain, (? [X0] : (cCrocodylidae(X0) & cXantusiidae(X0)) => (cCrocodylidae(sK111) & cXantusiidae(sK111))), introduced(choice_axiom, [])).
fof(f473, plain, (? [X0] : (cCrocodylidae(X0) & cXantusiidae(X0)) | ~ sP17), inference(rectify, [], [f472])).
fof(f472, plain, (? [X49] : (cCrocodylidae(X49) & cXantusiidae(X49)) | ~ sP17), inference(nnf_transformation, [], [f237])).
fof(f1484, plain, (~ spl133_139 | spl133_140), inference(avatar_split_clause, [], [f770, f1481, f1477])).
fof(f770, plain, (cCrocodylidae(sK111) | ~ sP17), inference(cnf_transformation, [], [f475])).
fof(f1475, plain, (~ spl133_136 | spl133_138), inference(avatar_split_clause, [], [f767, f1472, f1463])).
fof(f767, plain, (cBipedidae(sK110) | ~ sP18), inference(cnf_transformation, [], [f471])).
fof(f471, plain, ((cEmydidae(sK110) & cBipedidae(sK110)) | ~ sP18), inference(skolemisation, [status(esa), new_symbols(skolem, [sK110])], [f469, f470])).
fof(f470, plain, (? [X0] : (cEmydidae(X0) & cBipedidae(X0)) => (cEmydidae(sK110) & cBipedidae(sK110))), introduced(choice_axiom, [])).
fof(f469, plain, (? [X0] : (cEmydidae(X0) & cBipedidae(X0)) | ~ sP18), inference(rectify, [], [f468])).
fof(f468, plain, (? [X48] : (cEmydidae(X48) & cBipedidae(X48)) | ~ sP18), inference(nnf_transformation, [], [f238])).
fof(f1470, plain, (~ spl133_136 | spl133_137), inference(avatar_split_clause, [], [f768, f1467, f1463])).
fof(f768, plain, (cEmydidae(sK110) | ~ sP18), inference(cnf_transformation, [], [f471])).
fof(f1461, plain, (~ spl133_133 | spl133_135), inference(avatar_split_clause, [], [f765, f1458, f1449])).
fof(f765, plain, (cAmphisbaenidae(sK109) | ~ sP19), inference(cnf_transformation, [], [f467])).
fof(f467, plain, ((cEmydidae(sK109) & cAmphisbaenidae(sK109)) | ~ sP19), inference(skolemisation, [status(esa), new_symbols(skolem, [sK109])], [f465, f466])).
fof(f466, plain, (? [X0] : (cEmydidae(X0) & cAmphisbaenidae(X0)) => (cEmydidae(sK109) & cAmphisbaenidae(sK109))), introduced(choice_axiom, [])).
fof(f465, plain, (? [X0] : (cEmydidae(X0) & cAmphisbaenidae(X0)) | ~ sP19), inference(rectify, [], [f464])).
fof(f464, plain, (? [X47] : (cEmydidae(X47) & cAmphisbaenidae(X47)) | ~ sP19), inference(nnf_transformation, [], [f239])).
fof(f1456, plain, (~ spl133_133 | spl133_134), inference(avatar_split_clause, [], [f766, f1453, f1449])).
fof(f766, plain, (cEmydidae(sK109) | ~ sP19), inference(cnf_transformation, [], [f467])).
fof(f1447, plain, (~ spl133_130 | spl133_132), inference(avatar_split_clause, [], [f763, f1444, f1435])).
fof(f763, plain, (cAgamidae(sK108) | ~ sP20), inference(cnf_transformation, [], [f463])).
fof(f463, plain, ((cCrocodylidae(sK108) & cAgamidae(sK108)) | ~ sP20), inference(skolemisation, [status(esa), new_symbols(skolem, [sK108])], [f461, f462])).
fof(f462, plain, (? [X0] : (cCrocodylidae(X0) & cAgamidae(X0)) => (cCrocodylidae(sK108) & cAgamidae(sK108))), introduced(choice_axiom, [])).
fof(f461, plain, (? [X0] : (cCrocodylidae(X0) & cAgamidae(X0)) | ~ sP20), inference(rectify, [], [f460])).
fof(f460, plain, (? [X46] : (cCrocodylidae(X46) & cAgamidae(X46)) | ~ sP20), inference(nnf_transformation, [], [f240])).
fof(f1442, plain, (~ spl133_130 | spl133_131), inference(avatar_split_clause, [], [f764, f1439, f1435])).
fof(f764, plain, (cCrocodylidae(sK108) | ~ sP20), inference(cnf_transformation, [], [f463])).
fof(f1433, plain, (~ spl133_127 | spl133_129), inference(avatar_split_clause, [], [f761, f1430, f1421])).
fof(f761, plain, (cXantusiidae(sK107) | ~ sP21), inference(cnf_transformation, [], [f459])).
fof(f459, plain, ((cLoxocemidae(sK107) & cXantusiidae(sK107)) | ~ sP21), inference(skolemisation, [status(esa), new_symbols(skolem, [sK107])], [f457, f458])).
fof(f458, plain, (? [X0] : (cLoxocemidae(X0) & cXantusiidae(X0)) => (cLoxocemidae(sK107) & cXantusiidae(sK107))), introduced(choice_axiom, [])).
fof(f457, plain, (? [X0] : (cLoxocemidae(X0) & cXantusiidae(X0)) | ~ sP21), inference(rectify, [], [f456])).
fof(f456, plain, (? [X45] : (cLoxocemidae(X45) & cXantusiidae(X45)) | ~ sP21), inference(nnf_transformation, [], [f241])).
fof(f1428, plain, (~ spl133_127 | spl133_128), inference(avatar_split_clause, [], [f762, f1425, f1421])).
fof(f762, plain, (cLoxocemidae(sK107) | ~ sP21), inference(cnf_transformation, [], [f459])).
fof(f1419, plain, (~ spl133_124 | spl133_126), inference(avatar_split_clause, [], [f759, f1416, f1407])).
fof(f759, plain, (cXantusiidae(sK106) | ~ sP22), inference(cnf_transformation, [], [f455])).
fof(f455, plain, ((cEmydidae(sK106) & cXantusiidae(sK106)) | ~ sP22), inference(skolemisation, [status(esa), new_symbols(skolem, [sK106])], [f453, f454])).
fof(f454, plain, (? [X0] : (cEmydidae(X0) & cXantusiidae(X0)) => (cEmydidae(sK106) & cXantusiidae(sK106))), introduced(choice_axiom, [])).
fof(f453, plain, (? [X0] : (cEmydidae(X0) & cXantusiidae(X0)) | ~ sP22), inference(rectify, [], [f452])).
fof(f452, plain, (? [X44] : (cEmydidae(X44) & cXantusiidae(X44)) | ~ sP22), inference(nnf_transformation, [], [f242])).
fof(f1414, plain, (~ spl133_124 | spl133_125), inference(avatar_split_clause, [], [f760, f1411, f1407])).
fof(f760, plain, (cEmydidae(sK106) | ~ sP22), inference(cnf_transformation, [], [f455])).
fof(f1405, plain, (~ spl133_121 | spl133_123), inference(avatar_split_clause, [], [f757, f1402, f1393])).
fof(f757, plain, (cBipedidae(sK105) | ~ sP23), inference(cnf_transformation, [], [f451])).
fof(f451, plain, ((cLoxocemidae(sK105) & cBipedidae(sK105)) | ~ sP23), inference(skolemisation, [status(esa), new_symbols(skolem, [sK105])], [f449, f450])).
fof(f450, plain, (? [X0] : (cLoxocemidae(X0) & cBipedidae(X0)) => (cLoxocemidae(sK105) & cBipedidae(sK105))), introduced(choice_axiom, [])).
fof(f449, plain, (? [X0] : (cLoxocemidae(X0) & cBipedidae(X0)) | ~ sP23), inference(rectify, [], [f448])).
fof(f448, plain, (? [X43] : (cLoxocemidae(X43) & cBipedidae(X43)) | ~ sP23), inference(nnf_transformation, [], [f243])).
fof(f1400, plain, (~ spl133_121 | spl133_122), inference(avatar_split_clause, [], [f758, f1397, f1393])).
fof(f758, plain, (cLoxocemidae(sK105) | ~ sP23), inference(cnf_transformation, [], [f451])).
fof(f1391, plain, (~ spl133_118 | spl133_120), inference(avatar_split_clause, [], [f755, f1388, f1379])).
fof(f755, plain, (cBipedidae(sK104) | ~ sP24), inference(cnf_transformation, [], [f447])).
fof(f447, plain, ((cAgamidae(sK104) & cBipedidae(sK104)) | ~ sP24), inference(skolemisation, [status(esa), new_symbols(skolem, [sK104])], [f445, f446])).
fof(f446, plain, (? [X0] : (cAgamidae(X0) & cBipedidae(X0)) => (cAgamidae(sK104) & cBipedidae(sK104))), introduced(choice_axiom, [])).
fof(f445, plain, (? [X0] : (cAgamidae(X0) & cBipedidae(X0)) | ~ sP24), inference(rectify, [], [f444])).
fof(f444, plain, (? [X42] : (cAgamidae(X42) & cBipedidae(X42)) | ~ sP24), inference(nnf_transformation, [], [f244])).
fof(f1386, plain, (~ spl133_118 | spl133_119), inference(avatar_split_clause, [], [f756, f1383, f1379])).
fof(f756, plain, (cAgamidae(sK104) | ~ sP24), inference(cnf_transformation, [], [f447])).
fof(f1377, plain, (~ spl133_115 | spl133_117), inference(avatar_split_clause, [], [f753, f1374, f1365])).
fof(f753, plain, (cGekkonidae(sK103) | ~ sP25), inference(cnf_transformation, [], [f443])).
fof(f443, plain, ((cAmphisbaenidae(sK103) & cGekkonidae(sK103)) | ~ sP25), inference(skolemisation, [status(esa), new_symbols(skolem, [sK103])], [f441, f442])).
fof(f442, plain, (? [X0] : (cAmphisbaenidae(X0) & cGekkonidae(X0)) => (cAmphisbaenidae(sK103) & cGekkonidae(sK103))), introduced(choice_axiom, [])).
fof(f441, plain, (? [X0] : (cAmphisbaenidae(X0) & cGekkonidae(X0)) | ~ sP25), inference(rectify, [], [f440])).
fof(f440, plain, (? [X41] : (cAmphisbaenidae(X41) & cGekkonidae(X41)) | ~ sP25), inference(nnf_transformation, [], [f245])).
fof(f1372, plain, (~ spl133_115 | spl133_116), inference(avatar_split_clause, [], [f754, f1369, f1365])).
fof(f754, plain, (cAmphisbaenidae(sK103) | ~ sP25), inference(cnf_transformation, [], [f443])).
fof(f1363, plain, (~ spl133_112 | spl133_114), inference(avatar_split_clause, [], [f751, f1360, f1351])).
fof(f751, plain, (cLeptotyphlopidae(sK102) | ~ sP26), inference(cnf_transformation, [], [f439])).
fof(f439, plain, ((cCrocodylidae(sK102) & cLeptotyphlopidae(sK102)) | ~ sP26), inference(skolemisation, [status(esa), new_symbols(skolem, [sK102])], [f437, f438])).
fof(f438, plain, (? [X0] : (cCrocodylidae(X0) & cLeptotyphlopidae(X0)) => (cCrocodylidae(sK102) & cLeptotyphlopidae(sK102))), introduced(choice_axiom, [])).
fof(f437, plain, (? [X0] : (cCrocodylidae(X0) & cLeptotyphlopidae(X0)) | ~ sP26), inference(rectify, [], [f436])).
fof(f436, plain, (? [X40] : (cCrocodylidae(X40) & cLeptotyphlopidae(X40)) | ~ sP26), inference(nnf_transformation, [], [f246])).
fof(f1358, plain, (~ spl133_112 | spl133_113), inference(avatar_split_clause, [], [f752, f1355, f1351])).
fof(f752, plain, (cCrocodylidae(sK102) | ~ sP26), inference(cnf_transformation, [], [f439])).
fof(f1349, plain, (~ spl133_109 | spl133_111), inference(avatar_split_clause, [], [f749, f1346, f1337])).
fof(f749, plain, (cSphenodontidae(sK101) | ~ sP27), inference(cnf_transformation, [], [f435])).
fof(f435, plain, ((cCordylidae(sK101) & cSphenodontidae(sK101)) | ~ sP27), inference(skolemisation, [status(esa), new_symbols(skolem, [sK101])], [f433, f434])).
fof(f434, plain, (? [X0] : (cCordylidae(X0) & cSphenodontidae(X0)) => (cCordylidae(sK101) & cSphenodontidae(sK101))), introduced(choice_axiom, [])).
fof(f433, plain, (? [X0] : (cCordylidae(X0) & cSphenodontidae(X0)) | ~ sP27), inference(rectify, [], [f432])).
fof(f432, plain, (? [X39] : (cCordylidae(X39) & cSphenodontidae(X39)) | ~ sP27), inference(nnf_transformation, [], [f247])).
fof(f1344, plain, (~ spl133_109 | spl133_110), inference(avatar_split_clause, [], [f750, f1341, f1337])).
fof(f750, plain, (cCordylidae(sK101) | ~ sP27), inference(cnf_transformation, [], [f435])).
fof(f1335, plain, (~ spl133_106 | spl133_108), inference(avatar_split_clause, [], [f747, f1332, f1323])).
fof(f747, plain, (cAmphisbaenidae(sK100) | ~ sP28), inference(cnf_transformation, [], [f431])).
fof(f431, plain, ((cCordylidae(sK100) & cAmphisbaenidae(sK100)) | ~ sP28), inference(skolemisation, [status(esa), new_symbols(skolem, [sK100])], [f429, f430])).
fof(f430, plain, (? [X0] : (cCordylidae(X0) & cAmphisbaenidae(X0)) => (cCordylidae(sK100) & cAmphisbaenidae(sK100))), introduced(choice_axiom, [])).
fof(f429, plain, (? [X0] : (cCordylidae(X0) & cAmphisbaenidae(X0)) | ~ sP28), inference(rectify, [], [f428])).
fof(f428, plain, (? [X38] : (cCordylidae(X38) & cAmphisbaenidae(X38)) | ~ sP28), inference(nnf_transformation, [], [f248])).
fof(f1330, plain, (~ spl133_106 | spl133_107), inference(avatar_split_clause, [], [f748, f1327, f1323])).
fof(f748, plain, (cCordylidae(sK100) | ~ sP28), inference(cnf_transformation, [], [f431])).
fof(f1321, plain, (~ spl133_103 | spl133_105), inference(avatar_split_clause, [], [f745, f1318, f1309])).
fof(f745, plain, (cCordylidae(sK99) | ~ sP29), inference(cnf_transformation, [], [f427])).
fof(f427, plain, ((cLoxocemidae(sK99) & cCordylidae(sK99)) | ~ sP29), inference(skolemisation, [status(esa), new_symbols(skolem, [sK99])], [f425, f426])).
fof(f426, plain, (? [X0] : (cLoxocemidae(X0) & cCordylidae(X0)) => (cLoxocemidae(sK99) & cCordylidae(sK99))), introduced(choice_axiom, [])).
fof(f425, plain, (? [X0] : (cLoxocemidae(X0) & cCordylidae(X0)) | ~ sP29), inference(rectify, [], [f424])).
fof(f424, plain, (? [X37] : (cLoxocemidae(X37) & cCordylidae(X37)) | ~ sP29), inference(nnf_transformation, [], [f249])).
fof(f1316, plain, (~ spl133_103 | spl133_104), inference(avatar_split_clause, [], [f746, f1313, f1309])).
fof(f746, plain, (cLoxocemidae(sK99) | ~ sP29), inference(cnf_transformation, [], [f427])).
fof(f1307, plain, (~ spl133_100 | spl133_102), inference(avatar_split_clause, [], [f743, f1304, f1295])).
fof(f743, plain, (cGekkonidae(sK98) | ~ sP30), inference(cnf_transformation, [], [f423])).
fof(f423, plain, ((cCordylidae(sK98) & cGekkonidae(sK98)) | ~ sP30), inference(skolemisation, [status(esa), new_symbols(skolem, [sK98])], [f421, f422])).
fof(f422, plain, (? [X0] : (cCordylidae(X0) & cGekkonidae(X0)) => (cCordylidae(sK98) & cGekkonidae(sK98))), introduced(choice_axiom, [])).
fof(f421, plain, (? [X0] : (cCordylidae(X0) & cGekkonidae(X0)) | ~ sP30), inference(rectify, [], [f420])).
fof(f420, plain, (? [X36] : (cCordylidae(X36) & cGekkonidae(X36)) | ~ sP30), inference(nnf_transformation, [], [f250])).
fof(f1302, plain, (~ spl133_100 | spl133_101), inference(avatar_split_clause, [], [f744, f1299, f1295])).
fof(f744, plain, (cCordylidae(sK98) | ~ sP30), inference(cnf_transformation, [], [f423])).
fof(f1293, plain, (~ spl133_97 | spl133_99), inference(avatar_split_clause, [], [f741, f1290, f1281])).
fof(f741, plain, (cXantusiidae(sK97) | ~ sP31), inference(cnf_transformation, [], [f419])).
fof(f419, plain, ((cAgamidae(sK97) & cXantusiidae(sK97)) | ~ sP31), inference(skolemisation, [status(esa), new_symbols(skolem, [sK97])], [f417, f418])).
fof(f418, plain, (? [X0] : (cAgamidae(X0) & cXantusiidae(X0)) => (cAgamidae(sK97) & cXantusiidae(sK97))), introduced(choice_axiom, [])).
fof(f417, plain, (? [X0] : (cAgamidae(X0) & cXantusiidae(X0)) | ~ sP31), inference(rectify, [], [f416])).
fof(f416, plain, (? [X35] : (cAgamidae(X35) & cXantusiidae(X35)) | ~ sP31), inference(nnf_transformation, [], [f251])).
fof(f1288, plain, (~ spl133_97 | spl133_98), inference(avatar_split_clause, [], [f742, f1285, f1281])).
fof(f742, plain, (cAgamidae(sK97) | ~ sP31), inference(cnf_transformation, [], [f419])).
fof(f1279, plain, (~ spl133_94 | spl133_96), inference(avatar_split_clause, [], [f739, f1276, f1267])).
fof(f739, plain, (cAnomalepidae(sK96) | ~ sP32), inference(cnf_transformation, [], [f415])).
fof(f415, plain, ((cCordylidae(sK96) & cAnomalepidae(sK96)) | ~ sP32), inference(skolemisation, [status(esa), new_symbols(skolem, [sK96])], [f413, f414])).
fof(f414, plain, (? [X0] : (cCordylidae(X0) & cAnomalepidae(X0)) => (cCordylidae(sK96) & cAnomalepidae(sK96))), introduced(choice_axiom, [])).
fof(f413, plain, (? [X0] : (cCordylidae(X0) & cAnomalepidae(X0)) | ~ sP32), inference(rectify, [], [f412])).
fof(f412, plain, (? [X34] : (cCordylidae(X34) & cAnomalepidae(X34)) | ~ sP32), inference(nnf_transformation, [], [f252])).
fof(f1274, plain, (~ spl133_94 | spl133_95), inference(avatar_split_clause, [], [f740, f1271, f1267])).
fof(f740, plain, (cCordylidae(sK96) | ~ sP32), inference(cnf_transformation, [], [f415])).
fof(f1265, plain, (~ spl133_91 | spl133_93), inference(avatar_split_clause, [], [f737, f1262, f1253])).
fof(f737, plain, (cAgamidae(sK95) | ~ sP33), inference(cnf_transformation, [], [f411])).
fof(f411, plain, ((cEmydidae(sK95) & cAgamidae(sK95)) | ~ sP33), inference(skolemisation, [status(esa), new_symbols(skolem, [sK95])], [f409, f410])).
fof(f410, plain, (? [X0] : (cEmydidae(X0) & cAgamidae(X0)) => (cEmydidae(sK95) & cAgamidae(sK95))), introduced(choice_axiom, [])).
fof(f409, plain, (? [X0] : (cEmydidae(X0) & cAgamidae(X0)) | ~ sP33), inference(rectify, [], [f408])).
fof(f408, plain, (? [X33] : (cEmydidae(X33) & cAgamidae(X33)) | ~ sP33), inference(nnf_transformation, [], [f253])).
fof(f1260, plain, (~ spl133_91 | spl133_92), inference(avatar_split_clause, [], [f738, f1257, f1253])).
fof(f738, plain, (cEmydidae(sK95) | ~ sP33), inference(cnf_transformation, [], [f411])).
fof(f1251, plain, (~ spl133_88 | spl133_90), inference(avatar_split_clause, [], [f735, f1248, f1239])).
fof(f735, plain, (cCordylidae(sK94) | ~ sP34), inference(cnf_transformation, [], [f407])).
fof(f407, plain, ((cEmydidae(sK94) & cCordylidae(sK94)) | ~ sP34), inference(skolemisation, [status(esa), new_symbols(skolem, [sK94])], [f405, f406])).
fof(f406, plain, (? [X0] : (cEmydidae(X0) & cCordylidae(X0)) => (cEmydidae(sK94) & cCordylidae(sK94))), introduced(choice_axiom, [])).
fof(f405, plain, (? [X0] : (cEmydidae(X0) & cCordylidae(X0)) | ~ sP34), inference(rectify, [], [f404])).
fof(f404, plain, (? [X32] : (cEmydidae(X32) & cCordylidae(X32)) | ~ sP34), inference(nnf_transformation, [], [f254])).
fof(f1246, plain, (~ spl133_88 | spl133_89), inference(avatar_split_clause, [], [f736, f1243, f1239])).
fof(f736, plain, (cEmydidae(sK94) | ~ sP34), inference(cnf_transformation, [], [f407])).
fof(f1237, plain, (~ spl133_85 | spl133_87), inference(avatar_split_clause, [], [f733, f1234, f1225])).
fof(f733, plain, (cAgamidae(sK93) | ~ sP35), inference(cnf_transformation, [], [f403])).
fof(f403, plain, ((cLoxocemidae(sK93) & cAgamidae(sK93)) | ~ sP35), inference(skolemisation, [status(esa), new_symbols(skolem, [sK93])], [f401, f402])).
fof(f402, plain, (? [X0] : (cLoxocemidae(X0) & cAgamidae(X0)) => (cLoxocemidae(sK93) & cAgamidae(sK93))), introduced(choice_axiom, [])).
fof(f401, plain, (? [X0] : (cLoxocemidae(X0) & cAgamidae(X0)) | ~ sP35), inference(rectify, [], [f400])).
fof(f400, plain, (? [X31] : (cLoxocemidae(X31) & cAgamidae(X31)) | ~ sP35), inference(nnf_transformation, [], [f255])).
fof(f1232, plain, (~ spl133_85 | spl133_86), inference(avatar_split_clause, [], [f734, f1229, f1225])).
fof(f734, plain, (cLoxocemidae(sK93) | ~ sP35), inference(cnf_transformation, [], [f403])).
fof(f1223, plain, (~ spl133_82 | spl133_84), inference(avatar_split_clause, [], [f731, f1220, f1211])).
fof(f731, plain, (cXantusiidae(sK92) | ~ sP36), inference(cnf_transformation, [], [f399])).
fof(f399, plain, ((cGekkonidae(sK92) & cXantusiidae(sK92)) | ~ sP36), inference(skolemisation, [status(esa), new_symbols(skolem, [sK92])], [f397, f398])).
fof(f398, plain, (? [X0] : (cGekkonidae(X0) & cXantusiidae(X0)) => (cGekkonidae(sK92) & cXantusiidae(sK92))), introduced(choice_axiom, [])).
fof(f397, plain, (? [X0] : (cGekkonidae(X0) & cXantusiidae(X0)) | ~ sP36), inference(rectify, [], [f396])).
fof(f396, plain, (? [X30] : (cGekkonidae(X30) & cXantusiidae(X30)) | ~ sP36), inference(nnf_transformation, [], [f256])).
fof(f1218, plain, (~ spl133_82 | spl133_83), inference(avatar_split_clause, [], [f732, f1215, f1211])).
fof(f732, plain, (cGekkonidae(sK92) | ~ sP36), inference(cnf_transformation, [], [f399])).
fof(f1209, plain, (~ spl133_79 | spl133_81), inference(avatar_split_clause, [], [f729, f1206, f1197])).
fof(f729, plain, (cXantusiidae(sK91) | ~ sP37), inference(cnf_transformation, [], [f395])).
fof(f395, plain, ((cBipedidae(sK91) & cXantusiidae(sK91)) | ~ sP37), inference(skolemisation, [status(esa), new_symbols(skolem, [sK91])], [f393, f394])).
fof(f394, plain, (? [X0] : (cBipedidae(X0) & cXantusiidae(X0)) => (cBipedidae(sK91) & cXantusiidae(sK91))), introduced(choice_axiom, [])).
fof(f393, plain, (? [X0] : (cBipedidae(X0) & cXantusiidae(X0)) | ~ sP37), inference(rectify, [], [f392])).
fof(f392, plain, (? [X29] : (cBipedidae(X29) & cXantusiidae(X29)) | ~ sP37), inference(nnf_transformation, [], [f257])).
fof(f1204, plain, (~ spl133_79 | spl133_80), inference(avatar_split_clause, [], [f730, f1201, f1197])).
fof(f730, plain, (cBipedidae(sK91) | ~ sP37), inference(cnf_transformation, [], [f395])).
fof(f1195, plain, (~ spl133_76 | spl133_78), inference(avatar_split_clause, [], [f727, f1192, f1183])).
fof(f727, plain, (cAnomalepidae(sK90) | ~ sP38), inference(cnf_transformation, [], [f391])).
fof(f391, plain, ((cEmydidae(sK90) & cAnomalepidae(sK90)) | ~ sP38), inference(skolemisation, [status(esa), new_symbols(skolem, [sK90])], [f389, f390])).
fof(f390, plain, (? [X0] : (cEmydidae(X0) & cAnomalepidae(X0)) => (cEmydidae(sK90) & cAnomalepidae(sK90))), introduced(choice_axiom, [])).
fof(f389, plain, (? [X0] : (cEmydidae(X0) & cAnomalepidae(X0)) | ~ sP38), inference(rectify, [], [f388])).
fof(f388, plain, (? [X28] : (cEmydidae(X28) & cAnomalepidae(X28)) | ~ sP38), inference(nnf_transformation, [], [f258])).
fof(f1190, plain, (~ spl133_76 | spl133_77), inference(avatar_split_clause, [], [f728, f1187, f1183])).
fof(f728, plain, (cEmydidae(sK90) | ~ sP38), inference(cnf_transformation, [], [f391])).
fof(f1181, plain, (~ spl133_73 | spl133_75), inference(avatar_split_clause, [], [f725, f1178, f1169])).
fof(f725, plain, (cXantusiidae(sK89) | ~ sP39), inference(cnf_transformation, [], [f387])).
fof(f387, plain, ((cSphenodontidae(sK89) & cXantusiidae(sK89)) | ~ sP39), inference(skolemisation, [status(esa), new_symbols(skolem, [sK89])], [f385, f386])).
fof(f386, plain, (? [X0] : (cSphenodontidae(X0) & cXantusiidae(X0)) => (cSphenodontidae(sK89) & cXantusiidae(sK89))), introduced(choice_axiom, [])).
fof(f385, plain, (? [X0] : (cSphenodontidae(X0) & cXantusiidae(X0)) | ~ sP39), inference(rectify, [], [f384])).
fof(f384, plain, (? [X27] : (cSphenodontidae(X27) & cXantusiidae(X27)) | ~ sP39), inference(nnf_transformation, [], [f259])).
fof(f1176, plain, (~ spl133_73 | spl133_74), inference(avatar_split_clause, [], [f726, f1173, f1169])).
fof(f726, plain, (cSphenodontidae(sK89) | ~ sP39), inference(cnf_transformation, [], [f387])).
fof(f1167, plain, (~ spl133_70 | spl133_72), inference(avatar_split_clause, [], [f723, f1164, f1155])).
fof(f723, plain, (cLeptotyphlopidae(sK88) | ~ sP40), inference(cnf_transformation, [], [f383])).
fof(f383, plain, ((cAmphisbaenidae(sK88) & cLeptotyphlopidae(sK88)) | ~ sP40), inference(skolemisation, [status(esa), new_symbols(skolem, [sK88])], [f381, f382])).
fof(f382, plain, (? [X0] : (cAmphisbaenidae(X0) & cLeptotyphlopidae(X0)) => (cAmphisbaenidae(sK88) & cLeptotyphlopidae(sK88))), introduced(choice_axiom, [])).
fof(f381, plain, (? [X0] : (cAmphisbaenidae(X0) & cLeptotyphlopidae(X0)) | ~ sP40), inference(rectify, [], [f380])).
fof(f380, plain, (? [X26] : (cAmphisbaenidae(X26) & cLeptotyphlopidae(X26)) | ~ sP40), inference(nnf_transformation, [], [f260])).
fof(f1162, plain, (~ spl133_70 | spl133_71), inference(avatar_split_clause, [], [f724, f1159, f1155])).
fof(f724, plain, (cAmphisbaenidae(sK88) | ~ sP40), inference(cnf_transformation, [], [f383])).
fof(f1153, plain, (~ spl133_67 | spl133_69), inference(avatar_split_clause, [], [f721, f1150, f1141])).
fof(f721, plain, (cSphenodontidae(sK87) | ~ sP41), inference(cnf_transformation, [], [f379])).
fof(f379, plain, ((cEmydidae(sK87) & cSphenodontidae(sK87)) | ~ sP41), inference(skolemisation, [status(esa), new_symbols(skolem, [sK87])], [f377, f378])).
fof(f378, plain, (? [X0] : (cEmydidae(X0) & cSphenodontidae(X0)) => (cEmydidae(sK87) & cSphenodontidae(sK87))), introduced(choice_axiom, [])).
fof(f377, plain, (? [X0] : (cEmydidae(X0) & cSphenodontidae(X0)) | ~ sP41), inference(rectify, [], [f376])).
fof(f376, plain, (? [X25] : (cEmydidae(X25) & cSphenodontidae(X25)) | ~ sP41), inference(nnf_transformation, [], [f261])).
fof(f1148, plain, (~ spl133_67 | spl133_68), inference(avatar_split_clause, [], [f722, f1145, f1141])).
fof(f722, plain, (cEmydidae(sK87) | ~ sP41), inference(cnf_transformation, [], [f379])).
fof(f1139, plain, (~ spl133_64 | spl133_66), inference(avatar_split_clause, [], [f719, f1136, f1127])).
fof(f719, plain, (cLeptotyphlopidae(sK86) | ~ sP42), inference(cnf_transformation, [], [f375])).
fof(f375, plain, ((cCordylidae(sK86) & cLeptotyphlopidae(sK86)) | ~ sP42), inference(skolemisation, [status(esa), new_symbols(skolem, [sK86])], [f373, f374])).
fof(f374, plain, (? [X0] : (cCordylidae(X0) & cLeptotyphlopidae(X0)) => (cCordylidae(sK86) & cLeptotyphlopidae(sK86))), introduced(choice_axiom, [])).
fof(f373, plain, (? [X0] : (cCordylidae(X0) & cLeptotyphlopidae(X0)) | ~ sP42), inference(rectify, [], [f372])).
fof(f372, plain, (? [X24] : (cCordylidae(X24) & cLeptotyphlopidae(X24)) | ~ sP42), inference(nnf_transformation, [], [f262])).
fof(f1134, plain, (~ spl133_64 | spl133_65), inference(avatar_split_clause, [], [f720, f1131, f1127])).
fof(f720, plain, (cCordylidae(sK86) | ~ sP42), inference(cnf_transformation, [], [f375])).
fof(f1125, plain, (~ spl133_61 | spl133_63), inference(avatar_split_clause, [], [f717, f1122, f1113])).
fof(f717, plain, (cGekkonidae(sK85) | ~ sP43), inference(cnf_transformation, [], [f371])).
fof(f371, plain, ((cAnomalepidae(sK85) & cGekkonidae(sK85)) | ~ sP43), inference(skolemisation, [status(esa), new_symbols(skolem, [sK85])], [f369, f370])).
fof(f370, plain, (? [X0] : (cAnomalepidae(X0) & cGekkonidae(X0)) => (cAnomalepidae(sK85) & cGekkonidae(sK85))), introduced(choice_axiom, [])).
fof(f369, plain, (? [X0] : (cAnomalepidae(X0) & cGekkonidae(X0)) | ~ sP43), inference(rectify, [], [f368])).
fof(f368, plain, (? [X23] : (cAnomalepidae(X23) & cGekkonidae(X23)) | ~ sP43), inference(nnf_transformation, [], [f263])).
fof(f1120, plain, (~ spl133_61 | spl133_62), inference(avatar_split_clause, [], [f718, f1117, f1113])).
fof(f718, plain, (cAnomalepidae(sK85) | ~ sP43), inference(cnf_transformation, [], [f371])).
fof(f1111, plain, (~ spl133_58 | spl133_60), inference(avatar_split_clause, [], [f715, f1108, f1099])).
fof(f715, plain, (cBipedidae(sK84) | ~ sP44), inference(cnf_transformation, [], [f367])).
fof(f367, plain, ((cCordylidae(sK84) & cBipedidae(sK84)) | ~ sP44), inference(skolemisation, [status(esa), new_symbols(skolem, [sK84])], [f365, f366])).
fof(f366, plain, (? [X0] : (cCordylidae(X0) & cBipedidae(X0)) => (cCordylidae(sK84) & cBipedidae(sK84))), introduced(choice_axiom, [])).
fof(f365, plain, (? [X0] : (cCordylidae(X0) & cBipedidae(X0)) | ~ sP44), inference(rectify, [], [f364])).
fof(f364, plain, (? [X22] : (cCordylidae(X22) & cBipedidae(X22)) | ~ sP44), inference(nnf_transformation, [], [f264])).
fof(f1106, plain, (~ spl133_58 | spl133_59), inference(avatar_split_clause, [], [f716, f1103, f1099])).
fof(f716, plain, (cCordylidae(sK84) | ~ sP44), inference(cnf_transformation, [], [f367])).
fof(f1097, plain, (~ spl133_55 | spl133_57), inference(avatar_split_clause, [], [f713, f1094, f1085])).
fof(f713, plain, (cBipedidae(sK83) | ~ sP45), inference(cnf_transformation, [], [f363])).
fof(f363, plain, ((cAmphisbaenidae(sK83) & cBipedidae(sK83)) | ~ sP45), inference(skolemisation, [status(esa), new_symbols(skolem, [sK83])], [f361, f362])).
fof(f362, plain, (? [X0] : (cAmphisbaenidae(X0) & cBipedidae(X0)) => (cAmphisbaenidae(sK83) & cBipedidae(sK83))), introduced(choice_axiom, [])).
fof(f361, plain, (? [X0] : (cAmphisbaenidae(X0) & cBipedidae(X0)) | ~ sP45), inference(rectify, [], [f360])).
fof(f360, plain, (? [X21] : (cAmphisbaenidae(X21) & cBipedidae(X21)) | ~ sP45), inference(nnf_transformation, [], [f265])).
fof(f1092, plain, (~ spl133_55 | spl133_56), inference(avatar_split_clause, [], [f714, f1089, f1085])).
fof(f714, plain, (cAmphisbaenidae(sK83) | ~ sP45), inference(cnf_transformation, [], [f363])).
fof(f1083, plain, (~ spl133_52 | spl133_54), inference(avatar_split_clause, [], [f711, f1080, f1071])).
fof(f711, plain, (cXantusiidae(sK82) | ~ sP46), inference(cnf_transformation, [], [f359])).
fof(f359, plain, ((cCordylidae(sK82) & cXantusiidae(sK82)) | ~ sP46), inference(skolemisation, [status(esa), new_symbols(skolem, [sK82])], [f357, f358])).
fof(f358, plain, (? [X0] : (cCordylidae(X0) & cXantusiidae(X0)) => (cCordylidae(sK82) & cXantusiidae(sK82))), introduced(choice_axiom, [])).
fof(f357, plain, (? [X0] : (cCordylidae(X0) & cXantusiidae(X0)) | ~ sP46), inference(rectify, [], [f356])).
fof(f356, plain, (? [X20] : (cCordylidae(X20) & cXantusiidae(X20)) | ~ sP46), inference(nnf_transformation, [], [f266])).
fof(f1078, plain, (~ spl133_52 | spl133_53), inference(avatar_split_clause, [], [f712, f1075, f1071])).
fof(f712, plain, (cCordylidae(sK82) | ~ sP46), inference(cnf_transformation, [], [f359])).
fof(f1069, plain, (~ spl133_49 | spl133_51), inference(avatar_split_clause, [], [f709, f1066, f1057])).
fof(f709, plain, (cAnomalepidae(sK81) | ~ sP47), inference(cnf_transformation, [], [f355])).
fof(f355, plain, ((cAgamidae(sK81) & cAnomalepidae(sK81)) | ~ sP47), inference(skolemisation, [status(esa), new_symbols(skolem, [sK81])], [f353, f354])).
fof(f354, plain, (? [X0] : (cAgamidae(X0) & cAnomalepidae(X0)) => (cAgamidae(sK81) & cAnomalepidae(sK81))), introduced(choice_axiom, [])).
fof(f353, plain, (? [X0] : (cAgamidae(X0) & cAnomalepidae(X0)) | ~ sP47), inference(rectify, [], [f352])).
fof(f352, plain, (? [X19] : (cAgamidae(X19) & cAnomalepidae(X19)) | ~ sP47), inference(nnf_transformation, [], [f267])).
fof(f1064, plain, (~ spl133_49 | spl133_50), inference(avatar_split_clause, [], [f710, f1061, f1057])).
fof(f710, plain, (cAgamidae(sK81) | ~ sP47), inference(cnf_transformation, [], [f355])).
fof(f1055, plain, (~ spl133_46 | spl133_48), inference(avatar_split_clause, [], [f707, f1052, f1043])).
fof(f707, plain, (cSphenodontidae(sK80) | ~ sP48), inference(cnf_transformation, [], [f351])).
fof(f351, plain, ((cCrocodylidae(sK80) & cSphenodontidae(sK80)) | ~ sP48), inference(skolemisation, [status(esa), new_symbols(skolem, [sK80])], [f349, f350])).
fof(f350, plain, (? [X0] : (cCrocodylidae(X0) & cSphenodontidae(X0)) => (cCrocodylidae(sK80) & cSphenodontidae(sK80))), introduced(choice_axiom, [])).
fof(f349, plain, (? [X0] : (cCrocodylidae(X0) & cSphenodontidae(X0)) | ~ sP48), inference(rectify, [], [f348])).
fof(f348, plain, (? [X18] : (cCrocodylidae(X18) & cSphenodontidae(X18)) | ~ sP48), inference(nnf_transformation, [], [f268])).
fof(f1050, plain, (~ spl133_46 | spl133_47), inference(avatar_split_clause, [], [f708, f1047, f1043])).
fof(f708, plain, (cCrocodylidae(sK80) | ~ sP48), inference(cnf_transformation, [], [f351])).
fof(f1041, plain, (~ spl133_43 | spl133_45), inference(avatar_split_clause, [], [f705, f1038, f1029])).
fof(f705, plain, (cXantusiidae(sK79) | ~ sP49), inference(cnf_transformation, [], [f347])).
fof(f347, plain, ((cAmphisbaenidae(sK79) & cXantusiidae(sK79)) | ~ sP49), inference(skolemisation, [status(esa), new_symbols(skolem, [sK79])], [f345, f346])).
fof(f346, plain, (? [X0] : (cAmphisbaenidae(X0) & cXantusiidae(X0)) => (cAmphisbaenidae(sK79) & cXantusiidae(sK79))), introduced(choice_axiom, [])).
fof(f345, plain, (? [X0] : (cAmphisbaenidae(X0) & cXantusiidae(X0)) | ~ sP49), inference(rectify, [], [f344])).
fof(f344, plain, (? [X17] : (cAmphisbaenidae(X17) & cXantusiidae(X17)) | ~ sP49), inference(nnf_transformation, [], [f269])).
fof(f1036, plain, (~ spl133_43 | spl133_44), inference(avatar_split_clause, [], [f706, f1033, f1029])).
fof(f706, plain, (cAmphisbaenidae(sK79) | ~ sP49), inference(cnf_transformation, [], [f347])).
fof(f1027, plain, (~ spl133_40 | spl133_42), inference(avatar_split_clause, [], [f703, f1024, f1015])).
fof(f703, plain, (cGekkonidae(sK78) | ~ sP50), inference(cnf_transformation, [], [f343])).
fof(f343, plain, ((cEmydidae(sK78) & cGekkonidae(sK78)) | ~ sP50), inference(skolemisation, [status(esa), new_symbols(skolem, [sK78])], [f341, f342])).
fof(f342, plain, (? [X0] : (cEmydidae(X0) & cGekkonidae(X0)) => (cEmydidae(sK78) & cGekkonidae(sK78))), introduced(choice_axiom, [])).
fof(f341, plain, (? [X0] : (cEmydidae(X0) & cGekkonidae(X0)) | ~ sP50), inference(rectify, [], [f340])).
fof(f340, plain, (? [X16] : (cEmydidae(X16) & cGekkonidae(X16)) | ~ sP50), inference(nnf_transformation, [], [f270])).
fof(f1022, plain, (~ spl133_40 | spl133_41), inference(avatar_split_clause, [], [f704, f1019, f1015])).
fof(f704, plain, (cEmydidae(sK78) | ~ sP50), inference(cnf_transformation, [], [f343])).
fof(f1013, plain, (~ spl133_37 | spl133_39), inference(avatar_split_clause, [], [f701, f1010, f1001])).
fof(f701, plain, (cSphenodontidae(sK77) | ~ sP51), inference(cnf_transformation, [], [f339])).
fof(f339, plain, ((cLoxocemidae(sK77) & cSphenodontidae(sK77)) | ~ sP51), inference(skolemisation, [status(esa), new_symbols(skolem, [sK77])], [f337, f338])).
fof(f338, plain, (? [X0] : (cLoxocemidae(X0) & cSphenodontidae(X0)) => (cLoxocemidae(sK77) & cSphenodontidae(sK77))), introduced(choice_axiom, [])).
fof(f337, plain, (? [X0] : (cLoxocemidae(X0) & cSphenodontidae(X0)) | ~ sP51), inference(rectify, [], [f336])).
fof(f336, plain, (? [X15] : (cLoxocemidae(X15) & cSphenodontidae(X15)) | ~ sP51), inference(nnf_transformation, [], [f271])).
fof(f1008, plain, (~ spl133_37 | spl133_38), inference(avatar_split_clause, [], [f702, f1005, f1001])).
fof(f702, plain, (cLoxocemidae(sK77) | ~ sP51), inference(cnf_transformation, [], [f339])).
fof(f999, plain, (~ spl133_34 | spl133_36), inference(avatar_split_clause, [], [f699, f996, f987])).
fof(f699, plain, (cLeptotyphlopidae(sK76) | ~ sP52), inference(cnf_transformation, [], [f335])).
fof(f335, plain, ((cEmydidae(sK76) & cLeptotyphlopidae(sK76)) | ~ sP52), inference(skolemisation, [status(esa), new_symbols(skolem, [sK76])], [f333, f334])).
fof(f334, plain, (? [X0] : (cEmydidae(X0) & cLeptotyphlopidae(X0)) => (cEmydidae(sK76) & cLeptotyphlopidae(sK76))), introduced(choice_axiom, [])).
fof(f333, plain, (? [X0] : (cEmydidae(X0) & cLeptotyphlopidae(X0)) | ~ sP52), inference(rectify, [], [f332])).
fof(f332, plain, (? [X14] : (cEmydidae(X14) & cLeptotyphlopidae(X14)) | ~ sP52), inference(nnf_transformation, [], [f272])).
fof(f994, plain, (~ spl133_34 | spl133_35), inference(avatar_split_clause, [], [f700, f991, f987])).
fof(f700, plain, (cEmydidae(sK76) | ~ sP52), inference(cnf_transformation, [], [f335])).
fof(f985, plain, (~ spl133_31 | spl133_33), inference(avatar_split_clause, [], [f697, f982, f973])).
fof(f697, plain, (cAmphisbaenidae(sK75) | ~ sP53), inference(cnf_transformation, [], [f331])).
fof(f331, plain, ((cAnomalepidae(sK75) & cAmphisbaenidae(sK75)) | ~ sP53), inference(skolemisation, [status(esa), new_symbols(skolem, [sK75])], [f329, f330])).
fof(f330, plain, (? [X0] : (cAnomalepidae(X0) & cAmphisbaenidae(X0)) => (cAnomalepidae(sK75) & cAmphisbaenidae(sK75))), introduced(choice_axiom, [])).
fof(f329, plain, (? [X0] : (cAnomalepidae(X0) & cAmphisbaenidae(X0)) | ~ sP53), inference(rectify, [], [f328])).
fof(f328, plain, (? [X13] : (cAnomalepidae(X13) & cAmphisbaenidae(X13)) | ~ sP53), inference(nnf_transformation, [], [f273])).
fof(f980, plain, (~ spl133_31 | spl133_32), inference(avatar_split_clause, [], [f698, f977, f973])).
fof(f698, plain, (cAnomalepidae(sK75) | ~ sP53), inference(cnf_transformation, [], [f331])).
fof(f971, plain, (~ spl133_28 | spl133_30), inference(avatar_split_clause, [], [f695, f968, f959])).
fof(f695, plain, (cGekkonidae(sK74) | ~ sP54), inference(cnf_transformation, [], [f327])).
fof(f327, plain, ((cLoxocemidae(sK74) & cGekkonidae(sK74)) | ~ sP54), inference(skolemisation, [status(esa), new_symbols(skolem, [sK74])], [f325, f326])).
fof(f326, plain, (? [X0] : (cLoxocemidae(X0) & cGekkonidae(X0)) => (cLoxocemidae(sK74) & cGekkonidae(sK74))), introduced(choice_axiom, [])).
fof(f325, plain, (? [X0] : (cLoxocemidae(X0) & cGekkonidae(X0)) | ~ sP54), inference(rectify, [], [f324])).
fof(f324, plain, (? [X12] : (cLoxocemidae(X12) & cGekkonidae(X12)) | ~ sP54), inference(nnf_transformation, [], [f274])).
fof(f966, plain, (~ spl133_28 | spl133_29), inference(avatar_split_clause, [], [f696, f963, f959])).
fof(f696, plain, (cLoxocemidae(sK74) | ~ sP54), inference(cnf_transformation, [], [f327])).
fof(f957, plain, (~ spl133_25 | spl133_27), inference(avatar_split_clause, [], [f693, f954, f945])).
fof(f693, plain, (cAnomalepidae(sK73) | ~ sP55), inference(cnf_transformation, [], [f323])).
fof(f323, plain, ((cLoxocemidae(sK73) & cAnomalepidae(sK73)) | ~ sP55), inference(skolemisation, [status(esa), new_symbols(skolem, [sK73])], [f321, f322])).
fof(f322, plain, (? [X0] : (cLoxocemidae(X0) & cAnomalepidae(X0)) => (cLoxocemidae(sK73) & cAnomalepidae(sK73))), introduced(choice_axiom, [])).
fof(f321, plain, (? [X0] : (cLoxocemidae(X0) & cAnomalepidae(X0)) | ~ sP55), inference(rectify, [], [f320])).
fof(f320, plain, (? [X11] : (cLoxocemidae(X11) & cAnomalepidae(X11)) | ~ sP55), inference(nnf_transformation, [], [f275])).
fof(f952, plain, (~ spl133_25 | spl133_26), inference(avatar_split_clause, [], [f694, f949, f945])).
fof(f694, plain, (cLoxocemidae(sK73) | ~ sP55), inference(cnf_transformation, [], [f323])).
fof(f943, plain, (~ spl133_22 | spl133_24), inference(avatar_split_clause, [], [f691, f940, f931])).
fof(f691, plain, (cLeptotyphlopidae(sK72) | ~ sP56), inference(cnf_transformation, [], [f319])).
fof(f319, plain, ((cAnomalepidae(sK72) & cLeptotyphlopidae(sK72)) | ~ sP56), inference(skolemisation, [status(esa), new_symbols(skolem, [sK72])], [f317, f318])).
fof(f318, plain, (? [X0] : (cAnomalepidae(X0) & cLeptotyphlopidae(X0)) => (cAnomalepidae(sK72) & cLeptotyphlopidae(sK72))), introduced(choice_axiom, [])).
fof(f317, plain, (? [X0] : (cAnomalepidae(X0) & cLeptotyphlopidae(X0)) | ~ sP56), inference(rectify, [], [f316])).
fof(f316, plain, (? [X10] : (cAnomalepidae(X10) & cLeptotyphlopidae(X10)) | ~ sP56), inference(nnf_transformation, [], [f276])).
fof(f938, plain, (~ spl133_22 | spl133_23), inference(avatar_split_clause, [], [f692, f935, f931])).
fof(f692, plain, (cAnomalepidae(sK72) | ~ sP56), inference(cnf_transformation, [], [f319])).
fof(f929, plain, (~ spl133_19 | spl133_21), inference(avatar_split_clause, [], [f689, f926, f917])).
fof(f689, plain, (cCordylidae(sK71) | ~ sP57), inference(cnf_transformation, [], [f315])).
fof(f315, plain, ((cCrocodylidae(sK71) & cCordylidae(sK71)) | ~ sP57), inference(skolemisation, [status(esa), new_symbols(skolem, [sK71])], [f313, f314])).
fof(f314, plain, (? [X0] : (cCrocodylidae(X0) & cCordylidae(X0)) => (cCrocodylidae(sK71) & cCordylidae(sK71))), introduced(choice_axiom, [])).
fof(f313, plain, (? [X0] : (cCrocodylidae(X0) & cCordylidae(X0)) | ~ sP57), inference(rectify, [], [f312])).
fof(f312, plain, (? [X9] : (cCrocodylidae(X9) & cCordylidae(X9)) | ~ sP57), inference(nnf_transformation, [], [f277])).
fof(f924, plain, (~ spl133_19 | spl133_20), inference(avatar_split_clause, [], [f690, f921, f917])).
fof(f690, plain, (cCrocodylidae(sK71) | ~ sP57), inference(cnf_transformation, [], [f315])).
fof(f915, plain, (~ spl133_16 | spl133_18), inference(avatar_split_clause, [], [f687, f912, f903])).
fof(f687, plain, (cXantusiidae(sK70) | ~ sP58), inference(cnf_transformation, [], [f311])).
fof(f311, plain, ((cAnomalepidae(sK70) & cXantusiidae(sK70)) | ~ sP58), inference(skolemisation, [status(esa), new_symbols(skolem, [sK70])], [f309, f310])).
fof(f310, plain, (? [X0] : (cAnomalepidae(X0) & cXantusiidae(X0)) => (cAnomalepidae(sK70) & cXantusiidae(sK70))), introduced(choice_axiom, [])).
fof(f309, plain, (? [X0] : (cAnomalepidae(X0) & cXantusiidae(X0)) | ~ sP58), inference(rectify, [], [f308])).
fof(f308, plain, (? [X8] : (cAnomalepidae(X8) & cXantusiidae(X8)) | ~ sP58), inference(nnf_transformation, [], [f278])).
fof(f910, plain, (~ spl133_16 | spl133_17), inference(avatar_split_clause, [], [f688, f907, f903])).
fof(f688, plain, (cAnomalepidae(sK70) | ~ sP58), inference(cnf_transformation, [], [f311])).
fof(f901, plain, (~ spl133_13 | spl133_15), inference(avatar_split_clause, [], [f685, f898, f889])).
fof(f685, plain, (cAnomalepidae(sK69) | ~ sP59), inference(cnf_transformation, [], [f307])).
fof(f307, plain, ((cSphenodontidae(sK69) & cAnomalepidae(sK69)) | ~ sP59), inference(skolemisation, [status(esa), new_symbols(skolem, [sK69])], [f305, f306])).
fof(f306, plain, (? [X0] : (cSphenodontidae(X0) & cAnomalepidae(X0)) => (cSphenodontidae(sK69) & cAnomalepidae(sK69))), introduced(choice_axiom, [])).
fof(f305, plain, (? [X0] : (cSphenodontidae(X0) & cAnomalepidae(X0)) | ~ sP59), inference(rectify, [], [f304])).
fof(f304, plain, (? [X7] : (cSphenodontidae(X7) & cAnomalepidae(X7)) | ~ sP59), inference(nnf_transformation, [], [f279])).
fof(f896, plain, (~ spl133_13 | spl133_14), inference(avatar_split_clause, [], [f686, f893, f889])).
fof(f686, plain, (cSphenodontidae(sK69) | ~ sP59), inference(cnf_transformation, [], [f307])).
fof(f887, plain, (~ spl133_10 | spl133_12), inference(avatar_split_clause, [], [f683, f884, f875])).
fof(f683, plain, (cLeptotyphlopidae(sK68) | ~ sP60), inference(cnf_transformation, [], [f303])).
fof(f303, plain, ((cXantusiidae(sK68) & cLeptotyphlopidae(sK68)) | ~ sP60), inference(skolemisation, [status(esa), new_symbols(skolem, [sK68])], [f301, f302])).
fof(f302, plain, (? [X0] : (cXantusiidae(X0) & cLeptotyphlopidae(X0)) => (cXantusiidae(sK68) & cLeptotyphlopidae(sK68))), introduced(choice_axiom, [])).
fof(f301, plain, (? [X0] : (cXantusiidae(X0) & cLeptotyphlopidae(X0)) | ~ sP60), inference(rectify, [], [f300])).
fof(f300, plain, (? [X6] : (cXantusiidae(X6) & cLeptotyphlopidae(X6)) | ~ sP60), inference(nnf_transformation, [], [f280])).
fof(f882, plain, (~ spl133_10 | spl133_11), inference(avatar_split_clause, [], [f684, f879, f875])).
fof(f684, plain, (cXantusiidae(sK68) | ~ sP60), inference(cnf_transformation, [], [f303])).
fof(f873, plain, (~ spl133_7 | spl133_9), inference(avatar_split_clause, [], [f681, f870, f861])).
fof(f681, plain, (cGekkonidae(sK67) | ~ sP61), inference(cnf_transformation, [], [f299])).
fof(f299, plain, ((cAgamidae(sK67) & cGekkonidae(sK67)) | ~ sP61), inference(skolemisation, [status(esa), new_symbols(skolem, [sK67])], [f297, f298])).
fof(f298, plain, (? [X0] : (cAgamidae(X0) & cGekkonidae(X0)) => (cAgamidae(sK67) & cGekkonidae(sK67))), introduced(choice_axiom, [])).
fof(f297, plain, (? [X0] : (cAgamidae(X0) & cGekkonidae(X0)) | ~ sP61), inference(rectify, [], [f296])).
fof(f296, plain, (? [X5] : (cAgamidae(X5) & cGekkonidae(X5)) | ~ sP61), inference(nnf_transformation, [], [f281])).
fof(f868, plain, (~ spl133_7 | spl133_8), inference(avatar_split_clause, [], [f682, f865, f861])).
fof(f682, plain, (cAgamidae(sK67) | ~ sP61), inference(cnf_transformation, [], [f299])).
fof(f859, plain, (~ spl133_4 | spl133_6), inference(avatar_split_clause, [], [f679, f856, f847])).
fof(f679, plain, (cAgamidae(sK66) | ~ sP62), inference(cnf_transformation, [], [f295])).
fof(f295, plain, ((cCordylidae(sK66) & cAgamidae(sK66)) | ~ sP62), inference(skolemisation, [status(esa), new_symbols(skolem, [sK66])], [f293, f294])).
fof(f294, plain, (? [X0] : (cCordylidae(X0) & cAgamidae(X0)) => (cCordylidae(sK66) & cAgamidae(sK66))), introduced(choice_axiom, [])).
fof(f293, plain, (? [X0] : (cCordylidae(X0) & cAgamidae(X0)) | ~ sP62), inference(rectify, [], [f292])).
fof(f292, plain, (? [X4] : (cCordylidae(X4) & cAgamidae(X4)) | ~ sP62), inference(nnf_transformation, [], [f282])).
fof(f854, plain, (~ spl133_4 | spl133_5), inference(avatar_split_clause, [], [f680, f851, f847])).
fof(f680, plain, (cCordylidae(sK66) | ~ sP62), inference(cnf_transformation, [], [f295])).
fof(f845, plain, (~ spl133_1 | spl133_3), inference(avatar_split_clause, [], [f677, f842, f833])).
fof(f677, plain, (cLeptotyphlopidae(sK65) | ~ sP63), inference(cnf_transformation, [], [f291])).
fof(f291, plain, ((cLoxocemidae(sK65) & cLeptotyphlopidae(sK65)) | ~ sP63), inference(skolemisation, [status(esa), new_symbols(skolem, [sK65])], [f289, f290])).
fof(f290, plain, (? [X0] : (cLoxocemidae(X0) & cLeptotyphlopidae(X0)) => (cLoxocemidae(sK65) & cLeptotyphlopidae(sK65))), introduced(choice_axiom, [])).
fof(f289, plain, (? [X0] : (cLoxocemidae(X0) & cLeptotyphlopidae(X0)) | ~ sP63), inference(rectify, [], [f288])).
fof(f288, plain, (? [X3] : (cLoxocemidae(X3) & cLeptotyphlopidae(X3)) | ~ sP63), inference(nnf_transformation, [], [f283])).
fof(f840, plain, (~ spl133_1 | spl133_2), inference(avatar_split_clause, [], [f678, f837, f833])).
fof(f678, plain, (cLoxocemidae(sK65) | ~ sP63), inference(cnf_transformation, [], [f291])).