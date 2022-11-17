fof(f3608, plain, $false, inference(avatar_sat_refutation, [], [f671, f713, f755, f818, f923, f965, f1007, f1010, f1013, f1017, f1018, f1019, f1041, f1042, f1046, f1050, f1059, f1060, f1069, f1079, f1093, f1098, f1112, f1131, f1136, f1150, f1151, f1162, f1176, f1181, f1195, f1219, f1228, f1234, f1248, f1249, f1260, f1274, f1279, f1288, f1298, f1312, f1322, f1327, f1342, f1343, f1354, f1363, f1373, f1387, f1388, f1403, f1418, f1427, f1432, f1444, f1451, f1458, f1464, f1472, f1478, f1486, f1493, f1499, f1505, f1506, f1513, f1520, f1526, f1528, f1535, f1540, f1542, f1549, f1556, f1562, f1570, f1577, f1583, f1591, f1596, f1598, f1604, f1610, f1612, f1617, f1620, f1652, f1668, f1678, f1680, f1681, f1699, f1703, f1723, f1735, f1753, f1775, f1776, f1789, f1807, f1835, f1852, f1863, f1884, f1890, f1891, f1912, f1921, f1927, f1933, f1969, f1986, f1992, f2011, f2070, f2098, f2115, f2129, f2145, f2162, f2166, f2179, f2190, f2200, f2226, f2238, f2260, f2262, f2269, f2271, f2275, f2314, f2343, f2370, f2380, f2409, f2443, f2485, f2492, f2499, f2504, f2515, f2518, f2543, f2594, f2598, f2603, f2649, f2715, f2740, f2784, f2785, f2798, f2810, f2826, f2834, f2862, f2885, f2886, f2889, f2891, f2894, f2897, f2906, f2926, f2937, f2971, f2972, f3007, f3008, f3009, f3012, f3019, f3036, f3087, f3097, f3104, f3108, f3118, f3122, f3129, f3134, f3147, f3157, f3180, f3183, f3193, f3225, f3247, f3262, f3284, f3298, f3329, f3347, f3348, f3349, f3350, f3351, f3352, f3353, f3354, f3355, f3369, f3381, f3382, f3383, f3410, f3420, f3455, f3463, f3488, f3519, f3529, f3552, f3570, f3589, f3594, f3607])).
fof(f3607, plain, (~ spl48_49 | ~ spl48_109 | spl48_163), inference(avatar_contradiction_clause, [], [f3606])).
fof(f3606, plain, ($false | (~ spl48_49 | ~ spl48_109 | spl48_163)), inference(subsumption_resolution, [], [f3605, f918])).
fof(f918, plain, ((e3 = op(e0, e3)) | ~ spl48_109), inference(avatar_component_clause, [], [f916])).
fof(f916, plain, (spl48_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_109])])).
fof(f3605, plain, (~ (e3 = op(e0, e3)) | (~ spl48_49 | spl48_163)), inference(forward_demodulation, [], [f1218, f666])).
fof(f666, plain, ((e3 = op(e3, e0)) | ~ spl48_49), inference(avatar_component_clause, [], [f664])).
fof(f664, plain, (spl48_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_49])])).
fof(f1218, plain, (~ (op(e0, e3) = op(e3, e0)) | spl48_163), inference(avatar_component_clause, [], [f1216])).
fof(f1216, plain, (spl48_163 <=> (op(e0, e3) = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_163])])).
fof(f3594, plain, (spl48_18 | ~ spl48_70 | ~ spl48_174), inference(avatar_contradiction_clause, [], [f3593])).
fof(f3593, plain, ($false | (spl48_18 | ~ spl48_70 | ~ spl48_174)), inference(subsumption_resolution, [], [f3592, f535])).
fof(f535, plain, (~ (e2 = op(e4, e1)) | spl48_18), inference(avatar_component_clause, [], [f534])).
fof(f534, plain, (spl48_18 <=> (e2 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_18])])).
fof(f3592, plain, ((e2 = op(e4, e1)) | (~ spl48_70 | ~ spl48_174)), inference(forward_demodulation, [], [f1273, f754])).
fof(f754, plain, ((e4 = op(e2, e1)) | ~ spl48_70), inference(avatar_component_clause, [], [f752])).
fof(f752, plain, (spl48_70 <=> (e4 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_70])])).
fof(f1273, plain, ((e2 = op(op(e2, e1), e1)) | ~ spl48_174), inference(avatar_component_clause, [], [f1271])).
fof(f1271, plain, (spl48_174 <=> (e2 = op(op(e2, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_174])])).
fof(f3589, plain, (~ spl48_73 | ~ spl48_113 | spl48_179), inference(avatar_contradiction_clause, [], [f3588])).
fof(f3588, plain, ($false | (~ spl48_73 | ~ spl48_113 | spl48_179)), inference(subsumption_resolution, [], [f3587, f935])).
fof(f935, plain, ((e2 = op(e0, e2)) | ~ spl48_113), inference(avatar_component_clause, [], [f933])).
fof(f933, plain, (spl48_113 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_113])])).
fof(f3587, plain, (~ (e2 = op(e0, e2)) | (~ spl48_73 | spl48_179)), inference(forward_demodulation, [], [f1297, f767])).
fof(f767, plain, ((e2 = op(e2, e0)) | ~ spl48_73), inference(avatar_component_clause, [], [f765])).
fof(f765, plain, (spl48_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_73])])).
fof(f1297, plain, (~ (op(e0, e2) = op(e2, e0)) | spl48_179), inference(avatar_component_clause, [], [f1295])).
fof(f1295, plain, (spl48_179 <=> (op(e0, e2) = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_179])])).
fof(f3570, plain, (~ spl48_40 | ~ spl48_54 | spl48_165), inference(avatar_contradiction_clause, [], [f3569])).
fof(f3569, plain, ($false | (~ spl48_40 | ~ spl48_54 | spl48_165)), inference(subsumption_resolution, [], [f3568, f628])).
fof(f628, plain, ((e4 = op(e3, e2)) | ~ spl48_40), inference(avatar_component_clause, [], [f626])).
fof(f626, plain, (spl48_40 <=> (e4 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_40])])).
fof(f3568, plain, (~ (e4 = op(e3, e2)) | (~ spl48_54 | spl48_165)), inference(forward_demodulation, [], [f1227, f687])).
fof(f687, plain, ((e3 = op(e2, e4)) | ~ spl48_54), inference(avatar_component_clause, [], [f685])).
fof(f685, plain, (spl48_54 <=> (e3 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_54])])).
fof(f1227, plain, (~ (e4 = op(op(e2, e4), e2)) | spl48_165), inference(avatar_component_clause, [], [f1225])).
fof(f1225, plain, (spl48_165 <=> (e4 = op(op(e2, e4), e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_165])])).
fof(f3552, plain, (~ spl48_56 | spl48_108 | ~ spl48_169), inference(avatar_contradiction_clause, [], [f3551])).
fof(f3551, plain, ($false | (~ spl48_56 | spl48_108 | ~ spl48_169)), inference(subsumption_resolution, [], [f3550, f913])).
fof(f913, plain, (~ (e2 = op(e0, e3)) | spl48_108), inference(avatar_component_clause, [], [f912])).
fof(f912, plain, (spl48_108 <=> (e2 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_108])])).
fof(f3550, plain, ((e2 = op(e0, e3)) | (~ spl48_56 | ~ spl48_169)), inference(forward_demodulation, [], [f1247, f696])).
fof(f696, plain, ((e0 = op(e2, e3)) | ~ spl48_56), inference(avatar_component_clause, [], [f694])).
fof(f694, plain, (spl48_56 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_56])])).
fof(f1247, plain, ((e2 = op(op(e2, e3), e3)) | ~ spl48_169), inference(avatar_component_clause, [], [f1245])).
fof(f1245, plain, (spl48_169 <=> (e2 = op(op(e2, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_169])])).
fof(f3529, plain, (~ spl48_11 | ~ spl48_113 | ~ spl48_137), inference(avatar_contradiction_clause, [], [f3528])).
fof(f3528, plain, ($false | (~ spl48_11 | ~ spl48_113 | ~ spl48_137)), inference(subsumption_resolution, [], [f3527, f301])).
fof(f301, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG060+1.p', ax5)).
fof(f3527, plain, ((e2 = e4) | (~ spl48_11 | ~ spl48_113 | ~ spl48_137)), inference(forward_demodulation, [], [f3526, f935])).
fof(f3526, plain, ((e4 = op(e0, e2)) | (~ spl48_11 | ~ spl48_137)), inference(forward_demodulation, [], [f1092, f507])).
fof(f507, plain, ((e0 = op(e4, e2)) | ~ spl48_11), inference(avatar_component_clause, [], [f505])).
fof(f505, plain, (spl48_11 <=> (e0 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_11])])).
fof(f1092, plain, ((e4 = op(op(e4, e2), e2)) | ~ spl48_137), inference(avatar_component_clause, [], [f1090])).
fof(f1090, plain, (spl48_137 <=> (e4 = op(op(e4, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_137])])).
fof(f3519, plain, (~ spl48_11 | ~ spl48_40 | ~ spl48_154), inference(avatar_contradiction_clause, [], [f3518])).
fof(f3518, plain, ($false | (~ spl48_11 | ~ spl48_40 | ~ spl48_154)), inference(subsumption_resolution, [], [f3517, f295])).
fof(f295, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f5])).
fof(f3517, plain, ((e0 = e3) | (~ spl48_11 | ~ spl48_40 | ~ spl48_154)), inference(forward_demodulation, [], [f3516, f507])).
fof(f3516, plain, ((e3 = op(e4, e2)) | (~ spl48_40 | ~ spl48_154)), inference(forward_demodulation, [], [f1175, f628])).
fof(f1175, plain, ((e3 = op(op(e3, e2), e2)) | ~ spl48_154), inference(avatar_component_clause, [], [f1173])).
fof(f1173, plain, (spl48_154 <=> (e3 = op(op(e3, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_154])])).
fof(f3488, plain, (~ spl48_39 | ~ spl48_40), inference(avatar_contradiction_clause, [], [f3487])).
fof(f3487, plain, ($false | (~ spl48_39 | ~ spl48_40)), inference(subsumption_resolution, [], [f3486, f302])).
fof(f302, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f3486, plain, ((e3 = e4) | (~ spl48_39 | ~ spl48_40)), inference(forward_demodulation, [], [f628, f624])).
fof(f624, plain, ((e3 = op(e3, e2)) | ~ spl48_39), inference(avatar_component_clause, [], [f622])).
fof(f622, plain, (spl48_39 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_39])])).
fof(f3463, plain, (~ spl48_19 | ~ spl48_85 | spl48_184), inference(avatar_contradiction_clause, [], [f3462])).
fof(f3462, plain, ($false | (~ spl48_19 | ~ spl48_85 | spl48_184)), inference(subsumption_resolution, [], [f3460, f540])).
fof(f540, plain, ((e3 = op(e4, e1)) | ~ spl48_19), inference(avatar_component_clause, [], [f538])).
fof(f538, plain, (spl48_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_19])])).
fof(f3460, plain, (~ (e3 = op(e4, e1)) | (~ spl48_85 | spl48_184)), inference(backward_demodulation, [], [f1321, f817])).
fof(f817, plain, ((e4 = op(e1, e3)) | ~ spl48_85), inference(avatar_component_clause, [], [f815])).
fof(f815, plain, (spl48_85 <=> (e4 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_85])])).
fof(f1321, plain, (~ (e3 = op(op(e1, e3), e1)) | spl48_184), inference(avatar_component_clause, [], [f1319])).
fof(f1319, plain, (spl48_184 <=> (e3 = op(op(e1, e3), e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_184])])).
fof(f3455, plain, (spl48_37 | ~ spl48_89 | ~ spl48_188), inference(avatar_contradiction_clause, [], [f3454])).
fof(f3454, plain, ($false | (spl48_37 | ~ spl48_89 | ~ spl48_188)), inference(subsumption_resolution, [], [f3447, f615])).
fof(f615, plain, (~ (e1 = op(e3, e2)) | spl48_37), inference(avatar_component_clause, [], [f614])).
fof(f614, plain, (spl48_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_37])])).
fof(f3447, plain, ((e1 = op(e3, e2)) | (~ spl48_89 | ~ spl48_188)), inference(backward_demodulation, [], [f1341, f834])).
fof(f834, plain, ((e3 = op(e1, e2)) | ~ spl48_89), inference(avatar_component_clause, [], [f832])).
fof(f832, plain, (spl48_89 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_89])])).
fof(f1341, plain, ((e1 = op(op(e1, e2), e2)) | ~ spl48_188), inference(avatar_component_clause, [], [f1339])).
fof(f1339, plain, (spl48_188 <=> (e1 = op(op(e1, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_188])])).
fof(f3420, plain, (~ spl48_25 | ~ spl48_105 | spl48_146), inference(avatar_split_clause, [], [f3416, f1133, f899, f563])).
fof(f563, plain, (spl48_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_25])])).
fof(f899, plain, (spl48_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_105])])).
fof(f1133, plain, (spl48_146 <=> (op(e0, e4) = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_146])])).
fof(f3416, plain, (~ (e4 = op(e4, e0)) | (~ spl48_105 | spl48_146)), inference(backward_demodulation, [], [f1135, f901])).
fof(f901, plain, ((e4 = op(e0, e4)) | ~ spl48_105), inference(avatar_component_clause, [], [f899])).
fof(f1135, plain, (~ (op(e0, e4) = op(e4, e0)) | spl48_146), inference(avatar_component_clause, [], [f1133])).
fof(f3410, plain, (~ spl48_59 | ~ spl48_109), inference(avatar_split_clause, [], [f3401, f916, f706])).
fof(f706, plain, (spl48_59 <=> (e3 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_59])])).
fof(f3401, plain, (~ (e3 = op(e2, e3)) | ~ spl48_109), inference(backward_demodulation, [], [f224, f918])).
fof(f224, plain, ~ (op(e0, e3) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG060+1.p', ax4)).
fof(f3383, plain, (~ spl48_97 | ~ spl48_117 | spl48_205), inference(avatar_split_clause, [], [f3379, f1424, f950, f866])).
fof(f866, plain, (spl48_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_97])])).
fof(f950, plain, (spl48_117 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_117])])).
fof(f1424, plain, (spl48_205 <=> (e1 = op(op(e0, e1), e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_205])])).
fof(f3379, plain, (~ (e1 = op(e1, e0)) | (~ spl48_117 | spl48_205)), inference(backward_demodulation, [], [f1426, f952])).
fof(f952, plain, ((e1 = op(e0, e1)) | ~ spl48_117), inference(avatar_component_clause, [], [f950])).
fof(f1426, plain, (~ (e1 = op(op(e0, e1), e0)) | spl48_205), inference(avatar_component_clause, [], [f1424])).
fof(f3382, plain, (~ spl48_97 | ~ spl48_117 | spl48_194), inference(avatar_split_clause, [], [f3378, f1370, f950, f866])).
fof(f1370, plain, (spl48_194 <=> (op(e0, e1) = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_194])])).
fof(f3378, plain, (~ (e1 = op(e1, e0)) | (~ spl48_117 | spl48_194)), inference(backward_demodulation, [], [f1372, f952])).
fof(f1372, plain, (~ (op(e0, e1) = op(e1, e0)) | spl48_194), inference(avatar_component_clause, [], [f1370])).
fof(f3381, plain, (~ spl48_67 | ~ spl48_117), inference(avatar_split_clause, [], [f3371, f950, f740])).
fof(f740, plain, (spl48_67 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_67])])).
fof(f3371, plain, (~ (e1 = op(e2, e1)) | ~ spl48_117), inference(backward_demodulation, [], [f204, f952])).
fof(f204, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f3369, plain, (~ spl48_111 | ~ spl48_121), inference(avatar_split_clause, [], [f3361, f967, f925])).
fof(f925, plain, (spl48_111 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_111])])).
fof(f967, plain, (spl48_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_121])])).
fof(f3361, plain, (~ (e0 = op(e0, e2)) | ~ spl48_121), inference(backward_demodulation, [], [f244, f969])).
fof(f969, plain, ((e0 = op(e0, e0)) | ~ spl48_121), inference(avatar_component_clause, [], [f967])).
fof(f244, plain, ~ (op(e0, e0) = op(e0, e2)), inference(cnf_transformation, [], [f4])).
fof(f3355, plain, (spl48_25 | ~ spl48_126), inference(avatar_split_clause, [], [f3341, f988, f563])).
fof(f988, plain, (spl48_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl48_126])])).
fof(f3341, plain, ((e4 = op(e4, e0)) | ~ spl48_126), inference(backward_demodulation, [], [f141, f990])).
fof(f990, plain, ((e0 = unit) | ~ spl48_126), inference(avatar_component_clause, [], [f988])).
fof(f141, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG060+1.p', ax2)).
fof(f3354, plain, (spl48_105 | ~ spl48_126), inference(avatar_split_clause, [], [f3340, f988, f899])).
fof(f3340, plain, ((e4 = op(e0, e4)) | ~ spl48_126), inference(backward_demodulation, [], [f140, f990])).
fof(f140, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f3353, plain, (spl48_49 | ~ spl48_126), inference(avatar_split_clause, [], [f3339, f988, f664])).
fof(f3339, plain, ((e3 = op(e3, e0)) | ~ spl48_126), inference(backward_demodulation, [], [f139, f990])).
fof(f139, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f3352, plain, (spl48_109 | ~ spl48_126), inference(avatar_split_clause, [], [f3338, f988, f916])).
fof(f3338, plain, ((e3 = op(e0, e3)) | ~ spl48_126), inference(backward_demodulation, [], [f138, f990])).
fof(f138, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f3351, plain, (spl48_73 | ~ spl48_126), inference(avatar_split_clause, [], [f3337, f988, f765])).
fof(f3337, plain, ((e2 = op(e2, e0)) | ~ spl48_126), inference(backward_demodulation, [], [f137, f990])).
fof(f137, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f3350, plain, (spl48_113 | ~ spl48_126), inference(avatar_split_clause, [], [f3336, f988, f933])).
fof(f3336, plain, ((e2 = op(e0, e2)) | ~ spl48_126), inference(backward_demodulation, [], [f136, f990])).
fof(f136, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f3349, plain, (spl48_97 | ~ spl48_126), inference(avatar_split_clause, [], [f3335, f988, f866])).
fof(f3335, plain, ((e1 = op(e1, e0)) | ~ spl48_126), inference(backward_demodulation, [], [f135, f990])).
fof(f135, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f3348, plain, (spl48_117 | ~ spl48_126), inference(avatar_split_clause, [], [f3334, f988, f950])).
fof(f3334, plain, ((e1 = op(e0, e1)) | ~ spl48_126), inference(backward_demodulation, [], [f134, f990])).
fof(f134, plain, (e1 = op(unit, e1)), inference(cnf_transformation, [], [f2])).
fof(f3347, plain, (spl48_121 | ~ spl48_126), inference(avatar_split_clause, [], [f3333, f988, f967])).
fof(f3333, plain, ((e0 = op(e0, e0)) | ~ spl48_126), inference(backward_demodulation, [], [f133, f990])).
fof(f133, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f3329, plain, (~ spl48_41 | spl48_119 | ~ spl48_158), inference(avatar_contradiction_clause, [], [f3328])).
fof(f3328, plain, ($false | (~ spl48_41 | spl48_119 | ~ spl48_158)), inference(subsumption_resolution, [], [f3327, f959])).
fof(f959, plain, (~ (e3 = op(e0, e1)) | spl48_119), inference(avatar_component_clause, [], [f958])).
fof(f958, plain, (spl48_119 <=> (e3 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_119])])).
fof(f3327, plain, ((e3 = op(e0, e1)) | (~ spl48_41 | ~ spl48_158)), inference(forward_demodulation, [], [f1194, f633])).
fof(f633, plain, ((e0 = op(e3, e1)) | ~ spl48_41), inference(avatar_component_clause, [], [f631])).
fof(f631, plain, (spl48_41 <=> (e0 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_41])])).
fof(f1194, plain, ((e3 = op(op(e3, e1), e1)) | ~ spl48_158), inference(avatar_component_clause, [], [f1192])).
fof(f1192, plain, (spl48_158 <=> (e3 = op(op(e3, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_158])])).
fof(f3298, plain, (~ spl48_49 | ~ spl48_39), inference(avatar_split_clause, [], [f3199, f622, f664])).
fof(f3199, plain, (~ (e3 = op(e3, e0)) | ~ spl48_39), inference(forward_demodulation, [], [f274, f624])).
fof(f274, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f3284, plain, (~ spl48_28 | spl48_54 | ~ spl48_149), inference(avatar_contradiction_clause, [], [f3283])).
fof(f3283, plain, ($false | (~ spl48_28 | spl48_54 | ~ spl48_149)), inference(subsumption_resolution, [], [f3282, f686])).
fof(f686, plain, (~ (e3 = op(e2, e4)) | spl48_54), inference(avatar_component_clause, [], [f685])).
fof(f3282, plain, ((e3 = op(e2, e4)) | (~ spl48_28 | ~ spl48_149)), inference(forward_demodulation, [], [f1149, f578])).
fof(f578, plain, ((e2 = op(e3, e4)) | ~ spl48_28), inference(avatar_component_clause, [], [f576])).
fof(f576, plain, (spl48_28 <=> (e2 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_28])])).
fof(f1149, plain, ((e3 = op(op(e3, e4), e4)) | ~ spl48_149), inference(avatar_component_clause, [], [f1147])).
fof(f1147, plain, (spl48_149 <=> (e3 = op(op(e3, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_149])])).
fof(f3262, plain, (~ spl48_35 | ~ spl48_110), inference(avatar_split_clause, [], [f3258, f920, f605])).
fof(f605, plain, (spl48_35 <=> (e4 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_35])])).
fof(f920, plain, (spl48_110 <=> (e4 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_110])])).
fof(f3258, plain, (~ (e4 = op(e3, e3)) | ~ spl48_110), inference(backward_demodulation, [], [f225, f922])).
fof(f922, plain, ((e4 = op(e0, e3)) | ~ spl48_110), inference(avatar_component_clause, [], [f920])).
fof(f225, plain, ~ (op(e0, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f3247, plain, (~ spl48_23 | ~ spl48_71 | ~ spl48_145), inference(avatar_contradiction_clause, [], [f3246])).
fof(f3246, plain, ($false | (~ spl48_23 | ~ spl48_71 | ~ spl48_145)), inference(subsumption_resolution, [], [f3245, f296])).
fof(f296, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f3245, plain, ((e0 = e4) | (~ spl48_23 | ~ spl48_71 | ~ spl48_145)), inference(forward_demodulation, [], [f3244, f759])).
fof(f759, plain, ((e0 = op(e2, e0)) | ~ spl48_71), inference(avatar_component_clause, [], [f757])).
fof(f757, plain, (spl48_71 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_71])])).
fof(f3244, plain, ((e4 = op(e2, e0)) | (~ spl48_23 | ~ spl48_145)), inference(forward_demodulation, [], [f1130, f557])).
fof(f557, plain, ((e2 = op(e4, e0)) | ~ spl48_23), inference(avatar_component_clause, [], [f555])).
fof(f555, plain, (spl48_23 <=> (e2 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_23])])).
fof(f1130, plain, ((e4 = op(op(e4, e0), e0)) | ~ spl48_145), inference(avatar_component_clause, [], [f1128])).
fof(f1128, plain, (spl48_145 <=> (e4 = op(op(e4, e0), e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_145])])).
fof(f3225, plain, (~ spl48_47 | ~ spl48_107 | spl48_163), inference(avatar_split_clause, [], [f3221, f1216, f908, f656])).
fof(f656, plain, (spl48_47 <=> (e1 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_47])])).
fof(f908, plain, (spl48_107 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_107])])).
fof(f3221, plain, (~ (e1 = op(e3, e0)) | (~ spl48_107 | spl48_163)), inference(backward_demodulation, [], [f1218, f910])).
fof(f910, plain, ((e1 = op(e0, e3)) | ~ spl48_107), inference(avatar_component_clause, [], [f908])).
fof(f3193, plain, (~ spl48_34 | ~ spl48_59), inference(avatar_split_clause, [], [f3192, f706, f601])).
fof(f601, plain, (spl48_34 <=> (e3 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_34])])).
fof(f3192, plain, (~ (e3 = op(e3, e3)) | ~ spl48_59), inference(forward_demodulation, [], [f230, f708])).
fof(f708, plain, ((e3 = op(e2, e3)) | ~ spl48_59), inference(avatar_component_clause, [], [f706])).
fof(f230, plain, ~ (op(e2, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f3183, plain, (spl48_66 | ~ spl48_118 | ~ spl48_206), inference(avatar_contradiction_clause, [], [f3182])).
fof(f3182, plain, ($false | (spl48_66 | ~ spl48_118 | ~ spl48_206)), inference(subsumption_resolution, [], [f3181, f737])).
fof(f737, plain, (~ (e0 = op(e2, e1)) | spl48_66), inference(avatar_component_clause, [], [f736])).
fof(f736, plain, (spl48_66 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_66])])).
fof(f3181, plain, ((e0 = op(e2, e1)) | (~ spl48_118 | ~ spl48_206)), inference(forward_demodulation, [], [f1431, f956])).
fof(f956, plain, ((e2 = op(e0, e1)) | ~ spl48_118), inference(avatar_component_clause, [], [f954])).
fof(f954, plain, (spl48_118 <=> (e2 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_118])])).
fof(f1431, plain, ((e0 = op(op(e0, e1), e1)) | ~ spl48_206), inference(avatar_component_clause, [], [f1429])).
fof(f1429, plain, (spl48_206 <=> (e0 = op(op(e0, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_206])])).
fof(f3180, plain, (~ spl48_71 | ~ spl48_74), inference(avatar_contradiction_clause, [], [f3179])).
fof(f3179, plain, ($false | (~ spl48_71 | ~ spl48_74)), inference(subsumption_resolution, [], [f3178, f295])).
fof(f3178, plain, ((e0 = e3) | (~ spl48_71 | ~ spl48_74)), inference(forward_demodulation, [], [f771, f759])).
fof(f771, plain, ((e3 = op(e2, e0)) | ~ spl48_74), inference(avatar_component_clause, [], [f769])).
fof(f769, plain, (spl48_74 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_74])])).
fof(f3157, plain, (~ spl48_41 | ~ spl48_99 | spl48_192), inference(avatar_contradiction_clause, [], [f3156])).
fof(f3156, plain, ($false | (~ spl48_41 | ~ spl48_99 | spl48_192)), inference(subsumption_resolution, [], [f3155, f633])).
fof(f3155, plain, (~ (e0 = op(e3, e1)) | (~ spl48_99 | spl48_192)), inference(forward_demodulation, [], [f1362, f876])).
fof(f876, plain, ((e3 = op(e1, e0)) | ~ spl48_99), inference(avatar_component_clause, [], [f874])).
fof(f874, plain, (spl48_99 <=> (e3 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_99])])).
fof(f1362, plain, (~ (e0 = op(op(e1, e0), e1)) | spl48_192), inference(avatar_component_clause, [], [f1360])).
fof(f1360, plain, (spl48_192 <=> (e0 = op(op(e1, e0), e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_192])])).
fof(f3147, plain, (~ spl48_76 | spl48_102 | ~ spl48_182), inference(avatar_contradiction_clause, [], [f3146])).
fof(f3146, plain, ($false | (~ spl48_76 | spl48_102 | ~ spl48_182)), inference(subsumption_resolution, [], [f3145, f888])).
fof(f888, plain, (~ (e1 = op(e0, e4)) | spl48_102), inference(avatar_component_clause, [], [f887])).
fof(f887, plain, (spl48_102 <=> (e1 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_102])])).
fof(f3145, plain, ((e1 = op(e0, e4)) | (~ spl48_76 | ~ spl48_182)), inference(forward_demodulation, [], [f1311, f780])).
fof(f780, plain, ((e0 = op(e1, e4)) | ~ spl48_76), inference(avatar_component_clause, [], [f778])).
fof(f778, plain, (spl48_76 <=> (e0 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_76])])).
fof(f1311, plain, ((e1 = op(op(e1, e4), e4)) | ~ spl48_182), inference(avatar_component_clause, [], [f1309])).
fof(f1309, plain, (spl48_182 <=> (e1 = op(op(e1, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_182])])).
fof(f3134, plain, (spl48_26 | ~ spl48_104 | ~ spl48_197), inference(avatar_contradiction_clause, [], [f3133])).
fof(f3133, plain, ($false | (spl48_26 | ~ spl48_104 | ~ spl48_197)), inference(subsumption_resolution, [], [f3132, f569])).
fof(f569, plain, (~ (e0 = op(e3, e4)) | spl48_26), inference(avatar_component_clause, [], [f568])).
fof(f568, plain, (spl48_26 <=> (e0 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_26])])).
fof(f3132, plain, ((e0 = op(e3, e4)) | (~ spl48_104 | ~ spl48_197)), inference(forward_demodulation, [], [f1386, f897])).
fof(f897, plain, ((e3 = op(e0, e4)) | ~ spl48_104), inference(avatar_component_clause, [], [f895])).
fof(f895, plain, (spl48_104 <=> (e3 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_104])])).
fof(f1386, plain, ((e0 = op(op(e0, e4), e4)) | ~ spl48_197), inference(avatar_component_clause, [], [f1384])).
fof(f1384, plain, (spl48_197 <=> (e0 = op(op(e0, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_197])])).
fof(f3129, plain, (~ spl48_71 | ~ spl48_111 | spl48_177), inference(avatar_contradiction_clause, [], [f3128])).
fof(f3128, plain, ($false | (~ spl48_71 | ~ spl48_111 | spl48_177)), inference(subsumption_resolution, [], [f3127, f927])).
fof(f927, plain, ((e0 = op(e0, e2)) | ~ spl48_111), inference(avatar_component_clause, [], [f925])).
fof(f3127, plain, (~ (e0 = op(e0, e2)) | (~ spl48_71 | spl48_177)), inference(forward_demodulation, [], [f1287, f759])).
fof(f1287, plain, (~ (e0 = op(op(e2, e0), e2)) | spl48_177), inference(avatar_component_clause, [], [f1285])).
fof(f1285, plain, (spl48_177 <=> (e0 = op(op(e2, e0), e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_177])])).
fof(f3122, plain, (~ spl48_71 | ~ spl48_111 | spl48_179), inference(avatar_contradiction_clause, [], [f3121])).
fof(f3121, plain, ($false | (~ spl48_71 | ~ spl48_111 | spl48_179)), inference(subsumption_resolution, [], [f3120, f927])).
fof(f3120, plain, (~ (e0 = op(e0, e2)) | (~ spl48_71 | spl48_179)), inference(forward_demodulation, [], [f1297, f759])).
fof(f3118, plain, (~ spl48_19 | ~ spl48_41 | ~ spl48_141), inference(avatar_contradiction_clause, [], [f3117])).
fof(f3117, plain, ($false | (~ spl48_19 | ~ spl48_41 | ~ spl48_141)), inference(subsumption_resolution, [], [f3116, f296])).
fof(f3116, plain, ((e0 = e4) | (~ spl48_19 | ~ spl48_41 | ~ spl48_141)), inference(forward_demodulation, [], [f3115, f633])).
fof(f3115, plain, ((e4 = op(e3, e1)) | (~ spl48_19 | ~ spl48_141)), inference(forward_demodulation, [], [f1111, f540])).
fof(f1111, plain, ((e4 = op(op(e4, e1), e1)) | ~ spl48_141), inference(avatar_component_clause, [], [f1109])).
fof(f1109, plain, (spl48_141 <=> (e4 = op(op(e4, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_141])])).
fof(f3108, plain, (~ spl48_39 | ~ spl48_59 | spl48_155), inference(avatar_contradiction_clause, [], [f3107])).
fof(f3107, plain, ($false | (~ spl48_39 | ~ spl48_59 | spl48_155)), inference(subsumption_resolution, [], [f3106, f708])).
fof(f3106, plain, (~ (e3 = op(e2, e3)) | (~ spl48_39 | spl48_155)), inference(forward_demodulation, [], [f1180, f624])).
fof(f1180, plain, (~ (op(e2, e3) = op(e3, e2)) | spl48_155), inference(avatar_component_clause, [], [f1178])).
fof(f1178, plain, (spl48_155 <=> (op(e2, e3) = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_155])])).
fof(f3104, plain, (~ spl48_50 | ~ spl48_110 | spl48_163), inference(avatar_contradiction_clause, [], [f3103])).
fof(f3103, plain, ($false | (~ spl48_50 | ~ spl48_110 | spl48_163)), inference(subsumption_resolution, [], [f3102, f922])).
fof(f3102, plain, (~ (e4 = op(e0, e3)) | (~ spl48_50 | spl48_163)), inference(forward_demodulation, [], [f1218, f670])).
fof(f670, plain, ((e4 = op(e3, e0)) | ~ spl48_50), inference(avatar_component_clause, [], [f668])).
fof(f668, plain, (spl48_50 <=> (e4 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_50])])).
fof(f3097, plain, (~ spl48_67 | ~ spl48_87 | spl48_175), inference(avatar_contradiction_clause, [], [f3096])).
fof(f3096, plain, ($false | (~ spl48_67 | ~ spl48_87 | spl48_175)), inference(subsumption_resolution, [], [f3095, f826])).
fof(f826, plain, ((e1 = op(e1, e2)) | ~ spl48_87), inference(avatar_component_clause, [], [f824])).
fof(f824, plain, (spl48_87 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_87])])).
fof(f3095, plain, (~ (e1 = op(e1, e2)) | (~ spl48_67 | spl48_175)), inference(forward_demodulation, [], [f1278, f742])).
fof(f742, plain, ((e1 = op(e2, e1)) | ~ spl48_67), inference(avatar_component_clause, [], [f740])).
fof(f1278, plain, (~ (op(e1, e2) = op(e2, e1)) | spl48_175), inference(avatar_component_clause, [], [f1276])).
fof(f1276, plain, (spl48_175 <=> (op(e1, e2) = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_175])])).
fof(f3087, plain, (spl48_57 | ~ spl48_83 | ~ spl48_185), inference(avatar_contradiction_clause, [], [f3086])).
fof(f3086, plain, ($false | (spl48_57 | ~ spl48_83 | ~ spl48_185)), inference(subsumption_resolution, [], [f3085, f699])).
fof(f699, plain, (~ (e1 = op(e2, e3)) | spl48_57), inference(avatar_component_clause, [], [f698])).
fof(f698, plain, (spl48_57 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_57])])).
fof(f3085, plain, ((e1 = op(e2, e3)) | (~ spl48_83 | ~ spl48_185)), inference(forward_demodulation, [], [f1326, f809])).
fof(f809, plain, ((e2 = op(e1, e3)) | ~ spl48_83), inference(avatar_component_clause, [], [f807])).
fof(f807, plain, (spl48_83 <=> (e2 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_83])])).
fof(f1326, plain, ((e1 = op(op(e1, e3), e3)) | ~ spl48_185), inference(avatar_component_clause, [], [f1324])).
fof(f1324, plain, (spl48_185 <=> (e1 = op(op(e1, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_185])])).
fof(f3036, plain, (~ spl48_119 | ~ spl48_19), inference(avatar_split_clause, [], [f2757, f538, f958])).
fof(f2757, plain, (~ (e3 = op(e0, e1)) | ~ spl48_19), inference(forward_demodulation, [], [f206, f540])).
fof(f206, plain, ~ (op(e0, e1) = op(e4, e1)), inference(cnf_transformation, [], [f4])).
fof(f3019, plain, (~ spl48_69 | ~ spl48_19), inference(avatar_split_clause, [], [f2588, f538, f748])).
fof(f748, plain, (spl48_69 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_69])])).
fof(f2588, plain, (~ (e3 = op(e2, e1)) | ~ spl48_19), inference(forward_demodulation, [], [f211, f540])).
fof(f211, plain, ~ (op(e2, e1) = op(e4, e1)), inference(cnf_transformation, [], [f4])).
fof(f3012, plain, (spl48_67 | ~ spl48_128), inference(avatar_split_clause, [], [f2875, f996, f740])).
fof(f996, plain, (spl48_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl48_128])])).
fof(f2875, plain, ((e1 = op(e2, e1)) | ~ spl48_128), inference(backward_demodulation, [], [f134, f998])).
fof(f998, plain, ((e2 = unit) | ~ spl48_128), inference(avatar_component_clause, [], [f996])).
fof(f3009, plain, (spl48_59 | ~ spl48_128), inference(avatar_split_clause, [], [f2879, f996, f706])).
fof(f2879, plain, ((e3 = op(e2, e3)) | ~ spl48_128), inference(backward_demodulation, [], [f138, f998])).
fof(f3008, plain, (spl48_55 | ~ spl48_128), inference(avatar_split_clause, [], [f2881, f996, f689])).
fof(f689, plain, (spl48_55 <=> (e4 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_55])])).
fof(f2881, plain, ((e4 = op(e2, e4)) | ~ spl48_128), inference(backward_demodulation, [], [f140, f998])).
fof(f3007, plain, (~ spl48_55 | ~ spl48_15 | spl48_138), inference(avatar_split_clause, [], [f3006, f1095, f521, f689])).
fof(f521, plain, (spl48_15 <=> (e4 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_15])])).
fof(f1095, plain, (spl48_138 <=> (op(e2, e4) = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_138])])).
fof(f3006, plain, (~ (e4 = op(e2, e4)) | (~ spl48_15 | spl48_138)), inference(forward_demodulation, [], [f1097, f523])).
fof(f523, plain, ((e4 = op(e4, e2)) | ~ spl48_15), inference(avatar_component_clause, [], [f521])).
fof(f1097, plain, (~ (op(e2, e4) = op(e4, e2)) | spl48_138), inference(avatar_component_clause, [], [f1095])).
fof(f2972, plain, (~ spl48_35 | ~ spl48_50), inference(avatar_split_clause, [], [f2970, f668, f605])).
fof(f2970, plain, (~ (e4 = op(e3, e3)) | ~ spl48_50), inference(backward_demodulation, [], [f275, f670])).
fof(f275, plain, ~ (op(e3, e0) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f2971, plain, (~ spl48_25 | ~ spl48_50), inference(avatar_split_clause, [], [f2967, f668, f563])).
fof(f2967, plain, (~ (e4 = op(e4, e0)) | ~ spl48_50), inference(backward_demodulation, [], [f202, f670])).
fof(f202, plain, ~ (op(e3, e0) = op(e4, e0)), inference(cnf_transformation, [], [f4])).
fof(f2937, plain, (~ spl48_76 | ~ spl48_79), inference(avatar_contradiction_clause, [], [f2936])).
fof(f2936, plain, ($false | (~ spl48_76 | ~ spl48_79)), inference(subsumption_resolution, [], [f2935, f295])).
fof(f2935, plain, ((e0 = e3) | (~ spl48_76 | ~ spl48_79)), inference(forward_demodulation, [], [f792, f780])).
fof(f792, plain, ((e3 = op(e1, e4)) | ~ spl48_79), inference(avatar_component_clause, [], [f790])).
fof(f790, plain, (spl48_79 <=> (e3 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_79])])).
fof(f2926, plain, (~ spl48_49 | ~ spl48_99), inference(avatar_split_clause, [], [f2921, f874, f664])).
fof(f2921, plain, (~ (e3 = op(e3, e0)) | ~ spl48_99), inference(backward_demodulation, [], [f198, f876])).
fof(f198, plain, ~ (op(e1, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f2906, plain, (~ spl48_49 | ~ spl48_124), inference(avatar_split_clause, [], [f2900, f979, f664])).
fof(f979, plain, (spl48_124 <=> (op(e0, e0) = e3)), introduced(avatar_definition, [new_symbols(naming, [spl48_124])])).
fof(f2900, plain, (~ (e3 = op(e3, e0)) | ~ spl48_124), inference(backward_demodulation, [], [f195, f981])).
fof(f981, plain, ((op(e0, e0) = e3) | ~ spl48_124), inference(avatar_component_clause, [], [f979])).
fof(f195, plain, ~ (op(e0, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f2897, plain, (spl48_15 | ~ spl48_128), inference(avatar_split_clause, [], [f2882, f996, f521])).
fof(f2882, plain, ((e4 = op(e4, e2)) | ~ spl48_128), inference(backward_demodulation, [], [f141, f998])).
fof(f2894, plain, (spl48_39 | ~ spl48_128), inference(avatar_split_clause, [], [f2880, f996, f622])).
fof(f2880, plain, ((e3 = op(e3, e2)) | ~ spl48_128), inference(backward_demodulation, [], [f139, f998])).
fof(f2891, plain, (spl48_63 | ~ spl48_128), inference(avatar_split_clause, [], [f2878, f996, f723])).
fof(f723, plain, (spl48_63 <=> (e2 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_63])])).
fof(f2878, plain, ((e2 = op(e2, e2)) | ~ spl48_128), inference(backward_demodulation, [], [f137, f998])).
fof(f2889, plain, (spl48_87 | ~ spl48_128), inference(avatar_split_clause, [], [f2876, f996, f824])).
fof(f2876, plain, ((e1 = op(e1, e2)) | ~ spl48_128), inference(backward_demodulation, [], [f135, f998])).
fof(f2886, plain, (spl48_111 | ~ spl48_128), inference(avatar_split_clause, [], [f2874, f996, f925])).
fof(f2874, plain, ((e0 = op(e0, e2)) | ~ spl48_128), inference(backward_demodulation, [], [f133, f998])).
fof(f2885, plain, (spl48_71 | ~ spl48_128), inference(avatar_split_clause, [], [f2873, f996, f757])).
fof(f2873, plain, ((e0 = op(e2, e0)) | ~ spl48_128), inference(backward_demodulation, [], [f132, f998])).
fof(f132, plain, (e0 = op(unit, e0)), inference(cnf_transformation, [], [f2])).
fof(f2862, plain, (~ spl48_73 | ~ spl48_68), inference(avatar_split_clause, [], [f2732, f744, f765])).
fof(f744, plain, (spl48_68 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_68])])).
fof(f2732, plain, (~ (e2 = op(e2, e0)) | ~ spl48_68), inference(forward_demodulation, [], [f263, f746])).
fof(f746, plain, ((e2 = op(e2, e1)) | ~ spl48_68), inference(avatar_component_clause, [], [f744])).
fof(f263, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f2834, plain, (~ spl48_16 | ~ spl48_19), inference(avatar_contradiction_clause, [], [f2833])).
fof(f2833, plain, ($false | (~ spl48_16 | ~ spl48_19)), inference(subsumption_resolution, [], [f2830, f295])).
fof(f2830, plain, ((e0 = e3) | (~ spl48_16 | ~ spl48_19)), inference(backward_demodulation, [], [f540, f528])).
fof(f528, plain, ((e0 = op(e4, e1)) | ~ spl48_16), inference(avatar_component_clause, [], [f526])).
fof(f526, plain, (spl48_16 <=> (e0 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_16])])).
fof(f2826, plain, (~ spl48_34 | ~ spl48_49), inference(avatar_split_clause, [], [f2825, f664, f601])).
fof(f2825, plain, (~ (e3 = op(e3, e3)) | ~ spl48_49), inference(backward_demodulation, [], [f275, f666])).
fof(f2810, plain, (~ spl48_8 | ~ spl48_83), inference(avatar_split_clause, [], [f2807, f807, f492])).
fof(f492, plain, (spl48_8 <=> (e2 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_8])])).
fof(f2807, plain, (~ (e2 = op(e4, e3)) | ~ spl48_83), inference(backward_demodulation, [], [f229, f809])).
fof(f229, plain, ~ (op(e1, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f2798, plain, (~ spl48_82 | ~ spl48_97), inference(avatar_split_clause, [], [f2794, f866, f803])).
fof(f803, plain, (spl48_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_82])])).
fof(f2794, plain, (~ (e1 = op(e1, e3)) | ~ spl48_97), inference(backward_demodulation, [], [f255, f868])).
fof(f868, plain, ((e1 = op(e1, e0)) | ~ spl48_97), inference(avatar_component_clause, [], [f866])).
fof(f255, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2785, plain, (~ spl48_13 | ~ spl48_113), inference(avatar_split_clause, [], [f2782, f933, f513])).
fof(f513, plain, (spl48_13 <=> (e2 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_13])])).
fof(f2782, plain, (~ (e2 = op(e4, e2)) | ~ spl48_113), inference(backward_demodulation, [], [f216, f935])).
fof(f216, plain, ~ (op(e0, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f2784, plain, (~ spl48_108 | ~ spl48_113), inference(avatar_split_clause, [], [f2781, f933, f912])).
fof(f2781, plain, (~ (e2 = op(e0, e3)) | ~ spl48_113), inference(backward_demodulation, [], [f250, f935])).
fof(f250, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f2740, plain, (~ spl48_85 | ~ spl48_95), inference(avatar_split_clause, [], [f2739, f857, f815])).
fof(f857, plain, (spl48_95 <=> (e4 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_95])])).
fof(f2739, plain, (~ (e4 = op(e1, e3)) | ~ spl48_95), inference(forward_demodulation, [], [f258, f859])).
fof(f859, plain, ((e4 = op(e1, e1)) | ~ spl48_95), inference(avatar_component_clause, [], [f857])).
fof(f258, plain, ~ (op(e1, e1) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2715, plain, (~ spl48_8 | ~ spl48_28 | spl48_134), inference(avatar_split_clause, [], [f2714, f1076, f576, f492])).
fof(f1076, plain, (spl48_134 <=> (op(e3, e4) = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_134])])).
fof(f2714, plain, (~ (e2 = op(e4, e3)) | (~ spl48_28 | spl48_134)), inference(forward_demodulation, [], [f1078, f578])).
fof(f1078, plain, (~ (op(e3, e4) = op(e4, e3)) | spl48_134), inference(avatar_component_clause, [], [f1076])).
fof(f2649, plain, (~ spl48_28 | ~ spl48_129), inference(avatar_contradiction_clause, [], [f2648])).
fof(f2648, plain, ($false | (~ spl48_28 | ~ spl48_129)), inference(subsumption_resolution, [], [f2647, f301])).
fof(f2647, plain, ((e2 = e4) | (~ spl48_28 | ~ spl48_129)), inference(forward_demodulation, [], [f2632, f578])).
fof(f2632, plain, ((e4 = op(e3, e4)) | ~ spl48_129), inference(backward_demodulation, [], [f140, f1002])).
fof(f1002, plain, ((e3 = unit) | ~ spl48_129), inference(avatar_component_clause, [], [f1000])).
fof(f1000, plain, (spl48_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl48_129])])).
fof(f2603, plain, (~ spl48_94 | ~ spl48_19), inference(avatar_split_clause, [], [f2602, f538, f853])).
fof(f853, plain, (spl48_94 <=> (e3 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_94])])).
fof(f2602, plain, (~ (e3 = op(e1, e1)) | ~ spl48_19), inference(forward_demodulation, [], [f209, f540])).
fof(f209, plain, ~ (op(e1, e1) = op(e4, e1)), inference(cnf_transformation, [], [f4])).
fof(f2598, plain, (~ spl48_95 | ~ spl48_120), inference(avatar_contradiction_clause, [], [f2597])).
fof(f2597, plain, ($false | (~ spl48_95 | ~ spl48_120)), inference(subsumption_resolution, [], [f2596, f964])).
fof(f964, plain, ((e4 = op(e0, e1)) | ~ spl48_120), inference(avatar_component_clause, [], [f962])).
fof(f962, plain, (spl48_120 <=> (e4 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_120])])).
fof(f2596, plain, (~ (e4 = op(e0, e1)) | ~ spl48_95), inference(forward_demodulation, [], [f203, f859])).
fof(f203, plain, ~ (op(e0, e1) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f2594, plain, (~ spl48_87 | ~ spl48_82), inference(avatar_split_clause, [], [f2593, f803, f824])).
fof(f2593, plain, (~ (e1 = op(e1, e2)) | ~ spl48_82), inference(forward_demodulation, [], [f260, f805])).
fof(f805, plain, ((e1 = op(e1, e3)) | ~ spl48_82), inference(avatar_component_clause, [], [f803])).
fof(f260, plain, ~ (op(e1, e2) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2543, plain, (~ spl48_23 | ~ spl48_25), inference(avatar_contradiction_clause, [], [f2542])).
fof(f2542, plain, ($false | (~ spl48_23 | ~ spl48_25)), inference(subsumption_resolution, [], [f2541, f301])).
fof(f2541, plain, ((e2 = e4) | (~ spl48_23 | ~ spl48_25)), inference(backward_demodulation, [], [f565, f557])).
fof(f565, plain, ((e4 = op(e4, e0)) | ~ spl48_25), inference(avatar_component_clause, [], [f563])).
fof(f2518, plain, (~ spl48_59 | ~ spl48_60), inference(avatar_contradiction_clause, [], [f2517])).
fof(f2517, plain, ($false | (~ spl48_59 | ~ spl48_60)), inference(subsumption_resolution, [], [f2516, f302])).
fof(f2516, plain, ((e3 = e4) | (~ spl48_59 | ~ spl48_60)), inference(backward_demodulation, [], [f712, f708])).
fof(f712, plain, ((e4 = op(e2, e3)) | ~ spl48_60), inference(avatar_component_clause, [], [f710])).
fof(f710, plain, (spl48_60 <=> (e4 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_60])])).
fof(f2515, plain, (~ spl48_66 | ~ spl48_67), inference(avatar_contradiction_clause, [], [f2514])).
fof(f2514, plain, ($false | (~ spl48_66 | ~ spl48_67)), inference(subsumption_resolution, [], [f2513, f293])).
fof(f293, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f2513, plain, ((e0 = e1) | (~ spl48_66 | ~ spl48_67)), inference(backward_demodulation, [], [f742, f738])).
fof(f738, plain, ((e0 = op(e2, e1)) | ~ spl48_66), inference(avatar_component_clause, [], [f736])).
fof(f2504, plain, (~ spl48_93 | ~ spl48_95), inference(avatar_contradiction_clause, [], [f2503])).
fof(f2503, plain, ($false | (~ spl48_93 | ~ spl48_95)), inference(subsumption_resolution, [], [f2502, f301])).
fof(f2502, plain, ((e2 = e4) | (~ spl48_93 | ~ spl48_95)), inference(forward_demodulation, [], [f859, f851])).
fof(f851, plain, ((e2 = op(e1, e1)) | ~ spl48_93), inference(avatar_component_clause, [], [f849])).
fof(f849, plain, (spl48_93 <=> (e2 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_93])])).
fof(f2499, plain, (~ spl48_84 | ~ spl48_99), inference(avatar_split_clause, [], [f2496, f874, f811])).
fof(f811, plain, (spl48_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_84])])).
fof(f2496, plain, (~ (e3 = op(e1, e3)) | ~ spl48_99), inference(backward_demodulation, [], [f255, f876])).
fof(f2492, plain, (~ spl48_116 | ~ spl48_121), inference(avatar_split_clause, [], [f2489, f967, f946])).
fof(f946, plain, (spl48_116 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_116])])).
fof(f2489, plain, (~ (e0 = op(e0, e1)) | ~ spl48_121), inference(backward_demodulation, [], [f243, f969])).
fof(f243, plain, ~ (op(e0, e0) = op(e0, e1)), inference(cnf_transformation, [], [f4])).
fof(f2485, plain, (~ spl48_2 | ~ spl48_130), inference(avatar_contradiction_clause, [], [f2484])).
fof(f2484, plain, ($false | (~ spl48_2 | ~ spl48_130)), inference(subsumption_resolution, [], [f2483, f299])).
fof(f299, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f2483, plain, ((e1 = e4) | (~ spl48_2 | ~ spl48_130)), inference(forward_demodulation, [], [f2460, f469])).
fof(f469, plain, ((e1 = op(e4, e4)) | ~ spl48_2), inference(avatar_component_clause, [], [f467])).
fof(f467, plain, (spl48_2 <=> (e1 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_2])])).
fof(f2460, plain, ((e4 = op(e4, e4)) | ~ spl48_130), inference(backward_demodulation, [], [f141, f1006])).
fof(f1006, plain, ((e4 = unit) | ~ spl48_130), inference(avatar_component_clause, [], [f1004])).
fof(f1004, plain, (spl48_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl48_130])])).
fof(f2443, plain, (~ spl48_123 | ~ spl48_73), inference(avatar_split_clause, [], [f2442, f765, f975])).
fof(f975, plain, (spl48_123 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl48_123])])).
fof(f2442, plain, (~ (op(e0, e0) = e2) | ~ spl48_73), inference(forward_demodulation, [], [f194, f767])).
fof(f194, plain, ~ (op(e0, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f2409, plain, (~ spl48_85 | ~ spl48_60), inference(avatar_split_clause, [], [f2405, f710, f815])).
fof(f2405, plain, (~ (e4 = op(e1, e3)) | ~ spl48_60), inference(backward_demodulation, [], [f227, f712])).
fof(f227, plain, ~ (op(e1, e3) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f2380, plain, (~ spl48_33 | ~ spl48_28), inference(avatar_split_clause, [], [f2379, f576, f597])).
fof(f597, plain, (spl48_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_33])])).
fof(f2379, plain, (~ (e2 = op(e3, e3)) | ~ spl48_28), inference(forward_demodulation, [], [f282, f578])).
fof(f282, plain, ~ (op(e3, e3) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f2370, plain, (~ spl48_2 | ~ spl48_3), inference(avatar_contradiction_clause, [], [f2369])).
fof(f2369, plain, ($false | (~ spl48_2 | ~ spl48_3)), inference(subsumption_resolution, [], [f2368, f297])).
fof(f297, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f2368, plain, ((e1 = e2) | (~ spl48_2 | ~ spl48_3)), inference(forward_demodulation, [], [f473, f469])).
fof(f473, plain, ((e2 = op(e4, e4)) | ~ spl48_3), inference(avatar_component_clause, [], [f471])).
fof(f471, plain, (spl48_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_3])])).
fof(f2343, plain, (~ spl48_63 | ~ spl48_73), inference(avatar_split_clause, [], [f2341, f765, f723])).
fof(f2341, plain, (~ (e2 = op(e2, e2)) | ~ spl48_73), inference(backward_demodulation, [], [f264, f767])).
fof(f264, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f2314, plain, (~ spl48_54 | ~ spl48_128), inference(avatar_contradiction_clause, [], [f2313])).
fof(f2313, plain, ($false | (~ spl48_54 | ~ spl48_128)), inference(subsumption_resolution, [], [f2312, f302])).
fof(f2312, plain, ((e3 = e4) | (~ spl48_54 | ~ spl48_128)), inference(forward_demodulation, [], [f2298, f687])).
fof(f2298, plain, ((e4 = op(e2, e4)) | ~ spl48_128), inference(backward_demodulation, [], [f140, f998])).
fof(f2275, plain, (~ spl48_124 | ~ spl48_104), inference(avatar_split_clause, [], [f2274, f895, f979])).
fof(f2274, plain, (~ (op(e0, e0) = e3) | ~ spl48_104), inference(forward_demodulation, [], [f246, f897])).
fof(f246, plain, ~ (op(e0, e0) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f2271, plain, (~ spl48_118 | ~ spl48_113), inference(avatar_split_clause, [], [f2270, f933, f954])).
fof(f2270, plain, (~ (e2 = op(e0, e1)) | ~ spl48_113), inference(forward_demodulation, [], [f247, f935])).
fof(f247, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f4])).
fof(f2269, plain, (~ spl48_109 | ~ spl48_84), inference(avatar_split_clause, [], [f2268, f811, f916])).
fof(f2268, plain, (~ (e3 = op(e0, e3)) | ~ spl48_84), inference(forward_demodulation, [], [f223, f813])).
fof(f813, plain, ((e3 = op(e1, e3)) | ~ spl48_84), inference(avatar_component_clause, [], [f811])).
fof(f223, plain, ~ (op(e0, e3) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2262, plain, (~ spl48_102 | ~ spl48_2), inference(avatar_split_clause, [], [f2031, f467, f887])).
fof(f2031, plain, (~ (e1 = op(e0, e4)) | ~ spl48_2), inference(forward_demodulation, [], [f236, f469])).
fof(f236, plain, ~ (op(e0, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2260, plain, (~ spl48_92 | ~ spl48_67), inference(avatar_split_clause, [], [f2259, f740, f845])).
fof(f845, plain, (spl48_92 <=> (e1 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_92])])).
fof(f2259, plain, (~ (e1 = op(e1, e1)) | ~ spl48_67), inference(forward_demodulation, [], [f207, f742])).
fof(f207, plain, ~ (op(e1, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f2238, plain, (~ spl48_62 | ~ spl48_37), inference(avatar_split_clause, [], [f2237, f614, f719])).
fof(f719, plain, (spl48_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_62])])).
fof(f2237, plain, (~ (e1 = op(e2, e2)) | ~ spl48_37), inference(forward_demodulation, [], [f220, f616])).
fof(f616, plain, ((e1 = op(e3, e2)) | ~ spl48_37), inference(avatar_component_clause, [], [f614])).
fof(f220, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f2226, plain, (~ spl48_73 | ~ spl48_58), inference(avatar_split_clause, [], [f2225, f702, f765])).
fof(f702, plain, (spl48_58 <=> (e2 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_58])])).
fof(f2225, plain, (~ (e2 = op(e2, e0)) | ~ spl48_58), inference(forward_demodulation, [], [f265, f704])).
fof(f704, plain, ((e2 = op(e2, e3)) | ~ spl48_58), inference(avatar_component_clause, [], [f702])).
fof(f265, plain, ~ (op(e2, e0) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f2200, plain, (~ spl48_43 | ~ spl48_28), inference(avatar_split_clause, [], [f2199, f576, f639])).
fof(f639, plain, (spl48_43 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_43])])).
fof(f2199, plain, (~ (e2 = op(e3, e1)) | ~ spl48_28), inference(forward_demodulation, [], [f279, f578])).
fof(f279, plain, ~ (op(e3, e1) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f2190, plain, (~ spl48_19 | ~ spl48_24), inference(avatar_split_clause, [], [f2185, f559, f538])).
fof(f559, plain, (spl48_24 <=> (e3 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_24])])).
fof(f2185, plain, (~ (e3 = op(e4, e1)) | ~ spl48_24), inference(backward_demodulation, [], [f283, f561])).
fof(f561, plain, ((e3 = op(e4, e0)) | ~ spl48_24), inference(avatar_component_clause, [], [f559])).
fof(f283, plain, ~ (op(e4, e0) = op(e4, e1)), inference(cnf_transformation, [], [f4])).
fof(f2179, plain, (spl48_19 | ~ spl48_2), inference(avatar_split_clause, [], [f1922, f467, f538])).
fof(f1922, plain, ((e3 = op(e4, e1)) | ~ spl48_2), inference(forward_demodulation, [], [f306, f469])).
fof(f306, plain, (e3 = op(e4, op(e4, e4))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e3 = op(e4, op(e4, e4))) & (e2 = op(op(e4, op(e4, e4)), e4)) & (e1 = op(e4, e4)) & (e0 = op(op(e4, e4), e4))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG060+1.p', ax6)).
fof(f2166, plain, (~ spl48_104 | ~ spl48_6 | spl48_132), inference(avatar_split_clause, [], [f2165, f1066, f484, f895])).
fof(f484, plain, (spl48_6 <=> (e0 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_6])])).
fof(f1066, plain, (spl48_132 <=> (e3 = op(op(e4, e3), e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_132])])).
fof(f2165, plain, (~ (e3 = op(e0, e4)) | (~ spl48_6 | spl48_132)), inference(forward_demodulation, [], [f1068, f486])).
fof(f486, plain, ((e0 = op(e4, e3)) | ~ spl48_6), inference(avatar_component_clause, [], [f484])).
fof(f1068, plain, (~ (e3 = op(op(e4, e3), e4)) | spl48_132), inference(avatar_component_clause, [], [f1066])).
fof(f2162, plain, (~ spl48_1 | ~ spl48_2), inference(avatar_contradiction_clause, [], [f2161])).
fof(f2161, plain, ($false | (~ spl48_1 | ~ spl48_2)), inference(subsumption_resolution, [], [f2160, f293])).
fof(f2160, plain, ((e0 = e1) | (~ spl48_1 | ~ spl48_2)), inference(backward_demodulation, [], [f469, f465])).
fof(f465, plain, ((e0 = op(e4, e4)) | ~ spl48_1), inference(avatar_component_clause, [], [f463])).
fof(f463, plain, (spl48_1 <=> (e0 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_1])])).
fof(f2145, plain, (~ spl48_41 | ~ spl48_45), inference(avatar_contradiction_clause, [], [f2144])).
fof(f2144, plain, ($false | (~ spl48_41 | ~ spl48_45)), inference(subsumption_resolution, [], [f2143, f296])).
fof(f2143, plain, ((e0 = e4) | (~ spl48_41 | ~ spl48_45)), inference(forward_demodulation, [], [f649, f633])).
fof(f649, plain, ((e4 = op(e3, e1)) | ~ spl48_45), inference(avatar_component_clause, [], [f647])).
fof(f647, plain, (spl48_45 <=> (e4 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_45])])).
fof(f2129, plain, (~ spl48_62 | ~ spl48_67), inference(avatar_split_clause, [], [f2128, f740, f719])).
fof(f2128, plain, (~ (e1 = op(e2, e2)) | ~ spl48_67), inference(backward_demodulation, [], [f267, f742])).
fof(f267, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f2115, plain, (~ spl48_92 | ~ spl48_97), inference(avatar_split_clause, [], [f2111, f866, f845])).
fof(f2111, plain, (~ (e1 = op(e1, e1)) | ~ spl48_97), inference(backward_demodulation, [], [f253, f868])).
fof(f253, plain, ~ (op(e1, e0) = op(e1, e1)), inference(cnf_transformation, [], [f4])).
fof(f2098, plain, (~ spl48_108 | ~ spl48_118), inference(avatar_split_clause, [], [f2094, f954, f912])).
fof(f2094, plain, (~ (e2 = op(e0, e3)) | ~ spl48_118), inference(backward_demodulation, [], [f248, f956])).
fof(f248, plain, ~ (op(e0, e1) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f2070, plain, (spl48_96 | ~ spl48_127), inference(avatar_split_clause, [], [f2060, f992, f862])).
fof(f862, plain, (spl48_96 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_96])])).
fof(f992, plain, (spl48_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl48_127])])).
fof(f2060, plain, ((e0 = op(e1, e0)) | ~ spl48_127), inference(backward_demodulation, [], [f132, f994])).
fof(f994, plain, ((e1 = unit) | ~ spl48_127), inference(avatar_component_clause, [], [f992])).
fof(f2011, plain, (~ spl48_85 | ~ spl48_35), inference(avatar_split_clause, [], [f2010, f605, f815])).
fof(f2010, plain, (~ (e4 = op(e1, e3)) | ~ spl48_35), inference(forward_demodulation, [], [f228, f607])).
fof(f607, plain, ((e4 = op(e3, e3)) | ~ spl48_35), inference(avatar_component_clause, [], [f605])).
fof(f228, plain, ~ (op(e1, e3) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f1992, plain, (~ spl48_67 | ~ spl48_57), inference(avatar_split_clause, [], [f1991, f698, f740])).
fof(f1991, plain, (~ (e1 = op(e2, e1)) | ~ spl48_57), inference(forward_demodulation, [], [f268, f700])).
fof(f700, plain, ((e1 = op(e2, e3)) | ~ spl48_57), inference(avatar_component_clause, [], [f698])).
fof(f268, plain, ~ (op(e2, e1) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f1986, plain, (~ spl48_62 | ~ spl48_57), inference(avatar_split_clause, [], [f1985, f698, f719])).
fof(f1985, plain, (~ (e1 = op(e2, e2)) | ~ spl48_57), inference(forward_demodulation, [], [f270, f700])).
fof(f270, plain, ~ (op(e2, e2) = op(e2, e3)), inference(cnf_transformation, [], [f4])).
fof(f1969, plain, (~ spl48_48 | ~ spl48_28), inference(avatar_split_clause, [], [f1968, f576, f660])).
fof(f660, plain, (spl48_48 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_48])])).
fof(f1968, plain, (~ (e2 = op(e3, e0)) | ~ spl48_28), inference(forward_demodulation, [], [f276, f578])).
fof(f276, plain, ~ (op(e3, e0) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1933, plain, (~ spl48_13 | ~ spl48_15), inference(avatar_contradiction_clause, [], [f1932])).
fof(f1932, plain, ($false | (~ spl48_13 | ~ spl48_15)), inference(subsumption_resolution, [], [f1931, f301])).
fof(f1931, plain, ((e2 = e4) | (~ spl48_13 | ~ spl48_15)), inference(forward_demodulation, [], [f523, f515])).
fof(f515, plain, ((e2 = op(e4, e2)) | ~ spl48_13), inference(avatar_component_clause, [], [f513])).
fof(f1927, plain, (spl48_76 | ~ spl48_2), inference(avatar_split_clause, [], [f1926, f467, f778])).
fof(f1926, plain, ((e0 = op(e1, e4)) | ~ spl48_2), inference(forward_demodulation, [], [f303, f469])).
fof(f303, plain, (e0 = op(op(e4, e4), e4)), inference(cnf_transformation, [], [f6])).
fof(f1921, plain, (~ spl48_2 | ~ spl48_4), inference(avatar_contradiction_clause, [], [f1920])).
fof(f1920, plain, ($false | (~ spl48_2 | ~ spl48_4)), inference(subsumption_resolution, [], [f1919, f298])).
fof(f298, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f1919, plain, ((e1 = e3) | (~ spl48_2 | ~ spl48_4)), inference(backward_demodulation, [], [f477, f469])).
fof(f477, plain, ((e3 = op(e4, e4)) | ~ spl48_4), inference(avatar_component_clause, [], [f475])).
fof(f475, plain, (spl48_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_4])])).
fof(f1912, plain, (~ spl48_6 | ~ spl48_8), inference(avatar_contradiction_clause, [], [f1911])).
fof(f1911, plain, ($false | (~ spl48_6 | ~ spl48_8)), inference(subsumption_resolution, [], [f1910, f294])).
fof(f294, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f1910, plain, ((e0 = e2) | (~ spl48_6 | ~ spl48_8)), inference(backward_demodulation, [], [f494, f486])).
fof(f494, plain, ((e2 = op(e4, e3)) | ~ spl48_8), inference(avatar_component_clause, [], [f492])).
fof(f1891, plain, (~ spl48_5 | ~ spl48_15), inference(avatar_split_clause, [], [f1889, f521, f479])).
fof(f479, plain, (spl48_5 <=> (e4 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_5])])).
fof(f1889, plain, (~ (e4 = op(e4, e4)) | ~ spl48_15), inference(backward_demodulation, [], [f291, f523])).
fof(f291, plain, ~ (op(e4, e2) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1890, plain, (~ spl48_10 | ~ spl48_15), inference(avatar_split_clause, [], [f1888, f521, f500])).
fof(f500, plain, (spl48_10 <=> (e4 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_10])])).
fof(f1888, plain, (~ (e4 = op(e4, e3)) | ~ spl48_15), inference(backward_demodulation, [], [f290, f523])).
fof(f290, plain, ~ (op(e4, e2) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f1884, plain, (~ spl48_18 | ~ spl48_19), inference(avatar_contradiction_clause, [], [f1883])).
fof(f1883, plain, ($false | (~ spl48_18 | ~ spl48_19)), inference(subsumption_resolution, [], [f1882, f300])).
fof(f300, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f1882, plain, ((e2 = e3) | (~ spl48_18 | ~ spl48_19)), inference(backward_demodulation, [], [f540, f536])).
fof(f536, plain, ((e2 = op(e4, e1)) | ~ spl48_18), inference(avatar_component_clause, [], [f534])).
fof(f1863, plain, (~ spl48_5 | ~ spl48_25), inference(avatar_split_clause, [], [f1859, f563, f479])).
fof(f1859, plain, (~ (e4 = op(e4, e4)) | ~ spl48_25), inference(backward_demodulation, [], [f286, f565])).
fof(f286, plain, ~ (op(e4, e0) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1852, plain, (~ spl48_28 | ~ spl48_29), inference(avatar_contradiction_clause, [], [f1851])).
fof(f1851, plain, ($false | (~ spl48_28 | ~ spl48_29)), inference(subsumption_resolution, [], [f1850, f300])).
fof(f1850, plain, ((e2 = e3) | (~ spl48_28 | ~ spl48_29)), inference(backward_demodulation, [], [f582, f578])).
fof(f582, plain, ((e3 = op(e3, e4)) | ~ spl48_29), inference(avatar_component_clause, [], [f580])).
fof(f580, plain, (spl48_29 <=> (e3 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_29])])).
fof(f1835, plain, (~ spl48_26 | ~ spl48_41), inference(avatar_split_clause, [], [f1831, f631, f568])).
fof(f1831, plain, (~ (e0 = op(e3, e4)) | ~ spl48_41), inference(backward_demodulation, [], [f279, f633])).
fof(f1807, plain, (~ spl48_30 | ~ spl48_55), inference(avatar_split_clause, [], [f1805, f689, f584])).
fof(f584, plain, (spl48_30 <=> (e4 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl48_30])])).
fof(f1805, plain, (~ (e4 = op(e3, e4)) | ~ spl48_55), inference(backward_demodulation, [], [f240, f691])).
fof(f691, plain, ((e4 = op(e2, e4)) | ~ spl48_55), inference(avatar_component_clause, [], [f689])).
fof(f240, plain, ~ (op(e2, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1789, plain, (~ spl48_56 | ~ spl48_66), inference(avatar_split_clause, [], [f1784, f736, f694])).
fof(f1784, plain, (~ (e0 = op(e2, e3)) | ~ spl48_66), inference(backward_demodulation, [], [f268, f738])).
fof(f1776, plain, (~ spl48_21 | ~ spl48_71), inference(avatar_split_clause, [], [f1770, f757, f547])).
fof(f547, plain, (spl48_21 <=> (e0 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_21])])).
fof(f1770, plain, (~ (e0 = op(e4, e0)) | ~ spl48_71), inference(backward_demodulation, [], [f201, f759])).
fof(f201, plain, ~ (op(e2, e0) = op(e4, e0)), inference(cnf_transformation, [], [f4])).
fof(f1775, plain, (~ spl48_46 | ~ spl48_71), inference(avatar_split_clause, [], [f1769, f757, f652])).
fof(f652, plain, (spl48_46 <=> (e0 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl48_46])])).
fof(f1769, plain, (~ (e0 = op(e3, e0)) | ~ spl48_71), inference(backward_demodulation, [], [f200, f759])).
fof(f200, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f1753, plain, (~ spl48_76 | ~ spl48_81), inference(avatar_split_clause, [], [f1749, f799, f778])).
fof(f799, plain, (spl48_81 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_81])])).
fof(f1749, plain, (~ (e0 = op(e1, e4)) | ~ spl48_81), inference(backward_demodulation, [], [f262, f801])).
fof(f801, plain, ((e0 = op(e1, e3)) | ~ spl48_81), inference(avatar_component_clause, [], [f799])).
fof(f262, plain, ~ (op(e1, e3) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f1735, plain, (~ spl48_76 | ~ spl48_91), inference(avatar_split_clause, [], [f1729, f841, f778])).
fof(f841, plain, (spl48_91 <=> (e0 = op(e1, e1))), introduced(avatar_definition, [new_symbols(naming, [spl48_91])])).
fof(f1729, plain, (~ (e0 = op(e1, e4)) | ~ spl48_91), inference(backward_demodulation, [], [f259, f843])).
fof(f843, plain, ((e0 = op(e1, e1)) | ~ spl48_91), inference(avatar_component_clause, [], [f841])).
fof(f259, plain, ~ (op(e1, e1) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f1723, plain, (~ spl48_76 | ~ spl48_96), inference(avatar_split_clause, [], [f1716, f862, f778])).
fof(f1716, plain, (~ (e0 = op(e1, e4)) | ~ spl48_96), inference(backward_demodulation, [], [f256, f864])).
fof(f864, plain, ((e0 = op(e1, e0)) | ~ spl48_96), inference(avatar_component_clause, [], [f862])).
fof(f256, plain, ~ (op(e1, e0) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f1703, plain, (~ spl48_104 | ~ spl48_105), inference(avatar_contradiction_clause, [], [f1702])).
fof(f1702, plain, ($false | (~ spl48_104 | ~ spl48_105)), inference(subsumption_resolution, [], [f1701, f302])).
fof(f1701, plain, ((e3 = e4) | (~ spl48_104 | ~ spl48_105)), inference(backward_demodulation, [], [f901, f897])).
fof(f1699, plain, (~ spl48_30 | ~ spl48_105), inference(avatar_split_clause, [], [f1695, f899, f584])).
fof(f1695, plain, (~ (e4 = op(e3, e4)) | ~ spl48_105), inference(backward_demodulation, [], [f235, f901])).
fof(f235, plain, ~ (op(e0, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1681, plain, (~ spl48_106 | ~ spl48_111), inference(avatar_split_clause, [], [f1675, f925, f904])).
fof(f904, plain, (spl48_106 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl48_106])])).
fof(f1675, plain, (~ (e0 = op(e0, e3)) | ~ spl48_111), inference(backward_demodulation, [], [f250, f927])).
fof(f1680, plain, (~ spl48_11 | ~ spl48_111), inference(avatar_split_clause, [], [f1674, f925, f505])).
fof(f1674, plain, (~ (e0 = op(e4, e2)) | ~ spl48_111), inference(backward_demodulation, [], [f216, f927])).
fof(f1678, plain, (~ spl48_61 | ~ spl48_111), inference(avatar_split_clause, [], [f1672, f925, f715])).
fof(f715, plain, (spl48_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl48_61])])).
fof(f1672, plain, (~ (e0 = op(e2, e2)) | ~ spl48_111), inference(backward_demodulation, [], [f214, f927])).
fof(f214, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f1668, plain, (~ spl48_111 | ~ spl48_116), inference(avatar_split_clause, [], [f1661, f946, f925])).
fof(f1661, plain, (~ (e0 = op(e0, e2)) | ~ spl48_116), inference(backward_demodulation, [], [f247, f948])).
fof(f948, plain, ((e0 = op(e0, e1)) | ~ spl48_116), inference(avatar_component_clause, [], [f946])).
fof(f1652, plain, (~ spl48_21 | ~ spl48_121), inference(avatar_split_clause, [], [f1644, f967, f547])).
fof(f1644, plain, (~ (e0 = op(e4, e0)) | ~ spl48_121), inference(backward_demodulation, [], [f196, f969])).
fof(f196, plain, ~ (op(e0, e0) = op(e4, e0)), inference(cnf_transformation, [], [f4])).
fof(f1620, plain, (spl48_232 | spl48_231 | spl48_230 | spl48_229 | spl48_228 | spl48_227 | spl48_226 | spl48_225 | spl48_224 | spl48_223 | spl48_222 | spl48_221 | spl48_220 | spl48_219 | spl48_218 | spl48_217 | spl48_216 | spl48_215 | spl48_214 | spl48_213 | spl48_212 | spl48_211 | spl48_210 | spl48_209 | spl48_5), inference(avatar_split_clause, [], [f451, f479, f1446, f1453, f1460, f1467, f1474, f1481, f1488, f1495, f1502, f1509, f1516, f1523, f1530, f1537, f1544, f1551, f1558, f1565, f1572, f1579, f1586, f1593, f1600, f1607])).
fof(f1607, plain, (spl48_232 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl48_232])])).
fof(f1600, plain, (spl48_231 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl48_231])])).
fof(f1593, plain, (spl48_230 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl48_230])])).
fof(f1586, plain, (spl48_229 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl48_229])])).
fof(f1579, plain, (spl48_228 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl48_228])])).
fof(f1572, plain, (spl48_227 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl48_227])])).
fof(f1565, plain, (spl48_226 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl48_226])])).
fof(f1558, plain, (spl48_225 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl48_225])])).
fof(f1551, plain, (spl48_224 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl48_224])])).
fof(f1544, plain, (spl48_223 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl48_223])])).
fof(f1537, plain, (spl48_222 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl48_222])])).
fof(f1530, plain, (spl48_221 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl48_221])])).
fof(f1523, plain, (spl48_220 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl48_220])])).
fof(f1516, plain, (spl48_219 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl48_219])])).
fof(f1509, plain, (spl48_218 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl48_218])])).
fof(f1502, plain, (spl48_217 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl48_217])])).
fof(f1495, plain, (spl48_216 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl48_216])])).
fof(f1488, plain, (spl48_215 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl48_215])])).
fof(f1481, plain, (spl48_214 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl48_214])])).
fof(f1474, plain, (spl48_213 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl48_213])])).
fof(f1467, plain, (spl48_212 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl48_212])])).
fof(f1460, plain, (spl48_211 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl48_211])])).
fof(f1453, plain, (spl48_210 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl48_210])])).
fof(f1446, plain, (spl48_209 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl48_209])])).
fof(f451, plain, ((e4 = op(e4, e4)) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f58])).
fof(f58, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24) & ((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0)), inference(definition_folding, [], [f9, e57, e56, e55, e54, e53, e52, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
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
fof(f34, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP24), inference(usedef, [], [e34])).
fof(e34, plain, (sP24 <=> (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f35, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP25), inference(usedef, [], [e35])).
fof(e35, plain, (sP25 <=> (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f36, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP26), inference(usedef, [], [e36])).
fof(e36, plain, (sP26 <=> (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f37, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP27), inference(usedef, [], [e37])).
fof(e37, plain, (sP27 <=> (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f38, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP28), inference(usedef, [], [e38])).
fof(e38, plain, (sP28 <=> (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f39, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP29), inference(usedef, [], [e39])).
fof(e39, plain, (sP29 <=> (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f40, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP30), inference(usedef, [], [e40])).
fof(e40, plain, (sP30 <=> (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f41, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP31), inference(usedef, [], [e41])).
fof(e41, plain, (sP31 <=> (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f42, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP32), inference(usedef, [], [e42])).
fof(e42, plain, (sP32 <=> (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f43, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP33), inference(usedef, [], [e43])).
fof(e43, plain, (sP33 <=> (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f44, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP34), inference(usedef, [], [e44])).
fof(e44, plain, (sP34 <=> (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f45, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP35), inference(usedef, [], [e45])).
fof(e45, plain, (sP35 <=> (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f46, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP36), inference(usedef, [], [e46])).
fof(e46, plain, (sP36 <=> (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f47, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP37), inference(usedef, [], [e47])).
fof(e47, plain, (sP37 <=> (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f48, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP38), inference(usedef, [], [e48])).
fof(e48, plain, (sP38 <=> (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f49, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP39), inference(usedef, [], [e49])).
fof(e49, plain, (sP39 <=> (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f50, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP40), inference(usedef, [], [e50])).
fof(e50, plain, (sP40 <=> (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f51, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP41), inference(usedef, [], [e51])).
fof(e51, plain, (sP41 <=> (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f52, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP42), inference(usedef, [], [e52])).
fof(e52, plain, (sP42 <=> (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP42])])).
fof(f53, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP43), inference(usedef, [], [e53])).
fof(e53, plain, (sP43 <=> (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP43])])).
fof(f54, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP44), inference(usedef, [], [e54])).
fof(e54, plain, (sP44 <=> (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP44])])).
fof(f55, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP45), inference(usedef, [], [e55])).
fof(e55, plain, (sP45 <=> (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP45])])).
fof(f56, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP46), inference(usedef, [], [e56])).
fof(e56, plain, (sP46 <=> (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP46])])).
fof(f57, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP47), inference(usedef, [], [e57])).
fof(e57, plain, (sP47 <=> (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP47])])).
fof(f9, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & ((~ (e4 = op(e4, e4)) & (e4 = op(e4, e4)) & (e4 = op(e4, e4))) | (~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | (~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | (~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | (~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | (~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | (~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | (~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | (~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | (~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | (~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | (~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | (~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | (~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | (~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | (~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | (~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | (~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | (~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | (~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | (~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | (~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | (~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | (~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | (~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG060+1.p', co1)).
fof(f1617, plain, (spl48_207 | spl48_204 | spl48_201 | spl48_198 | spl48_195 | spl48_191 | spl48_189 | spl48_186 | spl48_183 | spl48_180 | spl48_176 | spl48_172 | spl48_170 | spl48_167 | spl48_164 | spl48_160 | spl48_156 | spl48_152 | spl48_150 | spl48_147 | spl48_143 | spl48_139 | spl48_135 | spl48_131), inference(avatar_split_clause, [], [f457, f1062, f1081, f1100, f1119, f1138, f1153, f1164, f1183, f1202, f1221, f1236, f1251, f1262, f1281, f1300, f1315, f1330, f1345, f1356, f1375, f1390, f1405, f1420, f1435])).
fof(f1435, plain, (spl48_207 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl48_207])])).
fof(f1420, plain, (spl48_204 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl48_204])])).
fof(f1405, plain, (spl48_201 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl48_201])])).
fof(f1390, plain, (spl48_198 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl48_198])])).
fof(f1375, plain, (spl48_195 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl48_195])])).
fof(f1356, plain, (spl48_191 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl48_191])])).
fof(f1345, plain, (spl48_189 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl48_189])])).
fof(f1330, plain, (spl48_186 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl48_186])])).
fof(f1315, plain, (spl48_183 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl48_183])])).
fof(f1300, plain, (spl48_180 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl48_180])])).
fof(f1281, plain, (spl48_176 <=> sP34), introduced(avatar_definition, [new_symbols(naming, [spl48_176])])).
fof(f1262, plain, (spl48_172 <=> sP35), introduced(avatar_definition, [new_symbols(naming, [spl48_172])])).
fof(f1251, plain, (spl48_170 <=> sP36), introduced(avatar_definition, [new_symbols(naming, [spl48_170])])).
fof(f1236, plain, (spl48_167 <=> sP37), introduced(avatar_definition, [new_symbols(naming, [spl48_167])])).
fof(f1221, plain, (spl48_164 <=> sP38), introduced(avatar_definition, [new_symbols(naming, [spl48_164])])).
fof(f1202, plain, (spl48_160 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl48_160])])).
fof(f1183, plain, (spl48_156 <=> sP40), introduced(avatar_definition, [new_symbols(naming, [spl48_156])])).
fof(f1164, plain, (spl48_152 <=> sP41), introduced(avatar_definition, [new_symbols(naming, [spl48_152])])).
fof(f1153, plain, (spl48_150 <=> sP42), introduced(avatar_definition, [new_symbols(naming, [spl48_150])])).
fof(f1138, plain, (spl48_147 <=> sP43), introduced(avatar_definition, [new_symbols(naming, [spl48_147])])).
fof(f1119, plain, (spl48_143 <=> sP44), introduced(avatar_definition, [new_symbols(naming, [spl48_143])])).
fof(f1100, plain, (spl48_139 <=> sP45), introduced(avatar_definition, [new_symbols(naming, [spl48_139])])).
fof(f1081, plain, (spl48_135 <=> sP46), introduced(avatar_definition, [new_symbols(naming, [spl48_135])])).
fof(f1062, plain, (spl48_131 <=> sP47), introduced(avatar_definition, [new_symbols(naming, [spl48_131])])).
fof(f457, plain, (sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24), inference(trivial_inequality_removal, [], [f454])).
fof(f454, plain, (~ (op(e4, e4) = op(e4, e4)) | sP47 | sP46 | sP45 | sP44 | sP43 | sP42 | sP41 | sP40 | sP39 | sP38 | sP37 | sP36 | sP35 | sP34 | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24), inference(cnf_transformation, [], [f58])).
fof(f1612, plain, (~ spl48_232 | spl48_121), inference(avatar_split_clause, [], [f448, f967, f1607])).
fof(f448, plain, ((e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f106])).
fof(f106, plain, ((~ (e0 = op(e0, e0)) & (e0 = op(e0, e0)) & (e0 = op(e0, e0))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f1610, plain, (~ spl48_232 | ~ spl48_121), inference(avatar_split_clause, [], [f450, f967, f1607])).
fof(f450, plain, (~ (e0 = op(e0, e0)) | ~ sP0), inference(cnf_transformation, [], [f106])).
fof(f1604, plain, (~ spl48_231 | spl48_91), inference(avatar_split_clause, [], [f446, f841, f1600])).
fof(f446, plain, ((e0 = op(e1, e1)) | ~ sP1), inference(cnf_transformation, [], [f105])).
fof(f105, plain, ((~ (e0 = op(e0, e1)) & (e0 = op(e1, e1)) & (op(e0, e0) = e1)) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f1598, plain, (~ spl48_230 | spl48_123), inference(avatar_split_clause, [], [f442, f975, f1593])).
fof(f442, plain, ((op(e0, e0) = e2) | ~ sP2), inference(cnf_transformation, [], [f104])).
fof(f104, plain, ((~ (e0 = op(e0, e2)) & (e0 = op(e2, e2)) & (op(e0, e0) = e2)) | ~ sP2), inference(nnf_transformation, [], [f12])).
fof(f1596, plain, (~ spl48_230 | ~ spl48_111), inference(avatar_split_clause, [], [f444, f925, f1593])).
fof(f444, plain, (~ (e0 = op(e0, e2)) | ~ sP2), inference(cnf_transformation, [], [f104])).
fof(f1591, plain, (~ spl48_229 | spl48_124), inference(avatar_split_clause, [], [f439, f979, f1586])).
fof(f439, plain, ((op(e0, e0) = e3) | ~ sP3), inference(cnf_transformation, [], [f103])).
fof(f103, plain, ((~ (e0 = op(e0, e3)) & (e0 = op(e3, e3)) & (op(e0, e0) = e3)) | ~ sP3), inference(nnf_transformation, [], [f13])).
fof(f1583, plain, (~ spl48_228 | spl48_1), inference(avatar_split_clause, [], [f437, f463, f1579])).
fof(f437, plain, ((e0 = op(e4, e4)) | ~ sP4), inference(cnf_transformation, [], [f102])).
fof(f102, plain, ((~ (e0 = op(e0, e4)) & (e0 = op(e4, e4)) & (op(e0, e0) = e4)) | ~ sP4), inference(nnf_transformation, [], [f14])).
fof(f1577, plain, (~ spl48_227 | spl48_91), inference(avatar_split_clause, [], [f433, f841, f1572])).
fof(f433, plain, ((e0 = op(e1, e1)) | ~ sP5), inference(cnf_transformation, [], [f101])).
fof(f101, plain, ((~ (e1 = op(e1, e0)) & (op(e0, e0) = e1) & (e0 = op(e1, e1))) | ~ sP5), inference(nnf_transformation, [], [f15])).
fof(f1570, plain, (~ spl48_226 | spl48_92), inference(avatar_split_clause, [], [f430, f845, f1565])).
fof(f430, plain, ((e1 = op(e1, e1)) | ~ sP6), inference(cnf_transformation, [], [f100])).
fof(f100, plain, ((~ (e1 = op(e1, e1)) & (e1 = op(e1, e1)) & (e1 = op(e1, e1))) | ~ sP6), inference(nnf_transformation, [], [f16])).
fof(f1562, plain, (~ spl48_225 | spl48_62), inference(avatar_split_clause, [], [f428, f719, f1558])).
fof(f428, plain, ((e1 = op(e2, e2)) | ~ sP7), inference(cnf_transformation, [], [f99])).
fof(f99, plain, ((~ (e1 = op(e1, e2)) & (e1 = op(e2, e2)) & (e2 = op(e1, e1))) | ~ sP7), inference(nnf_transformation, [], [f17])).
fof(f1556, plain, (~ spl48_224 | spl48_94), inference(avatar_split_clause, [], [f424, f853, f1551])).
fof(f424, plain, ((e3 = op(e1, e1)) | ~ sP8), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ((~ (e1 = op(e1, e3)) & (e1 = op(e3, e3)) & (e3 = op(e1, e1))) | ~ sP8), inference(nnf_transformation, [], [f18])).
fof(f1549, plain, (~ spl48_223 | spl48_95), inference(avatar_split_clause, [], [f421, f857, f1544])).
fof(f421, plain, ((e4 = op(e1, e1)) | ~ sP9), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ (e1 = op(e1, e4)) & (e1 = op(e4, e4)) & (e4 = op(e1, e1))) | ~ sP9), inference(nnf_transformation, [], [f19])).
fof(f1542, plain, (~ spl48_222 | spl48_61), inference(avatar_split_clause, [], [f418, f715, f1537])).
fof(f418, plain, ((e0 = op(e2, e2)) | ~ sP10), inference(cnf_transformation, [], [f96])).
fof(f96, plain, ((~ (e2 = op(e2, e0)) & (op(e0, e0) = e2) & (e0 = op(e2, e2))) | ~ sP10), inference(nnf_transformation, [], [f20])).
fof(f1540, plain, (~ spl48_222 | ~ spl48_73), inference(avatar_split_clause, [], [f420, f765, f1537])).
fof(f420, plain, (~ (e2 = op(e2, e0)) | ~ sP10), inference(cnf_transformation, [], [f96])).
fof(f1535, plain, (~ spl48_221 | spl48_62), inference(avatar_split_clause, [], [f415, f719, f1530])).
fof(f415, plain, ((e1 = op(e2, e2)) | ~ sP11), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((~ (e2 = op(e2, e1)) & (e2 = op(e1, e1)) & (e1 = op(e2, e2))) | ~ sP11), inference(nnf_transformation, [], [f21])).
fof(f1528, plain, (~ spl48_220 | spl48_63), inference(avatar_split_clause, [], [f412, f723, f1523])).
fof(f412, plain, ((e2 = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ((~ (e2 = op(e2, e2)) & (e2 = op(e2, e2)) & (e2 = op(e2, e2))) | ~ sP12), inference(nnf_transformation, [], [f22])).
fof(f1526, plain, (~ spl48_220 | ~ spl48_63), inference(avatar_split_clause, [], [f414, f723, f1523])).
fof(f414, plain, (~ (e2 = op(e2, e2)) | ~ sP12), inference(cnf_transformation, [], [f94])).
fof(f1520, plain, (~ spl48_219 | spl48_33), inference(avatar_split_clause, [], [f410, f597, f1516])).
fof(f410, plain, ((e2 = op(e3, e3)) | ~ sP13), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ((~ (e2 = op(e2, e3)) & (e2 = op(e3, e3)) & (e3 = op(e2, e2))) | ~ sP13), inference(nnf_transformation, [], [f23])).
fof(f1513, plain, (~ spl48_218 | spl48_3), inference(avatar_split_clause, [], [f407, f471, f1509])).
fof(f407, plain, ((e2 = op(e4, e4)) | ~ sP14), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ((~ (e2 = op(e2, e4)) & (e2 = op(e4, e4)) & (e4 = op(e2, e2))) | ~ sP14), inference(nnf_transformation, [], [f24])).
fof(f1506, plain, (~ spl48_217 | spl48_124), inference(avatar_split_clause, [], [f404, f979, f1502])).
fof(f404, plain, ((op(e0, e0) = e3) | ~ sP15), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ((~ (e3 = op(e3, e0)) & (op(e0, e0) = e3) & (e0 = op(e3, e3))) | ~ sP15), inference(nnf_transformation, [], [f25])).
fof(f1505, plain, (~ spl48_217 | ~ spl48_49), inference(avatar_split_clause, [], [f405, f664, f1502])).
fof(f405, plain, (~ (e3 = op(e3, e0)) | ~ sP15), inference(cnf_transformation, [], [f91])).
fof(f1499, plain, (~ spl48_216 | spl48_94), inference(avatar_split_clause, [], [f401, f853, f1495])).
fof(f401, plain, ((e3 = op(e1, e1)) | ~ sP16), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ((~ (e3 = op(e3, e1)) & (e3 = op(e1, e1)) & (e1 = op(e3, e3))) | ~ sP16), inference(nnf_transformation, [], [f26])).
fof(f1493, plain, (~ spl48_215 | spl48_33), inference(avatar_split_clause, [], [f397, f597, f1488])).
fof(f397, plain, ((e2 = op(e3, e3)) | ~ sP17), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ((~ (e3 = op(e3, e2)) & (e3 = op(e2, e2)) & (e2 = op(e3, e3))) | ~ sP17), inference(nnf_transformation, [], [f27])).
fof(f1486, plain, (~ spl48_214 | spl48_34), inference(avatar_split_clause, [], [f394, f601, f1481])).
fof(f394, plain, ((e3 = op(e3, e3)) | ~ sP18), inference(cnf_transformation, [], [f88])).
fof(f88, plain, ((~ (e3 = op(e3, e3)) & (e3 = op(e3, e3)) & (e3 = op(e3, e3))) | ~ sP18), inference(nnf_transformation, [], [f28])).
fof(f1478, plain, (~ spl48_213 | spl48_4), inference(avatar_split_clause, [], [f392, f475, f1474])).
fof(f392, plain, ((e3 = op(e4, e4)) | ~ sP19), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ((~ (e3 = op(e3, e4)) & (e3 = op(e4, e4)) & (e4 = op(e3, e3))) | ~ sP19), inference(nnf_transformation, [], [f29])).
fof(f1472, plain, (~ spl48_212 | spl48_1), inference(avatar_split_clause, [], [f388, f463, f1467])).
fof(f388, plain, ((e0 = op(e4, e4)) | ~ sP20), inference(cnf_transformation, [], [f86])).
fof(f86, plain, ((~ (e4 = op(e4, e0)) & (op(e0, e0) = e4) & (e0 = op(e4, e4))) | ~ sP20), inference(nnf_transformation, [], [f30])).
fof(f1464, plain, (~ spl48_211 | spl48_95), inference(avatar_split_clause, [], [f386, f857, f1460])).
fof(f386, plain, ((e4 = op(e1, e1)) | ~ sP21), inference(cnf_transformation, [], [f85])).
fof(f85, plain, ((~ (e4 = op(e4, e1)) & (e4 = op(e1, e1)) & (e1 = op(e4, e4))) | ~ sP21), inference(nnf_transformation, [], [f31])).
fof(f1458, plain, (~ spl48_210 | spl48_3), inference(avatar_split_clause, [], [f382, f471, f1453])).
fof(f382, plain, ((e2 = op(e4, e4)) | ~ sP22), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e4 = op(e4, e2)) & (e4 = op(e2, e2)) & (e2 = op(e4, e4))) | ~ sP22), inference(nnf_transformation, [], [f32])).
fof(f1451, plain, (~ spl48_209 | spl48_4), inference(avatar_split_clause, [], [f379, f475, f1446])).
fof(f379, plain, ((e3 = op(e4, e4)) | ~ sP23), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e4 = op(e4, e3)) & (e4 = op(e3, e3)) & (e3 = op(e4, e4))) | ~ sP23), inference(nnf_transformation, [], [f33])).
fof(f1444, plain, ~ spl48_207, inference(avatar_split_clause, [], [f458, f1435])).
fof(f458, plain, ~ sP24, inference(trivial_inequality_removal, [], [f376])).
fof(f376, plain, (~ (op(e0, e0) = op(e0, e0)) | ~ sP24), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP24), inference(nnf_transformation, [], [f34])).
fof(f1432, plain, (~ spl48_204 | spl48_206), inference(avatar_split_clause, [], [f374, f1429, f1420])).
fof(f374, plain, ((e0 = op(op(e0, e1), e1)) | ~ sP25), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP25), inference(nnf_transformation, [], [f35])).
fof(f1427, plain, (~ spl48_204 | ~ spl48_205), inference(avatar_split_clause, [], [f375, f1424, f1420])).
fof(f375, plain, (~ (e1 = op(op(e0, e1), e0)) | ~ sP25), inference(cnf_transformation, [], [f81])).
fof(f1418, plain, (~ spl48_201 | ~ spl48_179), inference(avatar_split_clause, [], [f370, f1295, f1405])).
fof(f370, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP26), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP26), inference(nnf_transformation, [], [f36])).
fof(f1403, plain, (~ spl48_198 | ~ spl48_163), inference(avatar_split_clause, [], [f367, f1216, f1390])).
fof(f367, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP27), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP27), inference(nnf_transformation, [], [f37])).
fof(f1388, plain, (~ spl48_195 | ~ spl48_146), inference(avatar_split_clause, [], [f364, f1133, f1375])).
fof(f364, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP28), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP28), inference(nnf_transformation, [], [f38])).
fof(f1387, plain, (~ spl48_195 | spl48_197), inference(avatar_split_clause, [], [f365, f1384, f1375])).
fof(f365, plain, ((e0 = op(op(e0, e4), e4)) | ~ sP28), inference(cnf_transformation, [], [f78])).
fof(f1373, plain, (~ spl48_191 | ~ spl48_194), inference(avatar_split_clause, [], [f361, f1370, f1356])).
fof(f361, plain, (~ (op(e0, e1) = op(e1, e0)) | ~ sP29), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP29), inference(nnf_transformation, [], [f39])).
fof(f1363, plain, (~ spl48_191 | ~ spl48_192), inference(avatar_split_clause, [], [f363, f1360, f1356])).
fof(f363, plain, (~ (e0 = op(op(e1, e0), e1)) | ~ sP29), inference(cnf_transformation, [], [f77])).
fof(f1354, plain, ~ spl48_189, inference(avatar_split_clause, [], [f459, f1345])).
fof(f459, plain, ~ sP30, inference(trivial_inequality_removal, [], [f358])).
fof(f358, plain, (~ (op(e1, e1) = op(e1, e1)) | ~ sP30), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP30), inference(nnf_transformation, [], [f40])).
fof(f1343, plain, (~ spl48_186 | ~ spl48_175), inference(avatar_split_clause, [], [f355, f1276, f1330])).
fof(f355, plain, (~ (op(e1, e2) = op(e2, e1)) | ~ sP31), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP31), inference(nnf_transformation, [], [f41])).
fof(f1342, plain, (~ spl48_186 | spl48_188), inference(avatar_split_clause, [], [f356, f1339, f1330])).
fof(f356, plain, ((e1 = op(op(e1, e2), e2)) | ~ sP31), inference(cnf_transformation, [], [f75])).
fof(f1327, plain, (~ spl48_183 | spl48_185), inference(avatar_split_clause, [], [f353, f1324, f1315])).
fof(f353, plain, ((e1 = op(op(e1, e3), e3)) | ~ sP32), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP32), inference(nnf_transformation, [], [f42])).
fof(f1322, plain, (~ spl48_183 | ~ spl48_184), inference(avatar_split_clause, [], [f354, f1319, f1315])).
fof(f354, plain, (~ (e3 = op(op(e1, e3), e1)) | ~ sP32), inference(cnf_transformation, [], [f74])).
fof(f1312, plain, (~ spl48_180 | spl48_182), inference(avatar_split_clause, [], [f350, f1309, f1300])).
fof(f350, plain, ((e1 = op(op(e1, e4), e4)) | ~ sP33), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP33), inference(nnf_transformation, [], [f43])).
fof(f1298, plain, (~ spl48_176 | ~ spl48_179), inference(avatar_split_clause, [], [f346, f1295, f1281])).
fof(f346, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP34), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP34), inference(nnf_transformation, [], [f44])).
fof(f1288, plain, (~ spl48_176 | ~ spl48_177), inference(avatar_split_clause, [], [f348, f1285, f1281])).
fof(f348, plain, (~ (e0 = op(op(e2, e0), e2)) | ~ sP34), inference(cnf_transformation, [], [f72])).
fof(f1279, plain, (~ spl48_172 | ~ spl48_175), inference(avatar_split_clause, [], [f343, f1276, f1262])).
fof(f343, plain, (~ (op(e1, e2) = op(e2, e1)) | ~ sP35), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP35), inference(nnf_transformation, [], [f45])).
fof(f1274, plain, (~ spl48_172 | spl48_174), inference(avatar_split_clause, [], [f344, f1271, f1262])).
fof(f344, plain, ((e2 = op(op(e2, e1), e1)) | ~ sP35), inference(cnf_transformation, [], [f71])).
fof(f1260, plain, ~ spl48_170, inference(avatar_split_clause, [], [f460, f1251])).
fof(f460, plain, ~ sP36, inference(trivial_inequality_removal, [], [f340])).
fof(f340, plain, (~ (op(e2, e2) = op(e2, e2)) | ~ sP36), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP36), inference(nnf_transformation, [], [f46])).
fof(f1249, plain, (~ spl48_167 | ~ spl48_155), inference(avatar_split_clause, [], [f337, f1178, f1236])).
fof(f337, plain, (~ (op(e2, e3) = op(e3, e2)) | ~ sP37), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP37), inference(nnf_transformation, [], [f47])).
fof(f1248, plain, (~ spl48_167 | spl48_169), inference(avatar_split_clause, [], [f338, f1245, f1236])).
fof(f338, plain, ((e2 = op(op(e2, e3), e3)) | ~ sP37), inference(cnf_transformation, [], [f69])).
fof(f1234, plain, (~ spl48_164 | ~ spl48_138), inference(avatar_split_clause, [], [f334, f1095, f1221])).
fof(f334, plain, (~ (op(e2, e4) = op(e4, e2)) | ~ sP38), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP38), inference(nnf_transformation, [], [f48])).
fof(f1228, plain, (~ spl48_164 | ~ spl48_165), inference(avatar_split_clause, [], [f336, f1225, f1221])).
fof(f336, plain, (~ (e4 = op(op(e2, e4), e2)) | ~ sP38), inference(cnf_transformation, [], [f68])).
fof(f1219, plain, (~ spl48_160 | ~ spl48_163), inference(avatar_split_clause, [], [f331, f1216, f1202])).
fof(f331, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP39), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP39), inference(nnf_transformation, [], [f49])).
fof(f1195, plain, (~ spl48_156 | spl48_158), inference(avatar_split_clause, [], [f329, f1192, f1183])).
fof(f329, plain, ((e3 = op(op(e3, e1), e1)) | ~ sP40), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP40), inference(nnf_transformation, [], [f50])).
fof(f1181, plain, (~ spl48_152 | ~ spl48_155), inference(avatar_split_clause, [], [f325, f1178, f1164])).
fof(f325, plain, (~ (op(e2, e3) = op(e3, e2)) | ~ sP41), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP41), inference(nnf_transformation, [], [f51])).
fof(f1176, plain, (~ spl48_152 | spl48_154), inference(avatar_split_clause, [], [f326, f1173, f1164])).
fof(f326, plain, ((e3 = op(op(e3, e2), e2)) | ~ sP41), inference(cnf_transformation, [], [f65])).
fof(f1162, plain, ~ spl48_150, inference(avatar_split_clause, [], [f461, f1153])).
fof(f461, plain, ~ sP42, inference(trivial_inequality_removal, [], [f322])).
fof(f322, plain, (~ (op(e3, e3) = op(e3, e3)) | ~ sP42), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP42), inference(nnf_transformation, [], [f52])).
fof(f1151, plain, (~ spl48_147 | ~ spl48_134), inference(avatar_split_clause, [], [f319, f1076, f1138])).
fof(f319, plain, (~ (op(e3, e4) = op(e4, e3)) | ~ sP43), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP43), inference(nnf_transformation, [], [f53])).
fof(f1150, plain, (~ spl48_147 | spl48_149), inference(avatar_split_clause, [], [f320, f1147, f1138])).
fof(f320, plain, ((e3 = op(op(e3, e4), e4)) | ~ sP43), inference(cnf_transformation, [], [f63])).
fof(f1136, plain, (~ spl48_143 | ~ spl48_146), inference(avatar_split_clause, [], [f316, f1133, f1119])).
fof(f316, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP44), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP44), inference(nnf_transformation, [], [f54])).
fof(f1131, plain, (~ spl48_143 | spl48_145), inference(avatar_split_clause, [], [f317, f1128, f1119])).
fof(f317, plain, ((e4 = op(op(e4, e0), e0)) | ~ sP44), inference(cnf_transformation, [], [f62])).
fof(f1112, plain, (~ spl48_139 | spl48_141), inference(avatar_split_clause, [], [f314, f1109, f1100])).
fof(f314, plain, ((e4 = op(op(e4, e1), e1)) | ~ sP45), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP45), inference(nnf_transformation, [], [f55])).
fof(f1098, plain, (~ spl48_135 | ~ spl48_138), inference(avatar_split_clause, [], [f310, f1095, f1081])).
fof(f310, plain, (~ (op(e2, e4) = op(e4, e2)) | ~ sP46), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP46), inference(nnf_transformation, [], [f56])).
fof(f1093, plain, (~ spl48_135 | spl48_137), inference(avatar_split_clause, [], [f311, f1090, f1081])).
fof(f311, plain, ((e4 = op(op(e4, e2), e2)) | ~ sP46), inference(cnf_transformation, [], [f60])).
fof(f1079, plain, (~ spl48_131 | ~ spl48_134), inference(avatar_split_clause, [], [f307, f1076, f1062])).
fof(f307, plain, (~ (op(e3, e4) = op(e4, e3)) | ~ sP47), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP47), inference(nnf_transformation, [], [f57])).
fof(f1069, plain, (~ spl48_131 | ~ spl48_132), inference(avatar_split_clause, [], [f309, f1066, f1062])).
fof(f309, plain, (~ (e3 = op(op(e4, e3), e4)) | ~ sP47), inference(cnf_transformation, [], [f59])).
fof(f1060, plain, spl48_2, inference(avatar_split_clause, [], [f304, f467])).
fof(f304, plain, (e1 = op(e4, e4)), inference(cnf_transformation, [], [f6])).
fof(f1059, plain, spl48_28, inference(avatar_split_clause, [], [f1058, f576])).
fof(f1058, plain, (e2 = op(e3, e4)), inference(forward_demodulation, [], [f305, f306])).
fof(f305, plain, (e2 = op(op(e4, op(e4, e4)), e4)), inference(cnf_transformation, [], [f6])).
fof(f1050, plain, (spl48_124 | spl48_99 | spl48_74 | spl48_49 | spl48_24), inference(avatar_split_clause, [], [f150, f559, f664, f769, f874, f979])).
fof(f150, plain, ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG060+1.p', ax3)).
fof(f1046, plain, (spl48_116 | spl48_91 | spl48_66 | spl48_41 | spl48_16), inference(avatar_split_clause, [], [f154, f526, f631, f736, f841, f946])).
fof(f154, plain, ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f3])).
fof(f1042, plain, (spl48_118 | spl48_93 | spl48_68 | spl48_43 | spl48_18), inference(avatar_split_clause, [], [f158, f534, f639, f744, f849, f954])).
fof(f158, plain, ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))), inference(cnf_transformation, [], [f3])).
fof(f1041, plain, (spl48_99 | spl48_94 | spl48_89 | spl48_84 | spl48_79), inference(avatar_split_clause, [], [f159, f790, f811, f832, f853, f874])).
fof(f159, plain, ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))), inference(cnf_transformation, [], [f3])).
fof(f1019, plain, (spl48_50 | spl48_45 | spl48_40 | spl48_35 | spl48_30), inference(avatar_split_clause, [], [f181, f584, f605, f626, f647, f668])).
fof(f181, plain, ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f1018, plain, (spl48_110 | spl48_85 | spl48_60 | spl48_35 | spl48_10), inference(avatar_split_clause, [], [f182, f500, f605, f710, f815, f920])).
fof(f182, plain, ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))), inference(cnf_transformation, [], [f3])).
fof(f1017, plain, (spl48_21 | spl48_16 | spl48_11 | spl48_6 | spl48_1), inference(avatar_split_clause, [], [f183, f463, f484, f505, f526, f547])).
fof(f183, plain, ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f1013, plain, (spl48_23 | spl48_18 | spl48_13 | spl48_8 | spl48_3), inference(avatar_split_clause, [], [f187, f471, f492, f513, f534, f555])).
fof(f187, plain, ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f1010, plain, (spl48_104 | spl48_79 | spl48_54 | spl48_29 | spl48_4), inference(avatar_split_clause, [], [f190, f475, f580, f685, f790, f895])).
fof(f190, plain, ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f1007, plain, (spl48_126 | spl48_127 | spl48_128 | spl48_129 | spl48_130), inference(avatar_split_clause, [], [f142, f1004, f1000, f996, f992, f988])).
fof(f142, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f965, plain, (spl48_116 | spl48_117 | spl48_118 | spl48_119 | spl48_120), inference(avatar_split_clause, [], [f108, f962, f958, f954, f950, f946])).
fof(f108, plain, ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG060+1.p', ax1)).
fof(f923, plain, (spl48_106 | spl48_107 | spl48_108 | spl48_109 | spl48_110), inference(avatar_split_clause, [], [f110, f920, f916, f912, f908, f904])).
fof(f110, plain, ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f1])).
fof(f818, plain, (spl48_81 | spl48_82 | spl48_83 | spl48_84 | spl48_85), inference(avatar_split_clause, [], [f115, f815, f811, f807, f803, f799])).
fof(f115, plain, ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))), inference(cnf_transformation, [], [f1])).
fof(f755, plain, (spl48_66 | spl48_67 | spl48_68 | spl48_69 | spl48_70), inference(avatar_split_clause, [], [f118, f752, f748, f744, f740, f736])).
fof(f118, plain, ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f713, plain, (spl48_56 | spl48_57 | spl48_58 | spl48_59 | spl48_60), inference(avatar_split_clause, [], [f120, f710, f706, f702, f698, f694])).
fof(f120, plain, ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))), inference(cnf_transformation, [], [f1])).
fof(f671, plain, (spl48_46 | spl48_47 | spl48_48 | spl48_49 | spl48_50), inference(avatar_split_clause, [], [f122, f668, f664, f660, f656, f652])).
fof(f122, plain, ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))), inference(cnf_transformation, [], [f1])).