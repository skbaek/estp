fof(f3852, plain, $false, inference(avatar_sat_refutation, [], [f586, f712, f733, f754, f796, f901, f964, f968, f969, f970, f980, f981, f983, f991, f993, f994, f1005, f1009, f1015, f1025, f1044, f1063, f1092, f1106, f1118, f1132, f1146, f1175, f1189, f1199, f1216, f1230, f1254, f1268, f1283, f1293, f1310, f1329, f1344, f1359, f1374, f1389, f1400, f1414, f1423, f1522, f1531, f1532, f1539, f1552, f1564, f1682, f1726, f1738, f1741, f1794, f1798, f1805, f1826, f1854, f1893, f1897, f1898, f1899, f1909, f1912, f1943, f1961, f1976, f1979, f1989, f2000, f2008, f2020, f2037, f2043, f2051, f2061, f2080, f2099, f2103, f2116, f2139, f2140, f2162, f2163, f2235, f2237, f2240, f2247, f2248, f2253, f2324, f2328, f2348, f2369, f2391, f2394, f2470, f2509, f2612, f2629, f2640, f2660, f2670, f2718, f2817, f2864, f2865, f2868, f2869, f2870, f2873, f2894, f2932, f3026, f3055, f3090, f3137, f3144, f3164, f3172, f3187, f3205, f3213, f3242, f3265, f3290, f3357, f3375, f3376, f3377, f3378, f3379, f3380, f3381, f3382, f3383, f3410, f3427, f3443, f3568, f3607, f3662, f3667, f3689, f3703, f3707, f3717, f3726, f3737, f3744, f3748, f3757, f3765, f3777, f3781, f3783, f3796, f3822])).
fof(f3822, plain, (~ spl34_8 | ~ spl34_33), inference(avatar_split_clause, [], [f3818, f554, f449])).
fof(f449, plain, (spl34_8 <=> (e2 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_8])])).
fof(f554, plain, (spl34_33 <=> (e2 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_33])])).
fof(f3818, plain, (~ (e2 = op(e4, e3)) | ~ spl34_33), inference(backward_demodulation, [], [f204, f556])).
fof(f556, plain, ((e2 = op(e3, e3)) | ~ spl34_33), inference(avatar_component_clause, [], [f554])).
fof(f204, plain, ~ (op(e3, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (~ (op(e4, e3) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e4)) & ~ (op(e4, e2) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e4)) & ~ (op(e4, e1) = op(e4, e3)) & ~ (op(e4, e1) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e4)) & ~ (op(e4, e0) = op(e4, e3)) & ~ (op(e4, e0) = op(e4, e2)) & ~ (op(e4, e0) = op(e4, e1)) & ~ (op(e3, e3) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e4)) & ~ (op(e3, e2) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e4)) & ~ (op(e3, e1) = op(e3, e3)) & ~ (op(e3, e1) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e4)) & ~ (op(e3, e0) = op(e3, e3)) & ~ (op(e3, e0) = op(e3, e2)) & ~ (op(e3, e0) = op(e3, e1)) & ~ (op(e2, e3) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e4)) & ~ (op(e2, e2) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e4)) & ~ (op(e2, e1) = op(e2, e3)) & ~ (op(e2, e1) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e4)) & ~ (op(e2, e0) = op(e2, e3)) & ~ (op(e2, e0) = op(e2, e2)) & ~ (op(e2, e0) = op(e2, e1)) & ~ (op(e1, e3) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e4)) & ~ (op(e1, e2) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e4)) & ~ (op(e1, e1) = op(e1, e3)) & ~ (op(e1, e1) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e4)) & ~ (op(e1, e0) = op(e1, e3)) & ~ (op(e1, e0) = op(e1, e2)) & ~ (op(e1, e0) = op(e1, e1)) & ~ (op(e0, e3) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e4)) & ~ (op(e0, e2) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e4)) & ~ (op(e0, e1) = op(e0, e3)) & ~ (op(e0, e1) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e4)) & ~ (op(e0, e0) = op(e0, e3)) & ~ (op(e0, e0) = op(e0, e2)) & ~ (op(e0, e0) = op(e0, e1)) & ~ (op(e3, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e4, e4)) & ~ (op(e2, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e4, e4)) & ~ (op(e1, e4) = op(e3, e4)) & ~ (op(e1, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e4, e4)) & ~ (op(e0, e4) = op(e3, e4)) & ~ (op(e0, e4) = op(e2, e4)) & ~ (op(e0, e4) = op(e1, e4)) & ~ (op(e3, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e4, e3)) & ~ (op(e2, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e4, e3)) & ~ (op(e1, e3) = op(e3, e3)) & ~ (op(e1, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e4, e3)) & ~ (op(e0, e3) = op(e3, e3)) & ~ (op(e0, e3) = op(e2, e3)) & ~ (op(e0, e3) = op(e1, e3)) & ~ (op(e3, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e4, e2)) & ~ (op(e2, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e4, e2)) & ~ (op(e1, e2) = op(e3, e2)) & ~ (op(e1, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e4, e2)) & ~ (op(e0, e2) = op(e3, e2)) & ~ (op(e0, e2) = op(e2, e2)) & ~ (op(e0, e2) = op(e1, e2)) & ~ (op(e3, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e4, e1)) & ~ (op(e2, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e4, e1)) & ~ (op(e1, e1) = op(e3, e1)) & ~ (op(e1, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e4, e1)) & ~ (op(e0, e1) = op(e3, e1)) & ~ (op(e0, e1) = op(e2, e1)) & ~ (op(e0, e1) = op(e1, e1)) & ~ (op(e3, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e4, e0)) & ~ (op(e2, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e4, e0)) & ~ (op(e1, e0) = op(e3, e0)) & ~ (op(e1, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e4, e0)) & ~ (op(e0, e0) = op(e3, e0)) & ~ (op(e0, e0) = op(e2, e0)) & ~ (op(e0, e0) = op(e1, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG057+1.p', ax4)).
fof(f3796, plain, (~ spl34_8 | ~ spl34_54 | spl34_238), inference(avatar_split_clause, [], [f3795, f1541, f642, f449])).
fof(f642, plain, (spl34_54 <=> (e3 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_54])])).
fof(f1541, plain, (spl34_238 <=> (e2 = op(e4, op(e2, e4)))), introduced(avatar_definition, [new_symbols(naming, [spl34_238])])).
fof(f3795, plain, (~ (e2 = op(e4, e3)) | (~ spl34_54 | spl34_238)), inference(forward_demodulation, [], [f1542, f644])).
fof(f644, plain, ((e3 = op(e2, e4)) | ~ spl34_54), inference(avatar_component_clause, [], [f642])).
fof(f1542, plain, (~ (e2 = op(e4, op(e2, e4))) | spl34_238), inference(avatar_component_clause, [], [f1541])).
fof(f3783, plain, (~ spl34_43 | ~ spl34_89 | spl34_187), inference(avatar_split_clause, [], [f3772, f1290, f789, f596])).
fof(f596, plain, (spl34_43 <=> (e2 = op(e3, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_43])])).
fof(f789, plain, (spl34_89 <=> (e3 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_89])])).
fof(f1290, plain, (spl34_187 <=> (e2 = op(op(e1, e2), e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_187])])).
fof(f3772, plain, (~ (e2 = op(e3, e1)) | (~ spl34_89 | spl34_187)), inference(forward_demodulation, [], [f1292, f791])).
fof(f791, plain, ((e3 = op(e1, e2)) | ~ spl34_89), inference(avatar_component_clause, [], [f789])).
fof(f1292, plain, (~ (e2 = op(op(e1, e2), e1)) | spl34_187), inference(avatar_component_clause, [], [f1290])).
fof(f3781, plain, (~ spl34_20 | ~ spl34_25), inference(avatar_split_clause, [], [f3780, f520, f499])).
fof(f499, plain, (spl34_20 <=> (e4 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_20])])).
fof(f520, plain, (spl34_25 <=> (e4 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_25])])).
fof(f3780, plain, (~ (e4 = op(e4, e1)) | ~ spl34_25), inference(forward_demodulation, [], [f255, f522])).
fof(f522, plain, ((e4 = op(e4, e0)) | ~ spl34_25), inference(avatar_component_clause, [], [f520])).
fof(f255, plain, ~ (op(e4, e0) = op(e4, e1)), inference(cnf_transformation, [], [f4])).
fof(f3777, plain, (~ spl34_9 | ~ spl34_109), inference(avatar_split_clause, [], [f3436, f873, f453])).
fof(f453, plain, (spl34_9 <=> (e3 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_9])])).
fof(f873, plain, (spl34_109 <=> (e3 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_109])])).
fof(f3436, plain, (~ (e3 = op(e4, e3)) | ~ spl34_109), inference(backward_demodulation, [], [f198, f875])).
fof(f875, plain, ((e3 = op(e0, e3)) | ~ spl34_109), inference(avatar_component_clause, [], [f873])).
fof(f198, plain, ~ (op(e0, e3) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f3765, plain, (~ spl34_27 | ~ spl34_78 | ~ spl34_149), inference(avatar_contradiction_clause, [], [f3764])).
fof(f3764, plain, ($false | (~ spl34_27 | ~ spl34_78 | ~ spl34_149)), inference(subsumption_resolution, [], [f3763, f272])).
fof(f272, plain, ~ (e2 = e3), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (e3 = e4) & ~ (e2 = e4) & ~ (e2 = e3) & ~ (e1 = e4) & ~ (e1 = e3) & ~ (e1 = e2) & ~ (e0 = e4) & ~ (e0 = e3) & ~ (e0 = e2) & ~ (e0 = e1)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG057+1.p', ax5)).
fof(f3763, plain, ((e2 = e3) | (~ spl34_27 | ~ spl34_78 | ~ spl34_149)), inference(forward_demodulation, [], [f3762, f745])).
fof(f745, plain, ((e2 = op(e1, e4)) | ~ spl34_78), inference(avatar_component_clause, [], [f743])).
fof(f743, plain, (spl34_78 <=> (e2 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_78])])).
fof(f3762, plain, ((e3 = op(e1, e4)) | (~ spl34_27 | ~ spl34_149)), inference(forward_demodulation, [], [f1105, f531])).
fof(f531, plain, ((e1 = op(e3, e4)) | ~ spl34_27), inference(avatar_component_clause, [], [f529])).
fof(f529, plain, (spl34_27 <=> (e1 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_27])])).
fof(f1105, plain, ((e3 = op(op(e3, e4), e4)) | ~ spl34_149), inference(avatar_component_clause, [], [f1103])).
fof(f1103, plain, (spl34_149 <=> (e3 = op(op(e3, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_149])])).
fof(f3757, plain, (~ spl34_27 | ~ spl34_54 | ~ spl34_166), inference(avatar_contradiction_clause, [], [f3756])).
fof(f3756, plain, ($false | (~ spl34_27 | ~ spl34_54 | ~ spl34_166)), inference(subsumption_resolution, [], [f3755, f269])).
fof(f269, plain, ~ (e1 = e2), inference(cnf_transformation, [], [f5])).
fof(f3755, plain, ((e1 = e2) | (~ spl34_27 | ~ spl34_54 | ~ spl34_166)), inference(forward_demodulation, [], [f3754, f531])).
fof(f3754, plain, ((e2 = op(e3, e4)) | (~ spl34_54 | ~ spl34_166)), inference(forward_demodulation, [], [f1188, f644])).
fof(f1188, plain, ((e2 = op(op(e2, e4), e4)) | ~ spl34_166), inference(avatar_component_clause, [], [f1186])).
fof(f1186, plain, (spl34_166 <=> (e2 = op(op(e2, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_166])])).
fof(f3748, plain, (spl34_52 | ~ spl34_78 | ~ spl34_182), inference(avatar_contradiction_clause, [], [f3747])).
fof(f3747, plain, ($false | (spl34_52 | ~ spl34_78 | ~ spl34_182)), inference(subsumption_resolution, [], [f3746, f635])).
fof(f635, plain, (~ (e1 = op(e2, e4)) | spl34_52), inference(avatar_component_clause, [], [f634])).
fof(f634, plain, (spl34_52 <=> (e1 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_52])])).
fof(f3746, plain, ((e1 = op(e2, e4)) | (~ spl34_78 | ~ spl34_182)), inference(forward_demodulation, [], [f1267, f745])).
fof(f1267, plain, ((e1 = op(op(e1, e4), e4)) | ~ spl34_182), inference(avatar_component_clause, [], [f1265])).
fof(f1265, plain, (spl34_182 <=> (e1 = op(op(e1, e4), e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_182])])).
fof(f3744, plain, (~ spl34_36 | ~ spl34_113 | ~ spl34_154), inference(avatar_contradiction_clause, [], [f3743])).
fof(f3743, plain, ($false | (~ spl34_36 | ~ spl34_113 | ~ spl34_154)), inference(subsumption_resolution, [], [f3742, f272])).
fof(f3742, plain, ((e2 = e3) | (~ spl34_36 | ~ spl34_113 | ~ spl34_154)), inference(forward_demodulation, [], [f3741, f892])).
fof(f892, plain, ((e2 = op(e0, e2)) | ~ spl34_113), inference(avatar_component_clause, [], [f890])).
fof(f890, plain, (spl34_113 <=> (e2 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_113])])).
fof(f3741, plain, ((e3 = op(e0, e2)) | (~ spl34_36 | ~ spl34_154)), inference(forward_demodulation, [], [f1131, f569])).
fof(f569, plain, ((e0 = op(e3, e2)) | ~ spl34_36), inference(avatar_component_clause, [], [f567])).
fof(f567, plain, (spl34_36 <=> (e0 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_36])])).
fof(f1131, plain, ((e3 = op(op(e3, e2), e2)) | ~ spl34_154), inference(avatar_component_clause, [], [f1129])).
fof(f1129, plain, (spl34_154 <=> (e3 = op(op(e3, e2), e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_154])])).
fof(f3737, plain, (~ spl34_43 | ~ spl34_57 | spl34_157), inference(avatar_contradiction_clause, [], [f3736])).
fof(f3736, plain, ($false | (~ spl34_43 | ~ spl34_57 | spl34_157)), inference(subsumption_resolution, [], [f3735, f657])).
fof(f657, plain, ((e1 = op(e2, e3)) | ~ spl34_57), inference(avatar_component_clause, [], [f655])).
fof(f655, plain, (spl34_57 <=> (e1 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_57])])).
fof(f3735, plain, (~ (e1 = op(e2, e3)) | (~ spl34_43 | spl34_157)), inference(forward_demodulation, [], [f1145, f598])).
fof(f598, plain, ((e2 = op(e3, e1)) | ~ spl34_43), inference(avatar_component_clause, [], [f596])).
fof(f1145, plain, (~ (e1 = op(op(e3, e1), e3)) | spl34_157), inference(avatar_component_clause, [], [f1143])).
fof(f1143, plain, (spl34_157 <=> (e1 = op(op(e3, e1), e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_157])])).
fof(f3726, plain, (~ spl34_57 | ~ spl34_89 | spl34_168), inference(avatar_contradiction_clause, [], [f3725])).
fof(f3725, plain, ($false | (~ spl34_57 | ~ spl34_89 | spl34_168)), inference(subsumption_resolution, [], [f3724, f791])).
fof(f3724, plain, (~ (e3 = op(e1, e2)) | (~ spl34_57 | spl34_168)), inference(forward_demodulation, [], [f1198, f657])).
fof(f1198, plain, (~ (e3 = op(op(e2, e3), e2)) | spl34_168), inference(avatar_component_clause, [], [f1196])).
fof(f1196, plain, (spl34_168 <=> (e3 = op(op(e2, e3), e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_168])])).
fof(f3717, plain, (~ spl34_49 | ~ spl34_109 | spl34_163), inference(avatar_contradiction_clause, [], [f3716])).
fof(f3716, plain, ($false | (~ spl34_49 | ~ spl34_109 | spl34_163)), inference(subsumption_resolution, [], [f3715, f875])).
fof(f3715, plain, (~ (e3 = op(e0, e3)) | (~ spl34_49 | spl34_163)), inference(forward_demodulation, [], [f1174, f623])).
fof(f623, plain, ((e3 = op(e3, e0)) | ~ spl34_49), inference(avatar_component_clause, [], [f621])).
fof(f621, plain, (spl34_49 <=> (e3 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_49])])).
fof(f1174, plain, (~ (op(e0, e3) = op(e3, e0)) | spl34_163), inference(avatar_component_clause, [], [f1172])).
fof(f1172, plain, (spl34_163 <=> (op(e0, e3) = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_163])])).
fof(f3707, plain, (~ spl34_81 | spl34_107 | ~ spl34_185), inference(avatar_contradiction_clause, [], [f3706])).
fof(f3706, plain, ($false | (~ spl34_81 | spl34_107 | ~ spl34_185)), inference(subsumption_resolution, [], [f3705, f866])).
fof(f866, plain, (~ (e1 = op(e0, e3)) | spl34_107), inference(avatar_component_clause, [], [f865])).
fof(f865, plain, (spl34_107 <=> (e1 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_107])])).
fof(f3705, plain, ((e1 = op(e0, e3)) | (~ spl34_81 | ~ spl34_185)), inference(forward_demodulation, [], [f1282, f758])).
fof(f758, plain, ((e0 = op(e1, e3)) | ~ spl34_81), inference(avatar_component_clause, [], [f756])).
fof(f756, plain, (spl34_81 <=> (e0 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_81])])).
fof(f1282, plain, ((e1 = op(op(e1, e3), e3)) | ~ spl34_185), inference(avatar_component_clause, [], [f1280])).
fof(f1280, plain, (spl34_185 <=> (e1 = op(op(e1, e3), e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_185])])).
fof(f3703, plain, (~ spl34_66 | ~ spl34_117 | ~ spl34_174), inference(avatar_contradiction_clause, [], [f3702])).
fof(f3702, plain, ($false | (~ spl34_66 | ~ spl34_117 | ~ spl34_174)), inference(subsumption_resolution, [], [f3701, f269])).
fof(f3701, plain, ((e1 = e2) | (~ spl34_66 | ~ spl34_117 | ~ spl34_174)), inference(forward_demodulation, [], [f3700, f909])).
fof(f909, plain, ((e1 = op(e0, e1)) | ~ spl34_117), inference(avatar_component_clause, [], [f907])).
fof(f907, plain, (spl34_117 <=> (e1 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_117])])).
fof(f3700, plain, ((e2 = op(e0, e1)) | (~ spl34_66 | ~ spl34_174)), inference(forward_demodulation, [], [f1229, f695])).
fof(f695, plain, ((e0 = op(e2, e1)) | ~ spl34_66), inference(avatar_component_clause, [], [f693])).
fof(f693, plain, (spl34_66 <=> (e0 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_66])])).
fof(f1229, plain, ((e2 = op(op(e2, e1), e1)) | ~ spl34_174), inference(avatar_component_clause, [], [f1227])).
fof(f1227, plain, (spl34_174 <=> (e2 = op(op(e2, e1), e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_174])])).
fof(f3689, plain, (~ spl34_73 | ~ spl34_113 | spl34_179), inference(avatar_contradiction_clause, [], [f3688])).
fof(f3688, plain, ($false | (~ spl34_73 | ~ spl34_113 | spl34_179)), inference(subsumption_resolution, [], [f3687, f892])).
fof(f3687, plain, (~ (e2 = op(e0, e2)) | (~ spl34_73 | spl34_179)), inference(forward_demodulation, [], [f1253, f724])).
fof(f724, plain, ((e2 = op(e2, e0)) | ~ spl34_73), inference(avatar_component_clause, [], [f722])).
fof(f722, plain, (spl34_73 <=> (e2 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_73])])).
fof(f1253, plain, (~ (op(e0, e2) = op(e2, e0)) | spl34_179), inference(avatar_component_clause, [], [f1251])).
fof(f1251, plain, (spl34_179 <=> (op(e0, e2) = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_179])])).
fof(f3667, plain, (~ spl34_25 | ~ spl34_105 | spl34_146), inference(avatar_contradiction_clause, [], [f3666])).
fof(f3666, plain, ($false | (~ spl34_25 | ~ spl34_105 | spl34_146)), inference(subsumption_resolution, [], [f3665, f858])).
fof(f858, plain, ((e4 = op(e0, e4)) | ~ spl34_105), inference(avatar_component_clause, [], [f856])).
fof(f856, plain, (spl34_105 <=> (e4 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_105])])).
fof(f3665, plain, (~ (e4 = op(e0, e4)) | (~ spl34_25 | spl34_146)), inference(forward_demodulation, [], [f1091, f522])).
fof(f1091, plain, (~ (op(e0, e4) = op(e4, e0)) | spl34_146), inference(avatar_component_clause, [], [f1089])).
fof(f1089, plain, (spl34_146 <=> (op(e0, e4) = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_146])])).
fof(f3662, plain, (~ spl34_97 | ~ spl34_117 | spl34_194), inference(avatar_contradiction_clause, [], [f3661])).
fof(f3661, plain, ($false | (~ spl34_97 | ~ spl34_117 | spl34_194)), inference(subsumption_resolution, [], [f3660, f909])).
fof(f3660, plain, (~ (e1 = op(e0, e1)) | (~ spl34_97 | spl34_194)), inference(forward_demodulation, [], [f1328, f825])).
fof(f825, plain, ((e1 = op(e1, e0)) | ~ spl34_97), inference(avatar_component_clause, [], [f823])).
fof(f823, plain, (spl34_97 <=> (e1 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_97])])).
fof(f1328, plain, (~ (op(e0, e1) = op(e1, e0)) | spl34_194), inference(avatar_component_clause, [], [f1326])).
fof(f1326, plain, (spl34_194 <=> (op(e0, e1) = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_194])])).
fof(f3607, plain, (~ spl34_19 | ~ spl34_27 | spl34_140), inference(avatar_contradiction_clause, [], [f3606])).
fof(f3606, plain, ($false | (~ spl34_19 | ~ spl34_27 | spl34_140)), inference(subsumption_resolution, [], [f3605, f531])).
fof(f3605, plain, (~ (e1 = op(e3, e4)) | (~ spl34_19 | spl34_140)), inference(forward_demodulation, [], [f1062, f497])).
fof(f497, plain, ((e3 = op(e4, e1)) | ~ spl34_19), inference(avatar_component_clause, [], [f495])).
fof(f495, plain, (spl34_19 <=> (e3 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_19])])).
fof(f1062, plain, (~ (e1 = op(op(e4, e1), e4)) | spl34_140), inference(avatar_component_clause, [], [f1060])).
fof(f1060, plain, (spl34_140 <=> (e1 = op(op(e4, e1), e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_140])])).
fof(f3568, plain, (~ spl34_12 | ~ spl34_78 | spl34_136), inference(avatar_contradiction_clause, [], [f3567])).
fof(f3567, plain, ($false | (~ spl34_12 | ~ spl34_78 | spl34_136)), inference(subsumption_resolution, [], [f3562, f745])).
fof(f3562, plain, (~ (e2 = op(e1, e4)) | (~ spl34_12 | spl34_136)), inference(backward_demodulation, [], [f1043, f468])).
fof(f468, plain, ((e1 = op(e4, e2)) | ~ spl34_12), inference(avatar_component_clause, [], [f466])).
fof(f466, plain, (spl34_12 <=> (e1 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_12])])).
fof(f1043, plain, (~ (e2 = op(op(e4, e2), e4)) | spl34_136), inference(avatar_component_clause, [], [f1041])).
fof(f1041, plain, (spl34_136 <=> (e2 = op(op(e4, e2), e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_136])])).
fof(f3443, plain, (~ spl34_80 | ~ spl34_105), inference(avatar_split_clause, [], [f3440, f856, f751])).
fof(f751, plain, (spl34_80 <=> (e4 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_80])])).
fof(f3440, plain, (~ (e4 = op(e1, e4)) | ~ spl34_105), inference(backward_demodulation, [], [f205, f858])).
fof(f205, plain, ~ (op(e0, e4) = op(e1, e4)), inference(cnf_transformation, [], [f4])).
fof(f3427, plain, (~ spl34_88 | ~ spl34_113), inference(avatar_split_clause, [], [f3419, f890, f785])).
fof(f785, plain, (spl34_88 <=> (e2 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_88])])).
fof(f3419, plain, (~ (e2 = op(e1, e2)) | ~ spl34_113), inference(backward_demodulation, [], [f185, f892])).
fof(f185, plain, ~ (op(e0, e2) = op(e1, e2)), inference(cnf_transformation, [], [f4])).
fof(f3410, plain, (~ spl34_112 | ~ spl34_117), inference(avatar_split_clause, [], [f3392, f907, f886])).
fof(f886, plain, (spl34_112 <=> (e1 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_112])])).
fof(f3392, plain, (~ (e1 = op(e0, e2)) | ~ spl34_117), inference(backward_demodulation, [], [f219, f909])).
fof(f219, plain, ~ (op(e0, e1) = op(e0, e2)), inference(cnf_transformation, [], [f4])).
fof(f3383, plain, (spl34_25 | ~ spl34_126), inference(avatar_split_clause, [], [f3369, f945, f520])).
fof(f945, plain, (spl34_126 <=> (e0 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_126])])).
fof(f3369, plain, ((e4 = op(e4, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f113, f947])).
fof(f947, plain, ((e0 = unit) | ~ spl34_126), inference(avatar_component_clause, [], [f945])).
fof(f113, plain, (e4 = op(e4, unit)), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)) & (e4 = op(e4, unit)) & (e4 = op(unit, e4)) & (e3 = op(e3, unit)) & (e3 = op(unit, e3)) & (e2 = op(e2, unit)) & (e2 = op(unit, e2)) & (e1 = op(e1, unit)) & (e1 = op(unit, e1)) & (e0 = op(e0, unit)) & (e0 = op(unit, e0))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG057+1.p', ax2)).
fof(f3382, plain, (spl34_105 | ~ spl34_126), inference(avatar_split_clause, [], [f3368, f945, f856])).
fof(f3368, plain, ((e4 = op(e0, e4)) | ~ spl34_126), inference(backward_demodulation, [], [f112, f947])).
fof(f112, plain, (e4 = op(unit, e4)), inference(cnf_transformation, [], [f2])).
fof(f3381, plain, (spl34_49 | ~ spl34_126), inference(avatar_split_clause, [], [f3367, f945, f621])).
fof(f3367, plain, ((e3 = op(e3, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f111, f947])).
fof(f111, plain, (e3 = op(e3, unit)), inference(cnf_transformation, [], [f2])).
fof(f3380, plain, (spl34_109 | ~ spl34_126), inference(avatar_split_clause, [], [f3366, f945, f873])).
fof(f3366, plain, ((e3 = op(e0, e3)) | ~ spl34_126), inference(backward_demodulation, [], [f110, f947])).
fof(f110, plain, (e3 = op(unit, e3)), inference(cnf_transformation, [], [f2])).
fof(f3379, plain, (spl34_73 | ~ spl34_126), inference(avatar_split_clause, [], [f3365, f945, f722])).
fof(f3365, plain, ((e2 = op(e2, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f109, f947])).
fof(f109, plain, (e2 = op(e2, unit)), inference(cnf_transformation, [], [f2])).
fof(f3378, plain, (spl34_113 | ~ spl34_126), inference(avatar_split_clause, [], [f3364, f945, f890])).
fof(f3364, plain, ((e2 = op(e0, e2)) | ~ spl34_126), inference(backward_demodulation, [], [f108, f947])).
fof(f108, plain, (e2 = op(unit, e2)), inference(cnf_transformation, [], [f2])).
fof(f3377, plain, (spl34_97 | ~ spl34_126), inference(avatar_split_clause, [], [f3363, f945, f823])).
fof(f3363, plain, ((e1 = op(e1, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f107, f947])).
fof(f107, plain, (e1 = op(e1, unit)), inference(cnf_transformation, [], [f2])).
fof(f3376, plain, (spl34_117 | ~ spl34_126), inference(avatar_split_clause, [], [f3362, f945, f907])).
fof(f3362, plain, ((e1 = op(e0, e1)) | ~ spl34_126), inference(backward_demodulation, [], [f106, f947])).
fof(f106, plain, (e1 = op(unit, e1)), inference(cnf_transformation, [], [f2])).
fof(f3375, plain, (spl34_121 | ~ spl34_126), inference(avatar_split_clause, [], [f3361, f945, f924])).
fof(f924, plain, (spl34_121 <=> (e0 = op(e0, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_121])])).
fof(f3361, plain, ((e0 = op(e0, e0)) | ~ spl34_126), inference(backward_demodulation, [], [f105, f947])).
fof(f105, plain, (e0 = op(e0, unit)), inference(cnf_transformation, [], [f2])).
fof(f3357, plain, (spl34_8 | ~ spl34_54 | ~ spl34_238), inference(avatar_split_clause, [], [f3356, f1541, f642, f449])).
fof(f3356, plain, ((e2 = op(e4, e3)) | (~ spl34_54 | ~ spl34_238)), inference(forward_demodulation, [], [f1543, f644])).
fof(f1543, plain, ((e2 = op(e4, op(e2, e4))) | ~ spl34_238), inference(avatar_component_clause, [], [f1541])).
fof(f3290, plain, (~ spl34_8 | ~ spl34_23), inference(avatar_split_clause, [], [f3285, f512, f449])).
fof(f512, plain, (spl34_23 <=> (e2 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_23])])).
fof(f3285, plain, (~ (e2 = op(e4, e3)) | ~ spl34_23), inference(backward_demodulation, [], [f257, f514])).
fof(f514, plain, ((e2 = op(e4, e0)) | ~ spl34_23), inference(avatar_component_clause, [], [f512])).
fof(f257, plain, ~ (op(e4, e0) = op(e4, e3)), inference(cnf_transformation, [], [f4])).
fof(f3265, plain, (~ spl34_107 | ~ spl34_112), inference(avatar_split_clause, [], [f3258, f886, f865])).
fof(f3258, plain, (~ (e1 = op(e0, e3)) | ~ spl34_112), inference(backward_demodulation, [], [f222, f888])).
fof(f888, plain, ((e1 = op(e0, e2)) | ~ spl34_112), inference(avatar_component_clause, [], [f886])).
fof(f222, plain, ~ (op(e0, e2) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f3242, plain, (spl34_19 | ~ spl34_27 | ~ spl34_239), inference(avatar_contradiction_clause, [], [f3241])).
fof(f3241, plain, ($false | (spl34_19 | ~ spl34_27 | ~ spl34_239)), inference(subsumption_resolution, [], [f3240, f496])).
fof(f496, plain, (~ (e3 = op(e4, e1)) | spl34_19), inference(avatar_component_clause, [], [f495])).
fof(f3240, plain, ((e3 = op(e4, e1)) | (~ spl34_27 | ~ spl34_239)), inference(forward_demodulation, [], [f1547, f531])).
fof(f1547, plain, ((e3 = op(e4, op(e3, e4))) | ~ spl34_239), inference(avatar_component_clause, [], [f1545])).
fof(f1545, plain, (spl34_239 <=> (e3 = op(e4, op(e3, e4)))), introduced(avatar_definition, [new_symbols(naming, [spl34_239])])).
fof(f3213, plain, (~ spl34_72 | ~ spl34_116 | ~ spl34_258), inference(avatar_contradiction_clause, [], [f3212])).
fof(f3212, plain, ($false | (~ spl34_72 | ~ spl34_116 | ~ spl34_258)), inference(subsumption_resolution, [], [f3211, f266])).
fof(f266, plain, ~ (e0 = e2), inference(cnf_transformation, [], [f5])).
fof(f3211, plain, ((e0 = e2) | (~ spl34_72 | ~ spl34_116 | ~ spl34_258)), inference(forward_demodulation, [], [f3210, f905])).
fof(f905, plain, ((e0 = op(e0, e1)) | ~ spl34_116), inference(avatar_component_clause, [], [f903])).
fof(f903, plain, (spl34_116 <=> (e0 = op(e0, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_116])])).
fof(f3210, plain, ((e2 = op(e0, e1)) | (~ spl34_72 | ~ spl34_258)), inference(forward_demodulation, [], [f1663, f720])).
fof(f720, plain, ((e1 = op(e2, e0)) | ~ spl34_72), inference(avatar_component_clause, [], [f718])).
fof(f718, plain, (spl34_72 <=> (e1 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_72])])).
fof(f1663, plain, ((e2 = op(e0, op(e2, e0))) | ~ spl34_258), inference(avatar_component_clause, [], [f1661])).
fof(f1661, plain, (spl34_258 <=> (e2 = op(e0, op(e2, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_258])])).
fof(f3205, plain, (~ spl34_8 | ~ spl34_54 | spl34_132), inference(avatar_contradiction_clause, [], [f3204])).
fof(f3204, plain, ($false | (~ spl34_8 | ~ spl34_54 | spl34_132)), inference(subsumption_resolution, [], [f3202, f644])).
fof(f3202, plain, (~ (e3 = op(e2, e4)) | (~ spl34_8 | spl34_132)), inference(backward_demodulation, [], [f1024, f451])).
fof(f451, plain, ((e2 = op(e4, e3)) | ~ spl34_8), inference(avatar_component_clause, [], [f449])).
fof(f1024, plain, (~ (e3 = op(op(e4, e3), e4)) | spl34_132), inference(avatar_component_clause, [], [f1022])).
fof(f1022, plain, (spl34_132 <=> (e3 = op(op(e4, e3), e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_132])])).
fof(f3187, plain, (~ spl34_110 | ~ spl34_125), inference(avatar_split_clause, [], [f3186, f940, f877])).
fof(f877, plain, (spl34_110 <=> (e4 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_110])])).
fof(f940, plain, (spl34_125 <=> (op(e0, e0) = e4)), introduced(avatar_definition, [new_symbols(naming, [spl34_125])])).
fof(f3186, plain, (~ (e4 = op(e0, e3)) | ~ spl34_125), inference(backward_demodulation, [], [f217, f942])).
fof(f942, plain, ((op(e0, e0) = e4) | ~ spl34_125), inference(avatar_component_clause, [], [f940])).
fof(f217, plain, ~ (op(e0, e0) = op(e0, e3)), inference(cnf_transformation, [], [f4])).
fof(f3172, plain, (spl34_110 | ~ spl34_24 | ~ spl34_260), inference(avatar_split_clause, [], [f3012, f1669, f516, f877])).
fof(f516, plain, (spl34_24 <=> (e3 = op(e4, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_24])])).
fof(f1669, plain, (spl34_260 <=> (e4 = op(e0, op(e4, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_260])])).
fof(f3012, plain, ((e4 = op(e0, e3)) | (~ spl34_24 | ~ spl34_260)), inference(forward_demodulation, [], [f1671, f518])).
fof(f518, plain, ((e3 = op(e4, e0)) | ~ spl34_24), inference(avatar_component_clause, [], [f516])).
fof(f1671, plain, ((e4 = op(e0, op(e4, e0))) | ~ spl34_260), inference(avatar_component_clause, [], [f1669])).
fof(f3164, plain, (spl34_20 | ~ spl34_127), inference(avatar_split_clause, [], [f2860, f949, f499])).
fof(f949, plain, (spl34_127 <=> (e1 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_127])])).
fof(f2860, plain, ((e4 = op(e4, e1)) | ~ spl34_127), inference(backward_demodulation, [], [f113, f951])).
fof(f951, plain, ((e1 = unit) | ~ spl34_127), inference(avatar_component_clause, [], [f949])).
fof(f3144, plain, (~ spl34_82 | ~ spl34_84), inference(avatar_contradiction_clause, [], [f3143])).
fof(f3143, plain, ($false | (~ spl34_82 | ~ spl34_84)), inference(subsumption_resolution, [], [f3141, f270])).
fof(f270, plain, ~ (e1 = e3), inference(cnf_transformation, [], [f5])).
fof(f3141, plain, ((e1 = e3) | (~ spl34_82 | ~ spl34_84)), inference(backward_demodulation, [], [f770, f762])).
fof(f762, plain, ((e1 = op(e1, e3)) | ~ spl34_82), inference(avatar_component_clause, [], [f760])).
fof(f760, plain, (spl34_82 <=> (e1 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_82])])).
fof(f770, plain, ((e3 = op(e1, e3)) | ~ spl34_84), inference(avatar_component_clause, [], [f768])).
fof(f768, plain, (spl34_84 <=> (e3 = op(e1, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_84])])).
fof(f3137, plain, (~ spl34_96 | ~ spl34_100), inference(avatar_contradiction_clause, [], [f3136])).
fof(f3136, plain, ($false | (~ spl34_96 | ~ spl34_100)), inference(subsumption_resolution, [], [f3135, f268])).
fof(f268, plain, ~ (e0 = e4), inference(cnf_transformation, [], [f5])).
fof(f3135, plain, ((e0 = e4) | (~ spl34_96 | ~ spl34_100)), inference(forward_demodulation, [], [f837, f821])).
fof(f821, plain, ((e0 = op(e1, e0)) | ~ spl34_96), inference(avatar_component_clause, [], [f819])).
fof(f819, plain, (spl34_96 <=> (e0 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_96])])).
fof(f837, plain, ((e4 = op(e1, e0)) | ~ spl34_100), inference(avatar_component_clause, [], [f835])).
fof(f835, plain, (spl34_100 <=> (e4 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_100])])).
fof(f3090, plain, (~ spl34_36 | ~ spl34_48 | spl34_262), inference(avatar_contradiction_clause, [], [f3089])).
fof(f3089, plain, ($false | (~ spl34_36 | ~ spl34_48 | spl34_262)), inference(subsumption_resolution, [], [f3088, f569])).
fof(f3088, plain, (~ (e0 = op(e3, e2)) | (~ spl34_48 | spl34_262)), inference(forward_demodulation, [], [f1681, f619])).
fof(f619, plain, ((e2 = op(e3, e0)) | ~ spl34_48), inference(avatar_component_clause, [], [f617])).
fof(f617, plain, (spl34_48 <=> (e2 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_48])])).
fof(f1681, plain, (~ (e0 = op(e3, op(e3, e0))) | spl34_262), inference(avatar_component_clause, [], [f1679])).
fof(f1679, plain, (spl34_262 <=> (e0 = op(e3, op(e3, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_262])])).
fof(f3055, plain, (~ spl34_48 | ~ spl34_50), inference(avatar_contradiction_clause, [], [f3054])).
fof(f3054, plain, ($false | (~ spl34_48 | ~ spl34_50)), inference(subsumption_resolution, [], [f3052, f273])).
fof(f273, plain, ~ (e2 = e4), inference(cnf_transformation, [], [f5])).
fof(f3052, plain, ((e2 = e4) | (~ spl34_48 | ~ spl34_50)), inference(backward_demodulation, [], [f627, f619])).
fof(f627, plain, ((e4 = op(e3, e0)) | ~ spl34_50), inference(avatar_component_clause, [], [f625])).
fof(f625, plain, (spl34_50 <=> (e4 = op(e3, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_50])])).
fof(f3026, plain, (~ spl34_7 | ~ spl34_54 | ~ spl34_238), inference(avatar_contradiction_clause, [], [f3025])).
fof(f3025, plain, ($false | (~ spl34_7 | ~ spl34_54 | ~ spl34_238)), inference(subsumption_resolution, [], [f3024, f269])).
fof(f3024, plain, ((e1 = e2) | (~ spl34_7 | ~ spl34_54 | ~ spl34_238)), inference(forward_demodulation, [], [f3023, f447])).
fof(f447, plain, ((e1 = op(e4, e3)) | ~ spl34_7), inference(avatar_component_clause, [], [f445])).
fof(f445, plain, (spl34_7 <=> (e1 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_7])])).
fof(f3023, plain, ((e2 = op(e4, e3)) | (~ spl34_54 | ~ spl34_238)), inference(forward_demodulation, [], [f1543, f644])).
fof(f2932, plain, (~ spl34_62 | ~ spl34_65), inference(avatar_contradiction_clause, [], [f2931])).
fof(f2931, plain, ($false | (~ spl34_62 | ~ spl34_65)), inference(subsumption_resolution, [], [f2930, f271])).
fof(f271, plain, ~ (e1 = e4), inference(cnf_transformation, [], [f5])).
fof(f2930, plain, ((e1 = e4) | (~ spl34_62 | ~ spl34_65)), inference(backward_demodulation, [], [f690, f678])).
fof(f678, plain, ((e1 = op(e2, e2)) | ~ spl34_62), inference(avatar_component_clause, [], [f676])).
fof(f676, plain, (spl34_62 <=> (e1 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_62])])).
fof(f690, plain, ((e4 = op(e2, e2)) | ~ spl34_65), inference(avatar_component_clause, [], [f688])).
fof(f688, plain, (spl34_65 <=> (e4 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_65])])).
fof(f2894, plain, (spl34_11 | ~ spl34_103 | ~ spl34_213), inference(avatar_contradiction_clause, [], [f2893])).
fof(f2893, plain, ($false | (spl34_11 | ~ spl34_103 | ~ spl34_213)), inference(subsumption_resolution, [], [f2891, f463])).
fof(f463, plain, (~ (e0 = op(e4, e2)) | spl34_11), inference(avatar_component_clause, [], [f462])).
fof(f462, plain, (spl34_11 <=> (e0 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_11])])).
fof(f2891, plain, ((e0 = op(e4, e2)) | (~ spl34_103 | ~ spl34_213)), inference(backward_demodulation, [], [f1422, f850])).
fof(f850, plain, ((e2 = op(e0, e4)) | ~ spl34_103), inference(avatar_component_clause, [], [f848])).
fof(f848, plain, (spl34_103 <=> (e2 = op(e0, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_103])])).
fof(f1422, plain, ((e0 = op(e4, op(e0, e4))) | ~ spl34_213), inference(avatar_component_clause, [], [f1420])).
fof(f1420, plain, (spl34_213 <=> (e0 = op(e4, op(e0, e4)))), introduced(avatar_definition, [new_symbols(naming, [spl34_213])])).
fof(f2873, plain, (~ spl34_78 | ~ spl34_127), inference(avatar_contradiction_clause, [], [f2872])).
fof(f2872, plain, ($false | (~ spl34_78 | ~ spl34_127)), inference(subsumption_resolution, [], [f2871, f273])).
fof(f2871, plain, ((e2 = e4) | (~ spl34_78 | ~ spl34_127)), inference(forward_demodulation, [], [f2859, f745])).
fof(f2859, plain, ((e4 = op(e1, e4)) | ~ spl34_127), inference(backward_demodulation, [], [f112, f951])).
fof(f2870, plain, (spl34_84 | ~ spl34_127), inference(avatar_split_clause, [], [f2857, f949, f768])).
fof(f2857, plain, ((e3 = op(e1, e3)) | ~ spl34_127), inference(backward_demodulation, [], [f110, f951])).
fof(f2869, plain, (spl34_68 | ~ spl34_127), inference(avatar_split_clause, [], [f2856, f949, f701])).
fof(f701, plain, (spl34_68 <=> (e2 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_68])])).
fof(f2856, plain, ((e2 = op(e2, e1)) | ~ spl34_127), inference(backward_demodulation, [], [f109, f951])).
fof(f2868, plain, (spl34_88 | ~ spl34_127), inference(avatar_split_clause, [], [f2855, f949, f785])).
fof(f2855, plain, ((e2 = op(e1, e2)) | ~ spl34_127), inference(backward_demodulation, [], [f108, f951])).
fof(f2865, plain, (spl34_116 | ~ spl34_127), inference(avatar_split_clause, [], [f2852, f949, f903])).
fof(f2852, plain, ((e0 = op(e0, e1)) | ~ spl34_127), inference(backward_demodulation, [], [f105, f951])).
fof(f2864, plain, (spl34_96 | ~ spl34_127), inference(avatar_split_clause, [], [f2851, f949, f819])).
fof(f2851, plain, ((e0 = op(e1, e0)) | ~ spl34_127), inference(backward_demodulation, [], [f104, f951])).
fof(f104, plain, (e0 = op(unit, e0)), inference(cnf_transformation, [], [f2])).
fof(f2817, plain, (~ spl34_103 | ~ spl34_123), inference(avatar_split_clause, [], [f2816, f932, f848])).
fof(f932, plain, (spl34_123 <=> (op(e0, e0) = e2)), introduced(avatar_definition, [new_symbols(naming, [spl34_123])])).
fof(f2816, plain, (~ (e2 = op(e0, e4)) | ~ spl34_123), inference(forward_demodulation, [], [f218, f934])).
fof(f934, plain, ((op(e0, e0) = e2) | ~ spl34_123), inference(avatar_component_clause, [], [f932])).
fof(f218, plain, ~ (op(e0, e0) = op(e0, e4)), inference(cnf_transformation, [], [f4])).
fof(f2718, plain, (~ spl34_54 | ~ spl34_128), inference(avatar_contradiction_clause, [], [f2717])).
fof(f2717, plain, ($false | (~ spl34_54 | ~ spl34_128)), inference(subsumption_resolution, [], [f2716, f274])).
fof(f274, plain, ~ (e3 = e4), inference(cnf_transformation, [], [f5])).
fof(f2716, plain, ((e3 = e4) | (~ spl34_54 | ~ spl34_128)), inference(forward_demodulation, [], [f2700, f644])).
fof(f2700, plain, ((e4 = op(e2, e4)) | ~ spl34_128), inference(backward_demodulation, [], [f112, f955])).
fof(f955, plain, ((e2 = unit) | ~ spl34_128), inference(avatar_component_clause, [], [f953])).
fof(f953, plain, (spl34_128 <=> (e2 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_128])])).
fof(f2670, plain, (~ spl34_74 | ~ spl34_54), inference(avatar_split_clause, [], [f2669, f642, f726])).
fof(f726, plain, (spl34_74 <=> (e3 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_74])])).
fof(f2669, plain, (~ (e3 = op(e2, e0)) | ~ spl34_54), inference(forward_demodulation, [], [f238, f644])).
fof(f238, plain, ~ (op(e2, e0) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f2660, plain, (~ spl34_31 | ~ spl34_36), inference(avatar_split_clause, [], [f2659, f567, f546])).
fof(f546, plain, (spl34_31 <=> (e0 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_31])])).
fof(f2659, plain, (~ (e0 = op(e3, e3)) | ~ spl34_36), inference(forward_demodulation, [], [f252, f569])).
fof(f252, plain, ~ (op(e3, e2) = op(e3, e3)), inference(cnf_transformation, [], [f4])).
fof(f2640, plain, (~ spl34_27 | ~ spl34_28), inference(avatar_contradiction_clause, [], [f2639])).
fof(f2639, plain, ($false | (~ spl34_27 | ~ spl34_28)), inference(subsumption_resolution, [], [f2638, f269])).
fof(f2638, plain, ((e1 = e2) | (~ spl34_27 | ~ spl34_28)), inference(forward_demodulation, [], [f535, f531])).
fof(f535, plain, ((e2 = op(e3, e4)) | ~ spl34_28), inference(avatar_component_clause, [], [f533])).
fof(f533, plain, (spl34_28 <=> (e2 = op(e3, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_28])])).
fof(f2629, plain, (~ spl34_61 | ~ spl34_65), inference(avatar_contradiction_clause, [], [f2628])).
fof(f2628, plain, ($false | (~ spl34_61 | ~ spl34_65)), inference(subsumption_resolution, [], [f2627, f268])).
fof(f2627, plain, ((e0 = e4) | (~ spl34_61 | ~ spl34_65)), inference(backward_demodulation, [], [f690, f674])).
fof(f674, plain, ((e0 = op(e2, e2)) | ~ spl34_61), inference(avatar_component_clause, [], [f672])).
fof(f672, plain, (spl34_61 <=> (e0 = op(e2, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_61])])).
fof(f2612, plain, (~ spl34_27 | ~ spl34_130), inference(avatar_contradiction_clause, [], [f2611])).
fof(f2611, plain, ($false | (~ spl34_27 | ~ spl34_130)), inference(subsumption_resolution, [], [f2610, f270])).
fof(f2610, plain, ((e1 = e3) | (~ spl34_27 | ~ spl34_130)), inference(forward_demodulation, [], [f2594, f531])).
fof(f2594, plain, ((e3 = op(e3, e4)) | ~ spl34_130), inference(backward_demodulation, [], [f111, f963])).
fof(f963, plain, ((e4 = unit) | ~ spl34_130), inference(avatar_component_clause, [], [f961])).
fof(f961, plain, (spl34_130 <=> (e4 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_130])])).
fof(f2509, plain, (~ spl34_51 | ~ spl34_54), inference(avatar_contradiction_clause, [], [f2508])).
fof(f2508, plain, ($false | (~ spl34_51 | ~ spl34_54)), inference(subsumption_resolution, [], [f2506, f267])).
fof(f267, plain, ~ (e0 = e3), inference(cnf_transformation, [], [f5])).
fof(f2506, plain, ((e0 = e3) | (~ spl34_51 | ~ spl34_54)), inference(backward_demodulation, [], [f644, f632])).
fof(f632, plain, ((e0 = op(e2, e4)) | ~ spl34_51), inference(avatar_component_clause, [], [f630])).
fof(f630, plain, (spl34_51 <=> (e0 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_51])])).
fof(f2470, plain, (~ spl34_27 | ~ spl34_129), inference(avatar_contradiction_clause, [], [f2469])).
fof(f2469, plain, ($false | (~ spl34_27 | ~ spl34_129)), inference(subsumption_resolution, [], [f2468, f271])).
fof(f2468, plain, ((e1 = e4) | (~ spl34_27 | ~ spl34_129)), inference(forward_demodulation, [], [f2447, f531])).
fof(f2447, plain, ((e4 = op(e3, e4)) | ~ spl34_129), inference(backward_demodulation, [], [f112, f959])).
fof(f959, plain, ((e3 = unit) | ~ spl34_129), inference(avatar_component_clause, [], [f957])).
fof(f957, plain, (spl34_129 <=> (e3 = unit)), introduced(avatar_definition, [new_symbols(naming, [spl34_129])])).
fof(f2394, plain, (~ spl34_13 | ~ spl34_113), inference(avatar_split_clause, [], [f2393, f890, f470])).
fof(f470, plain, (spl34_13 <=> (e2 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_13])])).
fof(f2393, plain, (~ (e2 = op(e4, e2)) | ~ spl34_113), inference(forward_demodulation, [], [f188, f892])).
fof(f188, plain, ~ (op(e0, e2) = op(e4, e2)), inference(cnf_transformation, [], [f4])).
fof(f2391, plain, (~ spl34_11 | ~ spl34_1), inference(avatar_split_clause, [], [f2223, f420, f462])).
fof(f420, plain, (spl34_1 <=> (e0 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_1])])).
fof(f2223, plain, (~ (e0 = op(e4, e2)) | ~ spl34_1), inference(forward_demodulation, [], [f263, f422])).
fof(f422, plain, ((e0 = op(e4, e4)) | ~ spl34_1), inference(avatar_component_clause, [], [f420])).
fof(f263, plain, ~ (op(e4, e2) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2369, plain, (~ spl34_1 | ~ spl34_3), inference(avatar_contradiction_clause, [], [f2368])).
fof(f2368, plain, ($false | (~ spl34_1 | ~ spl34_3)), inference(subsumption_resolution, [], [f2367, f266])).
fof(f2367, plain, ((e0 = e2) | (~ spl34_1 | ~ spl34_3)), inference(forward_demodulation, [], [f430, f422])).
fof(f430, plain, ((e2 = op(e4, e4)) | ~ spl34_3), inference(avatar_component_clause, [], [f428])).
fof(f428, plain, (spl34_3 <=> (e2 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_3])])).
fof(f2348, plain, (~ spl34_56 | ~ spl34_57), inference(avatar_contradiction_clause, [], [f2347])).
fof(f2347, plain, ($false | (~ spl34_56 | ~ spl34_57)), inference(subsumption_resolution, [], [f2346, f265])).
fof(f265, plain, ~ (e0 = e1), inference(cnf_transformation, [], [f5])).
fof(f2346, plain, ((e0 = e1) | (~ spl34_56 | ~ spl34_57)), inference(forward_demodulation, [], [f657, f653])).
fof(f653, plain, ((e0 = op(e2, e3)) | ~ spl34_56), inference(avatar_component_clause, [], [f651])).
fof(f651, plain, (spl34_56 <=> (e0 = op(e2, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_56])])).
fof(f2328, plain, (~ spl34_88 | ~ spl34_98), inference(avatar_split_clause, [], [f2325, f827, f785])).
fof(f827, plain, (spl34_98 <=> (e2 = op(e1, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_98])])).
fof(f2325, plain, (~ (e2 = op(e1, e2)) | ~ spl34_98), inference(backward_demodulation, [], [f226, f829])).
fof(f829, plain, ((e2 = op(e1, e0)) | ~ spl34_98), inference(avatar_component_clause, [], [f827])).
fof(f226, plain, ~ (op(e1, e0) = op(e1, e2)), inference(cnf_transformation, [], [f4])).
fof(f2324, plain, (~ spl34_107 | ~ spl34_109), inference(avatar_contradiction_clause, [], [f2323])).
fof(f2323, plain, ($false | (~ spl34_107 | ~ spl34_109)), inference(subsumption_resolution, [], [f2322, f270])).
fof(f2322, plain, ((e1 = e3) | (~ spl34_107 | ~ spl34_109)), inference(backward_demodulation, [], [f875, f867])).
fof(f867, plain, ((e1 = op(e0, e3)) | ~ spl34_107), inference(avatar_component_clause, [], [f865])).
fof(f2253, plain, (~ spl34_77 | ~ spl34_27), inference(avatar_split_clause, [], [f2252, f529, f739])).
fof(f739, plain, (spl34_77 <=> (e1 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_77])])).
fof(f2252, plain, (~ (e1 = op(e1, e4)) | ~ spl34_27), inference(forward_demodulation, [], [f210, f531])).
fof(f210, plain, ~ (op(e1, e4) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f2248, plain, (~ spl34_79 | ~ spl34_54), inference(avatar_split_clause, [], [f2245, f642, f747])).
fof(f747, plain, (spl34_79 <=> (e3 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_79])])).
fof(f2245, plain, (~ (e3 = op(e1, e4)) | ~ spl34_54), inference(backward_demodulation, [], [f209, f644])).
fof(f209, plain, ~ (op(e1, e4) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f2247, plain, (~ spl34_69 | ~ spl34_54), inference(avatar_split_clause, [], [f2244, f642, f705])).
fof(f705, plain, (spl34_69 <=> (e3 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_69])])).
fof(f2244, plain, (~ (e3 = op(e2, e1)) | ~ spl34_54), inference(backward_demodulation, [], [f241, f644])).
fof(f241, plain, ~ (op(e2, e1) = op(e2, e4)), inference(cnf_transformation, [], [f4])).
fof(f2240, plain, (~ spl34_37 | ~ spl34_27), inference(avatar_split_clause, [], [f2239, f529, f571])).
fof(f571, plain, (spl34_37 <=> (e1 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_37])])).
fof(f2239, plain, (~ (e1 = op(e3, e2)) | ~ spl34_27), inference(forward_demodulation, [], [f253, f531])).
fof(f253, plain, ~ (op(e3, e2) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f2237, plain, (~ spl34_39 | ~ spl34_49), inference(avatar_split_clause, [], [f2236, f621, f579])).
fof(f579, plain, (spl34_39 <=> (e3 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_39])])).
fof(f2236, plain, (~ (e3 = op(e3, e2)) | ~ spl34_49), inference(forward_demodulation, [], [f246, f623])).
fof(f246, plain, ~ (op(e3, e0) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f2235, plain, (~ spl34_38 | ~ spl34_113), inference(avatar_split_clause, [], [f2234, f890, f575])).
fof(f575, plain, (spl34_38 <=> (e2 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_38])])).
fof(f2234, plain, (~ (e2 = op(e3, e2)) | ~ spl34_113), inference(forward_demodulation, [], [f187, f892])).
fof(f187, plain, ~ (op(e0, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f2163, plain, (~ spl34_68 | ~ spl34_73), inference(avatar_split_clause, [], [f2161, f722, f701])).
fof(f2161, plain, (~ (e2 = op(e2, e1)) | ~ spl34_73), inference(backward_demodulation, [], [f235, f724])).
fof(f235, plain, ~ (op(e2, e0) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f2162, plain, (~ spl34_48 | ~ spl34_73), inference(avatar_split_clause, [], [f2160, f722, f617])).
fof(f2160, plain, (~ (e2 = op(e3, e0)) | ~ spl34_73), inference(backward_demodulation, [], [f172, f724])).
fof(f172, plain, ~ (op(e2, e0) = op(e3, e0)), inference(cnf_transformation, [], [f4])).
fof(f2140, plain, (~ spl34_82 | ~ spl34_97), inference(avatar_split_clause, [], [f2134, f823, f760])).
fof(f2134, plain, (~ (e1 = op(e1, e3)) | ~ spl34_97), inference(backward_demodulation, [], [f227, f825])).
fof(f227, plain, ~ (op(e1, e0) = op(e1, e3)), inference(cnf_transformation, [], [f4])).
fof(f2139, plain, (~ spl34_87 | ~ spl34_97), inference(avatar_split_clause, [], [f2133, f823, f781])).
fof(f781, plain, (spl34_87 <=> (e1 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_87])])).
fof(f2133, plain, (~ (e1 = op(e1, e2)) | ~ spl34_97), inference(backward_demodulation, [], [f226, f825])).
fof(f2116, plain, (~ spl34_67 | ~ spl34_117), inference(avatar_split_clause, [], [f2110, f907, f697])).
fof(f697, plain, (spl34_67 <=> (e1 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_67])])).
fof(f2110, plain, (~ (e1 = op(e2, e1)) | ~ spl34_117), inference(backward_demodulation, [], [f176, f909])).
fof(f176, plain, ~ (op(e0, e1) = op(e2, e1)), inference(cnf_transformation, [], [f4])).
fof(f2103, plain, (~ spl34_115 | ~ spl34_65), inference(avatar_split_clause, [], [f2102, f688, f898])).
fof(f898, plain, (spl34_115 <=> (e4 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_115])])).
fof(f2102, plain, (~ (e4 = op(e0, e2)) | ~ spl34_65), inference(forward_demodulation, [], [f186, f690])).
fof(f186, plain, ~ (op(e0, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f2099, plain, (~ spl34_114 | ~ spl34_14), inference(avatar_split_clause, [], [f2098, f474, f894])).
fof(f894, plain, (spl34_114 <=> (e3 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_114])])).
fof(f474, plain, (spl34_14 <=> (e3 = op(e4, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_14])])).
fof(f2098, plain, (~ (e3 = op(e0, e2)) | ~ spl34_14), inference(forward_demodulation, [], [f188, f476])).
fof(f476, plain, ((e3 = op(e4, e2)) | ~ spl34_14), inference(avatar_component_clause, [], [f474])).
fof(f2080, plain, (~ spl34_90 | ~ spl34_65), inference(avatar_split_clause, [], [f2079, f688, f793])).
fof(f793, plain, (spl34_90 <=> (e4 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_90])])).
fof(f2079, plain, (~ (e4 = op(e1, e2)) | ~ spl34_65), inference(forward_demodulation, [], [f189, f690])).
fof(f189, plain, ~ (op(e1, e2) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f2061, plain, (~ spl34_76 | ~ spl34_1), inference(avatar_split_clause, [], [f2060, f420, f735])).
fof(f735, plain, (spl34_76 <=> (e0 = op(e1, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_76])])).
fof(f2060, plain, (~ (e0 = op(e1, e4)) | ~ spl34_1), inference(forward_demodulation, [], [f211, f422])).
fof(f211, plain, ~ (op(e1, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2051, plain, (~ spl34_75 | ~ spl34_65), inference(avatar_split_clause, [], [f2050, f688, f730])).
fof(f730, plain, (spl34_75 <=> (e4 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_75])])).
fof(f2050, plain, (~ (e4 = op(e2, e0)) | ~ spl34_65), inference(forward_demodulation, [], [f236, f690])).
fof(f236, plain, ~ (op(e2, e0) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f2043, plain, (~ spl34_70 | ~ spl34_65), inference(avatar_split_clause, [], [f2042, f688, f709])).
fof(f709, plain, (spl34_70 <=> (e4 = op(e2, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_70])])).
fof(f2042, plain, (~ (e4 = op(e2, e1)) | ~ spl34_65), inference(forward_demodulation, [], [f239, f690])).
fof(f239, plain, ~ (op(e2, e1) = op(e2, e2)), inference(cnf_transformation, [], [f4])).
fof(f2037, plain, (~ spl34_52 | ~ spl34_54), inference(avatar_contradiction_clause, [], [f2036])).
fof(f2036, plain, ($false | (~ spl34_52 | ~ spl34_54)), inference(subsumption_resolution, [], [f2035, f270])).
fof(f2035, plain, ((e1 = e3) | (~ spl34_52 | ~ spl34_54)), inference(forward_demodulation, [], [f644, f636])).
fof(f636, plain, ((e1 = op(e2, e4)) | ~ spl34_52), inference(avatar_component_clause, [], [f634])).
fof(f2020, plain, (~ spl34_23 | ~ spl34_25), inference(avatar_contradiction_clause, [], [f2019])).
fof(f2019, plain, ($false | (~ spl34_23 | ~ spl34_25)), inference(subsumption_resolution, [], [f2018, f273])).
fof(f2018, plain, ((e2 = e4) | (~ spl34_23 | ~ spl34_25)), inference(forward_demodulation, [], [f522, f514])).
fof(f2008, plain, (~ spl34_6 | ~ spl34_1), inference(avatar_split_clause, [], [f2007, f420, f441])).
fof(f441, plain, (spl34_6 <=> (e0 = op(e4, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_6])])).
fof(f2007, plain, (~ (e0 = op(e4, e3)) | ~ spl34_1), inference(forward_demodulation, [], [f264, f422])).
fof(f264, plain, ~ (op(e4, e3) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f2000, plain, (~ spl34_8 | ~ spl34_9), inference(avatar_contradiction_clause, [], [f1999])).
fof(f1999, plain, ($false | (~ spl34_8 | ~ spl34_9)), inference(subsumption_resolution, [], [f1998, f272])).
fof(f1998, plain, ((e2 = e3) | (~ spl34_8 | ~ spl34_9)), inference(backward_demodulation, [], [f455, f451])).
fof(f455, plain, ((e3 = op(e4, e3)) | ~ spl34_9), inference(avatar_component_clause, [], [f453])).
fof(f1989, plain, (~ spl34_12 | ~ spl34_14), inference(avatar_contradiction_clause, [], [f1988])).
fof(f1988, plain, ($false | (~ spl34_12 | ~ spl34_14)), inference(subsumption_resolution, [], [f1987, f270])).
fof(f1987, plain, ((e1 = e3) | (~ spl34_12 | ~ spl34_14)), inference(backward_demodulation, [], [f476, f468])).
fof(f1979, plain, (~ spl34_18 | ~ spl34_19), inference(avatar_contradiction_clause, [], [f1978])).
fof(f1978, plain, ($false | (~ spl34_18 | ~ spl34_19)), inference(subsumption_resolution, [], [f1977, f272])).
fof(f1977, plain, ((e2 = e3) | (~ spl34_18 | ~ spl34_19)), inference(backward_demodulation, [], [f497, f493])).
fof(f493, plain, ((e2 = op(e4, e1)) | ~ spl34_18), inference(avatar_component_clause, [], [f491])).
fof(f491, plain, (spl34_18 <=> (e2 = op(e4, e1))), introduced(avatar_definition, [new_symbols(naming, [spl34_18])])).
fof(f1976, plain, (~ spl34_19 | ~ spl34_20), inference(avatar_contradiction_clause, [], [f1975])).
fof(f1975, plain, ($false | (~ spl34_19 | ~ spl34_20)), inference(subsumption_resolution, [], [f1974, f274])).
fof(f1974, plain, ((e3 = e4) | (~ spl34_19 | ~ spl34_20)), inference(backward_demodulation, [], [f501, f497])).
fof(f501, plain, ((e4 = op(e4, e1)) | ~ spl34_20), inference(avatar_component_clause, [], [f499])).
fof(f1961, plain, (~ spl34_24 | ~ spl34_25), inference(avatar_contradiction_clause, [], [f1960])).
fof(f1960, plain, ($false | (~ spl34_24 | ~ spl34_25)), inference(subsumption_resolution, [], [f1959, f274])).
fof(f1959, plain, ((e3 = e4) | (~ spl34_24 | ~ spl34_25)), inference(backward_demodulation, [], [f522, f518])).
fof(f1943, plain, (~ spl34_27 | ~ spl34_32), inference(avatar_split_clause, [], [f1941, f550, f529])).
fof(f550, plain, (spl34_32 <=> (e1 = op(e3, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_32])])).
fof(f1941, plain, (~ (e1 = op(e3, e4)) | ~ spl34_32), inference(backward_demodulation, [], [f254, f552])).
fof(f552, plain, ((e1 = op(e3, e3)) | ~ spl34_32), inference(avatar_component_clause, [], [f550])).
fof(f254, plain, ~ (op(e3, e3) = op(e3, e4)), inference(cnf_transformation, [], [f4])).
fof(f1912, plain, (~ spl34_53 | ~ spl34_54), inference(avatar_contradiction_clause, [], [f1911])).
fof(f1911, plain, ($false | (~ spl34_53 | ~ spl34_54)), inference(subsumption_resolution, [], [f1910, f272])).
fof(f1910, plain, ((e2 = e3) | (~ spl34_53 | ~ spl34_54)), inference(backward_demodulation, [], [f644, f640])).
fof(f640, plain, ((e2 = op(e2, e4)) | ~ spl34_53), inference(avatar_component_clause, [], [f638])).
fof(f638, plain, (spl34_53 <=> (e2 = op(e2, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_53])])).
fof(f1909, plain, (~ spl34_4 | ~ spl34_54), inference(avatar_split_clause, [], [f1907, f642, f432])).
fof(f432, plain, (spl34_4 <=> (e3 = op(e4, e4))), introduced(avatar_definition, [new_symbols(naming, [spl34_4])])).
fof(f1907, plain, (~ (e3 = op(e4, e4)) | ~ spl34_54), inference(backward_demodulation, [], [f213, f644])).
fof(f213, plain, ~ (op(e2, e4) = op(e4, e4)), inference(cnf_transformation, [], [f4])).
fof(f1899, plain, (spl34_27 | ~ spl34_65), inference(avatar_split_clause, [], [f1892, f688, f529])).
fof(f1892, plain, ((e1 = op(e3, e4)) | ~ spl34_65), inference(backward_demodulation, [], [f1016, f690])).
fof(f1016, plain, (e1 = op(e3, op(e2, e2))), inference(forward_demodulation, [], [f276, f277])).
fof(f277, plain, (e3 = op(e2, op(e2, e2))), inference(cnf_transformation, [], [f6])).
fof(f6, plain, ((e4 = op(e2, e2)) & (e3 = op(e2, op(e2, e2))) & (e1 = op(op(e2, op(e2, e2)), op(e2, e2))) & (e0 = op(op(e2, e2), op(e2, e2)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG057+1.p', ax6)).
fof(f276, plain, (e1 = op(op(e2, op(e2, e2)), op(e2, e2))), inference(cnf_transformation, [], [f6])).
fof(f1898, plain, (spl34_54 | ~ spl34_65), inference(avatar_split_clause, [], [f1891, f688, f642])).
fof(f1891, plain, ((e3 = op(e2, e4)) | ~ spl34_65), inference(backward_demodulation, [], [f277, f690])).
fof(f1897, plain, (spl34_1 | ~ spl34_65), inference(avatar_split_clause, [], [f1890, f688, f420])).
fof(f1890, plain, ((e0 = op(e4, e4)) | ~ spl34_65), inference(backward_demodulation, [], [f275, f690])).
fof(f275, plain, (e0 = op(op(e2, e2), op(e2, e2))), inference(cnf_transformation, [], [f6])).
fof(f1893, plain, (~ spl34_40 | ~ spl34_65), inference(avatar_split_clause, [], [f1886, f688, f583])).
fof(f583, plain, (spl34_40 <=> (e4 = op(e3, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_40])])).
fof(f1886, plain, (~ (e4 = op(e3, e2)) | ~ spl34_65), inference(backward_demodulation, [], [f192, f690])).
fof(f192, plain, ~ (op(e2, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f1854, plain, (~ spl34_80 | spl34_211), inference(avatar_contradiction_clause, [], [f1853])).
fof(f1853, plain, ($false | (~ spl34_80 | spl34_211)), inference(subsumption_resolution, [], [f1848, f753])).
fof(f753, plain, ((e4 = op(e1, e4)) | ~ spl34_80), inference(avatar_component_clause, [], [f751])).
fof(f1848, plain, (~ (e4 = op(e1, e4)) | (~ spl34_80 | spl34_211)), inference(backward_demodulation, [], [f1413, f753])).
fof(f1413, plain, (~ (e4 = op(e1, op(e1, e4))) | spl34_211), inference(avatar_component_clause, [], [f1411])).
fof(f1411, plain, (spl34_211 <=> (e4 = op(e1, op(e1, e4)))), introduced(avatar_definition, [new_symbols(naming, [spl34_211])])).
fof(f1826, plain, (~ spl34_36 | ~ spl34_86), inference(avatar_split_clause, [], [f1819, f777, f567])).
fof(f777, plain, (spl34_86 <=> (e0 = op(e1, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_86])])).
fof(f1819, plain, (~ (e0 = op(e3, e2)) | ~ spl34_86), inference(backward_demodulation, [], [f190, f779])).
fof(f779, plain, ((e0 = op(e1, e2)) | ~ spl34_86), inference(avatar_component_clause, [], [f777])).
fof(f190, plain, ~ (op(e1, e2) = op(e3, e2)), inference(cnf_transformation, [], [f4])).
fof(f1805, plain, (~ spl34_96 | spl34_234), inference(avatar_contradiction_clause, [], [f1804])).
fof(f1804, plain, ($false | (~ spl34_96 | spl34_234)), inference(subsumption_resolution, [], [f1793, f821])).
fof(f1793, plain, (~ (e0 = op(e1, e0)) | (~ spl34_96 | spl34_234)), inference(backward_demodulation, [], [f1521, f821])).
fof(f1521, plain, (~ (e0 = op(e1, op(e1, e0))) | spl34_234), inference(avatar_component_clause, [], [f1519])).
fof(f1519, plain, (spl34_234 <=> (e0 = op(e1, op(e1, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_234])])).
fof(f1798, plain, (~ spl34_86 | ~ spl34_96), inference(avatar_split_clause, [], [f1789, f819, f777])).
fof(f1789, plain, (~ (e0 = op(e1, e2)) | ~ spl34_96), inference(backward_demodulation, [], [f226, f821])).
fof(f1794, plain, (~ spl34_71 | ~ spl34_96), inference(avatar_split_clause, [], [f1785, f819, f714])).
fof(f714, plain, (spl34_71 <=> (e0 = op(e2, e0))), introduced(avatar_definition, [new_symbols(naming, [spl34_71])])).
fof(f1785, plain, (~ (e0 = op(e2, e0)) | ~ spl34_96), inference(backward_demodulation, [], [f169, f821])).
fof(f169, plain, ~ (op(e1, e0) = op(e2, e0)), inference(cnf_transformation, [], [f4])).
fof(f1741, plain, (~ spl34_111 | ~ spl34_116), inference(avatar_split_clause, [], [f1732, f903, f882])).
fof(f882, plain, (spl34_111 <=> (e0 = op(e0, e2))), introduced(avatar_definition, [new_symbols(naming, [spl34_111])])).
fof(f1732, plain, (~ (e0 = op(e0, e2)) | ~ spl34_116), inference(backward_demodulation, [], [f219, f905])).
fof(f1738, plain, (~ spl34_66 | ~ spl34_116), inference(avatar_split_clause, [], [f1729, f903, f693])).
fof(f1729, plain, (~ (e0 = op(e2, e1)) | ~ spl34_116), inference(backward_demodulation, [], [f176, f905])).
fof(f1726, plain, (~ spl34_106 | ~ spl34_121), inference(avatar_split_clause, [], [f1718, f924, f861])).
fof(f861, plain, (spl34_106 <=> (e0 = op(e0, e3))), introduced(avatar_definition, [new_symbols(naming, [spl34_106])])).
fof(f1718, plain, (~ (e0 = op(e0, e3)) | ~ spl34_121), inference(backward_demodulation, [], [f217, f926])).
fof(f926, plain, ((e0 = op(e0, e0)) | ~ spl34_121), inference(avatar_component_clause, [], [f924])).
fof(f1682, plain, (spl34_235 | spl34_232 | spl34_258 | ~ spl34_262 | spl34_260), inference(avatar_split_clause, [], [f376, f1669, f1679, f1661, f1510, f1524])).
fof(f1524, plain, (spl34_235 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl34_235])])).
fof(f1510, plain, (spl34_232 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl34_232])])).
fof(f376, plain, ((e4 = op(e0, op(e4, e0))) | ~ (e0 = op(e3, op(e3, e0))) | (e2 = op(e0, op(e2, e0))) | sP1 | sP0), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10) & (((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | sP9 | sP8) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | sP7 | sP6) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | sP5 | sP4) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | sP3 | sP2) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | sP1 | sP0)), inference(definition_folding, [], [f9, e43, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21, e20, e19, e18, e17, e16, e15, e14, e13, e12, e11, e10])).
fof(f10, plain, (((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))) | ~ sP0), inference(usedef, [], [e10])).
fof(e10, plain, (sP0 <=> ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f11, plain, (((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ~ sP1), inference(usedef, [], [e11])).
fof(e11, plain, (sP1 <=> ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f12, plain, (((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1)))) | ~ sP2), inference(usedef, [], [e12])).
fof(e12, plain, (sP2 <=> ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f13, plain, (((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ~ sP3), inference(usedef, [], [e13])).
fof(e13, plain, (sP3 <=> ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f14, plain, (((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2)))) | ~ sP4), inference(usedef, [], [e14])).
fof(e14, plain, (sP4 <=> ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f15, plain, (((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ~ sP5), inference(usedef, [], [e15])).
fof(e15, plain, (sP5 <=> ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f16, plain, (((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3)))) | ~ sP6), inference(usedef, [], [e16])).
fof(e16, plain, (sP6 <=> ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f17, plain, (((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ~ sP7), inference(usedef, [], [e17])).
fof(e17, plain, (sP7 <=> ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f18, plain, (((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4)))) | ~ sP8), inference(usedef, [], [e18])).
fof(e18, plain, (sP8 <=> ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f19, plain, (((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ~ sP9), inference(usedef, [], [e19])).
fof(e19, plain, (sP9 <=> ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f20, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP10), inference(usedef, [], [e20])).
fof(e20, plain, (sP10 <=> (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f21, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP11), inference(usedef, [], [e21])).
fof(e21, plain, (sP11 <=> (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f22, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP12), inference(usedef, [], [e22])).
fof(e22, plain, (sP12 <=> (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f23, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP13), inference(usedef, [], [e23])).
fof(e23, plain, (sP13 <=> (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f24, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP14), inference(usedef, [], [e24])).
fof(e24, plain, (sP14 <=> (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f25, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP15), inference(usedef, [], [e25])).
fof(e25, plain, (sP15 <=> (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f26, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP16), inference(usedef, [], [e26])).
fof(e26, plain, (sP16 <=> (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f27, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP17), inference(usedef, [], [e27])).
fof(e27, plain, (sP17 <=> (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f28, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP18), inference(usedef, [], [e28])).
fof(e28, plain, (sP18 <=> (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f29, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP19), inference(usedef, [], [e29])).
fof(e29, plain, (sP19 <=> (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f30, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP20), inference(usedef, [], [e30])).
fof(e30, plain, (sP20 <=> (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f31, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP21), inference(usedef, [], [e31])).
fof(e31, plain, (sP21 <=> (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f32, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP22), inference(usedef, [], [e32])).
fof(e32, plain, (sP22 <=> (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f33, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP23), inference(usedef, [], [e33])).
fof(e33, plain, (sP23 <=> (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f34, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP24), inference(usedef, [], [e34])).
fof(e34, plain, (sP24 <=> (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f35, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP25), inference(usedef, [], [e35])).
fof(e35, plain, (sP25 <=> (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f36, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP26), inference(usedef, [], [e36])).
fof(e36, plain, (sP26 <=> (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f37, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP27), inference(usedef, [], [e37])).
fof(e37, plain, (sP27 <=> (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f38, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP28), inference(usedef, [], [e38])).
fof(e38, plain, (sP28 <=> (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f39, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP29), inference(usedef, [], [e39])).
fof(e39, plain, (sP29 <=> (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f40, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP30), inference(usedef, [], [e40])).
fof(e40, plain, (sP30 <=> (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f41, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP31), inference(usedef, [], [e41])).
fof(e41, plain, (sP31 <=> (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f42, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP32), inference(usedef, [], [e42])).
fof(e42, plain, (sP32 <=> (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f43, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP33), inference(usedef, [], [e43])).
fof(e43, plain, (sP33 <=> (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f9, plain, (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & (((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))))), inference(flattening, [], [f8])).
fof(f8, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & (((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))))), inference(negated_conjecture, [], [f7])).
fof(f7, plain, ~ ~ (((~ (e4 = op(op(e4, e4), e4)) & (e4 = op(op(e4, e4), e4)) & ~ (op(e4, e4) = op(e4, e4))) | (~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | (~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | (~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | (~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | (~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | (~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | (~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | (~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | (~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | (~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | (~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | (~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | (~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | (~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0)))) & (((e4 = op(e4, op(e4, e4))) & ~ (e4 = op(e4, op(e4, e4)))) | ((e3 = op(e4, op(e3, e4))) & ~ (e4 = op(e3, op(e3, e4)))) | ((e2 = op(e4, op(e2, e4))) & ~ (e4 = op(e2, op(e2, e4)))) | ((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4))))) & (((e4 = op(e3, op(e4, e3))) & ~ (e3 = op(e4, op(e4, e3)))) | ((e3 = op(e3, op(e3, e3))) & ~ (e3 = op(e3, op(e3, e3)))) | ((e2 = op(e3, op(e2, e3))) & ~ (e3 = op(e2, op(e2, e3)))) | ((e1 = op(e3, op(e1, e3))) & ~ (e3 = op(e1, op(e1, e3)))) | ((e0 = op(e3, op(e0, e3))) & ~ (e3 = op(e0, op(e0, e3))))) & (((e4 = op(e2, op(e4, e2))) & ~ (e2 = op(e4, op(e4, e2)))) | ((e3 = op(e2, op(e3, e2))) & ~ (e2 = op(e3, op(e3, e2)))) | ((e2 = op(e2, op(e2, e2))) & ~ (e2 = op(e2, op(e2, e2)))) | ((e1 = op(e2, op(e1, e2))) & ~ (e2 = op(e1, op(e1, e2)))) | ((e0 = op(e2, op(e0, e2))) & ~ (e2 = op(e0, op(e0, e2))))) & (((e4 = op(e1, op(e4, e1))) & ~ (e1 = op(e4, op(e4, e1)))) | ((e3 = op(e1, op(e3, e1))) & ~ (e1 = op(e3, op(e3, e1)))) | ((e2 = op(e1, op(e2, e1))) & ~ (e1 = op(e2, op(e2, e1)))) | ((e1 = op(e1, op(e1, e1))) & ~ (e1 = op(e1, op(e1, e1)))) | ((e0 = op(e1, op(e0, e1))) & ~ (e1 = op(e0, op(e0, e1))))) & (((e4 = op(e0, op(e4, e0))) & ~ (e0 = op(e4, op(e4, e0)))) | ((e3 = op(e0, op(e3, e0))) & ~ (e0 = op(e3, op(e3, e0)))) | ((e2 = op(e0, op(e2, e0))) & ~ (e0 = op(e2, op(e2, e0)))) | ((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG057+1.p', co1)).
fof(f1564, plain, (spl34_212 | spl34_209 | spl34_238 | spl34_239 | ~ spl34_240), inference(avatar_split_clause, [], [f406, f1549, f1545, f1541, f1402, f1416])).
fof(f1416, plain, (spl34_212 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl34_212])])).
fof(f1402, plain, (spl34_209 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl34_209])])).
fof(f1549, plain, (spl34_240 <=> (e4 = op(e4, op(e4, e4)))), introduced(avatar_definition, [new_symbols(naming, [spl34_240])])).
fof(f406, plain, (~ (e4 = op(e4, op(e4, e4))) | (e3 = op(e4, op(e3, e4))) | (e2 = op(e4, op(e2, e4))) | sP9 | sP8), inference(cnf_transformation, [], [f44])).
fof(f1552, plain, (spl34_212 | spl34_209 | spl34_238 | spl34_239 | spl34_240), inference(avatar_split_clause, [], [f410, f1549, f1545, f1541, f1402, f1416])).
fof(f410, plain, ((e4 = op(e4, op(e4, e4))) | (e3 = op(e4, op(e3, e4))) | (e2 = op(e4, op(e2, e4))) | sP9 | sP8), inference(cnf_transformation, [], [f44])).
fof(f1539, plain, (spl34_207 | spl34_204 | spl34_201 | spl34_198 | spl34_195 | spl34_191 | spl34_189 | spl34_186 | spl34_183 | spl34_180 | spl34_176 | spl34_172 | spl34_170 | spl34_167 | spl34_164 | spl34_160 | spl34_156 | spl34_152 | spl34_150 | spl34_147 | spl34_143 | spl34_139 | spl34_135 | spl34_131), inference(avatar_split_clause, [], [f414, f1018, f1037, f1056, f1075, f1094, f1109, f1120, f1139, f1158, f1177, f1192, f1207, f1218, f1237, f1256, f1271, f1286, f1301, f1312, f1331, f1346, f1361, f1376, f1391])).
fof(f1391, plain, (spl34_207 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl34_207])])).
fof(f1376, plain, (spl34_204 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl34_204])])).
fof(f1361, plain, (spl34_201 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl34_201])])).
fof(f1346, plain, (spl34_198 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl34_198])])).
fof(f1331, plain, (spl34_195 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl34_195])])).
fof(f1312, plain, (spl34_191 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl34_191])])).
fof(f1301, plain, (spl34_189 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl34_189])])).
fof(f1286, plain, (spl34_186 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl34_186])])).
fof(f1271, plain, (spl34_183 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl34_183])])).
fof(f1256, plain, (spl34_180 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl34_180])])).
fof(f1237, plain, (spl34_176 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl34_176])])).
fof(f1218, plain, (spl34_172 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl34_172])])).
fof(f1207, plain, (spl34_170 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl34_170])])).
fof(f1192, plain, (spl34_167 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl34_167])])).
fof(f1177, plain, (spl34_164 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl34_164])])).
fof(f1158, plain, (spl34_160 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl34_160])])).
fof(f1139, plain, (spl34_156 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl34_156])])).
fof(f1120, plain, (spl34_152 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl34_152])])).
fof(f1109, plain, (spl34_150 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl34_150])])).
fof(f1094, plain, (spl34_147 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl34_147])])).
fof(f1075, plain, (spl34_143 <=> sP30), introduced(avatar_definition, [new_symbols(naming, [spl34_143])])).
fof(f1056, plain, (spl34_139 <=> sP31), introduced(avatar_definition, [new_symbols(naming, [spl34_139])])).
fof(f1037, plain, (spl34_135 <=> sP32), introduced(avatar_definition, [new_symbols(naming, [spl34_135])])).
fof(f1018, plain, (spl34_131 <=> sP33), introduced(avatar_definition, [new_symbols(naming, [spl34_131])])).
fof(f414, plain, (sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10), inference(trivial_inequality_removal, [], [f411])).
fof(f411, plain, (~ (op(e4, e4) = op(e4, e4)) | sP33 | sP32 | sP31 | sP30 | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15 | sP14 | sP13 | sP12 | sP11 | sP10), inference(cnf_transformation, [], [f44])).
fof(f1532, plain, (~ spl34_235 | ~ spl34_236), inference(avatar_split_clause, [], [f369, f1528, f1524])).
fof(f1528, plain, (spl34_236 <=> (e0 = op(e0, op(e0, e0)))), introduced(avatar_definition, [new_symbols(naming, [spl34_236])])).
fof(f369, plain, (~ (e0 = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f78])).
fof(f78, plain, (((e0 = op(e0, op(e0, e0))) & ~ (e0 = op(e0, op(e0, e0)))) | ~ sP0), inference(nnf_transformation, [], [f10])).
fof(f1531, plain, (~ spl34_235 | spl34_236), inference(avatar_split_clause, [], [f370, f1528, f1524])).
fof(f370, plain, ((e0 = op(e0, op(e0, e0))) | ~ sP0), inference(cnf_transformation, [], [f78])).
fof(f1522, plain, (~ spl34_232 | ~ spl34_234), inference(avatar_split_clause, [], [f367, f1519, f1510])).
fof(f367, plain, (~ (e0 = op(e1, op(e1, e0))) | ~ sP1), inference(cnf_transformation, [], [f77])).
fof(f77, plain, (((e1 = op(e0, op(e1, e0))) & ~ (e0 = op(e1, op(e1, e0)))) | ~ sP1), inference(nnf_transformation, [], [f11])).
fof(f1423, plain, (~ spl34_212 | spl34_213), inference(avatar_split_clause, [], [f354, f1420, f1416])).
fof(f354, plain, ((e0 = op(e4, op(e0, e4))) | ~ sP8), inference(cnf_transformation, [], [f70])).
fof(f70, plain, (((e0 = op(e4, op(e0, e4))) & ~ (e4 = op(e0, op(e0, e4)))) | ~ sP8), inference(nnf_transformation, [], [f18])).
fof(f1414, plain, (~ spl34_209 | ~ spl34_211), inference(avatar_split_clause, [], [f351, f1411, f1402])).
fof(f351, plain, (~ (e4 = op(e1, op(e1, e4))) | ~ sP9), inference(cnf_transformation, [], [f69])).
fof(f69, plain, (((e1 = op(e4, op(e1, e4))) & ~ (e4 = op(e1, op(e1, e4)))) | ~ sP9), inference(nnf_transformation, [], [f19])).
fof(f1400, plain, ~ spl34_207, inference(avatar_split_clause, [], [f415, f1391])).
fof(f415, plain, ~ sP10, inference(trivial_inequality_removal, [], [f348])).
fof(f348, plain, (~ (op(e0, e0) = op(e0, e0)) | ~ sP10), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ((~ (e0 = op(op(e0, e0), e0)) & (e0 = op(op(e0, e0), e0)) & ~ (op(e0, e0) = op(e0, e0))) | ~ sP10), inference(nnf_transformation, [], [f20])).
fof(f1389, plain, (~ spl34_204 | ~ spl34_194), inference(avatar_split_clause, [], [f345, f1326, f1376])).
fof(f345, plain, (~ (op(e0, e1) = op(e1, e0)) | ~ sP11), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ((~ (e1 = op(op(e0, e1), e0)) & (e0 = op(op(e0, e1), e1)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP11), inference(nnf_transformation, [], [f21])).
fof(f1374, plain, (~ spl34_201 | ~ spl34_179), inference(avatar_split_clause, [], [f342, f1251, f1361])).
fof(f342, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP12), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((~ (e2 = op(op(e0, e2), e0)) & (e0 = op(op(e0, e2), e2)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP12), inference(nnf_transformation, [], [f22])).
fof(f1359, plain, (~ spl34_198 | ~ spl34_163), inference(avatar_split_clause, [], [f339, f1172, f1346])).
fof(f339, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP13), inference(cnf_transformation, [], [f65])).
fof(f65, plain, ((~ (e3 = op(op(e0, e3), e0)) & (e0 = op(op(e0, e3), e3)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP13), inference(nnf_transformation, [], [f23])).
fof(f1344, plain, (~ spl34_195 | ~ spl34_146), inference(avatar_split_clause, [], [f336, f1089, f1331])).
fof(f336, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP14), inference(cnf_transformation, [], [f64])).
fof(f64, plain, ((~ (e4 = op(op(e0, e4), e0)) & (e0 = op(op(e0, e4), e4)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP14), inference(nnf_transformation, [], [f24])).
fof(f1329, plain, (~ spl34_191 | ~ spl34_194), inference(avatar_split_clause, [], [f333, f1326, f1312])).
fof(f333, plain, (~ (op(e0, e1) = op(e1, e0)) | ~ sP15), inference(cnf_transformation, [], [f63])).
fof(f63, plain, ((~ (e0 = op(op(e1, e0), e1)) & (e1 = op(op(e1, e0), e0)) & ~ (op(e0, e1) = op(e1, e0))) | ~ sP15), inference(nnf_transformation, [], [f25])).
fof(f1310, plain, ~ spl34_189, inference(avatar_split_clause, [], [f416, f1301])).
fof(f416, plain, ~ sP16, inference(trivial_inequality_removal, [], [f330])).
fof(f330, plain, (~ (op(e1, e1) = op(e1, e1)) | ~ sP16), inference(cnf_transformation, [], [f62])).
fof(f62, plain, ((~ (e1 = op(op(e1, e1), e1)) & (e1 = op(op(e1, e1), e1)) & ~ (op(e1, e1) = op(e1, e1))) | ~ sP16), inference(nnf_transformation, [], [f26])).
fof(f1293, plain, (~ spl34_186 | ~ spl34_187), inference(avatar_split_clause, [], [f329, f1290, f1286])).
fof(f329, plain, (~ (e2 = op(op(e1, e2), e1)) | ~ sP17), inference(cnf_transformation, [], [f61])).
fof(f61, plain, ((~ (e2 = op(op(e1, e2), e1)) & (e1 = op(op(e1, e2), e2)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP17), inference(nnf_transformation, [], [f27])).
fof(f1283, plain, (~ spl34_183 | spl34_185), inference(avatar_split_clause, [], [f325, f1280, f1271])).
fof(f325, plain, ((e1 = op(op(e1, e3), e3)) | ~ sP18), inference(cnf_transformation, [], [f60])).
fof(f60, plain, ((~ (e3 = op(op(e1, e3), e1)) & (e1 = op(op(e1, e3), e3)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP18), inference(nnf_transformation, [], [f28])).
fof(f1268, plain, (~ spl34_180 | spl34_182), inference(avatar_split_clause, [], [f322, f1265, f1256])).
fof(f322, plain, ((e1 = op(op(e1, e4), e4)) | ~ sP19), inference(cnf_transformation, [], [f59])).
fof(f59, plain, ((~ (e4 = op(op(e1, e4), e1)) & (e1 = op(op(e1, e4), e4)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP19), inference(nnf_transformation, [], [f29])).
fof(f1254, plain, (~ spl34_176 | ~ spl34_179), inference(avatar_split_clause, [], [f318, f1251, f1237])).
fof(f318, plain, (~ (op(e0, e2) = op(e2, e0)) | ~ sP20), inference(cnf_transformation, [], [f58])).
fof(f58, plain, ((~ (e0 = op(op(e2, e0), e2)) & (e2 = op(op(e2, e0), e0)) & ~ (op(e0, e2) = op(e2, e0))) | ~ sP20), inference(nnf_transformation, [], [f30])).
fof(f1230, plain, (~ spl34_172 | spl34_174), inference(avatar_split_clause, [], [f316, f1227, f1218])).
fof(f316, plain, ((e2 = op(op(e2, e1), e1)) | ~ sP21), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e1 = op(op(e2, e1), e2)) & (e2 = op(op(e2, e1), e1)) & ~ (op(e1, e2) = op(e2, e1))) | ~ sP21), inference(nnf_transformation, [], [f31])).
fof(f1216, plain, ~ spl34_170, inference(avatar_split_clause, [], [f417, f1207])).
fof(f417, plain, ~ sP22, inference(trivial_inequality_removal, [], [f312])).
fof(f312, plain, (~ (op(e2, e2) = op(e2, e2)) | ~ sP22), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e2 = op(op(e2, e2), e2)) & (e2 = op(op(e2, e2), e2)) & ~ (op(e2, e2) = op(e2, e2))) | ~ sP22), inference(nnf_transformation, [], [f32])).
fof(f1199, plain, (~ spl34_167 | ~ spl34_168), inference(avatar_split_clause, [], [f311, f1196, f1192])).
fof(f311, plain, (~ (e3 = op(op(e2, e3), e2)) | ~ sP23), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e3 = op(op(e2, e3), e2)) & (e2 = op(op(e2, e3), e3)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP23), inference(nnf_transformation, [], [f33])).
fof(f1189, plain, (~ spl34_164 | spl34_166), inference(avatar_split_clause, [], [f307, f1186, f1177])).
fof(f307, plain, ((e2 = op(op(e2, e4), e4)) | ~ sP24), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ((~ (e4 = op(op(e2, e4), e2)) & (e2 = op(op(e2, e4), e4)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP24), inference(nnf_transformation, [], [f34])).
fof(f1175, plain, (~ spl34_160 | ~ spl34_163), inference(avatar_split_clause, [], [f303, f1172, f1158])).
fof(f303, plain, (~ (op(e0, e3) = op(e3, e0)) | ~ sP25), inference(cnf_transformation, [], [f53])).
fof(f53, plain, ((~ (e0 = op(op(e3, e0), e3)) & (e3 = op(op(e3, e0), e0)) & ~ (op(e0, e3) = op(e3, e0))) | ~ sP25), inference(nnf_transformation, [], [f35])).
fof(f1146, plain, (~ spl34_156 | ~ spl34_157), inference(avatar_split_clause, [], [f302, f1143, f1139])).
fof(f302, plain, (~ (e1 = op(op(e3, e1), e3)) | ~ sP26), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ((~ (e1 = op(op(e3, e1), e3)) & (e3 = op(op(e3, e1), e1)) & ~ (op(e1, e3) = op(e3, e1))) | ~ sP26), inference(nnf_transformation, [], [f36])).
fof(f1132, plain, (~ spl34_152 | spl34_154), inference(avatar_split_clause, [], [f298, f1129, f1120])).
fof(f298, plain, ((e3 = op(op(e3, e2), e2)) | ~ sP27), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e2 = op(op(e3, e2), e3)) & (e3 = op(op(e3, e2), e2)) & ~ (op(e2, e3) = op(e3, e2))) | ~ sP27), inference(nnf_transformation, [], [f37])).
fof(f1118, plain, ~ spl34_150, inference(avatar_split_clause, [], [f418, f1109])).
fof(f418, plain, ~ sP28, inference(trivial_inequality_removal, [], [f294])).
fof(f294, plain, (~ (op(e3, e3) = op(e3, e3)) | ~ sP28), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e3 = op(op(e3, e3), e3)) & (e3 = op(op(e3, e3), e3)) & ~ (op(e3, e3) = op(e3, e3))) | ~ sP28), inference(nnf_transformation, [], [f38])).
fof(f1106, plain, (~ spl34_147 | spl34_149), inference(avatar_split_clause, [], [f292, f1103, f1094])).
fof(f292, plain, ((e3 = op(op(e3, e4), e4)) | ~ sP29), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e4 = op(op(e3, e4), e3)) & (e3 = op(op(e3, e4), e4)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP29), inference(nnf_transformation, [], [f39])).
fof(f1092, plain, (~ spl34_143 | ~ spl34_146), inference(avatar_split_clause, [], [f288, f1089, f1075])).
fof(f288, plain, (~ (op(e0, e4) = op(e4, e0)) | ~ sP30), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e0 = op(op(e4, e0), e4)) & (e4 = op(op(e4, e0), e0)) & ~ (op(e0, e4) = op(e4, e0))) | ~ sP30), inference(nnf_transformation, [], [f40])).
fof(f1063, plain, (~ spl34_139 | ~ spl34_140), inference(avatar_split_clause, [], [f287, f1060, f1056])).
fof(f287, plain, (~ (e1 = op(op(e4, e1), e4)) | ~ sP31), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e1 = op(op(e4, e1), e4)) & (e4 = op(op(e4, e1), e1)) & ~ (op(e1, e4) = op(e4, e1))) | ~ sP31), inference(nnf_transformation, [], [f41])).
fof(f1044, plain, (~ spl34_135 | ~ spl34_136), inference(avatar_split_clause, [], [f284, f1041, f1037])).
fof(f284, plain, (~ (e2 = op(op(e4, e2), e4)) | ~ sP32), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e2 = op(op(e4, e2), e4)) & (e4 = op(op(e4, e2), e2)) & ~ (op(e2, e4) = op(e4, e2))) | ~ sP32), inference(nnf_transformation, [], [f42])).
fof(f1025, plain, (~ spl34_131 | ~ spl34_132), inference(avatar_split_clause, [], [f281, f1022, f1018])).
fof(f281, plain, (~ (e3 = op(op(e4, e3), e4)) | ~ sP33), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e3 = op(op(e4, e3), e4)) & (e4 = op(op(e4, e3), e3)) & ~ (op(e3, e4) = op(e4, e3))) | ~ sP33), inference(nnf_transformation, [], [f43])).
fof(f1015, plain, spl34_65, inference(avatar_split_clause, [], [f278, f688])).
fof(f278, plain, (e4 = op(e2, e2)), inference(cnf_transformation, [], [f6])).
fof(f1009, plain, (spl34_123 | spl34_98 | spl34_73 | spl34_48 | spl34_23), inference(avatar_split_clause, [], [f120, f512, f617, f722, f827, f932])).
fof(f120, plain, ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e4 = op(e4, e4)) | (e4 = op(e3, e4)) | (e4 = op(e2, e4)) | (e4 = op(e1, e4)) | (e4 = op(e0, e4))) & ((e4 = op(e4, e4)) | (e4 = op(e4, e3)) | (e4 = op(e4, e2)) | (e4 = op(e4, e1)) | (e4 = op(e4, e0))) & ((e3 = op(e4, e4)) | (e3 = op(e3, e4)) | (e3 = op(e2, e4)) | (e3 = op(e1, e4)) | (e3 = op(e0, e4))) & ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))) & ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))) & ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))) & ((e1 = op(e4, e4)) | (e1 = op(e3, e4)) | (e1 = op(e2, e4)) | (e1 = op(e1, e4)) | (e1 = op(e0, e4))) & ((e1 = op(e4, e4)) | (e1 = op(e4, e3)) | (e1 = op(e4, e2)) | (e1 = op(e4, e1)) | (e1 = op(e4, e0))) & ((e0 = op(e4, e4)) | (e0 = op(e3, e4)) | (e0 = op(e2, e4)) | (e0 = op(e1, e4)) | (e0 = op(e0, e4))) & ((e0 = op(e4, e4)) | (e0 = op(e4, e3)) | (e0 = op(e4, e2)) | (e0 = op(e4, e1)) | (e0 = op(e4, e0))) & ((e4 = op(e4, e3)) | (e4 = op(e3, e3)) | (e4 = op(e2, e3)) | (e4 = op(e1, e3)) | (e4 = op(e0, e3))) & ((e4 = op(e3, e4)) | (e4 = op(e3, e3)) | (e4 = op(e3, e2)) | (e4 = op(e3, e1)) | (e4 = op(e3, e0))) & ((e3 = op(e4, e3)) | (e3 = op(e3, e3)) | (e3 = op(e2, e3)) | (e3 = op(e1, e3)) | (e3 = op(e0, e3))) & ((e3 = op(e3, e4)) | (e3 = op(e3, e3)) | (e3 = op(e3, e2)) | (e3 = op(e3, e1)) | (e3 = op(e3, e0))) & ((e2 = op(e4, e3)) | (e2 = op(e3, e3)) | (e2 = op(e2, e3)) | (e2 = op(e1, e3)) | (e2 = op(e0, e3))) & ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))) & ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))) & ((e1 = op(e3, e4)) | (e1 = op(e3, e3)) | (e1 = op(e3, e2)) | (e1 = op(e3, e1)) | (e1 = op(e3, e0))) & ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))) & ((e0 = op(e3, e4)) | (e0 = op(e3, e3)) | (e0 = op(e3, e2)) | (e0 = op(e3, e1)) | (e0 = op(e3, e0))) & ((e4 = op(e4, e2)) | (e4 = op(e3, e2)) | (e4 = op(e2, e2)) | (e4 = op(e1, e2)) | (e4 = op(e0, e2))) & ((e4 = op(e2, e4)) | (e4 = op(e2, e3)) | (e4 = op(e2, e2)) | (e4 = op(e2, e1)) | (e4 = op(e2, e0))) & ((e3 = op(e4, e2)) | (e3 = op(e3, e2)) | (e3 = op(e2, e2)) | (e3 = op(e1, e2)) | (e3 = op(e0, e2))) & ((e3 = op(e2, e4)) | (e3 = op(e2, e3)) | (e3 = op(e2, e2)) | (e3 = op(e2, e1)) | (e3 = op(e2, e0))) & ((e2 = op(e4, e2)) | (e2 = op(e3, e2)) | (e2 = op(e2, e2)) | (e2 = op(e1, e2)) | (e2 = op(e0, e2))) & ((e2 = op(e2, e4)) | (e2 = op(e2, e3)) | (e2 = op(e2, e2)) | (e2 = op(e2, e1)) | (e2 = op(e2, e0))) & ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))) & ((e1 = op(e2, e4)) | (e1 = op(e2, e3)) | (e1 = op(e2, e2)) | (e1 = op(e2, e1)) | (e1 = op(e2, e0))) & ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))) & ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))) & ((e4 = op(e4, e1)) | (e4 = op(e3, e1)) | (e4 = op(e2, e1)) | (e4 = op(e1, e1)) | (e4 = op(e0, e1))) & ((e4 = op(e1, e4)) | (e4 = op(e1, e3)) | (e4 = op(e1, e2)) | (e4 = op(e1, e1)) | (e4 = op(e1, e0))) & ((e3 = op(e4, e1)) | (e3 = op(e3, e1)) | (e3 = op(e2, e1)) | (e3 = op(e1, e1)) | (e3 = op(e0, e1))) & ((e3 = op(e1, e4)) | (e3 = op(e1, e3)) | (e3 = op(e1, e2)) | (e3 = op(e1, e1)) | (e3 = op(e1, e0))) & ((e2 = op(e4, e1)) | (e2 = op(e3, e1)) | (e2 = op(e2, e1)) | (e2 = op(e1, e1)) | (e2 = op(e0, e1))) & ((e2 = op(e1, e4)) | (e2 = op(e1, e3)) | (e2 = op(e1, e2)) | (e2 = op(e1, e1)) | (e2 = op(e1, e0))) & ((e1 = op(e4, e1)) | (e1 = op(e3, e1)) | (e1 = op(e2, e1)) | (e1 = op(e1, e1)) | (e1 = op(e0, e1))) & ((e1 = op(e1, e4)) | (e1 = op(e1, e3)) | (e1 = op(e1, e2)) | (e1 = op(e1, e1)) | (e1 = op(e1, e0))) & ((e0 = op(e4, e1)) | (e0 = op(e3, e1)) | (e0 = op(e2, e1)) | (e0 = op(e1, e1)) | (e0 = op(e0, e1))) & ((e0 = op(e1, e4)) | (e0 = op(e1, e3)) | (e0 = op(e1, e2)) | (e0 = op(e1, e1)) | (e0 = op(e1, e0))) & ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)) & ((e4 = op(e0, e4)) | (e4 = op(e0, e3)) | (e4 = op(e0, e2)) | (e4 = op(e0, e1)) | (op(e0, e0) = e4)) & ((e3 = op(e4, e0)) | (e3 = op(e3, e0)) | (e3 = op(e2, e0)) | (e3 = op(e1, e0)) | (op(e0, e0) = e3)) & ((e3 = op(e0, e4)) | (e3 = op(e0, e3)) | (e3 = op(e0, e2)) | (e3 = op(e0, e1)) | (op(e0, e0) = e3)) & ((e2 = op(e4, e0)) | (e2 = op(e3, e0)) | (e2 = op(e2, e0)) | (e2 = op(e1, e0)) | (op(e0, e0) = e2)) & ((e2 = op(e0, e4)) | (e2 = op(e0, e3)) | (e2 = op(e0, e2)) | (e2 = op(e0, e1)) | (op(e0, e0) = e2)) & ((e1 = op(e4, e0)) | (e1 = op(e3, e0)) | (e1 = op(e2, e0)) | (e1 = op(e1, e0)) | (op(e0, e0) = e1)) & ((e1 = op(e0, e4)) | (e1 = op(e0, e3)) | (e1 = op(e0, e2)) | (e1 = op(e0, e1)) | (op(e0, e0) = e1)) & ((e0 = op(e4, e0)) | (e0 = op(e3, e0)) | (e0 = op(e2, e0)) | (e0 = op(e1, e0)) | (e0 = op(e0, e0))) & ((e0 = op(e0, e4)) | (e0 = op(e0, e3)) | (e0 = op(e0, e2)) | (e0 = op(e0, e1)) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG057+1.p', ax3)).
fof(f1005, plain, (spl34_125 | spl34_100 | spl34_75 | spl34_50 | spl34_25), inference(avatar_split_clause, [], [f124, f520, f625, f730, f835, f940])).
fof(f124, plain, ((e4 = op(e4, e0)) | (e4 = op(e3, e0)) | (e4 = op(e2, e0)) | (e4 = op(e1, e0)) | (op(e0, e0) = e4)), inference(cnf_transformation, [], [f3])).
fof(f994, plain, (spl34_71 | spl34_66 | spl34_61 | spl34_56 | spl34_51), inference(avatar_split_clause, [], [f135, f630, f651, f672, f693, f714])).
fof(f135, plain, ((e0 = op(e2, e4)) | (e0 = op(e2, e3)) | (e0 = op(e2, e2)) | (e0 = op(e2, e1)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f3])).
fof(f993, plain, (spl34_111 | spl34_86 | spl34_61 | spl34_36 | spl34_11), inference(avatar_split_clause, [], [f136, f462, f567, f672, f777, f882])).
fof(f136, plain, ((e0 = op(e4, e2)) | (e0 = op(e3, e2)) | (e0 = op(e2, e2)) | (e0 = op(e1, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f3])).
fof(f991, plain, (spl34_112 | spl34_87 | spl34_62 | spl34_37 | spl34_12), inference(avatar_split_clause, [], [f138, f466, f571, f676, f781, f886])).
fof(f138, plain, ((e1 = op(e4, e2)) | (e1 = op(e3, e2)) | (e1 = op(e2, e2)) | (e1 = op(e1, e2)) | (e1 = op(e0, e2))), inference(cnf_transformation, [], [f3])).
fof(f983, plain, (spl34_106 | spl34_81 | spl34_56 | spl34_31 | spl34_6), inference(avatar_split_clause, [], [f146, f441, f546, f651, f756, f861])).
fof(f146, plain, ((e0 = op(e4, e3)) | (e0 = op(e3, e3)) | (e0 = op(e2, e3)) | (e0 = op(e1, e3)) | (e0 = op(e0, e3))), inference(cnf_transformation, [], [f3])).
fof(f981, plain, (spl34_107 | spl34_82 | spl34_57 | spl34_32 | spl34_7), inference(avatar_split_clause, [], [f148, f445, f550, f655, f760, f865])).
fof(f148, plain, ((e1 = op(e4, e3)) | (e1 = op(e3, e3)) | (e1 = op(e2, e3)) | (e1 = op(e1, e3)) | (e1 = op(e0, e3))), inference(cnf_transformation, [], [f3])).
fof(f980, plain, (spl34_48 | spl34_43 | spl34_38 | spl34_33 | spl34_28), inference(avatar_split_clause, [], [f149, f533, f554, f575, f596, f617])).
fof(f149, plain, ((e2 = op(e3, e4)) | (e2 = op(e3, e3)) | (e2 = op(e3, e2)) | (e2 = op(e3, e1)) | (e2 = op(e3, e0))), inference(cnf_transformation, [], [f3])).
fof(f970, plain, (spl34_23 | spl34_18 | spl34_13 | spl34_8 | spl34_3), inference(avatar_split_clause, [], [f159, f428, f449, f470, f491, f512])).
fof(f159, plain, ((e2 = op(e4, e4)) | (e2 = op(e4, e3)) | (e2 = op(e4, e2)) | (e2 = op(e4, e1)) | (e2 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f969, plain, (spl34_103 | spl34_78 | spl34_53 | spl34_28 | spl34_3), inference(avatar_split_clause, [], [f160, f428, f533, f638, f743, f848])).
fof(f160, plain, ((e2 = op(e4, e4)) | (e2 = op(e3, e4)) | (e2 = op(e2, e4)) | (e2 = op(e1, e4)) | (e2 = op(e0, e4))), inference(cnf_transformation, [], [f3])).
fof(f968, plain, (spl34_24 | spl34_19 | spl34_14 | spl34_9 | spl34_4), inference(avatar_split_clause, [], [f161, f432, f453, f474, f495, f516])).
fof(f161, plain, ((e3 = op(e4, e4)) | (e3 = op(e4, e3)) | (e3 = op(e4, e2)) | (e3 = op(e4, e1)) | (e3 = op(e4, e0))), inference(cnf_transformation, [], [f3])).
fof(f964, plain, (spl34_126 | spl34_127 | spl34_128 | spl34_129 | spl34_130), inference(avatar_split_clause, [], [f114, f961, f957, f953, f949, f945])).
fof(f114, plain, ((e4 = unit) | (e3 = unit) | (e2 = unit) | (e1 = unit) | (e0 = unit)), inference(cnf_transformation, [], [f2])).
fof(f901, plain, (spl34_111 | spl34_112 | spl34_113 | spl34_114 | spl34_115), inference(avatar_split_clause, [], [f81, f898, f894, f890, f886, f882])).
fof(f81, plain, ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e4 = op(e4, e4)) | (e3 = op(e4, e4)) | (e2 = op(e4, e4)) | (e1 = op(e4, e4)) | (e0 = op(e4, e4))) & ((e4 = op(e4, e3)) | (e3 = op(e4, e3)) | (e2 = op(e4, e3)) | (e1 = op(e4, e3)) | (e0 = op(e4, e3))) & ((e4 = op(e4, e2)) | (e3 = op(e4, e2)) | (e2 = op(e4, e2)) | (e1 = op(e4, e2)) | (e0 = op(e4, e2))) & ((e4 = op(e4, e1)) | (e3 = op(e4, e1)) | (e2 = op(e4, e1)) | (e1 = op(e4, e1)) | (e0 = op(e4, e1))) & ((e4 = op(e4, e0)) | (e3 = op(e4, e0)) | (e2 = op(e4, e0)) | (e1 = op(e4, e0)) | (e0 = op(e4, e0))) & ((e4 = op(e3, e4)) | (e3 = op(e3, e4)) | (e2 = op(e3, e4)) | (e1 = op(e3, e4)) | (e0 = op(e3, e4))) & ((e4 = op(e3, e3)) | (e3 = op(e3, e3)) | (e2 = op(e3, e3)) | (e1 = op(e3, e3)) | (e0 = op(e3, e3))) & ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))) & ((e4 = op(e3, e1)) | (e3 = op(e3, e1)) | (e2 = op(e3, e1)) | (e1 = op(e3, e1)) | (e0 = op(e3, e1))) & ((e4 = op(e3, e0)) | (e3 = op(e3, e0)) | (e2 = op(e3, e0)) | (e1 = op(e3, e0)) | (e0 = op(e3, e0))) & ((e4 = op(e2, e4)) | (e3 = op(e2, e4)) | (e2 = op(e2, e4)) | (e1 = op(e2, e4)) | (e0 = op(e2, e4))) & ((e4 = op(e2, e3)) | (e3 = op(e2, e3)) | (e2 = op(e2, e3)) | (e1 = op(e2, e3)) | (e0 = op(e2, e3))) & ((e4 = op(e2, e2)) | (e3 = op(e2, e2)) | (e2 = op(e2, e2)) | (e1 = op(e2, e2)) | (e0 = op(e2, e2))) & ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))) & ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))) & ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))) & ((e4 = op(e1, e3)) | (e3 = op(e1, e3)) | (e2 = op(e1, e3)) | (e1 = op(e1, e3)) | (e0 = op(e1, e3))) & ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))) & ((e4 = op(e1, e1)) | (e3 = op(e1, e1)) | (e2 = op(e1, e1)) | (e1 = op(e1, e1)) | (e0 = op(e1, e1))) & ((e4 = op(e1, e0)) | (e3 = op(e1, e0)) | (e2 = op(e1, e0)) | (e1 = op(e1, e0)) | (e0 = op(e1, e0))) & ((e4 = op(e0, e4)) | (e3 = op(e0, e4)) | (e2 = op(e0, e4)) | (e1 = op(e0, e4)) | (e0 = op(e0, e4))) & ((e4 = op(e0, e3)) | (e3 = op(e0, e3)) | (e2 = op(e0, e3)) | (e1 = op(e0, e3)) | (e0 = op(e0, e3))) & ((e4 = op(e0, e2)) | (e3 = op(e0, e2)) | (e2 = op(e0, e2)) | (e1 = op(e0, e2)) | (e0 = op(e0, e2))) & ((e4 = op(e0, e1)) | (e3 = op(e0, e1)) | (e2 = op(e0, e1)) | (e1 = op(e0, e1)) | (e0 = op(e0, e1))) & ((op(e0, e0) = e4) | (op(e0, e0) = e3) | (op(e0, e0) = e2) | (op(e0, e0) = e1) | (e0 = op(e0, e0)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG057+1.p', ax1)).
fof(f796, plain, (spl34_86 | spl34_87 | spl34_88 | spl34_89 | spl34_90), inference(avatar_split_clause, [], [f86, f793, f789, f785, f781, f777])).
fof(f86, plain, ((e4 = op(e1, e2)) | (e3 = op(e1, e2)) | (e2 = op(e1, e2)) | (e1 = op(e1, e2)) | (e0 = op(e1, e2))), inference(cnf_transformation, [], [f1])).
fof(f754, plain, (spl34_76 | spl34_77 | spl34_78 | spl34_79 | spl34_80), inference(avatar_split_clause, [], [f88, f751, f747, f743, f739, f735])).
fof(f88, plain, ((e4 = op(e1, e4)) | (e3 = op(e1, e4)) | (e2 = op(e1, e4)) | (e1 = op(e1, e4)) | (e0 = op(e1, e4))), inference(cnf_transformation, [], [f1])).
fof(f733, plain, (spl34_71 | spl34_72 | spl34_73 | spl34_74 | spl34_75), inference(avatar_split_clause, [], [f89, f730, f726, f722, f718, f714])).
fof(f89, plain, ((e4 = op(e2, e0)) | (e3 = op(e2, e0)) | (e2 = op(e2, e0)) | (e1 = op(e2, e0)) | (e0 = op(e2, e0))), inference(cnf_transformation, [], [f1])).
fof(f712, plain, (spl34_66 | spl34_67 | spl34_68 | spl34_69 | spl34_70), inference(avatar_split_clause, [], [f90, f709, f705, f701, f697, f693])).
fof(f90, plain, ((e4 = op(e2, e1)) | (e3 = op(e2, e1)) | (e2 = op(e2, e1)) | (e1 = op(e2, e1)) | (e0 = op(e2, e1))), inference(cnf_transformation, [], [f1])).
fof(f586, plain, (spl34_36 | spl34_37 | spl34_38 | spl34_39 | spl34_40), inference(avatar_split_clause, [], [f96, f583, f579, f575, f571, f567])).
fof(f96, plain, ((e4 = op(e3, e2)) | (e3 = op(e3, e2)) | (e2 = op(e3, e2)) | (e1 = op(e3, e2)) | (e0 = op(e3, e2))), inference(cnf_transformation, [], [f1])).