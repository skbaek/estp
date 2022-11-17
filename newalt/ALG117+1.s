fof(f3501, plain, $false, inference(avatar_sat_refutation, [], [f517, f568, f602, f722, f727, f732, f737, f738, f743, f748, f750, f753, f821, f974, f991, f1031, f1038, f1041, f1042, f1043, f1044, f1045, f1062, f1075, f1080, f1081, f1084, f1088, f1089, f1105, f1112, f1113, f1119, f1131, f1136, f1137, f1141, f1145, f1160, f1161, f1169, f1170, f1171, f1217, f1231, f1245, f1460, f1850, f1897, f1935, f1939, f1954, f1978, f1990, f2040, f2043, f2047, f2080, f2084, f2091, f2093, f2118, f2128, f2130, f2134, f2161, f2169, f2177, f2205, f2213, f2222, f2237, f2240, f2272, f2280, f2283, f2302, f2304, f2311, f2325, f2327, f2331, f2340, f2369, f2383, f2387, f2436, f2449, f2455, f2518, f2580, f2581, f2606, f2618, f2627, f2630, f2641, f2648, f2669, f2709, f2719, f2725, f2732, f2842, f2847, f2855, f2885, f2888, f2891, f2910, f2915, f2917, f2918, f2988, f2998, f3025, f3026, f3036, f3046, f3075, f3108, f3139, f3167, f3198, f3223, f3251, f3283, f3314, f3347, f3376, f3398, f3430, f3466, f3500])).
fof(f3500, plain, (~ spl20_24 | ~ spl20_88 | ~ spl20_138 | spl20_167), inference(avatar_contradiction_clause, [], [f3499])).
fof(f3499, plain, ($false | (~ spl20_24 | ~ spl20_88 | ~ spl20_138 | spl20_167)), inference(subsumption_resolution, [], [f3498, f854])).
fof(f854, plain, ((e23 = op2(e22, e22)) | ~ spl20_88), inference(avatar_component_clause, [], [f852])).
fof(f852, plain, (spl20_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_88])])).
fof(f3498, plain, (~ (e23 = op2(e22, e22)) | (~ spl20_24 | ~ spl20_138 | spl20_167)), inference(forward_demodulation, [], [f3497, f382])).
fof(f382, plain, (e23 = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, e23) = h4(e12)) & (op2(op2(e23, e23), e23) = h4(e11)) & (op2(e23, op2(e23, e23)) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax17)).
fof(f3497, plain, (~ (op2(e22, e22) = h4(e13)) | (~ spl20_24 | ~ spl20_138 | spl20_167)), inference(forward_demodulation, [], [f3496, f550])).
fof(f550, plain, ((e13 = op1(e12, e12)) | ~ spl20_24), inference(avatar_component_clause, [], [f548])).
fof(f548, plain, (spl20_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_24])])).
fof(f3496, plain, (~ (op2(e22, e22) = h4(op1(e12, e12))) | (~ spl20_138 | spl20_167)), inference(forward_demodulation, [], [f1399, f1215])).
fof(f1215, plain, ((e22 = h4(e12)) | ~ spl20_138), inference(avatar_component_clause, [], [f1214])).
fof(f1214, plain, (spl20_138 <=> (e22 = h4(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_138])])).
fof(f1399, plain, (~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | spl20_167), inference(avatar_component_clause, [], [f1397])).
fof(f1397, plain, (spl20_167 <=> (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_167])])).
fof(f3466, plain, (~ spl20_62 | ~ spl20_164 | spl20_182), inference(avatar_contradiction_clause, [], [f3465])).
fof(f3465, plain, ($false | (~ spl20_62 | ~ spl20_164 | spl20_182)), inference(subsumption_resolution, [], [f3464, f1355])).
fof(f1355, plain, ((e21 = h1(e12)) | ~ spl20_164), inference(avatar_component_clause, [], [f1354])).
fof(f1354, plain, (spl20_164 <=> (e21 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_164])])).
fof(f3464, plain, (~ (e21 = h1(e12)) | (~ spl20_62 | spl20_182)), inference(forward_demodulation, [], [f3463, f1205])).
fof(f1205, plain, (e21 = h4(e11)), inference(forward_demodulation, [], [f1204, f1203])).
fof(f1203, plain, (e21 = op2(h4(e12), e23)), inference(backward_demodulation, [], [f368, f385])).
fof(f385, plain, (op2(e23, e23) = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f368, plain, (e21 = op2(op2(e23, e23), e23)), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e22 = op2(e23, e23)) & (e21 = op2(op2(e23, e23), e23)) & (e20 = op2(e23, op2(e23, e23)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax13)).
fof(f1204, plain, (h4(e11) = op2(h4(e12), e23)), inference(forward_demodulation, [], [f384, f385])).
fof(f384, plain, (op2(op2(e23, e23), e23) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f3463, plain, (~ (h1(e12) = h4(e11)) | (~ spl20_62 | spl20_182)), inference(forward_demodulation, [], [f1459, f712])).
fof(f712, plain, ((op1(e10, e10) = e11) | ~ spl20_62), inference(avatar_component_clause, [], [f710])).
fof(f710, plain, (spl20_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl20_62])])).
fof(f1459, plain, (~ (h1(e12) = h4(op1(e10, e10))) | spl20_182), inference(avatar_component_clause, [], [f1457])).
fof(f1457, plain, (spl20_182 <=> (h1(e12) = h4(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_182])])).
fof(f3430, plain, (~ spl20_60 | ~ spl20_124 | spl20_181), inference(avatar_contradiction_clause, [], [f3429])).
fof(f3429, plain, ($false | (~ spl20_60 | ~ spl20_124 | spl20_181)), inference(subsumption_resolution, [], [f3428, f1007])).
fof(f1007, plain, ((e23 = op2(e20, e21)) | ~ spl20_124), inference(avatar_component_clause, [], [f1005])).
fof(f1005, plain, (spl20_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_124])])).
fof(f3428, plain, (~ (e23 = op2(e20, e21)) | (~ spl20_60 | spl20_181)), inference(forward_demodulation, [], [f3427, f382])).
fof(f3427, plain, (~ (op2(e20, e21) = h4(e13)) | (~ spl20_60 | spl20_181)), inference(forward_demodulation, [], [f1455, f703])).
fof(f703, plain, ((e13 = op1(e10, e11)) | ~ spl20_60), inference(avatar_component_clause, [], [f701])).
fof(f701, plain, (spl20_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_60])])).
fof(f1455, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | spl20_181), inference(avatar_component_clause, [], [f1453])).
fof(f1453, plain, (spl20_181 <=> (op2(e20, e21) = h4(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_181])])).
fof(f3398, plain, (~ spl20_55 | ~ spl20_119 | ~ spl20_138 | spl20_180), inference(avatar_contradiction_clause, [], [f3397])).
fof(f3397, plain, ($false | (~ spl20_55 | ~ spl20_119 | ~ spl20_138 | spl20_180)), inference(subsumption_resolution, [], [f3396, f986])).
fof(f986, plain, ((e22 = op2(e20, e22)) | ~ spl20_119), inference(avatar_component_clause, [], [f984])).
fof(f984, plain, (spl20_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_119])])).
fof(f3396, plain, (~ (e22 = op2(e20, e22)) | (~ spl20_55 | ~ spl20_138 | spl20_180)), inference(forward_demodulation, [], [f3395, f1215])).
fof(f3395, plain, (~ (op2(e20, e22) = h4(e12)) | (~ spl20_55 | ~ spl20_138 | spl20_180)), inference(forward_demodulation, [], [f3394, f682])).
fof(f682, plain, ((e12 = op1(e10, e12)) | ~ spl20_55), inference(avatar_component_clause, [], [f680])).
fof(f680, plain, (spl20_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_55])])).
fof(f3394, plain, (~ (op2(e20, e22) = h4(op1(e10, e12))) | (~ spl20_138 | spl20_180)), inference(forward_demodulation, [], [f1451, f1215])).
fof(f1451, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | spl20_180), inference(avatar_component_clause, [], [f1449])).
fof(f1449, plain, (spl20_180 <=> (h4(op1(e10, e12)) = op2(e20, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_180])])).
fof(f3376, plain, (~ spl20_38 | ~ spl20_102 | ~ spl20_138 | spl20_176), inference(avatar_contradiction_clause, [], [f3375])).
fof(f3375, plain, ($false | (~ spl20_38 | ~ spl20_102 | ~ spl20_138 | spl20_176)), inference(subsumption_resolution, [], [f3374, f914])).
fof(f914, plain, ((e21 = op2(e21, e22)) | ~ spl20_102), inference(avatar_component_clause, [], [f912])).
fof(f912, plain, (spl20_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_102])])).
fof(f3374, plain, (~ (e21 = op2(e21, e22)) | (~ spl20_38 | ~ spl20_138 | spl20_176)), inference(forward_demodulation, [], [f3373, f1205])).
fof(f3373, plain, (~ (op2(e21, e22) = h4(e11)) | (~ spl20_38 | ~ spl20_138 | spl20_176)), inference(forward_demodulation, [], [f3372, f610])).
fof(f610, plain, ((e11 = op1(e11, e12)) | ~ spl20_38), inference(avatar_component_clause, [], [f608])).
fof(f608, plain, (spl20_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_38])])).
fof(f3372, plain, (~ (op2(e21, e22) = h4(op1(e11, e12))) | (~ spl20_138 | spl20_176)), inference(forward_demodulation, [], [f1435, f1215])).
fof(f1435, plain, (~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | spl20_176), inference(avatar_component_clause, [], [f1433])).
fof(f1433, plain, (spl20_176 <=> (h4(op1(e11, e12)) = op2(e21, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_176])])).
fof(f3347, plain, (~ spl20_41 | ~ spl20_156 | spl20_177), inference(avatar_contradiction_clause, [], [f3346])).
fof(f3346, plain, ($false | (~ spl20_41 | ~ spl20_156 | spl20_177)), inference(subsumption_resolution, [], [f3345, f1315])).
fof(f1315, plain, ((e20 = h2(e12)) | ~ spl20_156), inference(avatar_component_clause, [], [f1314])).
fof(f1314, plain, (spl20_156 <=> (e20 = h2(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_156])])).
fof(f3345, plain, (~ (e20 = h2(e12)) | (~ spl20_41 | spl20_177)), inference(forward_demodulation, [], [f3344, f1207])).
fof(f1207, plain, (e20 = h4(e10)), inference(forward_demodulation, [], [f1206, f1202])).
fof(f1202, plain, (e20 = op2(e23, h4(e12))), inference(backward_demodulation, [], [f367, f385])).
fof(f367, plain, (e20 = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f13])).
fof(f1206, plain, (h4(e10) = op2(e23, h4(e12))), inference(forward_demodulation, [], [f383, f385])).
fof(f383, plain, (op2(e23, op2(e23, e23)) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f3344, plain, (~ (h2(e12) = h4(e10)) | (~ spl20_41 | spl20_177)), inference(forward_demodulation, [], [f1439, f623])).
fof(f623, plain, ((e10 = op1(e11, e11)) | ~ spl20_41), inference(avatar_component_clause, [], [f621])).
fof(f621, plain, (spl20_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_41])])).
fof(f1439, plain, (~ (h2(e12) = h4(op1(e11, e11))) | spl20_177), inference(avatar_component_clause, [], [f1437])).
fof(f1437, plain, (spl20_177 <=> (h2(e12) = h4(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_177])])).
fof(f3314, plain, (~ spl20_49 | ~ spl20_113 | spl20_179), inference(avatar_contradiction_clause, [], [f3313])).
fof(f3313, plain, ($false | (~ spl20_49 | ~ spl20_113 | spl20_179)), inference(subsumption_resolution, [], [f3312, f961])).
fof(f961, plain, ((e20 = op2(e20, e23)) | ~ spl20_113), inference(avatar_component_clause, [], [f959])).
fof(f959, plain, (spl20_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_113])])).
fof(f3312, plain, (~ (e20 = op2(e20, e23)) | (~ spl20_49 | spl20_179)), inference(forward_demodulation, [], [f3311, f1207])).
fof(f3311, plain, (~ (op2(e20, e23) = h4(e10)) | (~ spl20_49 | spl20_179)), inference(forward_demodulation, [], [f1447, f657])).
fof(f657, plain, ((e10 = op1(e10, e13)) | ~ spl20_49), inference(avatar_component_clause, [], [f655])).
fof(f655, plain, (spl20_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_49])])).
fof(f1447, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | spl20_179), inference(avatar_component_clause, [], [f1445])).
fof(f1445, plain, (spl20_179 <=> (op2(e20, e23) = h4(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_179])])).
fof(f3283, plain, (~ spl20_29 | ~ spl20_93 | ~ spl20_138 | spl20_174), inference(avatar_contradiction_clause, [], [f3282])).
fof(f3282, plain, ($false | (~ spl20_29 | ~ spl20_93 | ~ spl20_138 | spl20_174)), inference(subsumption_resolution, [], [f3281, f876])).
fof(f876, plain, ((e20 = op2(e22, e20)) | ~ spl20_93), inference(avatar_component_clause, [], [f874])).
fof(f874, plain, (spl20_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_93])])).
fof(f3281, plain, (~ (e20 = op2(e22, e20)) | (~ spl20_29 | ~ spl20_138 | spl20_174)), inference(forward_demodulation, [], [f3280, f1207])).
fof(f3280, plain, (~ (op2(e22, e20) = h4(e10)) | (~ spl20_29 | ~ spl20_138 | spl20_174)), inference(forward_demodulation, [], [f3279, f572])).
fof(f572, plain, ((e10 = op1(e12, e10)) | ~ spl20_29), inference(avatar_component_clause, [], [f570])).
fof(f570, plain, (spl20_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_29])])).
fof(f3279, plain, (~ (op2(e22, e20) = h4(op1(e12, e10))) | (~ spl20_138 | spl20_174)), inference(forward_demodulation, [], [f1427, f1215])).
fof(f1427, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | spl20_174), inference(avatar_component_clause, [], [f1425])).
fof(f1425, plain, (spl20_174 <=> (h4(op1(e12, e10)) = op2(h4(e12), e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_174])])).
fof(f3251, plain, (~ spl20_5 | spl20_169), inference(avatar_contradiction_clause, [], [f3250])).
fof(f3250, plain, ($false | (~ spl20_5 | spl20_169)), inference(subsumption_resolution, [], [f3249, f1207])).
fof(f3249, plain, (~ (e20 = h4(e10)) | (~ spl20_5 | spl20_169)), inference(forward_demodulation, [], [f1407, f470])).
fof(f470, plain, ((e10 = op1(e13, e12)) | ~ spl20_5), inference(avatar_component_clause, [], [f468])).
fof(f468, plain, (spl20_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_5])])).
fof(f1407, plain, (~ (e20 = h4(op1(e13, e12))) | spl20_169), inference(avatar_component_clause, [], [f1405])).
fof(f1405, plain, (spl20_169 <=> (e20 = h4(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_169])])).
fof(f3223, plain, (~ spl20_27 | ~ spl20_91 | ~ spl20_138 | spl20_173), inference(avatar_contradiction_clause, [], [f3222])).
fof(f3222, plain, ($false | (~ spl20_27 | ~ spl20_91 | ~ spl20_138 | spl20_173)), inference(subsumption_resolution, [], [f3221, f867])).
fof(f867, plain, ((e22 = op2(e22, e21)) | ~ spl20_91), inference(avatar_component_clause, [], [f865])).
fof(f865, plain, (spl20_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_91])])).
fof(f3221, plain, (~ (e22 = op2(e22, e21)) | (~ spl20_27 | ~ spl20_138 | spl20_173)), inference(forward_demodulation, [], [f3220, f1215])).
fof(f3220, plain, (~ (op2(e22, e21) = h4(e12)) | (~ spl20_27 | ~ spl20_138 | spl20_173)), inference(forward_demodulation, [], [f3219, f563])).
fof(f563, plain, ((e12 = op1(e12, e11)) | ~ spl20_27), inference(avatar_component_clause, [], [f561])).
fof(f561, plain, (spl20_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_27])])).
fof(f3219, plain, (~ (op2(e22, e21) = h4(op1(e12, e11))) | (~ spl20_138 | spl20_173)), inference(forward_demodulation, [], [f1423, f1215])).
fof(f1423, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | spl20_173), inference(avatar_component_clause, [], [f1421])).
fof(f1421, plain, (spl20_173 <=> (h4(op1(e12, e11)) = op2(h4(e12), e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_173])])).
fof(f3198, plain, (~ spl20_18 | spl20_172), inference(avatar_contradiction_clause, [], [f3197])).
fof(f3197, plain, ($false | (~ spl20_18 | spl20_172)), inference(subsumption_resolution, [], [f3196, f1205])).
fof(f3196, plain, (~ (e21 = h4(e11)) | (~ spl20_18 | spl20_172)), inference(forward_demodulation, [], [f1419, f525])).
fof(f525, plain, ((e11 = op1(e12, e13)) | ~ spl20_18), inference(avatar_component_clause, [], [f523])).
fof(f523, plain, (spl20_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_18])])).
fof(f1419, plain, (~ (e21 = h4(op1(e12, e13))) | spl20_172), inference(avatar_component_clause, [], [f1417])).
fof(f1417, plain, (spl20_172 <=> (e21 = h4(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_172])])).
fof(f3167, plain, (~ spl20_47 | ~ spl20_111 | ~ spl20_138 | spl20_178), inference(avatar_contradiction_clause, [], [f3166])).
fof(f3166, plain, ($false | (~ spl20_47 | ~ spl20_111 | ~ spl20_138 | spl20_178)), inference(subsumption_resolution, [], [f3165, f952])).
fof(f952, plain, ((e22 = op2(e21, e20)) | ~ spl20_111), inference(avatar_component_clause, [], [f950])).
fof(f950, plain, (spl20_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_111])])).
fof(f3165, plain, (~ (e22 = op2(e21, e20)) | (~ spl20_47 | ~ spl20_138 | spl20_178)), inference(forward_demodulation, [], [f3164, f1215])).
fof(f3164, plain, (~ (op2(e21, e20) = h4(e12)) | (~ spl20_47 | spl20_178)), inference(forward_demodulation, [], [f1443, f648])).
fof(f648, plain, ((e12 = op1(e11, e10)) | ~ spl20_47), inference(avatar_component_clause, [], [f646])).
fof(f646, plain, (spl20_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_47])])).
fof(f1443, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | spl20_178), inference(avatar_component_clause, [], [f1441])).
fof(f1441, plain, (spl20_178 <=> (op2(e21, e20) = h4(op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_178])])).
fof(f3139, plain, (~ spl20_10 | ~ spl20_74 | spl20_170), inference(avatar_contradiction_clause, [], [f3138])).
fof(f3138, plain, ($false | (~ spl20_10 | ~ spl20_74 | spl20_170)), inference(subsumption_resolution, [], [f3137, f795])).
fof(f795, plain, ((e21 = op2(e23, e21)) | ~ spl20_74), inference(avatar_component_clause, [], [f793])).
fof(f793, plain, (spl20_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_74])])).
fof(f3137, plain, (~ (e21 = op2(e23, e21)) | (~ spl20_10 | spl20_170)), inference(forward_demodulation, [], [f3136, f1205])).
fof(f3136, plain, (~ (op2(e23, e21) = h4(e11)) | (~ spl20_10 | spl20_170)), inference(forward_demodulation, [], [f1411, f491])).
fof(f491, plain, ((e11 = op1(e13, e11)) | ~ spl20_10), inference(avatar_component_clause, [], [f489])).
fof(f489, plain, (spl20_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_10])])).
fof(f1411, plain, (~ (op2(e23, e21) = h4(op1(e13, e11))) | spl20_170), inference(avatar_component_clause, [], [f1409])).
fof(f1409, plain, (spl20_170 <=> (op2(e23, e21) = h4(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_170])])).
fof(f3108, plain, (~ spl20_3 | spl20_168), inference(avatar_contradiction_clause, [], [f3107])).
fof(f3107, plain, ($false | (~ spl20_3 | spl20_168)), inference(trivial_inequality_removal, [], [f3106])).
fof(f3106, plain, (~ (h4(e12) = h4(e12)) | (~ spl20_3 | spl20_168)), inference(forward_demodulation, [], [f1403, f461])).
fof(f461, plain, ((e12 = op1(e13, e13)) | ~ spl20_3), inference(avatar_component_clause, [], [f459])).
fof(f459, plain, (spl20_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_3])])).
fof(f1403, plain, (~ (h4(e12) = h4(op1(e13, e13))) | spl20_168), inference(avatar_component_clause, [], [f1401])).
fof(f1401, plain, (spl20_168 <=> (h4(e12) = h4(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_168])])).
fof(f3075, plain, (~ spl20_103 | ~ spl20_119), inference(avatar_split_clause, [], [f3072, f984, f916])).
fof(f916, plain, (spl20_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_103])])).
fof(f3072, plain, (~ (e22 = op2(e21, e22)) | ~ spl20_119), inference(backward_demodulation, [], [f220, f986])).
fof(f220, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax6)).
fof(f3046, plain, (~ spl20_118 | ~ spl20_164), inference(avatar_split_clause, [], [f3039, f1354, f980])).
fof(f980, plain, (spl20_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_118])])).
fof(f3039, plain, (~ (e21 = op2(e20, e22)) | ~ spl20_164), inference(backward_demodulation, [], [f1176, f1355])).
fof(f1176, plain, ~ (op2(e20, e22) = h1(e12)), inference(backward_demodulation, [], [f233, f373])).
fof(f373, plain, (op2(e20, e20) = h1(e12)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(e20, e20) = h1(e12)) & (h1(e11) = op2(op2(e20, e20), e20)) & (h1(e10) = op2(e20, op2(e20, e20))) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax14)).
fof(f233, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f3036, plain, (~ spl20_16 | ~ spl20_80 | spl20_171), inference(avatar_contradiction_clause, [], [f3035])).
fof(f3035, plain, ($false | (~ spl20_16 | ~ spl20_80 | spl20_171)), inference(subsumption_resolution, [], [f3034, f820])).
fof(f820, plain, ((e23 = op2(e23, e20)) | ~ spl20_80), inference(avatar_component_clause, [], [f818])).
fof(f818, plain, (spl20_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_80])])).
fof(f3034, plain, (~ (e23 = op2(e23, e20)) | (~ spl20_16 | spl20_171)), inference(forward_demodulation, [], [f3033, f382])).
fof(f3033, plain, (~ (op2(e23, e20) = h4(e13)) | (~ spl20_16 | spl20_171)), inference(forward_demodulation, [], [f1415, f516])).
fof(f516, plain, ((e13 = op1(e13, e10)) | ~ spl20_16), inference(avatar_component_clause, [], [f514])).
fof(f514, plain, (spl20_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_16])])).
fof(f1415, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | spl20_171), inference(avatar_component_clause, [], [f1413])).
fof(f1413, plain, (spl20_171 <=> (op2(e23, e20) = h4(op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_171])])).
fof(f3026, plain, (~ spl20_92 | ~ spl20_193), inference(avatar_split_clause, [], [f3020, f1544, f869])).
fof(f869, plain, (spl20_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_92])])).
fof(f1544, plain, (spl20_193 <=> (e23 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_193])])).
fof(f3020, plain, (~ (e23 = op2(e22, e21)) | ~ spl20_193), inference(backward_demodulation, [], [f1192, f1545])).
fof(f1545, plain, ((e23 = h3(e12)) | ~ spl20_193), inference(avatar_component_clause, [], [f1544])).
fof(f1192, plain, ~ (op2(e22, e21) = h3(e12)), inference(backward_demodulation, [], [f247, f381])).
fof(f381, plain, (op2(e22, e22) = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e22, e22) = h3(e12)) & (h3(e11) = op2(op2(e22, e22), e22)) & (h3(e10) = op2(e22, op2(e22, e22))) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax16)).
fof(f247, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f3025, plain, (~ spl20_104 | ~ spl20_193), inference(avatar_split_clause, [], [f3019, f1544, f920])).
fof(f920, plain, (spl20_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_104])])).
fof(f3019, plain, (~ (e23 = op2(e21, e22)) | ~ spl20_193), inference(backward_demodulation, [], [f1189, f1545])).
fof(f1189, plain, ~ (op2(e21, e22) = h3(e12)), inference(backward_demodulation, [], [f223, f381])).
fof(f223, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2998, plain, (~ spl20_36 | ~ spl20_100 | spl20_175), inference(avatar_contradiction_clause, [], [f2997])).
fof(f2997, plain, ($false | (~ spl20_36 | ~ spl20_100 | spl20_175)), inference(subsumption_resolution, [], [f2996, f382])).
fof(f2996, plain, (~ (e23 = h4(e13)) | (~ spl20_36 | ~ spl20_100 | spl20_175)), inference(backward_demodulation, [], [f2810, f601])).
fof(f601, plain, ((e13 = op1(e11, e13)) | ~ spl20_36), inference(avatar_component_clause, [], [f599])).
fof(f599, plain, (spl20_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_36])])).
fof(f2810, plain, (~ (e23 = h4(op1(e11, e13))) | (~ spl20_100 | spl20_175)), inference(backward_demodulation, [], [f1431, f905])).
fof(f905, plain, ((e23 = op2(e21, e23)) | ~ spl20_100), inference(avatar_component_clause, [], [f903])).
fof(f903, plain, (spl20_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_100])])).
fof(f1431, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | spl20_175), inference(avatar_component_clause, [], [f1429])).
fof(f1429, plain, (spl20_175 <=> (op2(e21, e23) = h4(op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_175])])).
fof(f2988, plain, (~ spl20_66 | ~ spl20_67), inference(avatar_contradiction_clause, [], [f2987])).
fof(f2987, plain, ($false | (~ spl20_66 | ~ spl20_67)), inference(subsumption_resolution, [], [f2986, f265])).
fof(f265, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax8)).
fof(f2986, plain, ((e21 = e22) | (~ spl20_66 | ~ spl20_67)), inference(backward_demodulation, [], [f765, f761])).
fof(f761, plain, ((e21 = op2(e23, e23)) | ~ spl20_66), inference(avatar_component_clause, [], [f759])).
fof(f759, plain, (spl20_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_66])])).
fof(f765, plain, ((e22 = op2(e23, e23)) | ~ spl20_67), inference(avatar_component_clause, [], [f763])).
fof(f763, plain, (spl20_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_67])])).
fof(f2918, plain, (~ spl20_115 | ~ spl20_138), inference(avatar_split_clause, [], [f2398, f1214, f967])).
fof(f967, plain, (spl20_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_115])])).
fof(f2398, plain, (~ (e22 = op2(e20, e23)) | ~ spl20_138), inference(forward_demodulation, [], [f1196, f1215])).
fof(f1196, plain, ~ (op2(e20, e23) = h4(e12)), inference(backward_demodulation, [], [f228, f385])).
fof(f228, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2917, plain, (~ spl20_116 | ~ spl20_100), inference(avatar_split_clause, [], [f2916, f903, f971])).
fof(f971, plain, (spl20_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_116])])).
fof(f2916, plain, (~ (e23 = op2(e20, e23)) | ~ spl20_100), inference(forward_demodulation, [], [f226, f905])).
fof(f226, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2915, plain, (~ spl20_114 | ~ spl20_82), inference(avatar_split_clause, [], [f2595, f827, f963])).
fof(f963, plain, (spl20_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_114])])).
fof(f827, plain, (spl20_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_82])])).
fof(f2595, plain, (~ (e21 = op2(e20, e23)) | ~ spl20_82), inference(forward_demodulation, [], [f227, f829])).
fof(f829, plain, ((e21 = op2(e22, e23)) | ~ spl20_82), inference(avatar_component_clause, [], [f827])).
fof(f227, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2910, plain, (~ spl20_85 | spl20_148), inference(avatar_contradiction_clause, [], [f2909])).
fof(f2909, plain, ($false | (~ spl20_85 | spl20_148)), inference(subsumption_resolution, [], [f2908, f1275])).
fof(f1275, plain, (~ (e20 = h3(e12)) | spl20_148), inference(avatar_component_clause, [], [f1273])).
fof(f1273, plain, (spl20_148 <=> (e20 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_148])])).
fof(f2908, plain, ((e20 = h3(e12)) | ~ spl20_85), inference(forward_demodulation, [], [f381, f842])).
fof(f842, plain, ((e20 = op2(e22, e22)) | ~ spl20_85), inference(avatar_component_clause, [], [f840])).
fof(f840, plain, (spl20_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_85])])).
fof(f2891, plain, (~ spl20_21 | ~ spl20_5), inference(avatar_split_clause, [], [f2712, f468, f536])).
fof(f536, plain, (spl20_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_21])])).
fof(f2712, plain, (~ (e10 = op1(e12, e12)) | ~ spl20_5), inference(forward_demodulation, [], [f177, f470])).
fof(f177, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax5)).
fof(f2888, plain, (~ spl20_13 | ~ spl20_5), inference(avatar_split_clause, [], [f2715, f468, f502])).
fof(f502, plain, (spl20_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_13])])).
fof(f2715, plain, (~ (e10 = op1(e13, e10)) | ~ spl20_5), inference(forward_demodulation, [], [f203, f470])).
fof(f203, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2885, plain, (~ spl20_14 | ~ spl20_62), inference(avatar_split_clause, [], [f2884, f710, f506])).
fof(f506, plain, (spl20_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_14])])).
fof(f2884, plain, (~ (e11 = op1(e13, e10)) | ~ spl20_62), inference(forward_demodulation, [], [f162, f712])).
fof(f162, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2855, plain, (~ spl20_41 | ~ spl20_42), inference(avatar_contradiction_clause, [], [f2854])).
fof(f2854, plain, ($false | (~ spl20_41 | ~ spl20_42)), inference(subsumption_resolution, [], [f2853, f256])).
fof(f256, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax7)).
fof(f2853, plain, ((e10 = e11) | (~ spl20_41 | ~ spl20_42)), inference(backward_demodulation, [], [f627, f623])).
fof(f627, plain, ((e11 = op1(e11, e11)) | ~ spl20_42), inference(avatar_component_clause, [], [f625])).
fof(f625, plain, (spl20_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_42])])).
fof(f2847, plain, (~ spl20_39 | ~ spl20_47), inference(avatar_split_clause, [], [f2845, f646, f612])).
fof(f612, plain, (spl20_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_39])])).
fof(f2845, plain, (~ (e12 = op1(e11, e12)) | ~ spl20_47), inference(backward_demodulation, [], [f191, f648])).
fof(f191, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2842, plain, (~ spl20_49 | ~ spl20_52), inference(avatar_contradiction_clause, [], [f2841])).
fof(f2841, plain, ($false | (~ spl20_49 | ~ spl20_52)), inference(subsumption_resolution, [], [f2840, f258])).
fof(f258, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f2840, plain, ((e10 = e13) | (~ spl20_49 | ~ spl20_52)), inference(forward_demodulation, [], [f669, f657])).
fof(f669, plain, ((e13 = op1(e10, e13)) | ~ spl20_52), inference(avatar_component_clause, [], [f667])).
fof(f667, plain, (spl20_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_52])])).
fof(f2732, plain, (~ spl20_36 | ~ spl20_44), inference(avatar_split_clause, [], [f2730, f633, f599])).
fof(f633, plain, (spl20_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_44])])).
fof(f2730, plain, (~ (e13 = op1(e11, e13)) | ~ spl20_44), inference(backward_demodulation, [], [f194, f635])).
fof(f635, plain, ((e13 = op1(e11, e11)) | ~ spl20_44), inference(avatar_component_clause, [], [f633])).
fof(f194, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2725, plain, (~ spl20_35 | ~ spl20_3), inference(avatar_split_clause, [], [f2724, f459, f595])).
fof(f595, plain, (spl20_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_35])])).
fof(f2724, plain, (~ (e12 = op1(e11, e13)) | ~ spl20_3), inference(forward_demodulation, [], [f182, f461])).
fof(f182, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2719, plain, (spl20_18 | ~ spl20_3), inference(avatar_split_clause, [], [f2115, f459, f523])).
fof(f2115, plain, ((e11 = op1(e12, e13)) | ~ spl20_3), inference(backward_demodulation, [], [f365, f461])).
fof(f365, plain, (e11 = op1(op1(e13, e13), e13)), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e12 = op1(e13, e13)) & (e11 = op1(op1(e13, e13), e13)) & (e10 = op1(e13, op1(e13, e13)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax12)).
fof(f2709, plain, (~ spl20_5 | ~ spl20_6), inference(avatar_contradiction_clause, [], [f2708])).
fof(f2708, plain, ($false | (~ spl20_5 | ~ spl20_6)), inference(subsumption_resolution, [], [f2707, f256])).
fof(f2707, plain, ((e10 = e11) | (~ spl20_5 | ~ spl20_6)), inference(backward_demodulation, [], [f474, f470])).
fof(f474, plain, ((e11 = op1(e13, e12)) | ~ spl20_6), inference(avatar_component_clause, [], [f472])).
fof(f472, plain, (spl20_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_6])])).
fof(f2669, plain, (~ spl20_62 | ~ spl20_63), inference(avatar_contradiction_clause, [], [f2668])).
fof(f2668, plain, ($false | (~ spl20_62 | ~ spl20_63)), inference(subsumption_resolution, [], [f2667, f259])).
fof(f259, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f2667, plain, ((e11 = e12) | (~ spl20_62 | ~ spl20_63)), inference(backward_demodulation, [], [f716, f712])).
fof(f716, plain, ((op1(e10, e10) = e12) | ~ spl20_63), inference(avatar_component_clause, [], [f714])).
fof(f714, plain, (spl20_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl20_63])])).
fof(f2648, plain, (~ spl20_239 | ~ spl20_80 | ~ spl20_164), inference(avatar_split_clause, [], [f2645, f1354, f818, f1816])).
fof(f1816, plain, (spl20_239 <=> (e23 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_239])])).
fof(f2645, plain, (~ (e23 = h1(e11)) | (~ spl20_80 | ~ spl20_164)), inference(backward_demodulation, [], [f2410, f820])).
fof(f2410, plain, (~ (op2(e23, e20) = h1(e11)) | ~ spl20_164), inference(backward_demodulation, [], [f212, f2405])).
fof(f2405, plain, ((op2(e21, e20) = h1(e11)) | ~ spl20_164), inference(backward_demodulation, [], [f1178, f1355])).
fof(f1178, plain, (h1(e11) = op2(h1(e12), e20)), inference(forward_demodulation, [], [f372, f373])).
fof(f372, plain, (h1(e11) = op2(op2(e20, e20), e20)), inference(cnf_transformation, [], [f14])).
fof(f212, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2641, plain, (spl20_144 | ~ spl20_86), inference(avatar_split_clause, [], [f2639, f844, f1253])).
fof(f1253, plain, (spl20_144 <=> (e21 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_144])])).
fof(f844, plain, (spl20_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_86])])).
fof(f2639, plain, ((e21 = h3(e12)) | ~ spl20_86), inference(backward_demodulation, [], [f381, f846])).
fof(f846, plain, ((e21 = op2(e22, e22)) | ~ spl20_86), inference(avatar_component_clause, [], [f844])).
fof(f2630, plain, (~ spl20_105 | ~ spl20_108), inference(avatar_contradiction_clause, [], [f2629])).
fof(f2629, plain, ($false | (~ spl20_105 | ~ spl20_108)), inference(subsumption_resolution, [], [f2628, f264])).
fof(f264, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f2628, plain, ((e20 = e23) | (~ spl20_105 | ~ spl20_108)), inference(forward_demodulation, [], [f939, f927])).
fof(f927, plain, ((e20 = op2(e21, e21)) | ~ spl20_105), inference(avatar_component_clause, [], [f925])).
fof(f925, plain, (spl20_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_105])])).
fof(f939, plain, ((e23 = op2(e21, e21)) | ~ spl20_108), inference(avatar_component_clause, [], [f937])).
fof(f937, plain, (spl20_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_108])])).
fof(f2627, plain, (spl20_239 | ~ spl20_112 | ~ spl20_164), inference(avatar_split_clause, [], [f2626, f1354, f954, f1816])).
fof(f954, plain, (spl20_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_112])])).
fof(f2626, plain, ((e23 = h1(e11)) | (~ spl20_112 | ~ spl20_164)), inference(backward_demodulation, [], [f2405, f956])).
fof(f956, plain, ((e23 = op2(e21, e20)) | ~ spl20_112), inference(avatar_component_clause, [], [f954])).
fof(f2618, plain, (~ spl20_123 | ~ spl20_124), inference(avatar_contradiction_clause, [], [f2617])).
fof(f2617, plain, ($false | (~ spl20_123 | ~ spl20_124)), inference(subsumption_resolution, [], [f2616, f267])).
fof(f267, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f2616, plain, ((e22 = e23) | (~ spl20_123 | ~ spl20_124)), inference(backward_demodulation, [], [f1007, f1003])).
fof(f1003, plain, ((e22 = op2(e20, e21)) | ~ spl20_123), inference(avatar_component_clause, [], [f1001])).
fof(f1001, plain, (spl20_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_123])])).
fof(f2606, plain, (~ spl20_144 | ~ spl20_82), inference(avatar_split_clause, [], [f2605, f827, f1253])).
fof(f2605, plain, (~ (e21 = h3(e12)) | ~ spl20_82), inference(forward_demodulation, [], [f1193, f829])).
fof(f1193, plain, ~ (op2(e22, e23) = h3(e12)), inference(backward_demodulation, [], [f249, f381])).
fof(f249, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2581, plain, (~ spl20_99 | ~ spl20_138), inference(avatar_split_clause, [], [f2392, f1214, f899])).
fof(f899, plain, (spl20_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_99])])).
fof(f2392, plain, (~ (e22 = op2(e21, e23)) | ~ spl20_138), inference(forward_demodulation, [], [f1197, f1215])).
fof(f1197, plain, ~ (op2(e21, e23) = h4(e12)), inference(backward_demodulation, [], [f230, f385])).
fof(f230, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2580, plain, (~ spl20_78 | ~ spl20_164), inference(avatar_split_clause, [], [f2404, f1354, f810])).
fof(f810, plain, (spl20_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_78])])).
fof(f2404, plain, (~ (e21 = op2(e23, e20)) | ~ spl20_164), inference(backward_demodulation, [], [f1174, f1355])).
fof(f1174, plain, ~ (op2(e23, e20) = h1(e12)), inference(backward_demodulation, [], [f210, f373])).
fof(f210, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2518, plain, (~ spl20_1 | ~ spl20_3), inference(avatar_contradiction_clause, [], [f2517])).
fof(f2517, plain, ($false | (~ spl20_1 | ~ spl20_3)), inference(subsumption_resolution, [], [f2516, f257])).
fof(f257, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f2516, plain, ((e10 = e12) | (~ spl20_1 | ~ spl20_3)), inference(backward_demodulation, [], [f461, f453])).
fof(f453, plain, ((e10 = op1(e13, e13)) | ~ spl20_1), inference(avatar_component_clause, [], [f451])).
fof(f451, plain, (spl20_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_1])])).
fof(f2455, plain, (~ spl20_105 | ~ spl20_107), inference(avatar_contradiction_clause, [], [f2454])).
fof(f2454, plain, ($false | (~ spl20_105 | ~ spl20_107)), inference(subsumption_resolution, [], [f2453, f263])).
fof(f263, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f2453, plain, ((e20 = e22) | (~ spl20_105 | ~ spl20_107)), inference(backward_demodulation, [], [f935, f927])).
fof(f935, plain, ((e22 = op2(e21, e21)) | ~ spl20_107), inference(avatar_component_clause, [], [f933])).
fof(f933, plain, (spl20_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_107])])).
fof(f2449, plain, (~ spl20_150 | ~ spl20_113 | ~ spl20_193), inference(avatar_split_clause, [], [f2447, f1544, f959, f1283])).
fof(f1283, plain, (spl20_150 <=> (e20 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_150])])).
fof(f2447, plain, (~ (e20 = h3(e10)) | (~ spl20_113 | ~ spl20_193)), inference(backward_demodulation, [], [f2397, f961])).
fof(f2397, plain, (~ (op2(e20, e23) = h3(e10)) | ~ spl20_193), inference(forward_demodulation, [], [f227, f2220])).
fof(f2220, plain, ((op2(e22, e23) = h3(e10)) | ~ spl20_193), inference(backward_demodulation, [], [f1195, f1545])).
fof(f1195, plain, (h3(e10) = op2(e22, h3(e12))), inference(forward_demodulation, [], [f379, f381])).
fof(f379, plain, (h3(e10) = op2(e22, op2(e22, e22))), inference(cnf_transformation, [], [f16])).
fof(f2436, plain, (~ spl20_126 | ~ spl20_128), inference(avatar_contradiction_clause, [], [f2435])).
fof(f2435, plain, ($false | (~ spl20_126 | ~ spl20_128)), inference(subsumption_resolution, [], [f2434, f266])).
fof(f266, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f2434, plain, ((e21 = e23) | (~ spl20_126 | ~ spl20_128)), inference(forward_demodulation, [], [f1024, f1016])).
fof(f1016, plain, ((op2(e20, e20) = e21) | ~ spl20_126), inference(avatar_component_clause, [], [f1014])).
fof(f1014, plain, (spl20_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl20_126])])).
fof(f1024, plain, ((op2(e20, e20) = e23) | ~ spl20_128), inference(avatar_component_clause, [], [f1022])).
fof(f1022, plain, (spl20_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl20_128])])).
fof(f2387, plain, (~ spl20_79 | ~ spl20_138), inference(avatar_split_clause, [], [f2386, f1214, f814])).
fof(f814, plain, (spl20_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_79])])).
fof(f2386, plain, (~ (e22 = op2(e23, e20)) | ~ spl20_138), inference(forward_demodulation, [], [f1199, f1215])).
fof(f1199, plain, ~ (op2(e23, e20) = h4(e12)), inference(backward_demodulation, [], [f252, f385])).
fof(f252, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2383, plain, (~ spl20_75 | ~ spl20_138), inference(avatar_split_clause, [], [f2382, f1214, f797])).
fof(f797, plain, (spl20_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_75])])).
fof(f2382, plain, (~ (e22 = op2(e23, e21)) | ~ spl20_138), inference(forward_demodulation, [], [f1200, f1215])).
fof(f1200, plain, ~ (op2(e23, e21) = h4(e12)), inference(backward_demodulation, [], [f254, f385])).
fof(f254, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2369, plain, (spl20_69 | ~ spl20_138), inference(avatar_split_clause, [], [f2232, f1214, f772])).
fof(f772, plain, (spl20_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_69])])).
fof(f2232, plain, ((e20 = op2(e23, e22)) | ~ spl20_138), inference(backward_demodulation, [], [f1202, f1215])).
fof(f2340, plain, (~ spl20_3 | ~ spl20_17), inference(avatar_contradiction_clause, [], [f2339])).
fof(f2339, plain, ($false | (~ spl20_3 | ~ spl20_17)), inference(subsumption_resolution, [], [f2338, f256])).
fof(f2338, plain, ((e10 = e11) | (~ spl20_3 | ~ spl20_17)), inference(forward_demodulation, [], [f2115, f521])).
fof(f521, plain, ((e10 = op1(e12, e13)) | ~ spl20_17), inference(avatar_component_clause, [], [f519])).
fof(f519, plain, (spl20_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_17])])).
fof(f2331, plain, (~ spl20_15 | ~ spl20_3), inference(avatar_split_clause, [], [f2330, f459, f510])).
fof(f510, plain, (spl20_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_15])])).
fof(f2330, plain, (~ (e12 = op1(e13, e10)) | ~ spl20_3), inference(forward_demodulation, [], [f204, f461])).
fof(f204, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2327, plain, (~ spl20_7 | ~ spl20_3), inference(avatar_split_clause, [], [f2326, f459, f476])).
fof(f476, plain, (spl20_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_7])])).
fof(f2326, plain, (~ (e12 = op1(e13, e12)) | ~ spl20_3), inference(forward_demodulation, [], [f207, f461])).
fof(f207, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2325, plain, (spl20_5 | ~ spl20_3), inference(avatar_split_clause, [], [f2114, f459, f468])).
fof(f2114, plain, ((e10 = op1(e13, e12)) | ~ spl20_3), inference(backward_demodulation, [], [f364, f461])).
fof(f364, plain, (e10 = op1(e13, op1(e13, e13))), inference(cnf_transformation, [], [f12])).
fof(f2311, plain, (~ spl20_22 | ~ spl20_24), inference(avatar_contradiction_clause, [], [f2310])).
fof(f2310, plain, ($false | (~ spl20_22 | ~ spl20_24)), inference(subsumption_resolution, [], [f2309, f260])).
fof(f260, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f2309, plain, ((e11 = e13) | (~ spl20_22 | ~ spl20_24)), inference(backward_demodulation, [], [f550, f542])).
fof(f542, plain, ((e11 = op1(e12, e12)) | ~ spl20_22), inference(avatar_component_clause, [], [f540])).
fof(f540, plain, (spl20_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_22])])).
fof(f2304, plain, (~ spl20_27 | ~ spl20_31), inference(avatar_split_clause, [], [f2303, f578, f561])).
fof(f578, plain, (spl20_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_31])])).
fof(f2303, plain, (~ (e12 = op1(e12, e11)) | ~ spl20_31), inference(backward_demodulation, [], [f196, f580])).
fof(f580, plain, ((e12 = op1(e12, e10)) | ~ spl20_31), inference(avatar_component_clause, [], [f578])).
fof(f196, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f2302, plain, (~ spl20_34 | ~ spl20_36), inference(avatar_contradiction_clause, [], [f2301])).
fof(f2301, plain, ($false | (~ spl20_34 | ~ spl20_36)), inference(subsumption_resolution, [], [f2300, f260])).
fof(f2300, plain, ((e11 = e13) | (~ spl20_34 | ~ spl20_36)), inference(backward_demodulation, [], [f601, f593])).
fof(f593, plain, ((e11 = op1(e11, e13)) | ~ spl20_34), inference(avatar_component_clause, [], [f591])).
fof(f591, plain, (spl20_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_34])])).
fof(f2283, plain, (~ spl20_65 | ~ spl20_67), inference(avatar_contradiction_clause, [], [f2282])).
fof(f2282, plain, ($false | (~ spl20_65 | ~ spl20_67)), inference(subsumption_resolution, [], [f2281, f263])).
fof(f2281, plain, ((e20 = e22) | (~ spl20_65 | ~ spl20_67)), inference(backward_demodulation, [], [f765, f757])).
fof(f757, plain, ((e20 = op2(e23, e23)) | ~ spl20_65), inference(avatar_component_clause, [], [f755])).
fof(f755, plain, (spl20_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_65])])).
fof(f2280, plain, (~ spl20_69 | ~ spl20_70), inference(avatar_contradiction_clause, [], [f2279])).
fof(f2279, plain, ($false | (~ spl20_69 | ~ spl20_70)), inference(subsumption_resolution, [], [f2278, f262])).
fof(f262, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f2278, plain, ((e20 = e21) | (~ spl20_69 | ~ spl20_70)), inference(forward_demodulation, [], [f778, f774])).
fof(f774, plain, ((e20 = op2(e23, e22)) | ~ spl20_69), inference(avatar_component_clause, [], [f772])).
fof(f778, plain, ((e21 = op2(e23, e22)) | ~ spl20_70), inference(avatar_component_clause, [], [f776])).
fof(f776, plain, (spl20_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_70])])).
fof(f2272, plain, (spl20_150 | ~ spl20_81 | ~ spl20_193), inference(avatar_split_clause, [], [f2270, f1544, f823, f1283])).
fof(f823, plain, (spl20_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_81])])).
fof(f2270, plain, ((e20 = h3(e10)) | (~ spl20_81 | ~ spl20_193)), inference(backward_demodulation, [], [f2220, f825])).
fof(f825, plain, ((e20 = op2(e22, e23)) | ~ spl20_81), inference(avatar_component_clause, [], [f823])).
fof(f2240, plain, (spl20_164 | ~ spl20_126), inference(avatar_split_clause, [], [f2239, f1014, f1354])).
fof(f2239, plain, ((e21 = h1(e12)) | ~ spl20_126), inference(backward_demodulation, [], [f373, f1016])).
fof(f2237, plain, (spl20_82 | ~ spl20_138), inference(avatar_split_clause, [], [f2233, f1214, f827])).
fof(f2233, plain, ((e21 = op2(e22, e23)) | ~ spl20_138), inference(backward_demodulation, [], [f1203, f1215])).
fof(f2222, plain, (~ spl20_120 | ~ spl20_193), inference(avatar_split_clause, [], [f2214, f1544, f988])).
fof(f988, plain, (spl20_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_120])])).
fof(f2214, plain, (~ (e23 = op2(e20, e22)) | ~ spl20_193), inference(backward_demodulation, [], [f1188, f1545])).
fof(f1188, plain, ~ (op2(e20, e22) = h3(e12)), inference(backward_demodulation, [], [f221, f381])).
fof(f221, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2213, plain, (spl20_138 | ~ spl20_67), inference(avatar_split_clause, [], [f1979, f763, f1214])).
fof(f1979, plain, ((e22 = h4(e12)) | ~ spl20_67), inference(backward_demodulation, [], [f385, f765])).
fof(f2205, plain, (~ spl20_117 | ~ spl20_69), inference(avatar_split_clause, [], [f2204, f772, f976])).
fof(f976, plain, (spl20_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_117])])).
fof(f2204, plain, (~ (e20 = op2(e20, e22)) | ~ spl20_69), inference(forward_demodulation, [], [f222, f774])).
fof(f222, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2177, plain, (~ spl20_77 | ~ spl20_69), inference(avatar_split_clause, [], [f2176, f772, f806])).
fof(f806, plain, (spl20_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_77])])).
fof(f2176, plain, (~ (e20 = op2(e23, e20)) | ~ spl20_69), inference(forward_demodulation, [], [f251, f774])).
fof(f251, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2169, plain, (~ spl20_57 | ~ spl20_41), inference(avatar_split_clause, [], [f2168, f621, f689])).
fof(f689, plain, (spl20_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_57])])).
fof(f2168, plain, (~ (e10 = op1(e10, e11)) | ~ spl20_41), inference(forward_demodulation, [], [f166, f623])).
fof(f166, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2161, plain, (~ spl20_53 | ~ spl20_5), inference(avatar_split_clause, [], [f2160, f468, f672])).
fof(f672, plain, (spl20_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_53])])).
fof(f2160, plain, (~ (e10 = op1(e10, e12)) | ~ spl20_5), inference(forward_demodulation, [], [f174, f470])).
fof(f174, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2134, plain, (~ spl20_30 | ~ spl20_18), inference(avatar_split_clause, [], [f2133, f523, f574])).
fof(f574, plain, (spl20_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_30])])).
fof(f2133, plain, (~ (e11 = op1(e12, e10)) | ~ spl20_18), inference(forward_demodulation, [], [f198, f525])).
fof(f198, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2130, plain, (~ spl20_28 | ~ spl20_24), inference(avatar_split_clause, [], [f2129, f548, f565])).
fof(f565, plain, (spl20_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_28])])).
fof(f2129, plain, (~ (e13 = op1(e12, e11)) | ~ spl20_24), inference(forward_demodulation, [], [f199, f550])).
fof(f199, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2128, plain, (~ spl20_26 | ~ spl20_18), inference(avatar_split_clause, [], [f2127, f523, f557])).
fof(f557, plain, (spl20_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_26])])).
fof(f2127, plain, (~ (e11 = op1(e12, e11)) | ~ spl20_18), inference(forward_demodulation, [], [f200, f525])).
fof(f200, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2118, plain, (~ spl20_2 | ~ spl20_3), inference(avatar_contradiction_clause, [], [f2117])).
fof(f2117, plain, ($false | (~ spl20_2 | ~ spl20_3)), inference(subsumption_resolution, [], [f2116, f259])).
fof(f2116, plain, ((e11 = e12) | (~ spl20_2 | ~ spl20_3)), inference(backward_demodulation, [], [f461, f457])).
fof(f457, plain, ((e11 = op1(e13, e13)) | ~ spl20_2), inference(avatar_component_clause, [], [f455])).
fof(f455, plain, (spl20_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_2])])).
fof(f2093, plain, (~ spl20_4 | ~ spl20_16), inference(avatar_split_clause, [], [f2090, f514, f463])).
fof(f463, plain, (spl20_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_4])])).
fof(f2090, plain, (~ (e13 = op1(e13, e13)) | ~ spl20_16), inference(backward_demodulation, [], [f204, f516])).
fof(f2091, plain, (~ spl20_12 | ~ spl20_16), inference(avatar_split_clause, [], [f2088, f514, f497])).
fof(f497, plain, (spl20_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_12])])).
fof(f2088, plain, (~ (e13 = op1(e13, e11)) | ~ spl20_16), inference(backward_demodulation, [], [f202, f516])).
fof(f202, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2084, plain, (~ spl20_23 | ~ spl20_24), inference(avatar_contradiction_clause, [], [f2083])).
fof(f2083, plain, ($false | (~ spl20_23 | ~ spl20_24)), inference(subsumption_resolution, [], [f2082, f261])).
fof(f261, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f2082, plain, ((e12 = e13) | (~ spl20_23 | ~ spl20_24)), inference(backward_demodulation, [], [f550, f546])).
fof(f546, plain, ((e12 = op1(e12, e12)) | ~ spl20_23), inference(avatar_component_clause, [], [f544])).
fof(f544, plain, (spl20_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_23])])).
fof(f2080, plain, (~ spl20_20 | ~ spl20_24), inference(avatar_split_clause, [], [f2077, f548, f531])).
fof(f531, plain, (spl20_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_20])])).
fof(f2077, plain, (~ (e13 = op1(e12, e13)) | ~ spl20_24), inference(backward_demodulation, [], [f201, f550])).
fof(f201, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2047, plain, (~ spl20_22 | ~ spl20_38), inference(avatar_split_clause, [], [f2044, f608, f540])).
fof(f2044, plain, (~ (e11 = op1(e12, e12)) | ~ spl20_38), inference(backward_demodulation, [], [f175, f610])).
fof(f175, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2043, plain, (~ spl20_33 | ~ spl20_41), inference(avatar_split_clause, [], [f2039, f621, f587])).
fof(f587, plain, (spl20_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_33])])).
fof(f2039, plain, (~ (e10 = op1(e11, e13)) | ~ spl20_41), inference(backward_demodulation, [], [f194, f623])).
fof(f2040, plain, (~ spl20_25 | ~ spl20_41), inference(avatar_split_clause, [], [f2036, f621, f553])).
fof(f553, plain, (spl20_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_25])])).
fof(f2036, plain, (~ (e10 = op1(e12, e11)) | ~ spl20_41), inference(backward_demodulation, [], [f169, f623])).
fof(f169, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f1990, plain, (~ spl20_29 | ~ spl20_61), inference(avatar_split_clause, [], [f1983, f706, f570])).
fof(f706, plain, (spl20_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_61])])).
fof(f1983, plain, (~ (e10 = op1(e12, e10)) | ~ spl20_61), inference(backward_demodulation, [], [f161, f708])).
fof(f708, plain, ((e10 = op1(e10, e10)) | ~ spl20_61), inference(avatar_component_clause, [], [f706])).
fof(f161, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f1978, plain, (~ spl20_148 | ~ spl20_69), inference(avatar_split_clause, [], [f1976, f772, f1273])).
fof(f1976, plain, (~ (e20 = h3(e12)) | ~ spl20_69), inference(backward_demodulation, [], [f1190, f774])).
fof(f1190, plain, ~ (op2(e23, e22) = h3(e12)), inference(backward_demodulation, [], [f225, f381])).
fof(f225, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f1954, plain, (~ spl20_76 | ~ spl20_80), inference(avatar_split_clause, [], [f1951, f818, f801])).
fof(f801, plain, (spl20_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_76])])).
fof(f1951, plain, (~ (e23 = op2(e23, e21)) | ~ spl20_80), inference(backward_demodulation, [], [f250, f820])).
fof(f250, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f1939, plain, (spl20_193 | ~ spl20_88), inference(avatar_split_clause, [], [f1938, f852, f1544])).
fof(f1938, plain, ((e23 = h3(e12)) | ~ spl20_88), inference(backward_demodulation, [], [f381, f854])).
fof(f1935, plain, (~ spl20_156 | ~ spl20_89), inference(avatar_split_clause, [], [f1931, f857, f1314])).
fof(f857, plain, (spl20_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_89])])).
fof(f1931, plain, (~ (e20 = h2(e12)) | ~ spl20_89), inference(backward_demodulation, [], [f1181, f859])).
fof(f859, plain, ((e20 = op2(e22, e21)) | ~ spl20_89), inference(avatar_component_clause, [], [f857])).
fof(f1181, plain, ~ (op2(e22, e21) = h2(e12)), inference(backward_demodulation, [], [f217, f377])).
fof(f377, plain, (op2(e21, e21) = h2(e12)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, e21) = h2(e12)) & (h2(e11) = op2(op2(e21, e21), e21)) & (h2(e10) = op2(e21, op2(e21, e21))) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax15)).
fof(f217, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f1897, plain, (spl20_156 | ~ spl20_105), inference(avatar_split_clause, [], [f1896, f925, f1314])).
fof(f1896, plain, ((e20 = h2(e12)) | ~ spl20_105), inference(backward_demodulation, [], [f377, f927])).
fof(f1850, plain, (~ spl20_113 | ~ spl20_125), inference(avatar_split_clause, [], [f1842, f1010, f959])).
fof(f1010, plain, (spl20_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_125])])).
fof(f1842, plain, (~ (e20 = op2(e20, e23)) | ~ spl20_125), inference(backward_demodulation, [], [f1177, f1836])).
fof(f1836, plain, ((e20 = h1(e12)) | ~ spl20_125), inference(backward_demodulation, [], [f373, f1012])).
fof(f1012, plain, ((e20 = op2(e20, e20)) | ~ spl20_125), inference(avatar_component_clause, [], [f1010])).
fof(f1177, plain, ~ (op2(e20, e23) = h1(e12)), inference(backward_demodulation, [], [f234, f373])).
fof(f234, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f1460, plain, (~ spl20_167 | spl20_141 | spl20_139 | spl20_137 | ~ spl20_168 | ~ spl20_169 | ~ spl20_170 | ~ spl20_171 | ~ spl20_172 | ~ spl20_173 | ~ spl20_174 | ~ spl20_175 | ~ spl20_176 | ~ spl20_177 | ~ spl20_178 | ~ spl20_179 | ~ spl20_180 | ~ spl20_181 | ~ spl20_182), inference(avatar_split_clause, [], [f1395, f1457, f1453, f1449, f1445, f1441, f1437, f1433, f1429, f1425, f1421, f1417, f1413, f1409, f1405, f1401, f1210, f1222, f1235, f1397])).
fof(f1235, plain, (spl20_141 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl20_141])])).
fof(f1222, plain, (spl20_139 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl20_139])])).
fof(f1210, plain, (spl20_137 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl20_137])])).
fof(f1395, plain, (~ (h1(e12) = h4(op1(e10, e10))) | ~ (op2(e20, e21) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), inference(forward_demodulation, [], [f1394, f373])).
fof(f1394, plain, (~ (op2(e20, e20) = h4(op1(e10, e10))) | ~ (op2(e20, e21) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), inference(forward_demodulation, [], [f1393, f1207])).
fof(f1393, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1392, f1207])).
fof(f1392, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), e21)) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1391, f1205])).
fof(f1391, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1390, f1207])).
fof(f1390, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1389, f1207])).
fof(f1389, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1388, f382])).
fof(f1388, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1387, f1205])).
fof(f1387, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1386, f1207])).
fof(f1386, plain, (~ (h2(e12) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1385, f377])).
fof(f1385, plain, (~ (op2(e21, e21) = h4(op1(e11, e11))) | ~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1384, f1205])).
fof(f1384, plain, (~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | ~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1383, f1205])).
fof(f1383, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1382, f1205])).
fof(f1382, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1381, f382])).
fof(f1381, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1380, f1207])).
fof(f1380, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | ~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1379, f1205])).
fof(f1379, plain, (~ (e21 = h4(op1(e12, e13))) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1378, f1203])).
fof(f1378, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1377, f382])).
fof(f1377, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1376, f382])).
fof(f1376, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e20)) | ~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1375, f1207])).
fof(f1375, plain, (~ (op2(e23, e21) = h4(op1(e13, e11))) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1374, f382])).
fof(f1374, plain, (~ (h4(op1(e13, e11)) = op2(h4(e13), e21)) | ~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1373, f1205])).
fof(f1373, plain, (~ (e20 = h4(op1(e13, e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1372, f1202])).
fof(f1372, plain, (~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1371, f382])).
fof(f1371, plain, (~ (h4(e12) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1370, f385])).
fof(f1370, plain, (~ (op2(e23, e23) = h4(op1(e13, e13))) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1369, f382])).
fof(f1369, plain, (sP19 | sP18 | sP17 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f449, f382])).
fof(f449, plain, (~ (e23 = h4(e13)) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP19 | sP18 | sP17 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP16 | sP15 | sP14 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP13 | sP12 | sP11 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP10 | sP9 | sP8 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e42, e41, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31])).
fof(f31, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP8), inference(usedef, [], [e31])).
fof(e31, plain, (sP8 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f32, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP9), inference(usedef, [], [e32])).
fof(e32, plain, (sP9 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f33, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP10), inference(usedef, [], [e33])).
fof(e33, plain, (sP10 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f34, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP11), inference(usedef, [], [e34])).
fof(e34, plain, (sP11 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f35, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP12), inference(usedef, [], [e35])).
fof(e35, plain, (sP12 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f36, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP13), inference(usedef, [], [e36])).
fof(e36, plain, (sP13 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f37, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP14), inference(usedef, [], [e37])).
fof(e37, plain, (sP14 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f38, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP15), inference(usedef, [], [e38])).
fof(e38, plain, (sP15 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f39, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP16), inference(usedef, [], [e39])).
fof(e39, plain, (sP16 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f40, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP17), inference(usedef, [], [e40])).
fof(e40, plain, (sP17 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f41, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP18), inference(usedef, [], [e41])).
fof(e41, plain, (sP18 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f42, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP19), inference(usedef, [], [e42])).
fof(e42, plain, (sP19 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', co1)).
fof(f1245, plain, ~ spl20_141, inference(avatar_split_clause, [], [f1244, f1235])).
fof(f1244, plain, ~ sP17, inference(subsumption_resolution, [], [f394, f1207])).
fof(f394, plain, (~ (e20 = h4(e10)) | ~ sP17), inference(cnf_transformation, [], [f54])).
fof(f54, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP17), inference(nnf_transformation, [], [f40])).
fof(f1231, plain, ~ spl20_139, inference(avatar_split_clause, [], [f1230, f1222])).
fof(f1230, plain, ~ sP18, inference(subsumption_resolution, [], [f391, f1205])).
fof(f391, plain, (~ (e21 = h4(e11)) | ~ sP18), inference(cnf_transformation, [], [f53])).
fof(f53, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP18), inference(nnf_transformation, [], [f41])).
fof(f1217, plain, (~ spl20_137 | ~ spl20_138), inference(avatar_split_clause, [], [f388, f1214, f1210])).
fof(f388, plain, (~ (e22 = h4(e12)) | ~ sP19), inference(cnf_transformation, [], [f52])).
fof(f52, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP19), inference(nnf_transformation, [], [f42])).
fof(f1171, plain, spl20_67, inference(avatar_split_clause, [], [f369, f763])).
fof(f369, plain, (e22 = op2(e23, e23)), inference(cnf_transformation, [], [f13])).
fof(f1170, plain, spl20_3, inference(avatar_split_clause, [], [f366, f459])).
fof(f366, plain, (e12 = op1(e13, e13)), inference(cnf_transformation, [], [f12])).
fof(f1169, plain, (spl20_136 | spl20_105 | spl20_85 | spl20_65), inference(avatar_split_clause, [], [f332, f755, f840, f925, f1133])).
fof(f1133, plain, (spl20_136 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl20_136])])).
fof(f332, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e21)) | sP4), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21))) | sP7) & ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21))) | sP6) & ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | sP5) & ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21))) | sP4)), inference(definition_folding, [], [f11, e29, e28, e27, e26])).
fof(f26, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP4), inference(usedef, [], [e26])).
fof(e26, plain, (sP4 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f27, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21)) | ~ sP5), inference(usedef, [], [e27])).
fof(e27, plain, (sP5 <=> (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f28, plain, ((~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22)) | ~ sP6), inference(usedef, [], [e28])).
fof(e28, plain, (sP6 <=> (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f29, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23)) | ~ sP7), inference(usedef, [], [e29])).
fof(e29, plain, (sP7 <=> (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f11, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e22 = op2(e22, e23)) & (e23 = op2(e22, e22))) | (~ (e21 = op2(e21, e23)) & (e23 = op2(e21, e21))) | (~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23))) & ((~ (e23 = op2(e23, e22)) & (e22 = op2(e23, e23))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e21, e21))) | (~ (e20 = op2(e20, e22)) & (op2(e20, e20) = e22))) & ((~ (e23 = op2(e23, e21)) & (e21 = op2(e23, e23))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e22, e22))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21))) & ((~ (e23 = op2(e23, e20)) & (e20 = op2(e23, e23))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e22, e22))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e21, e21))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax11)).
fof(f1161, plain, (spl20_135 | spl20_106 | spl20_86 | spl20_66), inference(avatar_split_clause, [], [f340, f759, f844, f929, f1127])).
fof(f1127, plain, (spl20_135 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl20_135])])).
fof(f929, plain, (spl20_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_106])])).
fof(f340, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e21)) | sP5), inference(cnf_transformation, [], [f30])).
fof(f1160, plain, (spl20_135 | ~ spl20_106 | spl20_86 | spl20_66), inference(avatar_split_clause, [], [f341, f759, f844, f929, f1127])).
fof(f341, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e22, e22)) | ~ (e21 = op2(e21, e21)) | sP5), inference(cnf_transformation, [], [f30])).
fof(f1145, plain, (spl20_133 | spl20_108 | spl20_88 | spl20_68), inference(avatar_split_clause, [], [f356, f767, f852, f937, f1115])).
fof(f1115, plain, (spl20_133 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl20_133])])).
fof(f767, plain, (spl20_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_68])])).
fof(f356, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | sP7), inference(cnf_transformation, [], [f30])).
fof(f1141, plain, (spl20_133 | spl20_108 | spl20_88 | ~ spl20_68), inference(avatar_split_clause, [], [f360, f767, f852, f937, f1115])).
fof(f360, plain, (~ (e23 = op2(e23, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e21)) | sP7), inference(cnf_transformation, [], [f30])).
fof(f1137, plain, (~ spl20_136 | spl20_125), inference(avatar_split_clause, [], [f330, f1010, f1133])).
fof(f330, plain, ((e20 = op2(e20, e20)) | ~ sP4), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP4), inference(nnf_transformation, [], [f26])).
fof(f1136, plain, (~ spl20_136 | ~ spl20_125), inference(avatar_split_clause, [], [f331, f1010, f1133])).
fof(f331, plain, (~ (e20 = op2(e20, e20)) | ~ sP4), inference(cnf_transformation, [], [f51])).
fof(f1131, plain, (~ spl20_135 | spl20_126), inference(avatar_split_clause, [], [f328, f1014, f1127])).
fof(f328, plain, ((op2(e20, e20) = e21) | ~ sP5), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e20 = op2(e20, e21)) & (op2(e20, e20) = e21)) | ~ sP5), inference(nnf_transformation, [], [f27])).
fof(f1119, plain, (~ spl20_133 | spl20_128), inference(avatar_split_clause, [], [f324, f1022, f1115])).
fof(f324, plain, ((op2(e20, e20) = e23) | ~ sP7), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e20 = op2(e20, e23)) & (op2(e20, e20) = e23)) | ~ sP7), inference(nnf_transformation, [], [f29])).
fof(f1113, plain, (spl20_132 | spl20_41 | spl20_21 | spl20_1), inference(avatar_split_clause, [], [f292, f451, f536, f621, f1077])).
fof(f1077, plain, (spl20_132 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl20_132])])).
fof(f292, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e11)) | sP0), inference(cnf_transformation, [], [f25])).
fof(f25, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11))) | sP3) & ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11))) | sP2) & ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | sP1) & ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11))) | sP0)), inference(definition_folding, [], [f10, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11)) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12)) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13)) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f10, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e12 = op1(e12, e13)) & (e13 = op1(e12, e12))) | (~ (e11 = op1(e11, e13)) & (e13 = op1(e11, e11))) | (~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13))) & ((~ (e13 = op1(e13, e12)) & (e12 = op1(e13, e13))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e11, e11))) | (~ (e10 = op1(e10, e12)) & (op1(e10, e10) = e12))) & ((~ (e13 = op1(e13, e11)) & (e11 = op1(e13, e13))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e12, e12))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11))) & ((~ (e13 = op1(e13, e10)) & (e10 = op1(e13, e13))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e12, e12))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e11, e11))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax10)).
fof(f1112, plain, (spl20_132 | ~ spl20_46 | spl20_21 | spl20_1), inference(avatar_split_clause, [], [f293, f451, f536, f642, f1077])).
fof(f642, plain, (spl20_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_46])])).
fof(f293, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e12)) | ~ (e11 = op1(e11, e10)) | sP0), inference(cnf_transformation, [], [f25])).
fof(f1105, plain, (spl20_131 | spl20_42 | spl20_22 | spl20_2), inference(avatar_split_clause, [], [f300, f455, f540, f625, f1071])).
fof(f1071, plain, (spl20_131 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl20_131])])).
fof(f300, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e11)) | sP1), inference(cnf_transformation, [], [f25])).
fof(f1089, plain, (spl20_129 | spl20_44 | spl20_24 | spl20_4), inference(avatar_split_clause, [], [f316, f463, f548, f633, f1059])).
fof(f1059, plain, (spl20_129 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl20_129])])).
fof(f316, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e11)) | sP3), inference(cnf_transformation, [], [f25])).
fof(f1088, plain, (spl20_129 | ~ spl20_34 | spl20_24 | spl20_4), inference(avatar_split_clause, [], [f317, f463, f548, f591, f1059])).
fof(f317, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | ~ (e11 = op1(e11, e13)) | sP3), inference(cnf_transformation, [], [f25])).
fof(f1084, plain, (spl20_129 | ~ spl20_34 | spl20_24 | ~ spl20_4), inference(avatar_split_clause, [], [f321, f463, f548, f591, f1059])).
fof(f321, plain, (~ (e13 = op1(e13, e13)) | (e13 = op1(e12, e12)) | ~ (e11 = op1(e11, e13)) | sP3), inference(cnf_transformation, [], [f25])).
fof(f1081, plain, (~ spl20_132 | spl20_61), inference(avatar_split_clause, [], [f290, f706, f1077])).
fof(f290, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1080, plain, (~ spl20_132 | ~ spl20_61), inference(avatar_split_clause, [], [f291, f706, f1077])).
fof(f291, plain, (~ (e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f47])).
fof(f1075, plain, (~ spl20_131 | spl20_62), inference(avatar_split_clause, [], [f288, f710, f1071])).
fof(f288, plain, ((op1(e10, e10) = e11) | ~ sP1), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e10 = op1(e10, e11)) & (op1(e10, e10) = e11)) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1062, plain, (~ spl20_129 | ~ spl20_49), inference(avatar_split_clause, [], [f285, f655, f1059])).
fof(f285, plain, (~ (e10 = op1(e10, e13)) | ~ sP3), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ((~ (e10 = op1(e10, e13)) & (op1(e10, e10) = e13)) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f1045, plain, (spl20_111 | spl20_107 | spl20_103 | spl20_99), inference(avatar_split_clause, [], [f140, f899, f916, f933, f950])).
fof(f140, plain, ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax4)).
fof(f1044, plain, (spl20_123 | spl20_107 | spl20_91 | spl20_75), inference(avatar_split_clause, [], [f141, f797, f865, f933, f1001])).
fof(f141, plain, ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1043, plain, (spl20_112 | spl20_108 | spl20_104 | spl20_100), inference(avatar_split_clause, [], [f142, f903, f920, f937, f954])).
fof(f142, plain, ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1042, plain, (spl20_124 | spl20_108 | spl20_92 | spl20_76), inference(avatar_split_clause, [], [f143, f801, f869, f937, f1005])).
fof(f143, plain, ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1041, plain, (spl20_93 | spl20_89 | spl20_85 | spl20_81), inference(avatar_split_clause, [], [f144, f823, f840, f857, f874])).
fof(f144, plain, ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1038, plain, (spl20_118 | spl20_102 | spl20_86 | spl20_70), inference(avatar_split_clause, [], [f147, f776, f844, f912, f980])).
fof(f147, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1031, plain, (spl20_78 | spl20_74 | spl20_70 | spl20_66), inference(avatar_split_clause, [], [f154, f759, f776, f793, f810])).
fof(f154, plain, ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f991, plain, (spl20_117 | spl20_118 | spl20_119 | spl20_120), inference(avatar_split_clause, [], [f114, f988, f984, f980, f976])).
fof(f114, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax3)).
fof(f974, plain, (spl20_113 | spl20_114 | spl20_115 | spl20_116), inference(avatar_split_clause, [], [f115, f971, f967, f963, f959])).
fof(f115, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f821, plain, (spl20_77 | spl20_78 | spl20_79 | spl20_80), inference(avatar_split_clause, [], [f124, f818, f814, f810, f806])).
fof(f124, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f3])).
fof(f753, plain, (spl20_61 | spl20_57 | spl20_53 | spl20_49), inference(avatar_split_clause, [], [f80, f655, f672, f689, f706])).
fof(f80, plain, ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax2)).
fof(f750, plain, (spl20_62 | spl20_46 | spl20_30 | spl20_14), inference(avatar_split_clause, [], [f83, f506, f574, f642, f710])).
fof(f83, plain, ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f748, plain, (spl20_63 | spl20_47 | spl20_31 | spl20_15), inference(avatar_split_clause, [], [f85, f510, f578, f646, f714])).
fof(f85, plain, ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f743, plain, (spl20_46 | spl20_42 | spl20_38 | spl20_34), inference(avatar_split_clause, [], [f90, f591, f608, f625, f642])).
fof(f90, plain, ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f738, plain, (spl20_60 | spl20_44 | spl20_28 | spl20_12), inference(avatar_split_clause, [], [f95, f497, f565, f633, f701])).
fof(f95, plain, ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f737, plain, (spl20_29 | spl20_25 | spl20_21 | spl20_17), inference(avatar_split_clause, [], [f96, f519, f536, f553, f570])).
fof(f96, plain, ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f732, plain, (spl20_55 | spl20_39 | spl20_23 | spl20_7), inference(avatar_split_clause, [], [f101, f476, f544, f612, f680])).
fof(f101, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f727, plain, (spl20_14 | spl20_10 | spl20_6 | spl20_2), inference(avatar_split_clause, [], [f106, f455, f472, f489, f506])).
fof(f106, plain, ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f722, plain, (spl20_52 | spl20_36 | spl20_20 | spl20_4), inference(avatar_split_clause, [], [f111, f463, f531, f599, f667])).
fof(f111, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f602, plain, (spl20_33 | spl20_34 | spl20_35 | spl20_36), inference(avatar_split_clause, [], [f71, f599, f595, f591, f587])).
fof(f71, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG117+1.p', ax1)).
fof(f568, plain, (spl20_25 | spl20_26 | spl20_27 | spl20_28), inference(avatar_split_clause, [], [f73, f565, f561, f557, f553])).
fof(f73, plain, ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f517, plain, (spl20_13 | spl20_14 | spl20_15 | spl20_16), inference(avatar_split_clause, [], [f76, f514, f510, f506, f502])).
fof(f76, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).