fof(f3331, plain, $false, inference(avatar_sat_refutation, [], [f619, f636, f704, f722, f724, f725, f734, f736, f742, f746, f752, f787, f889, f923, f991, f1026, f1028, f1038, f1039, f1049, f1051, f1052, f1053, f1069, f1075, f1081, f1095, f1097, f1104, f1105, f1113, f1125, f1131, f1137, f1151, f1153, f1160, f1161, f1169, f1170, f1172, f1284, f1298, f1312, f1650, f1875, f1886, f1894, f1895, f1911, f1920, f1925, f1943, f1969, f1972, f1984, f2002, f2007, f2035, f2051, f2059, f2083, f2108, f2109, f2110, f2113, f2154, f2166, f2168, f2181, f2185, f2193, f2223, f2227, f2235, f2236, f2237, f2239, f2289, f2296, f2318, f2359, f2362, f2395, f2400, f2404, f2419, f2439, f2500, f2507, f2512, f2515, f2621, f2628, f2646, f2672, f2679, f2736, f2738, f2740, f2748, f2773, f2807, f2819, f2829, f2845, f2855, f2883, f2893, f2914, f2935, f2949, f2964, f2980, f2992, f3004, f3014, f3027, f3060, f3074, f3089, f3103, f3141, f3152, f3188, f3220, f3226, f3260, f3278, f3299, f3302, f3316])).
fof(f3316, plain, (~ spl20_103 | ~ spl20_39 | spl20_208), inference(avatar_split_clause, [], [f3315, f1623, f612, f916])).
fof(f916, plain, (spl20_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_103])])).
fof(f612, plain, (spl20_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_39])])).
fof(f1623, plain, (spl20_208 <=> (op2(e21, e22) = h3(op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_208])])).
fof(f3315, plain, (~ (e22 = op2(e21, e22)) | (~ spl20_39 | spl20_208)), inference(forward_demodulation, [], [f3314, f378])).
fof(f378, plain, (e22 = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e22, e22) = h3(e13)) & (op2(op2(op2(e22, e22), op2(e22, e22)), op2(e22, e22)) = h3(e11)) & (op2(op2(e22, e22), op2(e22, e22)) = h3(e10)) & (e22 = h3(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax16)).
fof(f3314, plain, (~ (op2(e21, e22) = h3(e12)) | (~ spl20_39 | spl20_208)), inference(forward_demodulation, [], [f1625, f614])).
fof(f614, plain, ((e12 = op1(e11, e12)) | ~ spl20_39), inference(avatar_component_clause, [], [f612])).
fof(f1625, plain, (~ (op2(e21, e22) = h3(op1(e11, e12))) | spl20_208), inference(avatar_component_clause, [], [f1623])).
fof(f3302, plain, (~ spl20_102 | ~ spl20_70), inference(avatar_split_clause, [], [f3301, f776, f912])).
fof(f912, plain, (spl20_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_102])])).
fof(f776, plain, (spl20_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_70])])).
fof(f3301, plain, (~ (e21 = op2(e21, e22)) | ~ spl20_70), inference(forward_demodulation, [], [f224, f778])).
fof(f778, plain, ((e21 = op2(e23, e22)) | ~ spl20_70), inference(avatar_component_clause, [], [f776])).
fof(f224, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax6)).
fof(f3299, plain, (~ spl20_102 | ~ spl20_110), inference(avatar_split_clause, [], [f3298, f946, f912])).
fof(f946, plain, (spl20_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_110])])).
fof(f3298, plain, (~ (e21 = op2(e21, e22)) | ~ spl20_110), inference(forward_demodulation, [], [f239, f948])).
fof(f948, plain, ((e21 = op2(e21, e20)) | ~ spl20_110), inference(avatar_component_clause, [], [f946])).
fof(f239, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f3278, plain, (~ spl20_59 | ~ spl20_123 | spl20_213), inference(avatar_contradiction_clause, [], [f3277])).
fof(f3277, plain, ($false | (~ spl20_59 | ~ spl20_123 | spl20_213)), inference(subsumption_resolution, [], [f3273, f378])).
fof(f3273, plain, (~ (e22 = h3(e12)) | (~ spl20_59 | ~ spl20_123 | spl20_213)), inference(backward_demodulation, [], [f3124, f699])).
fof(f699, plain, ((e12 = op1(e10, e11)) | ~ spl20_59), inference(avatar_component_clause, [], [f697])).
fof(f697, plain, (spl20_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_59])])).
fof(f3124, plain, (~ (e22 = h3(op1(e10, e11))) | (~ spl20_123 | spl20_213)), inference(forward_demodulation, [], [f1645, f1003])).
fof(f1003, plain, ((e22 = op2(e20, e21)) | ~ spl20_123), inference(avatar_component_clause, [], [f1001])).
fof(f1001, plain, (spl20_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_123])])).
fof(f1645, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | spl20_213), inference(avatar_component_clause, [], [f1643])).
fof(f1643, plain, (spl20_213 <=> (op2(e20, e21) = h3(op1(e10, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_213])])).
fof(f3260, plain, (~ spl20_65 | ~ spl20_68), inference(avatar_contradiction_clause, [], [f3259])).
fof(f3259, plain, ($false | (~ spl20_65 | ~ spl20_68)), inference(subsumption_resolution, [], [f3258, f264])).
fof(f264, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax8)).
fof(f3258, plain, ((e20 = e23) | (~ spl20_65 | ~ spl20_68)), inference(forward_demodulation, [], [f769, f757])).
fof(f757, plain, ((e20 = op2(e23, e23)) | ~ spl20_65), inference(avatar_component_clause, [], [f755])).
fof(f755, plain, (spl20_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_65])])).
fof(f769, plain, ((e23 = op2(e23, e23)) | ~ spl20_68), inference(avatar_component_clause, [], [f767])).
fof(f767, plain, (spl20_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_68])])).
fof(f3226, plain, (~ spl20_97 | ~ spl20_146), inference(avatar_split_clause, [], [f2572, f1259, f891])).
fof(f891, plain, (spl20_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_97])])).
fof(f1259, plain, (spl20_146 <=> (e20 = h4(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_146])])).
fof(f2572, plain, (~ (e20 = op2(e21, e23)) | ~ spl20_146), inference(forward_demodulation, [], [f1206, f1260])).
fof(f1260, plain, ((e20 = h4(e13)) | ~ spl20_146), inference(avatar_component_clause, [], [f1259])).
fof(f1206, plain, ~ (op2(e21, e23) = h4(e13)), inference(backward_demodulation, [], [f230, f385])).
fof(f385, plain, (op2(e23, e23) = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, e23) = h4(e13)) & (h4(e11) = op2(op2(op2(e23, e23), op2(e23, e23)), op2(e23, e23))) & (h4(e10) = op2(op2(e23, e23), op2(e23, e23))) & (e23 = h4(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax17)).
fof(f230, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3220, plain, (~ spl20_27 | ~ spl20_19), inference(avatar_split_clause, [], [f2830, f527, f561])).
fof(f561, plain, (spl20_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_27])])).
fof(f527, plain, (spl20_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_19])])).
fof(f2830, plain, (~ (e12 = op1(e12, e11)) | ~ spl20_19), inference(forward_demodulation, [], [f200, f529])).
fof(f529, plain, ((e12 = op1(e12, e13)) | ~ spl20_19), inference(avatar_component_clause, [], [f527])).
fof(f200, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax5)).
fof(f3188, plain, (~ spl20_48 | ~ spl20_36), inference(avatar_split_clause, [], [f2847, f599, f650])).
fof(f650, plain, (spl20_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_48])])).
fof(f599, plain, (spl20_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_36])])).
fof(f2847, plain, (~ (e13 = op1(e11, e10)) | ~ spl20_36), inference(forward_demodulation, [], [f192, f601])).
fof(f601, plain, ((e13 = op1(e11, e13)) | ~ spl20_36), inference(avatar_component_clause, [], [f599])).
fof(f192, plain, ~ (op1(e11, e10) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f3152, plain, (~ spl20_12 | ~ spl20_16), inference(avatar_split_clause, [], [f3147, f514, f497])).
fof(f497, plain, (spl20_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_12])])).
fof(f514, plain, (spl20_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_16])])).
fof(f3147, plain, (~ (e13 = op1(e13, e11)) | ~ spl20_16), inference(backward_demodulation, [], [f202, f516])).
fof(f516, plain, ((e13 = op1(e13, e10)) | ~ spl20_16), inference(avatar_component_clause, [], [f514])).
fof(f202, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3141, plain, (~ spl20_12 | ~ spl20_60), inference(avatar_split_clause, [], [f3136, f701, f497])).
fof(f701, plain, (spl20_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_60])])).
fof(f3136, plain, (~ (e13 = op1(e13, e11)) | ~ spl20_60), inference(backward_demodulation, [], [f168, f703])).
fof(f703, plain, ((e13 = op1(e10, e11)) | ~ spl20_60), inference(avatar_component_clause, [], [f701])).
fof(f168, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3103, plain, (~ spl20_64 | ~ spl20_198 | spl20_214 | ~ spl20_241), inference(avatar_contradiction_clause, [], [f3102])).
fof(f3102, plain, ($false | (~ spl20_64 | ~ spl20_198 | spl20_214 | ~ spl20_241)), inference(subsumption_resolution, [], [f3101, f1804])).
fof(f1804, plain, ((e23 = h1(e13)) | ~ spl20_241), inference(avatar_component_clause, [], [f1803])).
fof(f1803, plain, (spl20_241 <=> (e23 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_241])])).
fof(f3101, plain, (~ (e23 = h1(e13)) | (~ spl20_64 | ~ spl20_198 | spl20_214)), inference(forward_demodulation, [], [f3100, f1584])).
fof(f1584, plain, ((e23 = h3(e13)) | ~ spl20_198), inference(avatar_component_clause, [], [f1583])).
fof(f1583, plain, (spl20_198 <=> (e23 = h3(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_198])])).
fof(f3100, plain, (~ (h1(e13) = h3(e13)) | (~ spl20_64 | spl20_214)), inference(forward_demodulation, [], [f1649, f720])).
fof(f720, plain, ((op1(e10, e10) = e13) | ~ spl20_64), inference(avatar_component_clause, [], [f718])).
fof(f718, plain, (spl20_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl20_64])])).
fof(f1649, plain, (~ (h1(e13) = h3(op1(e10, e10))) | spl20_214), inference(avatar_component_clause, [], [f1647])).
fof(f1647, plain, (spl20_214 <=> (h1(e13) = h3(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_214])])).
fof(f3089, plain, (~ spl20_53 | ~ spl20_117 | spl20_212), inference(avatar_contradiction_clause, [], [f3088])).
fof(f3088, plain, ($false | (~ spl20_53 | ~ spl20_117 | spl20_212)), inference(subsumption_resolution, [], [f3087, f978])).
fof(f978, plain, ((e20 = op2(e20, e22)) | ~ spl20_117), inference(avatar_component_clause, [], [f976])).
fof(f976, plain, (spl20_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_117])])).
fof(f3087, plain, (~ (e20 = op2(e20, e22)) | (~ spl20_53 | spl20_212)), inference(forward_demodulation, [], [f3086, f1204])).
fof(f1204, plain, (e20 = h3(e10)), inference(forward_demodulation, [], [f1203, f1198])).
fof(f1198, plain, (e20 = op2(h3(e13), h3(e13))), inference(backward_demodulation, [], [f367, f381])).
fof(f381, plain, (op2(e22, e22) = h3(e13)), inference(cnf_transformation, [], [f16])).
fof(f367, plain, (e20 = op2(op2(e22, e22), op2(e22, e22))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e23 = op2(e22, e22)) & (e21 = op2(op2(op2(e22, e22), op2(e22, e22)), op2(e22, e22))) & (e20 = op2(op2(e22, e22), op2(e22, e22)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax13)).
fof(f1203, plain, (h3(e10) = op2(h3(e13), h3(e13))), inference(forward_demodulation, [], [f379, f381])).
fof(f379, plain, (op2(op2(e22, e22), op2(e22, e22)) = h3(e10)), inference(cnf_transformation, [], [f16])).
fof(f3086, plain, (~ (op2(e20, e22) = h3(e10)) | (~ spl20_53 | spl20_212)), inference(forward_demodulation, [], [f1641, f674])).
fof(f674, plain, ((e10 = op1(e10, e12)) | ~ spl20_53), inference(avatar_component_clause, [], [f672])).
fof(f672, plain, (spl20_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_53])])).
fof(f1641, plain, (~ (op2(e20, e22) = h3(op1(e10, e12))) | spl20_212), inference(avatar_component_clause, [], [f1639])).
fof(f1639, plain, (spl20_212 <=> (op2(e20, e22) = h3(op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_212])])).
fof(f3074, plain, (~ spl20_50 | spl20_211), inference(avatar_contradiction_clause, [], [f3073])).
fof(f3073, plain, ($false | (~ spl20_50 | spl20_211)), inference(subsumption_resolution, [], [f3072, f1202])).
fof(f1202, plain, (e21 = h3(e11)), inference(forward_demodulation, [], [f1201, f1199])).
fof(f1199, plain, (e21 = op2(e20, h3(e13))), inference(backward_demodulation, [], [f1173, f381])).
fof(f1173, plain, (e21 = op2(e20, op2(e22, e22))), inference(backward_demodulation, [], [f368, f367])).
fof(f368, plain, (e21 = op2(op2(op2(e22, e22), op2(e22, e22)), op2(e22, e22))), inference(cnf_transformation, [], [f13])).
fof(f1201, plain, (h3(e11) = op2(e20, h3(e13))), inference(forward_demodulation, [], [f1200, f1198])).
fof(f1200, plain, (h3(e11) = op2(op2(h3(e13), h3(e13)), h3(e13))), inference(forward_demodulation, [], [f380, f381])).
fof(f380, plain, (op2(op2(op2(e22, e22), op2(e22, e22)), op2(e22, e22)) = h3(e11)), inference(cnf_transformation, [], [f16])).
fof(f3072, plain, (~ (e21 = h3(e11)) | (~ spl20_50 | spl20_211)), inference(forward_demodulation, [], [f1637, f661])).
fof(f661, plain, ((e11 = op1(e10, e13)) | ~ spl20_50), inference(avatar_component_clause, [], [f659])).
fof(f659, plain, (spl20_50 <=> (e11 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_50])])).
fof(f1637, plain, (~ (e21 = h3(op1(e10, e13))) | spl20_211), inference(avatar_component_clause, [], [f1635])).
fof(f1635, plain, (spl20_211 <=> (e21 = h3(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_211])])).
fof(f3060, plain, (~ spl20_41 | ~ spl20_164 | spl20_209), inference(avatar_contradiction_clause, [], [f3059])).
fof(f3059, plain, ($false | (~ spl20_41 | ~ spl20_164 | spl20_209)), inference(subsumption_resolution, [], [f3058, f1360])).
fof(f1360, plain, ((e20 = h2(e13)) | ~ spl20_164), inference(avatar_component_clause, [], [f1359])).
fof(f1359, plain, (spl20_164 <=> (e20 = h2(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_164])])).
fof(f3058, plain, (~ (e20 = h2(e13)) | (~ spl20_41 | spl20_209)), inference(forward_demodulation, [], [f3057, f1204])).
fof(f3057, plain, (~ (h2(e13) = h3(e10)) | (~ spl20_41 | spl20_209)), inference(forward_demodulation, [], [f1629, f623])).
fof(f623, plain, ((e10 = op1(e11, e11)) | ~ spl20_41), inference(avatar_component_clause, [], [f621])).
fof(f621, plain, (spl20_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_41])])).
fof(f1629, plain, (~ (h2(e13) = h3(op1(e11, e11))) | spl20_209), inference(avatar_component_clause, [], [f1627])).
fof(f1627, plain, (spl20_209 <=> (h2(e13) = h3(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_209])])).
fof(f3027, plain, (~ spl20_36 | ~ spl20_100 | ~ spl20_198 | spl20_207), inference(avatar_contradiction_clause, [], [f3026])).
fof(f3026, plain, ($false | (~ spl20_36 | ~ spl20_100 | ~ spl20_198 | spl20_207)), inference(subsumption_resolution, [], [f3025, f905])).
fof(f905, plain, ((e23 = op2(e21, e23)) | ~ spl20_100), inference(avatar_component_clause, [], [f903])).
fof(f903, plain, (spl20_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_100])])).
fof(f3025, plain, (~ (e23 = op2(e21, e23)) | (~ spl20_36 | ~ spl20_198 | spl20_207)), inference(forward_demodulation, [], [f3024, f1584])).
fof(f3024, plain, (~ (op2(e21, e23) = h3(e13)) | (~ spl20_36 | ~ spl20_198 | spl20_207)), inference(forward_demodulation, [], [f3023, f601])).
fof(f3023, plain, (~ (op2(e21, e23) = h3(op1(e11, e13))) | (~ spl20_198 | spl20_207)), inference(forward_demodulation, [], [f1621, f1584])).
fof(f1621, plain, (~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | spl20_207), inference(avatar_component_clause, [], [f1619])).
fof(f1619, plain, (spl20_207 <=> (h3(op1(e11, e13)) = op2(e21, h3(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_207])])).
fof(f3014, plain, (~ spl20_29 | ~ spl20_93 | spl20_206), inference(avatar_contradiction_clause, [], [f3013])).
fof(f3013, plain, ($false | (~ spl20_29 | ~ spl20_93 | spl20_206)), inference(subsumption_resolution, [], [f3012, f876])).
fof(f876, plain, ((e20 = op2(e22, e20)) | ~ spl20_93), inference(avatar_component_clause, [], [f874])).
fof(f874, plain, (spl20_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_93])])).
fof(f3012, plain, (~ (e20 = op2(e22, e20)) | (~ spl20_29 | spl20_206)), inference(forward_demodulation, [], [f3011, f1204])).
fof(f3011, plain, (~ (op2(e22, e20) = h3(e10)) | (~ spl20_29 | spl20_206)), inference(forward_demodulation, [], [f1617, f572])).
fof(f572, plain, ((e10 = op1(e12, e10)) | ~ spl20_29), inference(avatar_component_clause, [], [f570])).
fof(f570, plain, (spl20_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_29])])).
fof(f1617, plain, (~ (op2(e22, e20) = h3(op1(e12, e10))) | spl20_206), inference(avatar_component_clause, [], [f1615])).
fof(f1615, plain, (spl20_206 <=> (op2(e22, e20) = h3(op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_206])])).
fof(f3004, plain, (~ spl20_26 | ~ spl20_90 | spl20_205), inference(avatar_contradiction_clause, [], [f3003])).
fof(f3003, plain, ($false | (~ spl20_26 | ~ spl20_90 | spl20_205)), inference(subsumption_resolution, [], [f3002, f863])).
fof(f863, plain, ((e21 = op2(e22, e21)) | ~ spl20_90), inference(avatar_component_clause, [], [f861])).
fof(f861, plain, (spl20_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_90])])).
fof(f3002, plain, (~ (e21 = op2(e22, e21)) | (~ spl20_26 | spl20_205)), inference(forward_demodulation, [], [f3001, f1202])).
fof(f3001, plain, (~ (op2(e22, e21) = h3(e11)) | (~ spl20_26 | spl20_205)), inference(forward_demodulation, [], [f1613, f559])).
fof(f559, plain, ((e11 = op1(e12, e11)) | ~ spl20_26), inference(avatar_component_clause, [], [f557])).
fof(f557, plain, (spl20_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_26])])).
fof(f1613, plain, (~ (op2(e22, e21) = h3(op1(e12, e11))) | spl20_205), inference(avatar_component_clause, [], [f1611])).
fof(f1611, plain, (spl20_205 <=> (op2(e22, e21) = h3(op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl20_205])])).
fof(f2992, plain, (~ spl20_24 | spl20_204), inference(avatar_contradiction_clause, [], [f2991])).
fof(f2991, plain, ($false | (~ spl20_24 | spl20_204)), inference(trivial_inequality_removal, [], [f2990])).
fof(f2990, plain, (~ (h3(e13) = h3(e13)) | (~ spl20_24 | spl20_204)), inference(forward_demodulation, [], [f1609, f550])).
fof(f550, plain, ((e13 = op1(e12, e12)) | ~ spl20_24), inference(avatar_component_clause, [], [f548])).
fof(f548, plain, (spl20_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_24])])).
fof(f1609, plain, (~ (h3(e13) = h3(op1(e12, e12))) | spl20_204), inference(avatar_component_clause, [], [f1607])).
fof(f1607, plain, (spl20_204 <=> (h3(e13) = h3(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl20_204])])).
fof(f2980, plain, (~ spl20_19 | ~ spl20_83 | ~ spl20_198 | spl20_203), inference(avatar_contradiction_clause, [], [f2979])).
fof(f2979, plain, ($false | (~ spl20_19 | ~ spl20_83 | ~ spl20_198 | spl20_203)), inference(subsumption_resolution, [], [f2978, f833])).
fof(f833, plain, ((e22 = op2(e22, e23)) | ~ spl20_83), inference(avatar_component_clause, [], [f831])).
fof(f831, plain, (spl20_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_83])])).
fof(f2978, plain, (~ (e22 = op2(e22, e23)) | (~ spl20_19 | ~ spl20_198 | spl20_203)), inference(forward_demodulation, [], [f2977, f378])).
fof(f2977, plain, (~ (op2(e22, e23) = h3(e12)) | (~ spl20_19 | ~ spl20_198 | spl20_203)), inference(forward_demodulation, [], [f2976, f529])).
fof(f2976, plain, (~ (op2(e22, e23) = h3(op1(e12, e13))) | (~ spl20_198 | spl20_203)), inference(forward_demodulation, [], [f1605, f1584])).
fof(f1605, plain, (~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | spl20_203), inference(avatar_component_clause, [], [f1603])).
fof(f1603, plain, (spl20_203 <=> (h3(op1(e12, e13)) = op2(e22, h3(e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_203])])).
fof(f2964, plain, (~ spl20_12 | ~ spl20_76 | ~ spl20_198 | spl20_201), inference(avatar_contradiction_clause, [], [f2963])).
fof(f2963, plain, ($false | (~ spl20_12 | ~ spl20_76 | ~ spl20_198 | spl20_201)), inference(subsumption_resolution, [], [f2962, f803])).
fof(f803, plain, ((e23 = op2(e23, e21)) | ~ spl20_76), inference(avatar_component_clause, [], [f801])).
fof(f801, plain, (spl20_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_76])])).
fof(f2962, plain, (~ (e23 = op2(e23, e21)) | (~ spl20_12 | ~ spl20_198 | spl20_201)), inference(forward_demodulation, [], [f2961, f1584])).
fof(f2961, plain, (~ (op2(e23, e21) = h3(e13)) | (~ spl20_12 | ~ spl20_198 | spl20_201)), inference(forward_demodulation, [], [f2960, f499])).
fof(f499, plain, ((e13 = op1(e13, e11)) | ~ spl20_12), inference(avatar_component_clause, [], [f497])).
fof(f2960, plain, (~ (op2(e23, e21) = h3(op1(e13, e11))) | (~ spl20_198 | spl20_201)), inference(forward_demodulation, [], [f1597, f1584])).
fof(f1597, plain, (~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | spl20_201), inference(avatar_component_clause, [], [f1595])).
fof(f1595, plain, (spl20_201 <=> (h3(op1(e13, e11)) = op2(h3(e13), e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_201])])).
fof(f2949, plain, (~ spl20_6 | ~ spl20_70 | ~ spl20_198 | spl20_200), inference(avatar_contradiction_clause, [], [f2948])).
fof(f2948, plain, ($false | (~ spl20_6 | ~ spl20_70 | ~ spl20_198 | spl20_200)), inference(subsumption_resolution, [], [f2947, f778])).
fof(f2947, plain, (~ (e21 = op2(e23, e22)) | (~ spl20_6 | ~ spl20_198 | spl20_200)), inference(forward_demodulation, [], [f2946, f1202])).
fof(f2946, plain, (~ (op2(e23, e22) = h3(e11)) | (~ spl20_6 | ~ spl20_198 | spl20_200)), inference(forward_demodulation, [], [f2945, f474])).
fof(f474, plain, ((e11 = op1(e13, e12)) | ~ spl20_6), inference(avatar_component_clause, [], [f472])).
fof(f472, plain, (spl20_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_6])])).
fof(f2945, plain, (~ (op2(e23, e22) = h3(op1(e13, e12))) | (~ spl20_198 | spl20_200)), inference(forward_demodulation, [], [f1593, f1584])).
fof(f1593, plain, (~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | spl20_200), inference(avatar_component_clause, [], [f1591])).
fof(f1591, plain, (spl20_200 <=> (h3(op1(e13, e12)) = op2(h3(e13), e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_200])])).
fof(f2935, plain, (~ spl20_1 | spl20_199), inference(avatar_contradiction_clause, [], [f2934])).
fof(f2934, plain, ($false | (~ spl20_1 | spl20_199)), inference(subsumption_resolution, [], [f2933, f1204])).
fof(f2933, plain, (~ (e20 = h3(e10)) | (~ spl20_1 | spl20_199)), inference(forward_demodulation, [], [f1589, f453])).
fof(f453, plain, ((e10 = op1(e13, e13)) | ~ spl20_1), inference(avatar_component_clause, [], [f451])).
fof(f451, plain, (spl20_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_1])])).
fof(f1589, plain, (~ (e20 = h3(op1(e13, e13))) | spl20_199), inference(avatar_component_clause, [], [f1587])).
fof(f1587, plain, (spl20_199 <=> (e20 = h3(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl20_199])])).
fof(f2914, plain, (~ spl20_46 | ~ spl20_110 | spl20_210), inference(avatar_contradiction_clause, [], [f2913])).
fof(f2913, plain, ($false | (~ spl20_46 | ~ spl20_110 | spl20_210)), inference(subsumption_resolution, [], [f2912, f948])).
fof(f2912, plain, (~ (e21 = op2(e21, e20)) | (~ spl20_46 | spl20_210)), inference(forward_demodulation, [], [f2911, f1202])).
fof(f2911, plain, (~ (op2(e21, e20) = h3(e11)) | (~ spl20_46 | spl20_210)), inference(forward_demodulation, [], [f1633, f644])).
fof(f644, plain, ((e11 = op1(e11, e10)) | ~ spl20_46), inference(avatar_component_clause, [], [f642])).
fof(f642, plain, (spl20_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_46])])).
fof(f1633, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | spl20_210), inference(avatar_component_clause, [], [f1631])).
fof(f1631, plain, (spl20_210 <=> (op2(e21, e20) = h3(op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl20_210])])).
fof(f2893, plain, (~ spl20_38 | ~ spl20_46), inference(avatar_split_clause, [], [f2891, f642, f608])).
fof(f608, plain, (spl20_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_38])])).
fof(f2891, plain, (~ (e11 = op1(e11, e12)) | ~ spl20_46), inference(backward_demodulation, [], [f191, f644])).
fof(f191, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2883, plain, (~ spl20_43 | ~ spl20_59), inference(avatar_split_clause, [], [f2881, f697, f629])).
fof(f629, plain, (spl20_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_43])])).
fof(f2881, plain, (~ (e12 = op1(e11, e11)) | ~ spl20_59), inference(backward_demodulation, [], [f166, f699])).
fof(f166, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2855, plain, (~ spl20_124 | ~ spl20_76), inference(avatar_split_clause, [], [f2854, f801, f1005])).
fof(f1005, plain, (spl20_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_124])])).
fof(f2854, plain, (~ (e23 = op2(e20, e21)) | ~ spl20_76), inference(forward_demodulation, [], [f216, f803])).
fof(f216, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2845, plain, (~ spl20_44 | ~ spl20_36), inference(avatar_split_clause, [], [f2844, f599, f633])).
fof(f633, plain, (spl20_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_44])])).
fof(f2844, plain, (~ (e13 = op1(e11, e11)) | ~ spl20_36), inference(forward_demodulation, [], [f194, f601])).
fof(f194, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2829, plain, (~ spl20_31 | ~ spl20_19), inference(avatar_split_clause, [], [f2828, f527, f578])).
fof(f578, plain, (spl20_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_31])])).
fof(f2828, plain, (~ (e12 = op1(e12, e10)) | ~ spl20_19), inference(forward_demodulation, [], [f198, f529])).
fof(f198, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2819, plain, (~ spl20_79 | ~ spl20_15 | ~ spl20_198 | spl20_202), inference(avatar_split_clause, [], [f2818, f1599, f1583, f510, f814])).
fof(f814, plain, (spl20_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_79])])).
fof(f510, plain, (spl20_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_15])])).
fof(f1599, plain, (spl20_202 <=> (h3(op1(e13, e10)) = op2(h3(e13), e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_202])])).
fof(f2818, plain, (~ (e22 = op2(e23, e20)) | (~ spl20_15 | ~ spl20_198 | spl20_202)), inference(forward_demodulation, [], [f2817, f378])).
fof(f2817, plain, (~ (op2(e23, e20) = h3(e12)) | (~ spl20_15 | ~ spl20_198 | spl20_202)), inference(forward_demodulation, [], [f2334, f512])).
fof(f512, plain, ((e12 = op1(e13, e10)) | ~ spl20_15), inference(avatar_component_clause, [], [f510])).
fof(f2334, plain, (~ (op2(e23, e20) = h3(op1(e13, e10))) | (~ spl20_198 | spl20_202)), inference(forward_demodulation, [], [f1601, f1584])).
fof(f1601, plain, (~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | spl20_202), inference(avatar_component_clause, [], [f1599])).
fof(f2807, plain, (~ spl20_21 | ~ spl20_24), inference(avatar_contradiction_clause, [], [f2806])).
fof(f2806, plain, ($false | (~ spl20_21 | ~ spl20_24)), inference(subsumption_resolution, [], [f2805, f258])).
fof(f258, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax7)).
fof(f2805, plain, ((e10 = e13) | (~ spl20_21 | ~ spl20_24)), inference(backward_demodulation, [], [f550, f538])).
fof(f538, plain, ((e10 = op1(e12, e12)) | ~ spl20_21), inference(avatar_component_clause, [], [f536])).
fof(f536, plain, (spl20_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_21])])).
fof(f2773, plain, (~ spl20_127 | ~ spl20_128), inference(avatar_contradiction_clause, [], [f2772])).
fof(f2772, plain, ($false | (~ spl20_127 | ~ spl20_128)), inference(subsumption_resolution, [], [f2771, f267])).
fof(f267, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f2771, plain, ((e22 = e23) | (~ spl20_127 | ~ spl20_128)), inference(backward_demodulation, [], [f1024, f1020])).
fof(f1020, plain, ((op2(e20, e20) = e22) | ~ spl20_127), inference(avatar_component_clause, [], [f1018])).
fof(f1018, plain, (spl20_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl20_127])])).
fof(f1024, plain, ((op2(e20, e20) = e23) | ~ spl20_128), inference(avatar_component_clause, [], [f1022])).
fof(f1022, plain, (spl20_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl20_128])])).
fof(f2748, plain, (spl20_241 | ~ spl20_128), inference(avatar_split_clause, [], [f2606, f1022, f1803])).
fof(f2606, plain, ((e23 = h1(e13)) | ~ spl20_128), inference(backward_demodulation, [], [f373, f1024])).
fof(f373, plain, (op2(e20, e20) = h1(e13)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(e20, e20) = h1(e13)) & (h1(e11) = op2(op2(op2(e20, e20), op2(e20, e20)), op2(e20, e20))) & (h1(e10) = op2(op2(e20, e20), op2(e20, e20))) & (e20 = h1(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax14)).
fof(f2740, plain, (~ spl20_95 | ~ spl20_83), inference(avatar_split_clause, [], [f2739, f831, f882])).
fof(f882, plain, (spl20_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_95])])).
fof(f2739, plain, (~ (e22 = op2(e22, e20)) | ~ spl20_83), inference(forward_demodulation, [], [f246, f833])).
fof(f246, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2738, plain, (~ spl20_94 | ~ spl20_110), inference(avatar_split_clause, [], [f2737, f946, f878])).
fof(f878, plain, (spl20_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_94])])).
fof(f2737, plain, (~ (e21 = op2(e22, e20)) | ~ spl20_110), inference(forward_demodulation, [], [f211, f948])).
fof(f211, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f2736, plain, (~ spl20_91 | ~ spl20_83), inference(avatar_split_clause, [], [f2735, f831, f865])).
fof(f865, plain, (spl20_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_91])])).
fof(f2735, plain, (~ (e22 = op2(e22, e21)) | ~ spl20_83), inference(forward_demodulation, [], [f248, f833])).
fof(f248, plain, ~ (op2(e22, e21) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2679, plain, (~ spl20_1 | ~ spl20_4), inference(avatar_contradiction_clause, [], [f2678])).
fof(f2678, plain, ($false | (~ spl20_1 | ~ spl20_4)), inference(subsumption_resolution, [], [f2677, f258])).
fof(f2677, plain, ((e10 = e13) | (~ spl20_1 | ~ spl20_4)), inference(forward_demodulation, [], [f465, f453])).
fof(f465, plain, ((e13 = op1(e13, e13)) | ~ spl20_4), inference(avatar_component_clause, [], [f463])).
fof(f463, plain, (spl20_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_4])])).
fof(f2672, plain, (~ spl20_10 | ~ spl20_12), inference(avatar_contradiction_clause, [], [f2671])).
fof(f2671, plain, ($false | (~ spl20_10 | ~ spl20_12)), inference(subsumption_resolution, [], [f2670, f260])).
fof(f260, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f2670, plain, ((e11 = e13) | (~ spl20_10 | ~ spl20_12)), inference(backward_demodulation, [], [f499, f491])).
fof(f491, plain, ((e11 = op1(e13, e11)) | ~ spl20_10), inference(avatar_component_clause, [], [f489])).
fof(f489, plain, (spl20_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_10])])).
fof(f2646, plain, (~ spl20_65 | ~ spl20_67), inference(avatar_contradiction_clause, [], [f2645])).
fof(f2645, plain, ($false | (~ spl20_65 | ~ spl20_67)), inference(subsumption_resolution, [], [f2644, f263])).
fof(f263, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f2644, plain, ((e20 = e22) | (~ spl20_65 | ~ spl20_67)), inference(forward_demodulation, [], [f765, f757])).
fof(f765, plain, ((e22 = op2(e23, e23)) | ~ spl20_67), inference(avatar_component_clause, [], [f763])).
fof(f763, plain, (spl20_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_67])])).
fof(f2628, plain, (~ spl20_110 | ~ spl20_111), inference(avatar_contradiction_clause, [], [f2627])).
fof(f2627, plain, ($false | (~ spl20_110 | ~ spl20_111)), inference(subsumption_resolution, [], [f2626, f265])).
fof(f265, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f2626, plain, ((e21 = e22) | (~ spl20_110 | ~ spl20_111)), inference(backward_demodulation, [], [f952, f948])).
fof(f952, plain, ((e22 = op2(e21, e20)) | ~ spl20_111), inference(avatar_component_clause, [], [f950])).
fof(f950, plain, (spl20_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_111])])).
fof(f2621, plain, (~ spl20_82 | ~ spl20_114), inference(avatar_split_clause, [], [f2619, f963, f827])).
fof(f827, plain, (spl20_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_82])])).
fof(f963, plain, (spl20_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_114])])).
fof(f2619, plain, (~ (e21 = op2(e22, e23)) | ~ spl20_114), inference(backward_demodulation, [], [f227, f965])).
fof(f965, plain, ((e21 = op2(e20, e23)) | ~ spl20_114), inference(avatar_component_clause, [], [f963])).
fof(f227, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2515, plain, (~ spl20_13 | ~ spl20_1), inference(avatar_split_clause, [], [f2337, f451, f502])).
fof(f502, plain, (spl20_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_13])])).
fof(f2337, plain, (~ (e10 = op1(e13, e10)) | ~ spl20_1), inference(forward_demodulation, [], [f204, f453])).
fof(f204, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2512, plain, (~ spl20_5 | ~ spl20_1), inference(avatar_split_clause, [], [f2323, f451, f468])).
fof(f468, plain, (spl20_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_5])])).
fof(f2323, plain, (~ (e10 = op1(e13, e12)) | ~ spl20_1), inference(forward_demodulation, [], [f207, f453])).
fof(f207, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2507, plain, (~ spl20_11 | ~ spl20_12), inference(avatar_contradiction_clause, [], [f2506])).
fof(f2506, plain, ($false | (~ spl20_11 | ~ spl20_12)), inference(subsumption_resolution, [], [f2505, f261])).
fof(f261, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f2505, plain, ((e12 = e13) | (~ spl20_11 | ~ spl20_12)), inference(backward_demodulation, [], [f499, f495])).
fof(f495, plain, ((e12 = op1(e13, e11)) | ~ spl20_11), inference(avatar_component_clause, [], [f493])).
fof(f493, plain, (spl20_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_11])])).
fof(f2500, plain, (~ spl20_37 | ~ spl20_38), inference(avatar_contradiction_clause, [], [f2499])).
fof(f2499, plain, ($false | (~ spl20_37 | ~ spl20_38)), inference(subsumption_resolution, [], [f2498, f256])).
fof(f256, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f2498, plain, ((e10 = e11) | (~ spl20_37 | ~ spl20_38)), inference(backward_demodulation, [], [f610, f606])).
fof(f606, plain, ((e10 = op1(e11, e12)) | ~ spl20_37), inference(avatar_component_clause, [], [f604])).
fof(f604, plain, (spl20_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_37])])).
fof(f610, plain, ((e11 = op1(e11, e12)) | ~ spl20_38), inference(avatar_component_clause, [], [f608])).
fof(f2439, plain, (~ spl20_114 | ~ spl20_118), inference(avatar_split_clause, [], [f2436, f980, f963])).
fof(f980, plain, (spl20_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_118])])).
fof(f2436, plain, (~ (e21 = op2(e20, e23)) | ~ spl20_118), inference(backward_demodulation, [], [f237, f982])).
fof(f982, plain, ((e21 = op2(e20, e22)) | ~ spl20_118), inference(avatar_component_clause, [], [f980])).
fof(f237, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f2419, plain, (~ spl20_69 | ~ spl20_146), inference(avatar_split_clause, [], [f2412, f1259, f772])).
fof(f772, plain, (spl20_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_69])])).
fof(f2412, plain, (~ (e20 = op2(e23, e22)) | ~ spl20_146), inference(backward_demodulation, [], [f1210, f1260])).
fof(f1210, plain, ~ (op2(e23, e22) = h4(e13)), inference(backward_demodulation, [], [f255, f385])).
fof(f255, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2404, plain, (~ spl20_119 | ~ spl20_103), inference(avatar_split_clause, [], [f2403, f916, f984])).
fof(f984, plain, (spl20_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_119])])).
fof(f2403, plain, (~ (e22 = op2(e20, e22)) | ~ spl20_103), inference(forward_demodulation, [], [f220, f918])).
fof(f918, plain, ((e22 = op2(e21, e22)) | ~ spl20_103), inference(avatar_component_clause, [], [f916])).
fof(f220, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2400, plain, (spl20_114 | ~ spl20_198), inference(avatar_split_clause, [], [f2233, f1583, f963])).
fof(f2233, plain, ((e21 = op2(e20, e23)) | ~ spl20_198), inference(backward_demodulation, [], [f1199, f1584])).
fof(f2395, plain, (~ spl20_84 | ~ spl20_198), inference(avatar_split_clause, [], [f2394, f1583, f835])).
fof(f835, plain, (spl20_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_84])])).
fof(f2394, plain, (~ (e23 = op2(e22, e23)) | ~ spl20_198), inference(forward_demodulation, [], [f1197, f1584])).
fof(f1197, plain, ~ (op2(e22, e23) = h3(e13)), inference(backward_demodulation, [], [f249, f381])).
fof(f249, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2362, plain, (spl20_50 | ~ spl20_24), inference(avatar_split_clause, [], [f2107, f548, f659])).
fof(f2107, plain, ((e11 = op1(e10, e13)) | ~ spl20_24), inference(backward_demodulation, [], [f1171, f550])).
fof(f1171, plain, (e11 = op1(e10, op1(e12, e12))), inference(backward_demodulation, [], [f365, f364])).
fof(f364, plain, (e10 = op1(op1(e12, e12), op1(e12, e12))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e13 = op1(e12, e12)) & (e11 = op1(op1(op1(e12, e12), op1(e12, e12)), op1(e12, e12))) & (e10 = op1(op1(e12, e12), op1(e12, e12)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax12)).
fof(f365, plain, (e11 = op1(op1(op1(e12, e12), op1(e12, e12)), op1(e12, e12))), inference(cnf_transformation, [], [f12])).
fof(f2359, plain, (~ spl20_50 | ~ spl20_58), inference(avatar_split_clause, [], [f2358, f693, f659])).
fof(f693, plain, (spl20_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_58])])).
fof(f2358, plain, (~ (e11 = op1(e10, e13)) | ~ spl20_58), inference(forward_demodulation, [], [f188, f695])).
fof(f695, plain, ((e11 = op1(e10, e11)) | ~ spl20_58), inference(avatar_component_clause, [], [f693])).
fof(f188, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2318, plain, (~ spl20_1 | ~ spl20_3), inference(avatar_contradiction_clause, [], [f2317])).
fof(f2317, plain, ($false | (~ spl20_1 | ~ spl20_3)), inference(subsumption_resolution, [], [f2316, f257])).
fof(f257, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f2316, plain, ((e10 = e12) | (~ spl20_1 | ~ spl20_3)), inference(backward_demodulation, [], [f461, f453])).
fof(f461, plain, ((e12 = op1(e13, e13)) | ~ spl20_3), inference(avatar_component_clause, [], [f459])).
fof(f459, plain, (spl20_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_3])])).
fof(f2296, plain, (~ spl20_41 | ~ spl20_45), inference(avatar_split_clause, [], [f2293, f638, f621])).
fof(f638, plain, (spl20_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_45])])).
fof(f2293, plain, (~ (e10 = op1(e11, e11)) | ~ spl20_45), inference(backward_demodulation, [], [f190, f640])).
fof(f640, plain, ((e10 = op1(e11, e10)) | ~ spl20_45), inference(avatar_component_clause, [], [f638])).
fof(f190, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2289, plain, (~ spl20_53 | ~ spl20_54), inference(avatar_contradiction_clause, [], [f2288])).
fof(f2288, plain, ($false | (~ spl20_53 | ~ spl20_54)), inference(subsumption_resolution, [], [f2287, f256])).
fof(f2287, plain, ((e10 = e11) | (~ spl20_53 | ~ spl20_54)), inference(forward_demodulation, [], [f678, f674])).
fof(f678, plain, ((e11 = op1(e10, e12)) | ~ spl20_54), inference(avatar_component_clause, [], [f676])).
fof(f676, plain, (spl20_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_54])])).
fof(f2239, plain, (spl20_65 | ~ spl20_198), inference(avatar_split_clause, [], [f2232, f1583, f755])).
fof(f2232, plain, ((e20 = op2(e23, e23)) | ~ spl20_198), inference(backward_demodulation, [], [f1198, f1584])).
fof(f2237, plain, (~ spl20_96 | ~ spl20_198), inference(avatar_split_clause, [], [f2230, f1583, f886])).
fof(f886, plain, (spl20_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_96])])).
fof(f2230, plain, (~ (e23 = op2(e22, e20)) | ~ spl20_198), inference(backward_demodulation, [], [f1195, f1584])).
fof(f1195, plain, ~ (op2(e22, e20) = h3(e13)), inference(backward_demodulation, [], [f245, f381])).
fof(f245, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2236, plain, (~ spl20_104 | ~ spl20_198), inference(avatar_split_clause, [], [f2229, f1583, f920])).
fof(f920, plain, (spl20_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_104])])).
fof(f2229, plain, (~ (e23 = op2(e21, e22)) | ~ spl20_198), inference(backward_demodulation, [], [f1193, f1584])).
fof(f1193, plain, ~ (op2(e21, e22) = h3(e13)), inference(backward_demodulation, [], [f223, f381])).
fof(f223, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2235, plain, (~ spl20_120 | ~ spl20_198), inference(avatar_split_clause, [], [f2228, f1583, f988])).
fof(f988, plain, (spl20_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_120])])).
fof(f2228, plain, (~ (e23 = op2(e20, e22)) | ~ spl20_198), inference(backward_demodulation, [], [f1192, f1584])).
fof(f1192, plain, ~ (op2(e20, e22) = h3(e13)), inference(backward_demodulation, [], [f221, f381])).
fof(f221, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2227, plain, (~ spl20_119 | ~ spl20_71), inference(avatar_split_clause, [], [f2226, f780, f984])).
fof(f780, plain, (spl20_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_71])])).
fof(f2226, plain, (~ (e22 = op2(e20, e22)) | ~ spl20_71), inference(forward_demodulation, [], [f222, f782])).
fof(f782, plain, ((e22 = op2(e23, e22)) | ~ spl20_71), inference(avatar_component_clause, [], [f780])).
fof(f222, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2223, plain, (~ spl20_114 | ~ spl20_116), inference(avatar_contradiction_clause, [], [f2222])).
fof(f2222, plain, ($false | (~ spl20_114 | ~ spl20_116)), inference(subsumption_resolution, [], [f2221, f266])).
fof(f266, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f2221, plain, ((e21 = e23) | (~ spl20_114 | ~ spl20_116)), inference(forward_demodulation, [], [f973, f965])).
fof(f973, plain, ((e23 = op2(e20, e23)) | ~ spl20_116), inference(avatar_component_clause, [], [f971])).
fof(f971, plain, (spl20_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_116])])).
fof(f2193, plain, (~ spl20_61 | ~ spl20_53), inference(avatar_split_clause, [], [f2192, f672, f706])).
fof(f706, plain, (spl20_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_61])])).
fof(f2192, plain, (~ (e10 = op1(e10, e10)) | ~ spl20_53), inference(forward_demodulation, [], [f185, f674])).
fof(f185, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f2185, plain, (~ spl20_57 | ~ spl20_53), inference(avatar_split_clause, [], [f2184, f672, f689])).
fof(f689, plain, (spl20_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_57])])).
fof(f2184, plain, (~ (e10 = op1(e10, e11)) | ~ spl20_53), inference(forward_demodulation, [], [f187, f674])).
fof(f187, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f2181, plain, (~ spl20_50 | ~ spl20_52), inference(avatar_contradiction_clause, [], [f2180])).
fof(f2180, plain, ($false | (~ spl20_50 | ~ spl20_52)), inference(subsumption_resolution, [], [f2179, f260])).
fof(f2179, plain, ((e11 = e13) | (~ spl20_50 | ~ spl20_52)), inference(forward_demodulation, [], [f669, f661])).
fof(f669, plain, ((e13 = op1(e10, e13)) | ~ spl20_52), inference(avatar_component_clause, [], [f667])).
fof(f667, plain, (spl20_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_52])])).
fof(f2168, plain, (~ spl20_40 | ~ spl20_24), inference(avatar_split_clause, [], [f2167, f548, f616])).
fof(f616, plain, (spl20_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_40])])).
fof(f2167, plain, (~ (e13 = op1(e11, e12)) | ~ spl20_24), inference(forward_demodulation, [], [f175, f550])).
fof(f175, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2166, plain, (~ spl20_39 | ~ spl20_7), inference(avatar_split_clause, [], [f2165, f476, f612])).
fof(f476, plain, (spl20_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_7])])).
fof(f2165, plain, (~ (e12 = op1(e11, e12)) | ~ spl20_7), inference(forward_demodulation, [], [f176, f478])).
fof(f478, plain, ((e12 = op1(e13, e12)) | ~ spl20_7), inference(avatar_component_clause, [], [f476])).
fof(f176, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2154, plain, (~ spl20_32 | ~ spl20_24), inference(avatar_split_clause, [], [f2153, f548, f582])).
fof(f582, plain, (spl20_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl20_32])])).
fof(f2153, plain, (~ (e13 = op1(e12, e10)) | ~ spl20_24), inference(forward_demodulation, [], [f197, f550])).
fof(f197, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2113, plain, (~ spl20_22 | ~ spl20_24), inference(avatar_contradiction_clause, [], [f2112])).
fof(f2112, plain, ($false | (~ spl20_22 | ~ spl20_24)), inference(subsumption_resolution, [], [f2111, f260])).
fof(f2111, plain, ((e11 = e13) | (~ spl20_22 | ~ spl20_24)), inference(backward_demodulation, [], [f550, f542])).
fof(f542, plain, ((e11 = op1(e12, e12)) | ~ spl20_22), inference(avatar_component_clause, [], [f540])).
fof(f540, plain, (spl20_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_22])])).
fof(f2110, plain, (spl20_1 | ~ spl20_24), inference(avatar_split_clause, [], [f2106, f548, f451])).
fof(f2106, plain, ((e10 = op1(e13, e13)) | ~ spl20_24), inference(backward_demodulation, [], [f364, f550])).
fof(f2109, plain, (~ spl20_20 | ~ spl20_24), inference(avatar_split_clause, [], [f2105, f548, f531])).
fof(f531, plain, (spl20_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_20])])).
fof(f2105, plain, (~ (e13 = op1(e12, e13)) | ~ spl20_24), inference(backward_demodulation, [], [f201, f550])).
fof(f201, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f2108, plain, (~ spl20_8 | ~ spl20_24), inference(avatar_split_clause, [], [f2104, f548, f480])).
fof(f480, plain, (spl20_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_8])])).
fof(f2104, plain, (~ (e13 = op1(e13, e12)) | ~ spl20_24), inference(backward_demodulation, [], [f177, f550])).
fof(f177, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2083, plain, (~ spl20_35 | ~ spl20_36), inference(avatar_contradiction_clause, [], [f2082])).
fof(f2082, plain, ($false | (~ spl20_35 | ~ spl20_36)), inference(subsumption_resolution, [], [f2081, f261])).
fof(f2081, plain, ((e12 = e13) | (~ spl20_35 | ~ spl20_36)), inference(backward_demodulation, [], [f601, f597])).
fof(f597, plain, ((e12 = op1(e11, e13)) | ~ spl20_35), inference(avatar_component_clause, [], [f595])).
fof(f595, plain, (spl20_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_35])])).
fof(f2059, plain, (~ spl20_42 | ~ spl20_46), inference(avatar_split_clause, [], [f2054, f642, f625])).
fof(f625, plain, (spl20_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl20_42])])).
fof(f2054, plain, (~ (e11 = op1(e11, e11)) | ~ spl20_46), inference(backward_demodulation, [], [f190, f644])).
fof(f2051, plain, (~ spl20_50 | ~ spl20_51), inference(avatar_contradiction_clause, [], [f2050])).
fof(f2050, plain, ($false | (~ spl20_50 | ~ spl20_51)), inference(subsumption_resolution, [], [f2049, f259])).
fof(f259, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f2049, plain, ((e11 = e12) | (~ spl20_50 | ~ spl20_51)), inference(backward_demodulation, [], [f665, f661])).
fof(f665, plain, ((e12 = op1(e10, e13)) | ~ spl20_51), inference(avatar_component_clause, [], [f663])).
fof(f663, plain, (spl20_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_51])])).
fof(f2035, plain, (~ spl20_37 | ~ spl20_53), inference(avatar_split_clause, [], [f2031, f672, f604])).
fof(f2031, plain, (~ (e10 = op1(e11, e12)) | ~ spl20_53), inference(backward_demodulation, [], [f172, f674])).
fof(f172, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2007, plain, (spl20_146 | ~ spl20_65), inference(avatar_split_clause, [], [f2006, f755, f1259])).
fof(f2006, plain, ((e20 = h4(e13)) | ~ spl20_65), inference(backward_demodulation, [], [f385, f757])).
fof(f2002, plain, (~ spl20_198 | ~ spl20_72), inference(avatar_split_clause, [], [f2000, f784, f1583])).
fof(f784, plain, (spl20_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_72])])).
fof(f2000, plain, (~ (e23 = h3(e13)) | ~ spl20_72), inference(backward_demodulation, [], [f1194, f786])).
fof(f786, plain, ((e23 = op2(e23, e22)) | ~ spl20_72), inference(avatar_component_clause, [], [f784])).
fof(f1194, plain, ~ (op2(e23, e22) = h3(e13)), inference(backward_demodulation, [], [f225, f381])).
fof(f225, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f1984, plain, (~ spl20_76 | ~ spl20_80), inference(avatar_split_clause, [], [f1980, f818, f801])).
fof(f818, plain, (spl20_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_80])])).
fof(f1980, plain, (~ (e23 = op2(e23, e21)) | ~ spl20_80), inference(backward_demodulation, [], [f250, f820])).
fof(f820, plain, ((e23 = op2(e23, e20)) | ~ spl20_80), inference(avatar_component_clause, [], [f818])).
fof(f250, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f1972, plain, (~ spl20_86 | ~ spl20_88), inference(avatar_contradiction_clause, [], [f1971])).
fof(f1971, plain, ($false | (~ spl20_86 | ~ spl20_88)), inference(subsumption_resolution, [], [f1970, f266])).
fof(f1970, plain, ((e21 = e23) | (~ spl20_86 | ~ spl20_88)), inference(backward_demodulation, [], [f854, f846])).
fof(f846, plain, ((e21 = op2(e22, e22)) | ~ spl20_86), inference(avatar_component_clause, [], [f844])).
fof(f844, plain, (spl20_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_86])])).
fof(f854, plain, ((e23 = op2(e22, e22)) | ~ spl20_88), inference(avatar_component_clause, [], [f852])).
fof(f852, plain, (spl20_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_88])])).
fof(f1969, plain, (spl20_198 | ~ spl20_88), inference(avatar_split_clause, [], [f1968, f852, f1583])).
fof(f1968, plain, ((e23 = h3(e13)) | ~ spl20_88), inference(backward_demodulation, [], [f381, f854])).
fof(f1943, plain, (~ spl20_99 | ~ spl20_100), inference(avatar_contradiction_clause, [], [f1942])).
fof(f1942, plain, ($false | (~ spl20_99 | ~ spl20_100)), inference(subsumption_resolution, [], [f1941, f267])).
fof(f1941, plain, ((e22 = e23) | (~ spl20_99 | ~ spl20_100)), inference(backward_demodulation, [], [f905, f901])).
fof(f901, plain, ((e22 = op2(e21, e23)) | ~ spl20_99), inference(avatar_component_clause, [], [f899])).
fof(f899, plain, (spl20_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_99])])).
fof(f1925, plain, (spl20_164 | ~ spl20_105), inference(avatar_split_clause, [], [f1924, f925, f1359])).
fof(f925, plain, (spl20_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_105])])).
fof(f1924, plain, ((e20 = h2(e13)) | ~ spl20_105), inference(backward_demodulation, [], [f377, f927])).
fof(f927, plain, ((e20 = op2(e21, e21)) | ~ spl20_105), inference(avatar_component_clause, [], [f925])).
fof(f377, plain, (op2(e21, e21) = h2(e13)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, e21) = h2(e13)) & (h2(e11) = op2(op2(op2(e21, e21), op2(e21, e21)), op2(e21, e21))) & (h2(e10) = op2(op2(e21, e21), op2(e21, e21))) & (e21 = h2(e12))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax15)).
fof(f1920, plain, (~ spl20_93 | ~ spl20_109), inference(avatar_split_clause, [], [f1914, f942, f874])).
fof(f942, plain, (spl20_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_109])])).
fof(f1914, plain, (~ (e20 = op2(e22, e20)) | ~ spl20_109), inference(backward_demodulation, [], [f211, f944])).
fof(f944, plain, ((e20 = op2(e21, e20)) | ~ spl20_109), inference(avatar_component_clause, [], [f942])).
fof(f1911, plain, (~ spl20_114 | ~ spl20_115), inference(avatar_contradiction_clause, [], [f1910])).
fof(f1910, plain, ($false | (~ spl20_114 | ~ spl20_115)), inference(subsumption_resolution, [], [f1909, f265])).
fof(f1909, plain, ((e21 = e22) | (~ spl20_114 | ~ spl20_115)), inference(backward_demodulation, [], [f969, f965])).
fof(f969, plain, ((e22 = op2(e20, e23)) | ~ spl20_115), inference(avatar_component_clause, [], [f967])).
fof(f967, plain, (spl20_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl20_115])])).
fof(f1895, plain, (~ spl20_101 | ~ spl20_117), inference(avatar_split_clause, [], [f1890, f976, f908])).
fof(f908, plain, (spl20_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_101])])).
fof(f1890, plain, (~ (e20 = op2(e21, e22)) | ~ spl20_117), inference(backward_demodulation, [], [f220, f978])).
fof(f1894, plain, (~ spl20_176 | ~ spl20_117), inference(avatar_split_clause, [], [f1889, f976, f1419])).
fof(f1419, plain, (spl20_176 <=> (e20 = h1(e13))), introduced(avatar_definition, [new_symbols(naming, [spl20_176])])).
fof(f1889, plain, (~ (e20 = h1(e13)) | ~ spl20_117), inference(backward_demodulation, [], [f1178, f978])).
fof(f1178, plain, ~ (op2(e20, e22) = h1(e13)), inference(backward_demodulation, [], [f233, f373])).
fof(f233, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f1886, plain, (~ spl20_117 | ~ spl20_121), inference(avatar_split_clause, [], [f1880, f993, f976])).
fof(f993, plain, (spl20_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_121])])).
fof(f1880, plain, (~ (e20 = op2(e20, e22)) | ~ spl20_121), inference(backward_demodulation, [], [f235, f995])).
fof(f995, plain, ((e20 = op2(e20, e21)) | ~ spl20_121), inference(avatar_component_clause, [], [f993])).
fof(f235, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f1875, plain, (spl20_176 | ~ spl20_125), inference(avatar_split_clause, [], [f1874, f1010, f1419])).
fof(f1010, plain, (spl20_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl20_125])])).
fof(f1874, plain, ((e20 = h1(e13)) | ~ spl20_125), inference(backward_demodulation, [], [f373, f1012])).
fof(f1012, plain, ((e20 = op2(e20, e20)) | ~ spl20_125), inference(avatar_component_clause, [], [f1010])).
fof(f1650, plain, (spl20_153 | spl20_151 | spl20_149 | ~ spl20_198 | ~ spl20_199 | ~ spl20_200 | ~ spl20_201 | ~ spl20_202 | ~ spl20_203 | ~ spl20_204 | ~ spl20_205 | ~ spl20_206 | ~ spl20_207 | ~ spl20_208 | ~ spl20_209 | ~ spl20_210 | ~ spl20_211 | ~ spl20_212 | ~ spl20_213 | ~ spl20_214), inference(avatar_split_clause, [], [f1581, f1647, f1643, f1639, f1635, f1631, f1627, f1623, f1619, f1615, f1611, f1607, f1603, f1599, f1595, f1591, f1587, f1583, f1275, f1288, f1301])).
fof(f1301, plain, (spl20_153 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl20_153])])).
fof(f1288, plain, (spl20_151 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl20_151])])).
fof(f1275, plain, (spl20_149 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl20_149])])).
fof(f1581, plain, (~ (h1(e13) = h3(op1(e10, e10))) | ~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (e21 = h3(op1(e10, e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14), inference(forward_demodulation, [], [f1580, f373])).
fof(f1580, plain, (~ (op2(e20, e20) = h3(op1(e10, e10))) | ~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (e21 = h3(op1(e10, e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14), inference(forward_demodulation, [], [f1579, f1204])).
fof(f1579, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (e21 = h3(op1(e10, e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1578, f1204])).
fof(f1578, plain, (~ (h3(op1(e10, e11)) = op2(h3(e10), e21)) | ~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (e21 = h3(op1(e10, e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1577, f1202])).
fof(f1577, plain, (~ (op2(e20, e22) = h3(op1(e10, e12))) | ~ (e21 = h3(op1(e10, e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1576, f1204])).
fof(f1576, plain, (~ (h3(op1(e10, e12)) = op2(h3(e10), e22)) | ~ (e21 = h3(op1(e10, e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1575, f378])).
fof(f1575, plain, (~ (e21 = h3(op1(e10, e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1574, f1199])).
fof(f1574, plain, (~ (h3(op1(e10, e13)) = op2(e20, h3(e13))) | ~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1573, f1204])).
fof(f1573, plain, (~ (op2(e21, e20) = h3(op1(e11, e10))) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1572, f1202])).
fof(f1572, plain, (~ (h3(op1(e11, e10)) = op2(h3(e11), e20)) | ~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1571, f1204])).
fof(f1571, plain, (~ (h2(e13) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1570, f377])).
fof(f1570, plain, (~ (op2(e21, e21) = h3(op1(e11, e11))) | ~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1569, f1202])).
fof(f1569, plain, (~ (op2(e21, e22) = h3(op1(e11, e12))) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1568, f1202])).
fof(f1568, plain, (~ (h3(op1(e11, e12)) = op2(h3(e11), e22)) | ~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1567, f378])).
fof(f1567, plain, (~ (h3(op1(e11, e13)) = op2(e21, h3(e13))) | ~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1566, f1202])).
fof(f1566, plain, (~ (op2(e22, e20) = h3(op1(e12, e10))) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1565, f378])).
fof(f1565, plain, (~ (h3(op1(e12, e10)) = op2(h3(e12), e20)) | ~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1564, f1204])).
fof(f1564, plain, (~ (op2(e22, e21) = h3(op1(e12, e11))) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1563, f378])).
fof(f1563, plain, (~ (h3(op1(e12, e11)) = op2(h3(e12), e21)) | ~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1562, f1202])).
fof(f1562, plain, (~ (h3(e13) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1561, f381])).
fof(f1561, plain, (~ (op2(e22, e22) = h3(op1(e12, e12))) | ~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1560, f378])).
fof(f1560, plain, (~ (h3(op1(e12, e13)) = op2(e22, h3(e13))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1559, f378])).
fof(f1559, plain, (~ (h3(op1(e13, e10)) = op2(h3(e13), e20)) | ~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1558, f1204])).
fof(f1558, plain, (~ (h3(op1(e13, e11)) = op2(h3(e13), e21)) | ~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1557, f1202])).
fof(f1557, plain, (~ (h3(op1(e13, e12)) = op2(h3(e13), e22)) | ~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f1556, f378])).
fof(f1556, plain, (~ (e20 = h3(op1(e13, e13))) | ~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(forward_demodulation, [], [f445, f1198])).
fof(f445, plain, (~ (e23 = h3(e13)) | sP16 | sP15 | sP14 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))), inference(cnf_transformation, [], [f43])).
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
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', co1)).
fof(f1312, plain, ~ spl20_153, inference(avatar_split_clause, [], [f1311, f1301])).
fof(f1311, plain, ~ sP14, inference(subsumption_resolution, [], [f406, f1204])).
fof(f406, plain, (~ (e20 = h3(e10)) | ~ sP14), inference(cnf_transformation, [], [f57])).
fof(f57, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP14), inference(nnf_transformation, [], [f37])).
fof(f1298, plain, ~ spl20_151, inference(avatar_split_clause, [], [f1297, f1288])).
fof(f1297, plain, ~ sP15, inference(subsumption_resolution, [], [f403, f1202])).
fof(f403, plain, (~ (e21 = h3(e11)) | ~ sP15), inference(cnf_transformation, [], [f56])).
fof(f56, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP15), inference(nnf_transformation, [], [f38])).
fof(f1284, plain, ~ spl20_149, inference(avatar_split_clause, [], [f1283, f1275])).
fof(f1283, plain, ~ sP16, inference(subsumption_resolution, [], [f400, f378])).
fof(f400, plain, (~ (e22 = h3(e12)) | ~ sP16), inference(cnf_transformation, [], [f55])).
fof(f55, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP16), inference(nnf_transformation, [], [f39])).
fof(f1172, plain, spl20_88, inference(avatar_split_clause, [], [f369, f852])).
fof(f369, plain, (e23 = op2(e22, e22)), inference(cnf_transformation, [], [f13])).
fof(f1170, plain, spl20_24, inference(avatar_split_clause, [], [f366, f548])).
fof(f366, plain, (e13 = op1(e12, e12)), inference(cnf_transformation, [], [f12])).
fof(f1169, plain, (spl20_136 | spl20_110 | spl20_95 | spl20_80), inference(avatar_split_clause, [], [f332, f818, f882, f946, f1133])).
fof(f1133, plain, (spl20_136 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl20_136])])).
fof(f332, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e21, e20)) | sP4), inference(cnf_transformation, [], [f30])).
fof(f30, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e22, e23))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e21, e23))) | sP7) & ((~ (e22 = op2(e22, e23)) & (e23 = op2(e23, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e21, e22))) | sP6) & ((~ (e21 = op2(e21, e23)) & (e23 = op2(e23, e21))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e22, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | sP5) & ((~ (e20 = op2(e20, e23)) & (e23 = op2(e23, e20))) | (~ (e20 = op2(e20, e22)) & (e22 = op2(e22, e20))) | (~ (e20 = op2(e20, e21)) & (e21 = op2(e21, e20))) | sP4)), inference(definition_folding, [], [f11, e29, e28, e27, e26])).
fof(f26, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP4), inference(usedef, [], [e26])).
fof(e26, plain, (sP4 <=> (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f27, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP5), inference(usedef, [], [e27])).
fof(e27, plain, (sP5 <=> (~ (e21 = op2(e21, e20)) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f28, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP6), inference(usedef, [], [e28])).
fof(e28, plain, (sP6 <=> (~ (e22 = op2(e22, e20)) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f29, plain, ((~ (e23 = op2(e23, e20)) & (e20 = op2(e20, e23))) | ~ sP7), inference(usedef, [], [e29])).
fof(e29, plain, (sP7 <=> (~ (e23 = op2(e23, e20)) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f11, plain, (((~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e23 = op2(e23, e22)) & (e22 = op2(e22, e23))) | (~ (e23 = op2(e23, e21)) & (e21 = op2(e21, e23))) | (~ (e23 = op2(e23, e20)) & (e20 = op2(e20, e23)))) & ((~ (e22 = op2(e22, e23)) & (e23 = op2(e23, e22))) | (~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e22 = op2(e22, e21)) & (e21 = op2(e21, e22))) | (~ (e22 = op2(e22, e20)) & (e20 = op2(e20, e22)))) & ((~ (e21 = op2(e21, e23)) & (e23 = op2(e23, e21))) | (~ (e21 = op2(e21, e22)) & (e22 = op2(e22, e21))) | (~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e21 = op2(e21, e20)) & (e20 = op2(e20, e21)))) & ((~ (e20 = op2(e20, e23)) & (e23 = op2(e23, e20))) | (~ (e20 = op2(e20, e22)) & (e22 = op2(e22, e20))) | (~ (e20 = op2(e20, e21)) & (e21 = op2(e21, e20))) | (~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax11)).
fof(f1161, plain, (spl20_135 | spl20_106 | spl20_91 | spl20_76), inference(avatar_split_clause, [], [f340, f801, f865, f929, f1127])).
fof(f1127, plain, (spl20_135 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl20_135])])).
fof(f929, plain, (spl20_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl20_106])])).
fof(f340, plain, ((e23 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e21, e21)) | sP5), inference(cnf_transformation, [], [f30])).
fof(f1160, plain, (spl20_135 | ~ spl20_106 | spl20_91 | spl20_76), inference(avatar_split_clause, [], [f341, f801, f865, f929, f1127])).
fof(f341, plain, ((e23 = op2(e23, e21)) | (e22 = op2(e22, e21)) | ~ (e21 = op2(e21, e21)) | sP5), inference(cnf_transformation, [], [f30])).
fof(f1153, plain, (spl20_134 | spl20_102 | spl20_87 | spl20_72), inference(avatar_split_clause, [], [f348, f784, f848, f912, f1121])).
fof(f1121, plain, (spl20_134 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl20_134])])).
fof(f848, plain, (spl20_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl20_87])])).
fof(f348, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e22)) | sP6), inference(cnf_transformation, [], [f30])).
fof(f1151, plain, (spl20_134 | spl20_102 | ~ spl20_87 | spl20_72), inference(avatar_split_clause, [], [f350, f784, f848, f912, f1121])).
fof(f350, plain, ((e23 = op2(e23, e22)) | ~ (e22 = op2(e22, e22)) | (e21 = op2(e21, e22)) | sP6), inference(cnf_transformation, [], [f30])).
fof(f1137, plain, (~ spl20_136 | spl20_125), inference(avatar_split_clause, [], [f330, f1010, f1133])).
fof(f330, plain, ((e20 = op2(e20, e20)) | ~ sP4), inference(cnf_transformation, [], [f51])).
fof(f51, plain, ((~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP4), inference(nnf_transformation, [], [f26])).
fof(f1131, plain, (~ spl20_135 | spl20_121), inference(avatar_split_clause, [], [f328, f993, f1127])).
fof(f328, plain, ((e20 = op2(e20, e21)) | ~ sP5), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e21 = op2(e21, e20)) & (e20 = op2(e20, e21))) | ~ sP5), inference(nnf_transformation, [], [f27])).
fof(f1125, plain, (~ spl20_134 | spl20_117), inference(avatar_split_clause, [], [f326, f976, f1121])).
fof(f326, plain, ((e20 = op2(e20, e22)) | ~ sP6), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e22 = op2(e22, e20)) & (e20 = op2(e20, e22))) | ~ sP6), inference(nnf_transformation, [], [f28])).
fof(f1113, plain, (spl20_132 | spl20_46 | spl20_31 | spl20_16), inference(avatar_split_clause, [], [f292, f514, f578, f642, f1077])).
fof(f1077, plain, (spl20_132 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl20_132])])).
fof(f292, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e11, e10)) | sP0), inference(cnf_transformation, [], [f25])).
fof(f25, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e12, e13))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e11, e13))) | sP3) & ((~ (e12 = op1(e12, e13)) & (e13 = op1(e13, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e11, e12))) | sP2) & ((~ (e11 = op1(e11, e13)) & (e13 = op1(e13, e11))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e12, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | sP1) & ((~ (e10 = op1(e10, e13)) & (e13 = op1(e13, e10))) | (~ (e10 = op1(e10, e12)) & (e12 = op1(e12, e10))) | (~ (e10 = op1(e10, e11)) & (e11 = op1(e11, e10))) | sP0)), inference(definition_folding, [], [f10, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e11 = op1(e11, e10)) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e12 = op1(e12, e10)) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e13 = op1(e13, e10)) & (e10 = op1(e10, e13))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e13 = op1(e13, e10)) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f10, plain, (((~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e13 = op1(e13, e12)) & (e12 = op1(e12, e13))) | (~ (e13 = op1(e13, e11)) & (e11 = op1(e11, e13))) | (~ (e13 = op1(e13, e10)) & (e10 = op1(e10, e13)))) & ((~ (e12 = op1(e12, e13)) & (e13 = op1(e13, e12))) | (~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e12 = op1(e12, e11)) & (e11 = op1(e11, e12))) | (~ (e12 = op1(e12, e10)) & (e10 = op1(e10, e12)))) & ((~ (e11 = op1(e11, e13)) & (e13 = op1(e13, e11))) | (~ (e11 = op1(e11, e12)) & (e12 = op1(e12, e11))) | (~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e11 = op1(e11, e10)) & (e10 = op1(e10, e11)))) & ((~ (e10 = op1(e10, e13)) & (e13 = op1(e13, e10))) | (~ (e10 = op1(e10, e12)) & (e12 = op1(e12, e10))) | (~ (e10 = op1(e10, e11)) & (e11 = op1(e11, e10))) | (~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax10)).
fof(f1105, plain, (spl20_131 | spl20_42 | spl20_27 | spl20_12), inference(avatar_split_clause, [], [f300, f497, f561, f625, f1071])).
fof(f1071, plain, (spl20_131 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl20_131])])).
fof(f300, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e11, e11)) | sP1), inference(cnf_transformation, [], [f25])).
fof(f1104, plain, (spl20_131 | ~ spl20_42 | spl20_27 | spl20_12), inference(avatar_split_clause, [], [f301, f497, f561, f625, f1071])).
fof(f301, plain, ((e13 = op1(e13, e11)) | (e12 = op1(e12, e11)) | ~ (e11 = op1(e11, e11)) | sP1), inference(cnf_transformation, [], [f25])).
fof(f1097, plain, (spl20_130 | spl20_38 | spl20_23 | spl20_8), inference(avatar_split_clause, [], [f308, f480, f544, f608, f1065])).
fof(f1065, plain, (spl20_130 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl20_130])])).
fof(f544, plain, (spl20_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl20_23])])).
fof(f308, plain, ((e13 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e12)) | sP2), inference(cnf_transformation, [], [f25])).
fof(f1095, plain, (spl20_130 | spl20_38 | ~ spl20_23 | spl20_8), inference(avatar_split_clause, [], [f310, f480, f544, f608, f1065])).
fof(f310, plain, ((e13 = op1(e13, e12)) | ~ (e12 = op1(e12, e12)) | (e11 = op1(e11, e12)) | sP2), inference(cnf_transformation, [], [f25])).
fof(f1081, plain, (~ spl20_132 | spl20_61), inference(avatar_split_clause, [], [f290, f706, f1077])).
fof(f290, plain, ((e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1075, plain, (~ spl20_131 | spl20_57), inference(avatar_split_clause, [], [f288, f689, f1071])).
fof(f288, plain, ((e10 = op1(e10, e11)) | ~ sP1), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (e11 = op1(e11, e10)) & (e10 = op1(e10, e11))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1069, plain, (~ spl20_130 | spl20_53), inference(avatar_split_clause, [], [f286, f672, f1065])).
fof(f286, plain, ((e10 = op1(e10, e12)) | ~ sP2), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (e12 = op1(e12, e10)) & (e10 = op1(e10, e12))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1053, plain, (spl20_127 | spl20_123 | spl20_119 | spl20_115), inference(avatar_split_clause, [], [f132, f967, f984, f1001, f1018])).
fof(f132, plain, ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax4)).
fof(f1052, plain, (spl20_127 | spl20_111 | spl20_95 | spl20_79), inference(avatar_split_clause, [], [f133, f814, f882, f950, f1018])).
fof(f133, plain, ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1051, plain, (spl20_128 | spl20_124 | spl20_120 | spl20_116), inference(avatar_split_clause, [], [f134, f971, f988, f1005, f1022])).
fof(f134, plain, ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f1049, plain, (spl20_109 | spl20_105 | spl20_101 | spl20_97), inference(avatar_split_clause, [], [f136, f891, f908, f925, f942])).
fof(f136, plain, ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1039, plain, (spl20_94 | spl20_90 | spl20_86 | spl20_82), inference(avatar_split_clause, [], [f146, f827, f844, f861, f878])).
fof(f146, plain, ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1038, plain, (spl20_118 | spl20_102 | spl20_86 | spl20_70), inference(avatar_split_clause, [], [f147, f776, f844, f912, f980])).
fof(f147, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1028, plain, (spl20_115 | spl20_99 | spl20_83 | spl20_67), inference(avatar_split_clause, [], [f157, f763, f831, f899, f967])).
fof(f157, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1026, plain, (spl20_116 | spl20_100 | spl20_84 | spl20_68), inference(avatar_split_clause, [], [f159, f767, f835, f903, f971])).
fof(f159, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f991, plain, (spl20_117 | spl20_118 | spl20_119 | spl20_120), inference(avatar_split_clause, [], [f114, f988, f984, f980, f976])).
fof(f114, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax3)).
fof(f923, plain, (spl20_101 | spl20_102 | spl20_103 | spl20_104), inference(avatar_split_clause, [], [f118, f920, f916, f912, f908])).
fof(f118, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f889, plain, (spl20_93 | spl20_94 | spl20_95 | spl20_96), inference(avatar_split_clause, [], [f120, f886, f882, f878, f874])).
fof(f120, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f787, plain, (spl20_69 | spl20_70 | spl20_71 | spl20_72), inference(avatar_split_clause, [], [f126, f784, f780, f776, f772])).
fof(f126, plain, ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))), inference(cnf_transformation, [], [f3])).
fof(f752, plain, (spl20_61 | spl20_45 | spl20_29 | spl20_13), inference(avatar_split_clause, [], [f81, f502, f570, f638, f706])).
fof(f81, plain, ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax2)).
fof(f746, plain, (spl20_64 | spl20_48 | spl20_32 | spl20_16), inference(avatar_split_clause, [], [f87, f514, f582, f650, f718])).
fof(f87, plain, ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f742, plain, (spl20_58 | spl20_42 | spl20_26 | spl20_10), inference(avatar_split_clause, [], [f91, f489, f557, f625, f693])).
fof(f91, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f736, plain, (spl20_53 | spl20_37 | spl20_21 | spl20_5), inference(avatar_split_clause, [], [f97, f468, f536, f604, f672])).
fof(f97, plain, ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f734, plain, (spl20_54 | spl20_38 | spl20_22 | spl20_6), inference(avatar_split_clause, [], [f99, f472, f540, f608, f676])).
fof(f99, plain, ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f725, plain, (spl20_15 | spl20_11 | spl20_7 | spl20_3), inference(avatar_split_clause, [], [f108, f459, f476, f493, f510])).
fof(f108, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f724, plain, (spl20_51 | spl20_35 | spl20_19 | spl20_3), inference(avatar_split_clause, [], [f109, f459, f527, f595, f663])).
fof(f109, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f722, plain, (spl20_52 | spl20_36 | spl20_20 | spl20_4), inference(avatar_split_clause, [], [f111, f463, f531, f599, f667])).
fof(f111, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f704, plain, (spl20_57 | spl20_58 | spl20_59 | spl20_60), inference(avatar_split_clause, [], [f65, f701, f697, f693, f689])).
fof(f65, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG110+1.p', ax1)).
fof(f636, plain, (spl20_41 | spl20_42 | spl20_43 | spl20_44), inference(avatar_split_clause, [], [f69, f633, f629, f625, f621])).
fof(f69, plain, ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f619, plain, (spl20_37 | spl20_38 | spl20_39 | spl20_40), inference(avatar_split_clause, [], [f70, f616, f612, f608, f604])).
fof(f70, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).