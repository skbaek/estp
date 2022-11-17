fof(f4488, plain, $false, inference(avatar_sat_refutation, [], [f431, f465, f516, f533, f567, f601, f635, f652, f669, f670, f676, f678, f680, f688, f690, f695, f696, f701, f769, f786, f837, f871, f905, f939, f956, f974, f980, f983, f984, f985, f987, f991, f995, f997, f1003, f1005, f1014, f1024, f1029, f1038, f1043, f1048, f1062, f1067, f1087, f1097, f1106, f1111, f1121, f1130, f1135, f1145, f1154, f1159, f1164, f1174, f1184, f1190, f1192, f1193, f1195, f1247, f1268, f1286, f1493, f1894, f1911, f1918, f1925, f1928, f1940, f1985, f1990, f1991, f2035, f2038, f2059, f2078, f2098, f2131, f2148, f2164, f2184, f2198, f2200, f2208, f2257, f2263, f2304, f2316, f2328, f2335, f2344, f2370, f2405, f2413, f2457, f2479, f2486, f2509, f2514, f2519, f2541, f2550, f2557, f2562, f2567, f2595, f2597, f2628, f2634, f2638, f2643, f2647, f2660, f2668, f2709, f2718, f2750, f2751, f2769, f2775, f2782, f2793, f2801, f2819, f2850, f2852, f2889, f2893, f2927, f2958, f2962, f2967, f2970, f2991, f2995, f3011, f3016, f3035, f3045, f3046, f3055, f3068, f3105, f3114, f3136, f3162, f3173, f3178, f3181, f3185, f3192, f3261, f3262, f3269, f3287, f3313, f3315, f3318, f3321, f3330, f3349, f3362, f3395, f3406, f3434, f3446, f3454, f3457, f3463, f3505, f3515, f3527, f3550, f3581, f3609, f3612, f3640, f3644, f3647, f3667, f3671, f3675, f3734, f3775, f3802, f3825, f3850, f3871, f3894, f3921, f3950, f3973, f3989, f4027, f4031, f4050, f4077, f4078, f4120, f4127, f4137, f4163, f4165, f4228, f4240, f4286, f4287, f4303, f4325, f4366, f4393, f4395, f4424, f4463])).
fof(f4463, plain, (~ spl18_7 | ~ spl18_15), inference(avatar_split_clause, [], [f4456, f458, f424])).
fof(f424, plain, (spl18_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_7])])).
fof(f458, plain, (spl18_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_15])])).
fof(f4456, plain, (~ (e12 = op1(e13, e12)) | ~ spl18_15), inference(backward_demodulation, [], [f199, f460])).
fof(f460, plain, ((e12 = op1(e13, e10)) | ~ spl18_15), inference(avatar_component_clause, [], [f458])).
fof(f199, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax5)).
fof(f4424, plain, (~ spl18_17 | ~ spl18_29), inference(avatar_split_clause, [], [f4417, f518, f467])).
fof(f467, plain, (spl18_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_17])])).
fof(f518, plain, (spl18_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_29])])).
fof(f4417, plain, (~ (e10 = op1(e12, e13)) | ~ spl18_29), inference(backward_demodulation, [], [f194, f520])).
fof(f520, plain, ((e10 = op1(e12, e10)) | ~ spl18_29), inference(avatar_component_clause, [], [f518])).
fof(f194, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4395, plain, (spl18_23 | ~ spl18_39 | ~ spl18_136), inference(avatar_contradiction_clause, [], [f4394])).
fof(f4394, plain, ($false | (spl18_23 | ~ spl18_39 | ~ spl18_136)), inference(subsumption_resolution, [], [f4389, f493])).
fof(f493, plain, (~ (e12 = op1(e12, e12)) | spl18_23), inference(avatar_component_clause, [], [f492])).
fof(f492, plain, (spl18_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_23])])).
fof(f4389, plain, ((e12 = op1(e12, e12)) | (~ spl18_39 | ~ spl18_136)), inference(backward_demodulation, [], [f1042, f562])).
fof(f562, plain, ((e12 = op1(e11, e12)) | ~ spl18_39), inference(avatar_component_clause, [], [f560])).
fof(f560, plain, (spl18_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_39])])).
fof(f1042, plain, ((e12 = op1(op1(e11, e12), op1(e11, e12))) | ~ spl18_136), inference(avatar_component_clause, [], [f1040])).
fof(f1040, plain, (spl18_136 <=> (e12 = op1(op1(e11, e12), op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_136])])).
fof(f4393, plain, (~ spl18_7 | ~ spl18_39), inference(avatar_split_clause, [], [f4386, f560, f424])).
fof(f4386, plain, (~ (e12 = op1(e13, e12)) | ~ spl18_39), inference(backward_demodulation, [], [f172, f562])).
fof(f172, plain, ~ (op1(e11, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f4366, plain, (~ spl18_37 | ~ spl18_53), inference(avatar_split_clause, [], [f4356, f620, f552])).
fof(f552, plain, (spl18_37 <=> (e10 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_37])])).
fof(f620, plain, (spl18_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_53])])).
fof(f4356, plain, (~ (e10 = op1(e11, e12)) | ~ spl18_53), inference(backward_demodulation, [], [f168, f622])).
fof(f622, plain, ((e10 = op1(e10, e12)) | ~ spl18_53), inference(avatar_component_clause, [], [f620])).
fof(f168, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4325, plain, (~ spl18_16 | ~ spl18_64), inference(avatar_split_clause, [], [f4318, f666, f462])).
fof(f462, plain, (spl18_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_16])])).
fof(f666, plain, (spl18_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl18_64])])).
fof(f4318, plain, (~ (e13 = op1(e13, e10)) | ~ spl18_64), inference(backward_demodulation, [], [f158, f668])).
fof(f668, plain, ((op1(e10, e10) = e13) | ~ spl18_64), inference(avatar_component_clause, [], [f666])).
fof(f158, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f4303, plain, (~ spl18_94 | ~ spl18_110), inference(avatar_split_clause, [], [f4297, f894, f826])).
fof(f826, plain, (spl18_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_94])])).
fof(f894, plain, (spl18_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_110])])).
fof(f4297, plain, (~ (e21 = op2(e22, e20)) | ~ spl18_110), inference(backward_demodulation, [], [f207, f896])).
fof(f896, plain, ((e21 = op2(e21, e20)) | ~ spl18_110), inference(avatar_component_clause, [], [f894])).
fof(f207, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax6)).
fof(f4287, plain, (~ spl18_111 | ~ spl18_186), inference(avatar_split_clause, [], [f4277, f1339, f898])).
fof(f898, plain, (spl18_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_111])])).
fof(f1339, plain, (spl18_186 <=> (e22 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_186])])).
fof(f4277, plain, (~ (e22 = op2(e21, e20)) | ~ spl18_186), inference(backward_demodulation, [], [f1208, f1340])).
fof(f1340, plain, ((e22 = h2(e11)) | ~ spl18_186), inference(avatar_component_clause, [], [f1339])).
fof(f1208, plain, ~ (op2(e21, e20) = h2(e11)), inference(backward_demodulation, [], [f234, f324])).
fof(f324, plain, (op2(e21, e21) = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((h2(e12) = op2(op2(e21, op2(e21, e21)), e21)) & (op2(e21, e21) = h2(e11)) & (h2(e10) = op2(e21, op2(e21, e21))) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax15)).
fof(f234, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4286, plain, (~ spl18_91 | ~ spl18_186), inference(avatar_split_clause, [], [f4276, f1339, f813])).
fof(f813, plain, (spl18_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_91])])).
fof(f4276, plain, (~ (e22 = op2(e22, e21)) | ~ spl18_186), inference(backward_demodulation, [], [f1206, f1340])).
fof(f1206, plain, ~ (op2(e22, e21) = h2(e11)), inference(backward_demodulation, [], [f213, f324])).
fof(f213, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f4240, plain, (~ spl18_20 | ~ spl18_24), inference(avatar_split_clause, [], [f4238, f496, f479])).
fof(f479, plain, (spl18_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_20])])).
fof(f496, plain, (spl18_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_24])])).
fof(f4238, plain, (~ (e13 = op1(e12, e13)) | ~ spl18_24), inference(backward_demodulation, [], [f197, f498])).
fof(f498, plain, ((e13 = op1(e12, e12)) | ~ spl18_24), inference(avatar_component_clause, [], [f496])).
fof(f197, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4228, plain, (~ spl18_33 | ~ spl18_37), inference(avatar_split_clause, [], [f4219, f552, f535])).
fof(f535, plain, (spl18_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_33])])).
fof(f4219, plain, (~ (e10 = op1(e11, e13)) | ~ spl18_37), inference(backward_demodulation, [], [f191, f554])).
fof(f554, plain, ((e10 = op1(e11, e12)) | ~ spl18_37), inference(avatar_component_clause, [], [f552])).
fof(f191, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4165, plain, (spl18_24 | ~ spl18_51 | ~ spl18_140), inference(avatar_split_clause, [], [f4164, f1059, f611, f496])).
fof(f611, plain, (spl18_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_51])])).
fof(f1059, plain, (spl18_140 <=> (e13 = op1(op1(e10, e13), op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_140])])).
fof(f4164, plain, ((e13 = op1(e12, e12)) | (~ spl18_51 | ~ spl18_140)), inference(forward_demodulation, [], [f1061, f613])).
fof(f613, plain, ((e12 = op1(e10, e13)) | ~ spl18_51), inference(avatar_component_clause, [], [f611])).
fof(f1061, plain, ((e13 = op1(op1(e10, e13), op1(e10, e13))) | ~ spl18_140), inference(avatar_component_clause, [], [f1059])).
fof(f4163, plain, (spl18_43 | ~ spl18_54 | ~ spl18_141), inference(avatar_split_clause, [], [f4162, f1064, f624, f577])).
fof(f577, plain, (spl18_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_43])])).
fof(f624, plain, (spl18_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_54])])).
fof(f1064, plain, (spl18_141 <=> (e12 = op1(op1(e10, e12), op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_141])])).
fof(f4162, plain, ((e12 = op1(e11, e11)) | (~ spl18_54 | ~ spl18_141)), inference(forward_demodulation, [], [f1066, f626])).
fof(f626, plain, ((e11 = op1(e10, e12)) | ~ spl18_54), inference(avatar_component_clause, [], [f624])).
fof(f1066, plain, ((e12 = op1(op1(e10, e12), op1(e10, e12))) | ~ spl18_141), inference(avatar_component_clause, [], [f1064])).
fof(f4137, plain, (~ spl18_254 | ~ spl18_124), inference(avatar_split_clause, [], [f4136, f953, f1749])).
fof(f1749, plain, (spl18_254 <=> (e23 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_254])])).
fof(f953, plain, (spl18_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_124])])).
fof(f4136, plain, (~ (e23 = h2(e11)) | ~ spl18_124), inference(forward_demodulation, [], [f1205, f955])).
fof(f955, plain, ((e23 = op2(e20, e21)) | ~ spl18_124), inference(avatar_component_clause, [], [f953])).
fof(f1205, plain, ~ (op2(e20, e21) = h2(e11)), inference(backward_demodulation, [], [f210, f324])).
fof(f210, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f4127, plain, (spl18_107 | ~ spl18_118 | ~ spl18_160), inference(avatar_split_clause, [], [f3679, f1156, f928, f881])).
fof(f881, plain, (spl18_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_107])])).
fof(f928, plain, (spl18_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_118])])).
fof(f1156, plain, (spl18_160 <=> (e22 = op2(op2(e20, e22), op2(e20, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_160])])).
fof(f3679, plain, ((e22 = op2(e21, e21)) | (~ spl18_118 | ~ spl18_160)), inference(backward_demodulation, [], [f1158, f930])).
fof(f930, plain, ((e21 = op2(e20, e22)) | ~ spl18_118), inference(avatar_component_clause, [], [f928])).
fof(f1158, plain, ((e22 = op2(op2(e20, e22), op2(e20, e22))) | ~ spl18_160), inference(avatar_component_clause, [], [f1156])).
fof(f4120, plain, (~ spl18_9 | ~ spl18_11), inference(avatar_contradiction_clause, [], [f4119])).
fof(f4119, plain, ($false | (~ spl18_9 | ~ spl18_11)), inference(subsumption_resolution, [], [f4118, f253])).
fof(f253, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax7)).
fof(f4118, plain, ((e10 = e12) | (~ spl18_9 | ~ spl18_11)), inference(forward_demodulation, [], [f443, f435])).
fof(f435, plain, ((e10 = op1(e13, e11)) | ~ spl18_9), inference(avatar_component_clause, [], [f433])).
fof(f433, plain, (spl18_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_9])])).
fof(f443, plain, ((e12 = op1(e13, e11)) | ~ spl18_11), inference(avatar_component_clause, [], [f441])).
fof(f441, plain, (spl18_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_11])])).
fof(f4078, plain, (~ spl18_36 | ~ spl18_40), inference(avatar_split_clause, [], [f4072, f564, f547])).
fof(f547, plain, (spl18_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_36])])).
fof(f564, plain, (spl18_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_40])])).
fof(f4072, plain, (~ (e13 = op1(e11, e13)) | ~ spl18_40), inference(backward_demodulation, [], [f191, f566])).
fof(f566, plain, ((e13 = op1(e11, e12)) | ~ spl18_40), inference(avatar_component_clause, [], [f564])).
fof(f4077, plain, (~ spl18_24 | ~ spl18_40), inference(avatar_split_clause, [], [f4071, f564, f496])).
fof(f4071, plain, (~ (e13 = op1(e12, e12)) | ~ spl18_40), inference(backward_demodulation, [], [f171, f566])).
fof(f171, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f4050, plain, (~ spl18_43 | ~ spl18_47), inference(avatar_split_clause, [], [f4044, f594, f577])).
fof(f594, plain, (spl18_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_47])])).
fof(f4044, plain, (~ (e12 = op1(e11, e11)) | ~ spl18_47), inference(backward_demodulation, [], [f186, f596])).
fof(f596, plain, ((e12 = op1(e11, e10)) | ~ spl18_47), inference(avatar_component_clause, [], [f594])).
fof(f186, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f4031, plain, (~ spl18_9 | ~ spl18_61 | ~ spl18_146), inference(avatar_contradiction_clause, [], [f4030])).
fof(f4030, plain, ($false | (~ spl18_9 | ~ spl18_61 | ~ spl18_146)), inference(subsumption_resolution, [], [f4029, f252])).
fof(f252, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f4029, plain, ((e10 = e11) | (~ spl18_9 | ~ spl18_61 | ~ spl18_146)), inference(forward_demodulation, [], [f4028, f656])).
fof(f656, plain, ((e10 = op1(e10, e10)) | ~ spl18_61), inference(avatar_component_clause, [], [f654])).
fof(f654, plain, (spl18_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_61])])).
fof(f4028, plain, ((op1(e10, e10) = e11) | (~ spl18_9 | ~ spl18_146)), inference(forward_demodulation, [], [f1091, f435])).
fof(f1091, plain, ((e11 = op1(op1(e13, e11), op1(e13, e11))) | ~ spl18_146), inference(avatar_component_clause, [], [f1089])).
fof(f1089, plain, (spl18_146 <=> (e11 = op1(op1(e13, e11), op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_146])])).
fof(f4027, plain, (spl18_1 | ~ spl18_16 | ~ spl18_147), inference(avatar_contradiction_clause, [], [f4026])).
fof(f4026, plain, ($false | (spl18_1 | ~ spl18_16 | ~ spl18_147)), inference(subsumption_resolution, [], [f4025, f400])).
fof(f400, plain, (~ (e10 = op1(e13, e13)) | spl18_1), inference(avatar_component_clause, [], [f399])).
fof(f399, plain, (spl18_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_1])])).
fof(f4025, plain, ((e10 = op1(e13, e13)) | (~ spl18_16 | ~ spl18_147)), inference(forward_demodulation, [], [f1096, f464])).
fof(f464, plain, ((e13 = op1(e13, e10)) | ~ spl18_16), inference(avatar_component_clause, [], [f462])).
fof(f1096, plain, ((e10 = op1(op1(e13, e10), op1(e13, e10))) | ~ spl18_147), inference(avatar_component_clause, [], [f1094])).
fof(f1094, plain, (spl18_147 <=> (e10 = op1(op1(e13, e10), op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_147])])).
fof(f3989, plain, (~ spl18_61 | ~ spl18_125 | spl18_215), inference(avatar_contradiction_clause, [], [f3988])).
fof(f3988, plain, ($false | (~ spl18_61 | ~ spl18_125 | spl18_215)), inference(subsumption_resolution, [], [f3987, f3654])).
fof(f3654, plain, ((e20 = h1(e11)) | ~ spl18_125), inference(backward_demodulation, [], [f320, f960])).
fof(f960, plain, ((e20 = op2(e20, e20)) | ~ spl18_125), inference(avatar_component_clause, [], [f958])).
fof(f958, plain, (spl18_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_125])])).
fof(f320, plain, (op2(e20, e20) = h1(e11)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((h1(e12) = op2(op2(e20, op2(e20, e20)), e20)) & (op2(e20, e20) = h1(e11)) & (h1(e10) = op2(e20, op2(e20, e20))) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax14)).
fof(f3987, plain, (~ (e20 = h1(e11)) | (~ spl18_61 | spl18_215)), inference(forward_demodulation, [], [f3986, f1237])).
fof(f1237, plain, (e20 = h4(e10)), inference(forward_demodulation, [], [f1236, f1234])).
fof(f1234, plain, (e20 = op2(e23, h4(e11))), inference(backward_demodulation, [], [f315, f332])).
fof(f332, plain, (op2(e23, e23) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(op2(e23, op2(e23, e23)), e23) = h4(e12)) & (op2(e23, e23) = h4(e11)) & (op2(e23, op2(e23, e23)) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax17)).
fof(f315, plain, (e20 = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e22 = op2(op2(e23, op2(e23, e23)), e23)) & (e21 = op2(e23, e23)) & (e20 = op2(e23, op2(e23, e23)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax13)).
fof(f1236, plain, (h4(e10) = op2(e23, h4(e11))), inference(forward_demodulation, [], [f331, f332])).
fof(f331, plain, (op2(e23, op2(e23, e23)) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f3986, plain, (~ (h1(e11) = h4(e10)) | (~ spl18_61 | spl18_215)), inference(forward_demodulation, [], [f1492, f656])).
fof(f1492, plain, (~ (h1(e11) = h4(op1(e10, e10))) | spl18_215), inference(avatar_component_clause, [], [f1490])).
fof(f1490, plain, (spl18_215 <=> (h1(e11) = h4(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_215])])).
fof(f3973, plain, (~ spl18_46 | ~ spl18_110 | ~ spl18_172 | spl18_211), inference(avatar_contradiction_clause, [], [f3972])).
fof(f3972, plain, ($false | (~ spl18_46 | ~ spl18_110 | ~ spl18_172 | spl18_211)), inference(subsumption_resolution, [], [f3971, f896])).
fof(f3971, plain, (~ (e21 = op2(e21, e20)) | (~ spl18_46 | ~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f3970, f1266])).
fof(f1266, plain, ((e21 = h4(e11)) | ~ spl18_172), inference(avatar_component_clause, [], [f1265])).
fof(f1265, plain, (spl18_172 <=> (e21 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_172])])).
fof(f3970, plain, (~ (op2(e21, e20) = h4(e11)) | (~ spl18_46 | ~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f3969, f592])).
fof(f592, plain, ((e11 = op1(e11, e10)) | ~ spl18_46), inference(avatar_component_clause, [], [f590])).
fof(f590, plain, (spl18_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_46])])).
fof(f3969, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | (~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f1476, f1266])).
fof(f1476, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | spl18_211), inference(avatar_component_clause, [], [f1474])).
fof(f1474, plain, (spl18_211 <=> (h4(op1(e11, e10)) = op2(h4(e11), e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_211])])).
fof(f3950, plain, (~ spl18_54 | ~ spl18_118 | ~ spl18_168 | ~ spl18_172 | spl18_213), inference(avatar_contradiction_clause, [], [f3949])).
fof(f3949, plain, ($false | (~ spl18_54 | ~ spl18_118 | ~ spl18_168 | ~ spl18_172 | spl18_213)), inference(subsumption_resolution, [], [f3948, f930])).
fof(f3948, plain, (~ (e21 = op2(e20, e22)) | (~ spl18_54 | ~ spl18_168 | ~ spl18_172 | spl18_213)), inference(forward_demodulation, [], [f3947, f1266])).
fof(f3947, plain, (~ (op2(e20, e22) = h4(e11)) | (~ spl18_54 | ~ spl18_168 | spl18_213)), inference(forward_demodulation, [], [f3946, f626])).
fof(f3946, plain, (~ (op2(e20, e22) = h4(op1(e10, e12))) | (~ spl18_168 | spl18_213)), inference(forward_demodulation, [], [f1484, f1245])).
fof(f1245, plain, ((e22 = h4(e12)) | ~ spl18_168), inference(avatar_component_clause, [], [f1244])).
fof(f1244, plain, (spl18_168 <=> (e22 = h4(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_168])])).
fof(f1484, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | spl18_213), inference(avatar_component_clause, [], [f1482])).
fof(f1482, plain, (spl18_213 <=> (h4(op1(e10, e12)) = op2(e20, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_213])])).
fof(f3921, plain, (~ spl18_60 | ~ spl18_124 | ~ spl18_172 | spl18_214), inference(avatar_contradiction_clause, [], [f3920])).
fof(f3920, plain, ($false | (~ spl18_60 | ~ spl18_124 | ~ spl18_172 | spl18_214)), inference(subsumption_resolution, [], [f3919, f955])).
fof(f3919, plain, (~ (e23 = op2(e20, e21)) | (~ spl18_60 | ~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f3918, f330])).
fof(f330, plain, (e23 = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f3918, plain, (~ (op2(e20, e21) = h4(e13)) | (~ spl18_60 | ~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f3917, f651])).
fof(f651, plain, ((e13 = op1(e10, e11)) | ~ spl18_60), inference(avatar_component_clause, [], [f649])).
fof(f649, plain, (spl18_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_60])])).
fof(f3917, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | (~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f1488, f1266])).
fof(f1488, plain, (~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | spl18_214), inference(avatar_component_clause, [], [f1486])).
fof(f1486, plain, (spl18_214 <=> (h4(op1(e10, e11)) = op2(e20, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_214])])).
fof(f3894, plain, (~ spl18_36 | ~ spl18_100 | ~ spl18_172 | spl18_210), inference(avatar_contradiction_clause, [], [f3893])).
fof(f3893, plain, ($false | (~ spl18_36 | ~ spl18_100 | ~ spl18_172 | spl18_210)), inference(subsumption_resolution, [], [f3892, f853])).
fof(f853, plain, ((e23 = op2(e21, e23)) | ~ spl18_100), inference(avatar_component_clause, [], [f851])).
fof(f851, plain, (spl18_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_100])])).
fof(f3892, plain, (~ (e23 = op2(e21, e23)) | (~ spl18_36 | ~ spl18_172 | spl18_210)), inference(forward_demodulation, [], [f3891, f330])).
fof(f3891, plain, (~ (op2(e21, e23) = h4(e13)) | (~ spl18_36 | ~ spl18_172 | spl18_210)), inference(forward_demodulation, [], [f3890, f549])).
fof(f549, plain, ((e13 = op1(e11, e13)) | ~ spl18_36), inference(avatar_component_clause, [], [f547])).
fof(f3890, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | (~ spl18_172 | spl18_210)), inference(forward_demodulation, [], [f1472, f1266])).
fof(f1472, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | spl18_210), inference(avatar_component_clause, [], [f1470])).
fof(f1470, plain, (spl18_210 <=> (h4(op1(e11, e13)) = op2(h4(e11), e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_210])])).
fof(f3871, plain, (~ spl18_7 | ~ spl18_71 | ~ spl18_168 | spl18_205), inference(avatar_contradiction_clause, [], [f3870])).
fof(f3870, plain, ($false | (~ spl18_7 | ~ spl18_71 | ~ spl18_168 | spl18_205)), inference(subsumption_resolution, [], [f3869, f730])).
fof(f730, plain, ((e22 = op2(e23, e22)) | ~ spl18_71), inference(avatar_component_clause, [], [f728])).
fof(f728, plain, (spl18_71 <=> (e22 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_71])])).
fof(f3869, plain, (~ (e22 = op2(e23, e22)) | (~ spl18_7 | ~ spl18_168 | spl18_205)), inference(forward_demodulation, [], [f3868, f1245])).
fof(f3868, plain, (~ (op2(e23, e22) = h4(e12)) | (~ spl18_7 | ~ spl18_168 | spl18_205)), inference(forward_demodulation, [], [f3867, f426])).
fof(f426, plain, ((e12 = op1(e13, e12)) | ~ spl18_7), inference(avatar_component_clause, [], [f424])).
fof(f3867, plain, (~ (op2(e23, e22) = h4(op1(e13, e12))) | (~ spl18_168 | spl18_205)), inference(forward_demodulation, [], [f1452, f1245])).
fof(f1452, plain, (~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | spl18_205), inference(avatar_component_clause, [], [f1450])).
fof(f1450, plain, (spl18_205 <=> (h4(op1(e13, e12)) = op2(e23, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_205])])).
fof(f3850, plain, (~ spl18_16 | ~ spl18_80 | spl18_207), inference(avatar_contradiction_clause, [], [f3849])).
fof(f3849, plain, ($false | (~ spl18_16 | ~ spl18_80 | spl18_207)), inference(subsumption_resolution, [], [f3848, f768])).
fof(f768, plain, ((e23 = op2(e23, e20)) | ~ spl18_80), inference(avatar_component_clause, [], [f766])).
fof(f766, plain, (spl18_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_80])])).
fof(f3848, plain, (~ (e23 = op2(e23, e20)) | (~ spl18_16 | spl18_207)), inference(forward_demodulation, [], [f3847, f330])).
fof(f3847, plain, (~ (op2(e23, e20) = h4(e13)) | (~ spl18_16 | spl18_207)), inference(forward_demodulation, [], [f1460, f464])).
fof(f1460, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | spl18_207), inference(avatar_component_clause, [], [f1458])).
fof(f1458, plain, (spl18_207 <=> (op2(e23, e20) = h4(op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_207])])).
fof(f3825, plain, (~ spl18_31 | ~ spl18_95 | ~ spl18_168 | spl18_209), inference(avatar_contradiction_clause, [], [f3824])).
fof(f3824, plain, ($false | (~ spl18_31 | ~ spl18_95 | ~ spl18_168 | spl18_209)), inference(subsumption_resolution, [], [f3823, f832])).
fof(f832, plain, ((e22 = op2(e22, e20)) | ~ spl18_95), inference(avatar_component_clause, [], [f830])).
fof(f830, plain, (spl18_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_95])])).
fof(f3823, plain, (~ (e22 = op2(e22, e20)) | (~ spl18_31 | ~ spl18_168 | spl18_209)), inference(forward_demodulation, [], [f3822, f1245])).
fof(f3822, plain, (~ (op2(e22, e20) = h4(e12)) | (~ spl18_31 | ~ spl18_168 | spl18_209)), inference(forward_demodulation, [], [f3821, f528])).
fof(f528, plain, ((e12 = op1(e12, e10)) | ~ spl18_31), inference(avatar_component_clause, [], [f526])).
fof(f526, plain, (spl18_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_31])])).
fof(f3821, plain, (~ (op2(e22, e20) = h4(op1(e12, e10))) | (~ spl18_168 | spl18_209)), inference(forward_demodulation, [], [f1468, f1245])).
fof(f1468, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | spl18_209), inference(avatar_component_clause, [], [f1466])).
fof(f1466, plain, (spl18_209 <=> (h4(op1(e12, e10)) = op2(h4(e12), e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_209])])).
fof(f3802, plain, (~ spl18_26 | ~ spl18_90 | ~ spl18_168 | ~ spl18_172 | spl18_202), inference(avatar_contradiction_clause, [], [f3801])).
fof(f3801, plain, ($false | (~ spl18_26 | ~ spl18_90 | ~ spl18_168 | ~ spl18_172 | spl18_202)), inference(subsumption_resolution, [], [f3800, f811])).
fof(f811, plain, ((e21 = op2(e22, e21)) | ~ spl18_90), inference(avatar_component_clause, [], [f809])).
fof(f809, plain, (spl18_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_90])])).
fof(f3800, plain, (~ (e21 = op2(e22, e21)) | (~ spl18_26 | ~ spl18_168 | ~ spl18_172 | spl18_202)), inference(forward_demodulation, [], [f3799, f1266])).
fof(f3799, plain, (~ (op2(e22, e21) = h4(e11)) | (~ spl18_26 | ~ spl18_168 | ~ spl18_172 | spl18_202)), inference(forward_demodulation, [], [f3798, f507])).
fof(f507, plain, ((e11 = op1(e12, e11)) | ~ spl18_26), inference(avatar_component_clause, [], [f505])).
fof(f505, plain, (spl18_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_26])])).
fof(f3798, plain, (~ (op2(e22, e21) = h4(op1(e12, e11))) | (~ spl18_168 | ~ spl18_172 | spl18_202)), inference(forward_demodulation, [], [f3797, f1245])).
fof(f3797, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | (~ spl18_172 | spl18_202)), inference(forward_demodulation, [], [f1440, f1266])).
fof(f1440, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | spl18_202), inference(avatar_component_clause, [], [f1438])).
fof(f1438, plain, (spl18_202 <=> (h4(op1(e12, e11)) = op2(h4(e12), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_202])])).
fof(f3775, plain, (~ spl18_37 | ~ spl18_101 | ~ spl18_168 | ~ spl18_172 | spl18_201), inference(avatar_contradiction_clause, [], [f3774])).
fof(f3774, plain, ($false | (~ spl18_37 | ~ spl18_101 | ~ spl18_168 | ~ spl18_172 | spl18_201)), inference(subsumption_resolution, [], [f3773, f858])).
fof(f858, plain, ((e20 = op2(e21, e22)) | ~ spl18_101), inference(avatar_component_clause, [], [f856])).
fof(f856, plain, (spl18_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_101])])).
fof(f3773, plain, (~ (e20 = op2(e21, e22)) | (~ spl18_37 | ~ spl18_168 | ~ spl18_172 | spl18_201)), inference(forward_demodulation, [], [f3772, f1237])).
fof(f3772, plain, (~ (op2(e21, e22) = h4(e10)) | (~ spl18_37 | ~ spl18_168 | ~ spl18_172 | spl18_201)), inference(forward_demodulation, [], [f3771, f554])).
fof(f3771, plain, (~ (op2(e21, e22) = h4(op1(e11, e12))) | (~ spl18_168 | ~ spl18_172 | spl18_201)), inference(forward_demodulation, [], [f3770, f1266])).
fof(f3770, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | (~ spl18_168 | spl18_201)), inference(forward_demodulation, [], [f1436, f1245])).
fof(f1436, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | spl18_201), inference(avatar_component_clause, [], [f1434])).
fof(f1434, plain, (spl18_201 <=> (h4(op1(e11, e12)) = op2(h4(e11), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_201])])).
fof(f3734, plain, (~ spl18_24 | ~ spl18_88 | ~ spl18_168 | spl18_203), inference(avatar_contradiction_clause, [], [f3733])).
fof(f3733, plain, ($false | (~ spl18_24 | ~ spl18_88 | ~ spl18_168 | spl18_203)), inference(subsumption_resolution, [], [f3732, f802])).
fof(f802, plain, ((e23 = op2(e22, e22)) | ~ spl18_88), inference(avatar_component_clause, [], [f800])).
fof(f800, plain, (spl18_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_88])])).
fof(f3732, plain, (~ (e23 = op2(e22, e22)) | (~ spl18_24 | ~ spl18_168 | spl18_203)), inference(forward_demodulation, [], [f3731, f330])).
fof(f3731, plain, (~ (op2(e22, e22) = h4(e13)) | (~ spl18_24 | ~ spl18_168 | spl18_203)), inference(forward_demodulation, [], [f3730, f498])).
fof(f3730, plain, (~ (op2(e22, e22) = h4(op1(e12, e12))) | (~ spl18_168 | spl18_203)), inference(forward_demodulation, [], [f1444, f1245])).
fof(f1444, plain, (~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | spl18_203), inference(avatar_component_clause, [], [f1442])).
fof(f1442, plain, (spl18_203 <=> (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_203])])).
fof(f3675, plain, (~ spl18_92 | ~ spl18_124), inference(avatar_split_clause, [], [f3673, f953, f817])).
fof(f817, plain, (spl18_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_92])])).
fof(f3673, plain, (~ (e23 = op2(e22, e21)) | ~ spl18_124), inference(backward_demodulation, [], [f211, f955])).
fof(f211, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f3671, plain, (~ spl18_125 | ~ spl18_195), inference(avatar_contradiction_clause, [], [f3670])).
fof(f3670, plain, ($false | (~ spl18_125 | ~ spl18_195)), inference(subsumption_resolution, [], [f3669, f259])).
fof(f259, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax8)).
fof(f3669, plain, ((e20 = e22) | (~ spl18_125 | ~ spl18_195)), inference(forward_demodulation, [], [f3665, f960])).
fof(f3665, plain, ((op2(e20, e20) = e22) | (~ spl18_125 | ~ spl18_195)), inference(backward_demodulation, [], [f3616, f3654])).
fof(f3616, plain, ((e22 = op2(e20, h1(e11))) | ~ spl18_195), inference(forward_demodulation, [], [f1203, f1386])).
fof(f1386, plain, ((e22 = h1(e10)) | ~ spl18_195), inference(avatar_component_clause, [], [f1385])).
fof(f1385, plain, (spl18_195 <=> (e22 = h1(e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_195])])).
fof(f1203, plain, (h1(e10) = op2(e20, h1(e11))), inference(forward_demodulation, [], [f319, f320])).
fof(f319, plain, (h1(e10) = op2(e20, op2(e20, e20))), inference(cnf_transformation, [], [f14])).
fof(f3667, plain, (~ spl18_109 | ~ spl18_125), inference(avatar_split_clause, [], [f3657, f958, f890])).
fof(f890, plain, (spl18_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_109])])).
fof(f3657, plain, (~ (e20 = op2(e21, e20)) | ~ spl18_125), inference(backward_demodulation, [], [f1196, f3654])).
fof(f1196, plain, ~ (op2(e21, e20) = h1(e11)), inference(backward_demodulation, [], [f204, f320])).
fof(f204, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f3647, plain, (spl18_88 | ~ spl18_115 | ~ spl18_159), inference(avatar_split_clause, [], [f3646, f1151, f915, f800])).
fof(f915, plain, (spl18_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_115])])).
fof(f1151, plain, (spl18_159 <=> (e23 = op2(op2(e20, e23), op2(e20, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl18_159])])).
fof(f3646, plain, ((e23 = op2(e22, e22)) | (~ spl18_115 | ~ spl18_159)), inference(forward_demodulation, [], [f1153, f917])).
fof(f917, plain, ((e22 = op2(e20, e23)) | ~ spl18_115), inference(avatar_component_clause, [], [f915])).
fof(f1153, plain, ((e23 = op2(op2(e20, e23), op2(e20, e23))) | ~ spl18_159), inference(avatar_component_clause, [], [f1151])).
fof(f3644, plain, (~ spl18_51 | spl18_212), inference(avatar_contradiction_clause, [], [f3643])).
fof(f3643, plain, ($false | (~ spl18_51 | spl18_212)), inference(trivial_inequality_removal, [], [f3642])).
fof(f3642, plain, (~ (h4(e12) = h4(e12)) | (~ spl18_51 | spl18_212)), inference(forward_demodulation, [], [f1480, f613])).
fof(f1480, plain, (~ (h4(e12) = h4(op1(e10, e13))) | spl18_212), inference(avatar_component_clause, [], [f1478])).
fof(f1478, plain, (spl18_212 <=> (h4(e12) = h4(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_212])])).
fof(f3640, plain, (~ spl18_66 | ~ spl18_150 | ~ spl18_235), inference(avatar_contradiction_clause, [], [f3639])).
fof(f3639, plain, ($false | (~ spl18_66 | ~ spl18_150 | ~ spl18_235)), inference(subsumption_resolution, [], [f3638, f261])).
fof(f261, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f3638, plain, ((e21 = e22) | (~ spl18_66 | ~ spl18_150 | ~ spl18_235)), inference(forward_demodulation, [], [f3631, f709])).
fof(f709, plain, ((e21 = op2(e23, e23)) | ~ spl18_66), inference(avatar_component_clause, [], [f707])).
fof(f707, plain, (spl18_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_66])])).
fof(f3631, plain, ((e22 = op2(e23, e23)) | (~ spl18_150 | ~ spl18_235)), inference(backward_demodulation, [], [f3528, f1636])).
fof(f1636, plain, ((e23 = h3(e11)) | ~ spl18_235), inference(avatar_component_clause, [], [f1635])).
fof(f1635, plain, (spl18_235 <=> (e23 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_235])])).
fof(f3528, plain, ((e22 = op2(h3(e11), h3(e11))) | ~ spl18_150), inference(forward_demodulation, [], [f1110, f328])).
fof(f328, plain, (op2(e22, e22) = h3(e11)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((h3(e12) = op2(op2(e22, op2(e22, e22)), e22)) & (op2(e22, e22) = h3(e11)) & (h3(e10) = op2(e22, op2(e22, e22))) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax16)).
fof(f1110, plain, ((e22 = op2(op2(e22, e22), op2(e22, e22))) | ~ spl18_150), inference(avatar_component_clause, [], [f1108])).
fof(f1108, plain, (spl18_150 <=> (e22 = op2(op2(e22, e22), op2(e22, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_150])])).
fof(f3612, plain, (~ spl18_119 | ~ spl18_168), inference(avatar_split_clause, [], [f2848, f1244, f932])).
fof(f932, plain, (spl18_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_119])])).
fof(f2848, plain, (~ (e22 = op2(e20, e22)) | ~ spl18_168), inference(forward_demodulation, [], [f1228, f1245])).
fof(f1228, plain, ~ (op2(e20, e22) = h4(e12)), inference(backward_demodulation, [], [f233, f1223])).
fof(f1223, plain, (op2(e20, e23) = h4(e12)), inference(forward_demodulation, [], [f333, f315])).
fof(f333, plain, (op2(op2(e23, op2(e23, e23)), e23) = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f233, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3609, plain, (~ spl18_112 | ~ spl18_100), inference(avatar_split_clause, [], [f3608, f851, f902])).
fof(f902, plain, (spl18_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_112])])).
fof(f3608, plain, (~ (e23 = op2(e21, e20)) | ~ spl18_100), inference(forward_demodulation, [], [f236, f853])).
fof(f236, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f3581, plain, (~ spl18_27 | ~ spl18_43), inference(avatar_split_clause, [], [f3574, f577, f509])).
fof(f509, plain, (spl18_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_27])])).
fof(f3574, plain, (~ (e12 = op1(e12, e11)) | ~ spl18_43), inference(backward_demodulation, [], [f165, f579])).
fof(f579, plain, ((e12 = op1(e11, e11)) | ~ spl18_43), inference(avatar_component_clause, [], [f577])).
fof(f165, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3550, plain, (~ spl18_66 | ~ spl18_100 | ~ spl18_154), inference(avatar_contradiction_clause, [], [f3549])).
fof(f3549, plain, ($false | (~ spl18_66 | ~ spl18_100 | ~ spl18_154)), inference(subsumption_resolution, [], [f3548, f262])).
fof(f262, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f3548, plain, ((e21 = e23) | (~ spl18_66 | ~ spl18_100 | ~ spl18_154)), inference(forward_demodulation, [], [f3547, f709])).
fof(f3547, plain, ((e23 = op2(e23, e23)) | (~ spl18_100 | ~ spl18_154)), inference(backward_demodulation, [], [f1129, f853])).
fof(f1129, plain, ((e23 = op2(op2(e21, e23), op2(e21, e23))) | ~ spl18_154), inference(avatar_component_clause, [], [f1127])).
fof(f1127, plain, (spl18_154 <=> (e23 = op2(op2(e21, e23), op2(e21, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl18_154])])).
fof(f3527, plain, (~ spl18_94 | spl18_105 | ~ spl18_152), inference(avatar_contradiction_clause, [], [f3526])).
fof(f3526, plain, ($false | (~ spl18_94 | spl18_105 | ~ spl18_152)), inference(subsumption_resolution, [], [f3525, f874])).
fof(f874, plain, (~ (e20 = op2(e21, e21)) | spl18_105), inference(avatar_component_clause, [], [f873])).
fof(f873, plain, (spl18_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_105])])).
fof(f3525, plain, ((e20 = op2(e21, e21)) | (~ spl18_94 | ~ spl18_152)), inference(forward_demodulation, [], [f1120, f828])).
fof(f828, plain, ((e21 = op2(e22, e20)) | ~ spl18_94), inference(avatar_component_clause, [], [f826])).
fof(f1120, plain, ((e20 = op2(op2(e22, e20), op2(e22, e20))) | ~ spl18_152), inference(avatar_component_clause, [], [f1118])).
fof(f1118, plain, (spl18_152 <=> (e20 = op2(op2(e22, e20), op2(e22, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_152])])).
fof(f3515, plain, (~ spl18_103 | ~ spl18_186), inference(avatar_split_clause, [], [f3508, f1339, f864])).
fof(f864, plain, (spl18_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_103])])).
fof(f3508, plain, (~ (e22 = op2(e21, e22)) | ~ spl18_186), inference(backward_demodulation, [], [f1209, f1340])).
fof(f1209, plain, ~ (op2(e21, e22) = h2(e11)), inference(backward_demodulation, [], [f237, f324])).
fof(f237, plain, ~ (op2(e21, e21) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f3505, plain, (~ spl18_2 | spl18_204), inference(avatar_contradiction_clause, [], [f3504])).
fof(f3504, plain, ($false | (~ spl18_2 | spl18_204)), inference(trivial_inequality_removal, [], [f3503])).
fof(f3503, plain, (~ (h4(e11) = h4(e11)) | (~ spl18_2 | spl18_204)), inference(forward_demodulation, [], [f1448, f405])).
fof(f405, plain, ((e11 = op1(e13, e13)) | ~ spl18_2), inference(avatar_component_clause, [], [f403])).
fof(f403, plain, (spl18_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_2])])).
fof(f1448, plain, (~ (h4(e11) = h4(op1(e13, e13))) | spl18_204), inference(avatar_component_clause, [], [f1446])).
fof(f1446, plain, (spl18_204 <=> (h4(e11) = h4(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_204])])).
fof(f3463, plain, (~ spl18_28 | ~ spl18_24), inference(avatar_split_clause, [], [f3462, f496, f513])).
fof(f513, plain, (spl18_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_28])])).
fof(f3462, plain, (~ (e13 = op1(e12, e11)) | ~ spl18_24), inference(forward_demodulation, [], [f195, f498])).
fof(f195, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f3457, plain, (~ spl18_17 | ~ spl18_20), inference(avatar_contradiction_clause, [], [f3456])).
fof(f3456, plain, ($false | (~ spl18_17 | ~ spl18_20)), inference(subsumption_resolution, [], [f3455, f254])).
fof(f254, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f3455, plain, ((e10 = e13) | (~ spl18_17 | ~ spl18_20)), inference(forward_demodulation, [], [f481, f469])).
fof(f469, plain, ((e10 = op1(e12, e13)) | ~ spl18_17), inference(avatar_component_clause, [], [f467])).
fof(f481, plain, ((e13 = op1(e12, e13)) | ~ spl18_20), inference(avatar_component_clause, [], [f479])).
fof(f3454, plain, (~ spl18_33 | ~ spl18_61 | ~ spl18_135), inference(avatar_contradiction_clause, [], [f3453])).
fof(f3453, plain, ($false | (~ spl18_33 | ~ spl18_61 | ~ spl18_135)), inference(subsumption_resolution, [], [f3452, f254])).
fof(f3452, plain, ((e10 = e13) | (~ spl18_33 | ~ spl18_61 | ~ spl18_135)), inference(forward_demodulation, [], [f3449, f656])).
fof(f3449, plain, ((op1(e10, e10) = e13) | (~ spl18_33 | ~ spl18_135)), inference(backward_demodulation, [], [f1037, f537])).
fof(f537, plain, ((e10 = op1(e11, e13)) | ~ spl18_33), inference(avatar_component_clause, [], [f535])).
fof(f1037, plain, ((e13 = op1(op1(e11, e13), op1(e11, e13))) | ~ spl18_135), inference(avatar_component_clause, [], [f1035])).
fof(f1035, plain, (spl18_135 <=> (e13 = op1(op1(e11, e13), op1(e11, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_135])])).
fof(f3446, plain, (~ spl18_2 | ~ spl18_40 | ~ spl18_136), inference(avatar_contradiction_clause, [], [f3445])).
fof(f3445, plain, ($false | (~ spl18_2 | ~ spl18_40 | ~ spl18_136)), inference(subsumption_resolution, [], [f3444, f255])).
fof(f255, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f3444, plain, ((e11 = e12) | (~ spl18_2 | ~ spl18_40 | ~ spl18_136)), inference(forward_demodulation, [], [f3442, f405])).
fof(f3442, plain, ((e12 = op1(e13, e13)) | (~ spl18_40 | ~ spl18_136)), inference(backward_demodulation, [], [f1042, f566])).
fof(f3434, plain, (~ spl18_81 | ~ spl18_84), inference(avatar_contradiction_clause, [], [f3433])).
fof(f3433, plain, ($false | (~ spl18_81 | ~ spl18_84)), inference(subsumption_resolution, [], [f3431, f260])).
fof(f260, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f3431, plain, ((e20 = e23) | (~ spl18_81 | ~ spl18_84)), inference(backward_demodulation, [], [f785, f773])).
fof(f773, plain, ((e20 = op2(e22, e23)) | ~ spl18_81), inference(avatar_component_clause, [], [f771])).
fof(f771, plain, (spl18_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_81])])).
fof(f785, plain, ((e23 = op2(e22, e23)) | ~ spl18_84), inference(avatar_component_clause, [], [f783])).
fof(f783, plain, (spl18_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_84])])).
fof(f3406, plain, (~ spl18_109 | spl18_125 | ~ spl18_157), inference(avatar_contradiction_clause, [], [f3405])).
fof(f3405, plain, ($false | (~ spl18_109 | spl18_125 | ~ spl18_157)), inference(subsumption_resolution, [], [f3401, f959])).
fof(f959, plain, (~ (e20 = op2(e20, e20)) | spl18_125), inference(avatar_component_clause, [], [f958])).
fof(f3401, plain, ((e20 = op2(e20, e20)) | (~ spl18_109 | ~ spl18_157)), inference(backward_demodulation, [], [f1144, f892])).
fof(f892, plain, ((e20 = op2(e21, e20)) | ~ spl18_109), inference(avatar_component_clause, [], [f890])).
fof(f1144, plain, ((e20 = op2(op2(e21, e20), op2(e21, e20))) | ~ spl18_157), inference(avatar_component_clause, [], [f1142])).
fof(f1142, plain, (spl18_157 <=> (e20 = op2(op2(e21, e20), op2(e21, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_157])])).
fof(f3395, plain, (spl18_106 | ~ spl18_122 | ~ spl18_161), inference(avatar_split_clause, [], [f3393, f1161, f945, f877])).
fof(f877, plain, (spl18_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_106])])).
fof(f945, plain, (spl18_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_122])])).
fof(f1161, plain, (spl18_161 <=> (e21 = op2(op2(e20, e21), op2(e20, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl18_161])])).
fof(f3393, plain, ((e21 = op2(e21, e21)) | (~ spl18_122 | ~ spl18_161)), inference(backward_demodulation, [], [f1163, f947])).
fof(f947, plain, ((e21 = op2(e20, e21)) | ~ spl18_122), inference(avatar_component_clause, [], [f945])).
fof(f1163, plain, ((e21 = op2(op2(e20, e21), op2(e20, e21))) | ~ spl18_161), inference(avatar_component_clause, [], [f1161])).
fof(f3362, plain, (~ spl18_124 | ~ spl18_273), inference(avatar_split_clause, [], [f3357, f1863, f953])).
fof(f1863, plain, (spl18_273 <=> (e23 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_273])])).
fof(f3357, plain, (~ (e23 = op2(e20, e21)) | ~ spl18_273), inference(backward_demodulation, [], [f1199, f1864])).
fof(f1864, plain, ((e23 = h1(e11)) | ~ spl18_273), inference(avatar_component_clause, [], [f1863])).
fof(f1199, plain, ~ (op2(e20, e21) = h1(e11)), inference(backward_demodulation, [], [f228, f320])).
fof(f228, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f3349, plain, (~ spl18_98 | ~ spl18_172), inference(avatar_split_clause, [], [f2762, f1265, f843])).
fof(f843, plain, (spl18_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_98])])).
fof(f2762, plain, (~ (e21 = op2(e21, e23)) | ~ spl18_172), inference(forward_demodulation, [], [f1229, f1266])).
fof(f1229, plain, ~ (op2(e21, e23) = h4(e11)), inference(backward_demodulation, [], [f226, f332])).
fof(f226, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3330, plain, (~ spl18_81 | ~ spl18_17 | ~ spl18_168 | spl18_208), inference(avatar_split_clause, [], [f3096, f1462, f1244, f467, f771])).
fof(f1462, plain, (spl18_208 <=> (h4(op1(e12, e13)) = op2(h4(e12), e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_208])])).
fof(f3096, plain, (~ (e20 = op2(e22, e23)) | (~ spl18_17 | ~ spl18_168 | spl18_208)), inference(forward_demodulation, [], [f3095, f1237])).
fof(f3095, plain, (~ (op2(e22, e23) = h4(e10)) | (~ spl18_17 | ~ spl18_168 | spl18_208)), inference(forward_demodulation, [], [f3094, f469])).
fof(f3094, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | (~ spl18_168 | spl18_208)), inference(forward_demodulation, [], [f1464, f1245])).
fof(f1464, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | spl18_208), inference(avatar_component_clause, [], [f1462])).
fof(f3321, plain, (~ spl18_5 | ~ spl18_9), inference(avatar_split_clause, [], [f2585, f433, f416])).
fof(f416, plain, (spl18_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_5])])).
fof(f2585, plain, (~ (e10 = op1(e13, e12)) | ~ spl18_9), inference(forward_demodulation, [], [f201, f435])).
fof(f201, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3318, plain, (~ spl18_8 | ~ spl18_16), inference(avatar_split_clause, [], [f3316, f462, f428])).
fof(f428, plain, (spl18_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_8])])).
fof(f3316, plain, (~ (e13 = op1(e13, e12)) | ~ spl18_16), inference(backward_demodulation, [], [f199, f464])).
fof(f3315, plain, (~ spl18_8 | ~ spl18_24), inference(avatar_split_clause, [], [f3314, f496, f428])).
fof(f3314, plain, (~ (e13 = op1(e13, e12)) | ~ spl18_24), inference(backward_demodulation, [], [f173, f498])).
fof(f173, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f3313, plain, (~ spl18_23 | ~ spl18_27), inference(avatar_split_clause, [], [f3312, f509, f492])).
fof(f3312, plain, (~ (e12 = op1(e12, e12)) | ~ spl18_27), inference(backward_demodulation, [], [f195, f511])).
fof(f511, plain, ((e12 = op1(e12, e11)) | ~ spl18_27), inference(avatar_component_clause, [], [f509])).
fof(f3287, plain, (~ spl18_38 | ~ spl18_42), inference(avatar_split_clause, [], [f3280, f573, f556])).
fof(f556, plain, (spl18_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_38])])).
fof(f573, plain, (spl18_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_42])])).
fof(f3280, plain, (~ (e11 = op1(e11, e12)) | ~ spl18_42), inference(backward_demodulation, [], [f189, f575])).
fof(f575, plain, ((e11 = op1(e11, e11)) | ~ spl18_42), inference(avatar_component_clause, [], [f573])).
fof(f189, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f3269, plain, (~ spl18_38 | ~ spl18_54), inference(avatar_split_clause, [], [f3263, f624, f556])).
fof(f3263, plain, (~ (e11 = op1(e11, e12)) | ~ spl18_54), inference(backward_demodulation, [], [f168, f626])).
fof(f3262, plain, (~ spl18_53 | ~ spl18_61), inference(avatar_split_clause, [], [f3255, f654, f620])).
fof(f3255, plain, (~ (e10 = op1(e10, e12)) | ~ spl18_61), inference(backward_demodulation, [], [f181, f656])).
fof(f181, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f3261, plain, (~ spl18_45 | ~ spl18_61), inference(avatar_split_clause, [], [f3252, f654, f586])).
fof(f586, plain, (spl18_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_45])])).
fof(f3252, plain, (~ (e10 = op1(e11, e10)) | ~ spl18_61), inference(backward_demodulation, [], [f156, f656])).
fof(f156, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f3192, plain, (spl18_108 | ~ spl18_66 | ~ spl18_163), inference(avatar_split_clause, [], [f3119, f1171, f707, f885])).
fof(f885, plain, (spl18_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_108])])).
fof(f1171, plain, (spl18_163 <=> (e23 = op2(op2(e23, e23), op2(e23, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl18_163])])).
fof(f3119, plain, ((e23 = op2(e21, e21)) | (~ spl18_66 | ~ spl18_163)), inference(forward_demodulation, [], [f1173, f709])).
fof(f1173, plain, ((e23 = op2(op2(e23, e23), op2(e23, e23))) | ~ spl18_163), inference(avatar_component_clause, [], [f1171])).
fof(f3185, plain, (~ spl18_56 | ~ spl18_60), inference(avatar_split_clause, [], [f3184, f649, f632])).
fof(f632, plain, (spl18_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_56])])).
fof(f3184, plain, (~ (e13 = op1(e10, e12)) | ~ spl18_60), inference(forward_demodulation, [], [f183, f651])).
fof(f183, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f3181, plain, (~ spl18_1 | ~ spl18_2), inference(avatar_contradiction_clause, [], [f3180])).
fof(f3180, plain, ($false | (~ spl18_1 | ~ spl18_2)), inference(subsumption_resolution, [], [f3179, f252])).
fof(f3179, plain, ((e10 = e11) | (~ spl18_1 | ~ spl18_2)), inference(backward_demodulation, [], [f405, f401])).
fof(f401, plain, ((e10 = op1(e13, e13)) | ~ spl18_1), inference(avatar_component_clause, [], [f399])).
fof(f3178, plain, (~ spl18_26 | spl18_42 | ~ spl18_132), inference(avatar_contradiction_clause, [], [f3177])).
fof(f3177, plain, ($false | (~ spl18_26 | spl18_42 | ~ spl18_132)), inference(subsumption_resolution, [], [f3175, f574])).
fof(f574, plain, (~ (e11 = op1(e11, e11)) | spl18_42), inference(avatar_component_clause, [], [f573])).
fof(f3175, plain, ((e11 = op1(e11, e11)) | (~ spl18_26 | ~ spl18_132)), inference(backward_demodulation, [], [f1023, f507])).
fof(f1023, plain, ((e11 = op1(op1(e12, e11), op1(e12, e11))) | ~ spl18_132), inference(avatar_component_clause, [], [f1021])).
fof(f1021, plain, (spl18_132 <=> (e11 = op1(op1(e12, e11), op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_132])])).
fof(f3173, plain, (spl18_1 | ~ spl18_32 | ~ spl18_133), inference(avatar_split_clause, [], [f3171, f1026, f530, f399])).
fof(f530, plain, (spl18_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_32])])).
fof(f1026, plain, (spl18_133 <=> (e10 = op1(op1(e12, e10), op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_133])])).
fof(f3171, plain, ((e10 = op1(e13, e13)) | (~ spl18_32 | ~ spl18_133)), inference(backward_demodulation, [], [f1028, f532])).
fof(f532, plain, ((e13 = op1(e12, e10)) | ~ spl18_32), inference(avatar_component_clause, [], [f530])).
fof(f1028, plain, ((e10 = op1(op1(e12, e10), op1(e12, e10))) | ~ spl18_133), inference(avatar_component_clause, [], [f1026])).
fof(f3162, plain, (~ spl18_58 | ~ spl18_62), inference(avatar_split_clause, [], [f3156, f658, f641])).
fof(f641, plain, (spl18_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_58])])).
fof(f658, plain, (spl18_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl18_62])])).
fof(f3156, plain, (~ (e11 = op1(e10, e11)) | ~ spl18_62), inference(backward_demodulation, [], [f180, f660])).
fof(f660, plain, ((op1(e10, e10) = e11) | ~ spl18_62), inference(avatar_component_clause, [], [f658])).
fof(f180, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f3136, plain, (spl18_65 | ~ spl18_112 | ~ spl18_157), inference(avatar_contradiction_clause, [], [f3135])).
fof(f3135, plain, ($false | (spl18_65 | ~ spl18_112 | ~ spl18_157)), inference(subsumption_resolution, [], [f3133, f704])).
fof(f704, plain, (~ (e20 = op2(e23, e23)) | spl18_65), inference(avatar_component_clause, [], [f703])).
fof(f703, plain, (spl18_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_65])])).
fof(f3133, plain, ((e20 = op2(e23, e23)) | (~ spl18_112 | ~ spl18_157)), inference(backward_demodulation, [], [f1144, f904])).
fof(f904, plain, ((e23 = op2(e21, e20)) | ~ spl18_112), inference(avatar_component_clause, [], [f902])).
fof(f3114, plain, (spl18_126 | ~ spl18_73 | ~ spl18_165), inference(avatar_split_clause, [], [f3113, f1181, f737, f962])).
fof(f962, plain, (spl18_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl18_126])])).
fof(f737, plain, (spl18_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_73])])).
fof(f1181, plain, (spl18_165 <=> (e21 = op2(op2(e23, e21), op2(e23, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl18_165])])).
fof(f3113, plain, ((op2(e20, e20) = e21) | (~ spl18_73 | ~ spl18_165)), inference(forward_demodulation, [], [f1183, f739])).
fof(f739, plain, ((e20 = op2(e23, e21)) | ~ spl18_73), inference(avatar_component_clause, [], [f737])).
fof(f1183, plain, ((e21 = op2(op2(e23, e21), op2(e23, e21))) | ~ spl18_165), inference(avatar_component_clause, [], [f1181])).
fof(f3105, plain, (~ spl18_110 | ~ spl18_198), inference(avatar_split_clause, [], [f3099, f1400, f894])).
fof(f1400, plain, (spl18_198 <=> (e21 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_198])])).
fof(f3099, plain, (~ (e21 = op2(e21, e20)) | ~ spl18_198), inference(backward_demodulation, [], [f1196, f1401])).
fof(f1401, plain, ((e21 = h1(e11)) | ~ spl18_198), inference(avatar_component_clause, [], [f1400])).
fof(f3068, plain, (~ spl18_123 | ~ spl18_168), inference(avatar_split_clause, [], [f2950, f1244, f949])).
fof(f949, plain, (spl18_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_123])])).
fof(f2950, plain, (~ (e22 = op2(e20, e21)) | ~ spl18_168), inference(forward_demodulation, [], [f1227, f1245])).
fof(f1227, plain, ~ (op2(e20, e21) = h4(e12)), inference(backward_demodulation, [], [f232, f1223])).
fof(f232, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3055, plain, (~ spl18_87 | ~ spl18_115 | ~ spl18_159), inference(avatar_contradiction_clause, [], [f3054])).
fof(f3054, plain, ($false | (~ spl18_87 | ~ spl18_115 | ~ spl18_159)), inference(subsumption_resolution, [], [f3053, f263])).
fof(f263, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f3053, plain, ((e22 = e23) | (~ spl18_87 | ~ spl18_115 | ~ spl18_159)), inference(forward_demodulation, [], [f2992, f798])).
fof(f798, plain, ((e22 = op2(e22, e22)) | ~ spl18_87), inference(avatar_component_clause, [], [f796])).
fof(f796, plain, (spl18_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_87])])).
fof(f2992, plain, ((e23 = op2(e22, e22)) | (~ spl18_115 | ~ spl18_159)), inference(forward_demodulation, [], [f1153, f917])).
fof(f3046, plain, (spl18_64 | ~ spl18_17 | ~ spl18_130), inference(avatar_split_clause, [], [f2730, f1011, f467, f666])).
fof(f1011, plain, (spl18_130 <=> (e13 = op1(op1(e12, e13), op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_130])])).
fof(f2730, plain, ((op1(e10, e10) = e13) | (~ spl18_17 | ~ spl18_130)), inference(backward_demodulation, [], [f1013, f469])).
fof(f1013, plain, ((e13 = op1(op1(e12, e13), op1(e12, e13))) | ~ spl18_130), inference(avatar_component_clause, [], [f1011])).
fof(f3045, plain, (~ spl18_62 | ~ spl18_9 | spl18_146), inference(avatar_split_clause, [], [f3001, f1089, f433, f658])).
fof(f3001, plain, (~ (op1(e10, e10) = e11) | (~ spl18_9 | spl18_146)), inference(forward_demodulation, [], [f1090, f435])).
fof(f1090, plain, (~ (e11 = op1(op1(e13, e11), op1(e13, e11))) | spl18_146), inference(avatar_component_clause, [], [f1089])).
fof(f3035, plain, (~ spl18_71 | ~ spl18_79), inference(avatar_split_clause, [], [f3034, f762, f728])).
fof(f762, plain, (spl18_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_79])])).
fof(f3034, plain, (~ (e22 = op2(e23, e22)) | ~ spl18_79), inference(backward_demodulation, [], [f247, f764])).
fof(f764, plain, ((e22 = op2(e23, e20)) | ~ spl18_79), inference(avatar_component_clause, [], [f762])).
fof(f247, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f3016, plain, (~ spl18_2 | ~ spl18_36 | ~ spl18_135), inference(avatar_contradiction_clause, [], [f3015])).
fof(f3015, plain, ($false | (~ spl18_2 | ~ spl18_36 | ~ spl18_135)), inference(subsumption_resolution, [], [f3014, f256])).
fof(f256, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f3014, plain, ((e11 = e13) | (~ spl18_2 | ~ spl18_36 | ~ spl18_135)), inference(forward_demodulation, [], [f3013, f405])).
fof(f3013, plain, ((e13 = op1(e13, e13)) | (~ spl18_36 | ~ spl18_135)), inference(forward_demodulation, [], [f1037, f549])).
fof(f3011, plain, (spl18_22 | ~ spl18_43 | ~ spl18_137), inference(avatar_contradiction_clause, [], [f3010])).
fof(f3010, plain, ($false | (spl18_22 | ~ spl18_43 | ~ spl18_137)), inference(subsumption_resolution, [], [f3009, f489])).
fof(f489, plain, (~ (e11 = op1(e12, e12)) | spl18_22), inference(avatar_component_clause, [], [f488])).
fof(f488, plain, (spl18_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_22])])).
fof(f3009, plain, ((e11 = op1(e12, e12)) | (~ spl18_43 | ~ spl18_137)), inference(forward_demodulation, [], [f1047, f579])).
fof(f1047, plain, ((e11 = op1(op1(e11, e11), op1(e11, e11))) | ~ spl18_137), inference(avatar_component_clause, [], [f1045])).
fof(f1045, plain, (spl18_137 <=> (e11 = op1(op1(e11, e11), op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_137])])).
fof(f2995, plain, (~ spl18_86 | ~ spl18_115 | ~ spl18_159), inference(avatar_contradiction_clause, [], [f2994])).
fof(f2994, plain, ($false | (~ spl18_86 | ~ spl18_115 | ~ spl18_159)), inference(subsumption_resolution, [], [f2993, f262])).
fof(f2993, plain, ((e21 = e23) | (~ spl18_86 | ~ spl18_115 | ~ spl18_159)), inference(forward_demodulation, [], [f2992, f794])).
fof(f794, plain, ((e21 = op2(e22, e22)) | ~ spl18_86), inference(avatar_component_clause, [], [f792])).
fof(f792, plain, (spl18_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_86])])).
fof(f2991, plain, (~ spl18_117 | spl18_127 | ~ spl18_160), inference(avatar_contradiction_clause, [], [f2990])).
fof(f2990, plain, ($false | (~ spl18_117 | spl18_127 | ~ spl18_160)), inference(subsumption_resolution, [], [f2989, f967])).
fof(f967, plain, (~ (op2(e20, e20) = e22) | spl18_127), inference(avatar_component_clause, [], [f966])).
fof(f966, plain, (spl18_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl18_127])])).
fof(f2989, plain, ((op2(e20, e20) = e22) | (~ spl18_117 | ~ spl18_160)), inference(forward_demodulation, [], [f1158, f926])).
fof(f926, plain, ((e20 = op2(e20, e22)) | ~ spl18_117), inference(avatar_component_clause, [], [f924])).
fof(f924, plain, (spl18_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_117])])).
fof(f2970, plain, (~ spl18_9 | spl18_206), inference(avatar_contradiction_clause, [], [f2969])).
fof(f2969, plain, ($false | (~ spl18_9 | spl18_206)), inference(subsumption_resolution, [], [f2968, f1237])).
fof(f2968, plain, (~ (e20 = h4(e10)) | (~ spl18_9 | spl18_206)), inference(forward_demodulation, [], [f1456, f435])).
fof(f1456, plain, (~ (e20 = h4(op1(e13, e11))) | spl18_206), inference(avatar_component_clause, [], [f1454])).
fof(f1454, plain, (spl18_206 <=> (e20 = h4(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_206])])).
fof(f2967, plain, (spl18_195 | ~ spl18_115 | ~ spl18_273), inference(avatar_split_clause, [], [f2966, f1863, f915, f1385])).
fof(f2966, plain, ((e22 = h1(e10)) | (~ spl18_115 | ~ spl18_273)), inference(forward_demodulation, [], [f2964, f917])).
fof(f2964, plain, ((op2(e20, e23) = h1(e10)) | ~ spl18_273), inference(backward_demodulation, [], [f1203, f1864])).
fof(f2962, plain, (spl18_273 | ~ spl18_128), inference(avatar_split_clause, [], [f2904, f970, f1863])).
fof(f970, plain, (spl18_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl18_128])])).
fof(f2904, plain, ((e23 = h1(e11)) | ~ spl18_128), inference(backward_demodulation, [], [f320, f972])).
fof(f972, plain, ((op2(e20, e20) = e23) | ~ spl18_128), inference(avatar_component_clause, [], [f970])).
fof(f2958, plain, (spl18_105 | ~ spl18_110 | ~ spl18_157), inference(avatar_split_clause, [], [f2957, f1142, f894, f873])).
fof(f2957, plain, ((e20 = op2(e21, e21)) | (~ spl18_110 | ~ spl18_157)), inference(forward_demodulation, [], [f1144, f896])).
fof(f2927, plain, (~ spl18_66 | ~ spl18_104 | ~ spl18_155), inference(avatar_contradiction_clause, [], [f2926])).
fof(f2926, plain, ($false | (~ spl18_66 | ~ spl18_104 | ~ spl18_155)), inference(subsumption_resolution, [], [f2925, f261])).
fof(f2925, plain, ((e21 = e22) | (~ spl18_66 | ~ spl18_104 | ~ spl18_155)), inference(forward_demodulation, [], [f2924, f709])).
fof(f2924, plain, ((e22 = op2(e23, e23)) | (~ spl18_104 | ~ spl18_155)), inference(backward_demodulation, [], [f1134, f870])).
fof(f870, plain, ((e23 = op2(e21, e22)) | ~ spl18_104), inference(avatar_component_clause, [], [f868])).
fof(f868, plain, (spl18_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_104])])).
fof(f1134, plain, ((e22 = op2(op2(e21, e22), op2(e21, e22))) | ~ spl18_155), inference(avatar_component_clause, [], [f1132])).
fof(f1132, plain, (spl18_155 <=> (e22 = op2(op2(e21, e22), op2(e21, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_155])])).
fof(f2893, plain, (~ spl18_2 | ~ spl18_8 | ~ spl18_145), inference(avatar_contradiction_clause, [], [f2892])).
fof(f2892, plain, ($false | (~ spl18_2 | ~ spl18_8 | ~ spl18_145)), inference(subsumption_resolution, [], [f2891, f255])).
fof(f2891, plain, ((e11 = e12) | (~ spl18_2 | ~ spl18_8 | ~ spl18_145)), inference(forward_demodulation, [], [f2890, f405])).
fof(f2890, plain, ((e12 = op1(e13, e13)) | (~ spl18_8 | ~ spl18_145)), inference(forward_demodulation, [], [f1086, f430])).
fof(f430, plain, ((e13 = op1(e13, e12)) | ~ spl18_8), inference(avatar_component_clause, [], [f428])).
fof(f1086, plain, ((e12 = op1(op1(e13, e12), op1(e13, e12))) | ~ spl18_145), inference(avatar_component_clause, [], [f1084])).
fof(f1084, plain, (spl18_145 <=> (e12 = op1(op1(e13, e12), op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_145])])).
fof(f2889, plain, (~ spl18_9 | spl18_62 | ~ spl18_146), inference(avatar_contradiction_clause, [], [f2888])).
fof(f2888, plain, ($false | (~ spl18_9 | spl18_62 | ~ spl18_146)), inference(subsumption_resolution, [], [f2887, f659])).
fof(f659, plain, (~ (op1(e10, e10) = e11) | spl18_62), inference(avatar_component_clause, [], [f658])).
fof(f2887, plain, ((op1(e10, e10) = e11) | (~ spl18_9 | ~ spl18_146)), inference(forward_demodulation, [], [f1091, f435])).
fof(f2852, plain, (spl18_128 | ~ spl18_97 | ~ spl18_154), inference(avatar_split_clause, [], [f2851, f1127, f839, f970])).
fof(f839, plain, (spl18_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_97])])).
fof(f2851, plain, ((op2(e20, e20) = e23) | (~ spl18_97 | ~ spl18_154)), inference(forward_demodulation, [], [f1129, f841])).
fof(f841, plain, ((e20 = op2(e21, e23)) | ~ spl18_97), inference(avatar_component_clause, [], [f839])).
fof(f2850, plain, (~ spl18_190 | ~ spl18_73), inference(avatar_split_clause, [], [f2474, f737, f1360])).
fof(f1360, plain, (spl18_190 <=> (e20 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_190])])).
fof(f2474, plain, (~ (e20 = h2(e11)) | ~ spl18_73), inference(forward_demodulation, [], [f1207, f739])).
fof(f1207, plain, ~ (op2(e23, e21) = h2(e11)), inference(backward_demodulation, [], [f214, f324])).
fof(f214, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2819, plain, (~ spl18_88 | spl18_235), inference(avatar_contradiction_clause, [], [f2818])).
fof(f2818, plain, ($false | (~ spl18_88 | spl18_235)), inference(subsumption_resolution, [], [f2817, f1637])).
fof(f1637, plain, (~ (e23 = h3(e11)) | spl18_235), inference(avatar_component_clause, [], [f1635])).
fof(f2817, plain, ((e23 = h3(e11)) | ~ spl18_88), inference(backward_demodulation, [], [f328, f802])).
fof(f2801, plain, (~ spl18_127 | spl18_194), inference(avatar_contradiction_clause, [], [f2800])).
fof(f2800, plain, ($false | (~ spl18_127 | spl18_194)), inference(subsumption_resolution, [], [f2799, f1382])).
fof(f1382, plain, (~ (e22 = h1(e11)) | spl18_194), inference(avatar_component_clause, [], [f1380])).
fof(f1380, plain, (spl18_194 <=> (e22 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_194])])).
fof(f2799, plain, ((e22 = h1(e11)) | ~ spl18_127), inference(backward_demodulation, [], [f320, f968])).
fof(f968, plain, ((op2(e20, e20) = e22) | ~ spl18_127), inference(avatar_component_clause, [], [f966])).
fof(f2793, plain, (~ spl18_53 | spl18_63 | ~ spl18_141), inference(avatar_contradiction_clause, [], [f2792])).
fof(f2792, plain, ($false | (~ spl18_53 | spl18_63 | ~ spl18_141)), inference(subsumption_resolution, [], [f2791, f663])).
fof(f663, plain, (~ (op1(e10, e10) = e12) | spl18_63), inference(avatar_component_clause, [], [f662])).
fof(f662, plain, (spl18_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl18_63])])).
fof(f2791, plain, ((op1(e10, e10) = e12) | (~ spl18_53 | ~ spl18_141)), inference(forward_demodulation, [], [f1066, f622])).
fof(f2782, plain, (spl18_85 | ~ spl18_111 | ~ spl18_157), inference(avatar_contradiction_clause, [], [f2781])).
fof(f2781, plain, ($false | (spl18_85 | ~ spl18_111 | ~ spl18_157)), inference(subsumption_resolution, [], [f2780, f789])).
fof(f789, plain, (~ (e20 = op2(e22, e22)) | spl18_85), inference(avatar_component_clause, [], [f788])).
fof(f788, plain, (spl18_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_85])])).
fof(f2780, plain, ((e20 = op2(e22, e22)) | (~ spl18_111 | ~ spl18_157)), inference(forward_demodulation, [], [f1144, f900])).
fof(f900, plain, ((e22 = op2(e21, e20)) | ~ spl18_111), inference(avatar_component_clause, [], [f898])).
fof(f2775, plain, (spl18_254 | ~ spl18_108), inference(avatar_split_clause, [], [f2651, f885, f1749])).
fof(f2651, plain, ((e23 = h2(e11)) | ~ spl18_108), inference(backward_demodulation, [], [f324, f887])).
fof(f887, plain, ((e23 = op2(e21, e21)) | ~ spl18_108), inference(avatar_component_clause, [], [f885])).
fof(f2769, plain, (~ spl18_110 | ~ spl18_102), inference(avatar_split_clause, [], [f2768, f860, f894])).
fof(f860, plain, (spl18_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_102])])).
fof(f2768, plain, (~ (e21 = op2(e21, e20)) | ~ spl18_102), inference(forward_demodulation, [], [f235, f862])).
fof(f862, plain, ((e21 = op2(e21, e22)) | ~ spl18_102), inference(avatar_component_clause, [], [f860])).
fof(f235, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2751, plain, (~ spl18_82 | ~ spl18_172), inference(avatar_split_clause, [], [f2490, f1265, f775])).
fof(f775, plain, (spl18_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_82])])).
fof(f2490, plain, (~ (e21 = op2(e22, e23)) | ~ spl18_172), inference(forward_demodulation, [], [f1230, f1266])).
fof(f1230, plain, ~ (op2(e22, e23) = h4(e11)), inference(backward_demodulation, [], [f227, f332])).
fof(f227, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2750, plain, (~ spl18_83 | ~ spl18_168), inference(avatar_split_clause, [], [f2606, f1244, f779])).
fof(f779, plain, (spl18_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_83])])).
fof(f2606, plain, (~ (e22 = op2(e22, e23)) | ~ spl18_168), inference(forward_demodulation, [], [f1225, f1245])).
fof(f1225, plain, ~ (op2(e22, e23) = h4(e12)), inference(backward_demodulation, [], [f223, f1223])).
fof(f223, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2718, plain, (~ spl18_30 | spl18_41 | ~ spl18_133), inference(avatar_contradiction_clause, [], [f2717])).
fof(f2717, plain, ($false | (~ spl18_30 | spl18_41 | ~ spl18_133)), inference(subsumption_resolution, [], [f2715, f570])).
fof(f570, plain, (~ (e10 = op1(e11, e11)) | spl18_41), inference(avatar_component_clause, [], [f569])).
fof(f569, plain, (spl18_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_41])])).
fof(f2715, plain, ((e10 = op1(e11, e11)) | (~ spl18_30 | ~ spl18_133)), inference(backward_demodulation, [], [f1028, f524])).
fof(f524, plain, ((e11 = op1(e12, e10)) | ~ spl18_30), inference(avatar_component_clause, [], [f522])).
fof(f522, plain, (spl18_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_30])])).
fof(f2709, plain, (~ spl18_22 | ~ spl18_38), inference(avatar_split_clause, [], [f2707, f556, f488])).
fof(f2707, plain, (~ (e11 = op1(e12, e12)) | ~ spl18_38), inference(backward_demodulation, [], [f171, f558])).
fof(f558, plain, ((e11 = op1(e11, e12)) | ~ spl18_38), inference(avatar_component_clause, [], [f556])).
fof(f2668, plain, (~ spl18_93 | spl18_125 | ~ spl18_152), inference(avatar_contradiction_clause, [], [f2667])).
fof(f2667, plain, ($false | (~ spl18_93 | spl18_125 | ~ spl18_152)), inference(subsumption_resolution, [], [f2663, f959])).
fof(f2663, plain, ((e20 = op2(e20, e20)) | (~ spl18_93 | ~ spl18_152)), inference(backward_demodulation, [], [f1120, f824])).
fof(f824, plain, ((e20 = op2(e22, e20)) | ~ spl18_93), inference(avatar_component_clause, [], [f822])).
fof(f822, plain, (spl18_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_93])])).
fof(f2660, plain, (~ spl18_101 | ~ spl18_102), inference(avatar_contradiction_clause, [], [f2659])).
fof(f2659, plain, ($false | (~ spl18_101 | ~ spl18_102)), inference(subsumption_resolution, [], [f2658, f258])).
fof(f258, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f2658, plain, ((e20 = e21) | (~ spl18_101 | ~ spl18_102)), inference(backward_demodulation, [], [f862, f858])).
fof(f2647, plain, (~ spl18_113 | ~ spl18_115), inference(avatar_contradiction_clause, [], [f2646])).
fof(f2646, plain, ($false | (~ spl18_113 | ~ spl18_115)), inference(subsumption_resolution, [], [f2645, f259])).
fof(f2645, plain, ((e20 = e22) | (~ spl18_113 | ~ spl18_115)), inference(backward_demodulation, [], [f917, f909])).
fof(f909, plain, ((e20 = op2(e20, e23)) | ~ spl18_113), inference(avatar_component_clause, [], [f907])).
fof(f907, plain, (spl18_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_113])])).
fof(f2643, plain, (spl18_198 | ~ spl18_126), inference(avatar_split_clause, [], [f2642, f962, f1400])).
fof(f2642, plain, ((e21 = h1(e11)) | ~ spl18_126), inference(backward_demodulation, [], [f320, f964])).
fof(f964, plain, ((op2(e20, e20) = e21) | ~ spl18_126), inference(avatar_component_clause, [], [f962])).
fof(f2638, plain, (~ spl18_198 | ~ spl18_122), inference(avatar_split_clause, [], [f2637, f945, f1400])).
fof(f2637, plain, (~ (e21 = h1(e11)) | ~ spl18_122), inference(forward_demodulation, [], [f1199, f947])).
fof(f2634, plain, (~ spl18_194 | ~ spl18_115), inference(avatar_split_clause, [], [f2633, f915, f1380])).
fof(f2633, plain, (~ (e22 = h1(e11)) | ~ spl18_115), inference(forward_demodulation, [], [f1201, f917])).
fof(f1201, plain, ~ (op2(e20, e23) = h1(e11)), inference(backward_demodulation, [], [f230, f320])).
fof(f230, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f2628, plain, (~ spl18_104 | ~ spl18_100), inference(avatar_split_clause, [], [f2500, f851, f868])).
fof(f2500, plain, (~ (e23 = op2(e21, e22)) | ~ spl18_100), inference(forward_demodulation, [], [f239, f853])).
fof(f239, plain, ~ (op2(e21, e22) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2597, plain, (~ spl18_48 | ~ spl18_36), inference(avatar_split_clause, [], [f2596, f547, f598])).
fof(f598, plain, (spl18_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_48])])).
fof(f2596, plain, (~ (e13 = op1(e11, e10)) | ~ spl18_36), inference(forward_demodulation, [], [f188, f549])).
fof(f188, plain, ~ (op1(e11, e10) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2595, plain, (~ spl18_39 | ~ spl18_43), inference(avatar_split_clause, [], [f2594, f577, f560])).
fof(f2594, plain, (~ (e12 = op1(e11, e12)) | ~ spl18_43), inference(forward_demodulation, [], [f189, f579])).
fof(f2567, plain, (~ spl18_15 | ~ spl18_31), inference(avatar_split_clause, [], [f2565, f526, f458])).
fof(f2565, plain, (~ (e12 = op1(e13, e10)) | ~ spl18_31), inference(backward_demodulation, [], [f161, f528])).
fof(f161, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2562, plain, (~ spl18_33 | ~ spl18_36), inference(avatar_contradiction_clause, [], [f2561])).
fof(f2561, plain, ($false | (~ spl18_33 | ~ spl18_36)), inference(subsumption_resolution, [], [f2559, f254])).
fof(f2559, plain, ((e10 = e13) | (~ spl18_33 | ~ spl18_36)), inference(backward_demodulation, [], [f549, f537])).
fof(f2557, plain, (~ spl18_186 | ~ spl18_43 | ~ spl18_168 | ~ spl18_172 | spl18_200), inference(avatar_split_clause, [], [f2556, f1430, f1265, f1244, f577, f1339])).
fof(f1430, plain, (spl18_200 <=> (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_200])])).
fof(f2556, plain, (~ (e22 = h2(e11)) | (~ spl18_43 | ~ spl18_168 | ~ spl18_172 | spl18_200)), inference(forward_demodulation, [], [f2554, f1245])).
fof(f2554, plain, (~ (h2(e11) = h4(e12)) | (~ spl18_43 | ~ spl18_172 | spl18_200)), inference(backward_demodulation, [], [f2318, f579])).
fof(f2318, plain, (~ (h2(e11) = h4(op1(e11, e11))) | (~ spl18_172 | spl18_200)), inference(forward_demodulation, [], [f2314, f324])).
fof(f2314, plain, (~ (op2(e21, e21) = h4(op1(e11, e11))) | (~ spl18_172 | spl18_200)), inference(backward_demodulation, [], [f1432, f1266])).
fof(f1432, plain, (~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | spl18_200), inference(avatar_component_clause, [], [f1430])).
fof(f2550, plain, (~ spl18_49 | ~ spl18_51), inference(avatar_contradiction_clause, [], [f2549])).
fof(f2549, plain, ($false | (~ spl18_49 | ~ spl18_51)), inference(subsumption_resolution, [], [f2548, f253])).
fof(f2548, plain, ((e10 = e12) | (~ spl18_49 | ~ spl18_51)), inference(backward_demodulation, [], [f613, f605])).
fof(f605, plain, ((e10 = op1(e10, e13)) | ~ spl18_49), inference(avatar_component_clause, [], [f603])).
fof(f603, plain, (spl18_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_49])])).
fof(f2541, plain, (~ spl18_66 | ~ spl18_68), inference(avatar_contradiction_clause, [], [f2540])).
fof(f2540, plain, ($false | (~ spl18_66 | ~ spl18_68)), inference(subsumption_resolution, [], [f2539, f262])).
fof(f2539, plain, ((e21 = e23) | (~ spl18_66 | ~ spl18_68)), inference(forward_demodulation, [], [f717, f709])).
fof(f717, plain, ((e23 = op2(e23, e23)) | ~ spl18_68), inference(avatar_component_clause, [], [f715])).
fof(f715, plain, (spl18_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_68])])).
fof(f2519, plain, (spl18_65 | ~ spl18_96 | ~ spl18_152), inference(avatar_contradiction_clause, [], [f2518])).
fof(f2518, plain, ($false | (spl18_65 | ~ spl18_96 | ~ spl18_152)), inference(subsumption_resolution, [], [f2515, f704])).
fof(f2515, plain, ((e20 = op2(e23, e23)) | (~ spl18_96 | ~ spl18_152)), inference(backward_demodulation, [], [f1120, f836])).
fof(f836, plain, ((e23 = op2(e22, e20)) | ~ spl18_96), inference(avatar_component_clause, [], [f834])).
fof(f834, plain, (spl18_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_96])])).
fof(f2514, plain, (~ spl18_97 | ~ spl18_100), inference(avatar_contradiction_clause, [], [f2513])).
fof(f2513, plain, ($false | (~ spl18_97 | ~ spl18_100)), inference(subsumption_resolution, [], [f2511, f260])).
fof(f2511, plain, ((e20 = e23) | (~ spl18_97 | ~ spl18_100)), inference(backward_demodulation, [], [f853, f841])).
fof(f2509, plain, (spl18_186 | ~ spl18_107), inference(avatar_split_clause, [], [f2508, f881, f1339])).
fof(f2508, plain, ((e22 = h2(e11)) | ~ spl18_107), inference(backward_demodulation, [], [f324, f883])).
fof(f883, plain, ((e22 = op2(e21, e21)) | ~ spl18_107), inference(avatar_component_clause, [], [f881])).
fof(f2486, plain, (~ spl18_81 | ~ spl18_125 | ~ spl18_149), inference(avatar_contradiction_clause, [], [f2485])).
fof(f2485, plain, ($false | (~ spl18_81 | ~ spl18_125 | ~ spl18_149)), inference(subsumption_resolution, [], [f2484, f260])).
fof(f2484, plain, ((e20 = e23) | (~ spl18_81 | ~ spl18_125 | ~ spl18_149)), inference(forward_demodulation, [], [f2483, f960])).
fof(f2483, plain, ((op2(e20, e20) = e23) | (~ spl18_81 | ~ spl18_149)), inference(forward_demodulation, [], [f1105, f773])).
fof(f1105, plain, ((e23 = op2(op2(e22, e23), op2(e22, e23))) | ~ spl18_149), inference(avatar_component_clause, [], [f1103])).
fof(f1103, plain, (spl18_149 <=> (e23 = op2(op2(e22, e23), op2(e22, e23)))), introduced(avatar_definition, [new_symbols(naming, [spl18_149])])).
fof(f2479, plain, (~ spl18_78 | ~ spl18_172), inference(avatar_split_clause, [], [f2478, f1265, f758])).
fof(f758, plain, (spl18_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_78])])).
fof(f2478, plain, (~ (e21 = op2(e23, e20)) | ~ spl18_172), inference(forward_demodulation, [], [f1231, f1266])).
fof(f1231, plain, ~ (op2(e23, e20) = h4(e11)), inference(backward_demodulation, [], [f248, f332])).
fof(f248, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2457, plain, (~ spl18_63 | ~ spl18_51), inference(avatar_split_clause, [], [f2456, f611, f662])).
fof(f2456, plain, (~ (op1(e10, e10) = e12) | ~ spl18_51), inference(forward_demodulation, [], [f182, f613])).
fof(f182, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2413, plain, (~ spl18_13 | ~ spl18_9), inference(avatar_split_clause, [], [f2412, f433, f450])).
fof(f450, plain, (spl18_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_13])])).
fof(f2412, plain, (~ (e10 = op1(e13, e10)) | ~ spl18_9), inference(forward_demodulation, [], [f198, f435])).
fof(f198, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2405, plain, (~ spl18_9 | ~ spl18_10), inference(avatar_contradiction_clause, [], [f2404])).
fof(f2404, plain, ($false | (~ spl18_9 | ~ spl18_10)), inference(subsumption_resolution, [], [f2403, f252])).
fof(f2403, plain, ((e10 = e11) | (~ spl18_9 | ~ spl18_10)), inference(backward_demodulation, [], [f439, f435])).
fof(f439, plain, ((e11 = op1(e13, e11)) | ~ spl18_10), inference(avatar_component_clause, [], [f437])).
fof(f437, plain, (spl18_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_10])])).
fof(f2370, plain, (~ spl18_24 | ~ spl18_56), inference(avatar_split_clause, [], [f2369, f632, f496])).
fof(f2369, plain, (~ (e13 = op1(e12, e12)) | ~ spl18_56), inference(backward_demodulation, [], [f169, f634])).
fof(f634, plain, ((e13 = op1(e10, e12)) | ~ spl18_56), inference(avatar_component_clause, [], [f632])).
fof(f169, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2344, plain, (spl18_85 | ~ spl18_95 | ~ spl18_152), inference(avatar_split_clause, [], [f2341, f1118, f830, f788])).
fof(f2341, plain, ((e20 = op2(e22, e22)) | (~ spl18_95 | ~ spl18_152)), inference(backward_demodulation, [], [f1120, f832])).
fof(f2335, plain, (~ spl18_106 | ~ spl18_122), inference(avatar_contradiction_clause, [], [f2334])).
fof(f2334, plain, ($false | (~ spl18_106 | ~ spl18_122)), inference(subsumption_resolution, [], [f2333, f2320])).
fof(f2320, plain, (~ (e21 = h2(e11)) | ~ spl18_122), inference(backward_demodulation, [], [f1205, f947])).
fof(f2333, plain, ((e21 = h2(e11)) | ~ spl18_106), inference(backward_demodulation, [], [f324, f879])).
fof(f879, plain, ((e21 = op2(e21, e21)) | ~ spl18_106), inference(avatar_component_clause, [], [f877])).
fof(f2328, plain, (~ spl18_235 | ~ spl18_120), inference(avatar_split_clause, [], [f2327, f936, f1635])).
fof(f936, plain, (spl18_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_120])])).
fof(f2327, plain, (~ (e23 = h3(e11)) | ~ spl18_120), inference(backward_demodulation, [], [f1214, f938])).
fof(f938, plain, ((e23 = op2(e20, e22)) | ~ spl18_120), inference(avatar_component_clause, [], [f936])).
fof(f1214, plain, ~ (op2(e20, e22) = h3(e11)), inference(backward_demodulation, [], [f217, f328])).
fof(f217, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2316, plain, (spl18_73 | ~ spl18_172), inference(avatar_split_clause, [], [f2312, f1265, f737])).
fof(f2312, plain, ((e20 = op2(e23, e21)) | ~ spl18_172), inference(backward_demodulation, [], [f1234, f1266])).
fof(f2304, plain, (spl18_168 | ~ spl18_115), inference(avatar_split_clause, [], [f2303, f915, f1244])).
fof(f2303, plain, ((e22 = h4(e12)) | ~ spl18_115), inference(forward_demodulation, [], [f1223, f917])).
fof(f2263, plain, (~ spl18_59 | ~ spl18_51), inference(avatar_split_clause, [], [f2262, f611, f645])).
fof(f645, plain, (spl18_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_59])])).
fof(f2262, plain, (~ (e12 = op1(e10, e11)) | ~ spl18_51), inference(forward_demodulation, [], [f184, f613])).
fof(f184, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2257, plain, (~ spl18_55 | ~ spl18_51), inference(avatar_split_clause, [], [f2256, f611, f628])).
fof(f628, plain, (spl18_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_55])])).
fof(f2256, plain, (~ (e12 = op1(e10, e12)) | ~ spl18_51), inference(forward_demodulation, [], [f185, f613])).
fof(f185, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f2208, plain, (~ spl18_14 | ~ spl18_2), inference(avatar_split_clause, [], [f2207, f403, f454])).
fof(f454, plain, (spl18_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_14])])).
fof(f2207, plain, (~ (e11 = op1(e13, e10)) | ~ spl18_2), inference(forward_demodulation, [], [f200, f405])).
fof(f200, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2200, plain, (~ spl18_6 | ~ spl18_2), inference(avatar_split_clause, [], [f2199, f403, f420])).
fof(f420, plain, (spl18_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_6])])).
fof(f2199, plain, (~ (e11 = op1(e13, e12)) | ~ spl18_2), inference(forward_demodulation, [], [f203, f405])).
fof(f203, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2198, plain, (spl18_9 | ~ spl18_2), inference(avatar_split_clause, [], [f2194, f403, f433])).
fof(f2194, plain, ((e10 = op1(e13, e11)) | ~ spl18_2), inference(backward_demodulation, [], [f312, f405])).
fof(f312, plain, (e10 = op1(e13, op1(e13, e13))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e12 = op1(op1(e13, op1(e13, e13)), e13)) & (e11 = op1(e13, e13)) & (e10 = op1(e13, op1(e13, e13)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax12)).
fof(f2184, plain, (~ spl18_4 | ~ spl18_8), inference(avatar_split_clause, [], [f2183, f428, f411])).
fof(f411, plain, (spl18_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_4])])).
fof(f2183, plain, (~ (e13 = op1(e13, e13)) | ~ spl18_8), inference(backward_demodulation, [], [f203, f430])).
fof(f2164, plain, (~ spl18_4 | ~ spl18_16), inference(avatar_split_clause, [], [f2160, f462, f411])).
fof(f2160, plain, (~ (e13 = op1(e13, e13)) | ~ spl18_16), inference(backward_demodulation, [], [f200, f464])).
fof(f2148, plain, (spl18_4 | ~ spl18_20 | ~ spl18_130), inference(avatar_split_clause, [], [f2146, f1011, f479, f411])).
fof(f2146, plain, ((e13 = op1(e13, e13)) | (~ spl18_20 | ~ spl18_130)), inference(backward_demodulation, [], [f1013, f481])).
fof(f2131, plain, (~ spl18_9 | ~ spl18_25), inference(avatar_split_clause, [], [f2127, f501, f433])).
fof(f501, plain, (spl18_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_25])])).
fof(f2127, plain, (~ (e10 = op1(e13, e11)) | ~ spl18_25), inference(backward_demodulation, [], [f167, f503])).
fof(f503, plain, ((e10 = op1(e12, e11)) | ~ spl18_25), inference(avatar_component_clause, [], [f501])).
fof(f167, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2098, plain, (~ spl18_9 | ~ spl18_41), inference(avatar_split_clause, [], [f2093, f569, f433])).
fof(f2093, plain, (~ (e10 = op1(e13, e11)) | ~ spl18_41), inference(backward_demodulation, [], [f166, f571])).
fof(f571, plain, ((e10 = op1(e11, e11)) | ~ spl18_41), inference(avatar_component_clause, [], [f569])).
fof(f166, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2078, plain, (~ spl18_51 | ~ spl18_52), inference(avatar_contradiction_clause, [], [f2077])).
fof(f2077, plain, ($false | (~ spl18_51 | ~ spl18_52)), inference(subsumption_resolution, [], [f2076, f257])).
fof(f257, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f2076, plain, ((e12 = e13) | (~ spl18_51 | ~ spl18_52)), inference(backward_demodulation, [], [f617, f613])).
fof(f617, plain, ((e13 = op1(e10, e13)) | ~ spl18_52), inference(avatar_component_clause, [], [f615])).
fof(f615, plain, (spl18_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_52])])).
fof(f2059, plain, (~ spl18_9 | ~ spl18_57), inference(avatar_split_clause, [], [f2054, f637, f433])).
fof(f637, plain, (spl18_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_57])])).
fof(f2054, plain, (~ (e10 = op1(e13, e11)) | ~ spl18_57), inference(backward_demodulation, [], [f164, f639])).
fof(f639, plain, ((e10 = op1(e10, e11)) | ~ spl18_57), inference(avatar_component_clause, [], [f637])).
fof(f164, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2038, plain, (~ spl18_65 | ~ spl18_66), inference(avatar_contradiction_clause, [], [f2037])).
fof(f2037, plain, ($false | (~ spl18_65 | ~ spl18_66)), inference(subsumption_resolution, [], [f2036, f258])).
fof(f2036, plain, ((e20 = e21) | (~ spl18_65 | ~ spl18_66)), inference(backward_demodulation, [], [f709, f705])).
fof(f705, plain, ((e20 = op2(e23, e23)) | ~ spl18_65), inference(avatar_component_clause, [], [f703])).
fof(f2035, plain, (spl18_172 | ~ spl18_66), inference(avatar_split_clause, [], [f2034, f707, f1265])).
fof(f2034, plain, ((e21 = h4(e11)) | ~ spl18_66), inference(backward_demodulation, [], [f332, f709])).
fof(f1991, plain, (~ spl18_235 | ~ spl18_84), inference(avatar_split_clause, [], [f1987, f783, f1635])).
fof(f1987, plain, (~ (e23 = h3(e11)) | ~ spl18_84), inference(backward_demodulation, [], [f1219, f785])).
fof(f1219, plain, ~ (op2(e22, e23) = h3(e11)), inference(backward_demodulation, [], [f245, f328])).
fof(f245, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f1990, plain, (spl18_68 | ~ spl18_84 | ~ spl18_149), inference(avatar_split_clause, [], [f1986, f1103, f783, f715])).
fof(f1986, plain, ((e23 = op2(e23, e23)) | (~ spl18_84 | ~ spl18_149)), inference(backward_demodulation, [], [f1105, f785])).
fof(f1985, plain, (spl18_182 | ~ spl18_85), inference(avatar_split_clause, [], [f1984, f788, f1319])).
fof(f1319, plain, (spl18_182 <=> (e20 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_182])])).
fof(f1984, plain, ((e20 = h3(e11)) | ~ spl18_85), inference(backward_demodulation, [], [f328, f790])).
fof(f790, plain, ((e20 = op2(e22, e22)) | ~ spl18_85), inference(avatar_component_clause, [], [f788])).
fof(f1940, plain, (spl18_190 | ~ spl18_105), inference(avatar_split_clause, [], [f1939, f873, f1360])).
fof(f1939, plain, ((e20 = h2(e11)) | ~ spl18_105), inference(backward_demodulation, [], [f324, f875])).
fof(f875, plain, ((e20 = op2(e21, e21)) | ~ spl18_105), inference(avatar_component_clause, [], [f873])).
fof(f1928, plain, (~ spl18_114 | ~ spl18_115), inference(avatar_contradiction_clause, [], [f1927])).
fof(f1927, plain, ($false | (~ spl18_114 | ~ spl18_115)), inference(subsumption_resolution, [], [f1926, f261])).
fof(f1926, plain, ((e21 = e22) | (~ spl18_114 | ~ spl18_115)), inference(backward_demodulation, [], [f917, f913])).
fof(f913, plain, ((e21 = op2(e20, e23)) | ~ spl18_114), inference(avatar_component_clause, [], [f911])).
fof(f911, plain, (spl18_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_114])])).
fof(f1925, plain, (~ spl18_115 | ~ spl18_116), inference(avatar_contradiction_clause, [], [f1924])).
fof(f1924, plain, ($false | (~ spl18_115 | ~ spl18_116)), inference(subsumption_resolution, [], [f1923, f263])).
fof(f1923, plain, ((e22 = e23) | (~ spl18_115 | ~ spl18_116)), inference(backward_demodulation, [], [f921, f917])).
fof(f921, plain, ((e23 = op2(e20, e23)) | ~ spl18_116), inference(avatar_component_clause, [], [f919])).
fof(f919, plain, (spl18_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_116])])).
fof(f1918, plain, (~ spl18_182 | ~ spl18_117), inference(avatar_split_clause, [], [f1914, f924, f1319])).
fof(f1914, plain, (~ (e20 = h3(e11)) | ~ spl18_117), inference(backward_demodulation, [], [f1214, f926])).
fof(f1911, plain, (~ spl18_73 | ~ spl18_121), inference(avatar_split_clause, [], [f1906, f941, f737])).
fof(f941, plain, (spl18_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_121])])).
fof(f1906, plain, (~ (e20 = op2(e23, e21)) | ~ spl18_121), inference(backward_demodulation, [], [f212, f943])).
fof(f943, plain, ((e20 = op2(e20, e21)) | ~ spl18_121), inference(avatar_component_clause, [], [f941])).
fof(f212, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f1894, plain, (~ spl18_77 | ~ spl18_125), inference(avatar_split_clause, [], [f1887, f958, f754])).
fof(f754, plain, (spl18_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_77])])).
fof(f1887, plain, (~ (e20 = op2(e23, e20)) | ~ spl18_125), inference(backward_demodulation, [], [f1198, f1884])).
fof(f1884, plain, ((e20 = h1(e11)) | ~ spl18_125), inference(backward_demodulation, [], [f320, f960])).
fof(f1198, plain, ~ (op2(e23, e20) = h1(e11)), inference(backward_demodulation, [], [f206, f320])).
fof(f206, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f1493, plain, (~ spl18_200 | ~ spl18_201 | ~ spl18_202 | ~ spl18_203 | spl18_173 | spl18_170 | spl18_167 | ~ spl18_204 | ~ spl18_205 | ~ spl18_206 | ~ spl18_207 | ~ spl18_208 | ~ spl18_209 | ~ spl18_210 | ~ spl18_211 | ~ spl18_212 | ~ spl18_213 | ~ spl18_214 | ~ spl18_215), inference(avatar_split_clause, [], [f1428, f1490, f1486, f1482, f1478, f1474, f1470, f1466, f1462, f1458, f1454, f1450, f1446, f1240, f1256, f1272, f1442, f1438, f1434, f1430])).
fof(f1272, plain, (spl18_173 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl18_173])])).
fof(f1256, plain, (spl18_170 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl18_170])])).
fof(f1240, plain, (spl18_167 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl18_167])])).
fof(f1428, plain, (~ (h1(e11) = h4(op1(e10, e10))) | ~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (h4(e12) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), inference(forward_demodulation, [], [f1427, f320])).
fof(f1427, plain, (~ (op2(e20, e20) = h4(op1(e10, e10))) | ~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (h4(e12) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), inference(forward_demodulation, [], [f1426, f1237])).
fof(f1426, plain, (~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (h4(e12) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1425, f1237])).
fof(f1425, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (h4(e12) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1424, f1237])).
fof(f1424, plain, (~ (h4(e12) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1423, f1223])).
fof(f1423, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1422, f1237])).
fof(f1422, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1421, f330])).
fof(f1421, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1420, f1237])).
fof(f1420, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1419, f330])).
fof(f1419, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1418, f1237])).
fof(f1418, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1417, f330])).
fof(f1417, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1416, f330])).
fof(f1416, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e20)) | ~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1415, f1237])).
fof(f1415, plain, (~ (e20 = h4(op1(e13, e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1414, f1234])).
fof(f1414, plain, (~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1413, f330])).
fof(f1413, plain, (~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1412, f330])).
fof(f1412, plain, (~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1411, f332])).
fof(f1411, plain, (~ (op2(e23, e23) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1410, f330])).
fof(f1410, plain, (sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f397, f330])).
fof(f397, plain, (~ (e23 = h4(e13)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f41])).
fof(f41, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP14 | sP13 | sP12 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP11 | sP10 | sP9 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP8 | sP7 | sP6 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e40, e39, e38, e37, e36, e35, e34, e33, e32, e31, e30, e29])).
fof(f29, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP6), inference(usedef, [], [e29])).
fof(e29, plain, (sP6 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f30, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP7), inference(usedef, [], [e30])).
fof(e30, plain, (sP7 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f31, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP8), inference(usedef, [], [e31])).
fof(e31, plain, (sP8 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f32, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP9), inference(usedef, [], [e32])).
fof(e32, plain, (sP9 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f33, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP10), inference(usedef, [], [e33])).
fof(e33, plain, (sP10 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f34, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP11), inference(usedef, [], [e34])).
fof(e34, plain, (sP11 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f35, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP12), inference(usedef, [], [e35])).
fof(e35, plain, (sP12 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f36, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP13), inference(usedef, [], [e36])).
fof(e36, plain, (sP13 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f37, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP14), inference(usedef, [], [e37])).
fof(e37, plain, (sP14 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f38, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP15), inference(usedef, [], [e38])).
fof(e38, plain, (sP15 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f39, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP16), inference(usedef, [], [e39])).
fof(e39, plain, (sP16 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f40, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP17), inference(usedef, [], [e40])).
fof(e40, plain, (sP17 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', co1)).
fof(f1286, plain, ~ spl18_173, inference(avatar_split_clause, [], [f1285, f1272])).
fof(f1285, plain, ~ sP15, inference(subsumption_resolution, [], [f342, f1237])).
fof(f342, plain, (~ (e20 = h4(e10)) | ~ sP15), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP15), inference(nnf_transformation, [], [f38])).
fof(f1268, plain, (~ spl18_170 | ~ spl18_172), inference(avatar_split_clause, [], [f339, f1265, f1256])).
fof(f339, plain, (~ (e21 = h4(e11)) | ~ sP16), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP16), inference(nnf_transformation, [], [f39])).
fof(f1247, plain, (~ spl18_167 | ~ spl18_168), inference(avatar_split_clause, [], [f336, f1244, f1240])).
fof(f336, plain, (~ (e22 = h4(e12)) | ~ sP17), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP17), inference(nnf_transformation, [], [f40])).
fof(f1195, plain, spl18_115, inference(avatar_split_clause, [], [f1194, f915])).
fof(f1194, plain, (e22 = op2(e20, e23)), inference(backward_demodulation, [], [f317, f315])).
fof(f317, plain, (e22 = op2(op2(e23, op2(e23, e23)), e23)), inference(cnf_transformation, [], [f13])).
fof(f1193, plain, spl18_66, inference(avatar_split_clause, [], [f316, f707])).
fof(f316, plain, (e21 = op2(e23, e23)), inference(cnf_transformation, [], [f13])).
fof(f1192, plain, spl18_51, inference(avatar_split_clause, [], [f1191, f611])).
fof(f1191, plain, (e12 = op1(e10, e13)), inference(backward_demodulation, [], [f314, f312])).
fof(f314, plain, (e12 = op1(op1(e13, op1(e13, e13)), e13)), inference(cnf_transformation, [], [f12])).
fof(f1190, plain, spl18_2, inference(avatar_split_clause, [], [f313, f403])).
fof(f313, plain, (e11 = op1(e13, e13)), inference(cnf_transformation, [], [f12])).
fof(f1184, plain, (spl18_158 | spl18_153 | spl18_148 | spl18_165), inference(avatar_split_clause, [], [f309, f1181, f1099, f1123, f1147])).
fof(f1147, plain, (spl18_158 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl18_158])])).
fof(f1123, plain, (spl18_153 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl18_153])])).
fof(f1099, plain, (spl18_148 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl18_148])])).
fof(f309, plain, ((e21 = op2(op2(e23, e21), op2(e23, e21))) | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f28])).
fof(f28, plain, (((e23 = op2(op2(e23, e23), op2(e23, e23))) & (e22 = op2(op2(e23, e22), op2(e23, e22))) & (e21 = op2(op2(e23, e21), op2(e23, e21))) & (e20 = op2(op2(e23, e20), op2(e23, e20)))) | sP5 | sP4 | sP3), inference(definition_folding, [], [f11, e27, e26, e25])).
fof(f25, plain, (((e23 = op2(op2(e20, e23), op2(e20, e23))) & (e22 = op2(op2(e20, e22), op2(e20, e22))) & (e21 = op2(op2(e20, e21), op2(e20, e21))) & (e20 = op2(op2(e20, e20), op2(e20, e20)))) | ~ sP3), inference(usedef, [], [e25])).
fof(e25, plain, (sP3 <=> ((e23 = op2(op2(e20, e23), op2(e20, e23))) & (e22 = op2(op2(e20, e22), op2(e20, e22))) & (e21 = op2(op2(e20, e21), op2(e20, e21))) & (e20 = op2(op2(e20, e20), op2(e20, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f26, plain, (((e23 = op2(op2(e21, e23), op2(e21, e23))) & (e22 = op2(op2(e21, e22), op2(e21, e22))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(op2(e21, e20), op2(e21, e20)))) | ~ sP4), inference(usedef, [], [e26])).
fof(e26, plain, (sP4 <=> ((e23 = op2(op2(e21, e23), op2(e21, e23))) & (e22 = op2(op2(e21, e22), op2(e21, e22))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(op2(e21, e20), op2(e21, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f27, plain, (((e23 = op2(op2(e22, e23), op2(e22, e23))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e21 = op2(op2(e22, e21), op2(e22, e21))) & (e20 = op2(op2(e22, e20), op2(e22, e20)))) | ~ sP5), inference(usedef, [], [e27])).
fof(e27, plain, (sP5 <=> ((e23 = op2(op2(e22, e23), op2(e22, e23))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e21 = op2(op2(e22, e21), op2(e22, e21))) & (e20 = op2(op2(e22, e20), op2(e22, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f11, plain, (((e23 = op2(op2(e23, e23), op2(e23, e23))) & (e22 = op2(op2(e23, e22), op2(e23, e22))) & (e21 = op2(op2(e23, e21), op2(e23, e21))) & (e20 = op2(op2(e23, e20), op2(e23, e20)))) | ((e23 = op2(op2(e22, e23), op2(e22, e23))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e21 = op2(op2(e22, e21), op2(e22, e21))) & (e20 = op2(op2(e22, e20), op2(e22, e20)))) | ((e23 = op2(op2(e21, e23), op2(e21, e23))) & (e22 = op2(op2(e21, e22), op2(e21, e22))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(op2(e21, e20), op2(e21, e20)))) | ((e23 = op2(op2(e20, e23), op2(e20, e23))) & (e22 = op2(op2(e20, e22), op2(e20, e22))) & (e21 = op2(op2(e20, e21), op2(e20, e21))) & (e20 = op2(op2(e20, e20), op2(e20, e20))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax11)).
fof(f1174, plain, (spl18_158 | spl18_153 | spl18_148 | spl18_163), inference(avatar_split_clause, [], [f311, f1171, f1099, f1123, f1147])).
fof(f311, plain, ((e23 = op2(op2(e23, e23), op2(e23, e23))) | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f28])).
fof(f1164, plain, (~ spl18_158 | spl18_161), inference(avatar_split_clause, [], [f305, f1161, f1147])).
fof(f305, plain, ((e21 = op2(op2(e20, e21), op2(e20, e21))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f47, plain, (((e23 = op2(op2(e20, e23), op2(e20, e23))) & (e22 = op2(op2(e20, e22), op2(e20, e22))) & (e21 = op2(op2(e20, e21), op2(e20, e21))) & (e20 = op2(op2(e20, e20), op2(e20, e20)))) | ~ sP3), inference(nnf_transformation, [], [f25])).
fof(f1159, plain, (~ spl18_158 | spl18_160), inference(avatar_split_clause, [], [f306, f1156, f1147])).
fof(f306, plain, ((e22 = op2(op2(e20, e22), op2(e20, e22))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f1154, plain, (~ spl18_158 | spl18_159), inference(avatar_split_clause, [], [f307, f1151, f1147])).
fof(f307, plain, ((e23 = op2(op2(e20, e23), op2(e20, e23))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f1145, plain, (~ spl18_153 | spl18_157), inference(avatar_split_clause, [], [f300, f1142, f1123])).
fof(f300, plain, ((e20 = op2(op2(e21, e20), op2(e21, e20))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f46, plain, (((e23 = op2(op2(e21, e23), op2(e21, e23))) & (e22 = op2(op2(e21, e22), op2(e21, e22))) & (e21 = op2(op2(e21, e21), op2(e21, e21))) & (e20 = op2(op2(e21, e20), op2(e21, e20)))) | ~ sP4), inference(nnf_transformation, [], [f26])).
fof(f1135, plain, (~ spl18_153 | spl18_155), inference(avatar_split_clause, [], [f302, f1132, f1123])).
fof(f302, plain, ((e22 = op2(op2(e21, e22), op2(e21, e22))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f1130, plain, (~ spl18_153 | spl18_154), inference(avatar_split_clause, [], [f303, f1127, f1123])).
fof(f303, plain, ((e23 = op2(op2(e21, e23), op2(e21, e23))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f1121, plain, (~ spl18_148 | spl18_152), inference(avatar_split_clause, [], [f296, f1118, f1099])).
fof(f296, plain, ((e20 = op2(op2(e22, e20), op2(e22, e20))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f45, plain, (((e23 = op2(op2(e22, e23), op2(e22, e23))) & (e22 = op2(op2(e22, e22), op2(e22, e22))) & (e21 = op2(op2(e22, e21), op2(e22, e21))) & (e20 = op2(op2(e22, e20), op2(e22, e20)))) | ~ sP5), inference(nnf_transformation, [], [f27])).
fof(f1111, plain, (~ spl18_148 | spl18_150), inference(avatar_split_clause, [], [f298, f1108, f1099])).
fof(f298, plain, ((e22 = op2(op2(e22, e22), op2(e22, e22))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f1106, plain, (~ spl18_148 | spl18_149), inference(avatar_split_clause, [], [f299, f1103, f1099])).
fof(f299, plain, ((e23 = op2(op2(e22, e23), op2(e22, e23))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f1097, plain, (spl18_139 | spl18_134 | spl18_129 | spl18_147), inference(avatar_split_clause, [], [f292, f1094, f1007, f1031, f1055])).
fof(f1055, plain, (spl18_139 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl18_139])])).
fof(f1031, plain, (spl18_134 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl18_134])])).
fof(f1007, plain, (spl18_129 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl18_129])])).
fof(f292, plain, ((e10 = op1(op1(e13, e10), op1(e13, e10))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f24])).
fof(f24, plain, (((e13 = op1(op1(e13, e13), op1(e13, e13))) & (e12 = op1(op1(e13, e12), op1(e13, e12))) & (e11 = op1(op1(e13, e11), op1(e13, e11))) & (e10 = op1(op1(e13, e10), op1(e13, e10)))) | sP2 | sP1 | sP0), inference(definition_folding, [], [f10, e23, e22, e21])).
fof(f21, plain, (((e13 = op1(op1(e10, e13), op1(e10, e13))) & (e12 = op1(op1(e10, e12), op1(e10, e12))) & (e11 = op1(op1(e10, e11), op1(e10, e11))) & (e10 = op1(op1(e10, e10), op1(e10, e10)))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> ((e13 = op1(op1(e10, e13), op1(e10, e13))) & (e12 = op1(op1(e10, e12), op1(e10, e12))) & (e11 = op1(op1(e10, e11), op1(e10, e11))) & (e10 = op1(op1(e10, e10), op1(e10, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, (((e13 = op1(op1(e11, e13), op1(e11, e13))) & (e12 = op1(op1(e11, e12), op1(e11, e12))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(op1(e11, e10), op1(e11, e10)))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> ((e13 = op1(op1(e11, e13), op1(e11, e13))) & (e12 = op1(op1(e11, e12), op1(e11, e12))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(op1(e11, e10), op1(e11, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, (((e13 = op1(op1(e12, e13), op1(e12, e13))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e11 = op1(op1(e12, e11), op1(e12, e11))) & (e10 = op1(op1(e12, e10), op1(e12, e10)))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> ((e13 = op1(op1(e12, e13), op1(e12, e13))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e11 = op1(op1(e12, e11), op1(e12, e11))) & (e10 = op1(op1(e12, e10), op1(e12, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f10, plain, (((e13 = op1(op1(e13, e13), op1(e13, e13))) & (e12 = op1(op1(e13, e12), op1(e13, e12))) & (e11 = op1(op1(e13, e11), op1(e13, e11))) & (e10 = op1(op1(e13, e10), op1(e13, e10)))) | ((e13 = op1(op1(e12, e13), op1(e12, e13))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e11 = op1(op1(e12, e11), op1(e12, e11))) & (e10 = op1(op1(e12, e10), op1(e12, e10)))) | ((e13 = op1(op1(e11, e13), op1(e11, e13))) & (e12 = op1(op1(e11, e12), op1(e11, e12))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(op1(e11, e10), op1(e11, e10)))) | ((e13 = op1(op1(e10, e13), op1(e10, e13))) & (e12 = op1(op1(e10, e12), op1(e10, e12))) & (e11 = op1(op1(e10, e11), op1(e10, e11))) & (e10 = op1(op1(e10, e10), op1(e10, e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax10)).
fof(f1087, plain, (spl18_139 | spl18_134 | spl18_129 | spl18_145), inference(avatar_split_clause, [], [f294, f1084, f1007, f1031, f1055])).
fof(f294, plain, ((e12 = op1(op1(e13, e12), op1(e13, e12))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f24])).
fof(f1067, plain, (~ spl18_139 | spl18_141), inference(avatar_split_clause, [], [f290, f1064, f1055])).
fof(f290, plain, ((e12 = op1(op1(e10, e12), op1(e10, e12))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f44, plain, (((e13 = op1(op1(e10, e13), op1(e10, e13))) & (e12 = op1(op1(e10, e12), op1(e10, e12))) & (e11 = op1(op1(e10, e11), op1(e10, e11))) & (e10 = op1(op1(e10, e10), op1(e10, e10)))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1062, plain, (~ spl18_139 | spl18_140), inference(avatar_split_clause, [], [f291, f1059, f1055])).
fof(f291, plain, ((e13 = op1(op1(e10, e13), op1(e10, e13))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f1048, plain, (~ spl18_134 | spl18_137), inference(avatar_split_clause, [], [f285, f1045, f1031])).
fof(f285, plain, ((e11 = op1(op1(e11, e11), op1(e11, e11))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f43, plain, (((e13 = op1(op1(e11, e13), op1(e11, e13))) & (e12 = op1(op1(e11, e12), op1(e11, e12))) & (e11 = op1(op1(e11, e11), op1(e11, e11))) & (e10 = op1(op1(e11, e10), op1(e11, e10)))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1043, plain, (~ spl18_134 | spl18_136), inference(avatar_split_clause, [], [f286, f1040, f1031])).
fof(f286, plain, ((e12 = op1(op1(e11, e12), op1(e11, e12))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f1038, plain, (~ spl18_134 | spl18_135), inference(avatar_split_clause, [], [f287, f1035, f1031])).
fof(f287, plain, ((e13 = op1(op1(e11, e13), op1(e11, e13))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f1029, plain, (~ spl18_129 | spl18_133), inference(avatar_split_clause, [], [f280, f1026, f1007])).
fof(f280, plain, ((e10 = op1(op1(e12, e10), op1(e12, e10))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f42, plain, (((e13 = op1(op1(e12, e13), op1(e12, e13))) & (e12 = op1(op1(e12, e12), op1(e12, e12))) & (e11 = op1(op1(e12, e11), op1(e12, e11))) & (e10 = op1(op1(e12, e10), op1(e12, e10)))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1024, plain, (~ spl18_129 | spl18_132), inference(avatar_split_clause, [], [f281, f1021, f1007])).
fof(f281, plain, ((e11 = op1(op1(e12, e11), op1(e12, e11))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f1014, plain, (~ spl18_129 | spl18_130), inference(avatar_split_clause, [], [f283, f1011, f1007])).
fof(f283, plain, ((e13 = op1(op1(e12, e13), op1(e12, e13))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f1005, plain, (spl18_125 | spl18_121 | spl18_117 | spl18_113), inference(avatar_split_clause, [], [f124, f907, f924, f941, f958])).
fof(f124, plain, ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax4)).
fof(f1003, plain, (spl18_126 | spl18_122 | spl18_118 | spl18_114), inference(avatar_split_clause, [], [f126, f911, f928, f945, f962])).
fof(f126, plain, ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f997, plain, (spl18_109 | spl18_105 | spl18_101 | spl18_97), inference(avatar_split_clause, [], [f132, f839, f856, f873, f890])).
fof(f132, plain, ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f995, plain, (spl18_110 | spl18_106 | spl18_102 | spl18_98), inference(avatar_split_clause, [], [f134, f843, f860, f877, f894])).
fof(f134, plain, ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f991, plain, (spl18_112 | spl18_108 | spl18_104 | spl18_100), inference(avatar_split_clause, [], [f138, f851, f868, f885, f902])).
fof(f138, plain, ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f987, plain, (spl18_94 | spl18_90 | spl18_86 | spl18_82), inference(avatar_split_clause, [], [f142, f775, f792, f809, f826])).
fof(f142, plain, ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f985, plain, (spl18_95 | spl18_91 | spl18_87 | spl18_83), inference(avatar_split_clause, [], [f144, f779, f796, f813, f830])).
fof(f144, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f984, plain, (spl18_119 | spl18_103 | spl18_87 | spl18_71), inference(avatar_split_clause, [], [f145, f728, f796, f864, f932])).
fof(f145, plain, ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f983, plain, (spl18_96 | spl18_92 | spl18_88 | spl18_84), inference(avatar_split_clause, [], [f146, f783, f800, f817, f834])).
fof(f146, plain, ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f980, plain, (spl18_113 | spl18_97 | spl18_81 | spl18_65), inference(avatar_split_clause, [], [f149, f703, f771, f839, f907])).
fof(f149, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f974, plain, (spl18_116 | spl18_100 | spl18_84 | spl18_68), inference(avatar_split_clause, [], [f155, f715, f783, f851, f919])).
fof(f155, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f956, plain, (spl18_121 | spl18_122 | spl18_123 | spl18_124), inference(avatar_split_clause, [], [f109, f953, f949, f945, f941])).
fof(f109, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax3)).
fof(f939, plain, (spl18_117 | spl18_118 | spl18_119 | spl18_120), inference(avatar_split_clause, [], [f110, f936, f932, f928, f924])).
fof(f110, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f905, plain, (spl18_109 | spl18_110 | spl18_111 | spl18_112), inference(avatar_split_clause, [], [f112, f902, f898, f894, f890])).
fof(f112, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f871, plain, (spl18_101 | spl18_102 | spl18_103 | spl18_104), inference(avatar_split_clause, [], [f114, f868, f864, f860, f856])).
fof(f114, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f837, plain, (spl18_93 | spl18_94 | spl18_95 | spl18_96), inference(avatar_split_clause, [], [f116, f834, f830, f826, f822])).
fof(f116, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f786, plain, (spl18_81 | spl18_82 | spl18_83 | spl18_84), inference(avatar_split_clause, [], [f119, f783, f779, f775, f771])).
fof(f119, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f3])).
fof(f769, plain, (spl18_77 | spl18_78 | spl18_79 | spl18_80), inference(avatar_split_clause, [], [f120, f766, f762, f758, f754])).
fof(f120, plain, ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f3])).
fof(f701, plain, (spl18_61 | spl18_57 | spl18_53 | spl18_49), inference(avatar_split_clause, [], [f76, f603, f620, f637, f654])).
fof(f76, plain, ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax2)).
fof(f696, plain, (spl18_63 | spl18_47 | spl18_31 | spl18_15), inference(avatar_split_clause, [], [f81, f458, f526, f594, f662])).
fof(f81, plain, ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f695, plain, (spl18_64 | spl18_60 | spl18_56 | spl18_52), inference(avatar_split_clause, [], [f82, f615, f632, f649, f666])).
fof(f82, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f690, plain, (spl18_58 | spl18_42 | spl18_26 | spl18_10), inference(avatar_split_clause, [], [f87, f437, f505, f573, f641])).
fof(f87, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f688, plain, (spl18_59 | spl18_43 | spl18_27 | spl18_11), inference(avatar_split_clause, [], [f89, f441, f509, f577, f645])).
fof(f89, plain, ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f680, plain, (spl18_55 | spl18_39 | spl18_23 | spl18_7), inference(avatar_split_clause, [], [f97, f424, f492, f560, f628])).
fof(f97, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f678, plain, (spl18_56 | spl18_40 | spl18_24 | spl18_8), inference(avatar_split_clause, [], [f99, f428, f496, f564, f632])).
fof(f99, plain, ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f676, plain, (spl18_49 | spl18_33 | spl18_17 | spl18_1), inference(avatar_split_clause, [], [f101, f399, f467, f535, f603])).
fof(f101, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f670, plain, (spl18_52 | spl18_36 | spl18_20 | spl18_4), inference(avatar_split_clause, [], [f107, f411, f479, f547, f615])).
fof(f107, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f669, plain, (spl18_61 | spl18_62 | spl18_63 | spl18_64), inference(avatar_split_clause, [], [f60, f666, f662, f658, f654])).
fof(f60, plain, ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG116+1.p', ax1)).
fof(f652, plain, (spl18_57 | spl18_58 | spl18_59 | spl18_60), inference(avatar_split_clause, [], [f61, f649, f645, f641, f637])).
fof(f61, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f635, plain, (spl18_53 | spl18_54 | spl18_55 | spl18_56), inference(avatar_split_clause, [], [f62, f632, f628, f624, f620])).
fof(f62, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f601, plain, (spl18_45 | spl18_46 | spl18_47 | spl18_48), inference(avatar_split_clause, [], [f64, f598, f594, f590, f586])).
fof(f64, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f567, plain, (spl18_37 | spl18_38 | spl18_39 | spl18_40), inference(avatar_split_clause, [], [f66, f564, f560, f556, f552])).
fof(f66, plain, ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))), inference(cnf_transformation, [], [f1])).
fof(f533, plain, (spl18_29 | spl18_30 | spl18_31 | spl18_32), inference(avatar_split_clause, [], [f68, f530, f526, f522, f518])).
fof(f68, plain, ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f516, plain, (spl18_25 | spl18_26 | spl18_27 | spl18_28), inference(avatar_split_clause, [], [f69, f513, f509, f505, f501])).
fof(f69, plain, ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f465, plain, (spl18_13 | spl18_14 | spl18_15 | spl18_16), inference(avatar_split_clause, [], [f72, f462, f458, f454, f450])).
fof(f72, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).
fof(f431, plain, (spl18_5 | spl18_6 | spl18_7 | spl18_8), inference(avatar_split_clause, [], [f74, f428, f424, f420, f416])).
fof(f74, plain, ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))), inference(cnf_transformation, [], [f1])).