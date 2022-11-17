fof(f3655, plain, $false, inference(avatar_sat_refutation, [], [f686, f704, f706, f710, f711, f714, f715, f716, f717, f721, f722, f723, f728, f729, f731, f734, f735, f837, f871, f905, f973, f990, f1008, f1010, f1014, f1015, f1017, f1019, f1020, f1021, f1026, f1033, f1038, f1053, f1058, f1063, f1077, f1087, f1101, f1111, f1112, f1113, f1114, f1116, f1118, f1119, f1120, f1121, f1124, f1134, f1139, f1144, f1158, f1163, f1168, f1182, f1192, f1206, f1216, f1217, f1218, f1220, f1221, f1222, f1224, f1225, f1226, f1229, f1239, f1244, f1249, f1250, f1252, f1253, f1255, f1307, f1328, f1346, f1553, f1971, f1982, f1992, f2002, f2027, f2079, f2085, f2096, f2099, f2124, f2135, f2145, f2156, f2183, f2224, f2231, f2232, f2236, f2278, f2280, f2289, f2295, f2304, f2312, f2320, f2322, f2326, f2328, f2342, f2357, f2379, f2398, f2420, f2421, f2424, f2443, f2449, f2453, f2475, f2481, f2503, f2525, f2532, f2537, f2556, f2563, f2592, f2606, f2609, f2612, f2618, f2643, f2655, f2659, f2665, f2693, f2699, f2754, f2763, f2777, f2781, f2783, f2800, f2803, f2825, f2855, f2866, f2871, f2892, f2918, f2932, f2937, f2956, f2960, f2965, f2986, f2994, f3015, f3045, f3047, f3063, f3064, f3066, f3081, f3082, f3103, f3115, f3128, f3151, f3159, f3200, f3211, f3224, f3232, f3264, f3287, f3304, f3326, f3357, f3384, f3404, f3424, f3437, f3453, f3457, f3467, f3547, f3554, f3578, f3579, f3624])).
fof(f3624, plain, (~ spl18_116 | ~ spl18_120), inference(avatar_split_clause, [], [f3623, f970, f953])).
fof(f953, plain, (spl18_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_116])])).
fof(f970, plain, (spl18_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_120])])).
fof(f3623, plain, (~ (e23 = op2(e20, e23)) | ~ spl18_120), inference(backward_demodulation, [], [f233, f972])).
fof(f972, plain, ((e23 = op2(e20, e22)) | ~ spl18_120), inference(avatar_component_clause, [], [f970])).
fof(f233, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax6)).
fof(f3579, plain, (~ spl18_103 | ~ spl18_91 | spl18_151), inference(avatar_split_clause, [], [f3572, f1160, f847, f898])).
fof(f898, plain, (spl18_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_103])])).
fof(f847, plain, (spl18_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_91])])).
fof(f1160, plain, (spl18_151 <=> (op2(e22, e21) = op2(e21, op2(e22, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl18_151])])).
fof(f3572, plain, (~ (e22 = op2(e21, e22)) | (~ spl18_91 | spl18_151)), inference(backward_demodulation, [], [f1162, f849])).
fof(f849, plain, ((e22 = op2(e22, e21)) | ~ spl18_91), inference(avatar_component_clause, [], [f847])).
fof(f1162, plain, (~ (op2(e22, e21) = op2(e21, op2(e22, e21))) | spl18_151), inference(avatar_component_clause, [], [f1160])).
fof(f3578, plain, (~ spl18_100 | ~ spl18_76 | spl18_165), inference(avatar_split_clause, [], [f2076, f1241, f783, f885])).
fof(f885, plain, (spl18_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_100])])).
fof(f783, plain, (spl18_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_76])])).
fof(f1241, plain, (spl18_165 <=> (op2(e23, e21) = op2(e21, op2(e23, e21)))), introduced(avatar_definition, [new_symbols(naming, [spl18_165])])).
fof(f2076, plain, (~ (e23 = op2(e21, e23)) | (~ spl18_76 | spl18_165)), inference(backward_demodulation, [], [f1243, f785])).
fof(f785, plain, ((e23 = op2(e23, e21)) | ~ spl18_76), inference(avatar_component_clause, [], [f783])).
fof(f1243, plain, (~ (op2(e23, e21) = op2(e21, op2(e23, e21))) | spl18_165), inference(avatar_component_clause, [], [f1241])).
fof(f3554, plain, (~ spl18_87 | ~ spl18_23 | ~ spl18_168 | spl18_202), inference(avatar_split_clause, [], [f3553, f1498, f1304, f526, f830])).
fof(f830, plain, (spl18_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_87])])).
fof(f526, plain, (spl18_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_23])])).
fof(f1304, plain, (spl18_168 <=> (e22 = h4(e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_168])])).
fof(f1498, plain, (spl18_202 <=> (h4(op1(e12, e12)) = op2(h4(e12), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_202])])).
fof(f3553, plain, (~ (e22 = op2(e22, e22)) | (~ spl18_23 | ~ spl18_168 | spl18_202)), inference(forward_demodulation, [], [f3552, f1305])).
fof(f1305, plain, ((e22 = h4(e12)) | ~ spl18_168), inference(avatar_component_clause, [], [f1304])).
fof(f3552, plain, (~ (op2(e22, e22) = h4(e12)) | (~ spl18_23 | ~ spl18_168 | spl18_202)), inference(forward_demodulation, [], [f3551, f528])).
fof(f528, plain, ((e12 = op1(e12, e12)) | ~ spl18_23), inference(avatar_component_clause, [], [f526])).
fof(f3551, plain, (~ (op2(e22, e22) = h4(op1(e12, e12))) | (~ spl18_168 | spl18_202)), inference(forward_demodulation, [], [f1500, f1305])).
fof(f1500, plain, (~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | spl18_202), inference(avatar_component_clause, [], [f1498])).
fof(f3547, plain, (~ spl18_122 | ~ spl18_110 | spl18_157), inference(avatar_split_clause, [], [f2884, f1189, f928, f979])).
fof(f979, plain, (spl18_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_122])])).
fof(f928, plain, (spl18_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_110])])).
fof(f1189, plain, (spl18_157 <=> (op2(e21, e20) = op2(e20, op2(e21, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_157])])).
fof(f2884, plain, (~ (e21 = op2(e20, e21)) | (~ spl18_110 | spl18_157)), inference(forward_demodulation, [], [f1191, f930])).
fof(f930, plain, ((e21 = op2(e21, e20)) | ~ spl18_110), inference(avatar_component_clause, [], [f928])).
fof(f1191, plain, (~ (op2(e21, e20) = op2(e20, op2(e21, e20))) | spl18_157), inference(avatar_component_clause, [], [f1189])).
fof(f3467, plain, (~ spl18_59 | ~ spl18_123 | ~ spl18_168 | ~ spl18_172 | spl18_214), inference(avatar_contradiction_clause, [], [f3466])).
fof(f3466, plain, ($false | (~ spl18_59 | ~ spl18_123 | ~ spl18_168 | ~ spl18_172 | spl18_214)), inference(subsumption_resolution, [], [f3465, f985])).
fof(f985, plain, ((e22 = op2(e20, e21)) | ~ spl18_123), inference(avatar_component_clause, [], [f983])).
fof(f983, plain, (spl18_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_123])])).
fof(f3465, plain, (~ (e22 = op2(e20, e21)) | (~ spl18_59 | ~ spl18_168 | ~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f3464, f1305])).
fof(f3464, plain, (~ (op2(e20, e21) = h4(e12)) | (~ spl18_59 | ~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f3463, f681])).
fof(f681, plain, ((e12 = op1(e10, e11)) | ~ spl18_59), inference(avatar_component_clause, [], [f679])).
fof(f679, plain, (spl18_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_59])])).
fof(f3463, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | (~ spl18_172 | spl18_214)), inference(forward_demodulation, [], [f1548, f1326])).
fof(f1326, plain, ((e21 = h4(e11)) | ~ spl18_172), inference(avatar_component_clause, [], [f1325])).
fof(f1325, plain, (spl18_172 <=> (e21 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_172])])).
fof(f1548, plain, (~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | spl18_214), inference(avatar_component_clause, [], [f1546])).
fof(f1546, plain, (spl18_214 <=> (h4(op1(e10, e11)) = op2(e20, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_214])])).
fof(f3457, plain, (~ spl18_61 | ~ spl18_125 | spl18_215), inference(avatar_contradiction_clause, [], [f3456])).
fof(f3456, plain, ($false | (~ spl18_61 | ~ spl18_125 | spl18_215)), inference(subsumption_resolution, [], [f3455, f3051])).
fof(f3051, plain, ((e20 = h1(e11)) | ~ spl18_125), inference(backward_demodulation, [], [f354, f994])).
fof(f994, plain, ((e20 = op2(e20, e20)) | ~ spl18_125), inference(avatar_component_clause, [], [f992])).
fof(f992, plain, (spl18_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_125])])).
fof(f354, plain, (op2(e20, e20) = h1(e11)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((h1(e12) = op2(e20, op2(op2(e20, e20), op2(e20, e20)))) & (op2(e20, e20) = h1(e11)) & (h1(e10) = op2(op2(e20, e20), op2(e20, e20))) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax14)).
fof(f3455, plain, (~ (e20 = h1(e11)) | (~ spl18_61 | spl18_215)), inference(forward_demodulation, [], [f3454, f1297])).
fof(f1297, plain, (e20 = h4(e10)), inference(forward_demodulation, [], [f1296, f1294])).
fof(f1294, plain, (e20 = op2(h4(e11), h4(e11))), inference(backward_demodulation, [], [f349, f366])).
fof(f366, plain, (op2(e23, e23) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, op2(op2(e23, e23), op2(e23, e23))) = h4(e12)) & (op2(e23, e23) = h4(e11)) & (op2(op2(e23, e23), op2(e23, e23)) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax17)).
fof(f349, plain, (e20 = op2(op2(e23, e23), op2(e23, e23))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e22 = op2(e23, op2(op2(e23, e23), op2(e23, e23)))) & (e21 = op2(e23, e23)) & (e20 = op2(op2(e23, e23), op2(e23, e23)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax13)).
fof(f1296, plain, (h4(e10) = op2(h4(e11), h4(e11))), inference(forward_demodulation, [], [f365, f366])).
fof(f365, plain, (op2(op2(e23, e23), op2(e23, e23)) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f3454, plain, (~ (h1(e11) = h4(e10)) | (~ spl18_61 | spl18_215)), inference(forward_demodulation, [], [f1552, f690])).
fof(f690, plain, ((e10 = op1(e10, e10)) | ~ spl18_61), inference(avatar_component_clause, [], [f688])).
fof(f688, plain, (spl18_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_61])])).
fof(f1552, plain, (~ (h1(e11) = h4(op1(e10, e10))) | spl18_215), inference(avatar_component_clause, [], [f1550])).
fof(f1550, plain, (spl18_215 <=> (h1(e11) = h4(op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_215])])).
fof(f3453, plain, (~ spl18_17 | ~ spl18_81 | ~ spl18_168 | spl18_207), inference(avatar_contradiction_clause, [], [f3452])).
fof(f3452, plain, ($false | (~ spl18_17 | ~ spl18_81 | ~ spl18_168 | spl18_207)), inference(subsumption_resolution, [], [f3451, f807])).
fof(f807, plain, ((e20 = op2(e22, e23)) | ~ spl18_81), inference(avatar_component_clause, [], [f805])).
fof(f805, plain, (spl18_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_81])])).
fof(f3451, plain, (~ (e20 = op2(e22, e23)) | (~ spl18_17 | ~ spl18_168 | spl18_207)), inference(forward_demodulation, [], [f3450, f1297])).
fof(f3450, plain, (~ (op2(e22, e23) = h4(e10)) | (~ spl18_17 | ~ spl18_168 | spl18_207)), inference(forward_demodulation, [], [f3449, f503])).
fof(f503, plain, ((e10 = op1(e12, e13)) | ~ spl18_17), inference(avatar_component_clause, [], [f501])).
fof(f501, plain, (spl18_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_17])])).
fof(f3449, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | (~ spl18_168 | spl18_207)), inference(forward_demodulation, [], [f1520, f1305])).
fof(f1520, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | spl18_207), inference(avatar_component_clause, [], [f1518])).
fof(f1518, plain, (spl18_207 <=> (h4(op1(e12, e13)) = op2(h4(e12), e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_207])])).
fof(f3437, plain, (~ spl18_54 | ~ spl18_118 | ~ spl18_168 | ~ spl18_172 | spl18_213), inference(avatar_contradiction_clause, [], [f3436])).
fof(f3436, plain, ($false | (~ spl18_54 | ~ spl18_118 | ~ spl18_168 | ~ spl18_172 | spl18_213)), inference(subsumption_resolution, [], [f3435, f964])).
fof(f964, plain, ((e21 = op2(e20, e22)) | ~ spl18_118), inference(avatar_component_clause, [], [f962])).
fof(f962, plain, (spl18_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_118])])).
fof(f3435, plain, (~ (e21 = op2(e20, e22)) | (~ spl18_54 | ~ spl18_168 | ~ spl18_172 | spl18_213)), inference(forward_demodulation, [], [f3434, f1326])).
fof(f3434, plain, (~ (op2(e20, e22) = h4(e11)) | (~ spl18_54 | ~ spl18_168 | spl18_213)), inference(forward_demodulation, [], [f3433, f660])).
fof(f660, plain, ((e11 = op1(e10, e12)) | ~ spl18_54), inference(avatar_component_clause, [], [f658])).
fof(f658, plain, (spl18_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_54])])).
fof(f3433, plain, (~ (op2(e20, e22) = h4(op1(e10, e12))) | (~ spl18_168 | spl18_213)), inference(forward_demodulation, [], [f1544, f1305])).
fof(f1544, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | spl18_213), inference(avatar_component_clause, [], [f1542])).
fof(f1542, plain, (spl18_213 <=> (h4(op1(e10, e12)) = op2(e20, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_213])])).
fof(f3424, plain, (~ spl18_46 | ~ spl18_110 | ~ spl18_172 | spl18_211), inference(avatar_contradiction_clause, [], [f3423])).
fof(f3423, plain, ($false | (~ spl18_46 | ~ spl18_110 | ~ spl18_172 | spl18_211)), inference(subsumption_resolution, [], [f3422, f930])).
fof(f3422, plain, (~ (e21 = op2(e21, e20)) | (~ spl18_46 | ~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f3421, f1326])).
fof(f3421, plain, (~ (op2(e21, e20) = h4(e11)) | (~ spl18_46 | ~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f3420, f626])).
fof(f626, plain, ((e11 = op1(e11, e10)) | ~ spl18_46), inference(avatar_component_clause, [], [f624])).
fof(f624, plain, (spl18_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_46])])).
fof(f3420, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | (~ spl18_172 | spl18_211)), inference(forward_demodulation, [], [f1536, f1326])).
fof(f1536, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | spl18_211), inference(avatar_component_clause, [], [f1534])).
fof(f1534, plain, (spl18_211 <=> (h4(op1(e11, e10)) = op2(h4(e11), e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_211])])).
fof(f3404, plain, (~ spl18_52 | ~ spl18_116 | spl18_212), inference(avatar_contradiction_clause, [], [f3403])).
fof(f3403, plain, ($false | (~ spl18_52 | ~ spl18_116 | spl18_212)), inference(subsumption_resolution, [], [f3402, f955])).
fof(f955, plain, ((e23 = op2(e20, e23)) | ~ spl18_116), inference(avatar_component_clause, [], [f953])).
fof(f3402, plain, (~ (e23 = op2(e20, e23)) | (~ spl18_52 | spl18_212)), inference(forward_demodulation, [], [f3401, f364])).
fof(f364, plain, (e23 = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f3401, plain, (~ (op2(e20, e23) = h4(e13)) | (~ spl18_52 | spl18_212)), inference(forward_demodulation, [], [f1540, f651])).
fof(f651, plain, ((e13 = op1(e10, e13)) | ~ spl18_52), inference(avatar_component_clause, [], [f649])).
fof(f649, plain, (spl18_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_52])])).
fof(f1540, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | spl18_212), inference(avatar_component_clause, [], [f1538])).
fof(f1538, plain, (spl18_212 <=> (op2(e20, e23) = h4(op1(e10, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_212])])).
fof(f3384, plain, (~ spl18_35 | ~ spl18_99 | ~ spl18_168 | ~ spl18_172 | spl18_209), inference(avatar_contradiction_clause, [], [f3383])).
fof(f3383, plain, ($false | (~ spl18_35 | ~ spl18_99 | ~ spl18_168 | ~ spl18_172 | spl18_209)), inference(subsumption_resolution, [], [f3382, f883])).
fof(f883, plain, ((e22 = op2(e21, e23)) | ~ spl18_99), inference(avatar_component_clause, [], [f881])).
fof(f881, plain, (spl18_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_99])])).
fof(f3382, plain, (~ (e22 = op2(e21, e23)) | (~ spl18_35 | ~ spl18_168 | ~ spl18_172 | spl18_209)), inference(forward_demodulation, [], [f3381, f1305])).
fof(f3381, plain, (~ (op2(e21, e23) = h4(e12)) | (~ spl18_35 | ~ spl18_172 | spl18_209)), inference(forward_demodulation, [], [f3380, f579])).
fof(f579, plain, ((e12 = op1(e11, e13)) | ~ spl18_35), inference(avatar_component_clause, [], [f577])).
fof(f577, plain, (spl18_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_35])])).
fof(f3380, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | (~ spl18_172 | spl18_209)), inference(forward_demodulation, [], [f1528, f1326])).
fof(f1528, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | spl18_209), inference(avatar_component_clause, [], [f1526])).
fof(f1526, plain, (spl18_209 <=> (h4(op1(e11, e13)) = op2(h4(e11), e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_209])])).
fof(f3357, plain, (~ spl18_41 | spl18_210), inference(avatar_contradiction_clause, [], [f3356])).
fof(f3356, plain, ($false | (~ spl18_41 | spl18_210)), inference(subsumption_resolution, [], [f3355, f1297])).
fof(f3355, plain, (~ (e20 = h4(e10)) | (~ spl18_41 | spl18_210)), inference(forward_demodulation, [], [f1532, f605])).
fof(f605, plain, ((e10 = op1(e11, e11)) | ~ spl18_41), inference(avatar_component_clause, [], [f603])).
fof(f603, plain, (spl18_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_41])])).
fof(f1532, plain, (~ (e20 = h4(op1(e11, e11))) | spl18_210), inference(avatar_component_clause, [], [f1530])).
fof(f1530, plain, (spl18_210 <=> (e20 = h4(op1(e11, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_210])])).
fof(f3326, plain, (~ spl18_15 | spl18_206), inference(avatar_contradiction_clause, [], [f3325])).
fof(f3325, plain, ($false | (~ spl18_15 | spl18_206)), inference(trivial_inequality_removal, [], [f3324])).
fof(f3324, plain, (~ (h4(e12) = h4(e12)) | (~ spl18_15 | spl18_206)), inference(forward_demodulation, [], [f1516, f494])).
fof(f494, plain, ((e12 = op1(e13, e10)) | ~ spl18_15), inference(avatar_component_clause, [], [f492])).
fof(f492, plain, (spl18_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_15])])).
fof(f1516, plain, (~ (h4(e12) = h4(op1(e13, e10))) | spl18_206), inference(avatar_component_clause, [], [f1514])).
fof(f1514, plain, (spl18_206 <=> (h4(e12) = h4(op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_206])])).
fof(f3304, plain, (~ spl18_32 | ~ spl18_96 | ~ spl18_168 | spl18_208), inference(avatar_contradiction_clause, [], [f3303])).
fof(f3303, plain, ($false | (~ spl18_32 | ~ spl18_96 | ~ spl18_168 | spl18_208)), inference(subsumption_resolution, [], [f3302, f870])).
fof(f870, plain, ((e23 = op2(e22, e20)) | ~ spl18_96), inference(avatar_component_clause, [], [f868])).
fof(f868, plain, (spl18_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_96])])).
fof(f3302, plain, (~ (e23 = op2(e22, e20)) | (~ spl18_32 | ~ spl18_168 | spl18_208)), inference(forward_demodulation, [], [f3301, f364])).
fof(f3301, plain, (~ (op2(e22, e20) = h4(e13)) | (~ spl18_32 | ~ spl18_168 | spl18_208)), inference(forward_demodulation, [], [f3300, f566])).
fof(f566, plain, ((e13 = op1(e12, e10)) | ~ spl18_32), inference(avatar_component_clause, [], [f564])).
fof(f564, plain, (spl18_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_32])])).
fof(f3300, plain, (~ (op2(e22, e20) = h4(op1(e12, e10))) | (~ spl18_168 | spl18_208)), inference(forward_demodulation, [], [f1524, f1305])).
fof(f1524, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | spl18_208), inference(avatar_component_clause, [], [f1522])).
fof(f1522, plain, (spl18_208 <=> (h4(op1(e12, e10)) = op2(h4(e12), e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_208])])).
fof(f3287, plain, (~ spl18_5 | ~ spl18_69 | ~ spl18_168 | spl18_204), inference(avatar_contradiction_clause, [], [f3286])).
fof(f3286, plain, ($false | (~ spl18_5 | ~ spl18_69 | ~ spl18_168 | spl18_204)), inference(subsumption_resolution, [], [f3285, f756])).
fof(f756, plain, ((e20 = op2(e23, e22)) | ~ spl18_69), inference(avatar_component_clause, [], [f754])).
fof(f754, plain, (spl18_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_69])])).
fof(f3285, plain, (~ (e20 = op2(e23, e22)) | (~ spl18_5 | ~ spl18_168 | spl18_204)), inference(forward_demodulation, [], [f3284, f1297])).
fof(f3284, plain, (~ (op2(e23, e22) = h4(e10)) | (~ spl18_5 | ~ spl18_168 | spl18_204)), inference(forward_demodulation, [], [f3283, f452])).
fof(f452, plain, ((e10 = op1(e13, e12)) | ~ spl18_5), inference(avatar_component_clause, [], [f450])).
fof(f450, plain, (spl18_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_5])])).
fof(f3283, plain, (~ (op2(e23, e22) = h4(op1(e13, e12))) | (~ spl18_168 | spl18_204)), inference(forward_demodulation, [], [f1508, f1305])).
fof(f1508, plain, (~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | spl18_204), inference(avatar_component_clause, [], [f1506])).
fof(f1506, plain, (spl18_204 <=> (h4(op1(e13, e12)) = op2(e23, h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_204])])).
fof(f3264, plain, (~ spl18_2 | spl18_203), inference(avatar_contradiction_clause, [], [f3263])).
fof(f3263, plain, ($false | (~ spl18_2 | spl18_203)), inference(trivial_inequality_removal, [], [f3262])).
fof(f3262, plain, (~ (h4(e11) = h4(e11)) | (~ spl18_2 | spl18_203)), inference(forward_demodulation, [], [f1504, f439])).
fof(f439, plain, ((e11 = op1(e13, e13)) | ~ spl18_2), inference(avatar_component_clause, [], [f437])).
fof(f437, plain, (spl18_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_2])])).
fof(f1504, plain, (~ (h4(e11) = h4(op1(e13, e13))) | spl18_203), inference(avatar_component_clause, [], [f1502])).
fof(f1502, plain, (spl18_203 <=> (h4(e11) = h4(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl18_203])])).
fof(f3232, plain, (~ spl18_20 | ~ spl18_32), inference(avatar_split_clause, [], [f3231, f564, f513])).
fof(f513, plain, (spl18_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_20])])).
fof(f3231, plain, (~ (e13 = op1(e12, e13)) | ~ spl18_32), inference(backward_demodulation, [], [f194, f566])).
fof(f194, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax5)).
fof(f3224, plain, (~ spl18_49 | ~ spl18_61), inference(avatar_split_clause, [], [f3216, f688, f637])).
fof(f637, plain, (spl18_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_49])])).
fof(f3216, plain, (~ (e10 = op1(e10, e13)) | ~ spl18_61), inference(backward_demodulation, [], [f182, f690])).
fof(f182, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f3211, plain, (~ spl18_29 | ~ spl18_5 | spl18_145), inference(avatar_split_clause, [], [f3210, f1131, f450, f552])).
fof(f552, plain, (spl18_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_29])])).
fof(f1131, plain, (spl18_145 <=> (op1(e13, e12) = op1(e12, op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_145])])).
fof(f3210, plain, (~ (e10 = op1(e12, e10)) | (~ spl18_5 | spl18_145)), inference(forward_demodulation, [], [f1133, f452])).
fof(f1133, plain, (~ (op1(e13, e12) = op1(e12, op1(e13, e12))) | spl18_145), inference(avatar_component_clause, [], [f1131])).
fof(f3200, plain, (~ spl18_12 | ~ spl18_76 | ~ spl18_172 | spl18_205), inference(avatar_contradiction_clause, [], [f3199])).
fof(f3199, plain, ($false | (~ spl18_12 | ~ spl18_76 | ~ spl18_172 | spl18_205)), inference(subsumption_resolution, [], [f3198, f785])).
fof(f3198, plain, (~ (e23 = op2(e23, e21)) | (~ spl18_12 | ~ spl18_172 | spl18_205)), inference(forward_demodulation, [], [f3197, f364])).
fof(f3197, plain, (~ (op2(e23, e21) = h4(e13)) | (~ spl18_12 | ~ spl18_172 | spl18_205)), inference(forward_demodulation, [], [f3196, f481])).
fof(f481, plain, ((e13 = op1(e13, e11)) | ~ spl18_12), inference(avatar_component_clause, [], [f479])).
fof(f479, plain, (spl18_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_12])])).
fof(f3196, plain, (~ (op2(e23, e21) = h4(op1(e13, e11))) | (~ spl18_172 | spl18_205)), inference(forward_demodulation, [], [f1512, f1326])).
fof(f1512, plain, (~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | spl18_205), inference(avatar_component_clause, [], [f1510])).
fof(f1510, plain, (spl18_205 <=> (h4(op1(e13, e11)) = op2(e23, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_205])])).
fof(f3159, plain, (~ spl18_23 | spl18_131), inference(avatar_contradiction_clause, [], [f3158])).
fof(f3158, plain, ($false | (~ spl18_23 | spl18_131)), inference(subsumption_resolution, [], [f3157, f528])).
fof(f3157, plain, (~ (e12 = op1(e12, e12)) | (~ spl18_23 | spl18_131)), inference(backward_demodulation, [], [f1052, f528])).
fof(f1052, plain, (~ (op1(e12, e12) = op1(e12, op1(e12, e12))) | spl18_131), inference(avatar_component_clause, [], [f1050])).
fof(f1050, plain, (spl18_131 <=> (op1(e12, e12) = op1(e12, op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_131])])).
fof(f3151, plain, (~ spl18_26 | ~ spl18_90 | ~ spl18_168 | ~ spl18_172 | spl18_201), inference(avatar_contradiction_clause, [], [f3150])).
fof(f3150, plain, ($false | (~ spl18_26 | ~ spl18_90 | ~ spl18_168 | ~ spl18_172 | spl18_201)), inference(subsumption_resolution, [], [f3146, f1326])).
fof(f3146, plain, (~ (e21 = h4(e11)) | (~ spl18_26 | ~ spl18_90 | ~ spl18_168 | ~ spl18_172 | spl18_201)), inference(backward_demodulation, [], [f3039, f541])).
fof(f541, plain, ((e11 = op1(e12, e11)) | ~ spl18_26), inference(avatar_component_clause, [], [f539])).
fof(f539, plain, (spl18_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_26])])).
fof(f3039, plain, (~ (e21 = h4(op1(e12, e11))) | (~ spl18_90 | ~ spl18_168 | ~ spl18_172 | spl18_201)), inference(forward_demodulation, [], [f3038, f845])).
fof(f845, plain, ((e21 = op2(e22, e21)) | ~ spl18_90), inference(avatar_component_clause, [], [f843])).
fof(f843, plain, (spl18_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_90])])).
fof(f3038, plain, (~ (op2(e22, e21) = h4(op1(e12, e11))) | (~ spl18_168 | ~ spl18_172 | spl18_201)), inference(forward_demodulation, [], [f3037, f1305])).
fof(f3037, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), e21)) | (~ spl18_172 | spl18_201)), inference(forward_demodulation, [], [f1496, f1326])).
fof(f1496, plain, (~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | spl18_201), inference(avatar_component_clause, [], [f1494])).
fof(f1494, plain, (spl18_201 <=> (h4(op1(e12, e11)) = op2(h4(e12), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_201])])).
fof(f3128, plain, (~ spl18_20 | ~ spl18_40 | spl18_136), inference(avatar_split_clause, [], [f3125, f1074, f598, f513])).
fof(f598, plain, (spl18_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_40])])).
fof(f1074, plain, (spl18_136 <=> (op1(e11, e12) = op1(e12, op1(e11, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_136])])).
fof(f3125, plain, (~ (e13 = op1(e12, e13)) | (~ spl18_40 | spl18_136)), inference(backward_demodulation, [], [f1076, f600])).
fof(f600, plain, ((e13 = op1(e11, e12)) | ~ spl18_40), inference(avatar_component_clause, [], [f598])).
fof(f1076, plain, (~ (op1(e11, e12) = op1(e12, op1(e11, e12))) | spl18_136), inference(avatar_component_clause, [], [f1074])).
fof(f3115, plain, (~ spl18_26 | ~ spl18_54 | spl18_141), inference(avatar_split_clause, [], [f3113, f1098, f658, f539])).
fof(f1098, plain, (spl18_141 <=> (op1(e10, e12) = op1(e12, op1(e10, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_141])])).
fof(f3113, plain, (~ (e11 = op1(e12, e11)) | (~ spl18_54 | spl18_141)), inference(backward_demodulation, [], [f1100, f660])).
fof(f1100, plain, (~ (op1(e10, e12) = op1(e12, op1(e10, e12))) | spl18_141), inference(avatar_component_clause, [], [f1098])).
fof(f3103, plain, (~ spl18_27 | ~ spl18_59), inference(avatar_split_clause, [], [f3097, f679, f543])).
fof(f543, plain, (spl18_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_27])])).
fof(f3097, plain, (~ (e12 = op1(e12, e11)) | ~ spl18_59), inference(backward_demodulation, [], [f163, f681])).
fof(f163, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3082, plain, (~ spl18_84 | ~ spl18_96), inference(avatar_split_clause, [], [f3079, f868, f817])).
fof(f817, plain, (spl18_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_84])])).
fof(f3079, plain, (~ (e23 = op2(e22, e23)) | ~ spl18_96), inference(backward_demodulation, [], [f242, f870])).
fof(f242, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3081, plain, (~ spl18_96 | ~ spl18_116 | spl18_152), inference(avatar_contradiction_clause, [], [f3080])).
fof(f3080, plain, ($false | (~ spl18_96 | ~ spl18_116 | spl18_152)), inference(subsumption_resolution, [], [f3078, f955])).
fof(f3078, plain, (~ (e23 = op2(e20, e23)) | (~ spl18_96 | spl18_152)), inference(backward_demodulation, [], [f1167, f870])).
fof(f1167, plain, (~ (op2(e22, e20) = op2(e20, op2(e22, e20))) | spl18_152), inference(avatar_component_clause, [], [f1165])).
fof(f1165, plain, (spl18_152 <=> (op2(e22, e20) = op2(e20, op2(e22, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_152])])).
fof(f3066, plain, (~ spl18_125 | spl18_162), inference(avatar_contradiction_clause, [], [f3065])).
fof(f3065, plain, ($false | (~ spl18_125 | spl18_162)), inference(subsumption_resolution, [], [f3059, f994])).
fof(f3059, plain, (~ (e20 = op2(e20, e20)) | (~ spl18_125 | spl18_162)), inference(backward_demodulation, [], [f3018, f3051])).
fof(f3018, plain, (~ (h1(e11) = op2(e20, h1(e11))) | spl18_162), inference(backward_demodulation, [], [f1215, f354])).
fof(f1215, plain, (~ (op2(e20, e20) = op2(e20, op2(e20, e20))) | spl18_162), inference(avatar_component_clause, [], [f1213])).
fof(f1213, plain, (spl18_162 <=> (op2(e20, e20) = op2(e20, op2(e20, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_162])])).
fof(f3064, plain, (~ spl18_113 | ~ spl18_125), inference(avatar_split_clause, [], [f3055, f992, f941])).
fof(f941, plain, (spl18_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_113])])).
fof(f3055, plain, (~ (e20 = op2(e20, e23)) | ~ spl18_125), inference(backward_demodulation, [], [f1261, f3051])).
fof(f1261, plain, ~ (op2(e20, e23) = h1(e11)), inference(backward_demodulation, [], [f230, f354])).
fof(f230, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3063, plain, (~ spl18_93 | ~ spl18_125), inference(avatar_split_clause, [], [f3054, f992, f856])).
fof(f856, plain, (spl18_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_93])])).
fof(f3054, plain, (~ (e20 = op2(e22, e20)) | ~ spl18_125), inference(backward_demodulation, [], [f1257, f3051])).
fof(f1257, plain, ~ (op2(e22, e20) = h1(e11)), inference(backward_demodulation, [], [f205, f354])).
fof(f205, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f3047, plain, (~ spl18_36 | ~ spl18_12 | spl18_146), inference(avatar_split_clause, [], [f3046, f1136, f479, f581])).
fof(f581, plain, (spl18_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_36])])).
fof(f1136, plain, (spl18_146 <=> (op1(e13, e11) = op1(e11, op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_146])])).
fof(f3046, plain, (~ (e13 = op1(e11, e13)) | (~ spl18_12 | spl18_146)), inference(forward_demodulation, [], [f1138, f481])).
fof(f1138, plain, (~ (op1(e13, e11) = op1(e11, op1(e13, e11))) | spl18_146), inference(avatar_component_clause, [], [f1136])).
fof(f3045, plain, (~ spl18_84 | ~ spl18_104 | spl18_155), inference(avatar_split_clause, [], [f3044, f1179, f902, f817])).
fof(f902, plain, (spl18_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_104])])).
fof(f1179, plain, (spl18_155 <=> (op2(e21, e22) = op2(e22, op2(e21, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_155])])).
fof(f3044, plain, (~ (e23 = op2(e22, e23)) | (~ spl18_104 | spl18_155)), inference(forward_demodulation, [], [f1181, f904])).
fof(f904, plain, ((e23 = op2(e21, e22)) | ~ spl18_104), inference(avatar_component_clause, [], [f902])).
fof(f1181, plain, (~ (op2(e21, e22) = op2(e22, op2(e21, e22))) | spl18_155), inference(avatar_component_clause, [], [f1179])).
fof(f3015, plain, (~ spl18_95 | ~ spl18_168), inference(avatar_split_clause, [], [f2794, f1304, f864])).
fof(f864, plain, (spl18_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_95])])).
fof(f2794, plain, (~ (e22 = op2(e22, e20)) | ~ spl18_168), inference(forward_demodulation, [], [f1285, f1305])).
fof(f1285, plain, ~ (op2(e22, e20) = h4(e12)), inference(backward_demodulation, [], [f209, f1283])).
fof(f1283, plain, (op2(e23, e20) = h4(e12)), inference(forward_demodulation, [], [f367, f349])).
fof(f367, plain, (op2(e23, op2(op2(e23, e23), op2(e23, e23))) = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f209, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f2994, plain, (~ spl18_35 | ~ spl18_39), inference(avatar_split_clause, [], [f2989, f594, f577])).
fof(f594, plain, (spl18_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_39])])).
fof(f2989, plain, (~ (e12 = op1(e11, e13)) | ~ spl18_39), inference(backward_demodulation, [], [f191, f596])).
fof(f596, plain, ((e12 = op1(e11, e12)) | ~ spl18_39), inference(avatar_component_clause, [], [f594])).
fof(f191, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2986, plain, (~ spl18_35 | ~ spl18_51), inference(avatar_split_clause, [], [f2984, f645, f577])).
fof(f645, plain, (spl18_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_51])])).
fof(f2984, plain, (~ (e12 = op1(e11, e13)) | ~ spl18_51), inference(backward_demodulation, [], [f174, f647])).
fof(f647, plain, ((e12 = op1(e10, e13)) | ~ spl18_51), inference(avatar_component_clause, [], [f645])).
fof(f174, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2965, plain, (~ spl18_61 | spl18_143), inference(avatar_contradiction_clause, [], [f2964])).
fof(f2964, plain, ($false | (~ spl18_61 | spl18_143)), inference(subsumption_resolution, [], [f2963, f690])).
fof(f2963, plain, (~ (e10 = op1(e10, e10)) | (~ spl18_61 | spl18_143)), inference(forward_demodulation, [], [f1110, f690])).
fof(f1110, plain, (~ (op1(e10, e10) = op1(e10, op1(e10, e10))) | spl18_143), inference(avatar_component_clause, [], [f1108])).
fof(f1108, plain, (spl18_143 <=> (op1(e10, e10) = op1(e10, op1(e10, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_143])])).
fof(f2960, plain, (~ spl18_55 | ~ spl18_15 | spl18_147), inference(avatar_split_clause, [], [f2959, f1141, f492, f662])).
fof(f662, plain, (spl18_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_55])])).
fof(f1141, plain, (spl18_147 <=> (op1(e13, e10) = op1(e10, op1(e13, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_147])])).
fof(f2959, plain, (~ (e12 = op1(e10, e12)) | (~ spl18_15 | spl18_147)), inference(forward_demodulation, [], [f1143, f494])).
fof(f1143, plain, (~ (op1(e13, e10) = op1(e10, op1(e13, e10))) | spl18_147), inference(avatar_component_clause, [], [f1141])).
fof(f2956, plain, (~ spl18_90 | ~ spl18_118 | spl18_160), inference(avatar_contradiction_clause, [], [f2955])).
fof(f2955, plain, ($false | (~ spl18_90 | ~ spl18_118 | spl18_160)), inference(subsumption_resolution, [], [f2954, f845])).
fof(f2954, plain, (~ (e21 = op2(e22, e21)) | (~ spl18_118 | spl18_160)), inference(forward_demodulation, [], [f1205, f964])).
fof(f1205, plain, (~ (op2(e20, e22) = op2(e22, op2(e20, e22))) | spl18_160), inference(avatar_component_clause, [], [f1203])).
fof(f1203, plain, (spl18_160 <=> (op2(e20, e22) = op2(e22, op2(e20, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_160])])).
fof(f2937, plain, (~ spl18_9 | ~ spl18_12), inference(avatar_contradiction_clause, [], [f2936])).
fof(f2936, plain, ($false | (~ spl18_9 | ~ spl18_12)), inference(subsumption_resolution, [], [f2935, f254])).
fof(f254, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax7)).
fof(f2935, plain, ((e10 = e13) | (~ spl18_9 | ~ spl18_12)), inference(backward_demodulation, [], [f481, f469])).
fof(f469, plain, ((e10 = op1(e13, e11)) | ~ spl18_9), inference(avatar_component_clause, [], [f467])).
fof(f467, plain, (spl18_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_9])])).
fof(f2932, plain, (~ spl18_17 | ~ spl18_19), inference(avatar_contradiction_clause, [], [f2931])).
fof(f2931, plain, ($false | (~ spl18_17 | ~ spl18_19)), inference(subsumption_resolution, [], [f2930, f253])).
fof(f253, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f2930, plain, ((e10 = e12) | (~ spl18_17 | ~ spl18_19)), inference(forward_demodulation, [], [f511, f503])).
fof(f511, plain, ((e12 = op1(e12, e13)) | ~ spl18_19), inference(avatar_component_clause, [], [f509])).
fof(f509, plain, (spl18_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_19])])).
fof(f2918, plain, (~ spl18_46 | ~ spl18_48), inference(avatar_contradiction_clause, [], [f2917])).
fof(f2917, plain, ($false | (~ spl18_46 | ~ spl18_48)), inference(subsumption_resolution, [], [f2916, f256])).
fof(f256, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f2916, plain, ((e11 = e13) | (~ spl18_46 | ~ spl18_48)), inference(forward_demodulation, [], [f634, f626])).
fof(f634, plain, ((e13 = op1(e11, e10)) | ~ spl18_48), inference(avatar_component_clause, [], [f632])).
fof(f632, plain, (spl18_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_48])])).
fof(f2892, plain, (~ spl18_46 | ~ spl18_58 | spl18_138), inference(avatar_contradiction_clause, [], [f2891])).
fof(f2891, plain, ($false | (~ spl18_46 | ~ spl18_58 | spl18_138)), inference(subsumption_resolution, [], [f2890, f677])).
fof(f677, plain, ((e11 = op1(e10, e11)) | ~ spl18_58), inference(avatar_component_clause, [], [f675])).
fof(f675, plain, (spl18_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_58])])).
fof(f2890, plain, (~ (e11 = op1(e10, e11)) | (~ spl18_46 | spl18_138)), inference(forward_demodulation, [], [f1086, f626])).
fof(f1086, plain, (~ (op1(e11, e10) = op1(e10, op1(e11, e10))) | spl18_138), inference(avatar_component_clause, [], [f1084])).
fof(f1084, plain, (spl18_138 <=> (op1(e11, e10) = op1(e10, op1(e11, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_138])])).
fof(f2871, plain, (~ spl18_103 | ~ spl18_87), inference(avatar_split_clause, [], [f2797, f830, f898])).
fof(f2797, plain, (~ (e22 = op2(e21, e22)) | ~ spl18_87), inference(forward_demodulation, [], [f1275, f2757])).
fof(f2757, plain, ((e22 = h3(e11)) | ~ spl18_87), inference(backward_demodulation, [], [f362, f832])).
fof(f832, plain, ((e22 = op2(e22, e22)) | ~ spl18_87), inference(avatar_component_clause, [], [f830])).
fof(f362, plain, (op2(e22, e22) = h3(e11)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((h3(e12) = op2(e22, op2(op2(e22, e22), op2(e22, e22)))) & (op2(e22, e22) = h3(e11)) & (h3(e10) = op2(op2(e22, e22), op2(e22, e22))) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax16)).
fof(f1275, plain, ~ (op2(e21, e22) = h3(e11)), inference(backward_demodulation, [], [f219, f362])).
fof(f219, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2866, plain, (~ spl18_97 | ~ spl18_190), inference(avatar_split_clause, [], [f2865, f1420, f873])).
fof(f873, plain, (spl18_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_97])])).
fof(f1420, plain, (spl18_190 <=> (e20 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_190])])).
fof(f2865, plain, (~ (e20 = op2(e21, e23)) | ~ spl18_190), inference(forward_demodulation, [], [f1270, f1421])).
fof(f1421, plain, ((e20 = h2(e11)) | ~ spl18_190), inference(avatar_component_clause, [], [f1420])).
fof(f1270, plain, ~ (op2(e21, e23) = h2(e11)), inference(backward_demodulation, [], [f238, f358])).
fof(f358, plain, (op2(e21, e21) = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((h2(e12) = op2(e21, op2(op2(e21, e21), op2(e21, e21)))) & (op2(e21, e21) = h2(e11)) & (h2(e10) = op2(op2(e21, e21), op2(e21, e21))) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax15)).
fof(f238, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f2855, plain, (~ spl18_7 | ~ spl18_15), inference(avatar_split_clause, [], [f2450, f492, f458])).
fof(f458, plain, (spl18_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_7])])).
fof(f2450, plain, (~ (e12 = op1(e13, e12)) | ~ spl18_15), inference(backward_demodulation, [], [f199, f494])).
fof(f199, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2825, plain, (~ spl18_87 | spl18_150), inference(avatar_contradiction_clause, [], [f2824])).
fof(f2824, plain, ($false | (~ spl18_87 | spl18_150)), inference(subsumption_resolution, [], [f2823, f832])).
fof(f2823, plain, (~ (e22 = op2(e22, e22)) | (~ spl18_87 | spl18_150)), inference(forward_demodulation, [], [f1157, f832])).
fof(f1157, plain, (~ (op2(e22, e22) = op2(e22, op2(e22, e22))) | spl18_150), inference(avatar_component_clause, [], [f1155])).
fof(f1155, plain, (spl18_150 <=> (op2(e22, e22) = op2(e22, op2(e22, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_150])])).
fof(f2803, plain, (spl18_273 | ~ spl18_128), inference(avatar_split_clause, [], [f2740, f1004, f1923])).
fof(f1923, plain, (spl18_273 <=> (e23 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_273])])).
fof(f1004, plain, (spl18_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl18_128])])).
fof(f2740, plain, ((e23 = h1(e11)) | ~ spl18_128), inference(backward_demodulation, [], [f354, f1006])).
fof(f1006, plain, ((op2(e20, e20) = e23) | ~ spl18_128), inference(avatar_component_clause, [], [f1004])).
fof(f2800, plain, (~ spl18_102 | ~ spl18_110), inference(avatar_split_clause, [], [f2799, f928, f894])).
fof(f894, plain, (spl18_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_102])])).
fof(f2799, plain, (~ (e21 = op2(e21, e22)) | ~ spl18_110), inference(forward_demodulation, [], [f235, f930])).
fof(f235, plain, ~ (op2(e21, e20) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2783, plain, (~ spl18_52 | ~ spl18_32 | spl18_133), inference(avatar_split_clause, [], [f2733, f1060, f564, f649])).
fof(f1060, plain, (spl18_133 <=> (op1(e12, e10) = op1(e10, op1(e12, e10)))), introduced(avatar_definition, [new_symbols(naming, [spl18_133])])).
fof(f2733, plain, (~ (e13 = op1(e10, e13)) | (~ spl18_32 | spl18_133)), inference(forward_demodulation, [], [f1062, f566])).
fof(f1062, plain, (~ (op1(e12, e10) = op1(e10, op1(e12, e10))) | spl18_133), inference(avatar_component_clause, [], [f1060])).
fof(f2781, plain, (~ spl18_39 | ~ spl18_27 | spl18_132), inference(avatar_split_clause, [], [f2735, f1055, f543, f594])).
fof(f1055, plain, (spl18_132 <=> (op1(e12, e11) = op1(e11, op1(e12, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl18_132])])).
fof(f2735, plain, (~ (e12 = op1(e11, e12)) | (~ spl18_27 | spl18_132)), inference(forward_demodulation, [], [f1057, f545])).
fof(f545, plain, ((e12 = op1(e12, e11)) | ~ spl18_27), inference(avatar_component_clause, [], [f543])).
fof(f1057, plain, (~ (op1(e12, e11) = op1(e11, op1(e12, e11))) | spl18_132), inference(avatar_component_clause, [], [f1055])).
fof(f2777, plain, (~ spl18_69 | ~ spl18_93 | spl18_164), inference(avatar_contradiction_clause, [], [f2776])).
fof(f2776, plain, ($false | (~ spl18_69 | ~ spl18_93 | spl18_164)), inference(subsumption_resolution, [], [f2773, f858])).
fof(f858, plain, ((e20 = op2(e22, e20)) | ~ spl18_93), inference(avatar_component_clause, [], [f856])).
fof(f2773, plain, (~ (e20 = op2(e22, e20)) | (~ spl18_69 | spl18_164)), inference(backward_demodulation, [], [f1238, f756])).
fof(f1238, plain, (~ (op2(e23, e22) = op2(e22, op2(e23, e22))) | spl18_164), inference(avatar_component_clause, [], [f1236])).
fof(f1236, plain, (spl18_164 <=> (op2(e23, e22) = op2(e22, op2(e23, e22)))), introduced(avatar_definition, [new_symbols(naming, [spl18_164])])).
fof(f2763, plain, (~ spl18_83 | ~ spl18_87), inference(avatar_split_clause, [], [f2759, f830, f813])).
fof(f813, plain, (spl18_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_83])])).
fof(f2759, plain, (~ (e22 = op2(e22, e23)) | ~ spl18_87), inference(backward_demodulation, [], [f1279, f2757])).
fof(f1279, plain, ~ (op2(e22, e23) = h3(e11)), inference(backward_demodulation, [], [f245, f362])).
fof(f245, plain, ~ (op2(e22, e22) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2754, plain, (~ spl18_93 | ~ spl18_96), inference(avatar_contradiction_clause, [], [f2753])).
fof(f2753, plain, ($false | (~ spl18_93 | ~ spl18_96)), inference(subsumption_resolution, [], [f2752, f260])).
fof(f260, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax8)).
fof(f2752, plain, ((e20 = e23) | (~ spl18_93 | ~ spl18_96)), inference(backward_demodulation, [], [f870, f858])).
fof(f2699, plain, (~ spl18_23 | ~ spl18_27), inference(avatar_split_clause, [], [f2698, f543, f526])).
fof(f2698, plain, (~ (e12 = op1(e12, e12)) | ~ spl18_27), inference(backward_demodulation, [], [f195, f545])).
fof(f195, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2693, plain, (~ spl18_23 | ~ spl18_39), inference(avatar_split_clause, [], [f2690, f594, f526])).
fof(f2690, plain, (~ (e12 = op1(e12, e12)) | ~ spl18_39), inference(backward_demodulation, [], [f171, f596])).
fof(f171, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f2665, plain, (~ spl18_66 | ~ spl18_68), inference(avatar_contradiction_clause, [], [f2664])).
fof(f2664, plain, ($false | (~ spl18_66 | ~ spl18_68)), inference(subsumption_resolution, [], [f2663, f262])).
fof(f262, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f2663, plain, ((e21 = e23) | (~ spl18_66 | ~ spl18_68)), inference(forward_demodulation, [], [f751, f743])).
fof(f743, plain, ((e21 = op2(e23, e23)) | ~ spl18_66), inference(avatar_component_clause, [], [f741])).
fof(f741, plain, (spl18_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_66])])).
fof(f751, plain, ((e23 = op2(e23, e23)) | ~ spl18_68), inference(avatar_component_clause, [], [f749])).
fof(f749, plain, (spl18_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_68])])).
fof(f2659, plain, (~ spl18_77 | ~ spl18_79), inference(avatar_contradiction_clause, [], [f2658])).
fof(f2658, plain, ($false | (~ spl18_77 | ~ spl18_79)), inference(subsumption_resolution, [], [f2657, f259])).
fof(f259, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f2657, plain, ((e20 = e22) | (~ spl18_77 | ~ spl18_79)), inference(backward_demodulation, [], [f798, f790])).
fof(f790, plain, ((e20 = op2(e23, e20)) | ~ spl18_77), inference(avatar_component_clause, [], [f788])).
fof(f788, plain, (spl18_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_77])])).
fof(f798, plain, ((e22 = op2(e23, e20)) | ~ spl18_79), inference(avatar_component_clause, [], [f796])).
fof(f796, plain, (spl18_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_79])])).
fof(f2655, plain, (~ spl18_81 | ~ spl18_83), inference(avatar_contradiction_clause, [], [f2654])).
fof(f2654, plain, ($false | (~ spl18_81 | ~ spl18_83)), inference(subsumption_resolution, [], [f2653, f259])).
fof(f2653, plain, ((e20 = e22) | (~ spl18_81 | ~ spl18_83)), inference(forward_demodulation, [], [f815, f807])).
fof(f815, plain, ((e22 = op2(e22, e23)) | ~ spl18_83), inference(avatar_component_clause, [], [f813])).
fof(f2643, plain, (~ spl18_178 | ~ spl18_118), inference(avatar_split_clause, [], [f2641, f962, f1359])).
fof(f1359, plain, (spl18_178 <=> (e21 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_178])])).
fof(f2641, plain, (~ (e21 = h3(e11)) | ~ spl18_118), inference(backward_demodulation, [], [f1274, f964])).
fof(f1274, plain, ~ (op2(e20, e22) = h3(e11)), inference(backward_demodulation, [], [f217, f362])).
fof(f217, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2618, plain, (~ spl18_273 | ~ spl18_96), inference(avatar_split_clause, [], [f2617, f868, f1923])).
fof(f2617, plain, (~ (e23 = h1(e11)) | ~ spl18_96), inference(forward_demodulation, [], [f1257, f870])).
fof(f2612, plain, (~ spl18_123 | ~ spl18_91), inference(avatar_split_clause, [], [f2611, f847, f983])).
fof(f2611, plain, (~ (e22 = op2(e20, e21)) | ~ spl18_91), inference(forward_demodulation, [], [f211, f849])).
fof(f211, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2609, plain, (~ spl18_120 | ~ spl18_104), inference(avatar_split_clause, [], [f2494, f902, f970])).
fof(f2494, plain, (~ (e23 = op2(e20, e22)) | ~ spl18_104), inference(forward_demodulation, [], [f216, f904])).
fof(f216, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f2606, plain, (~ spl18_119 | spl18_166 | ~ spl18_168), inference(avatar_split_clause, [], [f2520, f1304, f1246, f966])).
fof(f966, plain, (spl18_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_119])])).
fof(f1246, plain, (spl18_166 <=> (op2(e23, e20) = op2(e20, op2(e23, e20)))), introduced(avatar_definition, [new_symbols(naming, [spl18_166])])).
fof(f2520, plain, (~ (e22 = op2(e20, e22)) | (spl18_166 | ~ spl18_168)), inference(backward_demodulation, [], [f1943, f1305])).
fof(f1943, plain, (~ (h4(e12) = op2(e20, h4(e12))) | spl18_166), inference(forward_demodulation, [], [f1248, f1283])).
fof(f1248, plain, (~ (op2(e23, e20) = op2(e20, op2(e23, e20))) | spl18_166), inference(avatar_component_clause, [], [f1246])).
fof(f2592, plain, (~ spl18_33 | ~ spl18_41), inference(avatar_split_clause, [], [f2591, f603, f569])).
fof(f569, plain, (spl18_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_33])])).
fof(f2591, plain, (~ (e10 = op1(e11, e13)) | ~ spl18_41), inference(forward_demodulation, [], [f190, f605])).
fof(f190, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f2563, plain, (~ spl18_49 | ~ spl18_52), inference(avatar_contradiction_clause, [], [f2562])).
fof(f2562, plain, ($false | (~ spl18_49 | ~ spl18_52)), inference(subsumption_resolution, [], [f2560, f254])).
fof(f2560, plain, ((e10 = e13) | (~ spl18_49 | ~ spl18_52)), inference(backward_demodulation, [], [f651, f639])).
fof(f639, plain, ((e10 = op1(e10, e13)) | ~ spl18_49), inference(avatar_component_clause, [], [f637])).
fof(f2556, plain, (~ spl18_55 | ~ spl18_59), inference(avatar_split_clause, [], [f2554, f679, f662])).
fof(f2554, plain, (~ (e12 = op1(e10, e12)) | ~ spl18_59), inference(backward_demodulation, [], [f183, f681])).
fof(f183, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f2537, plain, (spl18_178 | ~ spl18_86), inference(avatar_split_clause, [], [f2536, f826, f1359])).
fof(f826, plain, (spl18_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_86])])).
fof(f2536, plain, ((e21 = h3(e11)) | ~ spl18_86), inference(backward_demodulation, [], [f362, f828])).
fof(f828, plain, ((e21 = op2(e22, e22)) | ~ spl18_86), inference(avatar_component_clause, [], [f826])).
fof(f2532, plain, (spl18_190 | ~ spl18_105), inference(avatar_split_clause, [], [f2531, f907, f1420])).
fof(f907, plain, (spl18_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_105])])).
fof(f2531, plain, ((e20 = h2(e11)) | ~ spl18_105), inference(backward_demodulation, [], [f358, f909])).
fof(f909, plain, ((e20 = op2(e21, e21)) | ~ spl18_105), inference(avatar_component_clause, [], [f907])).
fof(f2525, plain, (~ spl18_40 | ~ spl18_104 | ~ spl18_168 | ~ spl18_172 | spl18_200), inference(avatar_contradiction_clause, [], [f2524])).
fof(f2524, plain, ($false | (~ spl18_40 | ~ spl18_104 | ~ spl18_168 | ~ spl18_172 | spl18_200)), inference(subsumption_resolution, [], [f2521, f904])).
fof(f2521, plain, (~ (e23 = op2(e21, e22)) | (~ spl18_40 | ~ spl18_168 | ~ spl18_172 | spl18_200)), inference(backward_demodulation, [], [f2426, f1305])).
fof(f2426, plain, (~ (e23 = op2(e21, h4(e12))) | (~ spl18_40 | ~ spl18_172 | spl18_200)), inference(forward_demodulation, [], [f2425, f364])).
fof(f2425, plain, (~ (h4(e13) = op2(e21, h4(e12))) | (~ spl18_40 | ~ spl18_172 | spl18_200)), inference(backward_demodulation, [], [f2362, f600])).
fof(f2362, plain, (~ (h4(op1(e11, e12)) = op2(e21, h4(e12))) | (~ spl18_172 | spl18_200)), inference(backward_demodulation, [], [f1492, f1326])).
fof(f1492, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | spl18_200), inference(avatar_component_clause, [], [f1490])).
fof(f1490, plain, (spl18_200 <=> (h4(op1(e11, e12)) = op2(h4(e11), h4(e12)))), introduced(avatar_definition, [new_symbols(naming, [spl18_200])])).
fof(f2503, plain, (spl18_105 | ~ spl18_172), inference(avatar_split_clause, [], [f2360, f1325, f907])).
fof(f2360, plain, ((e20 = op2(e21, e21)) | ~ spl18_172), inference(backward_demodulation, [], [f1294, f1326])).
fof(f2481, plain, (~ spl18_70 | ~ spl18_172), inference(avatar_split_clause, [], [f2480, f1325, f758])).
fof(f758, plain, (spl18_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_70])])).
fof(f2480, plain, (~ (e21 = op2(e23, e22)) | ~ spl18_172), inference(forward_demodulation, [], [f1293, f1326])).
fof(f1293, plain, ~ (op2(e23, e22) = h4(e11)), inference(backward_demodulation, [], [f251, f366])).
fof(f251, plain, ~ (op2(e23, e22) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2475, plain, (~ spl18_56 | ~ spl18_40), inference(avatar_split_clause, [], [f2474, f598, f666])).
fof(f666, plain, (spl18_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_56])])).
fof(f2474, plain, (~ (e13 = op1(e10, e12)) | ~ spl18_40), inference(forward_demodulation, [], [f168, f600])).
fof(f168, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2453, plain, (~ spl18_18 | ~ spl18_2), inference(avatar_split_clause, [], [f2265, f437, f505])).
fof(f505, plain, (spl18_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_18])])).
fof(f2265, plain, (~ (e11 = op1(e12, e13)) | ~ spl18_2), inference(forward_demodulation, [], [f179, f439])).
fof(f179, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2449, plain, (~ spl18_6 | ~ spl18_2), inference(avatar_split_clause, [], [f2254, f437, f454])).
fof(f454, plain, (spl18_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_6])])).
fof(f2254, plain, (~ (e11 = op1(e13, e12)) | ~ spl18_2), inference(forward_demodulation, [], [f203, f439])).
fof(f203, plain, ~ (op1(e13, e12) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2443, plain, (~ spl18_13 | ~ spl18_15), inference(avatar_contradiction_clause, [], [f2442])).
fof(f2442, plain, ($false | (~ spl18_13 | ~ spl18_15)), inference(subsumption_resolution, [], [f2441, f253])).
fof(f2441, plain, ((e10 = e12) | (~ spl18_13 | ~ spl18_15)), inference(backward_demodulation, [], [f494, f486])).
fof(f486, plain, ((e10 = op1(e13, e10)) | ~ spl18_13), inference(avatar_component_clause, [], [f484])).
fof(f484, plain, (spl18_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_13])])).
fof(f2424, plain, (~ spl18_41 | ~ spl18_42), inference(avatar_contradiction_clause, [], [f2423])).
fof(f2423, plain, ($false | (~ spl18_41 | ~ spl18_42)), inference(subsumption_resolution, [], [f2422, f252])).
fof(f252, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f2422, plain, ((e10 = e11) | (~ spl18_41 | ~ spl18_42)), inference(backward_demodulation, [], [f609, f605])).
fof(f609, plain, ((e11 = op1(e11, e11)) | ~ spl18_42), inference(avatar_component_clause, [], [f607])).
fof(f607, plain, (spl18_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_42])])).
fof(f2421, plain, (~ spl18_38 | ~ spl18_46), inference(avatar_split_clause, [], [f2419, f624, f590])).
fof(f590, plain, (spl18_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_38])])).
fof(f2419, plain, (~ (e11 = op1(e11, e12)) | ~ spl18_46), inference(backward_demodulation, [], [f187, f626])).
fof(f187, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2420, plain, (~ spl18_30 | ~ spl18_46), inference(avatar_split_clause, [], [f2418, f624, f556])).
fof(f556, plain, (spl18_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_30])])).
fof(f2418, plain, (~ (e11 = op1(e12, e10)) | ~ spl18_46), inference(backward_demodulation, [], [f159, f626])).
fof(f159, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2398, plain, (~ spl18_73 | ~ spl18_76), inference(avatar_contradiction_clause, [], [f2397])).
fof(f2397, plain, ($false | (~ spl18_73 | ~ spl18_76)), inference(subsumption_resolution, [], [f2396, f260])).
fof(f2396, plain, ((e20 = e23) | (~ spl18_73 | ~ spl18_76)), inference(backward_demodulation, [], [f785, f773])).
fof(f773, plain, ((e20 = op2(e23, e21)) | ~ spl18_73), inference(avatar_component_clause, [], [f771])).
fof(f771, plain, (spl18_73 <=> (e20 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_73])])).
fof(f2379, plain, (~ spl18_94 | ~ spl18_110), inference(avatar_split_clause, [], [f2377, f928, f860])).
fof(f860, plain, (spl18_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_94])])).
fof(f2377, plain, (~ (e21 = op2(e22, e20)) | ~ spl18_110), inference(backward_demodulation, [], [f207, f930])).
fof(f207, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f2357, plain, (~ spl18_124 | ~ spl18_76), inference(avatar_split_clause, [], [f2356, f783, f987])).
fof(f987, plain, (spl18_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_124])])).
fof(f2356, plain, (~ (e23 = op2(e20, e21)) | ~ spl18_76), inference(forward_demodulation, [], [f212, f785])).
fof(f212, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2342, plain, (~ spl18_110 | ~ spl18_106), inference(avatar_split_clause, [], [f2341, f911, f928])).
fof(f911, plain, (spl18_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_106])])).
fof(f2341, plain, (~ (e21 = op2(e21, e20)) | ~ spl18_106), inference(forward_demodulation, [], [f1268, f2008])).
fof(f2008, plain, ((e21 = h2(e11)) | ~ spl18_106), inference(backward_demodulation, [], [f358, f913])).
fof(f913, plain, ((e21 = op2(e21, e21)) | ~ spl18_106), inference(avatar_component_clause, [], [f911])).
fof(f1268, plain, ~ (op2(e21, e20) = h2(e11)), inference(backward_demodulation, [], [f234, f358])).
fof(f234, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f2328, plain, (~ spl18_91 | ~ spl18_87), inference(avatar_split_clause, [], [f2327, f830, f847])).
fof(f2327, plain, (~ (e22 = op2(e22, e21)) | ~ spl18_87), inference(forward_demodulation, [], [f1278, f2051])).
fof(f2051, plain, ((e22 = h3(e11)) | ~ spl18_87), inference(backward_demodulation, [], [f362, f832])).
fof(f1278, plain, ~ (op2(e22, e21) = h3(e11)), inference(backward_demodulation, [], [f243, f362])).
fof(f243, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f2326, plain, (~ spl18_92 | ~ spl18_76), inference(avatar_split_clause, [], [f2325, f783, f851])).
fof(f851, plain, (spl18_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_92])])).
fof(f2325, plain, (~ (e23 = op2(e22, e21)) | ~ spl18_76), inference(forward_demodulation, [], [f215, f785])).
fof(f215, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2322, plain, (~ spl18_172 | ~ spl18_82), inference(avatar_split_clause, [], [f2321, f809, f1325])).
fof(f809, plain, (spl18_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_82])])).
fof(f2321, plain, (~ (e21 = h4(e11)) | ~ spl18_82), inference(forward_demodulation, [], [f1291, f811])).
fof(f811, plain, ((e21 = op2(e22, e23)) | ~ spl18_82), inference(avatar_component_clause, [], [f809])).
fof(f1291, plain, ~ (op2(e22, e23) = h4(e11)), inference(backward_demodulation, [], [f227, f366])).
fof(f227, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2320, plain, (spl18_168 | ~ spl18_79), inference(avatar_split_clause, [], [f2319, f796, f1304])).
fof(f2319, plain, ((e22 = h4(e12)) | ~ spl18_79), inference(forward_demodulation, [], [f1283, f798])).
fof(f2312, plain, (~ spl18_63 | ~ spl18_15), inference(avatar_split_clause, [], [f2311, f492, f696])).
fof(f696, plain, (spl18_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl18_63])])).
fof(f2311, plain, (~ (op1(e10, e10) = e12) | ~ spl18_15), inference(forward_demodulation, [], [f158, f494])).
fof(f158, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2304, plain, (~ spl18_60 | ~ spl18_12), inference(avatar_split_clause, [], [f2303, f479, f683])).
fof(f683, plain, (spl18_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_60])])).
fof(f2303, plain, (~ (e13 = op1(e10, e11)) | ~ spl18_12), inference(forward_demodulation, [], [f164, f481])).
fof(f164, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2295, plain, (~ spl18_47 | ~ spl18_15), inference(avatar_split_clause, [], [f2294, f492, f628])).
fof(f628, plain, (spl18_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_47])])).
fof(f2294, plain, (~ (e12 = op1(e11, e10)) | ~ spl18_15), inference(forward_demodulation, [], [f160, f494])).
fof(f160, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2289, plain, (spl18_41 | ~ spl18_2), inference(avatar_split_clause, [], [f2247, f437, f603])).
fof(f2247, plain, ((e10 = op1(e11, e11)) | ~ spl18_2), inference(backward_demodulation, [], [f346, f439])).
fof(f346, plain, (e10 = op1(op1(e13, e13), op1(e13, e13))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e12 = op1(e13, op1(op1(e13, e13), op1(e13, e13)))) & (e11 = op1(e13, e13)) & (e10 = op1(op1(e13, e13), op1(e13, e13)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax12)).
fof(f2280, plain, (~ spl18_34 | ~ spl18_2), inference(avatar_split_clause, [], [f2279, f437, f573])).
fof(f573, plain, (spl18_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_34])])).
fof(f2279, plain, (~ (e11 = op1(e11, e13)) | ~ spl18_2), inference(forward_demodulation, [], [f178, f439])).
fof(f178, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2278, plain, (~ spl18_31 | ~ spl18_15), inference(avatar_split_clause, [], [f2277, f492, f560])).
fof(f560, plain, (spl18_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_31])])).
fof(f2277, plain, (~ (e12 = op1(e12, e10)) | ~ spl18_15), inference(forward_demodulation, [], [f161, f494])).
fof(f161, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2236, plain, (~ spl18_11 | ~ spl18_12), inference(avatar_contradiction_clause, [], [f2235])).
fof(f2235, plain, ($false | (~ spl18_11 | ~ spl18_12)), inference(subsumption_resolution, [], [f2234, f257])).
fof(f257, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f2234, plain, ((e12 = e13) | (~ spl18_11 | ~ spl18_12)), inference(backward_demodulation, [], [f481, f477])).
fof(f477, plain, ((e12 = op1(e13, e11)) | ~ spl18_11), inference(avatar_component_clause, [], [f475])).
fof(f475, plain, (spl18_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_11])])).
fof(f2232, plain, (~ spl18_4 | ~ spl18_12), inference(avatar_split_clause, [], [f2229, f479, f445])).
fof(f445, plain, (spl18_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_4])])).
fof(f2229, plain, (~ (e13 = op1(e13, e13)) | ~ spl18_12), inference(backward_demodulation, [], [f202, f481])).
fof(f202, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2231, plain, (~ spl18_8 | ~ spl18_12), inference(avatar_split_clause, [], [f2228, f479, f462])).
fof(f462, plain, (spl18_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_8])])).
fof(f2228, plain, (~ (e13 = op1(e13, e12)) | ~ spl18_12), inference(backward_demodulation, [], [f201, f481])).
fof(f201, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2224, plain, (~ spl18_15 | ~ spl18_16), inference(avatar_contradiction_clause, [], [f2223])).
fof(f2223, plain, ($false | (~ spl18_15 | ~ spl18_16)), inference(subsumption_resolution, [], [f2222, f257])).
fof(f2222, plain, ((e12 = e13) | (~ spl18_15 | ~ spl18_16)), inference(backward_demodulation, [], [f498, f494])).
fof(f498, plain, ((e13 = op1(e13, e10)) | ~ spl18_16), inference(avatar_component_clause, [], [f496])).
fof(f496, plain, (spl18_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_16])])).
fof(f2183, plain, (~ spl18_35 | ~ spl18_36), inference(avatar_contradiction_clause, [], [f2182])).
fof(f2182, plain, ($false | (~ spl18_35 | ~ spl18_36)), inference(subsumption_resolution, [], [f2181, f257])).
fof(f2181, plain, ((e12 = e13) | (~ spl18_35 | ~ spl18_36)), inference(backward_demodulation, [], [f583, f579])).
fof(f583, plain, ((e13 = op1(e11, e13)) | ~ spl18_36), inference(avatar_component_clause, [], [f581])).
fof(f2156, plain, (~ spl18_41 | ~ spl18_45), inference(avatar_split_clause, [], [f2151, f620, f603])).
fof(f620, plain, (spl18_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl18_45])])).
fof(f2151, plain, (~ (e10 = op1(e11, e11)) | ~ spl18_45), inference(backward_demodulation, [], [f186, f622])).
fof(f622, plain, ((e10 = op1(e11, e10)) | ~ spl18_45), inference(avatar_component_clause, [], [f620])).
fof(f186, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2145, plain, (~ spl18_51 | ~ spl18_52), inference(avatar_contradiction_clause, [], [f2144])).
fof(f2144, plain, ($false | (~ spl18_51 | ~ spl18_52)), inference(subsumption_resolution, [], [f2143, f257])).
fof(f2143, plain, ((e12 = e13) | (~ spl18_51 | ~ spl18_52)), inference(backward_demodulation, [], [f651, f647])).
fof(f2135, plain, (~ spl18_5 | ~ spl18_53), inference(avatar_split_clause, [], [f2131, f654, f450])).
fof(f654, plain, (spl18_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_53])])).
fof(f2131, plain, (~ (e10 = op1(e13, e12)) | ~ spl18_53), inference(backward_demodulation, [], [f170, f656])).
fof(f656, plain, ((e10 = op1(e10, e12)) | ~ spl18_53), inference(avatar_component_clause, [], [f654])).
fof(f170, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2124, plain, (~ spl18_41 | ~ spl18_57), inference(avatar_split_clause, [], [f2119, f671, f603])).
fof(f671, plain, (spl18_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_57])])).
fof(f2119, plain, (~ (e10 = op1(e11, e11)) | ~ spl18_57), inference(backward_demodulation, [], [f162, f673])).
fof(f673, plain, ((e10 = op1(e10, e11)) | ~ spl18_57), inference(avatar_component_clause, [], [f671])).
fof(f162, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f2099, plain, (~ spl18_65 | ~ spl18_66), inference(avatar_contradiction_clause, [], [f2098])).
fof(f2098, plain, ($false | (~ spl18_65 | ~ spl18_66)), inference(subsumption_resolution, [], [f2097, f258])).
fof(f258, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f2097, plain, ((e20 = e21) | (~ spl18_65 | ~ spl18_66)), inference(backward_demodulation, [], [f743, f739])).
fof(f739, plain, ((e20 = op2(e23, e23)) | ~ spl18_65), inference(avatar_component_clause, [], [f737])).
fof(f737, plain, (spl18_65 <=> (e20 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_65])])).
fof(f2096, plain, (spl18_172 | ~ spl18_66), inference(avatar_split_clause, [], [f2095, f741, f1325])).
fof(f2095, plain, ((e21 = h4(e11)) | ~ spl18_66), inference(backward_demodulation, [], [f366, f743])).
fof(f2085, plain, (~ spl18_75 | ~ spl18_76), inference(avatar_contradiction_clause, [], [f2084])).
fof(f2084, plain, ($false | (~ spl18_75 | ~ spl18_76)), inference(subsumption_resolution, [], [f2083, f263])).
fof(f263, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f2083, plain, ((e22 = e23) | (~ spl18_75 | ~ spl18_76)), inference(backward_demodulation, [], [f785, f781])).
fof(f781, plain, ((e22 = op2(e23, e21)) | ~ spl18_75), inference(avatar_component_clause, [], [f779])).
fof(f779, plain, (spl18_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_75])])).
fof(f2079, plain, (~ spl18_72 | ~ spl18_76), inference(avatar_split_clause, [], [f2075, f783, f766])).
fof(f766, plain, (spl18_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_72])])).
fof(f2075, plain, (~ (e23 = op2(e23, e22)) | ~ spl18_76), inference(backward_demodulation, [], [f249, f785])).
fof(f249, plain, ~ (op2(e23, e21) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2027, plain, (~ spl18_69 | ~ spl18_101), inference(avatar_split_clause, [], [f2024, f890, f754])).
fof(f890, plain, (spl18_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_101])])).
fof(f2024, plain, (~ (e20 = op2(e23, e22)) | ~ spl18_101), inference(backward_demodulation, [], [f220, f892])).
fof(f892, plain, ((e20 = op2(e21, e22)) | ~ spl18_101), inference(avatar_component_clause, [], [f890])).
fof(f220, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2002, plain, (~ spl18_190 | ~ spl18_109), inference(avatar_split_clause, [], [f1997, f924, f1420])).
fof(f924, plain, (spl18_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl18_109])])).
fof(f1997, plain, (~ (e20 = h2(e11)) | ~ spl18_109), inference(backward_demodulation, [], [f1268, f926])).
fof(f926, plain, ((e20 = op2(e21, e20)) | ~ spl18_109), inference(avatar_component_clause, [], [f924])).
fof(f1992, plain, (~ spl18_115 | ~ spl18_116), inference(avatar_contradiction_clause, [], [f1991])).
fof(f1991, plain, ($false | (~ spl18_115 | ~ spl18_116)), inference(subsumption_resolution, [], [f1990, f263])).
fof(f1990, plain, ((e22 = e23) | (~ spl18_115 | ~ spl18_116)), inference(backward_demodulation, [], [f955, f951])).
fof(f951, plain, ((e22 = op2(e20, e23)) | ~ spl18_115), inference(avatar_component_clause, [], [f949])).
fof(f949, plain, (spl18_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_115])])).
fof(f1982, plain, (~ spl18_69 | ~ spl18_117), inference(avatar_split_clause, [], [f1978, f958, f754])).
fof(f958, plain, (spl18_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_117])])).
fof(f1978, plain, (~ (e20 = op2(e23, e22)) | ~ spl18_117), inference(backward_demodulation, [], [f218, f960])).
fof(f960, plain, ((e20 = op2(e20, e22)) | ~ spl18_117), inference(avatar_component_clause, [], [f958])).
fof(f218, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f1971, plain, (~ spl18_190 | ~ spl18_121), inference(avatar_split_clause, [], [f1966, f975, f1420])).
fof(f975, plain, (spl18_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_121])])).
fof(f1966, plain, (~ (e20 = h2(e11)) | ~ spl18_121), inference(backward_demodulation, [], [f1265, f977])).
fof(f977, plain, ((e20 = op2(e20, e21)) | ~ spl18_121), inference(avatar_component_clause, [], [f975])).
fof(f1265, plain, ~ (op2(e20, e21) = h2(e11)), inference(backward_demodulation, [], [f210, f358])).
fof(f210, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f1553, plain, (~ spl18_200 | ~ spl18_201 | ~ spl18_202 | spl18_173 | spl18_170 | spl18_167 | ~ spl18_203 | ~ spl18_204 | ~ spl18_205 | ~ spl18_206 | ~ spl18_207 | ~ spl18_208 | ~ spl18_209 | ~ spl18_210 | ~ spl18_211 | ~ spl18_212 | ~ spl18_213 | ~ spl18_214 | ~ spl18_215), inference(avatar_split_clause, [], [f1488, f1550, f1546, f1542, f1538, f1534, f1530, f1526, f1522, f1518, f1514, f1510, f1506, f1502, f1300, f1316, f1332, f1498, f1494, f1490])).
fof(f1332, plain, (spl18_173 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl18_173])])).
fof(f1316, plain, (spl18_170 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl18_170])])).
fof(f1300, plain, (spl18_167 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl18_167])])).
fof(f1488, plain, (~ (h1(e11) = h4(op1(e10, e10))) | ~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (e20 = h4(op1(e11, e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12)))), inference(forward_demodulation, [], [f1487, f354])).
fof(f1487, plain, (~ (op2(e20, e20) = h4(op1(e10, e10))) | ~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (e20 = h4(op1(e11, e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12)))), inference(forward_demodulation, [], [f1486, f1297])).
fof(f1486, plain, (~ (h4(op1(e10, e11)) = op2(e20, h4(e11))) | ~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (e20 = h4(op1(e11, e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1485, f1297])).
fof(f1485, plain, (~ (h4(op1(e10, e12)) = op2(e20, h4(e12))) | ~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (e20 = h4(op1(e11, e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1484, f1297])).
fof(f1484, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (e20 = h4(op1(e11, e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1483, f1297])).
fof(f1483, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (e20 = h4(op1(e11, e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1482, f364])).
fof(f1482, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | ~ (e20 = h4(op1(e11, e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1481, f1297])).
fof(f1481, plain, (~ (e20 = h4(op1(e11, e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1480, f1294])).
fof(f1480, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1479, f364])).
fof(f1479, plain, (~ (h4(op1(e12, e10)) = op2(h4(e12), e20)) | ~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1478, f1297])).
fof(f1478, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1477, f364])).
fof(f1477, plain, (~ (h4(e12) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1476, f1283])).
fof(f1476, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1475, f364])).
fof(f1475, plain, (~ (h4(op1(e13, e10)) = op2(h4(e13), e20)) | ~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1474, f1297])).
fof(f1474, plain, (~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1473, f364])).
fof(f1473, plain, (~ (h4(op1(e13, e12)) = op2(e23, h4(e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1472, f364])).
fof(f1472, plain, (~ (h4(e11) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1471, f366])).
fof(f1471, plain, (~ (op2(e23, e23) = h4(op1(e13, e13))) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1470, f364])).
fof(f1470, plain, (sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f431, f364])).
fof(f431, plain, (~ (e23 = h4(e13)) | sP17 | sP16 | sP15 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f41])).
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
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', co1)).
fof(f1346, plain, ~ spl18_173, inference(avatar_split_clause, [], [f1345, f1332])).
fof(f1345, plain, ~ sP15, inference(subsumption_resolution, [], [f376, f1297])).
fof(f376, plain, (~ (e20 = h4(e10)) | ~ sP15), inference(cnf_transformation, [], [f50])).
fof(f50, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP15), inference(nnf_transformation, [], [f38])).
fof(f1328, plain, (~ spl18_170 | ~ spl18_172), inference(avatar_split_clause, [], [f373, f1325, f1316])).
fof(f373, plain, (~ (e21 = h4(e11)) | ~ sP16), inference(cnf_transformation, [], [f49])).
fof(f49, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP16), inference(nnf_transformation, [], [f39])).
fof(f1307, plain, (~ spl18_167 | ~ spl18_168), inference(avatar_split_clause, [], [f370, f1304, f1300])).
fof(f370, plain, (~ (e22 = h4(e12)) | ~ sP17), inference(cnf_transformation, [], [f48])).
fof(f48, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP17), inference(nnf_transformation, [], [f40])).
fof(f1255, plain, spl18_79, inference(avatar_split_clause, [], [f1254, f796])).
fof(f1254, plain, (e22 = op2(e23, e20)), inference(backward_demodulation, [], [f351, f349])).
fof(f351, plain, (e22 = op2(e23, op2(op2(e23, e23), op2(e23, e23)))), inference(cnf_transformation, [], [f13])).
fof(f1253, plain, spl18_66, inference(avatar_split_clause, [], [f350, f741])).
fof(f350, plain, (e21 = op2(e23, e23)), inference(cnf_transformation, [], [f13])).
fof(f1252, plain, spl18_15, inference(avatar_split_clause, [], [f1251, f492])).
fof(f1251, plain, (e12 = op1(e13, e10)), inference(backward_demodulation, [], [f348, f346])).
fof(f348, plain, (e12 = op1(e13, op1(op1(e13, e13), op1(e13, e13)))), inference(cnf_transformation, [], [f12])).
fof(f1250, plain, spl18_2, inference(avatar_split_clause, [], [f347, f437])).
fof(f347, plain, (e11 = op1(e13, e13)), inference(cnf_transformation, [], [f12])).
fof(f1249, plain, (spl18_158 | spl18_153 | spl18_148 | ~ spl18_166), inference(avatar_split_clause, [], [f325, f1246, f1146, f1170, f1194])).
fof(f1194, plain, (spl18_158 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl18_158])])).
fof(f1170, plain, (spl18_153 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl18_153])])).
fof(f1146, plain, (spl18_148 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl18_148])])).
fof(f325, plain, (~ (op2(e23, e20) = op2(e20, op2(e23, e20))) | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f28])).
fof(f28, plain, (((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & ((e23 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e20))) & ((~ (op2(e23, e23) = op2(e23, op2(e23, e23))) & ~ (op2(e23, e22) = op2(e22, op2(e23, e22))) & ~ (op2(e23, e21) = op2(e21, op2(e23, e21))) & ~ (op2(e23, e20) = op2(e20, op2(e23, e20)))) | sP5 | sP4 | sP3)), inference(definition_folding, [], [f11, e27, e26, e25])).
fof(f25, plain, ((~ (op2(e20, e23) = op2(e23, op2(e20, e23))) & ~ (op2(e20, e22) = op2(e22, op2(e20, e22))) & ~ (op2(e20, e21) = op2(e21, op2(e20, e21))) & ~ (op2(e20, e20) = op2(e20, op2(e20, e20)))) | ~ sP3), inference(usedef, [], [e25])).
fof(e25, plain, (sP3 <=> (~ (op2(e20, e23) = op2(e23, op2(e20, e23))) & ~ (op2(e20, e22) = op2(e22, op2(e20, e22))) & ~ (op2(e20, e21) = op2(e21, op2(e20, e21))) & ~ (op2(e20, e20) = op2(e20, op2(e20, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f26, plain, ((~ (op2(e21, e23) = op2(e23, op2(e21, e23))) & ~ (op2(e21, e22) = op2(e22, op2(e21, e22))) & ~ (op2(e21, e21) = op2(e21, op2(e21, e21))) & ~ (op2(e21, e20) = op2(e20, op2(e21, e20)))) | ~ sP4), inference(usedef, [], [e26])).
fof(e26, plain, (sP4 <=> (~ (op2(e21, e23) = op2(e23, op2(e21, e23))) & ~ (op2(e21, e22) = op2(e22, op2(e21, e22))) & ~ (op2(e21, e21) = op2(e21, op2(e21, e21))) & ~ (op2(e21, e20) = op2(e20, op2(e21, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f27, plain, ((~ (op2(e22, e23) = op2(e23, op2(e22, e23))) & ~ (op2(e22, e22) = op2(e22, op2(e22, e22))) & ~ (op2(e22, e21) = op2(e21, op2(e22, e21))) & ~ (op2(e22, e20) = op2(e20, op2(e22, e20)))) | ~ sP5), inference(usedef, [], [e27])).
fof(e27, plain, (sP5 <=> (~ (op2(e22, e23) = op2(e23, op2(e22, e23))) & ~ (op2(e22, e22) = op2(e22, op2(e22, e22))) & ~ (op2(e22, e21) = op2(e21, op2(e22, e21))) & ~ (op2(e22, e20) = op2(e20, op2(e22, e20))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f11, plain, (((e23 = op2(e23, e23)) | ~ (e23 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))) & ((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))) & ((e23 = op2(e23, e20)) | ~ (e20 = op2(e23, e23))) & ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))) & ((e22 = op2(e22, e22)) | ~ (e22 = op2(e22, e22))) & ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))) & ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))) & ((e21 = op2(e21, e23)) | ~ (e23 = op2(e21, e21))) & ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))) & ((e21 = op2(e21, e21)) | ~ (e21 = op2(e21, e21))) & ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))) & ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)) & ((e20 = op2(e20, e22)) | ~ (op2(e20, e20) = e22)) & ((e20 = op2(e20, e21)) | ~ (op2(e20, e20) = e21)) & ((e20 = op2(e20, e20)) | ~ (e20 = op2(e20, e20))) & ((e23 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e20))) & ((~ (op2(e23, e23) = op2(e23, op2(e23, e23))) & ~ (op2(e23, e22) = op2(e22, op2(e23, e22))) & ~ (op2(e23, e21) = op2(e21, op2(e23, e21))) & ~ (op2(e23, e20) = op2(e20, op2(e23, e20)))) | (~ (op2(e22, e23) = op2(e23, op2(e22, e23))) & ~ (op2(e22, e22) = op2(e22, op2(e22, e22))) & ~ (op2(e22, e21) = op2(e21, op2(e22, e21))) & ~ (op2(e22, e20) = op2(e20, op2(e22, e20)))) | (~ (op2(e21, e23) = op2(e23, op2(e21, e23))) & ~ (op2(e21, e22) = op2(e22, op2(e21, e22))) & ~ (op2(e21, e21) = op2(e21, op2(e21, e21))) & ~ (op2(e21, e20) = op2(e20, op2(e21, e20)))) | (~ (op2(e20, e23) = op2(e23, op2(e20, e23))) & ~ (op2(e20, e22) = op2(e22, op2(e20, e22))) & ~ (op2(e20, e21) = op2(e21, op2(e20, e21))) & ~ (op2(e20, e20) = op2(e20, op2(e20, e20)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax11)).
fof(f1244, plain, (spl18_158 | spl18_153 | spl18_148 | ~ spl18_165), inference(avatar_split_clause, [], [f326, f1241, f1146, f1170, f1194])).
fof(f326, plain, (~ (op2(e23, e21) = op2(e21, op2(e23, e21))) | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f28])).
fof(f1239, plain, (spl18_158 | spl18_153 | spl18_148 | ~ spl18_164), inference(avatar_split_clause, [], [f327, f1236, f1146, f1170, f1194])).
fof(f327, plain, (~ (op2(e23, e22) = op2(e22, op2(e23, e22))) | sP5 | sP4 | sP3), inference(cnf_transformation, [], [f28])).
fof(f1229, plain, (spl18_125 | spl18_106 | spl18_87 | spl18_68), inference(avatar_split_clause, [], [f329, f749, f830, f911, f992])).
fof(f329, plain, ((e23 = op2(e23, e23)) | (e22 = op2(e22, e22)) | (e21 = op2(e21, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f28])).
fof(f1226, plain, (~ spl18_128 | spl18_113), inference(avatar_split_clause, [], [f333, f941, f1004])).
fof(f333, plain, ((e20 = op2(e20, e23)) | ~ (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f28])).
fof(f1225, plain, (~ spl18_105 | spl18_110), inference(avatar_split_clause, [], [f334, f928, f907])).
fof(f334, plain, ((e21 = op2(e21, e20)) | ~ (e20 = op2(e21, e21))), inference(cnf_transformation, [], [f28])).
fof(f1224, plain, (~ spl18_107 | spl18_102), inference(avatar_split_clause, [], [f336, f894, f915])).
fof(f915, plain, (spl18_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl18_107])])).
fof(f336, plain, ((e21 = op2(e21, e22)) | ~ (e22 = op2(e21, e21))), inference(cnf_transformation, [], [f28])).
fof(f1222, plain, (~ spl18_85 | spl18_95), inference(avatar_split_clause, [], [f338, f864, f822])).
fof(f822, plain, (spl18_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_85])])).
fof(f338, plain, ((e22 = op2(e22, e20)) | ~ (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f28])).
fof(f1221, plain, (~ spl18_86 | spl18_91), inference(avatar_split_clause, [], [f339, f847, f826])).
fof(f339, plain, ((e22 = op2(e22, e21)) | ~ (e21 = op2(e22, e22))), inference(cnf_transformation, [], [f28])).
fof(f1220, plain, (~ spl18_88 | spl18_83), inference(avatar_split_clause, [], [f341, f813, f834])).
fof(f834, plain, (spl18_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl18_88])])).
fof(f341, plain, ((e22 = op2(e22, e23)) | ~ (e23 = op2(e22, e22))), inference(cnf_transformation, [], [f28])).
fof(f1218, plain, (~ spl18_66 | spl18_76), inference(avatar_split_clause, [], [f343, f783, f741])).
fof(f343, plain, ((e23 = op2(e23, e21)) | ~ (e21 = op2(e23, e23))), inference(cnf_transformation, [], [f28])).
fof(f1217, plain, (~ spl18_67 | spl18_72), inference(avatar_split_clause, [], [f344, f766, f745])).
fof(f745, plain, (spl18_67 <=> (e22 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl18_67])])).
fof(f344, plain, ((e23 = op2(e23, e22)) | ~ (e22 = op2(e23, e23))), inference(cnf_transformation, [], [f28])).
fof(f1216, plain, (~ spl18_158 | ~ spl18_162), inference(avatar_split_clause, [], [f321, f1213, f1194])).
fof(f321, plain, (~ (op2(e20, e20) = op2(e20, op2(e20, e20))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f47, plain, ((~ (op2(e20, e23) = op2(e23, op2(e20, e23))) & ~ (op2(e20, e22) = op2(e22, op2(e20, e22))) & ~ (op2(e20, e21) = op2(e21, op2(e20, e21))) & ~ (op2(e20, e20) = op2(e20, op2(e20, e20)))) | ~ sP3), inference(nnf_transformation, [], [f25])).
fof(f1206, plain, (~ spl18_158 | ~ spl18_160), inference(avatar_split_clause, [], [f323, f1203, f1194])).
fof(f323, plain, (~ (op2(e20, e22) = op2(e22, op2(e20, e22))) | ~ sP3), inference(cnf_transformation, [], [f47])).
fof(f1192, plain, (~ spl18_153 | ~ spl18_157), inference(avatar_split_clause, [], [f317, f1189, f1170])).
fof(f317, plain, (~ (op2(e21, e20) = op2(e20, op2(e21, e20))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f46, plain, ((~ (op2(e21, e23) = op2(e23, op2(e21, e23))) & ~ (op2(e21, e22) = op2(e22, op2(e21, e22))) & ~ (op2(e21, e21) = op2(e21, op2(e21, e21))) & ~ (op2(e21, e20) = op2(e20, op2(e21, e20)))) | ~ sP4), inference(nnf_transformation, [], [f26])).
fof(f1182, plain, (~ spl18_153 | ~ spl18_155), inference(avatar_split_clause, [], [f319, f1179, f1170])).
fof(f319, plain, (~ (op2(e21, e22) = op2(e22, op2(e21, e22))) | ~ sP4), inference(cnf_transformation, [], [f46])).
fof(f1168, plain, (~ spl18_148 | ~ spl18_152), inference(avatar_split_clause, [], [f313, f1165, f1146])).
fof(f313, plain, (~ (op2(e22, e20) = op2(e20, op2(e22, e20))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f45, plain, ((~ (op2(e22, e23) = op2(e23, op2(e22, e23))) & ~ (op2(e22, e22) = op2(e22, op2(e22, e22))) & ~ (op2(e22, e21) = op2(e21, op2(e22, e21))) & ~ (op2(e22, e20) = op2(e20, op2(e22, e20)))) | ~ sP5), inference(nnf_transformation, [], [f27])).
fof(f1163, plain, (~ spl18_148 | ~ spl18_151), inference(avatar_split_clause, [], [f314, f1160, f1146])).
fof(f314, plain, (~ (op2(e22, e21) = op2(e21, op2(e22, e21))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f1158, plain, (~ spl18_148 | ~ spl18_150), inference(avatar_split_clause, [], [f315, f1155, f1146])).
fof(f315, plain, (~ (op2(e22, e22) = op2(e22, op2(e22, e22))) | ~ sP5), inference(cnf_transformation, [], [f45])).
fof(f1144, plain, (spl18_139 | spl18_134 | spl18_129 | ~ spl18_147), inference(avatar_split_clause, [], [f292, f1141, f1041, f1065, f1089])).
fof(f1089, plain, (spl18_139 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl18_139])])).
fof(f1065, plain, (spl18_134 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl18_134])])).
fof(f1041, plain, (spl18_129 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl18_129])])).
fof(f292, plain, (~ (op1(e13, e10) = op1(e10, op1(e13, e10))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f24])).
fof(f24, plain, (((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & ((e13 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e10))) & ((~ (op1(e13, e13) = op1(e13, op1(e13, e13))) & ~ (op1(e13, e12) = op1(e12, op1(e13, e12))) & ~ (op1(e13, e11) = op1(e11, op1(e13, e11))) & ~ (op1(e13, e10) = op1(e10, op1(e13, e10)))) | sP2 | sP1 | sP0)), inference(definition_folding, [], [f10, e23, e22, e21])).
fof(f21, plain, ((~ (op1(e10, e13) = op1(e13, op1(e10, e13))) & ~ (op1(e10, e12) = op1(e12, op1(e10, e12))) & ~ (op1(e10, e11) = op1(e11, op1(e10, e11))) & ~ (op1(e10, e10) = op1(e10, op1(e10, e10)))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (op1(e10, e13) = op1(e13, op1(e10, e13))) & ~ (op1(e10, e12) = op1(e12, op1(e10, e12))) & ~ (op1(e10, e11) = op1(e11, op1(e10, e11))) & ~ (op1(e10, e10) = op1(e10, op1(e10, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (op1(e11, e13) = op1(e13, op1(e11, e13))) & ~ (op1(e11, e12) = op1(e12, op1(e11, e12))) & ~ (op1(e11, e11) = op1(e11, op1(e11, e11))) & ~ (op1(e11, e10) = op1(e10, op1(e11, e10)))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (op1(e11, e13) = op1(e13, op1(e11, e13))) & ~ (op1(e11, e12) = op1(e12, op1(e11, e12))) & ~ (op1(e11, e11) = op1(e11, op1(e11, e11))) & ~ (op1(e11, e10) = op1(e10, op1(e11, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (op1(e12, e13) = op1(e13, op1(e12, e13))) & ~ (op1(e12, e12) = op1(e12, op1(e12, e12))) & ~ (op1(e12, e11) = op1(e11, op1(e12, e11))) & ~ (op1(e12, e10) = op1(e10, op1(e12, e10)))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (op1(e12, e13) = op1(e13, op1(e12, e13))) & ~ (op1(e12, e12) = op1(e12, op1(e12, e12))) & ~ (op1(e12, e11) = op1(e11, op1(e12, e11))) & ~ (op1(e12, e10) = op1(e10, op1(e12, e10))))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f10, plain, (((e13 = op1(e13, e13)) | ~ (e13 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))) & ((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))) & ((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))) & ((e12 = op1(e12, e13)) | ~ (e13 = op1(e12, e12))) & ((e12 = op1(e12, e12)) | ~ (e12 = op1(e12, e12))) & ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))) & ((e12 = op1(e12, e10)) | ~ (e10 = op1(e12, e12))) & ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))) & ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))) & ((e11 = op1(e11, e11)) | ~ (e11 = op1(e11, e11))) & ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))) & ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)) & ((e10 = op1(e10, e12)) | ~ (op1(e10, e10) = e12)) & ((e10 = op1(e10, e11)) | ~ (op1(e10, e10) = e11)) & ((e10 = op1(e10, e10)) | ~ (e10 = op1(e10, e10))) & ((e13 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e10))) & ((~ (op1(e13, e13) = op1(e13, op1(e13, e13))) & ~ (op1(e13, e12) = op1(e12, op1(e13, e12))) & ~ (op1(e13, e11) = op1(e11, op1(e13, e11))) & ~ (op1(e13, e10) = op1(e10, op1(e13, e10)))) | (~ (op1(e12, e13) = op1(e13, op1(e12, e13))) & ~ (op1(e12, e12) = op1(e12, op1(e12, e12))) & ~ (op1(e12, e11) = op1(e11, op1(e12, e11))) & ~ (op1(e12, e10) = op1(e10, op1(e12, e10)))) | (~ (op1(e11, e13) = op1(e13, op1(e11, e13))) & ~ (op1(e11, e12) = op1(e12, op1(e11, e12))) & ~ (op1(e11, e11) = op1(e11, op1(e11, e11))) & ~ (op1(e11, e10) = op1(e10, op1(e11, e10)))) | (~ (op1(e10, e13) = op1(e13, op1(e10, e13))) & ~ (op1(e10, e12) = op1(e12, op1(e10, e12))) & ~ (op1(e10, e11) = op1(e11, op1(e10, e11))) & ~ (op1(e10, e10) = op1(e10, op1(e10, e10)))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax10)).
fof(f1139, plain, (spl18_139 | spl18_134 | spl18_129 | ~ spl18_146), inference(avatar_split_clause, [], [f293, f1136, f1041, f1065, f1089])).
fof(f293, plain, (~ (op1(e13, e11) = op1(e11, op1(e13, e11))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f24])).
fof(f1134, plain, (spl18_139 | spl18_134 | spl18_129 | ~ spl18_145), inference(avatar_split_clause, [], [f294, f1131, f1041, f1065, f1089])).
fof(f294, plain, (~ (op1(e13, e12) = op1(e12, op1(e13, e12))) | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f24])).
fof(f1124, plain, (spl18_61 | spl18_42 | spl18_23 | spl18_4), inference(avatar_split_clause, [], [f296, f445, f526, f607, f688])).
fof(f296, plain, ((e13 = op1(e13, e13)) | (e12 = op1(e12, e12)) | (e11 = op1(e11, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f24])).
fof(f1121, plain, (~ spl18_64 | spl18_49), inference(avatar_split_clause, [], [f300, f637, f700])).
fof(f700, plain, (spl18_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl18_64])])).
fof(f300, plain, ((e10 = op1(e10, e13)) | ~ (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f24])).
fof(f1120, plain, (~ spl18_41 | spl18_46), inference(avatar_split_clause, [], [f301, f624, f603])).
fof(f301, plain, ((e11 = op1(e11, e10)) | ~ (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f24])).
fof(f1119, plain, (~ spl18_43 | spl18_38), inference(avatar_split_clause, [], [f303, f590, f611])).
fof(f611, plain, (spl18_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_43])])).
fof(f303, plain, ((e11 = op1(e11, e12)) | ~ (e12 = op1(e11, e11))), inference(cnf_transformation, [], [f24])).
fof(f1118, plain, (~ spl18_44 | spl18_34), inference(avatar_split_clause, [], [f304, f573, f615])).
fof(f615, plain, (spl18_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl18_44])])).
fof(f304, plain, ((e11 = op1(e11, e13)) | ~ (e13 = op1(e11, e11))), inference(cnf_transformation, [], [f24])).
fof(f1116, plain, (~ spl18_22 | spl18_27), inference(avatar_split_clause, [], [f306, f543, f522])).
fof(f522, plain, (spl18_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl18_22])])).
fof(f306, plain, ((e12 = op1(e12, e11)) | ~ (e11 = op1(e12, e12))), inference(cnf_transformation, [], [f24])).
fof(f1114, plain, (~ spl18_1 | spl18_16), inference(avatar_split_clause, [], [f309, f496, f433])).
fof(f433, plain, (spl18_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_1])])).
fof(f309, plain, ((e13 = op1(e13, e10)) | ~ (e10 = op1(e13, e13))), inference(cnf_transformation, [], [f24])).
fof(f1113, plain, (~ spl18_2 | spl18_12), inference(avatar_split_clause, [], [f310, f479, f437])).
fof(f310, plain, ((e13 = op1(e13, e11)) | ~ (e11 = op1(e13, e13))), inference(cnf_transformation, [], [f24])).
fof(f1112, plain, (~ spl18_3 | spl18_8), inference(avatar_split_clause, [], [f311, f462, f441])).
fof(f441, plain, (spl18_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl18_3])])).
fof(f311, plain, ((e13 = op1(e13, e12)) | ~ (e12 = op1(e13, e13))), inference(cnf_transformation, [], [f24])).
fof(f1111, plain, (~ spl18_139 | ~ spl18_143), inference(avatar_split_clause, [], [f288, f1108, f1089])).
fof(f288, plain, (~ (op1(e10, e10) = op1(e10, op1(e10, e10))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f44, plain, ((~ (op1(e10, e13) = op1(e13, op1(e10, e13))) & ~ (op1(e10, e12) = op1(e12, op1(e10, e12))) & ~ (op1(e10, e11) = op1(e11, op1(e10, e11))) & ~ (op1(e10, e10) = op1(e10, op1(e10, e10)))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1101, plain, (~ spl18_139 | ~ spl18_141), inference(avatar_split_clause, [], [f290, f1098, f1089])).
fof(f290, plain, (~ (op1(e10, e12) = op1(e12, op1(e10, e12))) | ~ sP0), inference(cnf_transformation, [], [f44])).
fof(f1087, plain, (~ spl18_134 | ~ spl18_138), inference(avatar_split_clause, [], [f284, f1084, f1065])).
fof(f284, plain, (~ (op1(e11, e10) = op1(e10, op1(e11, e10))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f43, plain, ((~ (op1(e11, e13) = op1(e13, op1(e11, e13))) & ~ (op1(e11, e12) = op1(e12, op1(e11, e12))) & ~ (op1(e11, e11) = op1(e11, op1(e11, e11))) & ~ (op1(e11, e10) = op1(e10, op1(e11, e10)))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1077, plain, (~ spl18_134 | ~ spl18_136), inference(avatar_split_clause, [], [f286, f1074, f1065])).
fof(f286, plain, (~ (op1(e11, e12) = op1(e12, op1(e11, e12))) | ~ sP1), inference(cnf_transformation, [], [f43])).
fof(f1063, plain, (~ spl18_129 | ~ spl18_133), inference(avatar_split_clause, [], [f280, f1060, f1041])).
fof(f280, plain, (~ (op1(e12, e10) = op1(e10, op1(e12, e10))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f42, plain, ((~ (op1(e12, e13) = op1(e13, op1(e12, e13))) & ~ (op1(e12, e12) = op1(e12, op1(e12, e12))) & ~ (op1(e12, e11) = op1(e11, op1(e12, e11))) & ~ (op1(e12, e10) = op1(e10, op1(e12, e10)))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1058, plain, (~ spl18_129 | ~ spl18_132), inference(avatar_split_clause, [], [f281, f1055, f1041])).
fof(f281, plain, (~ (op1(e12, e11) = op1(e11, op1(e12, e11))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f1053, plain, (~ spl18_129 | ~ spl18_131), inference(avatar_split_clause, [], [f282, f1050, f1041])).
fof(f282, plain, (~ (op1(e12, e12) = op1(e12, op1(e12, e12))) | ~ sP2), inference(cnf_transformation, [], [f42])).
fof(f1038, plain, (spl18_125 | spl18_109 | spl18_93 | spl18_77), inference(avatar_split_clause, [], [f125, f788, f856, f924, f992])).
fof(f125, plain, ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax4)).
fof(f1033, plain, (spl18_128 | spl18_124 | spl18_120 | spl18_116), inference(avatar_split_clause, [], [f130, f953, f970, f987, f1004])).
fof(f130, plain, ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)), inference(cnf_transformation, [], [f4])).
fof(f1026, plain, (spl18_123 | spl18_107 | spl18_91 | spl18_75), inference(avatar_split_clause, [], [f137, f779, f847, f915, f983])).
fof(f137, plain, ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1021, plain, (spl18_94 | spl18_90 | spl18_86 | spl18_82), inference(avatar_split_clause, [], [f142, f809, f826, f843, f860])).
fof(f142, plain, ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1020, plain, (spl18_118 | spl18_102 | spl18_86 | spl18_70), inference(avatar_split_clause, [], [f143, f758, f826, f894, f962])).
fof(f143, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1019, plain, (spl18_95 | spl18_91 | spl18_87 | spl18_83), inference(avatar_split_clause, [], [f144, f813, f830, f847, f864])).
fof(f144, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1017, plain, (spl18_96 | spl18_92 | spl18_88 | spl18_84), inference(avatar_split_clause, [], [f146, f817, f834, f851, f868])).
fof(f146, plain, ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1015, plain, (spl18_77 | spl18_73 | spl18_69 | spl18_65), inference(avatar_split_clause, [], [f148, f737, f754, f771, f788])).
fof(f148, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1014, plain, (spl18_113 | spl18_97 | spl18_81 | spl18_65), inference(avatar_split_clause, [], [f149, f737, f805, f873, f941])).
fof(f149, plain, ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1010, plain, (spl18_115 | spl18_99 | spl18_83 | spl18_67), inference(avatar_split_clause, [], [f153, f745, f813, f881, f949])).
fof(f153, plain, ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1008, plain, (spl18_116 | spl18_100 | spl18_84 | spl18_68), inference(avatar_split_clause, [], [f155, f749, f817, f885, f953])).
fof(f155, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f990, plain, (spl18_121 | spl18_122 | spl18_123 | spl18_124), inference(avatar_split_clause, [], [f109, f987, f983, f979, f975])).
fof(f109, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax3)).
fof(f973, plain, (spl18_117 | spl18_118 | spl18_119 | spl18_120), inference(avatar_split_clause, [], [f110, f970, f966, f962, f958])).
fof(f110, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f905, plain, (spl18_101 | spl18_102 | spl18_103 | spl18_104), inference(avatar_split_clause, [], [f114, f902, f898, f894, f890])).
fof(f114, plain, ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))), inference(cnf_transformation, [], [f3])).
fof(f871, plain, (spl18_93 | spl18_94 | spl18_95 | spl18_96), inference(avatar_split_clause, [], [f116, f868, f864, f860, f856])).
fof(f116, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f837, plain, (spl18_85 | spl18_86 | spl18_87 | spl18_88), inference(avatar_split_clause, [], [f118, f834, f830, f826, f822])).
fof(f118, plain, ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f3])).
fof(f735, plain, (spl18_61 | spl18_57 | spl18_53 | spl18_49), inference(avatar_split_clause, [], [f76, f637, f654, f671, f688])).
fof(f76, plain, ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax2)).
fof(f734, plain, (spl18_61 | spl18_45 | spl18_29 | spl18_13), inference(avatar_split_clause, [], [f77, f484, f552, f620, f688])).
fof(f77, plain, ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f731, plain, (spl18_63 | spl18_59 | spl18_55 | spl18_51), inference(avatar_split_clause, [], [f80, f645, f662, f679, f696])).
fof(f80, plain, ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f729, plain, (spl18_64 | spl18_60 | spl18_56 | spl18_52), inference(avatar_split_clause, [], [f82, f649, f666, f683, f700])).
fof(f82, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f728, plain, (spl18_64 | spl18_48 | spl18_32 | spl18_16), inference(avatar_split_clause, [], [f83, f496, f564, f632, f700])).
fof(f83, plain, ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f723, plain, (spl18_47 | spl18_43 | spl18_39 | spl18_35), inference(avatar_split_clause, [], [f88, f577, f594, f611, f628])).
fof(f88, plain, ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f722, plain, (spl18_59 | spl18_43 | spl18_27 | spl18_11), inference(avatar_split_clause, [], [f89, f475, f543, f611, f679])).
fof(f89, plain, ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f721, plain, (spl18_48 | spl18_44 | spl18_40 | spl18_36), inference(avatar_split_clause, [], [f90, f581, f598, f615, f632])).
fof(f90, plain, ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f717, plain, (spl18_30 | spl18_26 | spl18_22 | spl18_18), inference(avatar_split_clause, [], [f94, f505, f522, f539, f556])).
fof(f94, plain, ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f716, plain, (spl18_54 | spl18_38 | spl18_22 | spl18_6), inference(avatar_split_clause, [], [f95, f454, f522, f590, f658])).
fof(f95, plain, ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f715, plain, (spl18_31 | spl18_27 | spl18_23 | spl18_19), inference(avatar_split_clause, [], [f96, f509, f526, f543, f560])).
fof(f96, plain, ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f714, plain, (spl18_55 | spl18_39 | spl18_23 | spl18_7), inference(avatar_split_clause, [], [f97, f458, f526, f594, f662])).
fof(f97, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f711, plain, (spl18_13 | spl18_9 | spl18_5 | spl18_1), inference(avatar_split_clause, [], [f100, f433, f450, f467, f484])).
fof(f100, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f2])).
fof(f710, plain, (spl18_49 | spl18_33 | spl18_17 | spl18_1), inference(avatar_split_clause, [], [f101, f433, f501, f569, f637])).
fof(f101, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f706, plain, (spl18_51 | spl18_35 | spl18_19 | spl18_3), inference(avatar_split_clause, [], [f105, f441, f509, f577, f645])).
fof(f105, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f704, plain, (spl18_52 | spl18_36 | spl18_20 | spl18_4), inference(avatar_split_clause, [], [f107, f445, f513, f581, f649])).
fof(f107, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f686, plain, (spl18_57 | spl18_58 | spl18_59 | spl18_60), inference(avatar_split_clause, [], [f61, f683, f679, f675, f671])).
fof(f61, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG125+1.p', ax1)).