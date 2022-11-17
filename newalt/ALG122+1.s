fof(f6142, plain, $false, inference(avatar_sat_refutation, [], [f681, f698, f715, f732, f749, f766, f800, f817, f851, f868, f885, f886, f888, f892, f895, f896, f898, f901, f902, f903, f905, f906, f908, f911, f913, f914, f916, f917, f1002, f1019, f1036, f1053, f1070, f1104, f1121, f1138, f1155, f1172, f1189, f1190, f1191, f1198, f1199, f1201, f1202, f1203, f1205, f1207, f1209, f1210, f1211, f1213, f1216, f1217, f1218, f1219, f1220, f1221, f1231, f1241, f1246, f1247, f1249, f1260, f1261, f1266, f1268, f1270, f1271, f1281, f1286, f1287, f1289, f1291, f1301, f1306, f1307, f1308, f1311, f1316, f1320, f1321, f1326, f1327, f1329, f1341, f1351, f1356, f1357, f1359, f1360, f1361, f1366, f1367, f1370, f1376, f1377, f1394, f1411, f1445, f1455, f1465, f1470, f1471, f1473, f1484, f1485, f1492, f1494, f1495, f1505, f1511, f1513, f1515, f1525, f1531, f1532, f1535, f1543, f1544, f1545, f1553, f1554, f1555, f1565, f1575, f1582, f1584, f1585, f1594, f1595, f1601, f1618, f1635, f1669, f1670, f1672, f1673, f1675, f1730, f1751, f1959, f2354, f2365, f2367, f2378, f2397, f2403, f2408, f2417, f2420, f2444, f2450, f2456, f2465, f2481, f2484, f2505, f2510, f2551, f2555, f2564, f2589, f2606, f2617, f2628, f2629, f2631, f2633, f2635, f2637, f2639, f2646, f2650, f2656, f2660, f2666, f2673, f2694, f2712, f2749, f2759, f2771, f2789, f2794, f2797, f2814, f2834, f2839, f2841, f2843, f2854, f2858, f2865, f2870, f2885, f2887, f2903, f2914, f2921, f2935, f2960, f2970, f2986, f3002, f3039, f3050, f3053, f3079, f3082, f3098, f3101, f3119, f3140, f3160, f3162, f3179, f3181, f3192, f3193, f3201, f3203, f3216, f3219, f3243, f3281, f3289, f3294, f3297, f3304, f3317, f3332, f3341, f3352, f3367, f3395, f3424, f3431, f3469, f3490, f3499, f3508, f3519, f3528, f3534, f3538, f3540, f3547, f3558, f3572, f3581, f3583, f3603, f3656, f3679, f3691, f3696, f3721, f3741, f3752, f3772, f3774, f3778, f3794, f3836, f3853, f3893, f3903, f3926, f3940, f3954, f3961, f3970, f3976, f4001, f4002, f4024, f4053, f4066, f4077, f4130, f4180, f4181, f4193, f4200, f4217, f4292, f4298, f4325, f4346, f4361, f4370, f4392, f4488, f4526, f4530, f4620, f4641, f4653, f4779, f4787, f4796, f4852, f4939, f4942, f4962, f4964, f5027, f5083, f5102, f5103, f5145, f5152, f5153, f5161, f5170, f5263, f5271, f5275, f5304, f5307, f5328, f5339, f5344, f5353, f5368, f5382, f5399, f5402, f5477, f5482, f5483, f5522, f5523, f5533, f5535, f5543, f5553, f5561, f5574, f5576, f5584, f5587, f5591, f5633, f5634, f5653, f5654, f5670, f5675, f5694, f5696, f5710, f5726, f5737, f5748, f5749, f5763, f5764, f5774, f5775, f5794, f5808, f5816, f5837, f5853, f5883, f5916, f5921, f5971, f5986, f6019, f6022, f6023, f6025, f6034, f6120, f6134])).
fof(f6134, plain, (~ spl42_87 | ~ spl42_88), inference(avatar_contradiction_clause, [], [f6133])).
fof(f6133, plain, ($false | (~ spl42_87 | ~ spl42_88)), inference(subsumption_resolution, [], [f6132, f311])).
fof(f311, plain, ~ (e22 = e23), inference(cnf_transformation, [], [f8])).
fof(f8, plain, (~ (e22 = e23) & ~ (e21 = e23) & ~ (e21 = e22) & ~ (e20 = e23) & ~ (e20 = e22) & ~ (e20 = e21)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax8)).
fof(f6132, plain, ((e22 = e23) | (~ spl42_87 | ~ spl42_88)), inference(backward_demodulation, [], [f1018, f1014])).
fof(f1014, plain, ((e22 = op2(e22, e22)) | ~ spl42_87), inference(avatar_component_clause, [], [f1012])).
fof(f1012, plain, (spl42_87 <=> (e22 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_87])])).
fof(f1018, plain, ((e23 = op2(e22, e22)) | ~ spl42_88), inference(avatar_component_clause, [], [f1016])).
fof(f1016, plain, (spl42_88 <=> (e23 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_88])])).
fof(f6120, plain, (spl42_110 | ~ spl42_118 | ~ spl42_179), inference(avatar_contradiction_clause, [], [f6119])).
fof(f6119, plain, ($false | (spl42_110 | ~ spl42_118 | ~ spl42_179)), inference(subsumption_resolution, [], [f6117, f1111])).
fof(f1111, plain, (~ (e21 = op2(e21, e20)) | spl42_110), inference(avatar_component_clause, [], [f1110])).
fof(f1110, plain, (spl42_110 <=> (e21 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_110])])).
fof(f6117, plain, ((e21 = op2(e21, e20)) | (~ spl42_118 | ~ spl42_179)), inference(backward_demodulation, [], [f1622, f1146])).
fof(f1146, plain, ((e21 = op2(e20, e22)) | ~ spl42_118), inference(avatar_component_clause, [], [f1144])).
fof(f1144, plain, (spl42_118 <=> (e21 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_118])])).
fof(f1622, plain, ((op2(e20, e22) = op2(op2(e20, e22), e20)) | ~ spl42_179), inference(avatar_component_clause, [], [f1620])).
fof(f1620, plain, (spl42_179 <=> (op2(e20, e22) = op2(op2(e20, e22), e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_179])])).
fof(f6034, plain, (~ spl42_121 | ~ spl42_125), inference(avatar_split_clause, [], [f6033, f1174, f1157])).
fof(f1157, plain, (spl42_121 <=> (e20 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_121])])).
fof(f1174, plain, (spl42_125 <=> (e20 = op2(e20, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_125])])).
fof(f6033, plain, (~ (e20 = op2(e20, e21)) | ~ spl42_125), inference(forward_demodulation, [], [f1679, f5976])).
fof(f5976, plain, ((e20 = h1(e11)) | ~ spl42_125), inference(backward_demodulation, [], [f536, f1176])).
fof(f1176, plain, ((e20 = op2(e20, e20)) | ~ spl42_125), inference(avatar_component_clause, [], [f1174])).
fof(f536, plain, (op2(e20, e20) = h1(e11)), inference(cnf_transformation, [], [f14])).
fof(f14, plain, ((op2(e20, op2(e20, e20)) = h1(e12)) & (op2(e20, e20) = h1(e11)) & (h1(e10) = op2(e20, op2(e20, op2(e20, e20)))) & (e20 = h1(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax14)).
fof(f1679, plain, ~ (op2(e20, e21) = h1(e11)), inference(backward_demodulation, [], [f276, f536])).
fof(f276, plain, ~ (op2(e20, e20) = op2(e20, e21)), inference(cnf_transformation, [], [f6])).
fof(f6, plain, (~ (op2(e23, e22) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e23)) & ~ (op2(e23, e21) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e23)) & ~ (op2(e23, e20) = op2(e23, e22)) & ~ (op2(e23, e20) = op2(e23, e21)) & ~ (op2(e22, e22) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e23)) & ~ (op2(e22, e21) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e23)) & ~ (op2(e22, e20) = op2(e22, e22)) & ~ (op2(e22, e20) = op2(e22, e21)) & ~ (op2(e21, e22) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e23)) & ~ (op2(e21, e21) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e23)) & ~ (op2(e21, e20) = op2(e21, e22)) & ~ (op2(e21, e20) = op2(e21, e21)) & ~ (op2(e20, e22) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e23)) & ~ (op2(e20, e21) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e23)) & ~ (op2(e20, e20) = op2(e20, e22)) & ~ (op2(e20, e20) = op2(e20, e21)) & ~ (op2(e22, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e23, e23)) & ~ (op2(e21, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e23, e23)) & ~ (op2(e20, e23) = op2(e22, e23)) & ~ (op2(e20, e23) = op2(e21, e23)) & ~ (op2(e22, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e23, e22)) & ~ (op2(e21, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e23, e22)) & ~ (op2(e20, e22) = op2(e22, e22)) & ~ (op2(e20, e22) = op2(e21, e22)) & ~ (op2(e22, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e23, e21)) & ~ (op2(e21, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e23, e21)) & ~ (op2(e20, e21) = op2(e22, e21)) & ~ (op2(e20, e21) = op2(e21, e21)) & ~ (op2(e22, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e23, e20)) & ~ (op2(e21, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e23, e20)) & ~ (op2(e20, e20) = op2(e22, e20)) & ~ (op2(e20, e20) = op2(e21, e20))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax6)).
fof(f6025, plain, (~ spl42_120 | ~ spl42_116), inference(avatar_split_clause, [], [f5876, f1135, f1152])).
fof(f1152, plain, (spl42_120 <=> (e23 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_120])])).
fof(f1135, plain, (spl42_116 <=> (e23 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_116])])).
fof(f5876, plain, (~ (e23 = op2(e20, e22)) | ~ spl42_116), inference(forward_demodulation, [], [f281, f1137])).
fof(f1137, plain, ((e23 = op2(e20, e23)) | ~ spl42_116), inference(avatar_component_clause, [], [f1135])).
fof(f281, plain, ~ (op2(e20, e22) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f6023, plain, (~ spl42_118 | ~ spl42_102), inference(avatar_split_clause, [], [f5878, f1076, f1144])).
fof(f1076, plain, (spl42_102 <=> (e21 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_102])])).
fof(f5878, plain, (~ (e21 = op2(e20, e22)) | ~ spl42_102), inference(forward_demodulation, [], [f264, f1078])).
fof(f1078, plain, ((e21 = op2(e21, e22)) | ~ spl42_102), inference(avatar_component_clause, [], [f1076])).
fof(f264, plain, ~ (op2(e20, e22) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f6022, plain, (~ spl42_95 | ~ spl42_111), inference(avatar_split_clause, [], [f6021, f1114, f1046])).
fof(f1046, plain, (spl42_95 <=> (e22 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_95])])).
fof(f1114, plain, (spl42_111 <=> (e22 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_111])])).
fof(f6021, plain, (~ (e22 = op2(e22, e20)) | ~ spl42_111), inference(forward_demodulation, [], [f255, f1116])).
fof(f1116, plain, ((e22 = op2(e21, e20)) | ~ spl42_111), inference(avatar_component_clause, [], [f1114])).
fof(f255, plain, ~ (op2(e21, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f6019, plain, (~ spl42_78 | ~ spl42_192), inference(avatar_split_clause, [], [f3040, f1727, f974])).
fof(f974, plain, (spl42_78 <=> (e21 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_78])])).
fof(f1727, plain, (spl42_192 <=> (e21 = h4(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_192])])).
fof(f3040, plain, (~ (e21 = op2(e23, e20)) | ~ spl42_192), inference(forward_demodulation, [], [f1707, f1728])).
fof(f1728, plain, ((e21 = h4(e11)) | ~ spl42_192), inference(avatar_component_clause, [], [f1727])).
fof(f1707, plain, ~ (op2(e23, e20) = h4(e11)), inference(backward_demodulation, [], [f296, f548])).
fof(f548, plain, (op2(e23, e23) = h4(e11)), inference(cnf_transformation, [], [f17])).
fof(f17, plain, ((op2(e23, op2(e23, e23)) = h4(e12)) & (op2(e23, e23) = h4(e11)) & (op2(e23, op2(e23, op2(e23, e23))) = h4(e10)) & (e23 = h4(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax17)).
fof(f296, plain, ~ (op2(e23, e20) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f5986, plain, (~ spl42_109 | ~ spl42_125), inference(avatar_split_clause, [], [f5979, f1174, f1106])).
fof(f1106, plain, (spl42_109 <=> (e20 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_109])])).
fof(f5979, plain, (~ (e20 = op2(e21, e20)) | ~ spl42_125), inference(backward_demodulation, [], [f1676, f5976])).
fof(f1676, plain, ~ (op2(e21, e20) = h1(e11)), inference(backward_demodulation, [], [f252, f536])).
fof(f252, plain, ~ (op2(e20, e20) = op2(e21, e20)), inference(cnf_transformation, [], [f6])).
fof(f5971, plain, (~ spl42_69 | ~ spl42_88 | ~ spl42_181), inference(avatar_contradiction_clause, [], [f5970])).
fof(f5970, plain, ($false | (~ spl42_69 | ~ spl42_88 | ~ spl42_181)), inference(subsumption_resolution, [], [f5969, f308])).
fof(f308, plain, ~ (e20 = e23), inference(cnf_transformation, [], [f8])).
fof(f5969, plain, ((e20 = e23) | (~ spl42_69 | ~ spl42_88 | ~ spl42_181)), inference(forward_demodulation, [], [f5968, f938])).
fof(f938, plain, ((e20 = op2(e23, e22)) | ~ spl42_69), inference(avatar_component_clause, [], [f936])).
fof(f936, plain, (spl42_69 <=> (e20 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_69])])).
fof(f5968, plain, ((e23 = op2(e23, e22)) | (~ spl42_88 | ~ spl42_181)), inference(forward_demodulation, [], [f1630, f1018])).
fof(f1630, plain, ((op2(e22, e22) = op2(op2(e22, e22), e22)) | ~ spl42_181), inference(avatar_component_clause, [], [f1628])).
fof(f1628, plain, (spl42_181 <=> (op2(e22, e22) = op2(op2(e22, e22), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_181])])).
fof(f5921, plain, (~ spl42_121 | ~ spl42_57 | ~ spl42_192 | ~ spl42_196 | spl42_222), inference(avatar_split_clause, [], [f5920, f1900, f1748, f1727, f853, f1157])).
fof(f853, plain, (spl42_57 <=> (e10 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_57])])).
fof(f1748, plain, (spl42_196 <=> (e20 = h4(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_196])])).
fof(f1900, plain, (spl42_222 <=> (h4(op1(e10, e11)) = op2(h4(e10), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_222])])).
fof(f5920, plain, (~ (e20 = op2(e20, e21)) | (~ spl42_57 | ~ spl42_192 | ~ spl42_196 | spl42_222)), inference(forward_demodulation, [], [f5919, f1749])).
fof(f1749, plain, ((e20 = h4(e10)) | ~ spl42_196), inference(avatar_component_clause, [], [f1748])).
fof(f5919, plain, (~ (op2(e20, e21) = h4(e10)) | (~ spl42_57 | ~ spl42_192 | ~ spl42_196 | spl42_222)), inference(forward_demodulation, [], [f5918, f855])).
fof(f855, plain, ((e10 = op1(e10, e11)) | ~ spl42_57), inference(avatar_component_clause, [], [f853])).
fof(f5918, plain, (~ (op2(e20, e21) = h4(op1(e10, e11))) | (~ spl42_192 | ~ spl42_196 | spl42_222)), inference(forward_demodulation, [], [f5917, f1749])).
fof(f5917, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), e21)) | (~ spl42_192 | spl42_222)), inference(forward_demodulation, [], [f1902, f1728])).
fof(f1902, plain, (~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | spl42_222), inference(avatar_component_clause, [], [f1900])).
fof(f5916, plain, (~ spl42_109 | ~ spl42_45 | ~ spl42_192 | ~ spl42_196 | spl42_223), inference(avatar_split_clause, [], [f5915, f1904, f1748, f1727, f802, f1106])).
fof(f802, plain, (spl42_45 <=> (e10 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_45])])).
fof(f1904, plain, (spl42_223 <=> (h4(op1(e11, e10)) = op2(h4(e11), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_223])])).
fof(f5915, plain, (~ (e20 = op2(e21, e20)) | (~ spl42_45 | ~ spl42_192 | ~ spl42_196 | spl42_223)), inference(forward_demodulation, [], [f5914, f1749])).
fof(f5914, plain, (~ (op2(e21, e20) = h4(e10)) | (~ spl42_45 | ~ spl42_192 | ~ spl42_196 | spl42_223)), inference(forward_demodulation, [], [f5913, f804])).
fof(f804, plain, ((e10 = op1(e11, e10)) | ~ spl42_45), inference(avatar_component_clause, [], [f802])).
fof(f5913, plain, (~ (op2(e21, e20) = h4(op1(e11, e10))) | (~ spl42_192 | ~ spl42_196 | spl42_223)), inference(forward_demodulation, [], [f5912, f1728])).
fof(f5912, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), e20)) | (~ spl42_196 | spl42_223)), inference(forward_demodulation, [], [f1906, f1749])).
fof(f1906, plain, (~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | spl42_223), inference(avatar_component_clause, [], [f1904])).
fof(f5883, plain, (~ spl42_215 | ~ spl42_119), inference(avatar_split_clause, [], [f5882, f1148, f1845])).
fof(f1845, plain, (spl42_215 <=> (e22 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_215])])).
fof(f1148, plain, (spl42_119 <=> (e22 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_119])])).
fof(f5882, plain, (~ (e22 = h1(e11)) | ~ spl42_119), inference(forward_demodulation, [], [f1680, f1150])).
fof(f1150, plain, ((e22 = op2(e20, e22)) | ~ spl42_119), inference(avatar_component_clause, [], [f1148])).
fof(f1680, plain, ~ (op2(e20, e22) = h1(e11)), inference(backward_demodulation, [], [f277, f536])).
fof(f277, plain, ~ (op2(e20, e20) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f5853, plain, (~ spl42_92 | ~ spl42_28 | ~ spl42_192 | spl42_231), inference(avatar_split_clause, [], [f5852, f1936, f1727, f729, f1033])).
fof(f1033, plain, (spl42_92 <=> (e23 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_92])])).
fof(f729, plain, (spl42_28 <=> (e13 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_28])])).
fof(f1936, plain, (spl42_231 <=> (h4(op1(e12, e11)) = op2(e22, h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_231])])).
fof(f5852, plain, (~ (e23 = op2(e22, e21)) | (~ spl42_28 | ~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f5851, f546])).
fof(f546, plain, (e23 = h4(e13)), inference(cnf_transformation, [], [f17])).
fof(f5851, plain, (~ (op2(e22, e21) = h4(e13)) | (~ spl42_28 | ~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f5678, f731])).
fof(f731, plain, ((e13 = op1(e12, e11)) | ~ spl42_28), inference(avatar_component_clause, [], [f729])).
fof(f5678, plain, (~ (op2(e22, e21) = h4(op1(e12, e11))) | (~ spl42_192 | spl42_231)), inference(forward_demodulation, [], [f1938, f1728])).
fof(f1938, plain, (~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | spl42_231), inference(avatar_component_clause, [], [f1936])).
fof(f5837, plain, (~ spl42_17 | spl42_53 | ~ spl42_146), inference(avatar_contradiction_clause, [], [f5836])).
fof(f5836, plain, ($false | (~ spl42_17 | spl42_53 | ~ spl42_146)), inference(subsumption_resolution, [], [f5832, f837])).
fof(f837, plain, (~ (e10 = op1(e10, e12)) | spl42_53), inference(avatar_component_clause, [], [f836])).
fof(f836, plain, (spl42_53 <=> (e10 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_53])])).
fof(f5832, plain, ((e10 = op1(e10, e12)) | (~ spl42_17 | ~ spl42_146)), inference(backward_demodulation, [], [f1389, f685])).
fof(f685, plain, ((e10 = op1(e12, e13)) | ~ spl42_17), inference(avatar_component_clause, [], [f683])).
fof(f683, plain, (spl42_17 <=> (e10 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_17])])).
fof(f1389, plain, ((op1(e12, e13) = op1(op1(e12, e13), e12)) | ~ spl42_146), inference(avatar_component_clause, [], [f1387])).
fof(f1387, plain, (spl42_146 <=> (op1(e12, e13) = op1(op1(e12, e13), e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_146])])).
fof(f5816, plain, (~ spl42_20 | ~ spl42_28), inference(avatar_split_clause, [], [f5811, f729, f695])).
fof(f695, plain, (spl42_20 <=> (e13 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_20])])).
fof(f5811, plain, (~ (e13 = op1(e12, e13)) | ~ spl42_28), inference(backward_demodulation, [], [f244, f731])).
fof(f244, plain, ~ (op1(e12, e11) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5, plain, (~ (op1(e13, e12) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e13)) & ~ (op1(e13, e11) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e13)) & ~ (op1(e13, e10) = op1(e13, e12)) & ~ (op1(e13, e10) = op1(e13, e11)) & ~ (op1(e12, e12) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e13)) & ~ (op1(e12, e11) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e13)) & ~ (op1(e12, e10) = op1(e12, e12)) & ~ (op1(e12, e10) = op1(e12, e11)) & ~ (op1(e11, e12) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e13)) & ~ (op1(e11, e11) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e13)) & ~ (op1(e11, e10) = op1(e11, e12)) & ~ (op1(e11, e10) = op1(e11, e11)) & ~ (op1(e10, e12) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e13)) & ~ (op1(e10, e11) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e13)) & ~ (op1(e10, e10) = op1(e10, e12)) & ~ (op1(e10, e10) = op1(e10, e11)) & ~ (op1(e12, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e13, e13)) & ~ (op1(e11, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e13, e13)) & ~ (op1(e10, e13) = op1(e12, e13)) & ~ (op1(e10, e13) = op1(e11, e13)) & ~ (op1(e12, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e13, e12)) & ~ (op1(e11, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e13, e12)) & ~ (op1(e10, e12) = op1(e12, e12)) & ~ (op1(e10, e12) = op1(e11, e12)) & ~ (op1(e12, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e13, e11)) & ~ (op1(e11, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e13, e11)) & ~ (op1(e10, e11) = op1(e12, e11)) & ~ (op1(e10, e11) = op1(e11, e11)) & ~ (op1(e12, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e13, e10)) & ~ (op1(e11, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e13, e10)) & ~ (op1(e10, e10) = op1(e12, e10)) & ~ (op1(e10, e10) = op1(e11, e10))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax5)).
fof(f5808, plain, (~ spl42_23 | ~ spl42_31), inference(avatar_split_clause, [], [f5802, f742, f708])).
fof(f708, plain, (spl42_23 <=> (e12 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_23])])).
fof(f742, plain, (spl42_31 <=> (e12 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_31])])).
fof(f5802, plain, (~ (e12 = op1(e12, e12)) | ~ spl42_31), inference(backward_demodulation, [], [f241, f744])).
fof(f744, plain, ((e12 = op1(e12, e10)) | ~ spl42_31), inference(avatar_component_clause, [], [f742])).
fof(f241, plain, ~ (op1(e12, e10) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f5794, plain, (~ spl42_20 | ~ spl42_36), inference(avatar_split_clause, [], [f5788, f763, f695])).
fof(f763, plain, (spl42_36 <=> (e13 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_36])])).
fof(f5788, plain, (~ (e13 = op1(e12, e13)) | ~ spl42_36), inference(backward_demodulation, [], [f225, f765])).
fof(f765, plain, ((e13 = op1(e11, e13)) | ~ spl42_36), inference(avatar_component_clause, [], [f763])).
fof(f225, plain, ~ (op1(e11, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5775, plain, (~ spl42_31 | ~ spl42_51 | spl42_144), inference(avatar_split_clause, [], [f5769, f1379, f827, f742])).
fof(f827, plain, (spl42_51 <=> (e12 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_51])])).
fof(f1379, plain, (spl42_144 <=> (op1(e10, e13) = op1(op1(e10, e13), e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_144])])).
fof(f5769, plain, (~ (e12 = op1(e12, e10)) | (~ spl42_51 | spl42_144)), inference(backward_demodulation, [], [f1380, f829])).
fof(f829, plain, ((e12 = op1(e10, e13)) | ~ spl42_51), inference(avatar_component_clause, [], [f827])).
fof(f1380, plain, (~ (op1(e10, e13) = op1(op1(e10, e13), e10)) | spl42_144), inference(avatar_component_clause, [], [f1379])).
fof(f5774, plain, (~ spl42_35 | ~ spl42_51), inference(avatar_split_clause, [], [f5766, f827, f759])).
fof(f759, plain, (spl42_35 <=> (e12 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_35])])).
fof(f5766, plain, (~ (e12 = op1(e11, e13)) | ~ spl42_51), inference(backward_demodulation, [], [f222, f829])).
fof(f222, plain, ~ (op1(e10, e13) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f5764, plain, (~ spl42_49 | ~ spl42_57), inference(avatar_split_clause, [], [f5755, f853, f819])).
fof(f819, plain, (spl42_49 <=> (e10 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_49])])).
fof(f5755, plain, (~ (e10 = op1(e10, e13)) | ~ spl42_57), inference(backward_demodulation, [], [f232, f855])).
fof(f232, plain, ~ (op1(e10, e11) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f5763, plain, (~ spl42_25 | ~ spl42_57), inference(avatar_split_clause, [], [f5754, f853, f717])).
fof(f717, plain, (spl42_25 <=> (e10 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_25])])).
fof(f5754, plain, (~ (e10 = op1(e12, e11)) | ~ spl42_57), inference(backward_demodulation, [], [f211, f855])).
fof(f211, plain, ~ (op1(e10, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f5749, plain, (~ spl42_58 | ~ spl42_62), inference(avatar_split_clause, [], [f5741, f874, f857])).
fof(f857, plain, (spl42_58 <=> (e11 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_58])])).
fof(f874, plain, (spl42_62 <=> (op1(e10, e10) = e11)), introduced(avatar_definition, [new_symbols(naming, [spl42_62])])).
fof(f5741, plain, (~ (e11 = op1(e10, e11)) | ~ spl42_62), inference(backward_demodulation, [], [f228, f876])).
fof(f876, plain, ((op1(e10, e10) = e11) | ~ spl42_62), inference(avatar_component_clause, [], [f874])).
fof(f228, plain, ~ (op1(e10, e10) = op1(e10, e11)), inference(cnf_transformation, [], [f5])).
fof(f5748, plain, (~ spl42_30 | ~ spl42_62), inference(avatar_split_clause, [], [f5740, f874, f738])).
fof(f738, plain, (spl42_30 <=> (e11 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_30])])).
fof(f5740, plain, (~ (e11 = op1(e12, e10)) | ~ spl42_62), inference(backward_demodulation, [], [f205, f876])).
fof(f205, plain, ~ (op1(e10, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f5737, plain, (~ spl42_94 | ~ spl42_102 | spl42_189), inference(avatar_contradiction_clause, [], [f5736])).
fof(f5736, plain, ($false | (~ spl42_94 | ~ spl42_102 | spl42_189)), inference(subsumption_resolution, [], [f5733, f1078])).
fof(f5733, plain, (~ (e21 = op2(e21, e22)) | (~ spl42_94 | spl42_189)), inference(backward_demodulation, [], [f1663, f1044])).
fof(f1044, plain, ((e21 = op2(e22, e20)) | ~ spl42_94), inference(avatar_component_clause, [], [f1042])).
fof(f1042, plain, (spl42_94 <=> (e21 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_94])])).
fof(f1663, plain, (~ (op2(e22, e20) = op2(op2(e22, e20), e22)) | spl42_189), inference(avatar_component_clause, [], [f1662])).
fof(f1662, plain, (spl42_189 <=> (op2(e22, e20) = op2(op2(e22, e20), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_189])])).
fof(f5726, plain, (~ spl42_102 | spl42_106 | ~ spl42_180), inference(avatar_contradiction_clause, [], [f5725])).
fof(f5725, plain, ($false | (~ spl42_102 | spl42_106 | ~ spl42_180)), inference(subsumption_resolution, [], [f5722, f1094])).
fof(f1094, plain, (~ (e21 = op2(e21, e21)) | spl42_106), inference(avatar_component_clause, [], [f1093])).
fof(f1093, plain, (spl42_106 <=> (e21 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_106])])).
fof(f5722, plain, ((e21 = op2(e21, e21)) | (~ spl42_102 | ~ spl42_180)), inference(backward_demodulation, [], [f1626, f1078])).
fof(f1626, plain, ((op2(e21, e22) = op2(op2(e21, e22), e21)) | ~ spl42_180), inference(avatar_component_clause, [], [f1624])).
fof(f1624, plain, (spl42_180 <=> (op2(e21, e22) = op2(op2(e21, e22), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_180])])).
fof(f5710, plain, (~ spl42_121 | ~ spl42_122), inference(avatar_contradiction_clause, [], [f5709])).
fof(f5709, plain, ($false | (~ spl42_121 | ~ spl42_122)), inference(subsumption_resolution, [], [f5708, f306])).
fof(f306, plain, ~ (e20 = e21), inference(cnf_transformation, [], [f8])).
fof(f5708, plain, ((e20 = e21) | (~ spl42_121 | ~ spl42_122)), inference(forward_demodulation, [], [f1163, f1159])).
fof(f1159, plain, ((e20 = op2(e20, e21)) | ~ spl42_121), inference(avatar_component_clause, [], [f1157])).
fof(f1163, plain, ((e21 = op2(e20, e21)) | ~ spl42_122), inference(avatar_component_clause, [], [f1161])).
fof(f1161, plain, (spl42_122 <=> (e21 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_122])])).
fof(f5696, plain, (~ spl42_115 | ~ spl42_215), inference(avatar_split_clause, [], [f5687, f1845, f1131])).
fof(f1131, plain, (spl42_115 <=> (e22 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_115])])).
fof(f5687, plain, (~ (e22 = op2(e20, e23)) | ~ spl42_215), inference(backward_demodulation, [], [f1681, f1846])).
fof(f1846, plain, ((e22 = h1(e11)) | ~ spl42_215), inference(avatar_component_clause, [], [f1845])).
fof(f1681, plain, ~ (op2(e20, e23) = h1(e11)), inference(backward_demodulation, [], [f278, f536])).
fof(f278, plain, ~ (op2(e20, e20) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f5694, plain, (~ spl42_95 | ~ spl42_215), inference(avatar_split_clause, [], [f5685, f1845, f1046])).
fof(f5685, plain, (~ (e22 = op2(e22, e20)) | ~ spl42_215), inference(backward_demodulation, [], [f1677, f1846])).
fof(f1677, plain, ~ (op2(e22, e20) = h1(e11)), inference(backward_demodulation, [], [f253, f536])).
fof(f253, plain, ~ (op2(e20, e20) = op2(e22, e20)), inference(cnf_transformation, [], [f6])).
fof(f5675, plain, (~ spl42_103 | ~ spl42_39 | ~ spl42_192 | spl42_234), inference(avatar_split_clause, [], [f5674, f1948, f1727, f776, f1080])).
fof(f1080, plain, (spl42_103 <=> (e22 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_103])])).
fof(f776, plain, (spl42_39 <=> (e12 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_39])])).
fof(f1948, plain, (spl42_234 <=> (h4(op1(e11, e12)) = op2(h4(e11), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_234])])).
fof(f5674, plain, (~ (e22 = op2(e21, e22)) | (~ spl42_39 | ~ spl42_192 | spl42_234)), inference(forward_demodulation, [], [f5673, f1703])).
fof(f1703, plain, (e22 = h4(e12)), inference(forward_demodulation, [], [f549, f533])).
fof(f533, plain, (e22 = op2(e23, op2(e23, e23))), inference(cnf_transformation, [], [f13])).
fof(f13, plain, ((e22 = op2(e23, op2(e23, e23))) & (e21 = op2(e23, e23)) & (e20 = op2(e23, op2(e23, op2(e23, e23))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax13)).
fof(f549, plain, (op2(e23, op2(e23, e23)) = h4(e12)), inference(cnf_transformation, [], [f17])).
fof(f5673, plain, (~ (op2(e21, e22) = h4(e12)) | (~ spl42_39 | ~ spl42_192 | spl42_234)), inference(forward_demodulation, [], [f5672, f778])).
fof(f778, plain, ((e12 = op1(e11, e12)) | ~ spl42_39), inference(avatar_component_clause, [], [f776])).
fof(f5672, plain, (~ (op2(e21, e22) = h4(op1(e11, e12))) | (~ spl42_192 | spl42_234)), inference(forward_demodulation, [], [f1950, f1728])).
fof(f1950, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | spl42_234), inference(avatar_component_clause, [], [f1948])).
fof(f5670, plain, (~ spl42_120 | ~ spl42_56 | ~ spl42_196 | spl42_236), inference(avatar_split_clause, [], [f5669, f1956, f1748, f848, f1152])).
fof(f848, plain, (spl42_56 <=> (e13 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_56])])).
fof(f1956, plain, (spl42_236 <=> (h4(op1(e10, e12)) = op2(h4(e10), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_236])])).
fof(f5669, plain, (~ (e23 = op2(e20, e22)) | (~ spl42_56 | ~ spl42_196 | spl42_236)), inference(forward_demodulation, [], [f5668, f546])).
fof(f5668, plain, (~ (op2(e20, e22) = h4(e13)) | (~ spl42_56 | ~ spl42_196 | spl42_236)), inference(forward_demodulation, [], [f5667, f850])).
fof(f850, plain, ((e13 = op1(e10, e12)) | ~ spl42_56), inference(avatar_component_clause, [], [f848])).
fof(f5667, plain, (~ (op2(e20, e22) = h4(op1(e10, e12))) | (~ spl42_196 | spl42_236)), inference(forward_demodulation, [], [f1958, f1749])).
fof(f1958, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e22)) | spl42_236), inference(avatar_component_clause, [], [f1956])).
fof(f5654, plain, (~ spl42_92 | ~ spl42_256), inference(avatar_split_clause, [], [f5646, f2104, f1033])).
fof(f2104, plain, (spl42_256 <=> (e23 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_256])])).
fof(f5646, plain, (~ (e23 = op2(e22, e21)) | ~ spl42_256), inference(backward_demodulation, [], [f1698, f2105])).
fof(f2105, plain, ((e23 = h3(e11)) | ~ spl42_256), inference(avatar_component_clause, [], [f2104])).
fof(f1698, plain, ~ (op2(e22, e21) = h3(e11)), inference(backward_demodulation, [], [f291, f544])).
fof(f544, plain, (op2(e22, e22) = h3(e11)), inference(cnf_transformation, [], [f16])).
fof(f16, plain, ((op2(e22, op2(e22, e22)) = h3(e12)) & (op2(e22, e22) = h3(e11)) & (h3(e10) = op2(e22, op2(e22, op2(e22, e22)))) & (e22 = h3(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax16)).
fof(f291, plain, ~ (op2(e22, e21) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f5653, plain, (~ spl42_120 | ~ spl42_256), inference(avatar_split_clause, [], [f5643, f2104, f1152])).
fof(f5643, plain, (~ (e23 = op2(e20, e22)) | ~ spl42_256), inference(backward_demodulation, [], [f1694, f2105])).
fof(f1694, plain, ~ (op2(e20, e22) = h3(e11)), inference(backward_demodulation, [], [f265, f544])).
fof(f265, plain, ~ (op2(e20, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f5634, plain, (~ spl42_100 | ~ spl42_275), inference(avatar_split_clause, [], [f5624, f2218, f1067])).
fof(f1067, plain, (spl42_100 <=> (e23 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_100])])).
fof(f2218, plain, (spl42_275 <=> (e23 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_275])])).
fof(f5624, plain, (~ (e23 = op2(e21, e23)) | ~ spl42_275), inference(backward_demodulation, [], [f1690, f2219])).
fof(f2219, plain, ((e23 = h2(e11)) | ~ spl42_275), inference(avatar_component_clause, [], [f2218])).
fof(f1690, plain, ~ (op2(e21, e23) = h2(e11)), inference(backward_demodulation, [], [f286, f540])).
fof(f540, plain, (op2(e21, e21) = h2(e11)), inference(cnf_transformation, [], [f15])).
fof(f15, plain, ((op2(e21, op2(e21, e21)) = h2(e12)) & (op2(e21, e21) = h2(e11)) & (h2(e10) = op2(e21, op2(e21, op2(e21, e21)))) & (e21 = h2(e13))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax15)).
fof(f286, plain, ~ (op2(e21, e21) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f5633, plain, (~ spl42_92 | ~ spl42_275), inference(avatar_split_clause, [], [f5622, f2218, f1033])).
fof(f5622, plain, (~ (e23 = op2(e22, e21)) | ~ spl42_275), inference(backward_demodulation, [], [f1686, f2219])).
fof(f1686, plain, ~ (op2(e22, e21) = h2(e11)), inference(backward_demodulation, [], [f261, f540])).
fof(f261, plain, ~ (op2(e21, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f5591, plain, (~ spl42_91 | ~ spl42_75), inference(avatar_split_clause, [], [f3151, f961, f1029])).
fof(f1029, plain, (spl42_91 <=> (e22 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_91])])).
fof(f961, plain, (spl42_75 <=> (e22 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_75])])).
fof(f3151, plain, (~ (e22 = op2(e22, e21)) | ~ spl42_75), inference(forward_demodulation, [], [f263, f963])).
fof(f963, plain, ((e22 = op2(e23, e21)) | ~ spl42_75), inference(avatar_component_clause, [], [f961])).
fof(f263, plain, ~ (op2(e22, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f5587, plain, (~ spl42_52 | ~ spl42_56), inference(avatar_split_clause, [], [f5586, f848, f831])).
fof(f831, plain, (spl42_52 <=> (e13 = op1(e10, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_52])])).
fof(f5586, plain, (~ (e13 = op1(e10, e13)) | ~ spl42_56), inference(forward_demodulation, [], [f233, f850])).
fof(f233, plain, ~ (op1(e10, e12) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f5584, plain, (~ spl42_5 | ~ spl42_20 | ~ spl42_146), inference(avatar_contradiction_clause, [], [f5583])).
fof(f5583, plain, ($false | (~ spl42_5 | ~ spl42_20 | ~ spl42_146)), inference(subsumption_resolution, [], [f5582, f302])).
fof(f302, plain, ~ (e10 = e13), inference(cnf_transformation, [], [f7])).
fof(f7, plain, (~ (e12 = e13) & ~ (e11 = e13) & ~ (e11 = e12) & ~ (e10 = e13) & ~ (e10 = e12) & ~ (e10 = e11)), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax7)).
fof(f5582, plain, ((e10 = e13) | (~ spl42_5 | ~ spl42_20 | ~ spl42_146)), inference(forward_demodulation, [], [f5579, f634])).
fof(f634, plain, ((e10 = op1(e13, e12)) | ~ spl42_5), inference(avatar_component_clause, [], [f632])).
fof(f632, plain, (spl42_5 <=> (e10 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_5])])).
fof(f5579, plain, ((e13 = op1(e13, e12)) | (~ spl42_20 | ~ spl42_146)), inference(backward_demodulation, [], [f1389, f697])).
fof(f697, plain, ((e13 = op1(e12, e13)) | ~ spl42_20), inference(avatar_component_clause, [], [f695])).
fof(f5576, plain, (~ spl42_23 | spl42_150), inference(avatar_contradiction_clause, [], [f5575])).
fof(f5575, plain, ($false | (~ spl42_23 | spl42_150)), inference(subsumption_resolution, [], [f5571, f710])).
fof(f710, plain, ((e12 = op1(e12, e12)) | ~ spl42_23), inference(avatar_component_clause, [], [f708])).
fof(f5571, plain, (~ (e12 = op1(e12, e12)) | (~ spl42_23 | spl42_150)), inference(backward_demodulation, [], [f1405, f710])).
fof(f1405, plain, (~ (op1(e12, e12) = op1(op1(e12, e12), e12)) | spl42_150), inference(avatar_component_clause, [], [f1404])).
fof(f1404, plain, (spl42_150 <=> (op1(e12, e12) = op1(op1(e12, e12), e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_150])])).
fof(f5574, plain, (~ spl42_19 | ~ spl42_23), inference(avatar_split_clause, [], [f5569, f708, f691])).
fof(f691, plain, (spl42_19 <=> (e12 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_19])])).
fof(f5569, plain, (~ (e12 = op1(e12, e13)) | ~ spl42_23), inference(backward_demodulation, [], [f245, f710])).
fof(f245, plain, ~ (op1(e12, e12) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5561, plain, (~ spl42_19 | ~ spl42_35), inference(avatar_split_clause, [], [f5554, f759, f691])).
fof(f5554, plain, (~ (e12 = op1(e12, e13)) | ~ spl42_35), inference(backward_demodulation, [], [f225, f761])).
fof(f761, plain, ((e12 = op1(e11, e13)) | ~ spl42_35), inference(avatar_component_clause, [], [f759])).
fof(f5553, plain, (~ spl42_38 | ~ spl42_39), inference(avatar_contradiction_clause, [], [f5552])).
fof(f5552, plain, ($false | (~ spl42_38 | ~ spl42_39)), inference(subsumption_resolution, [], [f5551, f303])).
fof(f303, plain, ~ (e11 = e12), inference(cnf_transformation, [], [f7])).
fof(f5551, plain, ((e11 = e12) | (~ spl42_38 | ~ spl42_39)), inference(backward_demodulation, [], [f778, f774])).
fof(f774, plain, ((e11 = op1(e11, e12)) | ~ spl42_38), inference(avatar_component_clause, [], [f772])).
fof(f772, plain, (spl42_38 <=> (e11 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_38])])).
fof(f5543, plain, (~ spl42_36 | ~ spl42_44), inference(avatar_split_clause, [], [f5538, f797, f763])).
fof(f797, plain, (spl42_44 <=> (e13 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_44])])).
fof(f5538, plain, (~ (e13 = op1(e11, e13)) | ~ spl42_44), inference(backward_demodulation, [], [f238, f799])).
fof(f799, plain, ((e13 = op1(e11, e11)) | ~ spl42_44), inference(avatar_component_clause, [], [f797])).
fof(f238, plain, ~ (op1(e11, e11) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f5535, plain, (~ spl42_16 | ~ spl42_56 | spl42_148), inference(avatar_contradiction_clause, [], [f5534])).
fof(f5534, plain, ($false | (~ spl42_16 | ~ spl42_56 | spl42_148)), inference(subsumption_resolution, [], [f5528, f680])).
fof(f680, plain, ((e13 = op1(e13, e10)) | ~ spl42_16), inference(avatar_component_clause, [], [f678])).
fof(f678, plain, (spl42_16 <=> (e13 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_16])])).
fof(f5528, plain, (~ (e13 = op1(e13, e10)) | (~ spl42_56 | spl42_148)), inference(backward_demodulation, [], [f1397, f850])).
fof(f1397, plain, (~ (op1(e10, e12) = op1(op1(e10, e12), e10)) | spl42_148), inference(avatar_component_clause, [], [f1396])).
fof(f1396, plain, (spl42_148 <=> (op1(e10, e12) = op1(op1(e10, e12), e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_148])])).
fof(f5533, plain, (~ spl42_24 | ~ spl42_56), inference(avatar_split_clause, [], [f5527, f848, f712])).
fof(f712, plain, (spl42_24 <=> (e13 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_24])])).
fof(f5527, plain, (~ (e13 = op1(e12, e12)) | ~ spl42_56), inference(backward_demodulation, [], [f217, f850])).
fof(f217, plain, ~ (op1(e10, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f5523, plain, (~ spl42_54 | ~ spl42_58), inference(avatar_split_clause, [], [f5515, f857, f840])).
fof(f840, plain, (spl42_54 <=> (e11 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_54])])).
fof(f5515, plain, (~ (e11 = op1(e10, e12)) | ~ spl42_58), inference(backward_demodulation, [], [f231, f859])).
fof(f859, plain, ((e11 = op1(e10, e11)) | ~ spl42_58), inference(avatar_component_clause, [], [f857])).
fof(f231, plain, ~ (op1(e10, e11) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f5522, plain, (~ spl42_42 | ~ spl42_58), inference(avatar_split_clause, [], [f5514, f857, f789])).
fof(f789, plain, (spl42_42 <=> (e11 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_42])])).
fof(f5514, plain, (~ (e11 = op1(e11, e11)) | ~ spl42_58), inference(backward_demodulation, [], [f210, f859])).
fof(f210, plain, ~ (op1(e10, e11) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f5483, plain, (~ spl42_35 | ~ spl42_39), inference(avatar_split_clause, [], [f4202, f776, f759])).
fof(f4202, plain, (~ (e12 = op1(e11, e13)) | ~ spl42_39), inference(backward_demodulation, [], [f239, f778])).
fof(f239, plain, ~ (op1(e11, e12) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f5482, plain, (~ spl42_23 | ~ spl42_39), inference(avatar_split_clause, [], [f4201, f776, f708])).
fof(f4201, plain, (~ (e12 = op1(e12, e12)) | ~ spl42_39), inference(backward_demodulation, [], [f219, f778])).
fof(f219, plain, ~ (op1(e11, e12) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f5477, plain, (~ spl42_19 | spl42_23 | ~ spl42_146), inference(avatar_contradiction_clause, [], [f5476])).
fof(f5476, plain, ($false | (~ spl42_19 | spl42_23 | ~ spl42_146)), inference(subsumption_resolution, [], [f5475, f709])).
fof(f709, plain, (~ (e12 = op1(e12, e12)) | spl42_23), inference(avatar_component_clause, [], [f708])).
fof(f5475, plain, ((e12 = op1(e12, e12)) | (~ spl42_19 | ~ spl42_146)), inference(forward_demodulation, [], [f1389, f693])).
fof(f693, plain, ((e12 = op1(e12, e13)) | ~ spl42_19), inference(avatar_component_clause, [], [f691])).
fof(f5402, plain, (~ spl42_25 | ~ spl42_75 | ~ spl42_199 | ~ spl42_204 | ~ spl42_246 | ~ spl42_248), inference(avatar_contradiction_clause, [], [f5401])).
fof(f5401, plain, ($false | (~ spl42_25 | ~ spl42_75 | ~ spl42_199 | ~ spl42_204 | ~ spl42_246 | ~ spl42_248)), inference(subsumption_resolution, [], [f5400, f307])).
fof(f307, plain, ~ (e20 = e22), inference(cnf_transformation, [], [f8])).
fof(f5400, plain, ((e20 = e22) | (~ spl42_25 | ~ spl42_75 | ~ spl42_199 | ~ spl42_204 | ~ spl42_246 | ~ spl42_248)), inference(forward_demodulation, [], [f5398, f1790])).
fof(f1790, plain, ((e20 = h3(e10)) | ~ spl42_204), inference(avatar_component_clause, [], [f1789])).
fof(f1789, plain, (spl42_204 <=> (e20 = h3(e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_204])])).
fof(f5398, plain, ((e22 = h3(e10)) | (~ spl42_25 | ~ spl42_75 | ~ spl42_199 | ~ spl42_246 | ~ spl42_248)), inference(backward_demodulation, [], [f5223, f719])).
fof(f719, plain, ((e10 = op1(e12, e11)) | ~ spl42_25), inference(avatar_component_clause, [], [f717])).
fof(f5223, plain, ((e22 = h3(op1(e12, e11))) | (~ spl42_75 | ~ spl42_199 | ~ spl42_246 | ~ spl42_248)), inference(forward_demodulation, [], [f5214, f963])).
fof(f5214, plain, ((op2(e23, e21) = h3(op1(e12, e11))) | (~ spl42_199 | ~ spl42_246 | ~ spl42_248)), inference(backward_demodulation, [], [f5176, f2061])).
fof(f2061, plain, ((e23 = h3(e12)) | ~ spl42_248), inference(avatar_component_clause, [], [f2060])).
fof(f2060, plain, (spl42_248 <=> (e23 = h3(e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_248])])).
fof(f5176, plain, ((h3(op1(e12, e11)) = op2(h3(e12), e21)) | (~ spl42_199 | ~ spl42_246)), inference(forward_demodulation, [], [f2053, f1765])).
fof(f1765, plain, ((e21 = h3(e11)) | ~ spl42_199), inference(avatar_component_clause, [], [f1764])).
fof(f1764, plain, (spl42_199 <=> (e21 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_199])])).
fof(f2053, plain, ((h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ spl42_246), inference(avatar_component_clause, [], [f2052])).
fof(f2052, plain, (spl42_246 <=> (h3(op1(e12, e11)) = op2(h3(e12), h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_246])])).
fof(f5399, plain, (~ spl42_17 | ~ spl42_25), inference(avatar_split_clause, [], [f5392, f717, f683])).
fof(f5392, plain, (~ (e10 = op1(e12, e13)) | ~ spl42_25), inference(backward_demodulation, [], [f244, f719])).
fof(f5382, plain, (~ spl42_22 | ~ spl42_30), inference(avatar_split_clause, [], [f5374, f738, f704])).
fof(f704, plain, (spl42_22 <=> (e11 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_22])])).
fof(f5374, plain, (~ (e11 = op1(e12, e12)) | ~ spl42_30), inference(backward_demodulation, [], [f241, f740])).
fof(f740, plain, ((e11 = op1(e12, e10)) | ~ spl42_30), inference(avatar_component_clause, [], [f738])).
fof(f5368, plain, (~ spl42_17 | ~ spl42_49), inference(avatar_split_clause, [], [f5360, f819, f683])).
fof(f5360, plain, (~ (e10 = op1(e12, e13)) | ~ spl42_49), inference(backward_demodulation, [], [f223, f821])).
fof(f821, plain, ((e10 = op1(e10, e13)) | ~ spl42_49), inference(avatar_component_clause, [], [f819])).
fof(f223, plain, ~ (op1(e10, e13) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f5353, plain, (~ spl42_22 | ~ spl42_54), inference(avatar_split_clause, [], [f5345, f840, f704])).
fof(f5345, plain, (~ (e11 = op1(e12, e12)) | ~ spl42_54), inference(backward_demodulation, [], [f217, f842])).
fof(f842, plain, ((e11 = op1(e10, e12)) | ~ spl42_54), inference(avatar_component_clause, [], [f840])).
fof(f5344, plain, (~ spl42_60 | ~ spl42_121 | ~ spl42_199 | ~ spl42_204 | ~ spl42_219 | ~ spl42_240), inference(avatar_contradiction_clause, [], [f5343])).
fof(f5343, plain, ($false | (~ spl42_60 | ~ spl42_121 | ~ spl42_199 | ~ spl42_204 | ~ spl42_219 | ~ spl42_240)), inference(subsumption_resolution, [], [f5342, f307])).
fof(f5342, plain, ((e20 = e22) | (~ spl42_60 | ~ spl42_121 | ~ spl42_199 | ~ spl42_204 | ~ spl42_219 | ~ spl42_240)), inference(backward_demodulation, [], [f542, f5338])).
fof(f5338, plain, ((e20 = h3(e13)) | (~ spl42_60 | ~ spl42_121 | ~ spl42_199 | ~ spl42_204 | ~ spl42_219 | ~ spl42_240)), inference(backward_demodulation, [], [f5289, f867])).
fof(f867, plain, ((e13 = op1(e10, e11)) | ~ spl42_60), inference(avatar_component_clause, [], [f865])).
fof(f865, plain, (spl42_60 <=> (e13 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_60])])).
fof(f5289, plain, ((e20 = h3(op1(e10, e11))) | (~ spl42_121 | ~ spl42_199 | ~ spl42_204 | ~ spl42_219 | ~ spl42_240)), inference(backward_demodulation, [], [f5256, f5279])).
fof(f5279, plain, ((e20 = h1(e12)) | (~ spl42_121 | ~ spl42_219)), inference(backward_demodulation, [], [f5202, f1159])).
fof(f5202, plain, ((op2(e20, e21) = h1(e12)) | ~ spl42_219), inference(forward_demodulation, [], [f1682, f1866])).
fof(f1866, plain, ((e21 = h1(e11)) | ~ spl42_219), inference(avatar_component_clause, [], [f1865])).
fof(f1865, plain, (spl42_219 <=> (e21 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_219])])).
fof(f1682, plain, (h1(e12) = op2(e20, h1(e11))), inference(backward_demodulation, [], [f537, f536])).
fof(f537, plain, (op2(e20, op2(e20, e20)) = h1(e12)), inference(cnf_transformation, [], [f14])).
fof(f5256, plain, ((h1(e12) = h3(op1(e10, e11))) | (~ spl42_199 | ~ spl42_204 | ~ spl42_219 | ~ spl42_240)), inference(forward_demodulation, [], [f5248, f5202])).
fof(f5248, plain, ((op2(e20, e21) = h3(op1(e10, e11))) | (~ spl42_199 | ~ spl42_204 | ~ spl42_240)), inference(backward_demodulation, [], [f5183, f1790])).
fof(f5183, plain, ((h3(op1(e10, e11)) = op2(h3(e10), e21)) | (~ spl42_199 | ~ spl42_240)), inference(forward_demodulation, [], [f2029, f1765])).
fof(f2029, plain, ((h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ spl42_240), inference(avatar_component_clause, [], [f2028])).
fof(f2028, plain, (spl42_240 <=> (h3(op1(e10, e11)) = op2(h3(e10), h3(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_240])])).
fof(f542, plain, (e22 = h3(e13)), inference(cnf_transformation, [], [f16])).
fof(f5339, plain, (~ spl42_28 | ~ spl42_60), inference(avatar_split_clause, [], [f5332, f865, f729])).
fof(f5332, plain, (~ (e13 = op1(e12, e11)) | ~ spl42_60), inference(backward_demodulation, [], [f211, f867])).
fof(f5328, plain, (~ spl42_51 | ~ spl42_63), inference(avatar_split_clause, [], [f5321, f878, f827])).
fof(f878, plain, (spl42_63 <=> (op1(e10, e10) = e12)), introduced(avatar_definition, [new_symbols(naming, [spl42_63])])).
fof(f5321, plain, (~ (e12 = op1(e10, e13)) | ~ spl42_63), inference(backward_demodulation, [], [f230, f880])).
fof(f880, plain, ((op1(e10, e10) = e12) | ~ spl42_63), inference(avatar_component_clause, [], [f878])).
fof(f230, plain, ~ (op1(e10, e10) = op1(e10, e13)), inference(cnf_transformation, [], [f5])).
fof(f5307, plain, (~ spl42_109 | ~ spl42_110), inference(avatar_contradiction_clause, [], [f5306])).
fof(f5306, plain, ($false | (~ spl42_109 | ~ spl42_110)), inference(subsumption_resolution, [], [f5305, f306])).
fof(f5305, plain, ((e20 = e21) | (~ spl42_109 | ~ spl42_110)), inference(forward_demodulation, [], [f1112, f1108])).
fof(f1108, plain, ((e20 = op2(e21, e20)) | ~ spl42_109), inference(avatar_component_clause, [], [f1106])).
fof(f1112, plain, ((e21 = op2(e21, e20)) | ~ spl42_110), inference(avatar_component_clause, [], [f1110])).
fof(f5304, plain, (~ spl42_99 | ~ spl42_115), inference(avatar_split_clause, [], [f5300, f1131, f1063])).
fof(f1063, plain, (spl42_99 <=> (e22 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_99])])).
fof(f5300, plain, (~ (e22 = op2(e21, e23)) | ~ spl42_115), inference(backward_demodulation, [], [f270, f1133])).
fof(f1133, plain, ((e22 = op2(e20, e23)) | ~ spl42_115), inference(avatar_component_clause, [], [f1131])).
fof(f270, plain, ~ (op2(e20, e23) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f5275, plain, (~ spl42_11 | ~ spl42_36 | ~ spl42_145), inference(avatar_contradiction_clause, [], [f5274])).
fof(f5274, plain, ($false | (~ spl42_11 | ~ spl42_36 | ~ spl42_145)), inference(subsumption_resolution, [], [f5273, f305])).
fof(f305, plain, ~ (e12 = e13), inference(cnf_transformation, [], [f7])).
fof(f5273, plain, ((e12 = e13) | (~ spl42_11 | ~ spl42_36 | ~ spl42_145)), inference(forward_demodulation, [], [f5272, f659])).
fof(f659, plain, ((e12 = op1(e13, e11)) | ~ spl42_11), inference(avatar_component_clause, [], [f657])).
fof(f657, plain, (spl42_11 <=> (e12 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_11])])).
fof(f5272, plain, ((e13 = op1(e13, e11)) | (~ spl42_36 | ~ spl42_145)), inference(forward_demodulation, [], [f1385, f765])).
fof(f1385, plain, ((op1(e11, e13) = op1(op1(e11, e13), e11)) | ~ spl42_145), inference(avatar_component_clause, [], [f1383])).
fof(f1383, plain, (spl42_145 <=> (op1(e11, e13) = op1(op1(e11, e13), e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_145])])).
fof(f5271, plain, (spl42_49 | ~ spl42_5 | ~ spl42_151), inference(avatar_split_clause, [], [f5270, f1408, f632, f819])).
fof(f1408, plain, (spl42_151 <=> (op1(e13, e12) = op1(op1(e13, e12), e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_151])])).
fof(f5270, plain, ((e10 = op1(e10, e13)) | (~ spl42_5 | ~ spl42_151)), inference(forward_demodulation, [], [f1410, f634])).
fof(f1410, plain, ((op1(e13, e12) = op1(op1(e13, e12), e13)) | ~ spl42_151), inference(avatar_component_clause, [], [f1408])).
fof(f5263, plain, (spl42_121 | ~ spl42_109 | ~ spl42_188), inference(avatar_split_clause, [], [f5262, f1658, f1106, f1157])).
fof(f1658, plain, (spl42_188 <=> (op2(e21, e20) = op2(op2(e21, e20), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_188])])).
fof(f5262, plain, ((e20 = op2(e20, e21)) | (~ spl42_109 | ~ spl42_188)), inference(forward_demodulation, [], [f1660, f1108])).
fof(f1660, plain, ((op2(e21, e20) = op2(op2(e21, e20), e21)) | ~ spl42_188), inference(avatar_component_clause, [], [f1658])).
fof(f5170, plain, (~ spl42_86 | ~ spl42_87), inference(avatar_contradiction_clause, [], [f5169])).
fof(f5169, plain, ($false | (~ spl42_86 | ~ spl42_87)), inference(subsumption_resolution, [], [f5168, f309])).
fof(f309, plain, ~ (e21 = e22), inference(cnf_transformation, [], [f8])).
fof(f5168, plain, ((e21 = e22) | (~ spl42_86 | ~ spl42_87)), inference(backward_demodulation, [], [f1014, f1010])).
fof(f1010, plain, ((e21 = op2(e22, e22)) | ~ spl42_86), inference(avatar_component_clause, [], [f1008])).
fof(f1008, plain, (spl42_86 <=> (e21 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_86])])).
fof(f5161, plain, (~ spl42_83 | ~ spl42_99), inference(avatar_split_clause, [], [f5159, f1063, f995])).
fof(f995, plain, (spl42_83 <=> (e22 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_83])])).
fof(f5159, plain, (~ (e22 = op2(e22, e23)) | ~ spl42_99), inference(backward_demodulation, [], [f273, f1065])).
fof(f1065, plain, ((e22 = op2(e21, e23)) | ~ spl42_99), inference(avatar_component_clause, [], [f1063])).
fof(f273, plain, ~ (op2(e21, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f5153, plain, (spl42_95 | ~ spl42_119 | ~ spl42_179), inference(avatar_split_clause, [], [f5150, f1620, f1148, f1046])).
fof(f5150, plain, ((e22 = op2(e22, e20)) | (~ spl42_119 | ~ spl42_179)), inference(backward_demodulation, [], [f1622, f1150])).
fof(f5152, plain, (~ spl42_103 | ~ spl42_119), inference(avatar_split_clause, [], [f5148, f1148, f1080])).
fof(f5148, plain, (~ (e22 = op2(e21, e22)) | ~ spl42_119), inference(backward_demodulation, [], [f264, f1150])).
fof(f5145, plain, (~ spl42_66 | spl42_98 | ~ spl42_178), inference(avatar_contradiction_clause, [], [f5144])).
fof(f5144, plain, ($false | (~ spl42_66 | spl42_98 | ~ spl42_178)), inference(subsumption_resolution, [], [f5143, f1060])).
fof(f1060, plain, (~ (e21 = op2(e21, e23)) | spl42_98), inference(avatar_component_clause, [], [f1059])).
fof(f1059, plain, (spl42_98 <=> (e21 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_98])])).
fof(f5143, plain, ((e21 = op2(e21, e23)) | (~ spl42_66 | ~ spl42_178)), inference(forward_demodulation, [], [f1617, f925])).
fof(f925, plain, ((e21 = op2(e23, e23)) | ~ spl42_66), inference(avatar_component_clause, [], [f923])).
fof(f923, plain, (spl42_66 <=> (e21 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_66])])).
fof(f1617, plain, ((op2(e23, e23) = op2(op2(e23, e23), e23)) | ~ spl42_178), inference(avatar_component_clause, [], [f1615])).
fof(f1615, plain, (spl42_178 <=> (op2(e23, e23) = op2(op2(e23, e23), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_178])])).
fof(f5103, plain, (~ spl42_118 | ~ spl42_219), inference(avatar_split_clause, [], [f5097, f1865, f1144])).
fof(f5097, plain, (~ (e21 = op2(e20, e22)) | ~ spl42_219), inference(backward_demodulation, [], [f1680, f1866])).
fof(f5102, plain, (~ spl42_94 | ~ spl42_219), inference(avatar_split_clause, [], [f5096, f1865, f1042])).
fof(f5096, plain, (~ (e21 = op2(e22, e20)) | ~ spl42_219), inference(backward_demodulation, [], [f1677, f1866])).
fof(f5083, plain, (~ spl42_100 | ~ spl42_36 | ~ spl42_192 | spl42_233), inference(avatar_split_clause, [], [f5082, f1944, f1727, f763, f1067])).
fof(f1944, plain, (spl42_233 <=> (h4(op1(e11, e13)) = op2(h4(e11), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_233])])).
fof(f5082, plain, (~ (e23 = op2(e21, e23)) | (~ spl42_36 | ~ spl42_192 | spl42_233)), inference(forward_demodulation, [], [f5081, f546])).
fof(f5081, plain, (~ (op2(e21, e23) = h4(e13)) | (~ spl42_36 | ~ spl42_192 | spl42_233)), inference(forward_demodulation, [], [f5080, f765])).
fof(f5080, plain, (~ (op2(e21, e23) = h4(op1(e11, e13))) | (~ spl42_192 | spl42_233)), inference(forward_demodulation, [], [f1946, f1728])).
fof(f1946, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | spl42_233), inference(avatar_component_clause, [], [f1944])).
fof(f5027, plain, (~ spl42_94 | spl42_102 | ~ spl42_189), inference(avatar_contradiction_clause, [], [f5026])).
fof(f5026, plain, ($false | (~ spl42_94 | spl42_102 | ~ spl42_189)), inference(subsumption_resolution, [], [f5025, f1077])).
fof(f1077, plain, (~ (e21 = op2(e21, e22)) | spl42_102), inference(avatar_component_clause, [], [f1076])).
fof(f5025, plain, ((e21 = op2(e21, e22)) | (~ spl42_94 | ~ spl42_189)), inference(forward_demodulation, [], [f1664, f1044])).
fof(f1664, plain, ((op2(e22, e20) = op2(op2(e22, e20), e22)) | ~ spl42_189), inference(avatar_component_clause, [], [f1662])).
fof(f4964, plain, (~ spl42_97 | ~ spl42_109), inference(avatar_split_clause, [], [f4957, f1106, f1055])).
fof(f1055, plain, (spl42_97 <=> (e20 = op2(e21, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_97])])).
fof(f4957, plain, (~ (e20 = op2(e21, e23)) | ~ spl42_109), inference(backward_demodulation, [], [f284, f1108])).
fof(f284, plain, ~ (op2(e21, e20) = op2(e21, e23)), inference(cnf_transformation, [], [f6])).
fof(f4962, plain, (~ spl42_93 | ~ spl42_109), inference(avatar_split_clause, [], [f4955, f1106, f1038])).
fof(f1038, plain, (spl42_93 <=> (e20 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_93])])).
fof(f4955, plain, (~ (e20 = op2(e22, e20)) | ~ spl42_109), inference(backward_demodulation, [], [f255, f1108])).
fof(f4942, plain, (spl42_95 | ~ spl42_127 | ~ spl42_187), inference(avatar_split_clause, [], [f4940, f1654, f1182, f1046])).
fof(f1182, plain, (spl42_127 <=> (op2(e20, e20) = e22)), introduced(avatar_definition, [new_symbols(naming, [spl42_127])])).
fof(f1654, plain, (spl42_187 <=> (op2(e20, e20) = op2(op2(e20, e20), e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_187])])).
fof(f4940, plain, ((e22 = op2(e22, e20)) | (~ spl42_127 | ~ spl42_187)), inference(backward_demodulation, [], [f1656, f1184])).
fof(f1184, plain, ((op2(e20, e20) = e22) | ~ spl42_127), inference(avatar_component_clause, [], [f1182])).
fof(f1656, plain, ((op2(e20, e20) = op2(op2(e20, e20), e20)) | ~ spl42_187), inference(avatar_component_clause, [], [f1654])).
fof(f4939, plain, (spl42_113 | ~ spl42_69 | ~ spl42_182), inference(avatar_split_clause, [], [f4938, f1632, f936, f1123])).
fof(f1123, plain, (spl42_113 <=> (e20 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_113])])).
fof(f1632, plain, (spl42_182 <=> (op2(e23, e22) = op2(op2(e23, e22), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_182])])).
fof(f4938, plain, ((e20 = op2(e20, e23)) | (~ spl42_69 | ~ spl42_182)), inference(forward_demodulation, [], [f1634, f938])).
fof(f1634, plain, ((op2(e23, e22) = op2(op2(e23, e22), e23)) | ~ spl42_182), inference(avatar_component_clause, [], [f1632])).
fof(f4852, plain, (~ spl42_199 | ~ spl42_22 | ~ spl42_192 | spl42_230), inference(avatar_split_clause, [], [f4851, f1932, f1727, f704, f1764])).
fof(f1932, plain, (spl42_230 <=> (h3(e11) = h4(op1(e12, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_230])])).
fof(f4851, plain, (~ (e21 = h3(e11)) | (~ spl42_22 | ~ spl42_192 | spl42_230)), inference(forward_demodulation, [], [f4850, f1728])).
fof(f4850, plain, (~ (h3(e11) = h4(e11)) | (~ spl42_22 | spl42_230)), inference(forward_demodulation, [], [f1934, f706])).
fof(f706, plain, ((e11 = op1(e12, e12)) | ~ spl42_22), inference(avatar_component_clause, [], [f704])).
fof(f1934, plain, (~ (h3(e11) = h4(op1(e12, e12))) | spl42_230), inference(avatar_component_clause, [], [f1932])).
fof(f4796, plain, (~ spl42_66 | ~ spl42_68), inference(avatar_contradiction_clause, [], [f4795])).
fof(f4795, plain, ($false | (~ spl42_66 | ~ spl42_68)), inference(subsumption_resolution, [], [f4794, f310])).
fof(f310, plain, ~ (e21 = e23), inference(cnf_transformation, [], [f8])).
fof(f4794, plain, ((e21 = e23) | (~ spl42_66 | ~ spl42_68)), inference(forward_demodulation, [], [f933, f925])).
fof(f933, plain, ((e23 = op2(e23, e23)) | ~ spl42_68), inference(avatar_component_clause, [], [f931])).
fof(f931, plain, (spl42_68 <=> (e23 = op2(e23, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_68])])).
fof(f4787, plain, (~ spl42_69 | ~ spl42_84 | ~ spl42_177), inference(avatar_contradiction_clause, [], [f4786])).
fof(f4786, plain, ($false | (~ spl42_69 | ~ spl42_84 | ~ spl42_177)), inference(subsumption_resolution, [], [f4785, f308])).
fof(f4785, plain, ((e20 = e23) | (~ spl42_69 | ~ spl42_84 | ~ spl42_177)), inference(forward_demodulation, [], [f4784, f938])).
fof(f4784, plain, ((e23 = op2(e23, e22)) | (~ spl42_84 | ~ spl42_177)), inference(backward_demodulation, [], [f1613, f1001])).
fof(f1001, plain, ((e23 = op2(e22, e23)) | ~ spl42_84), inference(avatar_component_clause, [], [f999])).
fof(f999, plain, (spl42_84 <=> (e23 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_84])])).
fof(f1613, plain, ((op2(e22, e23) = op2(op2(e22, e23), e22)) | ~ spl42_177), inference(avatar_component_clause, [], [f1611])).
fof(f1611, plain, (spl42_177 <=> (op2(e22, e23) = op2(op2(e22, e23), e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_177])])).
fof(f4779, plain, (~ spl42_93 | ~ spl42_95), inference(avatar_contradiction_clause, [], [f4778])).
fof(f4778, plain, ($false | (~ spl42_93 | ~ spl42_95)), inference(subsumption_resolution, [], [f4777, f307])).
fof(f4777, plain, ((e20 = e22) | (~ spl42_93 | ~ spl42_95)), inference(backward_demodulation, [], [f1048, f1040])).
fof(f1040, plain, ((e20 = op2(e22, e20)) | ~ spl42_93), inference(avatar_component_clause, [], [f1038])).
fof(f1048, plain, ((e22 = op2(e22, e20)) | ~ spl42_95), inference(avatar_component_clause, [], [f1046])).
fof(f4653, plain, (~ spl42_77 | ~ spl42_196), inference(avatar_split_clause, [], [f4652, f1748, f970])).
fof(f970, plain, (spl42_77 <=> (e20 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_77])])).
fof(f4652, plain, (~ (e20 = op2(e23, e20)) | ~ spl42_196), inference(forward_demodulation, [], [f1716, f1749])).
fof(f1716, plain, ~ (op2(e23, e20) = h4(e10)), inference(backward_demodulation, [], [f295, f1712])).
fof(f1712, plain, (op2(e23, e22) = h4(e10)), inference(forward_demodulation, [], [f1711, f1710])).
fof(f1710, plain, (e22 = op2(e23, h4(e11))), inference(backward_demodulation, [], [f533, f548])).
fof(f1711, plain, (h4(e10) = op2(e23, op2(e23, h4(e11)))), inference(forward_demodulation, [], [f547, f548])).
fof(f547, plain, (op2(e23, op2(e23, op2(e23, e23))) = h4(e10)), inference(cnf_transformation, [], [f17])).
fof(f295, plain, ~ (op2(e23, e20) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f4641, plain, (~ spl42_113 | ~ spl42_115), inference(avatar_contradiction_clause, [], [f4640])).
fof(f4640, plain, ($false | (~ spl42_113 | ~ spl42_115)), inference(subsumption_resolution, [], [f4639, f307])).
fof(f4639, plain, ((e20 = e22) | (~ spl42_113 | ~ spl42_115)), inference(backward_demodulation, [], [f1133, f1125])).
fof(f1125, plain, ((e20 = op2(e20, e23)) | ~ spl42_113), inference(avatar_component_clause, [], [f1123])).
fof(f4620, plain, (~ spl42_81 | ~ spl42_17 | ~ spl42_196 | spl42_229), inference(avatar_split_clause, [], [f4619, f1928, f1748, f683, f987])).
fof(f987, plain, (spl42_81 <=> (e20 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_81])])).
fof(f1928, plain, (spl42_229 <=> (op2(e22, e23) = h4(op1(e12, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_229])])).
fof(f4619, plain, (~ (e20 = op2(e22, e23)) | (~ spl42_17 | ~ spl42_196 | spl42_229)), inference(forward_demodulation, [], [f4618, f1749])).
fof(f4618, plain, (~ (op2(e22, e23) = h4(e10)) | (~ spl42_17 | spl42_229)), inference(forward_demodulation, [], [f1930, f685])).
fof(f1930, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | spl42_229), inference(avatar_component_clause, [], [f1928])).
fof(f4530, plain, (~ spl42_82 | ~ spl42_192), inference(avatar_split_clause, [], [f3689, f1727, f991])).
fof(f991, plain, (spl42_82 <=> (e21 = op2(e22, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_82])])).
fof(f3689, plain, (~ (e21 = op2(e22, e23)) | ~ spl42_192), inference(forward_demodulation, [], [f1706, f1728])).
fof(f1706, plain, ~ (op2(e22, e23) = h4(e11)), inference(backward_demodulation, [], [f275, f548])).
fof(f275, plain, ~ (op2(e22, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f4526, plain, (~ spl42_51 | ~ spl42_115 | ~ spl42_196 | spl42_235), inference(avatar_contradiction_clause, [], [f4525])).
fof(f4525, plain, ($false | (~ spl42_51 | ~ spl42_115 | ~ spl42_196 | spl42_235)), inference(subsumption_resolution, [], [f4524, f1133])).
fof(f4524, plain, (~ (e22 = op2(e20, e23)) | (~ spl42_51 | ~ spl42_196 | spl42_235)), inference(forward_demodulation, [], [f4523, f1703])).
fof(f4523, plain, (~ (op2(e20, e23) = h4(e12)) | (~ spl42_51 | ~ spl42_196 | spl42_235)), inference(forward_demodulation, [], [f4522, f829])).
fof(f4522, plain, (~ (op2(e20, e23) = h4(op1(e10, e13))) | (~ spl42_196 | spl42_235)), inference(forward_demodulation, [], [f1954, f1749])).
fof(f1954, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | spl42_235), inference(avatar_component_clause, [], [f1952])).
fof(f1952, plain, (spl42_235 <=> (h4(op1(e10, e13)) = op2(h4(e10), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_235])])).
fof(f4488, plain, (~ spl42_31 | ~ spl42_95 | ~ spl42_196 | spl42_232), inference(avatar_contradiction_clause, [], [f4487])).
fof(f4487, plain, ($false | (~ spl42_31 | ~ spl42_95 | ~ spl42_196 | spl42_232)), inference(subsumption_resolution, [], [f4486, f1048])).
fof(f4486, plain, (~ (e22 = op2(e22, e20)) | (~ spl42_31 | ~ spl42_196 | spl42_232)), inference(forward_demodulation, [], [f4485, f1703])).
fof(f4485, plain, (~ (op2(e22, e20) = h4(e12)) | (~ spl42_31 | ~ spl42_196 | spl42_232)), inference(forward_demodulation, [], [f4484, f744])).
fof(f4484, plain, (~ (op2(e22, e20) = h4(op1(e12, e10))) | (~ spl42_196 | spl42_232)), inference(forward_demodulation, [], [f1942, f1749])).
fof(f1942, plain, (~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | spl42_232), inference(avatar_component_clause, [], [f1940])).
fof(f1940, plain, (spl42_232 <=> (h4(op1(e12, e10)) = op2(e22, h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_232])])).
fof(f4392, plain, (~ spl42_5 | spl42_226), inference(avatar_contradiction_clause, [], [f4391])).
fof(f4391, plain, ($false | (~ spl42_5 | spl42_226)), inference(trivial_inequality_removal, [], [f4390])).
fof(f4390, plain, (~ (h4(e10) = h4(e10)) | (~ spl42_5 | spl42_226)), inference(forward_demodulation, [], [f1918, f634])).
fof(f1918, plain, (~ (h4(e10) = h4(op1(e13, e12))) | spl42_226), inference(avatar_component_clause, [], [f1916])).
fof(f1916, plain, (spl42_226 <=> (h4(e10) = h4(op1(e13, e12)))), introduced(avatar_definition, [new_symbols(naming, [spl42_226])])).
fof(f4370, plain, (~ spl42_16 | ~ spl42_80 | ~ spl42_196 | spl42_228), inference(avatar_contradiction_clause, [], [f4369])).
fof(f4369, plain, ($false | (~ spl42_16 | ~ spl42_80 | ~ spl42_196 | spl42_228)), inference(subsumption_resolution, [], [f4368, f984])).
fof(f984, plain, ((e23 = op2(e23, e20)) | ~ spl42_80), inference(avatar_component_clause, [], [f982])).
fof(f982, plain, (spl42_80 <=> (e23 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_80])])).
fof(f4368, plain, (~ (e23 = op2(e23, e20)) | (~ spl42_16 | ~ spl42_196 | spl42_228)), inference(forward_demodulation, [], [f4367, f546])).
fof(f4367, plain, (~ (op2(e23, e20) = h4(e13)) | (~ spl42_16 | ~ spl42_196 | spl42_228)), inference(forward_demodulation, [], [f4366, f680])).
fof(f4366, plain, (~ (op2(e23, e20) = h4(op1(e13, e10))) | (~ spl42_196 | spl42_228)), inference(forward_demodulation, [], [f1926, f1749])).
fof(f1926, plain, (~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | spl42_228), inference(avatar_component_clause, [], [f1924])).
fof(f1924, plain, (spl42_228 <=> (h4(op1(e13, e10)) = op2(e23, h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_228])])).
fof(f4361, plain, (~ spl42_28 | ~ spl42_75 | ~ spl42_199 | spl42_246 | ~ spl42_248), inference(avatar_contradiction_clause, [], [f4360])).
fof(f4360, plain, ($false | (~ spl42_28 | ~ spl42_75 | ~ spl42_199 | spl42_246 | ~ spl42_248)), inference(subsumption_resolution, [], [f4359, f963])).
fof(f4359, plain, (~ (e22 = op2(e23, e21)) | (~ spl42_28 | ~ spl42_199 | spl42_246 | ~ spl42_248)), inference(forward_demodulation, [], [f4358, f542])).
fof(f4358, plain, (~ (op2(e23, e21) = h3(e13)) | (~ spl42_28 | ~ spl42_199 | spl42_246 | ~ spl42_248)), inference(forward_demodulation, [], [f4357, f731])).
fof(f4357, plain, (~ (op2(e23, e21) = h3(op1(e12, e11))) | (~ spl42_199 | spl42_246 | ~ spl42_248)), inference(forward_demodulation, [], [f4356, f2061])).
fof(f4356, plain, (~ (h3(op1(e12, e11)) = op2(h3(e12), e21)) | (~ spl42_199 | spl42_246)), inference(forward_demodulation, [], [f2054, f1765])).
fof(f2054, plain, (~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | spl42_246), inference(avatar_component_clause, [], [f2052])).
fof(f4346, plain, (~ spl42_42 | ~ spl42_106 | ~ spl42_192 | spl42_224), inference(avatar_contradiction_clause, [], [f4345])).
fof(f4345, plain, ($false | (~ spl42_42 | ~ spl42_106 | ~ spl42_192 | spl42_224)), inference(subsumption_resolution, [], [f4344, f1095])).
fof(f1095, plain, ((e21 = op2(e21, e21)) | ~ spl42_106), inference(avatar_component_clause, [], [f1093])).
fof(f4344, plain, (~ (e21 = op2(e21, e21)) | (~ spl42_42 | ~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f4343, f1728])).
fof(f4343, plain, (~ (op2(e21, e21) = h4(e11)) | (~ spl42_42 | ~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f4342, f791])).
fof(f791, plain, ((e11 = op1(e11, e11)) | ~ spl42_42), inference(avatar_component_clause, [], [f789])).
fof(f4342, plain, (~ (op2(e21, e21) = h4(op1(e11, e11))) | (~ spl42_192 | spl42_224)), inference(forward_demodulation, [], [f1910, f1728])).
fof(f1910, plain, (~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | spl42_224), inference(avatar_component_clause, [], [f1908])).
fof(f1908, plain, (spl42_224 <=> (h4(op1(e11, e11)) = op2(h4(e11), h4(e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_224])])).
fof(f4325, plain, (~ spl42_11 | spl42_227), inference(avatar_contradiction_clause, [], [f4324])).
fof(f4324, plain, ($false | (~ spl42_11 | spl42_227)), inference(subsumption_resolution, [], [f4323, f1703])).
fof(f4323, plain, (~ (e22 = h4(e12)) | (~ spl42_11 | spl42_227)), inference(forward_demodulation, [], [f1922, f659])).
fof(f1922, plain, (~ (e22 = h4(op1(e13, e11))) | spl42_227), inference(avatar_component_clause, [], [f1920])).
fof(f1920, plain, (spl42_227 <=> (e22 = h4(op1(e13, e11)))), introduced(avatar_definition, [new_symbols(naming, [spl42_227])])).
fof(f4298, plain, (~ spl42_2 | spl42_225), inference(avatar_contradiction_clause, [], [f4297])).
fof(f4297, plain, ($false | (~ spl42_2 | spl42_225)), inference(trivial_inequality_removal, [], [f4296])).
fof(f4296, plain, (~ (h4(e11) = h4(e11)) | (~ spl42_2 | spl42_225)), inference(forward_demodulation, [], [f1914, f621])).
fof(f621, plain, ((e11 = op1(e13, e13)) | ~ spl42_2), inference(avatar_component_clause, [], [f619])).
fof(f619, plain, (spl42_2 <=> (e11 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_2])])).
fof(f1914, plain, (~ (h4(e11) = h4(op1(e13, e13))) | spl42_225), inference(avatar_component_clause, [], [f1912])).
fof(f1912, plain, (spl42_225 <=> (h4(e11) = h4(op1(e13, e13)))), introduced(avatar_definition, [new_symbols(naming, [spl42_225])])).
fof(f4292, plain, (~ spl42_57 | ~ spl42_121 | ~ spl42_199 | ~ spl42_204 | spl42_240), inference(avatar_contradiction_clause, [], [f4291])).
fof(f4291, plain, ($false | (~ spl42_57 | ~ spl42_121 | ~ spl42_199 | ~ spl42_204 | spl42_240)), inference(subsumption_resolution, [], [f4290, f1159])).
fof(f4290, plain, (~ (e20 = op2(e20, e21)) | (~ spl42_57 | ~ spl42_199 | ~ spl42_204 | spl42_240)), inference(forward_demodulation, [], [f4289, f1790])).
fof(f4289, plain, (~ (op2(e20, e21) = h3(e10)) | (~ spl42_57 | ~ spl42_199 | ~ spl42_204 | spl42_240)), inference(forward_demodulation, [], [f4288, f855])).
fof(f4288, plain, (~ (op2(e20, e21) = h3(op1(e10, e11))) | (~ spl42_199 | ~ spl42_204 | spl42_240)), inference(forward_demodulation, [], [f4287, f1790])).
fof(f4287, plain, (~ (h3(op1(e10, e11)) = op2(h3(e10), e21)) | (~ spl42_199 | spl42_240)), inference(forward_demodulation, [], [f2030, f1765])).
fof(f2030, plain, (~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | spl42_240), inference(avatar_component_clause, [], [f2028])).
fof(f4217, plain, (~ spl42_19 | ~ spl42_31), inference(avatar_split_clause, [], [f4214, f742, f691])).
fof(f4214, plain, (~ (e12 = op1(e12, e13)) | ~ spl42_31), inference(backward_demodulation, [], [f242, f744])).
fof(f242, plain, ~ (op1(e12, e10) = op1(e12, e13)), inference(cnf_transformation, [], [f5])).
fof(f4200, plain, (~ spl42_38 | ~ spl42_42), inference(avatar_split_clause, [], [f4195, f789, f772])).
fof(f4195, plain, (~ (e11 = op1(e11, e12)) | ~ spl42_42), inference(backward_demodulation, [], [f237, f791])).
fof(f237, plain, ~ (op1(e11, e11) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f4193, plain, (~ spl42_33 | ~ spl42_45), inference(avatar_split_clause, [], [f4187, f802, f751])).
fof(f751, plain, (spl42_33 <=> (e10 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_33])])).
fof(f4187, plain, (~ (e10 = op1(e11, e13)) | ~ spl42_45), inference(backward_demodulation, [], [f236, f804])).
fof(f236, plain, ~ (op1(e11, e10) = op1(e11, e13)), inference(cnf_transformation, [], [f5])).
fof(f4181, plain, (spl42_31 | ~ spl42_51 | ~ spl42_144), inference(avatar_split_clause, [], [f4177, f1379, f827, f742])).
fof(f4177, plain, ((e12 = op1(e12, e10)) | (~ spl42_51 | ~ spl42_144)), inference(backward_demodulation, [], [f1381, f829])).
fof(f1381, plain, ((op1(e10, e13) = op1(op1(e10, e13), e10)) | ~ spl42_144), inference(avatar_component_clause, [], [f1379])).
fof(f4180, plain, (~ spl42_19 | ~ spl42_51), inference(avatar_split_clause, [], [f4175, f827, f691])).
fof(f4175, plain, (~ (e12 = op1(e12, e13)) | ~ spl42_51), inference(backward_demodulation, [], [f223, f829])).
fof(f4130, plain, (~ spl42_5 | ~ spl42_24 | ~ spl42_150), inference(avatar_contradiction_clause, [], [f4129])).
fof(f4129, plain, ($false | (~ spl42_5 | ~ spl42_24 | ~ spl42_150)), inference(subsumption_resolution, [], [f4128, f302])).
fof(f4128, plain, ((e10 = e13) | (~ spl42_5 | ~ spl42_24 | ~ spl42_150)), inference(forward_demodulation, [], [f4127, f634])).
fof(f4127, plain, ((e13 = op1(e13, e12)) | (~ spl42_24 | ~ spl42_150)), inference(forward_demodulation, [], [f1406, f714])).
fof(f714, plain, ((e13 = op1(e12, e12)) | ~ spl42_24), inference(avatar_component_clause, [], [f712])).
fof(f1406, plain, ((op1(e12, e12) = op1(op1(e12, e12), e12)) | ~ spl42_150), inference(avatar_component_clause, [], [f1404])).
fof(f4077, plain, (~ spl42_20 | ~ spl42_24), inference(avatar_split_clause, [], [f4076, f712, f695])).
fof(f4076, plain, (~ (e13 = op1(e12, e13)) | ~ spl42_24), inference(backward_demodulation, [], [f245, f714])).
fof(f4066, plain, (~ spl42_26 | ~ spl42_30), inference(avatar_split_clause, [], [f4060, f738, f721])).
fof(f721, plain, (spl42_26 <=> (e11 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_26])])).
fof(f4060, plain, (~ (e11 = op1(e12, e11)) | ~ spl42_30), inference(backward_demodulation, [], [f240, f740])).
fof(f240, plain, ~ (op1(e12, e10) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f4053, plain, (~ spl42_38 | spl42_42 | ~ spl42_149), inference(avatar_contradiction_clause, [], [f4052])).
fof(f4052, plain, ($false | (~ spl42_38 | spl42_42 | ~ spl42_149)), inference(subsumption_resolution, [], [f4051, f790])).
fof(f790, plain, (~ (e11 = op1(e11, e11)) | spl42_42), inference(avatar_component_clause, [], [f789])).
fof(f4051, plain, ((e11 = op1(e11, e11)) | (~ spl42_38 | ~ spl42_149)), inference(backward_demodulation, [], [f1402, f774])).
fof(f1402, plain, ((op1(e11, e12) = op1(op1(e11, e12), e11)) | ~ spl42_149), inference(avatar_component_clause, [], [f1400])).
fof(f1400, plain, (spl42_149 <=> (op1(e11, e12) = op1(op1(e11, e12), e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_149])])).
fof(f4024, plain, (~ spl42_23 | ~ spl42_55), inference(avatar_split_clause, [], [f4019, f844, f708])).
fof(f844, plain, (spl42_55 <=> (e12 = op1(e10, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_55])])).
fof(f4019, plain, (~ (e12 = op1(e12, e12)) | ~ spl42_55), inference(backward_demodulation, [], [f217, f846])).
fof(f846, plain, ((e12 = op1(e10, e12)) | ~ spl42_55), inference(avatar_component_clause, [], [f844])).
fof(f4002, plain, (~ spl42_49 | ~ spl42_61), inference(avatar_split_clause, [], [f3994, f870, f819])).
fof(f870, plain, (spl42_61 <=> (e10 = op1(e10, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_61])])).
fof(f3994, plain, (~ (e10 = op1(e10, e13)) | ~ spl42_61), inference(backward_demodulation, [], [f230, f872])).
fof(f872, plain, ((e10 = op1(e10, e10)) | ~ spl42_61), inference(avatar_component_clause, [], [f870])).
fof(f4001, plain, (~ spl42_29 | ~ spl42_61), inference(avatar_split_clause, [], [f3991, f870, f734])).
fof(f734, plain, (spl42_29 <=> (e10 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_29])])).
fof(f3991, plain, (~ (e10 = op1(e12, e10)) | ~ spl42_61), inference(backward_demodulation, [], [f205, f872])).
fof(f3976, plain, (~ spl42_22 | ~ spl42_26), inference(avatar_split_clause, [], [f3971, f721, f704])).
fof(f3971, plain, (~ (e11 = op1(e12, e12)) | ~ spl42_26), inference(backward_demodulation, [], [f243, f723])).
fof(f723, plain, ((e11 = op1(e12, e11)) | ~ spl42_26), inference(avatar_component_clause, [], [f721])).
fof(f243, plain, ~ (op1(e12, e11) = op1(e12, e12)), inference(cnf_transformation, [], [f5])).
fof(f3970, plain, (~ spl42_25 | ~ spl42_29), inference(avatar_split_clause, [], [f3964, f734, f717])).
fof(f3964, plain, (~ (e10 = op1(e12, e11)) | ~ spl42_29), inference(backward_demodulation, [], [f240, f736])).
fof(f736, plain, ((e10 = op1(e12, e10)) | ~ spl42_29), inference(avatar_component_clause, [], [f734])).
fof(f3961, plain, (~ spl42_25 | ~ spl42_41), inference(avatar_split_clause, [], [f3958, f785, f717])).
fof(f785, plain, (spl42_41 <=> (e10 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_41])])).
fof(f3958, plain, (~ (e10 = op1(e12, e11)) | ~ spl42_41), inference(backward_demodulation, [], [f213, f787])).
fof(f787, plain, ((e10 = op1(e11, e11)) | ~ spl42_41), inference(avatar_component_clause, [], [f785])).
fof(f213, plain, ~ (op1(e11, e11) = op1(e12, e11)), inference(cnf_transformation, [], [f5])).
fof(f3954, plain, (~ spl42_42 | ~ spl42_46), inference(avatar_split_clause, [], [f3949, f806, f789])).
fof(f806, plain, (spl42_46 <=> (e11 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_46])])).
fof(f3949, plain, (~ (e11 = op1(e11, e11)) | ~ spl42_46), inference(backward_demodulation, [], [f234, f808])).
fof(f808, plain, ((e11 = op1(e11, e10)) | ~ spl42_46), inference(avatar_component_clause, [], [f806])).
fof(f234, plain, ~ (op1(e11, e10) = op1(e11, e11)), inference(cnf_transformation, [], [f5])).
fof(f3940, plain, (~ spl42_55 | ~ spl42_63), inference(avatar_split_clause, [], [f3934, f878, f844])).
fof(f3934, plain, (~ (e12 = op1(e10, e12)) | ~ spl42_63), inference(backward_demodulation, [], [f229, f880])).
fof(f229, plain, ~ (op1(e10, e10) = op1(e10, e12)), inference(cnf_transformation, [], [f5])).
fof(f3926, plain, (~ spl42_2 | spl42_34 | ~ spl42_147), inference(avatar_contradiction_clause, [], [f3925])).
fof(f3925, plain, ($false | (~ spl42_2 | spl42_34 | ~ spl42_147)), inference(subsumption_resolution, [], [f3924, f756])).
fof(f756, plain, (~ (e11 = op1(e11, e13)) | spl42_34), inference(avatar_component_clause, [], [f755])).
fof(f755, plain, (spl42_34 <=> (e11 = op1(e11, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_34])])).
fof(f3924, plain, ((e11 = op1(e11, e13)) | (~ spl42_2 | ~ spl42_147)), inference(forward_demodulation, [], [f1393, f621])).
fof(f1393, plain, ((op1(e13, e13) = op1(op1(e13, e13), e13)) | ~ spl42_147), inference(avatar_component_clause, [], [f1391])).
fof(f1391, plain, (spl42_147 <=> (op1(e13, e13) = op1(op1(e13, e13), e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_147])])).
fof(f3903, plain, (~ spl42_44 | ~ spl42_60), inference(avatar_split_clause, [], [f3902, f865, f797])).
fof(f3902, plain, (~ (e13 = op1(e11, e11)) | ~ spl42_60), inference(forward_demodulation, [], [f210, f867])).
fof(f3893, plain, (spl42_204 | ~ spl42_81 | ~ spl42_248), inference(avatar_split_clause, [], [f3890, f2060, f987, f1789])).
fof(f3890, plain, ((e20 = h3(e10)) | (~ spl42_81 | ~ spl42_248)), inference(backward_demodulation, [], [f3398, f989])).
fof(f989, plain, ((e20 = op2(e22, e23)) | ~ spl42_81), inference(avatar_component_clause, [], [f987])).
fof(f3398, plain, ((op2(e22, e23) = h3(e10)) | ~ spl42_248), inference(backward_demodulation, [], [f1702, f2061])).
fof(f1702, plain, (h3(e10) = op2(e22, h3(e12))), inference(forward_demodulation, [], [f1701, f1700])).
fof(f1700, plain, (h3(e12) = op2(e22, h3(e11))), inference(backward_demodulation, [], [f545, f544])).
fof(f545, plain, (op2(e22, op2(e22, e22)) = h3(e12)), inference(cnf_transformation, [], [f16])).
fof(f1701, plain, (h3(e10) = op2(e22, op2(e22, h3(e11)))), inference(forward_demodulation, [], [f543, f544])).
fof(f543, plain, (h3(e10) = op2(e22, op2(e22, op2(e22, e22)))), inference(cnf_transformation, [], [f16])).
fof(f3853, plain, (~ spl42_49 | spl42_61 | ~ spl42_144), inference(avatar_contradiction_clause, [], [f3852])).
fof(f3852, plain, ($false | (~ spl42_49 | spl42_61 | ~ spl42_144)), inference(subsumption_resolution, [], [f3851, f871])).
fof(f871, plain, (~ (e10 = op1(e10, e10)) | spl42_61), inference(avatar_component_clause, [], [f870])).
fof(f3851, plain, ((e10 = op1(e10, e10)) | (~ spl42_49 | ~ spl42_144)), inference(forward_demodulation, [], [f1381, f821])).
fof(f3836, plain, (~ spl42_102 | ~ spl42_199), inference(avatar_split_clause, [], [f3828, f1764, f1076])).
fof(f3828, plain, (~ (e21 = op2(e21, e22)) | ~ spl42_199), inference(backward_demodulation, [], [f1695, f1765])).
fof(f1695, plain, ~ (op2(e21, e22) = h3(e11)), inference(backward_demodulation, [], [f267, f544])).
fof(f267, plain, ~ (op2(e21, e22) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f3794, plain, (spl42_27 | ~ spl42_35 | ~ spl42_145), inference(avatar_contradiction_clause, [], [f3793])).
fof(f3793, plain, ($false | (spl42_27 | ~ spl42_35 | ~ spl42_145)), inference(subsumption_resolution, [], [f3790, f726])).
fof(f726, plain, (~ (e12 = op1(e12, e11)) | spl42_27), inference(avatar_component_clause, [], [f725])).
fof(f725, plain, (spl42_27 <=> (e12 = op1(e12, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_27])])).
fof(f3790, plain, ((e12 = op1(e12, e11)) | (~ spl42_35 | ~ spl42_145)), inference(backward_demodulation, [], [f1385, f761])).
fof(f3778, plain, (spl42_31 | ~ spl42_55 | ~ spl42_148), inference(avatar_split_clause, [], [f3777, f1396, f844, f742])).
fof(f3777, plain, ((e12 = op1(e12, e10)) | (~ spl42_55 | ~ spl42_148)), inference(backward_demodulation, [], [f1398, f846])).
fof(f1398, plain, ((op1(e10, e12) = op1(op1(e10, e12), e10)) | ~ spl42_148), inference(avatar_component_clause, [], [f1396])).
fof(f3774, plain, (~ spl42_16 | ~ spl42_60 | spl42_152), inference(avatar_contradiction_clause, [], [f3773])).
fof(f3773, plain, ($false | (~ spl42_16 | ~ spl42_60 | spl42_152)), inference(subsumption_resolution, [], [f3770, f680])).
fof(f3770, plain, (~ (e13 = op1(e13, e10)) | (~ spl42_60 | spl42_152)), inference(backward_demodulation, [], [f1414, f867])).
fof(f1414, plain, (~ (op1(e10, e11) = op1(op1(e10, e11), e10)) | spl42_152), inference(avatar_component_clause, [], [f1413])).
fof(f1413, plain, (spl42_152 <=> (op1(e10, e11) = op1(op1(e10, e11), e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_152])])).
fof(f3772, plain, (~ spl42_52 | ~ spl42_60), inference(avatar_split_clause, [], [f3769, f865, f831])).
fof(f3769, plain, (~ (e13 = op1(e10, e13)) | ~ spl42_60), inference(backward_demodulation, [], [f232, f867])).
fof(f3752, plain, (spl42_91 | ~ spl42_99 | ~ spl42_176), inference(avatar_contradiction_clause, [], [f3751])).
fof(f3751, plain, ($false | (spl42_91 | ~ spl42_99 | ~ spl42_176)), inference(subsumption_resolution, [], [f3749, f1030])).
fof(f1030, plain, (~ (e22 = op2(e22, e21)) | spl42_91), inference(avatar_component_clause, [], [f1029])).
fof(f3749, plain, ((e22 = op2(e22, e21)) | (~ spl42_99 | ~ spl42_176)), inference(backward_demodulation, [], [f1609, f1065])).
fof(f1609, plain, ((op2(e21, e23) = op2(op2(e21, e23), e21)) | ~ spl42_176), inference(avatar_component_clause, [], [f1607])).
fof(f1607, plain, (spl42_176 <=> (op2(e21, e23) = op2(op2(e21, e23), e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_176])])).
fof(f3741, plain, (~ spl42_106 | ~ spl42_108), inference(avatar_contradiction_clause, [], [f3740])).
fof(f3740, plain, ($false | (~ spl42_106 | ~ spl42_108)), inference(subsumption_resolution, [], [f3739, f310])).
fof(f3739, plain, ((e21 = e23) | (~ spl42_106 | ~ spl42_108)), inference(forward_demodulation, [], [f1103, f1095])).
fof(f1103, plain, ((e23 = op2(e21, e21)) | ~ spl42_108), inference(avatar_component_clause, [], [f1101])).
fof(f1101, plain, (spl42_108 <=> (e23 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_108])])).
fof(f3721, plain, (spl42_286 | ~ spl42_124 | ~ spl42_219), inference(avatar_split_clause, [], [f3720, f1865, f1169, f2288])).
fof(f2288, plain, (spl42_286 <=> (e23 = h1(e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_286])])).
fof(f1169, plain, (spl42_124 <=> (e23 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_124])])).
fof(f3720, plain, ((e23 = h1(e12)) | (~ spl42_124 | ~ spl42_219)), inference(backward_demodulation, [], [f3409, f1171])).
fof(f1171, plain, ((e23 = op2(e20, e21)) | ~ spl42_124), inference(avatar_component_clause, [], [f1169])).
fof(f3409, plain, ((op2(e20, e21) = h1(e12)) | ~ spl42_219), inference(backward_demodulation, [], [f1682, f1866])).
fof(f3696, plain, (~ spl42_90 | ~ spl42_106), inference(avatar_split_clause, [], [f3695, f1093, f1025])).
fof(f1025, plain, (spl42_90 <=> (e21 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_90])])).
fof(f3695, plain, (~ (e21 = op2(e22, e21)) | ~ spl42_106), inference(backward_demodulation, [], [f1686, f3448])).
fof(f3448, plain, ((e21 = h2(e11)) | ~ spl42_106), inference(backward_demodulation, [], [f540, f1095])).
fof(f3691, plain, (~ spl42_102 | ~ spl42_106), inference(avatar_split_clause, [], [f3543, f1093, f1076])).
fof(f3543, plain, (~ (e21 = op2(e21, e22)) | ~ spl42_106), inference(forward_demodulation, [], [f1689, f3448])).
fof(f1689, plain, ~ (op2(e21, e22) = h2(e11)), inference(backward_demodulation, [], [f285, f540])).
fof(f285, plain, ~ (op2(e21, e21) = op2(e21, e22)), inference(cnf_transformation, [], [f6])).
fof(f3679, plain, (~ spl42_59 | ~ spl42_11), inference(avatar_split_clause, [], [f3678, f657, f861])).
fof(f861, plain, (spl42_59 <=> (e12 = op1(e10, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_59])])).
fof(f3678, plain, (~ (e12 = op1(e10, e11)) | ~ spl42_11), inference(forward_demodulation, [], [f212, f659])).
fof(f212, plain, ~ (op1(e10, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f3656, plain, (~ spl42_2 | ~ spl42_3), inference(avatar_contradiction_clause, [], [f3655])).
fof(f3655, plain, ($false | (~ spl42_2 | ~ spl42_3)), inference(subsumption_resolution, [], [f3654, f303])).
fof(f3654, plain, ((e11 = e12) | (~ spl42_2 | ~ spl42_3)), inference(forward_demodulation, [], [f625, f621])).
fof(f625, plain, ((e12 = op1(e13, e13)) | ~ spl42_3), inference(avatar_component_clause, [], [f623])).
fof(f623, plain, (spl42_3 <=> (e12 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_3])])).
fof(f3603, plain, (~ spl42_105 | ~ spl42_106), inference(avatar_contradiction_clause, [], [f3602])).
fof(f3602, plain, ($false | (~ spl42_105 | ~ spl42_106)), inference(subsumption_resolution, [], [f3601, f306])).
fof(f3601, plain, ((e20 = e21) | (~ spl42_105 | ~ spl42_106)), inference(backward_demodulation, [], [f1095, f1091])).
fof(f1091, plain, ((e20 = op2(e21, e21)) | ~ spl42_105), inference(avatar_component_clause, [], [f1089])).
fof(f1089, plain, (spl42_105 <=> (e20 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_105])])).
fof(f3583, plain, (~ spl42_80 | ~ spl42_120 | spl42_179), inference(avatar_contradiction_clause, [], [f3582])).
fof(f3582, plain, ($false | (~ spl42_80 | ~ spl42_120 | spl42_179)), inference(subsumption_resolution, [], [f3578, f984])).
fof(f3578, plain, (~ (e23 = op2(e23, e20)) | (~ spl42_120 | spl42_179)), inference(backward_demodulation, [], [f1621, f1154])).
fof(f1154, plain, ((e23 = op2(e20, e22)) | ~ spl42_120), inference(avatar_component_clause, [], [f1152])).
fof(f1621, plain, (~ (op2(e20, e22) = op2(op2(e20, e22), e20)) | spl42_179), inference(avatar_component_clause, [], [f1620])).
fof(f3581, plain, (~ spl42_286 | ~ spl42_120 | ~ spl42_219), inference(avatar_split_clause, [], [f3577, f1865, f1152, f2288])).
fof(f3577, plain, (~ (e23 = h1(e12)) | (~ spl42_120 | ~ spl42_219)), inference(backward_demodulation, [], [f3557, f1154])).
fof(f3557, plain, (~ (op2(e20, e22) = h1(e12)) | ~ spl42_219), inference(backward_demodulation, [], [f279, f3409])).
fof(f279, plain, ~ (op2(e20, e21) = op2(e20, e22)), inference(cnf_transformation, [], [f6])).
fof(f3572, plain, (~ spl42_126 | ~ spl42_127), inference(avatar_contradiction_clause, [], [f3571])).
fof(f3571, plain, ($false | (~ spl42_126 | ~ spl42_127)), inference(subsumption_resolution, [], [f3570, f309])).
fof(f3570, plain, ((e21 = e22) | (~ spl42_126 | ~ spl42_127)), inference(forward_demodulation, [], [f1184, f1180])).
fof(f1180, plain, ((op2(e20, e20) = e21) | ~ spl42_126), inference(avatar_component_clause, [], [f1178])).
fof(f1178, plain, (spl42_126 <=> (op2(e20, e20) = e21)), introduced(avatar_definition, [new_symbols(naming, [spl42_126])])).
fof(f3558, plain, (~ spl42_122 | ~ spl42_219), inference(avatar_split_clause, [], [f3407, f1865, f1161])).
fof(f3407, plain, (~ (e21 = op2(e20, e21)) | ~ spl42_219), inference(backward_demodulation, [], [f1679, f1866])).
fof(f3547, plain, (~ spl42_112 | ~ spl42_80), inference(avatar_split_clause, [], [f3035, f982, f1118])).
fof(f1118, plain, (spl42_112 <=> (e23 = op2(e21, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_112])])).
fof(f3035, plain, (~ (e23 = op2(e21, e20)) | ~ spl42_80), inference(forward_demodulation, [], [f256, f984])).
fof(f256, plain, ~ (op2(e21, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3540, plain, (~ spl42_56 | ~ spl42_40), inference(avatar_split_clause, [], [f3539, f780, f848])).
fof(f780, plain, (spl42_40 <=> (e13 = op1(e11, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_40])])).
fof(f3539, plain, (~ (e13 = op1(e10, e12)) | ~ spl42_40), inference(forward_demodulation, [], [f216, f782])).
fof(f782, plain, ((e13 = op1(e11, e12)) | ~ spl42_40), inference(avatar_component_clause, [], [f780])).
fof(f216, plain, ~ (op1(e10, e12) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f3538, plain, (~ spl42_54 | ~ spl42_62), inference(avatar_split_clause, [], [f3537, f874, f840])).
fof(f3537, plain, (~ (e11 = op1(e10, e12)) | ~ spl42_62), inference(forward_demodulation, [], [f229, f876])).
fof(f3534, plain, (~ spl42_34 | ~ spl42_2), inference(avatar_split_clause, [], [f2832, f619, f755])).
fof(f2832, plain, (~ (e11 = op1(e11, e13)) | ~ spl42_2), inference(forward_demodulation, [], [f226, f621])).
fof(f226, plain, ~ (op1(e11, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3528, plain, (~ spl42_32 | ~ spl42_16), inference(avatar_split_clause, [], [f2830, f678, f746])).
fof(f746, plain, (spl42_32 <=> (e13 = op1(e12, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_32])])).
fof(f2830, plain, (~ (e13 = op1(e12, e10)) | ~ spl42_16), inference(forward_demodulation, [], [f209, f680])).
fof(f209, plain, ~ (op1(e12, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f3519, plain, (~ spl42_5 | ~ spl42_6), inference(avatar_contradiction_clause, [], [f3518])).
fof(f3518, plain, ($false | (~ spl42_5 | ~ spl42_6)), inference(subsumption_resolution, [], [f3517, f300])).
fof(f300, plain, ~ (e10 = e11), inference(cnf_transformation, [], [f7])).
fof(f3517, plain, ((e10 = e11) | (~ spl42_5 | ~ spl42_6)), inference(forward_demodulation, [], [f638, f634])).
fof(f638, plain, ((e11 = op1(e13, e12)) | ~ spl42_6), inference(avatar_component_clause, [], [f636])).
fof(f636, plain, (spl42_6 <=> (e11 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_6])])).
fof(f3508, plain, (~ spl42_26 | ~ spl42_42), inference(avatar_split_clause, [], [f3503, f789, f721])).
fof(f3503, plain, (~ (e11 = op1(e12, e11)) | ~ spl42_42), inference(backward_demodulation, [], [f213, f791])).
fof(f3499, plain, (spl42_27 | ~ spl42_47 | ~ spl42_157), inference(avatar_contradiction_clause, [], [f3498])).
fof(f3498, plain, ($false | (spl42_27 | ~ spl42_47 | ~ spl42_157)), inference(subsumption_resolution, [], [f3497, f726])).
fof(f3497, plain, ((e12 = op1(e12, e11)) | (~ spl42_47 | ~ spl42_157)), inference(forward_demodulation, [], [f1436, f812])).
fof(f812, plain, ((e12 = op1(e11, e10)) | ~ spl42_47), inference(avatar_component_clause, [], [f810])).
fof(f810, plain, (spl42_47 <=> (e12 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_47])])).
fof(f1436, plain, ((op1(e11, e10) = op1(op1(e11, e10), e11)) | ~ spl42_157), inference(avatar_component_clause, [], [f1434])).
fof(f1434, plain, (spl42_157 <=> (op1(e11, e10) = op1(op1(e11, e10), e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_157])])).
fof(f3490, plain, (spl42_110 | ~ spl42_126 | ~ spl42_187), inference(avatar_contradiction_clause, [], [f3489])).
fof(f3489, plain, ($false | (spl42_110 | ~ spl42_126 | ~ spl42_187)), inference(subsumption_resolution, [], [f3488, f1111])).
fof(f3488, plain, ((e21 = op2(e21, e20)) | (~ spl42_126 | ~ spl42_187)), inference(forward_demodulation, [], [f1656, f1180])).
fof(f3469, plain, (~ spl42_18 | ~ spl42_2), inference(avatar_split_clause, [], [f2640, f619, f687])).
fof(f687, plain, (spl42_18 <=> (e11 = op1(e12, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_18])])).
fof(f2640, plain, (~ (e11 = op1(e12, e13)) | ~ spl42_2), inference(forward_demodulation, [], [f227, f621])).
fof(f227, plain, ~ (op1(e12, e13) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f3431, plain, (~ spl42_29 | spl42_53 | ~ spl42_158), inference(avatar_contradiction_clause, [], [f3430])).
fof(f3430, plain, ($false | (~ spl42_29 | spl42_53 | ~ spl42_158)), inference(subsumption_resolution, [], [f3429, f837])).
fof(f3429, plain, ((e10 = op1(e10, e12)) | (~ spl42_29 | ~ spl42_158)), inference(forward_demodulation, [], [f1440, f736])).
fof(f1440, plain, ((op1(e12, e10) = op1(op1(e12, e10), e12)) | ~ spl42_158), inference(avatar_component_clause, [], [f1438])).
fof(f1438, plain, (spl42_158 <=> (op1(e12, e10) = op1(op1(e12, e10), e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_158])])).
fof(f3424, plain, (spl42_91 | ~ spl42_111 | ~ spl42_188), inference(avatar_contradiction_clause, [], [f3423])).
fof(f3423, plain, ($false | (spl42_91 | ~ spl42_111 | ~ spl42_188)), inference(subsumption_resolution, [], [f3422, f1030])).
fof(f3422, plain, ((e22 = op2(e22, e21)) | (~ spl42_111 | ~ spl42_188)), inference(forward_demodulation, [], [f1660, f1116])).
fof(f3395, plain, (spl42_219 | ~ spl42_126), inference(avatar_split_clause, [], [f3342, f1178, f1865])).
fof(f3342, plain, ((e21 = h1(e11)) | ~ spl42_126), inference(backward_demodulation, [], [f536, f1180])).
fof(f3367, plain, (~ spl42_97 | ~ spl42_100), inference(avatar_contradiction_clause, [], [f3366])).
fof(f3366, plain, ($false | (~ spl42_97 | ~ spl42_100)), inference(subsumption_resolution, [], [f3365, f308])).
fof(f3365, plain, ((e20 = e23) | (~ spl42_97 | ~ spl42_100)), inference(forward_demodulation, [], [f1069, f1057])).
fof(f1057, plain, ((e20 = op2(e21, e23)) | ~ spl42_97), inference(avatar_component_clause, [], [f1055])).
fof(f1069, plain, ((e23 = op2(e21, e23)) | ~ spl42_100), inference(avatar_component_clause, [], [f1067])).
fof(f3352, plain, (~ spl42_116 | ~ spl42_124), inference(avatar_split_clause, [], [f3350, f1169, f1135])).
fof(f3350, plain, (~ (e23 = op2(e20, e23)) | ~ spl42_124), inference(backward_demodulation, [], [f280, f1171])).
fof(f280, plain, ~ (op2(e20, e21) = op2(e20, e23)), inference(cnf_transformation, [], [f6])).
fof(f3341, plain, (spl42_46 | ~ spl42_62 | ~ spl42_156), inference(avatar_contradiction_clause, [], [f3340])).
fof(f3340, plain, ($false | (spl42_46 | ~ spl42_62 | ~ spl42_156)), inference(subsumption_resolution, [], [f3339, f807])).
fof(f807, plain, (~ (e11 = op1(e11, e10)) | spl42_46), inference(avatar_component_clause, [], [f806])).
fof(f3339, plain, ((e11 = op1(e11, e10)) | (~ spl42_62 | ~ spl42_156)), inference(forward_demodulation, [], [f1432, f876])).
fof(f1432, plain, ((op1(e10, e10) = op1(op1(e10, e10), e10)) | ~ spl42_156), inference(avatar_component_clause, [], [f1430])).
fof(f1430, plain, (spl42_156 <=> (op1(e10, e10) = op1(op1(e10, e10), e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_156])])).
fof(f3332, plain, (~ spl42_66 | ~ spl42_80 | ~ spl42_190), inference(avatar_contradiction_clause, [], [f3331])).
fof(f3331, plain, ($false | (~ spl42_66 | ~ spl42_80 | ~ spl42_190)), inference(subsumption_resolution, [], [f3330, f310])).
fof(f3330, plain, ((e21 = e23) | (~ spl42_66 | ~ spl42_80 | ~ spl42_190)), inference(forward_demodulation, [], [f3329, f925])).
fof(f3329, plain, ((e23 = op2(e23, e23)) | (~ spl42_80 | ~ spl42_190)), inference(forward_demodulation, [], [f1668, f984])).
fof(f1668, plain, ((op2(e23, e20) = op2(op2(e23, e20), e23)) | ~ spl42_190), inference(avatar_component_clause, [], [f1666])).
fof(f1666, plain, (spl42_190 <=> (op2(e23, e20) = op2(op2(e23, e20), e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_190])])).
fof(f3317, plain, (~ spl42_89 | ~ spl42_211), inference(avatar_split_clause, [], [f3312, f1825, f1021])).
fof(f1021, plain, (spl42_89 <=> (e20 = op2(e22, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_89])])).
fof(f1825, plain, (spl42_211 <=> (e20 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_211])])).
fof(f3312, plain, (~ (e20 = op2(e22, e21)) | ~ spl42_211), inference(backward_demodulation, [], [f1686, f1826])).
fof(f1826, plain, ((e20 = h2(e11)) | ~ spl42_211), inference(avatar_component_clause, [], [f1825])).
fof(f3304, plain, (~ spl42_207 | ~ spl42_75), inference(avatar_split_clause, [], [f3303, f961, f1804])).
fof(f1804, plain, (spl42_207 <=> (e22 = h2(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_207])])).
fof(f3303, plain, (~ (e22 = h2(e11)) | ~ spl42_75), inference(forward_demodulation, [], [f1687, f963])).
fof(f1687, plain, ~ (op2(e23, e21) = h2(e11)), inference(backward_demodulation, [], [f262, f540])).
fof(f262, plain, ~ (op2(e21, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3297, plain, (~ spl42_203 | ~ spl42_69), inference(avatar_split_clause, [], [f2859, f936, f1784])).
fof(f1784, plain, (spl42_203 <=> (e20 = h3(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_203])])).
fof(f2859, plain, (~ (e20 = h3(e11)) | ~ spl42_69), inference(forward_demodulation, [], [f1696, f938])).
fof(f1696, plain, ~ (op2(e23, e22) = h3(e11)), inference(backward_demodulation, [], [f269, f544])).
fof(f269, plain, ~ (op2(e22, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f3294, plain, (~ spl42_126 | ~ spl42_62 | ~ spl42_192 | ~ spl42_196 | spl42_221), inference(avatar_split_clause, [], [f3293, f1896, f1748, f1727, f874, f1178])).
fof(f1896, plain, (spl42_221 <=> (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), introduced(avatar_definition, [new_symbols(naming, [spl42_221])])).
fof(f3293, plain, (~ (op2(e20, e20) = e21) | (~ spl42_62 | ~ spl42_192 | ~ spl42_196 | spl42_221)), inference(forward_demodulation, [], [f3292, f1728])).
fof(f3292, plain, (~ (op2(e20, e20) = h4(e11)) | (~ spl42_62 | ~ spl42_196 | spl42_221)), inference(forward_demodulation, [], [f3204, f876])).
fof(f3204, plain, (~ (op2(e20, e20) = h4(op1(e10, e10))) | (~ spl42_196 | spl42_221)), inference(backward_demodulation, [], [f1898, f1749])).
fof(f1898, plain, (~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10))) | spl42_221), inference(avatar_component_clause, [], [f1896])).
fof(f3289, plain, (~ spl42_123 | ~ spl42_75), inference(avatar_split_clause, [], [f3288, f961, f1165])).
fof(f1165, plain, (spl42_123 <=> (e22 = op2(e20, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_123])])).
fof(f3288, plain, (~ (e22 = op2(e20, e21)) | ~ spl42_75), inference(forward_demodulation, [], [f260, f963])).
fof(f260, plain, ~ (op2(e20, e21) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f3281, plain, (~ spl42_215 | ~ spl42_111), inference(avatar_split_clause, [], [f3280, f1114, f1845])).
fof(f3280, plain, (~ (e22 = h1(e11)) | ~ spl42_111), inference(forward_demodulation, [], [f1676, f1116])).
fof(f3243, plain, (~ spl42_46 | ~ spl42_62), inference(avatar_split_clause, [], [f3240, f874, f806])).
fof(f3240, plain, (~ (e11 = op1(e11, e10)) | ~ spl42_62), inference(backward_demodulation, [], [f204, f876])).
fof(f204, plain, ~ (op1(e10, e10) = op1(e11, e10)), inference(cnf_transformation, [], [f5])).
fof(f3219, plain, (~ spl42_103 | ~ spl42_104), inference(avatar_contradiction_clause, [], [f3218])).
fof(f3218, plain, ($false | (~ spl42_103 | ~ spl42_104)), inference(subsumption_resolution, [], [f3217, f311])).
fof(f3217, plain, ((e22 = e23) | (~ spl42_103 | ~ spl42_104)), inference(forward_demodulation, [], [f1086, f1082])).
fof(f1082, plain, ((e22 = op2(e21, e22)) | ~ spl42_103), inference(avatar_component_clause, [], [f1080])).
fof(f1086, plain, ((e23 = op2(e21, e22)) | ~ spl42_104), inference(avatar_component_clause, [], [f1084])).
fof(f1084, plain, (spl42_104 <=> (e23 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_104])])).
fof(f3216, plain, (~ spl42_110 | ~ spl42_111), inference(avatar_contradiction_clause, [], [f3215])).
fof(f3215, plain, ($false | (~ spl42_110 | ~ spl42_111)), inference(subsumption_resolution, [], [f3214, f309])).
fof(f3214, plain, ((e21 = e22) | (~ spl42_110 | ~ spl42_111)), inference(forward_demodulation, [], [f1116, f1112])).
fof(f3203, plain, (spl42_196 | ~ spl42_69), inference(avatar_split_clause, [], [f3055, f936, f1748])).
fof(f3055, plain, ((e20 = h4(e10)) | ~ spl42_69), inference(forward_demodulation, [], [f1712, f938])).
fof(f3201, plain, (~ spl42_125 | spl42_187), inference(avatar_contradiction_clause, [], [f3200])).
fof(f3200, plain, ($false | (~ spl42_125 | spl42_187)), inference(subsumption_resolution, [], [f3191, f1176])).
fof(f3191, plain, (~ (e20 = op2(e20, e20)) | (~ spl42_125 | spl42_187)), inference(backward_demodulation, [], [f3052, f3184])).
fof(f3184, plain, ((e20 = h1(e11)) | ~ spl42_125), inference(forward_demodulation, [], [f536, f1176])).
fof(f3052, plain, (~ (h1(e11) = op2(h1(e11), e20)) | spl42_187), inference(backward_demodulation, [], [f1655, f536])).
fof(f1655, plain, (~ (op2(e20, e20) = op2(op2(e20, e20), e20)) | spl42_187), inference(avatar_component_clause, [], [f1654])).
fof(f3193, plain, (~ spl42_113 | ~ spl42_125), inference(avatar_split_clause, [], [f3186, f1174, f1123])).
fof(f3186, plain, (~ (e20 = op2(e20, e23)) | ~ spl42_125), inference(backward_demodulation, [], [f1681, f3184])).
fof(f3192, plain, (~ spl42_93 | ~ spl42_125), inference(avatar_split_clause, [], [f3185, f1174, f1038])).
fof(f3185, plain, (~ (e20 = op2(e22, e20)) | ~ spl42_125), inference(backward_demodulation, [], [f1677, f3184])).
fof(f3181, plain, (~ spl42_115 | ~ spl42_119), inference(avatar_split_clause, [], [f3180, f1148, f1131])).
fof(f3180, plain, (~ (e22 = op2(e20, e23)) | ~ spl42_119), inference(forward_demodulation, [], [f281, f1150])).
fof(f3179, plain, (~ spl42_115 | ~ spl42_83), inference(avatar_split_clause, [], [f2699, f995, f1131])).
fof(f2699, plain, (~ (e22 = op2(e20, e23)) | ~ spl42_83), inference(forward_demodulation, [], [f271, f997])).
fof(f997, plain, ((e22 = op2(e22, e23)) | ~ spl42_83), inference(avatar_component_clause, [], [f995])).
fof(f271, plain, ~ (op2(e20, e23) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f3162, plain, (~ spl42_94 | ~ spl42_199), inference(avatar_split_clause, [], [f3161, f1764, f1042])).
fof(f3161, plain, (~ (e21 = op2(e22, e20)) | ~ spl42_199), inference(forward_demodulation, [], [f1697, f1765])).
fof(f1697, plain, ~ (op2(e22, e20) = h3(e11)), inference(backward_demodulation, [], [f289, f544])).
fof(f289, plain, ~ (op2(e22, e20) = op2(e22, e22)), inference(cnf_transformation, [], [f6])).
fof(f3160, plain, (~ spl42_96 | ~ spl42_80), inference(avatar_split_clause, [], [f3033, f982, f1050])).
fof(f1050, plain, (spl42_96 <=> (e23 = op2(e22, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_96])])).
fof(f3033, plain, (~ (e23 = op2(e22, e20)) | ~ spl42_80), inference(forward_demodulation, [], [f257, f984])).
fof(f257, plain, ~ (op2(e22, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3140, plain, (~ spl42_1 | ~ spl42_2), inference(avatar_contradiction_clause, [], [f3139])).
fof(f3139, plain, ($false | (~ spl42_1 | ~ spl42_2)), inference(subsumption_resolution, [], [f3138, f300])).
fof(f3138, plain, ((e10 = e11) | (~ spl42_1 | ~ spl42_2)), inference(backward_demodulation, [], [f621, f617])).
fof(f617, plain, ((e10 = op1(e13, e13)) | ~ spl42_1), inference(avatar_component_clause, [], [f615])).
fof(f615, plain, (spl42_1 <=> (e10 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_1])])).
fof(f3119, plain, (~ spl42_83 | ~ spl42_84), inference(avatar_contradiction_clause, [], [f3118])).
fof(f3118, plain, ($false | (~ spl42_83 | ~ spl42_84)), inference(subsumption_resolution, [], [f3117, f311])).
fof(f3117, plain, ((e22 = e23) | (~ spl42_83 | ~ spl42_84)), inference(forward_demodulation, [], [f1001, f997])).
fof(f3101, plain, (~ spl42_125 | ~ spl42_127), inference(avatar_contradiction_clause, [], [f3100])).
fof(f3100, plain, ($false | (~ spl42_125 | ~ spl42_127)), inference(subsumption_resolution, [], [f3099, f307])).
fof(f3099, plain, ((e20 = e22) | (~ spl42_125 | ~ spl42_127)), inference(backward_demodulation, [], [f1184, f1176])).
fof(f3098, plain, (~ spl42_127 | spl42_215), inference(avatar_contradiction_clause, [], [f3097])).
fof(f3097, plain, ($false | (~ spl42_127 | spl42_215)), inference(subsumption_resolution, [], [f3096, f1847])).
fof(f1847, plain, (~ (e22 = h1(e11)) | spl42_215), inference(avatar_component_clause, [], [f1845])).
fof(f3096, plain, ((e22 = h1(e11)) | ~ spl42_127), inference(backward_demodulation, [], [f536, f1184])).
fof(f3082, plain, (~ spl42_93 | spl42_117 | ~ spl42_189), inference(avatar_contradiction_clause, [], [f3081])).
fof(f3081, plain, ($false | (~ spl42_93 | spl42_117 | ~ spl42_189)), inference(subsumption_resolution, [], [f3080, f1141])).
fof(f1141, plain, (~ (e20 = op2(e20, e22)) | spl42_117), inference(avatar_component_clause, [], [f1140])).
fof(f1140, plain, (spl42_117 <=> (e20 = op2(e20, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_117])])).
fof(f3080, plain, ((e20 = op2(e20, e22)) | (~ spl42_93 | ~ spl42_189)), inference(forward_demodulation, [], [f1664, f1040])).
fof(f3079, plain, (spl42_248 | ~ spl42_92 | ~ spl42_199), inference(avatar_split_clause, [], [f3078, f1764, f1033, f2060])).
fof(f3078, plain, ((e23 = h3(e12)) | (~ spl42_92 | ~ spl42_199)), inference(forward_demodulation, [], [f3075, f1035])).
fof(f1035, plain, ((e23 = op2(e22, e21)) | ~ spl42_92), inference(avatar_component_clause, [], [f1033])).
fof(f3075, plain, ((op2(e22, e21) = h3(e12)) | ~ spl42_199), inference(backward_demodulation, [], [f1700, f1765])).
fof(f3053, plain, (spl42_199 | ~ spl42_86), inference(avatar_split_clause, [], [f2943, f1008, f1764])).
fof(f2943, plain, ((e21 = h3(e11)) | ~ spl42_86), inference(backward_demodulation, [], [f544, f1010])).
fof(f3050, plain, (~ spl42_98 | ~ spl42_192), inference(avatar_split_clause, [], [f2898, f1727, f1059])).
fof(f2898, plain, (~ (e21 = op2(e21, e23)) | ~ spl42_192), inference(forward_demodulation, [], [f1705, f1728])).
fof(f1705, plain, ~ (op2(e21, e23) = h4(e11)), inference(backward_demodulation, [], [f274, f548])).
fof(f274, plain, ~ (op2(e21, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f3039, plain, (~ spl42_294 | ~ spl42_80), inference(avatar_split_clause, [], [f3038, f982, f2332])).
fof(f2332, plain, (spl42_294 <=> (e23 = h1(e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_294])])).
fof(f3038, plain, (~ (e23 = h1(e11)) | ~ spl42_80), inference(forward_demodulation, [], [f1678, f984])).
fof(f1678, plain, ~ (op2(e23, e20) = h1(e11)), inference(backward_demodulation, [], [f254, f536])).
fof(f254, plain, ~ (op2(e20, e20) = op2(e23, e20)), inference(cnf_transformation, [], [f6])).
fof(f3002, plain, (~ spl42_17 | ~ spl42_29), inference(avatar_split_clause, [], [f3001, f734, f683])).
fof(f3001, plain, (~ (e10 = op1(e12, e13)) | ~ spl42_29), inference(forward_demodulation, [], [f242, f736])).
fof(f2986, plain, (~ spl42_33 | ~ spl42_35), inference(avatar_contradiction_clause, [], [f2985])).
fof(f2985, plain, ($false | (~ spl42_33 | ~ spl42_35)), inference(subsumption_resolution, [], [f2983, f301])).
fof(f301, plain, ~ (e10 = e12), inference(cnf_transformation, [], [f7])).
fof(f2983, plain, ((e10 = e12) | (~ spl42_33 | ~ spl42_35)), inference(backward_demodulation, [], [f761, f753])).
fof(f753, plain, ((e10 = op1(e11, e13)) | ~ spl42_33), inference(avatar_component_clause, [], [f751])).
fof(f2970, plain, (~ spl42_38 | ~ spl42_46), inference(avatar_split_clause, [], [f2966, f806, f772])).
fof(f2966, plain, (~ (e11 = op1(e11, e12)) | ~ spl42_46), inference(backward_demodulation, [], [f235, f808])).
fof(f235, plain, ~ (op1(e11, e10) = op1(e11, e12)), inference(cnf_transformation, [], [f5])).
fof(f2960, plain, (~ spl42_47 | ~ spl42_63), inference(avatar_split_clause, [], [f2959, f878, f810])).
fof(f2959, plain, (~ (e12 = op1(e11, e10)) | ~ spl42_63), inference(backward_demodulation, [], [f204, f880])).
fof(f2935, plain, (spl42_207 | ~ spl42_107), inference(avatar_split_clause, [], [f2934, f1097, f1804])).
fof(f1097, plain, (spl42_107 <=> (e22 = op2(e21, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_107])])).
fof(f2934, plain, ((e22 = h2(e11)) | ~ spl42_107), inference(backward_demodulation, [], [f540, f1099])).
fof(f1099, plain, ((e22 = op2(e21, e21)) | ~ spl42_107), inference(avatar_component_clause, [], [f1097])).
fof(f2921, plain, (spl42_106 | ~ spl42_110 | ~ spl42_188), inference(avatar_split_clause, [], [f2920, f1658, f1110, f1093])).
fof(f2920, plain, ((e21 = op2(e21, e21)) | (~ spl42_110 | ~ spl42_188)), inference(forward_demodulation, [], [f1660, f1112])).
fof(f2914, plain, (spl42_125 | ~ spl42_113 | ~ spl42_175), inference(avatar_split_clause, [], [f2913, f1603, f1123, f1174])).
fof(f1603, plain, (spl42_175 <=> (op2(e20, e23) = op2(op2(e20, e23), e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_175])])).
fof(f2913, plain, ((e20 = op2(e20, e20)) | (~ spl42_113 | ~ spl42_175)), inference(forward_demodulation, [], [f1605, f1125])).
fof(f1605, plain, ((op2(e20, e23) = op2(op2(e20, e23), e20)) | ~ spl42_175), inference(avatar_component_clause, [], [f1603])).
fof(f2903, plain, (~ spl42_113 | ~ spl42_116), inference(avatar_contradiction_clause, [], [f2902])).
fof(f2902, plain, ($false | (~ spl42_113 | ~ spl42_116)), inference(subsumption_resolution, [], [f2901, f308])).
fof(f2901, plain, ((e20 = e23) | (~ spl42_113 | ~ spl42_116)), inference(forward_demodulation, [], [f1137, f1125])).
fof(f2887, plain, (~ spl42_95 | ~ spl42_83), inference(avatar_split_clause, [], [f2886, f995, f1046])).
fof(f2886, plain, (~ (e22 = op2(e22, e20)) | ~ spl42_83), inference(forward_demodulation, [], [f290, f997])).
fof(f290, plain, ~ (op2(e22, e20) = op2(e22, e23)), inference(cnf_transformation, [], [f6])).
fof(f2885, plain, (~ spl42_94 | ~ spl42_110), inference(avatar_split_clause, [], [f2884, f1110, f1042])).
fof(f2884, plain, (~ (e21 = op2(e22, e20)) | ~ spl42_110), inference(forward_demodulation, [], [f255, f1112])).
fof(f2870, plain, (~ spl42_74 | ~ spl42_192), inference(avatar_split_clause, [], [f2869, f1727, f957])).
fof(f957, plain, (spl42_74 <=> (e21 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_74])])).
fof(f2869, plain, (~ (e21 = op2(e23, e21)) | ~ spl42_192), inference(forward_demodulation, [], [f1708, f1728])).
fof(f1708, plain, ~ (op2(e23, e21) = h4(e11)), inference(backward_demodulation, [], [f298, f548])).
fof(f298, plain, ~ (op2(e23, e21) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2865, plain, (spl42_75 | ~ spl42_192), inference(avatar_contradiction_clause, [], [f2864])).
fof(f2864, plain, ($false | (spl42_75 | ~ spl42_192)), inference(subsumption_resolution, [], [f2732, f962])).
fof(f962, plain, (~ (e22 = op2(e23, e21)) | spl42_75), inference(avatar_component_clause, [], [f961])).
fof(f2732, plain, ((e22 = op2(e23, e21)) | ~ spl42_192), inference(backward_demodulation, [], [f1710, f1728])).
fof(f2858, plain, (~ spl42_64 | ~ spl42_16), inference(avatar_split_clause, [], [f2857, f678, f882])).
fof(f882, plain, (spl42_64 <=> (op1(e10, e10) = e13)), introduced(avatar_definition, [new_symbols(naming, [spl42_64])])).
fof(f2857, plain, (~ (op1(e10, e10) = e13) | ~ spl42_16), inference(forward_demodulation, [], [f206, f680])).
fof(f206, plain, ~ (op1(e10, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2854, plain, (~ spl42_36 | ~ spl42_52), inference(avatar_split_clause, [], [f2853, f831, f763])).
fof(f2853, plain, (~ (e13 = op1(e11, e13)) | ~ spl42_52), inference(forward_demodulation, [], [f222, f833])).
fof(f833, plain, ((e13 = op1(e10, e13)) | ~ spl42_52), inference(avatar_component_clause, [], [f831])).
fof(f2843, plain, (~ spl42_47 | ~ spl42_31), inference(avatar_split_clause, [], [f2842, f742, f810])).
fof(f2842, plain, (~ (e12 = op1(e11, e10)) | ~ spl42_31), inference(forward_demodulation, [], [f207, f744])).
fof(f207, plain, ~ (op1(e11, e10) = op1(e12, e10)), inference(cnf_transformation, [], [f5])).
fof(f2841, plain, (~ spl42_48 | ~ spl42_16), inference(avatar_split_clause, [], [f2840, f678, f814])).
fof(f814, plain, (spl42_48 <=> (e13 = op1(e11, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_48])])).
fof(f2840, plain, (~ (e13 = op1(e11, e10)) | ~ spl42_16), inference(forward_demodulation, [], [f208, f680])).
fof(f208, plain, ~ (op1(e11, e10) = op1(e13, e10)), inference(cnf_transformation, [], [f5])).
fof(f2839, plain, (~ spl42_44 | ~ spl42_28), inference(avatar_split_clause, [], [f2838, f729, f797])).
fof(f2838, plain, (~ (e13 = op1(e11, e11)) | ~ spl42_28), inference(forward_demodulation, [], [f213, f731])).
fof(f2834, plain, (~ spl42_33 | ~ spl42_17), inference(avatar_split_clause, [], [f2833, f683, f751])).
fof(f2833, plain, (~ (e10 = op1(e11, e13)) | ~ spl42_17), inference(forward_demodulation, [], [f225, f685])).
fof(f2814, plain, (~ spl42_2 | ~ spl42_16 | ~ spl42_159), inference(avatar_contradiction_clause, [], [f2813])).
fof(f2813, plain, ($false | (~ spl42_2 | ~ spl42_16 | ~ spl42_159)), inference(subsumption_resolution, [], [f2812, f304])).
fof(f304, plain, ~ (e11 = e13), inference(cnf_transformation, [], [f7])).
fof(f2812, plain, ((e11 = e13) | (~ spl42_2 | ~ spl42_16 | ~ spl42_159)), inference(forward_demodulation, [], [f2811, f621])).
fof(f2811, plain, ((e13 = op1(e13, e13)) | (~ spl42_16 | ~ spl42_159)), inference(backward_demodulation, [], [f1444, f680])).
fof(f1444, plain, ((op1(e13, e10) = op1(op1(e13, e10), e13)) | ~ spl42_159), inference(avatar_component_clause, [], [f1442])).
fof(f1442, plain, (spl42_159 <=> (op1(e13, e10) = op1(op1(e13, e10), e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_159])])).
fof(f2797, plain, (~ spl42_51 | ~ spl42_52), inference(avatar_contradiction_clause, [], [f2796])).
fof(f2796, plain, ($false | (~ spl42_51 | ~ spl42_52)), inference(subsumption_resolution, [], [f2795, f305])).
fof(f2795, plain, ((e12 = e13) | (~ spl42_51 | ~ spl42_52)), inference(forward_demodulation, [], [f833, f829])).
fof(f2794, plain, (~ spl42_30 | ~ spl42_55 | ~ spl42_148), inference(avatar_contradiction_clause, [], [f2793])).
fof(f2793, plain, ($false | (~ spl42_30 | ~ spl42_55 | ~ spl42_148)), inference(subsumption_resolution, [], [f2792, f303])).
fof(f2792, plain, ((e11 = e12) | (~ spl42_30 | ~ spl42_55 | ~ spl42_148)), inference(forward_demodulation, [], [f2791, f740])).
fof(f2791, plain, ((e12 = op1(e12, e10)) | (~ spl42_55 | ~ spl42_148)), inference(backward_demodulation, [], [f1398, f846])).
fof(f2789, plain, (~ spl42_57 | ~ spl42_58), inference(avatar_contradiction_clause, [], [f2788])).
fof(f2788, plain, ($false | (~ spl42_57 | ~ spl42_58)), inference(subsumption_resolution, [], [f2787, f300])).
fof(f2787, plain, ((e10 = e11) | (~ spl42_57 | ~ spl42_58)), inference(forward_demodulation, [], [f859, f855])).
fof(f2771, plain, (~ spl42_85 | spl42_203), inference(avatar_contradiction_clause, [], [f2770])).
fof(f2770, plain, ($false | (~ spl42_85 | spl42_203)), inference(subsumption_resolution, [], [f2768, f1786])).
fof(f1786, plain, (~ (e20 = h3(e11)) | spl42_203), inference(avatar_component_clause, [], [f1784])).
fof(f2768, plain, ((e20 = h3(e11)) | ~ spl42_85), inference(backward_demodulation, [], [f544, f1006])).
fof(f1006, plain, ((e20 = op2(e22, e22)) | ~ spl42_85), inference(avatar_component_clause, [], [f1004])).
fof(f1004, plain, (spl42_85 <=> (e20 = op2(e22, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_85])])).
fof(f2759, plain, (spl42_275 | ~ spl42_108), inference(avatar_split_clause, [], [f2758, f1101, f2218])).
fof(f2758, plain, ((e23 = h2(e11)) | ~ spl42_108), inference(backward_demodulation, [], [f540, f1103])).
fof(f2749, plain, (~ spl42_118 | ~ spl42_119), inference(avatar_contradiction_clause, [], [f2748])).
fof(f2748, plain, ($false | (~ spl42_118 | ~ spl42_119)), inference(subsumption_resolution, [], [f2747, f309])).
fof(f2747, plain, ((e21 = e22) | (~ spl42_118 | ~ spl42_119)), inference(backward_demodulation, [], [f1150, f1146])).
fof(f2712, plain, (~ spl42_121 | ~ spl42_89), inference(avatar_split_clause, [], [f2711, f1021, f1157])).
fof(f2711, plain, (~ (e20 = op2(e20, e21)) | ~ spl42_89), inference(forward_demodulation, [], [f259, f1023])).
fof(f1023, plain, ((e20 = op2(e22, e21)) | ~ spl42_89), inference(avatar_component_clause, [], [f1021])).
fof(f259, plain, ~ (op2(e20, e21) = op2(e22, e21)), inference(cnf_transformation, [], [f6])).
fof(f2694, plain, (~ spl42_192 | ~ spl42_114), inference(avatar_split_clause, [], [f2693, f1127, f1727])).
fof(f1127, plain, (spl42_114 <=> (e21 = op2(e20, e23))), introduced(avatar_definition, [new_symbols(naming, [spl42_114])])).
fof(f2693, plain, (~ (e21 = h4(e11)) | ~ spl42_114), inference(forward_demodulation, [], [f1704, f1129])).
fof(f1129, plain, ((e21 = op2(e20, e23)) | ~ spl42_114), inference(avatar_component_clause, [], [f1127])).
fof(f1704, plain, ~ (op2(e20, e23) = h4(e11)), inference(backward_demodulation, [], [f272, f548])).
fof(f272, plain, ~ (op2(e20, e23) = op2(e23, e23)), inference(cnf_transformation, [], [f6])).
fof(f2673, plain, (~ spl42_69 | ~ spl42_72), inference(avatar_contradiction_clause, [], [f2672])).
fof(f2672, plain, ($false | (~ spl42_69 | ~ spl42_72)), inference(subsumption_resolution, [], [f2671, f308])).
fof(f2671, plain, ((e20 = e23) | (~ spl42_69 | ~ spl42_72)), inference(forward_demodulation, [], [f950, f938])).
fof(f950, plain, ((e23 = op2(e23, e22)) | ~ spl42_72), inference(avatar_component_clause, [], [f948])).
fof(f948, plain, (spl42_72 <=> (e23 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_72])])).
fof(f2666, plain, (~ spl42_61 | ~ spl42_57), inference(avatar_split_clause, [], [f2665, f853, f870])).
fof(f2665, plain, (~ (e10 = op1(e10, e10)) | ~ spl42_57), inference(forward_demodulation, [], [f228, f855])).
fof(f2660, plain, (~ spl42_53 | ~ spl42_5), inference(avatar_split_clause, [], [f2659, f632, f836])).
fof(f2659, plain, (~ (e10 = op1(e10, e12)) | ~ spl42_5), inference(forward_demodulation, [], [f218, f634])).
fof(f218, plain, ~ (op1(e10, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2656, plain, (~ spl42_43 | ~ spl42_11), inference(avatar_split_clause, [], [f2655, f657, f793])).
fof(f793, plain, (spl42_43 <=> (e12 = op1(e11, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_43])])).
fof(f2655, plain, (~ (e12 = op1(e11, e11)) | ~ spl42_11), inference(forward_demodulation, [], [f214, f659])).
fof(f214, plain, ~ (op1(e11, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2650, plain, (~ spl42_27 | ~ spl42_11), inference(avatar_split_clause, [], [f2649, f657, f725])).
fof(f2649, plain, (~ (e12 = op1(e12, e11)) | ~ spl42_11), inference(forward_demodulation, [], [f215, f659])).
fof(f215, plain, ~ (op1(e12, e11) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2646, plain, (~ spl42_21 | ~ spl42_5), inference(avatar_split_clause, [], [f2645, f632, f700])).
fof(f700, plain, (spl42_21 <=> (e10 = op1(e12, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_21])])).
fof(f2645, plain, (~ (e10 = op1(e12, e12)) | ~ spl42_5), inference(forward_demodulation, [], [f221, f634])).
fof(f221, plain, ~ (op1(e12, e12) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2639, plain, (~ spl42_15 | ~ spl42_11), inference(avatar_split_clause, [], [f2638, f657, f674])).
fof(f674, plain, (spl42_15 <=> (e12 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_15])])).
fof(f2638, plain, (~ (e12 = op1(e13, e10)) | ~ spl42_11), inference(forward_demodulation, [], [f246, f659])).
fof(f246, plain, ~ (op1(e13, e10) = op1(e13, e11)), inference(cnf_transformation, [], [f5])).
fof(f2637, plain, (~ spl42_13 | ~ spl42_5), inference(avatar_split_clause, [], [f2636, f632, f666])).
fof(f666, plain, (spl42_13 <=> (e10 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_13])])).
fof(f2636, plain, (~ (e10 = op1(e13, e10)) | ~ spl42_5), inference(forward_demodulation, [], [f247, f634])).
fof(f247, plain, ~ (op1(e13, e10) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2635, plain, (~ spl42_14 | ~ spl42_2), inference(avatar_split_clause, [], [f2634, f619, f670])).
fof(f670, plain, (spl42_14 <=> (e11 = op1(e13, e10))), introduced(avatar_definition, [new_symbols(naming, [spl42_14])])).
fof(f2634, plain, (~ (e11 = op1(e13, e10)) | ~ spl42_2), inference(forward_demodulation, [], [f248, f621])).
fof(f248, plain, ~ (op1(e13, e10) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2633, plain, (~ spl42_9 | ~ spl42_5), inference(avatar_split_clause, [], [f2632, f632, f649])).
fof(f649, plain, (spl42_9 <=> (e10 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_9])])).
fof(f2632, plain, (~ (e10 = op1(e13, e11)) | ~ spl42_5), inference(forward_demodulation, [], [f249, f634])).
fof(f249, plain, ~ (op1(e13, e11) = op1(e13, e12)), inference(cnf_transformation, [], [f5])).
fof(f2631, plain, (~ spl42_10 | ~ spl42_2), inference(avatar_split_clause, [], [f2630, f619, f653])).
fof(f653, plain, (spl42_10 <=> (e11 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_10])])).
fof(f2630, plain, (~ (e11 = op1(e13, e11)) | ~ spl42_2), inference(forward_demodulation, [], [f250, f621])).
fof(f250, plain, ~ (op1(e13, e11) = op1(e13, e13)), inference(cnf_transformation, [], [f5])).
fof(f2629, plain, (spl42_11 | ~ spl42_2), inference(avatar_split_clause, [], [f2618, f619, f657])).
fof(f2618, plain, ((e12 = op1(e13, e11)) | ~ spl42_2), inference(backward_demodulation, [], [f530, f621])).
fof(f530, plain, (e12 = op1(e13, op1(e13, e13))), inference(cnf_transformation, [], [f12])).
fof(f12, plain, ((e12 = op1(e13, op1(e13, e13))) & (e11 = op1(e13, e13)) & (e10 = op1(e13, op1(e13, op1(e13, e13))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax12)).
fof(f2628, plain, (~ spl42_5 | ~ spl42_8), inference(avatar_contradiction_clause, [], [f2627])).
fof(f2627, plain, ($false | (~ spl42_5 | ~ spl42_8)), inference(subsumption_resolution, [], [f2626, f302])).
fof(f2626, plain, ((e10 = e13) | (~ spl42_5 | ~ spl42_8)), inference(forward_demodulation, [], [f646, f634])).
fof(f646, plain, ((e13 = op1(e13, e12)) | ~ spl42_8), inference(avatar_component_clause, [], [f644])).
fof(f644, plain, (spl42_8 <=> (e13 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_8])])).
fof(f2617, plain, (~ spl42_5 | ~ spl42_7), inference(avatar_contradiction_clause, [], [f2616])).
fof(f2616, plain, ($false | (~ spl42_5 | ~ spl42_7)), inference(subsumption_resolution, [], [f2615, f301])).
fof(f2615, plain, ((e10 = e12) | (~ spl42_5 | ~ spl42_7)), inference(backward_demodulation, [], [f642, f634])).
fof(f642, plain, ((e12 = op1(e13, e12)) | ~ spl42_7), inference(avatar_component_clause, [], [f640])).
fof(f640, plain, (spl42_7 <=> (e12 = op1(e13, e12))), introduced(avatar_definition, [new_symbols(naming, [spl42_7])])).
fof(f2606, plain, (~ spl42_11 | ~ spl42_12), inference(avatar_contradiction_clause, [], [f2605])).
fof(f2605, plain, ($false | (~ spl42_11 | ~ spl42_12)), inference(subsumption_resolution, [], [f2604, f305])).
fof(f2604, plain, ((e12 = e13) | (~ spl42_11 | ~ spl42_12)), inference(backward_demodulation, [], [f663, f659])).
fof(f663, plain, ((e13 = op1(e13, e11)) | ~ spl42_12), inference(avatar_component_clause, [], [f661])).
fof(f661, plain, (spl42_12 <=> (e13 = op1(e13, e11))), introduced(avatar_definition, [new_symbols(naming, [spl42_12])])).
fof(f2589, plain, (~ spl42_17 | ~ spl42_19), inference(avatar_contradiction_clause, [], [f2588])).
fof(f2588, plain, ($false | (~ spl42_17 | ~ spl42_19)), inference(subsumption_resolution, [], [f2587, f301])).
fof(f2587, plain, ((e10 = e12) | (~ spl42_17 | ~ spl42_19)), inference(backward_demodulation, [], [f693, f685])).
fof(f2564, plain, (~ spl42_35 | ~ spl42_36), inference(avatar_contradiction_clause, [], [f2563])).
fof(f2563, plain, ($false | (~ spl42_35 | ~ spl42_36)), inference(subsumption_resolution, [], [f2562, f305])).
fof(f2562, plain, ((e12 = e13) | (~ spl42_35 | ~ spl42_36)), inference(backward_demodulation, [], [f765, f761])).
fof(f2555, plain, (~ spl42_22 | ~ spl42_38), inference(avatar_split_clause, [], [f2552, f772, f704])).
fof(f2552, plain, (~ (e11 = op1(e12, e12)) | ~ spl42_38), inference(backward_demodulation, [], [f219, f774])).
fof(f2551, plain, (~ spl42_33 | ~ spl42_41), inference(avatar_split_clause, [], [f2547, f785, f751])).
fof(f2547, plain, (~ (e10 = op1(e11, e13)) | ~ spl42_41), inference(backward_demodulation, [], [f238, f787])).
fof(f2510, plain, (spl42_61 | ~ spl42_57 | ~ spl42_152), inference(avatar_split_clause, [], [f2504, f1413, f853, f870])).
fof(f2504, plain, ((e10 = op1(e10, e10)) | (~ spl42_57 | ~ spl42_152)), inference(backward_demodulation, [], [f1415, f855])).
fof(f1415, plain, ((op1(e10, e11) = op1(op1(e10, e11), e10)) | ~ spl42_152), inference(avatar_component_clause, [], [f1413])).
fof(f2505, plain, (~ spl42_41 | ~ spl42_57), inference(avatar_split_clause, [], [f2499, f853, f785])).
fof(f2499, plain, (~ (e10 = op1(e11, e11)) | ~ spl42_57), inference(backward_demodulation, [], [f210, f855])).
fof(f2484, plain, (spl42_192 | ~ spl42_66), inference(avatar_split_clause, [], [f2483, f923, f1727])).
fof(f2483, plain, ((e21 = h4(e11)) | ~ spl42_66), inference(backward_demodulation, [], [f548, f925])).
fof(f2481, plain, (~ spl42_69 | ~ spl42_70), inference(avatar_contradiction_clause, [], [f2480])).
fof(f2480, plain, ($false | (~ spl42_69 | ~ spl42_70)), inference(subsumption_resolution, [], [f2479, f306])).
fof(f2479, plain, ((e20 = e21) | (~ spl42_69 | ~ spl42_70)), inference(backward_demodulation, [], [f942, f938])).
fof(f942, plain, ((e21 = op2(e23, e22)) | ~ spl42_70), inference(avatar_component_clause, [], [f940])).
fof(f940, plain, (spl42_70 <=> (e21 = op2(e23, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_70])])).
fof(f2465, plain, (~ spl42_75 | ~ spl42_76), inference(avatar_contradiction_clause, [], [f2464])).
fof(f2464, plain, ($false | (~ spl42_75 | ~ spl42_76)), inference(subsumption_resolution, [], [f2463, f311])).
fof(f2463, plain, ((e22 = e23) | (~ spl42_75 | ~ spl42_76)), inference(backward_demodulation, [], [f967, f963])).
fof(f967, plain, ((e23 = op2(e23, e21)) | ~ spl42_76), inference(avatar_component_clause, [], [f965])).
fof(f965, plain, (spl42_76 <=> (e23 = op2(e23, e21))), introduced(avatar_definition, [new_symbols(naming, [spl42_76])])).
fof(f2456, plain, (~ spl42_75 | ~ spl42_79), inference(avatar_split_clause, [], [f2454, f978, f961])).
fof(f978, plain, (spl42_79 <=> (e22 = op2(e23, e20))), introduced(avatar_definition, [new_symbols(naming, [spl42_79])])).
fof(f2454, plain, (~ (e22 = op2(e23, e21)) | ~ spl42_79), inference(backward_demodulation, [], [f294, f980])).
fof(f980, plain, ((e22 = op2(e23, e20)) | ~ spl42_79), inference(avatar_component_clause, [], [f978])).
fof(f294, plain, ~ (op2(e23, e20) = op2(e23, e21)), inference(cnf_transformation, [], [f6])).
fof(f2450, plain, (~ spl42_81 | ~ spl42_83), inference(avatar_contradiction_clause, [], [f2449])).
fof(f2449, plain, ($false | (~ spl42_81 | ~ spl42_83)), inference(subsumption_resolution, [], [f2448, f307])).
fof(f2448, plain, ((e20 = e22) | (~ spl42_81 | ~ spl42_83)), inference(backward_demodulation, [], [f997, f989])).
fof(f2444, plain, (spl42_256 | ~ spl42_88), inference(avatar_split_clause, [], [f2443, f1016, f2104])).
fof(f2443, plain, ((e23 = h3(e11)) | ~ spl42_88), inference(backward_demodulation, [], [f544, f1018])).
fof(f2420, plain, (~ spl42_99 | ~ spl42_100), inference(avatar_contradiction_clause, [], [f2419])).
fof(f2419, plain, ($false | (~ spl42_99 | ~ spl42_100)), inference(subsumption_resolution, [], [f2418, f311])).
fof(f2418, plain, ((e22 = e23) | (~ spl42_99 | ~ spl42_100)), inference(backward_demodulation, [], [f1069, f1065])).
fof(f2417, plain, (~ spl42_84 | ~ spl42_100), inference(avatar_split_clause, [], [f2414, f1067, f999])).
fof(f2414, plain, (~ (e23 = op2(e22, e23)) | ~ spl42_100), inference(backward_demodulation, [], [f273, f1069])).
fof(f2408, plain, (~ spl42_196 | ~ spl42_101), inference(avatar_split_clause, [], [f2404, f1072, f1748])).
fof(f1072, plain, (spl42_101 <=> (e20 = op2(e21, e22))), introduced(avatar_definition, [new_symbols(naming, [spl42_101])])).
fof(f2404, plain, (~ (e20 = h4(e10)) | ~ spl42_101), inference(backward_demodulation, [], [f1715, f1074])).
fof(f1074, plain, ((e20 = op2(e21, e22)) | ~ spl42_101), inference(avatar_component_clause, [], [f1072])).
fof(f1715, plain, ~ (op2(e21, e22) = h4(e10)), inference(backward_demodulation, [], [f268, f1712])).
fof(f268, plain, ~ (op2(e21, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2403, plain, (spl42_211 | ~ spl42_105), inference(avatar_split_clause, [], [f2402, f1089, f1825])).
fof(f2402, plain, ((e20 = h2(e11)) | ~ spl42_105), inference(backward_demodulation, [], [f540, f1091])).
fof(f2397, plain, (~ spl42_211 | ~ spl42_109), inference(avatar_split_clause, [], [f2391, f1106, f1825])).
fof(f2391, plain, (~ (e20 = h2(e11)) | ~ spl42_109), inference(backward_demodulation, [], [f1688, f1108])).
fof(f1688, plain, ~ (op2(e21, e20) = h2(e11)), inference(backward_demodulation, [], [f282, f540])).
fof(f282, plain, ~ (op2(e21, e20) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f2378, plain, (~ spl42_196 | ~ spl42_117), inference(avatar_split_clause, [], [f2373, f1140, f1748])).
fof(f2373, plain, (~ (e20 = h4(e10)) | ~ spl42_117), inference(backward_demodulation, [], [f1714, f1142])).
fof(f1142, plain, ((e20 = op2(e20, e22)) | ~ spl42_117), inference(avatar_component_clause, [], [f1140])).
fof(f1714, plain, ~ (op2(e20, e22) = h4(e10)), inference(backward_demodulation, [], [f266, f1712])).
fof(f266, plain, ~ (op2(e20, e22) = op2(e23, e22)), inference(cnf_transformation, [], [f6])).
fof(f2367, plain, (~ spl42_211 | ~ spl42_121), inference(avatar_split_clause, [], [f2361, f1157, f1825])).
fof(f2361, plain, (~ (e20 = h2(e11)) | ~ spl42_121), inference(backward_demodulation, [], [f1685, f1159])).
fof(f1685, plain, ~ (op2(e20, e21) = h2(e11)), inference(backward_demodulation, [], [f258, f540])).
fof(f258, plain, ~ (op2(e20, e21) = op2(e21, e21)), inference(cnf_transformation, [], [f6])).
fof(f2365, plain, (~ spl42_113 | ~ spl42_121), inference(avatar_split_clause, [], [f2358, f1157, f1123])).
fof(f2358, plain, (~ (e20 = op2(e20, e23)) | ~ spl42_121), inference(backward_demodulation, [], [f280, f1159])).
fof(f2354, plain, (spl42_294 | ~ spl42_128), inference(avatar_split_clause, [], [f2353, f1186, f2332])).
fof(f1186, plain, (spl42_128 <=> (op2(e20, e20) = e23)), introduced(avatar_definition, [new_symbols(naming, [spl42_128])])).
fof(f2353, plain, ((e23 = h1(e11)) | ~ spl42_128), inference(backward_demodulation, [], [f536, f1188])).
fof(f1188, plain, ((op2(e20, e20) = e23) | ~ spl42_128), inference(avatar_component_clause, [], [f1186])).
fof(f1959, plain, (~ spl42_221 | ~ spl42_222 | ~ spl42_223 | ~ spl42_224 | spl42_194 | spl42_191 | ~ spl42_225 | ~ spl42_226 | ~ spl42_227 | ~ spl42_228 | ~ spl42_229 | ~ spl42_230 | ~ spl42_231 | ~ spl42_232 | ~ spl42_233 | ~ spl42_234 | ~ spl42_235 | ~ spl42_236), inference(avatar_split_clause, [], [f1894, f1956, f1952, f1948, f1944, f1940, f1936, f1932, f1928, f1924, f1920, f1916, f1912, f1723, f1739, f1908, f1904, f1900, f1896])).
fof(f1739, plain, (spl42_194 <=> sP39), introduced(avatar_definition, [new_symbols(naming, [spl42_194])])).
fof(f1723, plain, (spl42_191 <=> sP40), introduced(avatar_definition, [new_symbols(naming, [spl42_191])])).
fof(f1894, plain, (~ (h4(op1(e10, e12)) = op2(h4(e10), e22)) | ~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1893, f1703])).
fof(f1893, plain, (~ (h4(op1(e10, e13)) = op2(h4(e10), e23)) | ~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1892, f546])).
fof(f1892, plain, (~ (h4(op1(e11, e12)) = op2(h4(e11), e22)) | ~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1891, f1703])).
fof(f1891, plain, (~ (h4(op1(e11, e13)) = op2(h4(e11), e23)) | ~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1890, f546])).
fof(f1890, plain, (~ (h4(op1(e12, e10)) = op2(e22, h4(e10))) | ~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1889, f1703])).
fof(f1889, plain, (~ (h4(op1(e12, e11)) = op2(e22, h4(e11))) | ~ (h3(e11) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1888, f1703])).
fof(f1888, plain, (~ (h3(e11) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1887, f544])).
fof(f1887, plain, (~ (op2(e22, e22) = h4(op1(e12, e12))) | ~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1886, f1703])).
fof(f1886, plain, (~ (op2(e22, e23) = h4(op1(e12, e13))) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1885, f1703])).
fof(f1885, plain, (~ (h4(op1(e12, e13)) = op2(h4(e12), e23)) | ~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1884, f546])).
fof(f1884, plain, (~ (h4(op1(e13, e10)) = op2(e23, h4(e10))) | ~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1883, f546])).
fof(f1883, plain, (~ (e22 = h4(op1(e13, e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1882, f1710])).
fof(f1882, plain, (~ (h4(op1(e13, e11)) = op2(e23, h4(e11))) | ~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1881, f546])).
fof(f1881, plain, (~ (h4(e10) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1880, f1712])).
fof(f1880, plain, (~ (op2(e23, e22) = h4(op1(e13, e12))) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1879, f546])).
fof(f1879, plain, (~ (h4(op1(e13, e12)) = op2(h4(e13), e22)) | ~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1878, f1703])).
fof(f1878, plain, (~ (h4(e11) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1877, f548])).
fof(f1877, plain, (~ (op2(e23, e23) = h4(op1(e13, e13))) | sP40 | sP39 | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(forward_demodulation, [], [f1876, f546])).
fof(f1876, plain, (sP40 | sP39 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f1875, f1719])).
fof(f1719, plain, ~ sP41, inference(subsumption_resolution, [], [f552, f1703])).
fof(f552, plain, (~ (e22 = h4(e12)) | ~ sP41), inference(cnf_transformation, [], [f96])).
fof(f96, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP41), inference(nnf_transformation, [], [f64])).
fof(f64, plain, ((~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | ~ sP41), inference(usedef, [], [e64])).
fof(e64, plain, (sP41 <=> (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP41])])).
fof(f1875, plain, (sP41 | sP40 | sP39 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(subsumption_resolution, [], [f613, f546])).
fof(f613, plain, (~ (e23 = h4(e13)) | sP41 | sP40 | sP39 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))), inference(cnf_transformation, [], [f65])).
fof(f65, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | sP41 | sP40 | sP39 | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | sP38 | sP37 | sP36 | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | sP35 | sP34 | sP33 | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | sP32 | sP31 | sP30 | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(definition_folding, [], [f20, e64, e63, e62, e61, e60, e59, e58, e57, e56, e55, e54, e53])).
fof(f53, plain, ((~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ sP30), inference(usedef, [], [e53])).
fof(e53, plain, (sP30 <=> (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP30])])).
fof(f54, plain, ((~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | ~ sP31), inference(usedef, [], [e54])).
fof(e54, plain, (sP31 <=> (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP31])])).
fof(f55, plain, ((~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | ~ sP32), inference(usedef, [], [e55])).
fof(e55, plain, (sP32 <=> (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP32])])).
fof(f56, plain, ((~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ sP33), inference(usedef, [], [e56])).
fof(e56, plain, (sP33 <=> (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP33])])).
fof(f57, plain, ((~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | ~ sP34), inference(usedef, [], [e57])).
fof(e57, plain, (sP34 <=> (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP34])])).
fof(f58, plain, ((~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | ~ sP35), inference(usedef, [], [e58])).
fof(e58, plain, (sP35 <=> (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP35])])).
fof(f59, plain, ((~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ sP36), inference(usedef, [], [e59])).
fof(e59, plain, (sP36 <=> (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP36])])).
fof(f60, plain, ((~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | ~ sP37), inference(usedef, [], [e60])).
fof(e60, plain, (sP37 <=> (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP37])])).
fof(f61, plain, ((~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | ~ sP38), inference(usedef, [], [e61])).
fof(e61, plain, (sP38 <=> (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP38])])).
fof(f62, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP39), inference(usedef, [], [e62])).
fof(e62, plain, (sP39 <=> (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP39])])).
fof(f63, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP40), inference(usedef, [], [e63])).
fof(e63, plain, (sP40 <=> (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP40])])).
fof(f20, plain, (((~ (e23 = h4(e13)) & ~ (e23 = h4(e12)) & ~ (e23 = h4(e11)) & ~ (e23 = h4(e10))) | (~ (e22 = h4(e13)) & ~ (e22 = h4(e12)) & ~ (e22 = h4(e11)) & ~ (e22 = h4(e10))) | (~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | (~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) | ~ (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) | ~ (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) | ~ (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) | ~ (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) | ~ (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) | ~ (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) | ~ (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) | ~ (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) | ~ (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) | ~ (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) | ~ (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) | ~ (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) | ~ (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) | ~ (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) | ~ (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) & ((~ (e23 = h3(e13)) & ~ (e23 = h3(e12)) & ~ (e23 = h3(e11)) & ~ (e23 = h3(e10))) | (~ (e22 = h3(e13)) & ~ (e22 = h3(e12)) & ~ (e22 = h3(e11)) & ~ (e22 = h3(e10))) | (~ (e21 = h3(e13)) & ~ (e21 = h3(e12)) & ~ (e21 = h3(e11)) & ~ (e21 = h3(e10))) | (~ (e20 = h3(e13)) & ~ (e20 = h3(e12)) & ~ (e20 = h3(e11)) & ~ (e20 = h3(e10))) | ~ (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) | ~ (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) | ~ (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) | ~ (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) | ~ (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) | ~ (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) | ~ (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) | ~ (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) | ~ (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) | ~ (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) | ~ (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) | ~ (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) | ~ (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) | ~ (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) | ~ (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) | ~ (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) & ((~ (e23 = h2(e13)) & ~ (e23 = h2(e12)) & ~ (e23 = h2(e11)) & ~ (e23 = h2(e10))) | (~ (e22 = h2(e13)) & ~ (e22 = h2(e12)) & ~ (e22 = h2(e11)) & ~ (e22 = h2(e10))) | (~ (e21 = h2(e13)) & ~ (e21 = h2(e12)) & ~ (e21 = h2(e11)) & ~ (e21 = h2(e10))) | (~ (e20 = h2(e13)) & ~ (e20 = h2(e12)) & ~ (e20 = h2(e11)) & ~ (e20 = h2(e10))) | ~ (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) | ~ (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) | ~ (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) | ~ (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) | ~ (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) | ~ (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) | ~ (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) | ~ (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) | ~ (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) | ~ (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) | ~ (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) | ~ (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) | ~ (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) | ~ (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) | ~ (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) | ~ (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) & ((~ (e23 = h1(e13)) & ~ (e23 = h1(e12)) & ~ (e23 = h1(e11)) & ~ (e23 = h1(e10))) | (~ (e22 = h1(e13)) & ~ (e22 = h1(e12)) & ~ (e22 = h1(e11)) & ~ (e22 = h1(e10))) | (~ (e21 = h1(e13)) & ~ (e21 = h1(e12)) & ~ (e21 = h1(e11)) & ~ (e21 = h1(e10))) | (~ (e20 = h1(e13)) & ~ (e20 = h1(e12)) & ~ (e20 = h1(e11)) & ~ (e20 = h1(e10))) | ~ (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) | ~ (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) | ~ (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) | ~ (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) | ~ (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) | ~ (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) | ~ (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) | ~ (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) | ~ (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) | ~ (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) | ~ (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) | ~ (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) | ~ (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) | ~ (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) | ~ (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) | ~ (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(ennf_transformation, [], [f19])).
fof(f19, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), inference(negated_conjecture, [], [f18])).
fof(f18, plain, ~ ((((e23 = h4(e13)) | (e23 = h4(e12)) | (e23 = h4(e11)) | (e23 = h4(e10))) & ((e22 = h4(e13)) | (e22 = h4(e12)) | (e22 = h4(e11)) | (e22 = h4(e10))) & ((e21 = h4(e13)) | (e21 = h4(e12)) | (e21 = h4(e11)) | (e21 = h4(e10))) & ((e20 = h4(e13)) | (e20 = h4(e12)) | (e20 = h4(e11)) | (e20 = h4(e10))) & (h4(op1(e13, e13)) = op2(h4(e13), h4(e13))) & (h4(op1(e13, e12)) = op2(h4(e13), h4(e12))) & (h4(op1(e13, e11)) = op2(h4(e13), h4(e11))) & (h4(op1(e13, e10)) = op2(h4(e13), h4(e10))) & (h4(op1(e12, e13)) = op2(h4(e12), h4(e13))) & (h4(op1(e12, e12)) = op2(h4(e12), h4(e12))) & (h4(op1(e12, e11)) = op2(h4(e12), h4(e11))) & (h4(op1(e12, e10)) = op2(h4(e12), h4(e10))) & (h4(op1(e11, e13)) = op2(h4(e11), h4(e13))) & (h4(op1(e11, e12)) = op2(h4(e11), h4(e12))) & (h4(op1(e11, e11)) = op2(h4(e11), h4(e11))) & (h4(op1(e11, e10)) = op2(h4(e11), h4(e10))) & (h4(op1(e10, e13)) = op2(h4(e10), h4(e13))) & (h4(op1(e10, e12)) = op2(h4(e10), h4(e12))) & (h4(op1(e10, e11)) = op2(h4(e10), h4(e11))) & (h4(op1(e10, e10)) = op2(h4(e10), h4(e10)))) | (((e23 = h3(e13)) | (e23 = h3(e12)) | (e23 = h3(e11)) | (e23 = h3(e10))) & ((e22 = h3(e13)) | (e22 = h3(e12)) | (e22 = h3(e11)) | (e22 = h3(e10))) & ((e21 = h3(e13)) | (e21 = h3(e12)) | (e21 = h3(e11)) | (e21 = h3(e10))) & ((e20 = h3(e13)) | (e20 = h3(e12)) | (e20 = h3(e11)) | (e20 = h3(e10))) & (h3(op1(e13, e13)) = op2(h3(e13), h3(e13))) & (h3(op1(e13, e12)) = op2(h3(e13), h3(e12))) & (h3(op1(e13, e11)) = op2(h3(e13), h3(e11))) & (h3(op1(e13, e10)) = op2(h3(e13), h3(e10))) & (h3(op1(e12, e13)) = op2(h3(e12), h3(e13))) & (h3(op1(e12, e12)) = op2(h3(e12), h3(e12))) & (h3(op1(e12, e11)) = op2(h3(e12), h3(e11))) & (h3(op1(e12, e10)) = op2(h3(e12), h3(e10))) & (h3(op1(e11, e13)) = op2(h3(e11), h3(e13))) & (h3(op1(e11, e12)) = op2(h3(e11), h3(e12))) & (h3(op1(e11, e11)) = op2(h3(e11), h3(e11))) & (h3(op1(e11, e10)) = op2(h3(e11), h3(e10))) & (h3(op1(e10, e13)) = op2(h3(e10), h3(e13))) & (h3(op1(e10, e12)) = op2(h3(e10), h3(e12))) & (h3(op1(e10, e11)) = op2(h3(e10), h3(e11))) & (h3(op1(e10, e10)) = op2(h3(e10), h3(e10)))) | (((e23 = h2(e13)) | (e23 = h2(e12)) | (e23 = h2(e11)) | (e23 = h2(e10))) & ((e22 = h2(e13)) | (e22 = h2(e12)) | (e22 = h2(e11)) | (e22 = h2(e10))) & ((e21 = h2(e13)) | (e21 = h2(e12)) | (e21 = h2(e11)) | (e21 = h2(e10))) & ((e20 = h2(e13)) | (e20 = h2(e12)) | (e20 = h2(e11)) | (e20 = h2(e10))) & (h2(op1(e13, e13)) = op2(h2(e13), h2(e13))) & (h2(op1(e13, e12)) = op2(h2(e13), h2(e12))) & (h2(op1(e13, e11)) = op2(h2(e13), h2(e11))) & (h2(op1(e13, e10)) = op2(h2(e13), h2(e10))) & (h2(op1(e12, e13)) = op2(h2(e12), h2(e13))) & (h2(op1(e12, e12)) = op2(h2(e12), h2(e12))) & (h2(op1(e12, e11)) = op2(h2(e12), h2(e11))) & (h2(op1(e12, e10)) = op2(h2(e12), h2(e10))) & (h2(op1(e11, e13)) = op2(h2(e11), h2(e13))) & (h2(op1(e11, e12)) = op2(h2(e11), h2(e12))) & (h2(op1(e11, e11)) = op2(h2(e11), h2(e11))) & (h2(op1(e11, e10)) = op2(h2(e11), h2(e10))) & (h2(op1(e10, e13)) = op2(h2(e10), h2(e13))) & (h2(op1(e10, e12)) = op2(h2(e10), h2(e12))) & (h2(op1(e10, e11)) = op2(h2(e10), h2(e11))) & (h2(op1(e10, e10)) = op2(h2(e10), h2(e10)))) | (((e23 = h1(e13)) | (e23 = h1(e12)) | (e23 = h1(e11)) | (e23 = h1(e10))) & ((e22 = h1(e13)) | (e22 = h1(e12)) | (e22 = h1(e11)) | (e22 = h1(e10))) & ((e21 = h1(e13)) | (e21 = h1(e12)) | (e21 = h1(e11)) | (e21 = h1(e10))) & ((e20 = h1(e13)) | (e20 = h1(e12)) | (e20 = h1(e11)) | (e20 = h1(e10))) & (h1(op1(e13, e13)) = op2(h1(e13), h1(e13))) & (h1(op1(e13, e12)) = op2(h1(e13), h1(e12))) & (h1(op1(e13, e11)) = op2(h1(e13), h1(e11))) & (h1(op1(e13, e10)) = op2(h1(e13), h1(e10))) & (h1(op1(e12, e13)) = op2(h1(e12), h1(e13))) & (h1(op1(e12, e12)) = op2(h1(e12), h1(e12))) & (h1(op1(e12, e11)) = op2(h1(e12), h1(e11))) & (h1(op1(e12, e10)) = op2(h1(e12), h1(e10))) & (h1(op1(e11, e13)) = op2(h1(e11), h1(e13))) & (h1(op1(e11, e12)) = op2(h1(e11), h1(e12))) & (h1(op1(e11, e11)) = op2(h1(e11), h1(e11))) & (h1(op1(e11, e10)) = op2(h1(e11), h1(e10))) & (h1(op1(e10, e13)) = op2(h1(e10), h1(e13))) & (h1(op1(e10, e12)) = op2(h1(e10), h1(e12))) & (h1(op1(e10, e11)) = op2(h1(e10), h1(e11))) & (h1(op1(e10, e10)) = op2(h1(e10), h1(e10))))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', co1)).
fof(f1751, plain, (~ spl42_194 | ~ spl42_196), inference(avatar_split_clause, [], [f558, f1748, f1739])).
fof(f558, plain, (~ (e20 = h4(e10)) | ~ sP39), inference(cnf_transformation, [], [f98])).
fof(f98, plain, ((~ (e20 = h4(e13)) & ~ (e20 = h4(e12)) & ~ (e20 = h4(e11)) & ~ (e20 = h4(e10))) | ~ sP39), inference(nnf_transformation, [], [f62])).
fof(f1730, plain, (~ spl42_191 | ~ spl42_192), inference(avatar_split_clause, [], [f555, f1727, f1723])).
fof(f555, plain, (~ (e21 = h4(e11)) | ~ sP40), inference(cnf_transformation, [], [f97])).
fof(f97, plain, ((~ (e21 = h4(e13)) & ~ (e21 = h4(e12)) & ~ (e21 = h4(e11)) & ~ (e21 = h4(e10))) | ~ sP40), inference(nnf_transformation, [], [f63])).
fof(f1675, plain, spl42_69, inference(avatar_split_clause, [], [f1674, f936])).
fof(f1674, plain, (e20 = op2(e23, e22)), inference(forward_demodulation, [], [f531, f533])).
fof(f531, plain, (e20 = op2(e23, op2(e23, op2(e23, e23)))), inference(cnf_transformation, [], [f13])).
fof(f1673, plain, spl42_66, inference(avatar_split_clause, [], [f532, f923])).
fof(f532, plain, (e21 = op2(e23, e23)), inference(cnf_transformation, [], [f13])).
fof(f1672, plain, spl42_5, inference(avatar_split_clause, [], [f1671, f632])).
fof(f1671, plain, (e10 = op1(e13, e12)), inference(forward_demodulation, [], [f528, f530])).
fof(f528, plain, (e10 = op1(e13, op1(e13, op1(e13, e13)))), inference(cnf_transformation, [], [f12])).
fof(f1670, plain, spl42_2, inference(avatar_split_clause, [], [f529, f619])).
fof(f529, plain, (e11 = op1(e13, e13)), inference(cnf_transformation, [], [f12])).
fof(f1669, plain, (spl42_187 | spl42_188 | spl42_189 | spl42_190), inference(avatar_split_clause, [], [f518, f1666, f1662, f1658, f1654])).
fof(f518, plain, ((op2(e23, e20) = op2(op2(e23, e20), e23)) | (op2(e22, e20) = op2(op2(e22, e20), e22)) | (op2(e21, e20) = op2(op2(e21, e20), e21)) | (op2(e20, e20) = op2(op2(e20, e20), e20))), inference(cnf_transformation, [], [f52])).
fof(f52, plain, (((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15) & ((op2(e23, e23) = op2(op2(e23, e23), e23)) | (op2(e22, e23) = op2(op2(e22, e23), e22)) | (op2(e21, e23) = op2(op2(e21, e23), e21)) | (op2(e20, e23) = op2(op2(e20, e23), e20))) & ((op2(e23, e22) = op2(op2(e23, e22), e23)) | (op2(e22, e22) = op2(op2(e22, e22), e22)) | (op2(e21, e22) = op2(op2(e21, e22), e21)) | (op2(e20, e22) = op2(op2(e20, e22), e20))) & ((op2(e23, e21) = op2(op2(e23, e21), e23)) | (op2(e22, e21) = op2(op2(e22, e21), e22)) | (op2(e21, e21) = op2(op2(e21, e21), e21)) | (op2(e20, e21) = op2(op2(e20, e21), e20))) & ((op2(e23, e20) = op2(op2(e23, e20), e23)) | (op2(e22, e20) = op2(op2(e22, e20), e22)) | (op2(e21, e20) = op2(op2(e21, e20), e21)) | (op2(e20, e20) = op2(op2(e20, e20), e20)))), inference(definition_folding, [], [f11, e51, e50, e49, e48, e47, e46, e45, e44, e43, e42, e41, e40, e39, e38, e37])).
fof(f37, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP15), inference(usedef, [], [e37])).
fof(e37, plain, (sP15 <=> (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP15])])).
fof(f38, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP16), inference(usedef, [], [e38])).
fof(e38, plain, (sP16 <=> (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP16])])).
fof(f39, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP17), inference(usedef, [], [e39])).
fof(e39, plain, (sP17 <=> (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP17])])).
fof(f40, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP18), inference(usedef, [], [e40])).
fof(e40, plain, (sP18 <=> (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP18])])).
fof(f41, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP19), inference(usedef, [], [e41])).
fof(e41, plain, (sP19 <=> (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP19])])).
fof(f42, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(usedef, [], [e42])).
fof(e42, plain, (sP20 <=> (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP20])])).
fof(f43, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP21), inference(usedef, [], [e43])).
fof(e43, plain, (sP21 <=> (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP21])])).
fof(f44, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP22), inference(usedef, [], [e44])).
fof(e44, plain, (sP22 <=> (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP22])])).
fof(f45, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP23), inference(usedef, [], [e45])).
fof(e45, plain, (sP23 <=> (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP23])])).
fof(f46, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP24), inference(usedef, [], [e46])).
fof(e46, plain, (sP24 <=> (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP24])])).
fof(f47, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(usedef, [], [e47])).
fof(e47, plain, (sP25 <=> (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP25])])).
fof(f48, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP26), inference(usedef, [], [e48])).
fof(e48, plain, (sP26 <=> (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP26])])).
fof(f49, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP27), inference(usedef, [], [e49])).
fof(e49, plain, (sP27 <=> (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP27])])).
fof(f50, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP28), inference(usedef, [], [e50])).
fof(e50, plain, (sP28 <=> (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP28])])).
fof(f51, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP29), inference(usedef, [], [e51])).
fof(e51, plain, (sP29 <=> (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP29])])).
fof(f11, plain, (((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e23, e23)) & (e23 = op2(e23, e23))) | (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | (~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | (~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | (~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | (~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20)))) & ((op2(e23, e23) = op2(op2(e23, e23), e23)) | (op2(e22, e23) = op2(op2(e22, e23), e22)) | (op2(e21, e23) = op2(op2(e21, e23), e21)) | (op2(e20, e23) = op2(op2(e20, e23), e20))) & ((op2(e23, e22) = op2(op2(e23, e22), e23)) | (op2(e22, e22) = op2(op2(e22, e22), e22)) | (op2(e21, e22) = op2(op2(e21, e22), e21)) | (op2(e20, e22) = op2(op2(e20, e22), e20))) & ((op2(e23, e21) = op2(op2(e23, e21), e23)) | (op2(e22, e21) = op2(op2(e22, e21), e22)) | (op2(e21, e21) = op2(op2(e21, e21), e21)) | (op2(e20, e21) = op2(op2(e20, e21), e20))) & ((op2(e23, e20) = op2(op2(e23, e20), e23)) | (op2(e22, e20) = op2(op2(e22, e20), e22)) | (op2(e21, e20) = op2(op2(e21, e20), e21)) | (op2(e20, e20) = op2(op2(e20, e20), e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax11)).
fof(f1635, plain, (spl42_179 | spl42_180 | spl42_181 | spl42_182), inference(avatar_split_clause, [], [f520, f1632, f1628, f1624, f1620])).
fof(f520, plain, ((op2(e23, e22) = op2(op2(e23, e22), e23)) | (op2(e22, e22) = op2(op2(e22, e22), e22)) | (op2(e21, e22) = op2(op2(e21, e22), e21)) | (op2(e20, e22) = op2(op2(e20, e22), e20))), inference(cnf_transformation, [], [f52])).
fof(f1618, plain, (spl42_175 | spl42_176 | spl42_177 | spl42_178), inference(avatar_split_clause, [], [f521, f1615, f1611, f1607, f1603])).
fof(f521, plain, ((op2(e23, e23) = op2(op2(e23, e23), e23)) | (op2(e22, e23) = op2(op2(e22, e23), e22)) | (op2(e21, e23) = op2(op2(e21, e23), e21)) | (op2(e20, e23) = op2(op2(e20, e23), e20))), inference(cnf_transformation, [], [f52])).
fof(f1601, plain, (spl42_174 | spl42_173 | spl42_172 | spl42_171 | spl42_170 | spl42_169 | spl42_168 | spl42_167 | spl42_166 | spl42_165 | spl42_164 | spl42_163 | spl42_162 | spl42_161 | spl42_160 | spl42_68), inference(avatar_split_clause, [], [f522, f931, f1447, f1457, f1467, f1477, f1487, f1497, f1507, f1517, f1527, f1537, f1547, f1557, f1567, f1577, f1587])).
fof(f1587, plain, (spl42_174 <=> sP15), introduced(avatar_definition, [new_symbols(naming, [spl42_174])])).
fof(f1577, plain, (spl42_173 <=> sP16), introduced(avatar_definition, [new_symbols(naming, [spl42_173])])).
fof(f1567, plain, (spl42_172 <=> sP17), introduced(avatar_definition, [new_symbols(naming, [spl42_172])])).
fof(f1557, plain, (spl42_171 <=> sP18), introduced(avatar_definition, [new_symbols(naming, [spl42_171])])).
fof(f1547, plain, (spl42_170 <=> sP19), introduced(avatar_definition, [new_symbols(naming, [spl42_170])])).
fof(f1537, plain, (spl42_169 <=> sP20), introduced(avatar_definition, [new_symbols(naming, [spl42_169])])).
fof(f1527, plain, (spl42_168 <=> sP21), introduced(avatar_definition, [new_symbols(naming, [spl42_168])])).
fof(f1517, plain, (spl42_167 <=> sP22), introduced(avatar_definition, [new_symbols(naming, [spl42_167])])).
fof(f1507, plain, (spl42_166 <=> sP23), introduced(avatar_definition, [new_symbols(naming, [spl42_166])])).
fof(f1497, plain, (spl42_165 <=> sP24), introduced(avatar_definition, [new_symbols(naming, [spl42_165])])).
fof(f1487, plain, (spl42_164 <=> sP25), introduced(avatar_definition, [new_symbols(naming, [spl42_164])])).
fof(f1477, plain, (spl42_163 <=> sP26), introduced(avatar_definition, [new_symbols(naming, [spl42_163])])).
fof(f1467, plain, (spl42_162 <=> sP27), introduced(avatar_definition, [new_symbols(naming, [spl42_162])])).
fof(f1457, plain, (spl42_161 <=> sP28), introduced(avatar_definition, [new_symbols(naming, [spl42_161])])).
fof(f1447, plain, (spl42_160 <=> sP29), introduced(avatar_definition, [new_symbols(naming, [spl42_160])])).
fof(f522, plain, ((e23 = op2(e23, e23)) | sP29 | sP28 | sP27 | sP26 | sP25 | sP24 | sP23 | sP22 | sP21 | sP20 | sP19 | sP18 | sP17 | sP16 | sP15), inference(cnf_transformation, [], [f52])).
fof(f1595, plain, (~ spl42_174 | spl42_125), inference(avatar_split_clause, [], [f512, f1174, f1587])).
fof(f512, plain, ((e20 = op2(e20, e20)) | ~ sP15), inference(cnf_transformation, [], [f95])).
fof(f95, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e20, e20)) & (e20 = op2(e20, e20))) | ~ sP15), inference(nnf_transformation, [], [f37])).
fof(f1594, plain, (~ spl42_174 | ~ spl42_125), inference(avatar_split_clause, [], [f513, f1174, f1587])).
fof(f513, plain, (~ (e20 = op2(e20, e20)) | ~ sP15), inference(cnf_transformation, [], [f95])).
fof(f1585, plain, (~ spl42_173 | spl42_121), inference(avatar_split_clause, [], [f506, f1157, f1577])).
fof(f506, plain, ((e20 = op2(e20, e21)) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f94, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (op2(e20, e20) = e21) & (e20 = op2(e20, e21))) | ~ sP16), inference(nnf_transformation, [], [f38])).
fof(f1584, plain, (~ spl42_173 | ~ spl42_126), inference(avatar_split_clause, [], [f507, f1178, f1577])).
fof(f507, plain, (~ (op2(e20, e20) = e21) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f1582, plain, (~ spl42_173 | ~ spl42_106), inference(avatar_split_clause, [], [f509, f1093, f1577])).
fof(f509, plain, (~ (e21 = op2(e21, e21)) | ~ sP16), inference(cnf_transformation, [], [f94])).
fof(f1575, plain, (~ spl42_172 | spl42_117), inference(avatar_split_clause, [], [f500, f1140, f1567])).
fof(f500, plain, ((e20 = op2(e20, e22)) | ~ sP17), inference(cnf_transformation, [], [f93])).
fof(f93, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (op2(e20, e20) = e22) & (e20 = op2(e20, e22))) | ~ sP17), inference(nnf_transformation, [], [f39])).
fof(f1565, plain, (~ spl42_171 | spl42_113), inference(avatar_split_clause, [], [f494, f1123, f1557])).
fof(f494, plain, ((e20 = op2(e20, e23)) | ~ sP18), inference(cnf_transformation, [], [f92])).
fof(f92, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (op2(e20, e20) = e23) & (e20 = op2(e20, e23))) | ~ sP18), inference(nnf_transformation, [], [f40])).
fof(f1555, plain, (~ spl42_170 | spl42_110), inference(avatar_split_clause, [], [f488, f1110, f1547])).
fof(f488, plain, ((e21 = op2(e21, e20)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f91, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e21, e21)) & (e21 = op2(e21, e20))) | ~ sP19), inference(nnf_transformation, [], [f41])).
fof(f1554, plain, (~ spl42_170 | ~ spl42_105), inference(avatar_split_clause, [], [f489, f1089, f1547])).
fof(f489, plain, (~ (e20 = op2(e21, e21)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f1553, plain, (~ spl42_170 | ~ spl42_125), inference(avatar_split_clause, [], [f490, f1174, f1547])).
fof(f490, plain, (~ (e20 = op2(e20, e20)) | ~ sP19), inference(cnf_transformation, [], [f91])).
fof(f1545, plain, (~ spl42_169 | spl42_106), inference(avatar_split_clause, [], [f482, f1093, f1537])).
fof(f482, plain, ((e21 = op2(e21, e21)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f90, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e21, e21)) & (e21 = op2(e21, e21))) | ~ sP20), inference(nnf_transformation, [], [f42])).
fof(f1544, plain, (~ spl42_169 | ~ spl42_106), inference(avatar_split_clause, [], [f483, f1093, f1537])).
fof(f483, plain, (~ (e21 = op2(e21, e21)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f1543, plain, (~ spl42_169 | ~ spl42_109), inference(avatar_split_clause, [], [f484, f1106, f1537])).
fof(f484, plain, (~ (e20 = op2(e21, e20)) | ~ sP20), inference(cnf_transformation, [], [f90])).
fof(f1535, plain, (~ spl42_168 | spl42_102), inference(avatar_split_clause, [], [f476, f1076, f1527])).
fof(f476, plain, ((e21 = op2(e21, e22)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f89, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e21, e21)) & (e21 = op2(e21, e22))) | ~ sP21), inference(nnf_transformation, [], [f43])).
fof(f1532, plain, (~ spl42_168 | ~ spl42_90), inference(avatar_split_clause, [], [f479, f1025, f1527])).
fof(f479, plain, (~ (e21 = op2(e22, e21)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1531, plain, (~ spl42_168 | ~ spl42_87), inference(avatar_split_clause, [], [f480, f1012, f1527])).
fof(f480, plain, (~ (e22 = op2(e22, e22)) | ~ sP21), inference(cnf_transformation, [], [f89])).
fof(f1525, plain, (~ spl42_167 | spl42_98), inference(avatar_split_clause, [], [f470, f1059, f1517])).
fof(f470, plain, ((e21 = op2(e21, e23)) | ~ sP22), inference(cnf_transformation, [], [f88])).
fof(f88, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e21, e21)) & (e21 = op2(e21, e23))) | ~ sP22), inference(nnf_transformation, [], [f44])).
fof(f1515, plain, (~ spl42_166 | spl42_95), inference(avatar_split_clause, [], [f464, f1046, f1507])).
fof(f464, plain, ((e22 = op2(e22, e20)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f87, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e22, e22)) & (e22 = op2(e22, e20))) | ~ sP23), inference(nnf_transformation, [], [f45])).
fof(f1513, plain, (~ spl42_166 | ~ spl42_125), inference(avatar_split_clause, [], [f466, f1174, f1507])).
fof(f466, plain, (~ (e20 = op2(e20, e20)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f1511, plain, (~ spl42_166 | ~ spl42_119), inference(avatar_split_clause, [], [f468, f1148, f1507])).
fof(f468, plain, (~ (e22 = op2(e20, e22)) | ~ sP23), inference(cnf_transformation, [], [f87])).
fof(f1505, plain, (~ spl42_165 | spl42_91), inference(avatar_split_clause, [], [f458, f1029, f1497])).
fof(f458, plain, ((e22 = op2(e22, e21)) | ~ sP24), inference(cnf_transformation, [], [f86])).
fof(f86, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e22, e22)) & (e22 = op2(e22, e21))) | ~ sP24), inference(nnf_transformation, [], [f46])).
fof(f1495, plain, (~ spl42_164 | spl42_87), inference(avatar_split_clause, [], [f452, f1012, f1487])).
fof(f452, plain, ((e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f85, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e22, e22)) & (e22 = op2(e22, e22))) | ~ sP25), inference(nnf_transformation, [], [f47])).
fof(f1494, plain, (~ spl42_164 | ~ spl42_87), inference(avatar_split_clause, [], [f453, f1012, f1487])).
fof(f453, plain, (~ (e22 = op2(e22, e22)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f1492, plain, (~ spl42_164 | ~ spl42_90), inference(avatar_split_clause, [], [f455, f1025, f1487])).
fof(f455, plain, (~ (e21 = op2(e22, e21)) | ~ sP25), inference(cnf_transformation, [], [f85])).
fof(f1485, plain, (~ spl42_163 | spl42_83), inference(avatar_split_clause, [], [f446, f995, f1477])).
fof(f446, plain, ((e22 = op2(e22, e23)) | ~ sP26), inference(cnf_transformation, [], [f84])).
fof(f84, plain, ((~ (e23 = op2(e23, e23)) & ~ (e22 = op2(e23, e22)) & ~ (e21 = op2(e23, e21)) & ~ (e20 = op2(e23, e20)) & ~ (e23 = op2(e22, e22)) & (e22 = op2(e22, e23))) | ~ sP26), inference(nnf_transformation, [], [f48])).
fof(f1484, plain, (~ spl42_163 | ~ spl42_88), inference(avatar_split_clause, [], [f447, f1016, f1477])).
fof(f447, plain, (~ (e23 = op2(e22, e22)) | ~ sP26), inference(cnf_transformation, [], [f84])).
fof(f1473, plain, (~ spl42_162 | ~ spl42_125), inference(avatar_split_clause, [], [f442, f1174, f1467])).
fof(f442, plain, (~ (e20 = op2(e20, e20)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f83, plain, ((~ (e23 = op2(e20, e23)) & ~ (e22 = op2(e20, e22)) & ~ (e21 = op2(e20, e21)) & ~ (e20 = op2(e20, e20)) & ~ (e20 = op2(e23, e23)) & (e23 = op2(e23, e20))) | ~ sP27), inference(nnf_transformation, [], [f49])).
fof(f1471, plain, (~ spl42_162 | ~ spl42_119), inference(avatar_split_clause, [], [f444, f1148, f1467])).
fof(f444, plain, (~ (e22 = op2(e20, e22)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f1470, plain, (~ spl42_162 | ~ spl42_116), inference(avatar_split_clause, [], [f445, f1135, f1467])).
fof(f445, plain, (~ (e23 = op2(e20, e23)) | ~ sP27), inference(cnf_transformation, [], [f83])).
fof(f1465, plain, (~ spl42_161 | spl42_76), inference(avatar_split_clause, [], [f434, f965, f1457])).
fof(f434, plain, ((e23 = op2(e23, e21)) | ~ sP28), inference(cnf_transformation, [], [f82])).
fof(f82, plain, ((~ (e23 = op2(e21, e23)) & ~ (e22 = op2(e21, e22)) & ~ (e21 = op2(e21, e21)) & ~ (e20 = op2(e21, e20)) & ~ (e21 = op2(e23, e23)) & (e23 = op2(e23, e21))) | ~ sP28), inference(nnf_transformation, [], [f50])).
fof(f1455, plain, (~ spl42_160 | spl42_72), inference(avatar_split_clause, [], [f428, f948, f1447])).
fof(f428, plain, ((e23 = op2(e23, e22)) | ~ sP29), inference(cnf_transformation, [], [f81])).
fof(f81, plain, ((~ (e23 = op2(e22, e23)) & ~ (e22 = op2(e22, e22)) & ~ (e21 = op2(e22, e21)) & ~ (e20 = op2(e22, e20)) & ~ (e22 = op2(e23, e23)) & (e23 = op2(e23, e22))) | ~ sP29), inference(nnf_transformation, [], [f51])).
fof(f1445, plain, (spl42_156 | spl42_157 | spl42_158 | spl42_159), inference(avatar_split_clause, [], [f418, f1442, f1438, f1434, f1430])).
fof(f418, plain, ((op1(e13, e10) = op1(op1(e13, e10), e13)) | (op1(e12, e10) = op1(op1(e12, e10), e12)) | (op1(e11, e10) = op1(op1(e11, e10), e11)) | (op1(e10, e10) = op1(op1(e10, e10), e10))), inference(cnf_transformation, [], [f36])).
fof(f36, plain, (((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0) & ((op1(e13, e13) = op1(op1(e13, e13), e13)) | (op1(e12, e13) = op1(op1(e12, e13), e12)) | (op1(e11, e13) = op1(op1(e11, e13), e11)) | (op1(e10, e13) = op1(op1(e10, e13), e10))) & ((op1(e13, e12) = op1(op1(e13, e12), e13)) | (op1(e12, e12) = op1(op1(e12, e12), e12)) | (op1(e11, e12) = op1(op1(e11, e12), e11)) | (op1(e10, e12) = op1(op1(e10, e12), e10))) & ((op1(e13, e11) = op1(op1(e13, e11), e13)) | (op1(e12, e11) = op1(op1(e12, e11), e12)) | (op1(e11, e11) = op1(op1(e11, e11), e11)) | (op1(e10, e11) = op1(op1(e10, e11), e10))) & ((op1(e13, e10) = op1(op1(e13, e10), e13)) | (op1(e12, e10) = op1(op1(e12, e10), e12)) | (op1(e11, e10) = op1(op1(e11, e10), e11)) | (op1(e10, e10) = op1(op1(e10, e10), e10)))), inference(definition_folding, [], [f10, e35, e34, e33, e32, e31, e30, e29, e28, e27, e26, e25, e24, e23, e22, e21])).
fof(f21, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(usedef, [], [e21])).
fof(e21, plain, (sP0 <=> (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP0])])).
fof(f22, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP1), inference(usedef, [], [e22])).
fof(e22, plain, (sP1 <=> (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP1])])).
fof(f23, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP2), inference(usedef, [], [e23])).
fof(e23, plain, (sP2 <=> (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP2])])).
fof(f24, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP3), inference(usedef, [], [e24])).
fof(e24, plain, (sP3 <=> (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP3])])).
fof(f25, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP4), inference(usedef, [], [e25])).
fof(e25, plain, (sP4 <=> (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP4])])).
fof(f26, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(usedef, [], [e26])).
fof(e26, plain, (sP5 <=> (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP5])])).
fof(f27, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP6), inference(usedef, [], [e27])).
fof(e27, plain, (sP6 <=> (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP6])])).
fof(f28, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP7), inference(usedef, [], [e28])).
fof(e28, plain, (sP7 <=> (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP7])])).
fof(f29, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP8), inference(usedef, [], [e29])).
fof(e29, plain, (sP8 <=> (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP8])])).
fof(f30, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP9), inference(usedef, [], [e30])).
fof(e30, plain, (sP9 <=> (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP9])])).
fof(f31, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(usedef, [], [e31])).
fof(e31, plain, (sP10 <=> (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP10])])).
fof(f32, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP11), inference(usedef, [], [e32])).
fof(e32, plain, (sP11 <=> (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP11])])).
fof(f33, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP12), inference(usedef, [], [e33])).
fof(e33, plain, (sP12 <=> (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP12])])).
fof(f34, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP13), inference(usedef, [], [e34])).
fof(e34, plain, (sP13 <=> (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP13])])).
fof(f35, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP14), inference(usedef, [], [e35])).
fof(e35, plain, (sP14 <=> (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12)))), introduced(predicate_definition_introduction, [new_symbols(naming, [sP14])])).
fof(f10, plain, (((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e13, e13)) & (e13 = op1(e13, e13))) | (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | (~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | (~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | (~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | (~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10)))) & ((op1(e13, e13) = op1(op1(e13, e13), e13)) | (op1(e12, e13) = op1(op1(e12, e13), e12)) | (op1(e11, e13) = op1(op1(e11, e13), e11)) | (op1(e10, e13) = op1(op1(e10, e13), e10))) & ((op1(e13, e12) = op1(op1(e13, e12), e13)) | (op1(e12, e12) = op1(op1(e12, e12), e12)) | (op1(e11, e12) = op1(op1(e11, e12), e11)) | (op1(e10, e12) = op1(op1(e10, e12), e10))) & ((op1(e13, e11) = op1(op1(e13, e11), e13)) | (op1(e12, e11) = op1(op1(e12, e11), e12)) | (op1(e11, e11) = op1(op1(e11, e11), e11)) | (op1(e10, e11) = op1(op1(e10, e11), e10))) & ((op1(e13, e10) = op1(op1(e13, e10), e13)) | (op1(e12, e10) = op1(op1(e12, e10), e12)) | (op1(e11, e10) = op1(op1(e11, e10), e11)) | (op1(e10, e10) = op1(op1(e10, e10), e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax10)).
fof(f1411, plain, (spl42_148 | spl42_149 | spl42_150 | spl42_151), inference(avatar_split_clause, [], [f420, f1408, f1404, f1400, f1396])).
fof(f420, plain, ((op1(e13, e12) = op1(op1(e13, e12), e13)) | (op1(e12, e12) = op1(op1(e12, e12), e12)) | (op1(e11, e12) = op1(op1(e11, e12), e11)) | (op1(e10, e12) = op1(op1(e10, e12), e10))), inference(cnf_transformation, [], [f36])).
fof(f1394, plain, (spl42_144 | spl42_145 | spl42_146 | spl42_147), inference(avatar_split_clause, [], [f421, f1391, f1387, f1383, f1379])).
fof(f421, plain, ((op1(e13, e13) = op1(op1(e13, e13), e13)) | (op1(e12, e13) = op1(op1(e12, e13), e12)) | (op1(e11, e13) = op1(op1(e11, e13), e11)) | (op1(e10, e13) = op1(op1(e10, e13), e10))), inference(cnf_transformation, [], [f36])).
fof(f1377, plain, (spl42_143 | spl42_142 | spl42_141 | spl42_140 | spl42_139 | spl42_138 | spl42_137 | spl42_136 | spl42_135 | spl42_134 | spl42_133 | spl42_132 | spl42_131 | spl42_130 | spl42_129 | spl42_4), inference(avatar_split_clause, [], [f422, f627, f1223, f1233, f1243, f1253, f1263, f1273, f1283, f1293, f1303, f1313, f1323, f1333, f1343, f1353, f1363])).
fof(f1363, plain, (spl42_143 <=> sP0), introduced(avatar_definition, [new_symbols(naming, [spl42_143])])).
fof(f1353, plain, (spl42_142 <=> sP1), introduced(avatar_definition, [new_symbols(naming, [spl42_142])])).
fof(f1343, plain, (spl42_141 <=> sP2), introduced(avatar_definition, [new_symbols(naming, [spl42_141])])).
fof(f1333, plain, (spl42_140 <=> sP3), introduced(avatar_definition, [new_symbols(naming, [spl42_140])])).
fof(f1323, plain, (spl42_139 <=> sP4), introduced(avatar_definition, [new_symbols(naming, [spl42_139])])).
fof(f1313, plain, (spl42_138 <=> sP5), introduced(avatar_definition, [new_symbols(naming, [spl42_138])])).
fof(f1303, plain, (spl42_137 <=> sP6), introduced(avatar_definition, [new_symbols(naming, [spl42_137])])).
fof(f1293, plain, (spl42_136 <=> sP7), introduced(avatar_definition, [new_symbols(naming, [spl42_136])])).
fof(f1283, plain, (spl42_135 <=> sP8), introduced(avatar_definition, [new_symbols(naming, [spl42_135])])).
fof(f1273, plain, (spl42_134 <=> sP9), introduced(avatar_definition, [new_symbols(naming, [spl42_134])])).
fof(f1263, plain, (spl42_133 <=> sP10), introduced(avatar_definition, [new_symbols(naming, [spl42_133])])).
fof(f1253, plain, (spl42_132 <=> sP11), introduced(avatar_definition, [new_symbols(naming, [spl42_132])])).
fof(f1243, plain, (spl42_131 <=> sP12), introduced(avatar_definition, [new_symbols(naming, [spl42_131])])).
fof(f1233, plain, (spl42_130 <=> sP13), introduced(avatar_definition, [new_symbols(naming, [spl42_130])])).
fof(f1223, plain, (spl42_129 <=> sP14), introduced(avatar_definition, [new_symbols(naming, [spl42_129])])).
fof(f627, plain, (spl42_4 <=> (e13 = op1(e13, e13))), introduced(avatar_definition, [new_symbols(naming, [spl42_4])])).
fof(f422, plain, ((e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f1376, plain, (spl42_143 | spl42_142 | spl42_141 | spl42_140 | spl42_139 | spl42_138 | spl42_137 | spl42_136 | spl42_135 | spl42_134 | spl42_133 | spl42_132 | spl42_131 | spl42_130 | spl42_129 | ~ spl42_4), inference(avatar_split_clause, [], [f423, f627, f1223, f1233, f1243, f1253, f1263, f1273, f1283, f1293, f1303, f1313, f1323, f1333, f1343, f1353, f1363])).
fof(f423, plain, (~ (e13 = op1(e13, e13)) | sP14 | sP13 | sP12 | sP11 | sP10 | sP9 | sP8 | sP7 | sP6 | sP5 | sP4 | sP3 | sP2 | sP1 | sP0), inference(cnf_transformation, [], [f36])).
fof(f1370, plain, (~ spl42_143 | ~ spl42_61), inference(avatar_split_clause, [], [f413, f870, f1363])).
fof(f413, plain, (~ (e10 = op1(e10, e10)) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f80, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e10, e10)) & (e10 = op1(e10, e10))) | ~ sP0), inference(nnf_transformation, [], [f21])).
fof(f1367, plain, (~ spl42_143 | ~ spl42_55), inference(avatar_split_clause, [], [f416, f844, f1363])).
fof(f416, plain, (~ (e12 = op1(e10, e12)) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f1366, plain, (~ spl42_143 | ~ spl42_52), inference(avatar_split_clause, [], [f417, f831, f1363])).
fof(f417, plain, (~ (e13 = op1(e10, e13)) | ~ sP0), inference(cnf_transformation, [], [f80])).
fof(f1361, plain, (~ spl42_142 | spl42_57), inference(avatar_split_clause, [], [f406, f853, f1353])).
fof(f406, plain, ((e10 = op1(e10, e11)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f79, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (op1(e10, e10) = e11) & (e10 = op1(e10, e11))) | ~ sP1), inference(nnf_transformation, [], [f22])).
fof(f1360, plain, (~ spl42_142 | ~ spl42_62), inference(avatar_split_clause, [], [f407, f874, f1353])).
fof(f407, plain, (~ (op1(e10, e10) = e11) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f1359, plain, (~ spl42_142 | ~ spl42_45), inference(avatar_split_clause, [], [f408, f802, f1353])).
fof(f408, plain, (~ (e10 = op1(e11, e10)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f1357, plain, (~ spl42_142 | ~ spl42_39), inference(avatar_split_clause, [], [f410, f776, f1353])).
fof(f410, plain, (~ (e12 = op1(e11, e12)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f1356, plain, (~ spl42_142 | ~ spl42_36), inference(avatar_split_clause, [], [f411, f763, f1353])).
fof(f411, plain, (~ (e13 = op1(e11, e13)) | ~ sP1), inference(cnf_transformation, [], [f79])).
fof(f1351, plain, (~ spl42_141 | spl42_53), inference(avatar_split_clause, [], [f400, f836, f1343])).
fof(f400, plain, ((e10 = op1(e10, e12)) | ~ sP2), inference(cnf_transformation, [], [f78])).
fof(f78, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (op1(e10, e10) = e12) & (e10 = op1(e10, e12))) | ~ sP2), inference(nnf_transformation, [], [f23])).
fof(f1341, plain, (~ spl42_140 | spl42_49), inference(avatar_split_clause, [], [f394, f819, f1333])).
fof(f394, plain, ((e10 = op1(e10, e13)) | ~ sP3), inference(cnf_transformation, [], [f77])).
fof(f77, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (op1(e10, e10) = e13) & (e10 = op1(e10, e13))) | ~ sP3), inference(nnf_transformation, [], [f24])).
fof(f1329, plain, (~ spl42_139 | ~ spl42_61), inference(avatar_split_clause, [], [f390, f870, f1323])).
fof(f390, plain, (~ (e10 = op1(e10, e10)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f76, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e11, e11)) & (e11 = op1(e11, e10))) | ~ sP4), inference(nnf_transformation, [], [f25])).
fof(f1327, plain, (~ spl42_139 | ~ spl42_55), inference(avatar_split_clause, [], [f392, f844, f1323])).
fof(f392, plain, (~ (e12 = op1(e10, e12)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f1326, plain, (~ spl42_139 | ~ spl42_52), inference(avatar_split_clause, [], [f393, f831, f1323])).
fof(f393, plain, (~ (e13 = op1(e10, e13)) | ~ sP4), inference(cnf_transformation, [], [f76])).
fof(f1321, plain, (~ spl42_138 | spl42_42), inference(avatar_split_clause, [], [f382, f789, f1313])).
fof(f382, plain, ((e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f75, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e11, e11)) & (e11 = op1(e11, e11))) | ~ sP5), inference(nnf_transformation, [], [f26])).
fof(f1320, plain, (~ spl42_138 | ~ spl42_42), inference(avatar_split_clause, [], [f383, f789, f1313])).
fof(f383, plain, (~ (e11 = op1(e11, e11)) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f1316, plain, (~ spl42_138 | ~ spl42_36), inference(avatar_split_clause, [], [f387, f763, f1313])).
fof(f387, plain, (~ (e13 = op1(e11, e13)) | ~ sP5), inference(cnf_transformation, [], [f75])).
fof(f1311, plain, (~ spl42_137 | spl42_38), inference(avatar_split_clause, [], [f376, f772, f1303])).
fof(f376, plain, ((e11 = op1(e11, e12)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f74, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e11, e11)) & (e11 = op1(e11, e12))) | ~ sP6), inference(nnf_transformation, [], [f27])).
fof(f1308, plain, (~ spl42_137 | ~ spl42_26), inference(avatar_split_clause, [], [f379, f721, f1303])).
fof(f379, plain, (~ (e11 = op1(e12, e11)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1307, plain, (~ spl42_137 | ~ spl42_23), inference(avatar_split_clause, [], [f380, f708, f1303])).
fof(f380, plain, (~ (e12 = op1(e12, e12)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1306, plain, (~ spl42_137 | ~ spl42_20), inference(avatar_split_clause, [], [f381, f695, f1303])).
fof(f381, plain, (~ (e13 = op1(e12, e13)) | ~ sP6), inference(cnf_transformation, [], [f74])).
fof(f1301, plain, (~ spl42_136 | spl42_34), inference(avatar_split_clause, [], [f370, f755, f1293])).
fof(f370, plain, ((e11 = op1(e11, e13)) | ~ sP7), inference(cnf_transformation, [], [f73])).
fof(f73, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e11, e11)) & (e11 = op1(e11, e13))) | ~ sP7), inference(nnf_transformation, [], [f28])).
fof(f1291, plain, (~ spl42_135 | spl42_31), inference(avatar_split_clause, [], [f364, f742, f1283])).
fof(f364, plain, ((e12 = op1(e12, e10)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f72, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e12, e12)) & (e12 = op1(e12, e10))) | ~ sP8), inference(nnf_transformation, [], [f29])).
fof(f1289, plain, (~ spl42_135 | ~ spl42_61), inference(avatar_split_clause, [], [f366, f870, f1283])).
fof(f366, plain, (~ (e10 = op1(e10, e10)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f1287, plain, (~ spl42_135 | ~ spl42_55), inference(avatar_split_clause, [], [f368, f844, f1283])).
fof(f368, plain, (~ (e12 = op1(e10, e12)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f1286, plain, (~ spl42_135 | ~ spl42_52), inference(avatar_split_clause, [], [f369, f831, f1283])).
fof(f369, plain, (~ (e13 = op1(e10, e13)) | ~ sP8), inference(cnf_transformation, [], [f72])).
fof(f1281, plain, (~ spl42_134 | spl42_27), inference(avatar_split_clause, [], [f358, f725, f1273])).
fof(f358, plain, ((e12 = op1(e12, e11)) | ~ sP9), inference(cnf_transformation, [], [f71])).
fof(f71, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e12, e12)) & (e12 = op1(e12, e11))) | ~ sP9), inference(nnf_transformation, [], [f30])).
fof(f1271, plain, (~ spl42_133 | spl42_23), inference(avatar_split_clause, [], [f352, f708, f1263])).
fof(f352, plain, ((e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f70, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e12, e12)) & (e12 = op1(e12, e12))) | ~ sP10), inference(nnf_transformation, [], [f31])).
fof(f1270, plain, (~ spl42_133 | ~ spl42_23), inference(avatar_split_clause, [], [f353, f708, f1263])).
fof(f353, plain, (~ (e12 = op1(e12, e12)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f1268, plain, (~ spl42_133 | ~ spl42_26), inference(avatar_split_clause, [], [f355, f721, f1263])).
fof(f355, plain, (~ (e11 = op1(e12, e11)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f1266, plain, (~ spl42_133 | ~ spl42_20), inference(avatar_split_clause, [], [f357, f695, f1263])).
fof(f357, plain, (~ (e13 = op1(e12, e13)) | ~ sP10), inference(cnf_transformation, [], [f70])).
fof(f1261, plain, (~ spl42_132 | spl42_19), inference(avatar_split_clause, [], [f346, f691, f1253])).
fof(f346, plain, ((e12 = op1(e12, e13)) | ~ sP11), inference(cnf_transformation, [], [f69])).
fof(f69, plain, ((~ (e13 = op1(e13, e13)) & ~ (e12 = op1(e13, e12)) & ~ (e11 = op1(e13, e11)) & ~ (e10 = op1(e13, e10)) & ~ (e13 = op1(e12, e12)) & (e12 = op1(e12, e13))) | ~ sP11), inference(nnf_transformation, [], [f32])).
fof(f1260, plain, (~ spl42_132 | ~ spl42_24), inference(avatar_split_clause, [], [f347, f712, f1253])).
fof(f347, plain, (~ (e13 = op1(e12, e12)) | ~ sP11), inference(cnf_transformation, [], [f69])).
fof(f1249, plain, (~ spl42_131 | ~ spl42_61), inference(avatar_split_clause, [], [f342, f870, f1243])).
fof(f342, plain, (~ (e10 = op1(e10, e10)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f68, plain, ((~ (e13 = op1(e10, e13)) & ~ (e12 = op1(e10, e12)) & ~ (e11 = op1(e10, e11)) & ~ (e10 = op1(e10, e10)) & ~ (e10 = op1(e13, e13)) & (e13 = op1(e13, e10))) | ~ sP12), inference(nnf_transformation, [], [f33])).
fof(f1247, plain, (~ spl42_131 | ~ spl42_55), inference(avatar_split_clause, [], [f344, f844, f1243])).
fof(f344, plain, (~ (e12 = op1(e10, e12)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f1246, plain, (~ spl42_131 | ~ spl42_52), inference(avatar_split_clause, [], [f345, f831, f1243])).
fof(f345, plain, (~ (e13 = op1(e10, e13)) | ~ sP12), inference(cnf_transformation, [], [f68])).
fof(f1241, plain, (~ spl42_130 | spl42_12), inference(avatar_split_clause, [], [f334, f661, f1233])).
fof(f334, plain, ((e13 = op1(e13, e11)) | ~ sP13), inference(cnf_transformation, [], [f67])).
fof(f67, plain, ((~ (e13 = op1(e11, e13)) & ~ (e12 = op1(e11, e12)) & ~ (e11 = op1(e11, e11)) & ~ (e10 = op1(e11, e10)) & ~ (e11 = op1(e13, e13)) & (e13 = op1(e13, e11))) | ~ sP13), inference(nnf_transformation, [], [f34])).
fof(f1231, plain, (~ spl42_129 | spl42_8), inference(avatar_split_clause, [], [f328, f644, f1223])).
fof(f328, plain, ((e13 = op1(e13, e12)) | ~ sP14), inference(cnf_transformation, [], [f66])).
fof(f66, plain, ((~ (e13 = op1(e12, e13)) & ~ (e12 = op1(e12, e12)) & ~ (e11 = op1(e12, e11)) & ~ (e10 = op1(e12, e10)) & ~ (e12 = op1(e13, e13)) & (e13 = op1(e13, e12))) | ~ sP14), inference(nnf_transformation, [], [f35])).
fof(f1221, plain, (spl42_125 | spl42_121 | spl42_117 | spl42_113), inference(avatar_split_clause, [], [f172, f1123, f1140, f1157, f1174])).
fof(f172, plain, ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f4, plain, (((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))) & ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))) & ((e22 = op2(e23, e23)) | (e22 = op2(e22, e23)) | (e22 = op2(e21, e23)) | (e22 = op2(e20, e23))) & ((e22 = op2(e23, e23)) | (e22 = op2(e23, e22)) | (e22 = op2(e23, e21)) | (e22 = op2(e23, e20))) & ((e21 = op2(e23, e23)) | (e21 = op2(e22, e23)) | (e21 = op2(e21, e23)) | (e21 = op2(e20, e23))) & ((e21 = op2(e23, e23)) | (e21 = op2(e23, e22)) | (e21 = op2(e23, e21)) | (e21 = op2(e23, e20))) & ((e20 = op2(e23, e23)) | (e20 = op2(e22, e23)) | (e20 = op2(e21, e23)) | (e20 = op2(e20, e23))) & ((e20 = op2(e23, e23)) | (e20 = op2(e23, e22)) | (e20 = op2(e23, e21)) | (e20 = op2(e23, e20))) & ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))) & ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))) & ((e22 = op2(e23, e22)) | (e22 = op2(e22, e22)) | (e22 = op2(e21, e22)) | (e22 = op2(e20, e22))) & ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))) & ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))) & ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))) & ((e20 = op2(e23, e22)) | (e20 = op2(e22, e22)) | (e20 = op2(e21, e22)) | (e20 = op2(e20, e22))) & ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))) & ((e23 = op2(e23, e21)) | (e23 = op2(e22, e21)) | (e23 = op2(e21, e21)) | (e23 = op2(e20, e21))) & ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))) & ((e22 = op2(e23, e21)) | (e22 = op2(e22, e21)) | (e22 = op2(e21, e21)) | (e22 = op2(e20, e21))) & ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))) & ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))) & ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))) & ((e20 = op2(e23, e21)) | (e20 = op2(e22, e21)) | (e20 = op2(e21, e21)) | (e20 = op2(e20, e21))) & ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))) & ((e23 = op2(e23, e20)) | (e23 = op2(e22, e20)) | (e23 = op2(e21, e20)) | (op2(e20, e20) = e23)) & ((e23 = op2(e20, e23)) | (e23 = op2(e20, e22)) | (e23 = op2(e20, e21)) | (op2(e20, e20) = e23)) & ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)) & ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)) & ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)) & ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)) & ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))) & ((e20 = op2(e20, e23)) | (e20 = op2(e20, e22)) | (e20 = op2(e20, e21)) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax4)).
fof(f1220, plain, (spl42_125 | spl42_109 | spl42_93 | spl42_77), inference(avatar_split_clause, [], [f173, f970, f1038, f1106, f1174])).
fof(f173, plain, ((e20 = op2(e23, e20)) | (e20 = op2(e22, e20)) | (e20 = op2(e21, e20)) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f4])).
fof(f1219, plain, (spl42_126 | spl42_122 | spl42_118 | spl42_114), inference(avatar_split_clause, [], [f174, f1127, f1144, f1161, f1178])).
fof(f174, plain, ((e21 = op2(e20, e23)) | (e21 = op2(e20, e22)) | (e21 = op2(e20, e21)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f1218, plain, (spl42_126 | spl42_110 | spl42_94 | spl42_78), inference(avatar_split_clause, [], [f175, f974, f1042, f1110, f1178])).
fof(f175, plain, ((e21 = op2(e23, e20)) | (e21 = op2(e22, e20)) | (e21 = op2(e21, e20)) | (op2(e20, e20) = e21)), inference(cnf_transformation, [], [f4])).
fof(f1217, plain, (spl42_127 | spl42_123 | spl42_119 | spl42_115), inference(avatar_split_clause, [], [f176, f1131, f1148, f1165, f1182])).
fof(f176, plain, ((e22 = op2(e20, e23)) | (e22 = op2(e20, e22)) | (e22 = op2(e20, e21)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1216, plain, (spl42_127 | spl42_111 | spl42_95 | spl42_79), inference(avatar_split_clause, [], [f177, f978, f1046, f1114, f1182])).
fof(f177, plain, ((e22 = op2(e23, e20)) | (e22 = op2(e22, e20)) | (e22 = op2(e21, e20)) | (op2(e20, e20) = e22)), inference(cnf_transformation, [], [f4])).
fof(f1213, plain, (spl42_109 | spl42_105 | spl42_101 | spl42_97), inference(avatar_split_clause, [], [f180, f1055, f1072, f1089, f1106])).
fof(f180, plain, ((e20 = op2(e21, e23)) | (e20 = op2(e21, e22)) | (e20 = op2(e21, e21)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1211, plain, (spl42_110 | spl42_106 | spl42_102 | spl42_98), inference(avatar_split_clause, [], [f182, f1059, f1076, f1093, f1110])).
fof(f182, plain, ((e21 = op2(e21, e23)) | (e21 = op2(e21, e22)) | (e21 = op2(e21, e21)) | (e21 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1210, plain, (spl42_122 | spl42_106 | spl42_90 | spl42_74), inference(avatar_split_clause, [], [f183, f957, f1025, f1093, f1161])).
fof(f183, plain, ((e21 = op2(e23, e21)) | (e21 = op2(e22, e21)) | (e21 = op2(e21, e21)) | (e21 = op2(e20, e21))), inference(cnf_transformation, [], [f4])).
fof(f1209, plain, (spl42_111 | spl42_107 | spl42_103 | spl42_99), inference(avatar_split_clause, [], [f184, f1063, f1080, f1097, f1114])).
fof(f184, plain, ((e22 = op2(e21, e23)) | (e22 = op2(e21, e22)) | (e22 = op2(e21, e21)) | (e22 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1207, plain, (spl42_112 | spl42_108 | spl42_104 | spl42_100), inference(avatar_split_clause, [], [f186, f1067, f1084, f1101, f1118])).
fof(f186, plain, ((e23 = op2(e21, e23)) | (e23 = op2(e21, e22)) | (e23 = op2(e21, e21)) | (e23 = op2(e21, e20))), inference(cnf_transformation, [], [f4])).
fof(f1205, plain, (spl42_93 | spl42_89 | spl42_85 | spl42_81), inference(avatar_split_clause, [], [f188, f987, f1004, f1021, f1038])).
fof(f188, plain, ((e20 = op2(e22, e23)) | (e20 = op2(e22, e22)) | (e20 = op2(e22, e21)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1203, plain, (spl42_94 | spl42_90 | spl42_86 | spl42_82), inference(avatar_split_clause, [], [f190, f991, f1008, f1025, f1042])).
fof(f190, plain, ((e21 = op2(e22, e23)) | (e21 = op2(e22, e22)) | (e21 = op2(e22, e21)) | (e21 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1202, plain, (spl42_118 | spl42_102 | spl42_86 | spl42_70), inference(avatar_split_clause, [], [f191, f940, f1008, f1076, f1144])).
fof(f191, plain, ((e21 = op2(e23, e22)) | (e21 = op2(e22, e22)) | (e21 = op2(e21, e22)) | (e21 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1201, plain, (spl42_95 | spl42_91 | spl42_87 | spl42_83), inference(avatar_split_clause, [], [f192, f995, f1012, f1029, f1046])).
fof(f192, plain, ((e22 = op2(e22, e23)) | (e22 = op2(e22, e22)) | (e22 = op2(e22, e21)) | (e22 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1199, plain, (spl42_96 | spl42_92 | spl42_88 | spl42_84), inference(avatar_split_clause, [], [f194, f999, f1016, f1033, f1050])).
fof(f194, plain, ((e23 = op2(e22, e23)) | (e23 = op2(e22, e22)) | (e23 = op2(e22, e21)) | (e23 = op2(e22, e20))), inference(cnf_transformation, [], [f4])).
fof(f1198, plain, (spl42_120 | spl42_104 | spl42_88 | spl42_72), inference(avatar_split_clause, [], [f195, f948, f1016, f1084, f1152])).
fof(f195, plain, ((e23 = op2(e23, e22)) | (e23 = op2(e22, e22)) | (e23 = op2(e21, e22)) | (e23 = op2(e20, e22))), inference(cnf_transformation, [], [f4])).
fof(f1191, plain, (spl42_80 | spl42_76 | spl42_72 | spl42_68), inference(avatar_split_clause, [], [f202, f931, f948, f965, f982])).
fof(f202, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e23, e22)) | (e23 = op2(e23, e21)) | (e23 = op2(e23, e20))), inference(cnf_transformation, [], [f4])).
fof(f1190, plain, (spl42_116 | spl42_100 | spl42_84 | spl42_68), inference(avatar_split_clause, [], [f203, f931, f999, f1067, f1135])).
fof(f203, plain, ((e23 = op2(e23, e23)) | (e23 = op2(e22, e23)) | (e23 = op2(e21, e23)) | (e23 = op2(e20, e23))), inference(cnf_transformation, [], [f4])).
fof(f1189, plain, (spl42_125 | spl42_126 | spl42_127 | spl42_128), inference(avatar_split_clause, [], [f156, f1186, f1182, f1178, f1174])).
fof(f156, plain, ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20))), inference(cnf_transformation, [], [f3])).
fof(f3, plain, (((e23 = op2(e23, e23)) | (e22 = op2(e23, e23)) | (e21 = op2(e23, e23)) | (e20 = op2(e23, e23))) & ((e23 = op2(e23, e22)) | (e22 = op2(e23, e22)) | (e21 = op2(e23, e22)) | (e20 = op2(e23, e22))) & ((e23 = op2(e23, e21)) | (e22 = op2(e23, e21)) | (e21 = op2(e23, e21)) | (e20 = op2(e23, e21))) & ((e23 = op2(e23, e20)) | (e22 = op2(e23, e20)) | (e21 = op2(e23, e20)) | (e20 = op2(e23, e20))) & ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))) & ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))) & ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))) & ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))) & ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))) & ((e23 = op2(e21, e22)) | (e22 = op2(e21, e22)) | (e21 = op2(e21, e22)) | (e20 = op2(e21, e22))) & ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))) & ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))) & ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))) & ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))) & ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))) & ((op2(e20, e20) = e23) | (op2(e20, e20) = e22) | (op2(e20, e20) = e21) | (e20 = op2(e20, e20)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax3)).
fof(f1172, plain, (spl42_121 | spl42_122 | spl42_123 | spl42_124), inference(avatar_split_clause, [], [f157, f1169, f1165, f1161, f1157])).
fof(f157, plain, ((e23 = op2(e20, e21)) | (e22 = op2(e20, e21)) | (e21 = op2(e20, e21)) | (e20 = op2(e20, e21))), inference(cnf_transformation, [], [f3])).
fof(f1155, plain, (spl42_117 | spl42_118 | spl42_119 | spl42_120), inference(avatar_split_clause, [], [f158, f1152, f1148, f1144, f1140])).
fof(f158, plain, ((e23 = op2(e20, e22)) | (e22 = op2(e20, e22)) | (e21 = op2(e20, e22)) | (e20 = op2(e20, e22))), inference(cnf_transformation, [], [f3])).
fof(f1138, plain, (spl42_113 | spl42_114 | spl42_115 | spl42_116), inference(avatar_split_clause, [], [f159, f1135, f1131, f1127, f1123])).
fof(f159, plain, ((e23 = op2(e20, e23)) | (e22 = op2(e20, e23)) | (e21 = op2(e20, e23)) | (e20 = op2(e20, e23))), inference(cnf_transformation, [], [f3])).
fof(f1121, plain, (spl42_109 | spl42_110 | spl42_111 | spl42_112), inference(avatar_split_clause, [], [f160, f1118, f1114, f1110, f1106])).
fof(f160, plain, ((e23 = op2(e21, e20)) | (e22 = op2(e21, e20)) | (e21 = op2(e21, e20)) | (e20 = op2(e21, e20))), inference(cnf_transformation, [], [f3])).
fof(f1104, plain, (spl42_105 | spl42_106 | spl42_107 | spl42_108), inference(avatar_split_clause, [], [f161, f1101, f1097, f1093, f1089])).
fof(f161, plain, ((e23 = op2(e21, e21)) | (e22 = op2(e21, e21)) | (e21 = op2(e21, e21)) | (e20 = op2(e21, e21))), inference(cnf_transformation, [], [f3])).
fof(f1070, plain, (spl42_97 | spl42_98 | spl42_99 | spl42_100), inference(avatar_split_clause, [], [f163, f1067, f1063, f1059, f1055])).
fof(f163, plain, ((e23 = op2(e21, e23)) | (e22 = op2(e21, e23)) | (e21 = op2(e21, e23)) | (e20 = op2(e21, e23))), inference(cnf_transformation, [], [f3])).
fof(f1053, plain, (spl42_93 | spl42_94 | spl42_95 | spl42_96), inference(avatar_split_clause, [], [f164, f1050, f1046, f1042, f1038])).
fof(f164, plain, ((e23 = op2(e22, e20)) | (e22 = op2(e22, e20)) | (e21 = op2(e22, e20)) | (e20 = op2(e22, e20))), inference(cnf_transformation, [], [f3])).
fof(f1036, plain, (spl42_89 | spl42_90 | spl42_91 | spl42_92), inference(avatar_split_clause, [], [f165, f1033, f1029, f1025, f1021])).
fof(f165, plain, ((e23 = op2(e22, e21)) | (e22 = op2(e22, e21)) | (e21 = op2(e22, e21)) | (e20 = op2(e22, e21))), inference(cnf_transformation, [], [f3])).
fof(f1019, plain, (spl42_85 | spl42_86 | spl42_87 | spl42_88), inference(avatar_split_clause, [], [f166, f1016, f1012, f1008, f1004])).
fof(f166, plain, ((e23 = op2(e22, e22)) | (e22 = op2(e22, e22)) | (e21 = op2(e22, e22)) | (e20 = op2(e22, e22))), inference(cnf_transformation, [], [f3])).
fof(f1002, plain, (spl42_81 | spl42_82 | spl42_83 | spl42_84), inference(avatar_split_clause, [], [f167, f999, f995, f991, f987])).
fof(f167, plain, ((e23 = op2(e22, e23)) | (e22 = op2(e22, e23)) | (e21 = op2(e22, e23)) | (e20 = op2(e22, e23))), inference(cnf_transformation, [], [f3])).
fof(f917, plain, (spl42_61 | spl42_57 | spl42_53 | spl42_49), inference(avatar_split_clause, [], [f124, f819, f836, f853, f870])).
fof(f124, plain, ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f2, plain, (((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))) & ((e13 = op1(e13, e13)) | (e13 = op1(e13, e12)) | (e13 = op1(e13, e11)) | (e13 = op1(e13, e10))) & ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))) & ((e12 = op1(e13, e13)) | (e12 = op1(e13, e12)) | (e12 = op1(e13, e11)) | (e12 = op1(e13, e10))) & ((e11 = op1(e13, e13)) | (e11 = op1(e12, e13)) | (e11 = op1(e11, e13)) | (e11 = op1(e10, e13))) & ((e11 = op1(e13, e13)) | (e11 = op1(e13, e12)) | (e11 = op1(e13, e11)) | (e11 = op1(e13, e10))) & ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))) & ((e10 = op1(e13, e13)) | (e10 = op1(e13, e12)) | (e10 = op1(e13, e11)) | (e10 = op1(e13, e10))) & ((e13 = op1(e13, e12)) | (e13 = op1(e12, e12)) | (e13 = op1(e11, e12)) | (e13 = op1(e10, e12))) & ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))) & ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))) & ((e12 = op1(e12, e13)) | (e12 = op1(e12, e12)) | (e12 = op1(e12, e11)) | (e12 = op1(e12, e10))) & ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))) & ((e11 = op1(e12, e13)) | (e11 = op1(e12, e12)) | (e11 = op1(e12, e11)) | (e11 = op1(e12, e10))) & ((e10 = op1(e13, e12)) | (e10 = op1(e12, e12)) | (e10 = op1(e11, e12)) | (e10 = op1(e10, e12))) & ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))) & ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))) & ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))) & ((e12 = op1(e13, e11)) | (e12 = op1(e12, e11)) | (e12 = op1(e11, e11)) | (e12 = op1(e10, e11))) & ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))) & ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))) & ((e11 = op1(e11, e13)) | (e11 = op1(e11, e12)) | (e11 = op1(e11, e11)) | (e11 = op1(e11, e10))) & ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))) & ((e10 = op1(e11, e13)) | (e10 = op1(e11, e12)) | (e10 = op1(e11, e11)) | (e10 = op1(e11, e10))) & ((e13 = op1(e13, e10)) | (e13 = op1(e12, e10)) | (e13 = op1(e11, e10)) | (op1(e10, e10) = e13)) & ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)) & ((e12 = op1(e13, e10)) | (e12 = op1(e12, e10)) | (e12 = op1(e11, e10)) | (op1(e10, e10) = e12)) & ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)) & ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)) & ((e11 = op1(e10, e13)) | (e11 = op1(e10, e12)) | (e11 = op1(e10, e11)) | (op1(e10, e10) = e11)) & ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))) & ((e10 = op1(e10, e13)) | (e10 = op1(e10, e12)) | (e10 = op1(e10, e11)) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax2)).
fof(f916, plain, (spl42_61 | spl42_45 | spl42_29 | spl42_13), inference(avatar_split_clause, [], [f125, f666, f734, f802, f870])).
fof(f125, plain, ((e10 = op1(e13, e10)) | (e10 = op1(e12, e10)) | (e10 = op1(e11, e10)) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f2])).
fof(f914, plain, (spl42_62 | spl42_46 | spl42_30 | spl42_14), inference(avatar_split_clause, [], [f127, f670, f738, f806, f874])).
fof(f127, plain, ((e11 = op1(e13, e10)) | (e11 = op1(e12, e10)) | (e11 = op1(e11, e10)) | (op1(e10, e10) = e11)), inference(cnf_transformation, [], [f2])).
fof(f913, plain, (spl42_63 | spl42_59 | spl42_55 | spl42_51), inference(avatar_split_clause, [], [f128, f827, f844, f861, f878])).
fof(f128, plain, ((e12 = op1(e10, e13)) | (e12 = op1(e10, e12)) | (e12 = op1(e10, e11)) | (op1(e10, e10) = e12)), inference(cnf_transformation, [], [f2])).
fof(f911, plain, (spl42_64 | spl42_60 | spl42_56 | spl42_52), inference(avatar_split_clause, [], [f130, f831, f848, f865, f882])).
fof(f130, plain, ((e13 = op1(e10, e13)) | (e13 = op1(e10, e12)) | (e13 = op1(e10, e11)) | (op1(e10, e10) = e13)), inference(cnf_transformation, [], [f2])).
fof(f908, plain, (spl42_57 | spl42_41 | spl42_25 | spl42_9), inference(avatar_split_clause, [], [f133, f649, f717, f785, f853])).
fof(f133, plain, ((e10 = op1(e13, e11)) | (e10 = op1(e12, e11)) | (e10 = op1(e11, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f906, plain, (spl42_58 | spl42_42 | spl42_26 | spl42_10), inference(avatar_split_clause, [], [f135, f653, f721, f789, f857])).
fof(f135, plain, ((e11 = op1(e13, e11)) | (e11 = op1(e12, e11)) | (e11 = op1(e11, e11)) | (e11 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f905, plain, (spl42_47 | spl42_43 | spl42_39 | spl42_35), inference(avatar_split_clause, [], [f136, f759, f776, f793, f810])).
fof(f136, plain, ((e12 = op1(e11, e13)) | (e12 = op1(e11, e12)) | (e12 = op1(e11, e11)) | (e12 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f903, plain, (spl42_48 | spl42_44 | spl42_40 | spl42_36), inference(avatar_split_clause, [], [f138, f763, f780, f797, f814])).
fof(f138, plain, ((e13 = op1(e11, e13)) | (e13 = op1(e11, e12)) | (e13 = op1(e11, e11)) | (e13 = op1(e11, e10))), inference(cnf_transformation, [], [f2])).
fof(f902, plain, (spl42_60 | spl42_44 | spl42_28 | spl42_12), inference(avatar_split_clause, [], [f139, f661, f729, f797, f865])).
fof(f139, plain, ((e13 = op1(e13, e11)) | (e13 = op1(e12, e11)) | (e13 = op1(e11, e11)) | (e13 = op1(e10, e11))), inference(cnf_transformation, [], [f2])).
fof(f901, plain, (spl42_29 | spl42_25 | spl42_21 | spl42_17), inference(avatar_split_clause, [], [f140, f683, f700, f717, f734])).
fof(f140, plain, ((e10 = op1(e12, e13)) | (e10 = op1(e12, e12)) | (e10 = op1(e12, e11)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f898, plain, (spl42_54 | spl42_38 | spl42_22 | spl42_6), inference(avatar_split_clause, [], [f143, f636, f704, f772, f840])).
fof(f143, plain, ((e11 = op1(e13, e12)) | (e11 = op1(e12, e12)) | (e11 = op1(e11, e12)) | (e11 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f896, plain, (spl42_55 | spl42_39 | spl42_23 | spl42_7), inference(avatar_split_clause, [], [f145, f640, f708, f776, f844])).
fof(f145, plain, ((e12 = op1(e13, e12)) | (e12 = op1(e12, e12)) | (e12 = op1(e11, e12)) | (e12 = op1(e10, e12))), inference(cnf_transformation, [], [f2])).
fof(f895, plain, (spl42_32 | spl42_28 | spl42_24 | spl42_20), inference(avatar_split_clause, [], [f146, f695, f712, f729, f746])).
fof(f146, plain, ((e13 = op1(e12, e13)) | (e13 = op1(e12, e12)) | (e13 = op1(e12, e11)) | (e13 = op1(e12, e10))), inference(cnf_transformation, [], [f2])).
fof(f892, plain, (spl42_49 | spl42_33 | spl42_17 | spl42_1), inference(avatar_split_clause, [], [f149, f615, f683, f751, f819])).
fof(f149, plain, ((e10 = op1(e13, e13)) | (e10 = op1(e12, e13)) | (e10 = op1(e11, e13)) | (e10 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f888, plain, (spl42_51 | spl42_35 | spl42_19 | spl42_3), inference(avatar_split_clause, [], [f153, f623, f691, f759, f827])).
fof(f153, plain, ((e12 = op1(e13, e13)) | (e12 = op1(e12, e13)) | (e12 = op1(e11, e13)) | (e12 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f886, plain, (spl42_52 | spl42_36 | spl42_20 | spl42_4), inference(avatar_split_clause, [], [f155, f627, f695, f763, f831])).
fof(f155, plain, ((e13 = op1(e13, e13)) | (e13 = op1(e12, e13)) | (e13 = op1(e11, e13)) | (e13 = op1(e10, e13))), inference(cnf_transformation, [], [f2])).
fof(f885, plain, (spl42_61 | spl42_62 | spl42_63 | spl42_64), inference(avatar_split_clause, [], [f108, f882, f878, f874, f870])).
fof(f108, plain, ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10))), inference(cnf_transformation, [], [f1])).
fof(f1, plain, (((e13 = op1(e13, e13)) | (e12 = op1(e13, e13)) | (e11 = op1(e13, e13)) | (e10 = op1(e13, e13))) & ((e13 = op1(e13, e12)) | (e12 = op1(e13, e12)) | (e11 = op1(e13, e12)) | (e10 = op1(e13, e12))) & ((e13 = op1(e13, e11)) | (e12 = op1(e13, e11)) | (e11 = op1(e13, e11)) | (e10 = op1(e13, e11))) & ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))) & ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))) & ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))) & ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))) & ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))) & ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))) & ((e13 = op1(e11, e12)) | (e12 = op1(e11, e12)) | (e11 = op1(e11, e12)) | (e10 = op1(e11, e12))) & ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))) & ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))) & ((e13 = op1(e10, e13)) | (e12 = op1(e10, e13)) | (e11 = op1(e10, e13)) | (e10 = op1(e10, e13))) & ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))) & ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))) & ((op1(e10, e10) = e13) | (op1(e10, e10) = e12) | (op1(e10, e10) = e11) | (e10 = op1(e10, e10)))), file('/home/ubuntu/library/tptp/Problems/ALG/ALG122+1.p', ax1)).
fof(f868, plain, (spl42_57 | spl42_58 | spl42_59 | spl42_60), inference(avatar_split_clause, [], [f109, f865, f861, f857, f853])).
fof(f109, plain, ((e13 = op1(e10, e11)) | (e12 = op1(e10, e11)) | (e11 = op1(e10, e11)) | (e10 = op1(e10, e11))), inference(cnf_transformation, [], [f1])).
fof(f851, plain, (spl42_53 | spl42_54 | spl42_55 | spl42_56), inference(avatar_split_clause, [], [f110, f848, f844, f840, f836])).
fof(f110, plain, ((e13 = op1(e10, e12)) | (e12 = op1(e10, e12)) | (e11 = op1(e10, e12)) | (e10 = op1(e10, e12))), inference(cnf_transformation, [], [f1])).
fof(f817, plain, (spl42_45 | spl42_46 | spl42_47 | spl42_48), inference(avatar_split_clause, [], [f112, f814, f810, f806, f802])).
fof(f112, plain, ((e13 = op1(e11, e10)) | (e12 = op1(e11, e10)) | (e11 = op1(e11, e10)) | (e10 = op1(e11, e10))), inference(cnf_transformation, [], [f1])).
fof(f800, plain, (spl42_41 | spl42_42 | spl42_43 | spl42_44), inference(avatar_split_clause, [], [f113, f797, f793, f789, f785])).
fof(f113, plain, ((e13 = op1(e11, e11)) | (e12 = op1(e11, e11)) | (e11 = op1(e11, e11)) | (e10 = op1(e11, e11))), inference(cnf_transformation, [], [f1])).
fof(f766, plain, (spl42_33 | spl42_34 | spl42_35 | spl42_36), inference(avatar_split_clause, [], [f115, f763, f759, f755, f751])).
fof(f115, plain, ((e13 = op1(e11, e13)) | (e12 = op1(e11, e13)) | (e11 = op1(e11, e13)) | (e10 = op1(e11, e13))), inference(cnf_transformation, [], [f1])).
fof(f749, plain, (spl42_29 | spl42_30 | spl42_31 | spl42_32), inference(avatar_split_clause, [], [f116, f746, f742, f738, f734])).
fof(f116, plain, ((e13 = op1(e12, e10)) | (e12 = op1(e12, e10)) | (e11 = op1(e12, e10)) | (e10 = op1(e12, e10))), inference(cnf_transformation, [], [f1])).
fof(f732, plain, (spl42_25 | spl42_26 | spl42_27 | spl42_28), inference(avatar_split_clause, [], [f117, f729, f725, f721, f717])).
fof(f117, plain, ((e13 = op1(e12, e11)) | (e12 = op1(e12, e11)) | (e11 = op1(e12, e11)) | (e10 = op1(e12, e11))), inference(cnf_transformation, [], [f1])).
fof(f715, plain, (spl42_21 | spl42_22 | spl42_23 | spl42_24), inference(avatar_split_clause, [], [f118, f712, f708, f704, f700])).
fof(f118, plain, ((e13 = op1(e12, e12)) | (e12 = op1(e12, e12)) | (e11 = op1(e12, e12)) | (e10 = op1(e12, e12))), inference(cnf_transformation, [], [f1])).
fof(f698, plain, (spl42_17 | spl42_18 | spl42_19 | spl42_20), inference(avatar_split_clause, [], [f119, f695, f691, f687, f683])).
fof(f119, plain, ((e13 = op1(e12, e13)) | (e12 = op1(e12, e13)) | (e11 = op1(e12, e13)) | (e10 = op1(e12, e13))), inference(cnf_transformation, [], [f1])).
fof(f681, plain, (spl42_13 | spl42_14 | spl42_15 | spl42_16), inference(avatar_split_clause, [], [f120, f678, f674, f670, f666])).
fof(f120, plain, ((e13 = op1(e13, e10)) | (e12 = op1(e13, e10)) | (e11 = op1(e13, e10)) | (e10 = op1(e13, e10))), inference(cnf_transformation, [], [f1])).